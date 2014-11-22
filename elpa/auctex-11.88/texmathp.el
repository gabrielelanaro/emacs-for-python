;;; texmathp.el -- Code to check if point is inside LaTeX math environment

;; Copyright (C) 1998, 2004 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.LeidenUniv.nl>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;;  This code provides a function to determine if point in a buffer is
;;  inside a (La)TeX math environment.  This is not trivial since many
;;  different ways are used to switch between the two.  Examples:
;;
;;    \begin{equation}  ... \end{equation}
;;    $ ... $
;;    $$ ... $$
;;    \[ ... \]
;;    \ensuremath{...}
;;    \mbox{...}
;;
;;  To install, put this file on your load-path and compile it.
;;
;;  To use this in a Lisp program, do
;;
;;     (require 'texmathp)
;;
;;  You can then write code like this:
;;
;;     (if (texmathp) ...)
;;
;;  The call to `texmathp' leaves some extra information in the
;;  variable `texmathp-why'.  It's value is a cons cell (MATCH . POSITION),
;;  specifying which command at what position is responsible for math
;;  mode being on or off.
;;
;;  To configure which macros and environments influence LaTeX math mode,
;;  customize the variable `texmathp-tex-commands'.  By default
;;  it recognizes the LaTeX core as well as AMS-LaTeX (see the variable
;;  `texmathp-tex-commands-default', also as an example).
;;
;;  To try out the code interactively, use `M-x texmathp RET'.
;;
;;  Of course, in order to work this function has to assume that the
;;  LaTeX above point is syntactically correct.  In particular:
;;
;;  o The different math delimiters are paired correctly.  Thus if
;;    you do things like "\begin{equation} $"  or "\[ ... \)"
;;    the result of (texmathp) is undefined.  It is in fact possible
;;    in LaTeX to pair \[ with $$ and \( with $, but this will confuse
;;    texmathp (and human readers as well).
;;
;;  o However, texmathp will correctly work with nested delimiters.
;;    Something like the following will be parsed correctly at any point:
;;
;;       \begin{equation}
;;          x = y \mbox{abc \ensuremath{\alpha} cba $2^3$}
;;       \end{equation}
;;
;;  o texmathp is somewhat forgiving if you have an empty line inside
;;    the current math environment, which is not legal in TeX but may
;;    easily happen during editing.  Depending upon the variable
;;    `texmathp-search-n-paragraphs' several paragraphs are checked
;;    backwards, by default 2.  Paragraph here means something limited
;;    by an empty line.
;;--------------------------------------------------------------------------
;;
;;  BUGS:
;;
;;  If any of the the special macros like \mbox or \ensuremath has optional
;;  arguments, math mode inside these optional arguments is *not* influenced
;;  by the macro.
;;--------------------------------------------------------------------------

;;; Code:

(defgroup texmathp nil
  "Testing TeX and LaTeX documents for math mode."
  :tag "Test For TeX and LaTeX Math Mode"
  :prefix "texmathp-"
  :group 'tex)

;; Some internal variables which are computed from `texmathp-tex-commands'
;; and `texmathp-tex-commands-default'.
(defvar texmathp-environments nil)
(defvar texmathp-macros nil)
(defvar texmathp-onoff-regexp nil)
(defvar texmathp-toggle-regexp nil)
(defvar texmathp-tex-commands1 nil)
(defvar texmathp-memory nil)

(defvar texmathp-tex-commands)		; silence the compiler

(defvar texmathp-tex-commands-default
  '(;; Plain TeX
    ("$$"            sw-toggle)   ("$"             sw-toggle)
    ("\\hbox"        arg-off)
    ("\\vbox"        arg-off)
    ("\\vtop"        arg-off)
    ("\\vcenter"     arg-off)

    ;; Standard LaTeX
    ("equation"      env-on)
    ("eqnarray"      env-on)      ("eqnarray*"     env-on)
    ("math"          env-on)
    ("displaymath"   env-on)
    ("minipage"      env-off)
    ("\\fbox"        arg-off)
    ("\\mbox"        arg-off)
    ("\\framebox"    arg-off)
    ("\\label"       arg-off)
    ("\\textrm"      arg-off)
    ("\\("           sw-on)       ("\\)"           sw-off)
    ("\\["           sw-on)       ("\\]"           sw-off)
    ("\\ensuremath"  arg-on)

    ;; AMS-LaTeX
    ("equation*"     env-on)
    ("align"         env-on)      ("align*"        env-on)
    ("gather"        env-on)      ("gather*"       env-on)
    ("multline"      env-on)      ("multline*"     env-on)
    ("flalign"       env-on)      ("flalign*"      env-on)
    ("alignat"       env-on)      ("alignat*"      env-on)
    ("xalignat"      env-on)      ("xalignat*"     env-on)
    ("xxalignat"     env-on)      ("\\boxed"       arg-on)
    ("\\text"        arg-off)     ("\\intertext"   arg-off)

    ;; mathtools
    ("\\shortintertext"   arg-off))
  "The default entries for `texmathp-tex-commands', which see.")

(defun texmathp-compile ()
  "Compile the value of `texmathp-tex-commands' into the internal lists.
Call this when you have changed the value of that variable without using
customize (customize calls it when setting the variable)."
  (interactive)
  ;; Extract lists and regexp.
  (setq texmathp-macros nil texmathp-environments nil)
  (setq texmathp-memory
	(cons texmathp-tex-commands texmathp-tex-commands-default))
  (setq texmathp-tex-commands1 (append texmathp-tex-commands
				       texmathp-tex-commands-default))
  (let ((list (reverse texmathp-tex-commands1))
	var entry type switches togglers)
    (while (setq entry (car list))
      (setq type (nth 1 entry)
	    list (cdr list)
	    var (cond ((memq type '(env-on env-off)) 'texmathp-environments)
		      ((memq type '(arg-on arg-off)) 'texmathp-macros)
		      ((memq type '(sw-on sw-off))   'switches)
		      ((memq type '(sw-toggle))      'togglers)))
      (set var (cons (car entry) (symbol-value var))))
    (setq texmathp-onoff-regexp
	  (concat "[^\\\\]\\("
		  (mapconcat 'regexp-quote switches "\\|")
		  "\\)")
	  texmathp-toggle-regexp
	  (concat "\\([^\\\\\\$]\\|\\`\\)\\("
		  (mapconcat 'regexp-quote togglers "\\|")
		  "\\)"))))

(defcustom texmathp-tex-commands nil
  "List of environments and macros influencing (La)TeX math mode.
This user-defined list is used in addition to LaTeX and AMSLaTeX defaults.
The structure of each entry is (NAME TYPE)

- The first item in each entry is the name of an environment or macro.
  If it's a macro, include the backslash.

- The second item is a symbol indicating how the command works:
    `env-on'     Environment: turns math mode for its body  on
    `env-off'    Environment: turns math mode for its body  off
    `arg-on'     Command: turns math mode for its arguments on
    `arg-off'    Command: turns math mode for its arguments off
    `sw-on'      Switch: turns math-mode of following text  on
    `sw-off'     Switch: turns math-mode of following text  off
    `sw-toggle'  Switch: toggles math mode of following text"
  :group 'texmathp
  :set '(lambda (symbol value) (set-default symbol value) (texmathp-compile))
  :type
  '(repeat
    (list :value ("" env-on)
     (string  :tag "Name")
     (choice  :tag "Type"
      (const :tag "Environment: turns math mode for its body on" env-on)
      (const :tag "Environment: turns math mode for its body off" env-off)
      (const :tag "Command: turns math mode for its argument on" arg-on)
      (const :tag "Command: turns math-mode for its argument off" arg-off)
      (const :tag "Switch: turns math-mode of following text on" sw-on)
      (const :tag "Switch: turns math-mode of following text off" sw-off)
      (const :tag "Switch: toggles math mode of following text" sw-toggle)))))

(defcustom texmathp-search-n-paragraphs 2
  "*Number of paragraphs to check before point.
Normally, you cannot have an empty line in a math environment in (La)TeX.
The fastest method to test for math mode is then limiting the search
backward to the nearest empty line.
However, during editing it happens that such lines exist temporarily.
Therefore we look a little further.  This variable determines how many
empty lines we go back to fix the search limit."
  :group 'texmathp
  :type 'number)

(defcustom texmathp-allow-detached-args nil
  "*Non-nil means, allow arguments of macros to be detached by whitespace.
When this is t, `aaa' will be interpreted as an argument of \bb in the
following construct:  \bbb [xxx] {aaa}
This is legal in TeX.  The disadvantage is that any number of braces expressions
will be considered arguments of the macro independent of its definition."
  :group 'texmathp
  :type 'boolean)

(defvar texmathp-why nil
  "After a call to `texmathp' this variable shows why math-mode is on or off.
The value is a cons cell (MATCH . POSITION).
MATCH is a string like a car of an entry in `texmathp-tex-commands', e.q.
\"equation\" or \"\\ensuremath\" or \"\\=\\[\" or \"$\".
POSITION is the buffer position of the match.  If there was no match,
it points to the limit used for searches, usually two paragraphs up.")

;; We need our own syntax table to play with the syntax of () [] and {}
;; For speed reasons we define it statically instead of copying it each time.
(defvar texmathp-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (lambda (x) (modify-syntax-entry (car x) (cdr x) table))
	  '((?\\ . "\\") (?\f .">")  (?\n . ">")  (?% . "<")
	    (?\[ . ".")  (?\] . ".") (?\{ . "(}") (?\} . "){")
	    (?\( . ".")  (?\) . ".") (?\" . ".")  (?& . ".")   (?_ . ".")
	    (?@ . "_")   (?~ . " ")  (?$ . "$")   (?' . "w")))
    table)
  "Syntax table used while texmathp is parsing.")

;;;###autoload
(defun texmathp ()
  "Determine if point is inside (La)TeX math mode.
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked."
  (interactive)
  (let* ((pos (point)) math-on sw-match
	 (bound (save-excursion
		  (if (re-search-backward "[\n\t][ \t]*[\n\r]"
					  nil 1 texmathp-search-n-paragraphs)
		      (match-beginning 0)
		    (point-min))))
	 (mac-match (texmathp-match-macro bound))
	 (env-match (texmathp-match-environment
		     (if (and mac-match (> (cdr mac-match) bound))
			 (cdr mac-match)
		       bound)))
	 (match (cons nil bound)))

    ;; Select the nearer match
    (and env-match (setq match env-match))
    (and mac-match (> (cdr mac-match) (cdr match)) (setq match mac-match))
    (setq math-on (memq (nth 1 (assoc (car match) texmathp-tex-commands1))
			'(env-on arg-on)))

    ;; Check for switches
    (and (not math-on)
	 (setq sw-match (texmathp-match-switch bound))
	 (> (cdr sw-match) (cdr match))
	 (eq (nth 1 (assoc (car sw-match) texmathp-tex-commands1)) 'sw-on)
	 (setq match sw-match math-on t))

    ;; Check for togglers
    (if (not math-on)
	(save-excursion
	  (goto-char (cdr match))
	  (while (re-search-forward texmathp-toggle-regexp pos t)
	    (if (setq math-on (not math-on))
		(setq sw-match (cons (match-string 2) (match-beginning 2)))
	      (setq sw-match nil)))
	  (and math-on sw-match (setq match sw-match))))

    ;; Store info, show as message when interactive, and return
    (setq texmathp-why match)
    (and (interactive-p)
	 (message "math-mode is %s: %s begins at buffer position %d"
		  (if math-on "on" "off")
		  (or (car match) "new paragraph")
		  (cdr match)))
    (and math-on t)))

(defun texmathp-match-environment (bound)
  "Find out if point is inside any of the math environments.
Limit searched to BOUND.  The return value is like (\"equation\" . (point))."
  (catch 'exit
    (save-excursion
      (and (null texmathp-environments) (throw 'exit nil))
      ;; Check if the line we are starting with is a commented one.
      (let ((orig-comment-flag
	     ;; Could be replaced by `TeX-in-commented-line'.
	     (progn
	       (save-excursion
		 (beginning-of-line)
		 (skip-chars-forward " \t")
		 (string= (buffer-substring-no-properties
			   (point) (min (point-max)
					(+ (point) (length comment-start))))
			  comment-start))))
	    end-list env)
	(while (re-search-backward "\\\\\\(begin\\|end\\)[ \t]*{\\([^}]+\\)}"
				   bound t)
	  ;; Check if the match found is inside of a comment.
	  (let ((current-comment-flag
		 ;; Could be replaced by `TeX-in-comment'.
		 (when (save-match-data
			 (re-search-backward comment-start-skip
					     (line-beginning-position) t))
		   ;; We need a t for comparison with `orig-comment-flag',
		   ;; not a number.
		   t)))
	    ;; Only consider matching alternatives with respect to
	    ;; "in-commentness", i.e. if we started with a comment
	    ;; only consider matches which are in comments as well and
	    ;; vice versa.
	    (when (eq orig-comment-flag current-comment-flag)
	      (setq env (buffer-substring-no-properties
			 (match-beginning 2) (match-end 2)))
	      (cond ((string= (match-string 1) "end")
		     (setq end-list (cons env end-list)))
		    ((equal env (car end-list))
		     (setq end-list (cdr end-list)))
		    ((member env texmathp-environments)
		     (throw 'exit (cons env (point))))))))
	nil))))

(defun texmathp-match-macro (bound)
  "Find out if point is within the arguments of any of the Math macros.
Limit searches to BOUND.  The return value is like (\"\\macro\" . (point))."
  (catch 'exit
    (and (null texmathp-macros) (throw 'exit nil))
    (let (pos cmd (syntax-table (syntax-table)))
      (unwind-protect
	  (save-restriction
	    (save-excursion
	      (set-syntax-table texmathp-syntax-table)
	      (narrow-to-region (max 1 bound) (point))
	      ;; Move back out of the current parenthesis
	      (while (condition-case nil (progn (up-list -1) t) (error nil))
		;; Move back over any touching sexps (in fact also non-touching)
		(while
		    (and
		     (cond
		      ((memq (preceding-char) '(?\] ?\})))
		      ((and
			texmathp-allow-detached-args
			(re-search-backward
			"[]}][ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*\\="
			bound t))
		       (goto-char (1+ (match-beginning 0))) t))
		     (if (eq (preceding-char) ?\})
			 ;; Jump back over {}
			 (condition-case nil
			     (progn (backward-sexp) t)
			   (error nil))
		       ;; Jump back over []. Modify syntax temporarily for this.
		       (unwind-protect
			   (progn
			     (modify-syntax-entry ?\{ ".")
			     (modify-syntax-entry ?\} ".")
			     (modify-syntax-entry ?\[ "(]")
			     (modify-syntax-entry ?\] ")[")
			     (condition-case nil
				 (progn (backward-sexp) t)
			       (error nil)))
			 (modify-syntax-entry ?\{ "(}")
			 (modify-syntax-entry ?\} "){")
			 (modify-syntax-entry ?\[ ".")
			 (modify-syntax-entry ?\] ".")
			 nil))))
		(setq pos (point))
		(and (memq (following-char) '(?\[ ?\{))
		     (re-search-backward "\\\\[*a-zA-Z]+\\=" nil t)
		     (setq cmd (buffer-substring-no-properties
				(match-beginning 0) (match-end 0)))
		     (member cmd texmathp-macros)
		     (throw 'exit (cons cmd (point))))
		(goto-char pos))
	      (throw 'exit nil)))
	(set-syntax-table syntax-table)))))

;;;###autoload
(defun texmathp-match-switch (bound)
  "Search backward for any of the math switches.
Limit searched to BOUND."
  ;; The return value is like ("\\(" . (point)).
  (save-excursion
    (if (re-search-backward texmathp-onoff-regexp bound t)
	(cons (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	      (match-beginning 1))
      nil)))

(provide 'texmathp)

;;; texmathp.el ends here
