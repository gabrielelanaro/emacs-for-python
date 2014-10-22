;; flyspell-babel.el -- Switch flyspell language according to LaTeX
;;                      Babel commands
;;
;; Copyright (C) 2004 P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/Software/Emacs
;; Version: 3.2
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; If you do not have a copy of the GNU General Public License, you
;; can obtain one by writing to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Installation:
;;
;; Flyspell is an Emacs package that highlights misspelled words as
;; you type; Babel is the standard mechanism for switching languages
;; in LaTeX.  There are a number of Emacs packages available that will
;; try to guess the current language of a buffer or part of a buffer,
;; and make flyspell switch to a different dictionary; but I didn't
;; find one that used the explicit language-switching commands
;; available in a LaTeX file for this purpose.  This package makes
;; flyspell use the correct dictionary for the language used in each
;; part of a LaTeX file.  There are some restrictions on the usage of
;; Babel commands, on which see below.
;;
;; flyspell-babel requires ispell-multi to be installed; it may be
;; found in the same directory indicated by the URL above.  It also
;; needs flyspell to be installed (version 1.7f or better) and
;; flyspell-mode to be active.  If you are using Emacs 21.3 or
;; earlier, you will need to upgrade to ispell.el version 3.6,
;; available from http://www.kdstevens.com/~stevens/ispell-page.html.
;;
;; To use this file, put it somewhere in your load-path, and add this
;; to your .emacs file:
;;
;;    (autoload 'flyspell-babel-setup "flyspell-babel")
;;    (add-hook 'latex-mode-hook 'flyspell-babel-setup)
;;
;; You will need to reload flyspell-babel.el if you install any new
;; ispell languages or language aliases.
;;
;; I have only tested this with GNU Emacs.

;;; Commentary:
;;
;; This package examines the LaTeX code around point to discover the
;; language of the current text, and then it caches that value in a
;; text-property.  This means that, if you modify or add a Babel
;; command to change the language of some text, the current language
;; may be out of sync with the cached value.  In this case, you can
;; just stop typing for a bit, and the surrounding text will be
;; re-parsed, and the new, correct language should be determined.  The
;; length of time that Emacs is idle before this re-parsing happens is
;; configurable via the variable flyspell-babel-delay (default is 5
;; seconds).
;;
;; The parsing done by this package has its limits limited, and so it
;; will not work with arbitrary LaTeX code.  I hope that these
;; restrictions will not in practice impinge on the typical usage of
;; most people.  The first language declaration is usually determined
;; by the final language option passed to the babel \usepackage
;; command, which takes effect after \begin{document}.  Thereafter,
;; you can switch the declared language with \selectlanguage
;; statements, otherlanguage environments, and \foreignlanguage
;; commands.  You can also define your own language-switching
;; commands, and register these with flyspell-babel.
;;
;; This package does not understand complex LaTeX constructs, such as
;; \input.  If you want to set the default language for a particular
;; file (for example, one that has no Babel declaration, but is going
;; to be \input into a file that does), you can just put a redundant
;; \selectlanguage declaration at the start of the file.  You can
;; limit the scope of a \selectlanguage declaration by putting an
;; opening brace immediately before it, and flyspell-babel will
;; respect that scoping, but not otherwise, since that would make the
;; task of parsing too complex.
;;
;; By default, an ispell dictionary is invoked with the same name as
;; the current Babel language or dialect, which works in many cases.
;; If your ispell has a different name for that language, you have two
;; options.  You can make ispell recognize the Babel name by adding
;; symlinks under that name in your Ispell directory.  Alternatively,
;; you can customize flyspell-babel-to-ispell-alist, which maps Babel
;; languages and dialects to Ispell language names.  If you map a
;; language to 'nil, that means not to spell-check that language,
;; which can be useful for languages without an ispell dictionary.

;;; Customization:
;;
;; The code that follows is an example of my customization of this
;; package.  The first form tells the package to turn on debugging
;; messages to see when we switch dictionaries as we move from place
;; to place.  The second tells it not to spell-check the languages
;; "latin" and "ibycus" (an encoding for ancient Greek), since I don't
;; have ispell dictionaries for them; it also tells it to translate
;; the Babel language "french" to the ispell dictionary "francais".
;; The third form defines some language-switching shortcut commands,
;; so that I can more easily say \fr{merci} and \itl{grazie}.  The
;; fourth defines some short-cut environments, since \begin{german} is
;; a lot easier to write than \begin{otherlanguage}{german}.  The last
;; form defines some shortcut declarations for switching between
;; American and British spelling.
;;
;;    (setq flyspell-babel-verbose t)
;;     
;;    (setq flyspell-babel-to-ispell-alist
;;          '(("latin" nil)
;;            ("ibycus" nil)
;;            ("french" "francais")))
;;     
;;    (setq flyspell-babel-command-alist
;;          '(("lat" "latin")
;;            ("gk" "ibycus")
;;            ("fr" "french")
;;            ("ger" "german")
;;            ("itl" "italian")))
;;     
;;    (setq flyspell-babel-environment-alist
;;          '(("latin" "latin")
;;            ("greek" "ibycus")
;;            ("french" "french")
;;            ("german" "german")
;;            ("italian" "italian")))
;;     
;;    (setq flyspell-babel-declaration-alist
;;          '(("yank" "american")
;;            ("brit" "british")))
;;
;; Here is the LaTeX code that defines these short-cuts:
;;
;; \usepackage[ibycus,latin,french,german,italian,british,american]{babel}
;;  
;; \newcommand{\lat}[1]{\foreignlanguage{latin}{\emph{#1}}}
;; \newenvironment{latin}{\begin{otherlanguage}{latin}}{\end{otherlanguage}}
;;  
;; \newcommand{\fr}[1]{\foreignlanguage{french}{\emph{#1}}}
;; \newenvironment{french}{\begin{otherlanguage}{french}}{\end{otherlanguage}}
;;  
;; \newcommand{\ger}[1]{\foreignlanguage{german}{\emph{#1}}}
;; \newenvironment{german}{\begin{otherlanguage}{german}}{\end{otherlanguage}}
;;  
;; \newcommand{\itl}[1]{\foreignlanguage{italian}{\emph{#1}}}
;; \newenvironment{italian}{\begin{otherlanguage}{italian}}{\end{otherlanguage}}
;;  
;; \newcommand{\yank}{\selectlanguage{american}}
;; \newcommand{\brit}{\selectlanguage{british}}

;;; Bugs:
;;
;; If a word comes to be erroneously highlighted as misspelled because
;; you have recently changed a Babel command, then the normal thing to
;; do is to wait a few seconds until the idle-timer runs the code to
;; re-parse the text.  This works, except when the new Babel language
;; is mapped to 'nil, meaning that it shouldn't be spell-checked at
;; all.  When this is the case, the spell-checking mechanism is
;; by-passed completely, so the erroneous highlighting will not be
;; removed, since those words will not be re-recognized as "correctly"
;; spelled.  The best thing to do in this case is to switch
;; flyspell-mode off and on again, which will remove all flyspell
;; highlighting.

;;; Changes
;;
;; 3.2 Made flyspell-babel-parse-block non-recursive to avoid blowing
;;     max-lisp-eval-depth in long files
;; 3.1 Fixed bug when \usepackage[foo]{babel} was commented out
;; 3.0 Re-factored to use new ispell-multi.el.
;; 2.0 Major re-write.  Instead of partially-parsing every time a word
;;     is spell-checked, we now cache the language in a text property,
;;     and use an idle timer to re-parse in case the language has
;;     changed.
;; 1.3 Only use ispell-valid-dictionary-list if it's available
;; 1.2 Removed dependency on AUCTeX and newcomment and fixed bug when
;;     disabling flyspell-large-region
;; 1.1 Removed error report when \usepackage{babel} not present
;; 1.0 Initial public release

(require 'ispell-multi)
(require 'flyspell)

(defgroup flyspell-babel nil
  "Switch flyspell language according to LaTeX babel commands"
  :tag "Switch flyspell language according to Babel commands"
  :group 'tex
  :prefix "flyspell-babel-")

(defcustom flyspell-babel-to-ispell-alist ()
  "Maps LaTeX babel language or dialect names to ispell
  dictionaries"
  :type 'alist
  :group 'flyspell-babel)

(defcustom flyspell-babel-declaration-alist ()
  "Maps LaTeX language-switching declarations (other than the
  built-in babel \\selectlanguage declaration) to babel
  languages" 
  :type 'alist
  :group 'flyspell-babel)

(defcustom flyspell-babel-environment-alist ()
  "Maps LaTeX language-switching environments (other than the
  built-in babel \"otherlanguage\" environment) to babel languages"
  :type 'alist
  :group 'flyspell-babel)

(defcustom flyspell-babel-command-alist ()
  "Maps LaTeX language-switching commands (other than the
  built-in babel \\foreignlanguage command) to babel languages"
  :type 'alist
  :group 'flyspell-babel)

(defcustom flyspell-babel-verbose nil
  "Whether routinely to report changing from one language to another"
  :type 'boolean
  :group 'flyspell-babel)

(defcustom flyspell-babel-delay 5
  "Seconds of idleness before current Babel block is re-parsed."
  :type 'integer
  :group 'flyspell-babel)


(defvar flyspell-babel-declaration-alist-all nil)
(defvar flyspell-babel-decl-regexp nil)
(defvar flyspell-babel-environment-alist-all nil)
(defvar flyspell-babel-env-regexp nil)
(defvar flyspell-babel-command-alist-all nil)
(defvar flyspell-babel-com-regexp nil)
(defvar flyspell-babel-decl-env-com-regexp nil)

(setq flyspell-babel-declaration-alist-all
      (append '(("selectlanguage" "selectlanguage"))
	      flyspell-babel-declaration-alist))

(setq flyspell-babel-decl-regexp
      (concat "\\\\begin[ \t\n]*{document}" "\\|"
	      (mapconcat (lambda (pair) (concat "\\\\" (car pair) "[ \t\n{]"))
			 flyspell-babel-declaration-alist-all "\\|")))

(setq flyspell-babel-environment-alist-all
      (append '(("otherlanguage" "otherlanguage"))
	      flyspell-babel-environment-alist))

(setq flyspell-babel-env-regexp
      (mapconcat (lambda (pair) (concat "\\\\begin{" (car pair) "}"))
		 flyspell-babel-environment-alist-all "\\|"))

(setq flyspell-babel-command-alist-all
      (append '(("foreignlanguage" "foreignlanguage"))
	      flyspell-babel-command-alist))

(setq flyspell-babel-com-regexp
       (mapconcat (lambda (pair) (concat "\\\\" (car pair) "[ \t\n{]"))
		  flyspell-babel-command-alist-all "\\|"))

(setq flyspell-babel-decl-env-com-regexp
       (mapconcat 'identity (list flyspell-babel-decl-regexp 
				  flyspell-babel-env-regexp
				  flyspell-babel-com-regexp) "\\|"))


(defun flyspell-babel-parse-buffer ()
  (save-excursion
    (goto-char (point-max))
    (flyspell-babel-parse)))
    
(defun flyspell-babel-parse ()
  "Parse backward from point until containing element or bob is
found; then run forward and flag regions found."
  (let ((current-position (point))
        beg end lang macro-begin tag-list finished)
    (while (and (not finished)
                (not (input-pending-p))
                (flyspell-babel-find-previous-macro nil))
      ;; We standardize on having the language switch happening
      ;; just *after* regexp-match, since re-search-backward
      ;; will fail to match when we are in the middle of a macro.
      (setq beg (match-end 0))
      (setq macro-begin (point))
      (if (looking-at flyspell-babel-com-regexp)
          (flyspell-babel-check-com)
        (if (looking-at flyspell-babel-env-regexp)
            (flyspell-babel-check-env)
          (if (looking-at flyspell-babel-decl-regexp)
              (flyspell-babel-check-decl)
            (flyspell-babel-message "internal error 1"))))
      (setq end (point))
      (when (< current-position end)
        ;; Got it
        (setq finished t)
        ;; As an optimization, we flag the text after point
        ;; as far as the start of the next Babel command.
        (goto-char current-position)
        (when (and (flyspell-babel-find-next-macro end)
                   (< current-position (point))
                   (< (point) end))
          (setq end (1+ (point)))))
      (setq tag-list (cons (list beg end lang) tag-list))
      (goto-char macro-begin))
    ;; Background is default lang
    (setq tag-list (cons (list (point-min) current-position "no-command-found") tag-list))
    ;; We now run through in a forward direction
    (while tag-list
      (apply 'flyspell-babel-flag-region (car tag-list))
      (setq tag-list (cdr tag-list)))))



(defun flyspell-babel-find-previous-macro (end)
  (let ((found))
    (while (and (not found)
                (re-search-backward flyspell-babel-decl-env-com-regexp end t))
      (setq found (not (flyspell-babel-in-comment-p))))
    found))
                
(defun flyspell-babel-find-next-macro (end)
  (let ((found))
    (while (and (not found)
                (re-search-forward flyspell-babel-decl-env-com-regexp end t))
      (setq found (not (flyspell-babel-in-comment-p))))
    found))

(defun flyspell-babel-check-com ()
  (if (re-search-forward
       "\\=\\\\foreignlanguage[ \t\n]*{\\([^}]+\\)}[ \t\n]*{" nil t)
      (setq lang (match-string 1))
    (if (re-search-forward "\\=\\\\\\([^{ \t\n]+\\)[ \t\n]*{" nil t)
        (setq lang (cadr
                    (assoc (match-string 1)
                           flyspell-babel-command-alist-all)))
      (flyspell-babel-message "internal error 2")))
  (backward-char)
  (flyspell-babel-forward-sexp))

(defun flyspell-babel-check-env ()
  (let ((env))
    (if (looking-at "\\=\\\\begin{otherlanguage}[ \t\n]*{\\([^}]+\\)}")
        (setq env "otherlanguage" lang (match-string 1))
      (if (looking-at "\\=\\\\begin[ \t\n]*{\\([^}]+\\)}")
          (setq env (match-string 1)
                lang (cadr
                      (assoc env flyspell-babel-environment-alist-all)))
        (flyspell-babel-message "internal error 3")))
    (flyspell-babel-find-matching-end env)
;    (backward-char)))
    ))

(defun flyspell-babel-check-decl ()
  (if (looking-at "\\\\begin[ \t\n]*{document}")
      (progn
        (if (save-excursion
              (re-search-backward
               ;; To exclude commented lines, only allow spaces before
               "^[ \t\n]*\\\\usepackage.*[[,]\\([^]]+\\)\\]{babel}" nil t))
            (setq lang (match-string 1))
          (setq lang "no-command-found"))
        (goto-char (point-max)))
    (if (looking-at "\\\\selectlanguage[ \t\n]*{\\([^}]+\\)}")
        (setq lang (match-string 1))
      (if (and (looking-at "\\\\\\([^{ \t\n]+\\)")
               (cadr (assoc (match-string 1)
                            flyspell-babel-declaration-alist-all)))
          (setq lang
                (cadr (assoc (match-string 1)
                             flyspell-babel-declaration-alist-all)))
        (flyspell-babel-message "internal error 4")))
    (unless (bobp)
      (backward-char))
    (if (looking-at "{")
        (flyspell-babel-forward-sexp)
      (goto-char (point-max)))))

(defun flyspell-babel-flag-region (beg end lang)
  (let ((trans (assoc lang flyspell-babel-to-ispell-alist))
        (buffer-modified-before (buffer-modified-p))
        (after-change-functions nil))
    (when trans
      ;; We have a translation of a Babel language name to ispell
      ;; nomenclature
      (setq lang (cadr trans)))
    (cond
     ((not lang)
      ;; A parsed region with a nil dict -- don't spell-check.
      (setq lang "void"))
     ((equal lang "no-command-found")
      (setq lang "default"))
     ((and ispell-multi-valid-dictionary-list
           (not (member lang ispell-multi-valid-dictionary-list)))
      ;; A parsed region with an uninstalled dict
      (message "Flyspell-babel warning: no dictionary installed for %s" lang)
      (setq lang "void")))
    (put-text-property beg end 'ispell-multi-lang lang)
    (set-buffer-modified-p buffer-modified-before)))


(defun flyspell-babel-forward-sexp (&optional arg)
  "Makes sure to ignore comments when using forward-sexp, and
  trap errors for unbalanced braces."
  (interactive "p")
  (let ((parse-sexp-ignore-comments t))
    (condition-case nil 
	(forward-sexp arg)
      (error (goto-char (point-max))))))

(defun flyspell-babel-find-matching-end (env)
  "Find end of current environment, or end of file when there is
  no matching \end."
  (interactive)
  (let ((regexp (concat "\\\\\\(begin\\|end\\)[ \t\n]*{" env "}"))
	(level 0)
	(proceed t))
    (while proceed
      (if (re-search-forward regexp nil t)
	  (let ((match (match-string 1)))
	    (unless (flyspell-babel-in-comment-p)
	      (if (string= match "begin")
		  (setq level (1+ level))
		(if (string= match "end")
		    (setq level (1- level))
		  (flyspell-babel-message "internal error 5")))))
	(goto-char (point-max))
	(setq proceed nil))
      (when (= 0 level)
	(setq proceed nil)))))

(defun flyspell-babel-in-comment-p ()
  "Are we in a Latex comment? (Stolen from auctex tex.el)"
  (save-match-data
    (if (or (bolp)
            (null comment-start-skip)
            (eq (preceding-char) ?\r))
        nil
      (save-excursion
        (let ((pos (point)))
          (re-search-backward "^\\|\r" nil t)
          (or (looking-at comment-start-skip)
              (re-search-forward comment-start-skip pos t)))))))

(defun flyspell-babel-message (mess &optional force)
  (when (or flyspell-babel-verbose force)
    (message "Flyspell-babel -- %s" mess)))

(defun flyspell-babel-start ()
  (flyspell-babel-parse-buffer)
  (setq ispell-multi-nil-callback 'flyspell-babel-parse)
  (make-local-variable 'flyspell-large-region)
  (setq flyspell-large-region 'nil)
  (flyspell-mode 1)
  (setq flyspell-generic-check-word-p 'ispell-multi-verify)
  (setq ispell-multi-idler-callback 'flyspell-babel-parse-buffer)
  (ispell-multi-idler-setup flyspell-babel-delay)
  (ispell-multi-hack-flyspell-modeline))

(defun flyspell-babel-stop ()
;  (ispell-multi-idler-cancel)
  (setq flyspell-generic-check-word-p nil)
  (ispell-multi-unhack-flyspell-modeline)
  (flyspell-mode -1))

(define-minor-mode flyspell-babel-mode
  "Mode to make flyspell language selection obey LaTeX Babel commands" nil 
      :group 'flyspell-babel
      (if flyspell-babel-mode
          (flyspell-babel-start)
	(flyspell-babel-stop)))

(defun flyspell-babel-setup ()
  (flyspell-babel-mode 1))

(provide 'flyspell-babel)

