;;; tex.el --- Support for TeX documents.

;; Copyright (C) 1985-1987, 1991, 1993, 1994, 1996, 1997, 1999-2012
;;   Free Software Foundation, Inc.

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

;; This file provides basic functions used by the AUCTeX modes.

;;; Code:

(when (< emacs-major-version 21)
  (error "AUCTeX requires Emacs 21 or later"))

(require 'custom)
(require 'tex-site)
(eval-when-compile
  (require 'cl))

(defun TeX--call-3/2 (f arg1 arg2 arg3)
  (condition-case nil
      (funcall f arg1 arg2 arg3)
    (wrong-number-of-arguments (funcall f arg1 arg2))))

(defgroup TeX-file nil
  "Files used by AUCTeX."
  :group 'AUCTeX)

(defgroup TeX-command nil
  "Calling external commands from AUCTeX."
  :group 'AUCTeX)

(defgroup LaTeX nil
  "LaTeX support in AUCTeX."
  :tag "LaTeX"
  :group 'AUCTeX
  :prefix "LaTeX-")

(defgroup TeX-misc nil
  "Various AUCTeX settings."
  :group 'AUCTeX)

;;; Site Customization
;;
;; The following variables are likely to need to be changed for your
;; site.  You should do this with customize.

(defcustom TeX-command "tex"
  "Command to run plain TeX."
  :group 'TeX-command
  :type 'string)

(defcustom TeX-Omega-command "omega"
  "Command to run plain TeX on Omega."
  :group 'TeX-command
  :type 'string)

(defcustom LaTeX-command "latex"
  "Command to run LaTeX."
  :group 'TeX-command
  :type 'string)

(defcustom LaTeX-Omega-command "lambda"
  "Command to run LaTeX on Omega."
  :group 'TeX-command
  :type 'string)

(defcustom ConTeXt-engine nil
  "Engine to use for --engine in the texexec command.
If nil, none is specified."
  :group 'TeX-command
  :type '(choice (const :tag "Unspecified" nil)
		 string))

(defcustom ConTeXt-Omega-engine TeX-Omega-command
  "Engine to use for --engine in the texexec command in Omega mode.
If nil, none is specified."
  :group 'TeX-command
  :type '(choice (const :tag "Unspecified" nil)
		 string))
;; At least in TeXLive 2009 ConTeXt does not support an omega option anymore.
(TeX--call-3/2 #'make-obsolete-variable 'ConTeXt-Omega-engine
               'TeX-engine-alist "before 11.86")

(defcustom TeX-mode-hook nil
  "A hook run in TeX mode buffers."
  :type 'hook
  :group 'TeX-misc)

(defcustom AmS-TeX-mode-hook nil
  "A hook run in AmS-TeX mode buffers."
  :type 'hook
  :group 'TeX-misc)

;; This is the major configuration variable.  Most sites will only
;; need to change the second string in each entry, which is the name
;; of a command to send to the shell.  If you use other formatters
;; like AMSLaTeX or AMSTeX, you can add those to the list.  See
;; TeX-expand-list for a description of the % escapes

(defcustom TeX-command-list
  `(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil
     (plain-tex-mode ams-tex-mode texinfo-mode) :help "Run plain TeX")
    ("LaTeX" "%`%l%(mode)%' %t"
     TeX-run-TeX nil
     (latex-mode doctex-mode) :help "Run LaTeX")
	;; Not part of standard TeX.
    ("Makeinfo" "makeinfo %t" TeX-run-compile nil
     (texinfo-mode) :help "Run Makeinfo with Info output")
    ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
     (texinfo-mode) :help "Run Makeinfo with HTML output")
    ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX")
    ;; support for ConTeXt  --pg
    ;; first version of ConTeXt to support nonstopmode: 2003.2.10
    ("ConTeXt" "texexec --once --texutil %(execopts)%t"
     TeX-run-TeX nil (context-mode) :help "Run ConTeXt once")
    ("ConTeXt Full" "texexec %(execopts)%t"
     TeX-run-TeX nil
     (context-mode) :help "Run ConTeXt until completion")
    ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
    ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
    ,(if (or window-system (getenv "DISPLAY"))
	'("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
       '("View" "dvi2tty -q -w 132 %s" TeX-run-command t t
	 :help "Run Text viewer"))
    ("Print" "%p" TeX-run-command t t :help "Print the file")
    ("Queue" "%q" TeX-run-background nil t :help "View the printer queue"
     :visible TeX-queue-command)
    ("File" "%(o?)dvips %d -o %f " TeX-run-command t t
     :help "Generate PostScript file")
    ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
    ("Check" "lacheck %s" TeX-run-compile nil (latex-mode)
     :help "Check LaTeX file for correctness")
    ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t
     :help "Spell-check the document")
    ("Clean" "TeX-clean" TeX-run-function nil t
     :help "Delete generated intermediate files")
    ("Clean All" "(TeX-clean t)" TeX-run-function nil t
     :help "Delete generated intermediate and output files")
    ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))
  "List of commands to execute on the current document.

Each element is a list, whose first element is the name of the command
as it will be presented to the user.

The second element is the string handed to the shell after being
expanded.  The expansion is done using the information found in
`TeX-expand-list'.

The third element is the function which actually start the process.
Several such hooks has been defined:

TeX-run-command: Start up the process and show the output in a
separate buffer.  Check that there is not two commands running for the
same file.  Return the process object.

TeX-run-format: As `TeX-run-command', but assume the output is created
by a TeX macro package.  Return the process object.

TeX-run-TeX: For TeX output.

TeX-run-interactive: Run TeX or LaTeX interactively.

TeX-run-BibTeX: For BibTeX output.

TeX-run-Biber: For Biber output.

TeX-run-compile: Use `compile' to run the process.

TeX-run-shell: Use `shell-command' to run the process.

TeX-run-discard: Start the process in the background, discarding its
output.

TeX-run-background: Start the process in the background, show output
in other window.

TeX-run-silent: Start the process in the background.

TeX-run-discard-foreground: Start the process in the foreground,
discarding its output.

TeX-run-function: Execute the Lisp function or function call
specified by the string in the second element.  Consequently,
this hook does not start a process.

TeX-run-discard-or-function: If the command is a Lisp function,
execute it as such, otherwise start the command as a process,
discarding its output.

To create your own hook, define a function taking three arguments: The
name of the command, the command string, and the name of the file to
process.  It might be useful to use `TeX-run-command' in order to
create an asynchronous process.

If the fourth element is non-nil, the user will get a chance to
modify the expanded string.

The fifth element indicates in which mode(s) the command should be
present in the Command menu.  Use t if it should be active in any
mode.  If it should only be present in some modes, specify a list with
the respective mode names.

Any additional elements get just transferred to the respective menu entries."
  :group 'TeX-command
  :type '(repeat (group :value ("" "" TeX-run-command nil t)
			(string :tag "Name")
			(string :tag "Command")
			(choice :tag "How"
				:value TeX-run-command
				(function-item TeX-run-command)
				(function-item TeX-run-format)
				(function-item TeX-run-TeX)
				(function-item TeX-run-interactive)
				(function-item TeX-run-BibTeX)
				(function-item TeX-run-Biber)
				(function-item TeX-run-compile)
				(function-item TeX-run-shell)
				(function-item TeX-run-discard)
				(function-item TeX-run-background)
				(function-item TeX-run-silent)
				(function-item TeX-run-discard-foreground)
				(function-item TeX-run-function)
				(function-item TeX-run-discard-or-function)
				(function :tag "Other"))
			(boolean :tag "Prompt")
			(choice :tag "Modes"
				(const :tag "All" t)
				(set (const :tag "Plain TeX" plain-tex-mode)
				     (const :tag "LaTeX" latex-mode)
				     (const :tag "DocTeX" doctex-mode)
				     (const :tag "ConTeXt" context-mode)
				     (const :tag "Texinfo" texinfo-mode)
				     (const :tag "AmSTeX" ams-tex-mode)))
			(repeat :tag "Menu elements" :inline t sexp))))

(defcustom TeX-command-output-list
  '(
; Add the following line if you want to use htlatex (tex4ht)
;    ("\\`htlatex" ("html"))
    )
  "List of regexps and file extensions.

Each element is a list, whose first element is a regular expression to
match against the name of the command that will be used to process the TeX
file.

The second element is either a string or a list with a string as element.
If it is a string this is the default file extension that will be expected
for output files that are produced by commands that match the first
element.  The real file extension will be obtained from the logging output
if possible, defaulting to the given string.
If it is a list, the element of the list will be the fixed extension used
without looking at the logging output.

If this list does not yield an extension, the default is either \"dvi\"
or \"pdf\", depending on the setting of `TeX-PDF-mode'.
Extensions must be given without the \".\"."

  :group 'TeX-command
  :type '(repeat (group (regexp :tag "Command Regexp")
			(choice (string :tag "Default Extension")
				(group (string :tag "Fixed Extension"))))))

;; You may want to change the default LaTeX version for your site.
(defcustom LaTeX-version "2e"
  "Default LaTeX version.  Currently recognized is \"2\" and \"2e\"."
  :group 'LaTeX
  :type '(radio (const :format "%v\n%h"
		       :doc "\
The executable `latex' is LaTeX version 2."
		       "2")
		(const :format "%v\n%h"
		       :doc "\
The executable `latex' is LaTeX version 2e."
		       "2e")
		(string :tag "Other")))


;; Use different compilation commands depending on style.
;; Only works if parsing is enabled.

(defcustom LaTeX-command-style
  ;; They have all been combined in LaTeX 2e.
  '(("" "%(PDF)%(latex) %S%(PDFout)"))
"List of style options and LaTeX commands.

If the first element (a regular expression) matches the name of one of
the style files, any occurrence of the string `%l' in a command in
`TeX-command-list' will be replaced with the second element.  The first
match is used, if no match is found the `%l' is replaced with the empty
string."
  :group 'TeX-command
  :type '(repeat (group :value ("" "")
			regexp (string :tag "Style"))))

;; Printing: If you want to print, TeX-print-command must be non-nil
;; (if it is nil, you'll get a complaint when using the print menu).
;; If you want to view the queue, TeX-queue-command needs to be
;; non-nil (if it is nil, it won't get mentioned in the menu).  If
;; TeX-printer-list is nil, nothing else gets asked: the menu entries
;; lead directly to the respective commands.  If those commands
;; contain %p, the value of TeX-printer-default gets inserted there,
;; no questions asked.  Now if TeX-printer-list is non-nil, you'll
;; always get asked which printer you want to use.  You can enter a
;; configured printer from TeX-printer-list, or an unknown one.  The
;; respective menus will show all configured printers.  Since you can
;; enter unknown printers, the printer name _must_ be set with %p in
;; TeX-print-command.

(defcustom TeX-print-command
  "{ test -e %s.dvi && %(o?)dvips -P%p %r %s; } || lpr -P%p %o"
  "Command used to print a file.

First `%p' is expanded to the printer name, then ordinary expansion is
performed as specified in `TeX-expand-list'.  If it is nil,
then customization is requested."
  :group 'TeX-command
  :type '(choice (string :tag "Print command")
		 (const :tag "No print command customized" nil)))

(defcustom TeX-queue-command "lpq -P%p"
  "Command used to show the status of a printer queue.

First `%p' is expanded to the printer name, then ordinary expansion is
performed as specified in `TeX-expand-list'.  If this is nil,
the printer has no corresponding command."
  :group 'TeX-command
  :type '(choice (string :tag "Queue check command")
		 (const :tag "No such command" nil)))

;; Enter the names of the printers available at your site, or nil if
;; you only have one printer.

(defcustom TeX-printer-list
  '(("Default"
     ;; Print to the (unnamed) default printer.  If there is a DVI
     ;; file print via Dvips.  If not, pass the output file (which
     ;; should then be a Postscript or PDF file) directly to lpr.
     "{ test -e %s.dvi && %(o?)dvips -f %r %s | lpr; } || lpr %o"
     ;; Show the queue for the (unnamed) default printer.
     "lpq"))
  "List of available printers.

The first element of each entry is the printer name.

The second element is the command used to print to this
printer.  It defaults to the value of `TeX-print-command' when nil.

The third element is the command used to examine the print queue for
this printer.  It defaults to the value of `TeX-queue-command' similarly.

Any occurrence of `%p' in the second or third element is expanded to
the printer name given in the first element, then ordinary expansion
is performed as specified in `TeX-expand-list'.

If this list is empty, only `TeX-print-command' and `TeX-queue-command'
get consulted."
  :group 'TeX-command
  :type '(repeat (group (string :tag "Name")
			(option (group :inline t
				       :extra-offset -4
				       (choice :tag "Print"
					       (const :tag "default")
					       (string :format "%v"))
				       (option (choice :tag "Queue"
						       (const :tag "default")
						       (string
							:format "%v"))))))))

;; The name of the most used printer.

(defcustom TeX-printer-default (or (getenv "PRINTER")
				   (and TeX-printer-list
					(car (car TeX-printer-list)))
				   "lp")
  "Default printer to use with `TeX-command'."
  :group 'TeX-command
  :type 'string)

(defcustom TeX-print-style '(("^landscape$" "-t landscape"))
  "List of style options and print options.

If the first element (a regular expression) matches the name of one of
the style files, any occurrence of the string `%r' in a command in
`TeX-command-list' will be replaced with the second element.  The first
match is used, if no match is found the `%r' is replaced with the empty
string."
  :group 'TeX-command
  :type '(repeat (group regexp (string :tag "Command"))))

;; This is the list of expansion for the commands in
;; TeX-command-list.  Not likely to be changed, but you may e.g. want
;; to handle .ps files.

(defcustom TeX-expand-list
  '(("%p" TeX-printer-query)	;%p must be the first entry
    ("%q" (lambda ()
	    (TeX-printer-query t)))
    ("%V" (lambda ()
	    (TeX-source-correlate-start-server-maybe)
	    (TeX-view-command-raw)))
    ("%vv" (lambda ()
	    (TeX-source-correlate-start-server-maybe)
	    (TeX-output-style-check TeX-output-view-style)))
    ("%v" (lambda ()
	    (TeX-source-correlate-start-server-maybe)
	    (TeX-style-check TeX-view-style)))
    ("%r" (lambda ()
	    (TeX-style-check TeX-print-style)))
    ("%l" (lambda ()
	    (TeX-style-check LaTeX-command-style)))
    ("%(PDF)" (lambda ()
		(if (and (eq TeX-engine 'default)
			 (or TeX-PDF-mode
			     TeX-DVI-via-PDFTeX))
		    "pdf"
		  "")))
    ("%(PDFout)" (lambda ()
		   (cond ((and (eq TeX-engine 'xetex)
			       (not TeX-PDF-mode))
			  " -no-pdf")
			 ((and (eq TeX-engine 'luatex)
			       (not TeX-PDF-mode))
			  " --output-format=dvi")
			 ((and (eq TeX-engine 'default)
			       (not TeX-PDF-mode)
			       TeX-DVI-via-PDFTeX)
			  " \"\\pdfoutput=0 \"")
			 (t ""))))
    ("%(mode)" (lambda ()
		 (if TeX-interactive-mode
		     ""
		   " -interaction=nonstopmode")))
    ("%(o?)" (lambda () (if (eq TeX-engine 'omega) "o" "")))
    ("%(tex)" (lambda () (eval (nth 2 (assq TeX-engine (TeX-engine-alist))))))
    ("%(latex)" (lambda () (eval (nth 3 (assq TeX-engine (TeX-engine-alist))))))
    ("%(execopts)" ConTeXt-expand-options)
    ("%S" TeX-source-correlate-expand-options)
    ("%dS" TeX-source-specials-view-expand-options)
    ("%cS" TeX-source-specials-view-expand-client)
    ("%(outpage)" (lambda ()
		    (or (when TeX-source-correlate-output-page-function
			  (funcall TeX-source-correlate-output-page-function))
			"1")))
    ;; `file' means to call `TeX-master-file' or `TeX-region-file'
    ("%s" file nil t)
    ("%t" file t t)
    ("%`" (lambda nil
	    (setq TeX-command-pos t TeX-command-text "")))
    (" \"\\" (lambda nil
	       (if (eq TeX-command-pos t)
		   (setq TeX-command-pos pos
			 pos (+ 3 pos))
		 (setq pos (1+ pos)))))
    ("\"" (lambda nil (if (numberp TeX-command-pos)
			  (setq TeX-command-text
				(concat
				 TeX-command-text
				 (substring command
					    TeX-command-pos
					    (1+ pos)))
				command
				(concat
				 (substring command
					    0
					    TeX-command-pos)
				 (substring command
					    (1+ pos)))
				pos TeX-command-pos
				TeX-command-pos t)
			(setq pos (1+ pos)))))
    ("%'" (lambda nil
	    (prog1
		(if (stringp TeX-command-text)
		    (progn
		      (setq pos (+ (length TeX-command-text) 9)
			    TeX-command-pos
			    (and (string-match " "
					      (funcall file t t))
				 "\""))
		      (concat TeX-command-text " \"\\input\""))
		  (setq TeX-command-pos nil)
		  "")
	      (setq TeX-command-text nil))))
    ("%n" TeX-current-line)
    ("%d" file "dvi" t)
    ("%f" file "ps" t)
    ("%o" (lambda nil (funcall file (TeX-output-extension) t)))
    ;; for source specials the file name generated for the xdvi
    ;; command needs to be relative to the master file, just in
    ;; case the file is in a different subdirectory
    ("%b" TeX-current-file-name-master-relative)
    ;; the following is for preview-latex.
    ("%m" preview-create-subdirectory))
  "List of expansion strings for TeX command names.

Each entry is a list with two or more elements.  The first element is
the string to be expanded.  The second element is the name of a
function returning the expanded string when called with the remaining
elements as arguments.  The special value `file' will be expanded to
the name of the file being processed, with an optional extension."
  :group 'TeX-command
  :type '(repeat (group (string :tag "Key")
			(sexp :tag "Expander")
			(repeat :inline t
				:tag "Arguments"
				(sexp :format "%v")))))


;; The following dependencies are not done with autoload cookies since
;; they are only useful when tex.el is loaded, anyway.  tex-buf.el
;; should remain unloaded as long as one is only editing files, so
;; requiring it here would be wrong.

(autoload 'TeX-region-create "tex-buf" nil nil)
(autoload 'TeX-save-document "tex-buf" nil t)
(autoload 'TeX-home-buffer "tex-buf" nil t)
(autoload 'TeX-pin-region "tex-buf" nil t)
(autoload 'TeX-command-region "tex-buf" nil t)
(autoload 'TeX-command-buffer "tex-buf" nil t)
(autoload 'TeX-command-master "tex-buf" nil t)
(autoload 'TeX-command "tex-buf" nil nil)
(autoload 'TeX-kill-job "tex-buf" nil t)
(autoload 'TeX-recenter-output-buffer "tex-buf" nil t)
(autoload 'TeX-next-error "tex-buf" nil t)
(autoload 'TeX-region-file "tex-buf" nil nil)
(autoload 'TeX-current-offset "tex-buf" nil nil)
(autoload 'TeX-process-set-variable "tex-buf" nil nil)
(autoload 'TeX-view "tex-buf" nil t)

;;; Portability.

(require 'easymenu)

(eval-and-compile
  (if (featurep 'xemacs)
      (defun TeX-maybe-remove-help (menu)
      "Removes :help entries from menus, since XEmacs does not like them.
Also does other stuff."
      (cond ((consp menu)
	     (cond ((eq (car menu) :help)
		    (TeX-maybe-remove-help (cddr menu)))
		   ((eq (car menu) :visible)
		    (cons :included
			  (cons (cadr menu)
				(TeX-maybe-remove-help (cddr menu)))))
		   (t (cons (TeX-maybe-remove-help (car menu))
			    (TeX-maybe-remove-help (cdr menu))))))
	    ((vectorp menu)
	     (vconcat (TeX-maybe-remove-help (append menu nil))))
	    (t menu)))
    (defun TeX-maybe-remove-help (menu)
      "Compatibility function that would remove :help entries if on XEmacs,
but does nothing in Emacs."
      menu))
  (defmacro TeX-menu-with-help (menu)
    "Compatibility macro that removes :help entries if on XEmacs.
Also does other stuff."
    (TeX-maybe-remove-help menu)))


;;; Documentation for Info-goto-emacs-command-node and similar

(eval-after-load 'info '(dolist (elt '("TeX" "LaTeX" "ConTeXt" "Texinfo"
				       "docTeX"))
			  (add-to-list 'Info-file-list-for-emacs
				       (cons elt "AUCTeX"))))

(defadvice hack-one-local-variable (after TeX-hack-one-local-variable-after
					  activate)
  "Call minor mode function if minor mode variable is found."
  (let ((var (ad-get-arg 0))
	(val (ad-get-arg 1)))
    ;; Instead of checking for each mode explicitely `minor-mode-list'
    ;; could be used.  But this may make the byte compiler pop up.
    (when (memq var '(TeX-PDF-mode
		      TeX-source-correlate-mode TeX-interactive-mode
		      TeX-fold-mode LaTeX-math-mode))
      (if (symbol-value val) (funcall var 1) (funcall var 0)))))

(defvar TeX-overlay-priority-step 16
  "Numerical difference of priorities between nested overlays.
The step should be big enough to allow setting a priority for new
overlays between two existing ones.")


;;; Special support for XEmacs

(when (featurep 'xemacs)

  (defun TeX-read-string
    (prompt &optional initial-input history default-value)
    (condition-case nil
	(read-string prompt initial-input history default-value t)
      (wrong-number-of-arguments
       (read-string prompt initial-input history default-value))))

  (defun TeX-mark-active ()
    ;; In Lucid (mark) returns nil when not active.
    (if zmacs-regions
	(mark)
      (mark t)))

  (defun TeX-active-mark ()
    (and zmacs-regions (mark)))

  (fset 'TeX-activate-region (symbol-function 'zmacs-activate-region))

  ;; I am aware that this counteracts coding conventions but I am sick
  ;; of it.
  (unless (fboundp 'line-beginning-position)
    (defalias 'line-beginning-position 'point-at-bol))
  (unless (fboundp 'line-end-position)
    (defalias 'line-end-position 'point-at-eol))

  (defun TeX-overlay-prioritize (start end)
    "Calculate a priority for an overlay extending from START to END.
The calculated priority is lower than the minimum of priorities
of surrounding overlays and higher than the maximum of enclosed
overlays."
    (let (inner-priority outer-priority
			 (prios (cons nil nil)))
      (map-extents
       #'(lambda (ov prios)
	   (and
	    (or (eq (extent-property ov 'category) 'TeX-fold)
		(extent-property ov 'preview-state))
	    (setcar prios
		    (max (or (car prios) 0)
			 (extent-property ov 'priority))))
	   nil)
       nil start end prios 'start-and-end-in-region 'priority)
      (map-extents
       #'(lambda (ov prios)
	   (and
	    (or (eq (extent-property ov 'category) 'TeX-fold)
		(extent-property ov 'preview-state))
	    (setcdr prios
		    (min (or (cdr prios) most-positive-fixnum)
			 (extent-property ov 'priority))))
	   nil)
       nil start end prios
       '(start-and-end-in-region negate-in-region) 'priority)
      (setq inner-priority (car prios) outer-priority (cdr prios))
      (cond ((and inner-priority (not outer-priority))
	     (+ inner-priority TeX-overlay-priority-step))
	    ((and (not inner-priority) outer-priority)
	     (/ outer-priority 2))
	    ((and inner-priority outer-priority)
	     (+ (/ (- outer-priority inner-priority) 2) inner-priority))
	    (t TeX-overlay-priority-step)))) )


(if (fboundp 'completing-read-multiple)
    (defalias 'TeX-completing-read-multiple 'completing-read-multiple)
  (defun TeX-completing-read-multiple
    (prompt table &optional predicate require-match initial-input
	    hist def inherit-input-method)
    "Poor mans implementation of Emacs' `completing-read-multiple' for XEmacs.
The XEmacs package edit-utils-2.32 includes `crm.el'."
    (multi-prompt "," nil prompt table predicate require-match initial-input
		  hist)))

(if (fboundp 'line-number-at-pos)
    (defalias 'TeX-line-number-at-pos 'line-number-at-pos)
  ;; `line-number-at-pos' from `simple.el' in Emacs CVS (2006-06-07)
  (defun TeX-line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
	(goto-char (point-min))
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines start (point)))))))

;;; Special support for GNU Emacs

(unless (featurep 'xemacs)

  (defun TeX-read-string (prompt &optional initial-input history default-value)
    (read-string prompt initial-input history default-value t))
  
  (defun TeX-mark-active ()
    ;; In FSF 19 mark-active indicates if mark is active.
    mark-active)

  (defun TeX-active-mark ()
    (and transient-mark-mode mark-active))

  (defun TeX-activate-region ()
    nil)

  (defun TeX-overlay-prioritize (start end)
    "Calculate a priority for an overlay extending from START to END.
The calculated priority is lower than the minimum of priorities
of surrounding overlays and higher than the maximum of enclosed
overlays."
    (let (outer-priority inner-priority ov-priority)
      (dolist (ov (overlays-in start end))
	(when (or (eq (overlay-get ov 'category) 'TeX-fold)
		  (overlay-get ov 'preview-state))
	  (setq ov-priority (overlay-get ov 'priority))
	  (if (>= (overlay-start ov) start)
	      (setq inner-priority (max ov-priority (or inner-priority
							ov-priority)))
	    (setq outer-priority (min ov-priority (or outer-priority
						      ov-priority))))))
      (cond ((and inner-priority (not outer-priority))
	     (+ inner-priority TeX-overlay-priority-step))
	    ((and (not inner-priority) outer-priority)
	     (/ outer-priority 2))
	    ((and inner-priority outer-priority)
	     (+ (/ (- outer-priority inner-priority) 2) inner-priority))
	    (t TeX-overlay-priority-step)))) )

(defun TeX-delete-dups-by-car (alist &optional keep-list)
  "Return a list of all elements in ALIST, but each car only once.
Elements of KEEP-LIST are not removed even if duplicate."
  ;; Copy of `reftex-uniquify-by-car' (written by David Kastrup).
  (setq keep-list (sort (copy-sequence keep-list) #'string<))
  (setq alist (sort (copy-sequence alist)
		    (lambda (a b)
		      (string< (car a) (car b)))))
  (let ((new alist) elt)
    (while new
      (setq elt (caar new))
      (while (and keep-list (string< (car keep-list) elt))
	(setq keep-list (cdr keep-list)))
      (unless (and keep-list (string= elt (car keep-list)))
	(while (string= elt (car (cadr new)))
	  (setcdr new (cddr new))))
      (setq new (cdr new))))
  alist)

(defun TeX-delete-duplicate-strings (list)
  "Return a list of all strings in LIST, but each only once."
  (setq list (TeX-sort-strings list))
  (let ((new list) elt)
    (while new
      (setq elt (car new))
      (while (string= elt (cadr new))
	(setcdr new (cddr new)))
      (setq new (cdr new))))
  list)

(defun TeX-sort-strings (list)
  "Return sorted list of all strings in LIST."
  (sort (copy-sequence list) #'string<))

;;; Buffer

(defgroup TeX-output nil
  "Parsing TeX output."
  :prefix "TeX-"
  :group 'AUCTeX)

(defcustom TeX-display-help t
  "Control type of help display when stepping through errors with \\[TeX-next-error].
If t display help buffer.  If nil display message about error in
echo area.  If `expert' display output buffer with raw processor output."
  :group 'TeX-output
  :type '(choice (const :tag "Help buffer" t)
		 (const :tag "Echo area" nil)
		 (const :tag "Output buffer" expert)))

(defcustom TeX-debug-bad-boxes nil
  "Non-nil means also find overfull/underfull box warnings with \\[TeX-next-error]."
  :group 'TeX-output
  :type 'boolean)

(defcustom TeX-debug-warnings nil
  "Non-nil means also find LaTeX or package warnings with \\[TeX-next-error]."
  :group 'TeX-output
  :type 'boolean)

(defun TeX-toggle-debug-bad-boxes ()
  "Toggle if the debugger should display \"bad boxes\" too."
  (interactive)
  (setq TeX-debug-bad-boxes (not TeX-debug-bad-boxes))
  (message (concat "TeX-debug-bad-boxes: "
		   (if TeX-debug-bad-boxes "on" "off"))))

(defun TeX-toggle-debug-warnings ()
  "Toggle if the debugger should display warnings too."
  (interactive)
  (setq TeX-debug-warnings (not TeX-debug-warnings))
  (message (concat "TeX-debug-warnings: "
		   (if TeX-debug-warnings "on" "off"))))

;;; Mode names.

(defvar TeX-base-mode-name nil
  "Base name of mode.")
(make-variable-buffer-local 'TeX-base-mode-name)

(defun TeX-set-mode-name (&optional changed local reset)
  "Build and set the mode name.
The base mode name will be concatenated with indicators for
helper modes where appropriate.

If CHANGED is non-nil, it indicates which global mode
may have changed so that all corresponding buffers
without a local value might get their name updated.
A value of t will thus update all buffer names.

If LOCAL is non-nil and CHANGED is buffer-local, only
a local change has been performed and only the local
name is to be updated.

If RESET is non-nil, `TeX-command-next' is reset to
`TeX-command-default' in updated buffers."
  (if (and changed
	   (not (and local (local-variable-p changed (current-buffer)))))
      (dolist (buffer (buffer-list))
	(and (local-variable-p 'TeX-mode-p buffer)
	     (not (local-variable-p changed buffer))
	     (with-current-buffer buffer (TeX-set-mode-name nil nil reset))))
    (if TeX-mode-p
	(let ((trailing-flags
	       (concat
		(and (boundp 'TeX-fold-mode) TeX-fold-mode "F")
		(and (boundp 'LaTeX-math-mode) LaTeX-math-mode "M")
		(and TeX-PDF-mode "P")
		(and TeX-interactive-mode "I")
		(and TeX-source-correlate-mode "S"))))
	  (setq mode-name (concat TeX-base-mode-name
				  (when (> (length trailing-flags) 0)
				    (concat "/" trailing-flags))))
	  (when reset
	    (TeX-process-set-variable (TeX-master-file)
				      'TeX-command-next TeX-command-default)
	    (TeX-process-set-variable (TeX-region-file)
				      'TeX-command-next TeX-command-default))
	  (set-buffer-modified-p (buffer-modified-p))))))

(defun TeX-mode-prefix (&optional mode)
  "Return the prefix for the symbol MODE as string.
If no mode is given the current major mode is used."
  (cdr (assoc (or mode major-mode) '((plain-tex-mode . "plain-TeX")
				     (latex-mode . "LaTeX")
				     (doctex-mode . "docTeX")
				     (texinfo-mode . "Texinfo")
				     (context-mode . "ConTeXt")))))

;;; Viewing

(defgroup TeX-view nil
  "Calling viewers from AUCTeX."
  :group 'TeX-command)

(defcustom TeX-view-style
  `((,(concat
      "^" (regexp-opt '("a4paper" "a4dutch" "a4wide" "sem-a4")) "$")
     "%(o?)xdvi %dS -paper a4 %d")
    (,(concat "^" (regexp-opt '("a5paper" "a5comb")) "$")
     "%(o?)xdvi %dS -paper a5 %d")
    ("^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
    ("^letterpaper$" "%(o?)xdvi %dS -paper us %d")
    ("^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
    ("^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
    ("^landscape$" "%(o?)xdvi %dS -paper a4r -s 0 %d")
    ;; The latest xdvi can show embedded postscript.  If you don't
    ;; have that, uncomment next line.
    ;; ("^epsf$" "ghostview %f")
    ("." "%(o?)xdvi %dS %d"))
  "List of style options and view options.

If the first element (a regular expression) matches the name of
one of the style files, any occurrence of the string `%v' in a
command in `TeX-command-list' will be replaced with the second
element.  The first match is used, if no match is found the `%v'
is replaced with the empty string.

As a default, the \"View\" command in `TeX-command-list' is set
to `%V'.  This means that `TeX-output-view-style' will be
consulted before `TeX-view-style'.  Only if no match is found in
`TeX-output-view-style' the settings in `TeX-view-style' will be
considered.  If you want to bypass `TeX-output-view-style', which
is not recommended because it is more powerful than
`TeX-view-style', use `%v' in the \"View\" command."
  :group 'TeX-view
  :type '(repeat (group regexp (string :tag "Command"))))

(defcustom TeX-output-view-style
  `(("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
     "%(o?)dvips -t landscape %d -o && gv %f")
    ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
    ("^dvi$" (,(concat
		"^" (regexp-opt '("a4paper" "a4dutch" "a4wide" "sem-a4")) "$")
	      "^landscape$")
     "%(o?)xdvi %dS -paper a4r -s 0 %d")
    ("^dvi$" ,(concat
	       "^" (regexp-opt '("a4paper" "a4dutch" "a4wide" "sem-a4")) "$")
     "%(o?)xdvi %dS -paper a4 %d")
    ("^dvi$" (,(concat "^" (regexp-opt '("a5paper" "a5comb")) "$")
	      "^landscape$")
     "%(o?)xdvi %dS -paper a5r -s 0 %d")
    ("^dvi$" ,(concat "^" (regexp-opt '("a5paper" "a5comb")) "$")
     "%(o?)xdvi %dS -paper a5 %d")
    ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
    ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
    ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
    ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
    ("^dvi$" "." "%(o?)xdvi %dS %d")
    ("^pdf$" "." "xpdf -remote %s -raise %o %(outpage)")
    ("^html?$" "." "netscape %o"))
  "List of output file extensions and view options.

If the first element (a regular expression) matches the output
file extension, and the second element (a regular expression)
matches the name of one of the style options, any occurrence of
the string `%V' in a command in `TeX-command-list' will be
replaced with the third element.  The first match is used; if no
match is found the `%V' is replaced with `%v'.  The outcome of `%v'
is determined by the settings in `TeX-view-style' which therefore
serves as a fallback for `TeX-output-view-style'.  The second
element may also be a list of regular expressions, in which case
all the regular expressions must match for the element to apply."
  :group 'TeX-view
  :type '(repeat (group
		  (regexp :tag "Extension")
		  (choice regexp (repeat :tag "List" regexp))
		  (string :tag "Command"))))

;;; Viewing (new implementation)

(defvar TeX-view-predicate-list-builtin
  '((output-dvi
     (string-match "dvi" (TeX-output-extension)))
    (output-pdf
     (string-match "pdf" (TeX-output-extension)))
    (output-html
     (string-match "html" (TeX-output-extension)))
    (style-pstricks
     (TeX-match-style "^pstricks$\\|^pst-\\|^psfrag$"))
    (engine-omega
     (eq TeX-engine 'omega))
    (engine-xetex
     (eq TeX-engine 'xetex))
    (mode-io-correlate
     TeX-source-correlate-mode)
    (paper-landscape
     (TeX-match-style "\\`landscape\\'"))
    (paper-portrait
     (not (TeX-match-style "\\`landscape\\'")))
    (paper-a4
     (TeX-match-style "\\`a4paper\\|a4dutch\\|a4wide\\|sem-a4\\'"))
    (paper-a5
     (TeX-match-style "\\`a5paper\\|a5comb\\'"))
    (paper-b5
     (TeX-match-style "\\`b5paper\\'"))
    (paper-letter
     (TeX-match-style "\\`letterpaper\\'"))
    (paper-legal
     (TeX-match-style "\\`legalpaper\\'"))
    (paper-executive
     (TeX-match-style "\\`executivepaper\\'")))
  "Alist of built-in predicates for viewer selection and invocation.
See the doc string of `TeX-view-predicate-list' for a short
description of each predicate.")

(defcustom TeX-view-predicate-list nil
  "Alist of predicates for viewer selection and invocation.
The key of each list item is a symbol and the value a Lisp form
to be evaluated.  The form should return nil if the predicate is
not fulfilled.

Built-in predicates provided in `TeX-view-predicate-list-builtin'
can be overwritten by defining predicates with the same symbol.

The following built-in predicates are available:
  `output-dvi': The output is a DVI file.
  `output-pdf': The output is a PDF file.
  `output-html': The output is an HTML file.
  `style-pstricks': The document loads a PSTricks package.
  `engine-omega': The Omega engine is used for typesetting.
  `engine-xetex': The XeTeX engine is used for typesetting.
  `mode-io-correlate': TeX Source Correlate mode is active.
  `paper-landscape': The document is typeset in landscape orientation.
  `paper-portrait': The document is not typeset in landscape orientation.
  `paper-a4': The paper format is A4.
  `paper-a5': The paper format is A5.
  `paper-b5': The paper format is B5.
  `paper-letter': The paper format is letter.
  `paper-legal': The paper format is legal.
  `paper-executive': The paper format is executive."
  :group 'TeX-view
  :type '(alist :key-type symbol :value-type (group sexp)))

(defun TeX-evince-dbus-p (&rest options)
  "Return non-nil, if evince is installed and accessible via DBUS.
Additional OPTIONS may be given to extend the check.  If none are
given, only the minimal requirements needed by backward search
are checked.  If OPTIONS include `:forward', which is currently
the only option, then additional requirements needed by forward
search are checked, too."
  (and (not (featurep 'xemacs)) ; XEmacs 21.4 has no `require' with
			        ; arity 3, and no dbus support anyway.
       (require 'dbus nil :no-error)
       (functionp 'dbus-register-signal)
       (getenv "DBUS_SESSION_BUS_ADDRESS")
       (executable-find "evince")
       (or (not (memq :forward options))
	   (let ((spec (dbus-introspect-get-method
			:session "org.gnome.evince.Daemon"
			"/org/gnome/evince/Daemon"
			"org.gnome.evince.Daemon"
			"FindDocument")))
	     ;; FindDocument must exist, and its signature must be (String,
	     ;; Boolean, String).  Evince versions between 2.30 and 2.91.x
	     ;; didn't have the Boolean spawn argument we need to start evince
	     ;; initially.
	     (and spec
		  (equal '("s" "b" "s")
			 (delq nil (mapcar (lambda (elem)
					     (when (and (listp elem)
							(eq (car elem) 'arg))
					       (cdr (caar (cdr elem)))))
					   spec))))))))

(defun TeX-evince-sync-view ()
  "Focus the focused page/paragraph in Evince with the position
of point in emacs by using Evince's DBUS API.  Used by default
for the Evince viewer entry in `TeX-view-program-list-builtin' if
the requirements are met."
  (let* ((uri (concat "file://" (expand-file-name
				 (concat file "." (TeX-output-extension)))))
	 (owner (dbus-call-method
		 :session "org.gnome.evince.Daemon"
		 "/org/gnome/evince/Daemon"
		 "org.gnome.evince.Daemon"
		 "FindDocument"
		 uri
		 t)))
    (if owner
	(dbus-call-method
	 :session owner
	 "/org/gnome/evince/Window/0"
	 "org.gnome.evince.Window"
	 "SyncView"
	 (buffer-file-name)
	 (list :struct :int32 (line-number-at-pos) :int32 (1+ (current-column)))
	 :uint32 (let ((time (float-time)))
		   ;; FIXME: Evince wants a timestamp as UInt32, but POSIX time
		   ;; is too large for emacs integers on 32 bit systems.  Emacs
		   ;; 24.2 will allow providing DBUS ints as floats, and this
		   ;; dbus version will be identifiable by its new variables
		   ;; `dbus-compiled-version' and `dbus-runtime-version'.  But
		   ;; it seems providing just 1 as timestamp has no negative
		   ;; consequences, anyway.
		   (if (> most-positive-fixnum time)
		       (round time)
		     1)))
      (error "Couldn't find the Evince instance for %s" uri))))

(defvar TeX-view-program-list-builtin
  (cond
   ((eq system-type 'windows-nt)
    '(("Yap" ("yap -1" (mode-io-correlate " -s %n%b") " %o"))
      ("dvips and start" "dvips %d -o && start \"\" %f")
      ("start" "start \"\" %o")))
;; XXX: We need the advice of a Mac OS X user to configure this
;; correctly and test it.
;;    ((eq system-type 'darwin)
;;     '(("Preview.app" "open -a Preview.app %o")
;;       ("Skim" "open -a Skim.app %o")
;;       ("displayline" "displayline %n %o %b")
;;       ("open" "open %o")))
   (t
    `(("xdvi" ("%(o?)xdvi"
	       (mode-io-correlate " -sourceposition \"%n %b\" -editor \"%cS\"")
	       ((paper-a4 paper-portrait) " -paper a4")
	       ((paper-a4 paper-landscape) " -paper a4r")
	       ((paper-a5 paper-portrait) " -paper a5")
	       ((paper-a5 paper-landscape) " -paper a5r")
	       (paper-b5 " -paper b5")
	       (paper-letter " -paper us")
	       (paper-legal " -paper legal")
	       (paper-executive " -paper 7.25x10.5in")
	       " %d"))
      ("dvips and gv" "%(o?)dvips %d -o && gv %f")
      ("gv" "gv %o")
      ("xpdf" ("xpdf -remote %s -raise %o" (mode-io-correlate " %(outpage)")))
      ("Evince" ,(if (TeX-evince-dbus-p :forward)
		     'TeX-evince-sync-view
		   `("evince" (mode-io-correlate
			       ;; With evince 3, -p N opens the page *labeled* N,
			       ;; and -i,--page-index the physical page N.
			       ,(if (string-match "--page-index"
						  (shell-command-to-string "evince --help"))
				    " -i %(outpage)"
				  " -p %(outpage)")) " %o")))
      ("Okular" ("okular --unique %o" (mode-io-correlate "#src:%n%b")))
      ("xdg-open" "xdg-open %o"))))
  "Alist of built-in viewer specifications.
This variable should not be changed by the user who can use
`TeX-view-program-list' to add new viewers or overwrite the
definition of built-in ones.  The latter variable also contains a
description of the data format.")

(defcustom TeX-view-program-list nil
  "Alist of viewer specifications.
This variable can be used to specify how a viewer is to be
invoked and thereby add new viewers on top of the built-in list
of viewers defined in `TeX-view-program-list-builtin' or override
entries in the latter.

The car of each item is a string with a user-readable name.  The
second element can be a command line to be run as a process or a
Lisp function to be executed.  The command line can either be
specified as a single string or a list of strings and two-part
lists.  The first element of the two-part lists is a symbol or a
list of symbols referring to one or more of the predicates in
`TeX-view-predicate-list' or `TeX-view-predicate-list-builtin'.
The second part of the two-part lists is a command line part.
The command line for the viewer is constructed by concatenating
the command line parts.  Parts with a predicate are only
considered if the predicate was evaluated with a positive result.
Note that the command line can contain placeholders as defined in
`TeX-expand-list' which are expanded before the viewer is called.

The use of a function as the second element only works if the
View command in `TeX-command-list' makes use of the hook
`TeX-run-discard-or-function'.

Note: Predicates defined in the current Emacs session will only
show up in the customization interface for this variable after
restarting Emacs."
  :group 'TeX-view
  :type
  `(alist
    :key-type (string :tag "Name")
    :value-type
    (choice
     (group :tag "Command" (string :tag "Command"))
     (group :tag "Command parts"
	    (repeat
	     :tag "Command parts"
	     (choice
	      (string :tag "Command part")
	      (list :tag "Predicate and command part"
		    ,(let (list)
		       ;; Build the list of available predicates.
		       (mapc (lambda (spec)
			       (add-to-list 'list `(const ,(car spec))))
			     (append TeX-view-predicate-list
				     TeX-view-predicate-list-builtin))
		       ;; Sort the list alphabetically.
		       (setq list (sort list
					(lambda (a b)
					  (string<
					   (downcase (symbol-name (cadr a)))
					   (downcase (symbol-name (cadr b)))))))
		       `(choice
			 (choice :tag "Predicate" ,@list)
			 (repeat :tag "List of predicates"
				 (choice :tag "Predicate" ,@list))))
		    (string :tag "Command part")))))
     (group :tag "Function" function))))

;; XXX: Regarding a possibility to (manually) run an update command,
;; one could support this through `TeX-view' by letting it temporarily
;; set a variable which is checked with a predicate in the viewer
;; selection.  If the check is positive, the update command is run
;; instead of the normal viewer command.  Direct support through the
;; View command would require a predicate which knows when an update
;; has to be done.
(defcustom TeX-view-program-selection
  (cond
   ((eq system-type 'windows-nt)
    '(((output-dvi style-pstricks) "dvips and start")
      (output-dvi "Yap")
      (output-pdf "start")
      (output-html "start")))
;; XXX: We need the advice of a Mac OS X user to configure this
;; correctly and test it.
;;    ((eq system-type 'darwin)
;;     '((output-dvi "open")
;;       (output-pdf "open")
;;       (output-html "open")))
   (t
    '(((output-dvi style-pstricks) "dvips and gv")
      (output-dvi "xdvi")
      (output-pdf "Evince")
      (output-html "xdg-open"))))
  "Alist of predicates and viewers.
Each entry consists of a list with two elements.  The first
element is a symbol or list of symbols referring to predicates as
defined in `TeX-view-predicate-list' or
`TeX-view-predicate-list-builtin'.  The second element is a
string referring to the name of a viewer as defined in
`TeX-view-program-list' or `TeX-view-program-list-builtin'.
\(Note: Viewers added to `TeX-view-program-list' in the current
Emacs session will not show up in the customization interface of
`TeX-view-program-selection' until you restart Emacs.)

When a viewer is called for, the entries are evaluated in turn
and the viewer related to the first entry all predicates of which
are evaluated positively is chosen."
  :group 'TeX-view
  :type `(alist :key-type
		;; Offer list of defined predicates.
		,(let (list)
		   (mapc (lambda (spec)
			   (add-to-list 'list `(const ,(car spec))))
			 (append TeX-view-predicate-list
				 TeX-view-predicate-list-builtin))
		   (setq list (sort list
				    (lambda (a b)
				      (string<
				       (downcase (symbol-name (cadr a)))
				       (downcase (symbol-name (cadr b)))))))
		   `(choice (choice :tag "Single predicate" ,@list)
			    (repeat :tag "Multiple predicates"
				    (choice ,@list))))
		:value-type
		;; Offer list of defined viewers.
		(group (choice :tag "Viewer"
			       ,@(let (list)
				   (mapc (lambda (spec)
					   (add-to-list 'list
							`(const ,(car spec))))
				     (append TeX-view-program-list
					     TeX-view-program-list-builtin))
				   (sort list
					 (lambda (a b)
					   (string< (downcase (cadr a))
						    (downcase (cadr b))))))))))

(defun TeX-match-style (regexp)
  "Check if a style matching REGEXP is active."
  (TeX-member regexp (TeX-style-list) 'string-match))

(defun TeX-view-match-predicate (predicate)
  "Check if PREDICATE is true.
PREDICATE can be a symbol or a list of symbols defined in
`TeX-view-predicate-list-builtin' or `TeX-view-predicate-list'.
In case of a single symbol, return t if the predicate is true,
nil otherwise.  In case of a list of symbols, return t if all
predicates are true, nil otherwise."
  (let ((pred-symbols (if (listp predicate) predicate (list predicate)))
	(pred-defs (append TeX-view-predicate-list
			   TeX-view-predicate-list-builtin))
	(result t)
	elt)
    (while (and (setq elt (pop pred-symbols)) result)
      (unless (eval (cadr (assq elt pred-defs)))
	(setq result nil)))
    result))

(defun TeX-view-command-raw ()
  "Choose a viewer and return its unexpanded command string."
  (let ((selection TeX-view-program-selection)
	entry viewer spec command)
    ;; Find the appropriate viewer.
    (while (and (setq entry (pop selection)) (not viewer))
      (when (TeX-view-match-predicate (car entry))
	(setq viewer (cadr entry))))
    (unless viewer
      (error "No matching viewer found"))
    ;; Get the command line or function spec.
    (setq spec (cadr (assoc viewer (append TeX-view-program-list
					   TeX-view-program-list-builtin))))
    (cond ((functionp spec)
	   ;; Converting the function call to a string is ugly, but
	   ;; the backend currently only supports strings.
	   (prin1-to-string spec))
	  ((stringp spec)
	   spec)
	  (t
	   ;; Build the unexpanded command line.  Pieces with predicates are
	   ;; only added if the predicate is evaluated positively.
	   (dolist (elt spec)
	     (cond ((stringp elt)
		    (setq command (concat command elt)))
		   ((listp elt)
		    (when (TeX-view-match-predicate (car elt))
		      (setq command (concat command (cadr elt)))))))
	   command))))

;;; Engine

(defvar TeX-engine-alist-builtin
  '((default "Default" TeX-command LaTeX-command ConTeXt-engine)
    (xetex "XeTeX" "xetex" "xelatex" "xetex")
    ;; Some lualatex versions before 0.71 would use "texput" as file
    ;; name if --jobname were not supplied
    (luatex "LuaTeX" "luatex" "lualatex --jobname=%s" "luatex")
    (omega "Omega" TeX-Omega-command LaTeX-Omega-command ConTeXt-Omega-engine))
  "Alist of built-in TeX engines and associated commands.
For a description of the format see `TeX-engine-alist'.")

(defcustom TeX-engine-alist nil
  "Alist of TeX engines and associated commands.
Each entry is a list with a maximum of five elements.  The first
element is a symbol used to identify the engine.  The second is a
string describing the engine.  The third is the command to be
used for plain TeX.  The fourth is the command to be used for
LaTeX.  The fifth is the command to be used for the --engine
parameter of ConTeXt's texexec program.  Each command can either
be a variable or a string.  An empty string or nil means there is
no command available.

You can override a built-in engine defined in the variable
`TeX-engine-alist-builtin' by adding an entry beginning with the
same symbol as the built-in entry to `TeX-engine-alist'."
  :group 'TeX-command
  :type '(repeat (group symbol
			(string :tag "Name")
			(choice :tag "Plain TeX command" string variable)
			(choice :tag "LaTeX command" string variable)
			(choice :tag "ConTeXt command" string variable))))

(defun TeX-engine-alist ()
  "Return an alist of TeX engines.
The function appends the built-in engine specs from
`TeX-engine-alist-builtin' and the user-defined engines from
`TeX-engine-alist' and deletes any entries from the built-in part
where an entry with the same car exists in the user-defined part."
  (TeX-delete-dups-by-car (append TeX-engine-alist TeX-engine-alist-builtin)))

(defcustom TeX-engine 'default
  (concat "Type of TeX engine to use.
It should be one of the following symbols:\n\n"
	  (mapconcat (lambda (x) (format "* `%s'" (car x)))
		     (TeX-engine-alist) "\n"))
  :group 'TeX-command
  :type `(choice ,@(mapcar (lambda (x)
			     `(const :tag ,(nth 1 x) ,(car x)))
			   (TeX-engine-alist))))
(make-variable-buffer-local 'TeX-engine)
(put 'TeX-engine 'safe-local-variable
     (lambda (arg) (memq arg (mapcar 'car TeX-engine-alist-builtin))))

(defun TeX-engine-set (type)
  "Set TeX engine to TYPE.
For available TYPEs, see variable `TeX-engine'."
  (interactive (list (completing-read "Engine: "
				      (mapcar (lambda (x)
						(symbol-name (car x)))
					      (TeX-engine-alist))
				      nil t)))
  (when (stringp type)
    (setq type (intern type)))
  (setq TeX-engine type)
  ;; Automatically enable or disable TeX PDF mode as a convenience
  (cond ((eq type 'xetex) (TeX-PDF-mode 1))
	((eq type 'omega) (TeX-PDF-mode 0))))

(define-minor-mode TeX-Omega-mode
  "Minor mode for using the Omega engine."
  nil nil nil
  :group 'TeX-command
  (TeX-engine-set (if TeX-Omega-mode 'omega 'default)))
(defalias 'tex-omega-mode 'TeX-Omega-mode)
(TeX--call-3/2 #'make-obsolete 'TeX-Omega-mode 'TeX-engine-set "before 11.86")
(TeX--call-3/2 #'make-obsolete-variable 'TeX-Omega-mode
               'TeX-engine "before 11.86")

;;; Forward and inverse search

(defcustom TeX-source-correlate-method 'auto
  "Method to use for enabling forward and inverse search.
This can be `source-specials' if source specials should be used,
`synctex' if SyncTeX should be used, or`auto' if AUCTeX should
decide.

Setting this variable does not take effect if TeX Source
Correlate mode has already been active.  Restart Emacs in this
case."
  :type '(choice (const auto) (const synctex) (const source-specials))
  :group 'TeX-view)

(defvar TeX-source-correlate-method-active nil
  "Method actually used for forward and inverse search.")

(defvar TeX-source-correlate-output-page-function nil
  "Symbol of function returning an output page relating to buffer position.
The function should take no arguments and return the page numer
as a string.")
(make-variable-buffer-local 'TeX-source-correlate-output-page-function)

(defcustom TeX-source-correlate-start-server 'ask
  "Control if server should be started for inverse search."
  :type '(choice (const :tag "Always" t)
		 (const :tag "Never" nil)
		 (const :tag "Ask" ask))
  :group 'TeX-view)
(when (fboundp 'defvaralias)
  (defvaralias 'TeX-source-specials-view-start-server
    'TeX-source-correlate-start-server))

(defvar TeX-source-correlate-start-server-asked nil
  "Keep track if question about server start search was asked.")

(defvar TeX-source-correlate-start-server-flag nil
  "If non-nil, `TeX-source-correlate-start-server-maybe' will start a server.
Code related to features requiring a server, e.g. for inverse
search, can set the variable.")

(defun TeX-source-correlate-gnuserv-p ()
  "Guess whether to use gnuserv when a server is requested."
  (cond ((and (boundp 'gnuserv-process)
	      (processp gnuserv-process)))
	((and (boundp 'server-process)
	      (processp server-process))
	 nil)
	((featurep 'xemacs))))

(defun TeX-source-correlate-server-enabled-p ()
  "Return non-nil if Emacs server or gnuserv is enabled."
  (let* ((gnuserv-p (TeX-source-correlate-gnuserv-p))
	 (process (if gnuserv-p 'gnuserv-process 'server-process)))
    (and (boundp process) (processp (symbol-value process)))))

(defun TeX-source-correlate-start-server-maybe ()
  "Start Emacs server or gnuserv if a feature using it is enabled.
This is the case if `TeX-source-correlate-start-server-flag' is non-nil."
  (when (and TeX-source-correlate-start-server-flag
	     (not (TeX-source-correlate-server-enabled-p)))
    (let* ((gnuserv-p (TeX-source-correlate-gnuserv-p))
	   (start (if gnuserv-p 'gnuserv-start 'server-start)))
      (cond
       ;; Server should be started unconditionally
       ((eq TeX-source-correlate-start-server t)
	(funcall start))
       ;; Ask user if server is to be started
       ((and (eq TeX-source-correlate-start-server 'ask)
	     (not TeX-source-correlate-start-server-asked)
	     (prog1
		 (y-or-n-p (format "Start %s for inverse search in viewer? "
				   (if gnuserv-p
				       "gnuserv"
				     "Emacs server")))
	       (setq TeX-source-correlate-start-server-asked t)))
	(funcall start))))))

(defun TeX-source-correlate-determine-method ()
  "Determine which method is available for forward and inverse search."
  (let ((help (condition-case nil
		  (with-output-to-string
		    (call-process LaTeX-command
				  nil (list standard-output nil) nil "--help"))
		(error ""))))
    (if (string-match "^[ ]*-?-synctex" help)
	'synctex
      'source-specials)))

(defun TeX-source-correlate-expand-options ()
  "Return TeX engine command line option for forward search facilities.
The return value depends on the value of `TeX-source-correlate-mode'.
If this is nil, an empty string will be returned."
  (if TeX-source-correlate-mode
      (if (eq TeX-source-correlate-method-active 'source-specials)
	  (concat TeX-source-specials-tex-flags
		  (if TeX-source-specials-places
		      ;; -src-specials=WHERE: insert source specials
		      ;; in certain places of the DVI file. WHERE is a
		      ;; comma-separated value list: cr display hbox
		      ;; math par parend vbox
		      (concat "=" (mapconcat 'identity
					     TeX-source-specials-places ","))))
	TeX-synctex-tex-flags)
    ""))

(defvar TeX-source-correlate-map
  (let ((map (make-sparse-keymap)))
    ;; (if (featurep 'xemacs)
    ;;	   (define-key map [(control button1)] #'TeX-view-mouse)
    ;;   (define-key map [C-down-mouse-1] #'TeX-view-mouse))
    map)
  "Keymap for `TeX-source-correlate-mode'.
You could use this for unusual mouse bindings.")

(defun TeX-source-correlate-sync-source (file linecol &rest ignored)
  "Show TeX FILE with point at LINECOL.
This function is called when emacs receives a SyncSource signal
emitted from the Evince document viewer.  IGNORED absorbs an
unused id field accompanying the DBUS signal sent by Evince-3.0.0
or newer."
  ;; FILE may be given as relative path to the TeX-master root document or as
  ;; absolute file:// URL.  In the former case, the tex file has to be already
  ;; opened.
  (let ((buf (let ((f (condition-case nil
			  (progn
			    (require 'url-parse)
			    (aref (url-generic-parse-url file) 6))
			;; For Emacs 21 compatibility, which doesn't have the
			;; url package.
			(file-error (replace-regexp-in-string "^file://" "" file)))))
	       (if (file-name-absolute-p f)
		   (find-file f)
		 (get-buffer (file-name-nondirectory file)))))
        (line (car linecol))
        (col (cadr linecol)))
    (if (null buf)
        (message "No buffer for %s." file)
      (switch-to-buffer buf)
      (push-mark (point) 'nomsg)
      (goto-char (point-min))
      (forward-line (1- line))
      (unless (= col -1)
        (move-to-column col)))))

(define-minor-mode TeX-source-correlate-mode
  "Minor mode for forward and inverse search.

If enabled, the viewer can be advised to show the output page
corresponding to the point in the source and vice versa.

The method to be used can be controlled with the variable
`TeX-source-correlate-method'.  Currently source specials or
SyncTeX are recognized."
  :group 'TeX-view
  ;; Since this is a global minor mode and we don't want to require
  ;; tex.el when the mode variable is set, the mode function is called
  ;; explicitely (if necessary) in `VirTeX-common-initialization'.  We
  ;; do it there because otherwise `kill-all-local-variables' would
  ;; reset `TeX-source-correlate-output-page-function' which is
  ;; buffer-local.
  :global t
  (set-keymap-parent TeX-mode-map (and TeX-source-correlate-mode
				       TeX-source-correlate-map))
  (TeX-set-mode-name 'TeX-source-correlate-mode t t)
  (setq TeX-source-correlate-start-server-flag TeX-source-correlate-mode)
  ;; Register Emacs for the SyncSource DBUS signal emitted by Evince.
  (when (TeX-evince-dbus-p)
    (dbus-register-signal
     :session nil "/org/gnome/evince/Window/0"
     "org.gnome.evince.Window" "SyncSource"
     'TeX-source-correlate-sync-source))
  (unless TeX-source-correlate-method-active
    (setq TeX-source-correlate-method-active
	  (if (eq TeX-source-correlate-method 'auto)
	      (TeX-source-correlate-determine-method)
	    TeX-source-correlate-method)))
  (when (eq TeX-source-correlate-method-active 'synctex)
    (setq TeX-source-correlate-output-page-function
	  (when TeX-source-correlate-mode
	    'TeX-synctex-output-page))))
(defalias 'TeX-source-specials-mode 'TeX-source-correlate-mode)
(TeX--call-3/2 #'make-obsolete 'TeX-source-specials-mode
               'TeX-source-correlate-mode "before 11.86")
(defalias 'tex-source-correlate-mode 'TeX-source-correlate-mode)
(put 'TeX-source-correlate-mode 'safe-local-variable 'TeX-booleanp)
;; We do not want the custom variable to require tex.el.  This is only
;; necessary if AUCTeX was compiled with Emacs 21.
(put 'TeX-source-correlate-mode 'custom-requests nil)
(setq minor-mode-map-alist
      (delq (assq 'TeX-source-correlate-mode minor-mode-map-alist)
	    minor-mode-map-alist))


;;; Source Specials

(defcustom TeX-source-specials-tex-flags "-src-specials"
  "Extra flags to pass to TeX commands to generate source specials."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-places nil
  "List of places where to insert source specials into the DVI file.
If nil, use (La)TeX's defaults."
  :group 'TeX-view
  :type '(list (set :inline t
		    ;; :tag "Options known to work"
		    ;; cr display hbox math par parend vbox
		    (const "cr")
		    (const "display")
		    (const "hbox")
		    (const "math")
		    (const "par")
		    (const "parend")
		    (const "vbox"))
	       (repeat :inline t
		       :tag "Other options"
		       (string))))

(defcustom TeX-source-specials-view-position-flags
  "-sourceposition \"%n %b\""
  "Flags to pass to the DVI viewer commands for the position in the source."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-view-editor-flags
  "-editor \"%cS\""
  "Flags to pass to DVI viewer commands for inverse search."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-view-gnuclient-flags
  "-q +%%l %%f"
  "Flags to pass to gnuclient for inverse search."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-view-emacsclient-flags
  "--no-wait +%%l %%f"
  "Flags to emacsclient for inverse search."
  :group 'TeX-view
  :type 'string)

;; FIXME: Make client binaries configurable.
(defun TeX-source-specials-view-expand-client ()
  "Return gnuclient or emacslient executable with options.
Return the full path to the executable if possible."
  (let* ((gnuserv-p (TeX-source-correlate-gnuserv-p))
	 (client-base (if gnuserv-p
			  "gnuclient"
			"emacsclient"))
	 (client-full (and invocation-directory
			   (expand-file-name client-base
					     invocation-directory)))
	 (options (if gnuserv-p
		      TeX-source-specials-view-gnuclient-flags
		    TeX-source-specials-view-emacsclient-flags)))
    (if (and client-full (file-executable-p client-full))
	(concat client-full " " options)
      (concat client-base " " options))))

(defun TeX-source-specials-view-expand-options (&optional viewer)
  "Return source specials command line option for viewer command.
The return value depends on the values of
`TeX-source-correlate-mode' and
`TeX-source-correlate-method-active'.  If those are nil or not
`source-specials' respectively, an empty string will be
returned."
  (if (and TeX-source-correlate-mode
	   (eq TeX-source-correlate-method-active 'source-specials))
      (concat TeX-source-specials-view-position-flags
	      (when (TeX-source-correlate-server-enabled-p)
		(concat " " TeX-source-specials-view-editor-flags)))
    ""))

;;; SyncTeX

(defvar TeX-synctex-tex-flags "--synctex=1"
  "Extra flags to pass to TeX commands to enable SyncTeX.")

(defun TeX-synctex-output-page-1 (file)
  "Return the page corresponding to the current position in FILE.
This method assumes that the document was compiled with SyncTeX
enabled and the `synctex' binary is available."
  (let ((synctex-output
	 (with-output-to-string
	   (call-process "synctex" nil (list standard-output nil) nil "view"
			 "-i" (format "%s:%s:%s" (line-number-at-pos)
				      (current-column)
				      file)
			 "-o" (TeX-active-master (TeX-output-extension))))))
    (when (string-match "Page:\\([0-9]+\\)" synctex-output)
      (match-string 1 synctex-output))))

(defun TeX-synctex-output-page ()
  "Return the page corresponding to the position in the current buffer.
This method assumes that the document was compiled with SyncTeX
enabled and the `synctex' binary is available."
  (let* ((file (file-relative-name (buffer-file-name)
				   (file-name-directory
				    (TeX-active-master))))
	 (abs-file (concat (expand-file-name (or (file-name-directory (TeX-active-master))
						 (file-name-directory (buffer-file-name))))
			   "./" file)))
    ;; It's known that depending on synctex version one of
    ;; /absolute/path/./foo/bar.tex, foo/bar.tex, or ./foo/bar.tex (relative to
    ;; TeX-master, and the "." in the absolute path is important) are needed.
    ;; So try all variants before falling back to page 1.
    (or (TeX-synctex-output-page-1 abs-file)
	(TeX-synctex-output-page-1 file)
	(TeX-synctex-output-page-1 (concat "./" file))
	"1")))

;;; Miscellaneous minor modes

(defvar TeX-mode-p nil
  "This indicates a TeX mode being active.")
(make-variable-buffer-local 'TeX-mode-p)

(defun TeX-mode-set (var value)
  (set-default var value)
  (TeX-set-mode-name var nil t))

(defcustom TeX-PDF-mode nil nil
  :group 'TeX-command
  :set 'TeX-mode-set
  :type 'boolean)
(put 'TeX-PDF-mode 'safe-local-variable 'TeX-booleanp)

(define-minor-mode TeX-PDF-mode
  "Minor mode for using PDFTeX.

If enabled, PDFTeX will be used as an executable by default.
You can customize an initial value, and you can use the
function `TeX-global-PDF-mode' for toggling this value."
  :group 'TeX-command
  (when (eq TeX-engine 'omega)
    (setq TeX-PDF-mode nil))
  (setq TeX-PDF-mode-parsed nil)
  (TeX-set-mode-name nil nil t)
  (setq TeX-output-extension
	(if TeX-PDF-mode "pdf" "dvi")))
(add-to-list 'minor-mode-alist '(TeX-PDF-mode ""))

(defun TeX-global-PDF-mode (&optional arg)
  "Toggle default for `TeX-PDF-mode'."
  (interactive "P")
  (prog1
      (setq-default TeX-PDF-mode
		    (if arg (> (prefix-numeric-value arg) 0)
		      (not (default-value 'TeX-PDF-mode))))
    (TeX-set-mode-name 'TeX-PDF-mode nil t)))

(defalias 'tex-pdf-mode 'TeX-PDF-mode)

(defvar TeX-PDF-mode-parsed nil
  "Set if `TeX-PDF-mode' has come about by parsing.")

(make-variable-buffer-local 'TeX-PDF-mode-parsed)

(defun TeX-PDF-mode-parsed (arg)
  "Change `TeX-PDF-mode' to ARG based on parsing.
If this conflicts with previous parsed settings,
just use the default.  If an explicit setting is
already established, don't do anything."

;; Basically we have the following situations:
;; TeX-PDF-mode-parsed (local-variable-p 'TeX-PDF-mode):
;; nil nil : virgin state
;; nil t   : stably set state (possibly because of conflicting parse info)
;; t   t   : non-conflicting parsed info

  (if TeX-PDF-mode-parsed
      (unless (eq TeX-PDF-mode arg)
	(TeX-PDF-mode (if (default-value 'TeX-PDF-mode) 1 0)))
    (unless (local-variable-p 'TeX-PDF-mode (current-buffer))
      (TeX-PDF-mode (if arg 1 0))
      (setq TeX-PDF-mode-parsed t))))
  
(defun TeX-PDF-mode-on ()
  "Use only from parsing routines."
  (TeX-PDF-mode-parsed t))

(defun TeX-PDF-mode-off ()
  "Use only from parsing routines."
  (TeX-PDF-mode-parsed nil))

(defcustom TeX-DVI-via-PDFTeX nil
  "Whether to use PDFTeX also for producing DVI files."
  :group 'TeX-command
  :type 'boolean)

(define-minor-mode TeX-interactive-mode
  "Minor mode for interactive runs of TeX."
  nil nil nil
  :group 'TeX-command
  (TeX-set-mode-name 'TeX-interactive-mode t t))
(defalias 'tex-interactive-mode 'TeX-interactive-mode)
(add-to-list 'minor-mode-alist '(TeX-interactive-mode ""))

;;; Commands

(defgroup TeX-command-name nil
  "Names for external commands in AUCTeX."
  :group 'TeX-command)

(defcustom TeX-command-BibTeX "BibTeX"
  "*The name of the BibTeX entry in `TeX-command-list'."
  :group 'TeX-command-name
  :type 'string)
  (make-variable-buffer-local 'TeX-command-BibTeX)

(defcustom TeX-command-Biber "Biber"
  "*The name of the Biber entry in `TeX-command-list'."
  :group 'TeX-command-name
  :type 'string)
  (make-variable-buffer-local 'TeX-command-Biber)

(defcustom TeX-command-Show "View"
  "*The default command to show (view or print) a TeX file.
Must be the car of an entry in `TeX-command-list'."
  :group 'TeX-command-name
  :type 'string)
  (make-variable-buffer-local 'TeX-command-Show)

(defcustom TeX-command-Print "Print"
  "The name of the Print entry in `TeX-command-Print'."
  :group 'TeX-command-name
  :type 'string)

(defcustom TeX-command-Queue "Queue"
  "The name of the Queue entry in `TeX-command-Queue'."
  :group 'TeX-command-name
  :type 'string)

(defvar TeX-trailer-start nil
  "Regular expression delimiting start of trailer in a TeX file.")

 (make-variable-buffer-local 'TeX-trailer-start)

(defvar TeX-header-end nil
  "Regular expression delimiting end of header in a TeX file.")

 (make-variable-buffer-local 'TeX-header-end)

(defvar TeX-command-default nil
  "The default command for `TeX-command' in the current major mode.")

 (make-variable-buffer-local 'TeX-command-default)

(put 'TeX-command-default 'safe-local-variable 'stringp)

(defvar TeX-clean-default-intermediate-suffixes
  '("\\.aux" "\\.bbl" "\\.blg" "\\.brf" "\\.fot"
    "\\.glo" "\\.gls" "\\.idx" "\\.ilg" "\\.ind"
    "\\.lof" "\\.log" "\\.lot" "\\.nav" "\\.out"
    "\\.snm" "\\.toc" "\\.url" "\\.synctex\\.gz"
    "\\.bcf" "\\.run\\.xml")
  "List of regexps matching suffixes of files to be cleaned.
Used as a default in TeX, LaTeX and docTeX mode.")

(defvar TeX-clean-default-output-suffixes
  '("\\.dvi" "\\.pdf" "\\.ps" "\\.xdv")
  "List of regexps matching suffixes of files to be cleaned.
Used as a default in TeX, LaTeX and docTeX mode.")

(defcustom TeX-clean-confirm t
  "If non-nil, ask before deleting files."
  :type 'boolean
  :group 'TeX-command)

(autoload 'dired-mark-pop-up "dired")

(defun TeX-clean (&optional arg)
  "Delete generated files associated with current master and region files.
If prefix ARG is non-nil, not only remove intermediate but also
output files."
  (interactive "P")
  (let* ((mode-prefix (TeX-mode-prefix))
	 (suffixes (append (symbol-value
			    (intern (concat mode-prefix
					    "-clean-intermediate-suffixes")))
			   (when arg
			     (symbol-value
			      (intern (concat mode-prefix
					      "-clean-output-suffixes"))))))
	 (master (TeX-active-master))
	 (master-dir (file-name-directory master))
	 (regexp (concat "\\("
			 (regexp-quote (file-name-nondirectory master)) "\\|"
			 (TeX-region-file nil t)
			 "\\)"
			 "\\("
			 (mapconcat 'identity suffixes "\\|")
			 "\\)\\'"
			 "\\|" (TeX-region-file t t)))
	 (files (when regexp
		  (directory-files (or master-dir ".") nil regexp))))
    (if files
	(when (or (not TeX-clean-confirm)
		  (condition-case nil
		      (dired-mark-pop-up " *Deletions*" 'delete
					 (if (> (length files) 1) 
					     files
					   (cons t files))
					 'y-or-n-p "Delete files? ")
		    (wrong-type-argument ; e.g. with Emacs 21
		     (y-or-n-p (format "Delete %S? " (car files))))))
	  (dolist (file files)
	    (delete-file (concat master-dir file))))
      (message "No files to be deleted"))))


;;; Master File

(defcustom TeX-master t
  "*The master file associated with the current buffer.
If the file being edited is actually included from another file, you
can tell AUCTeX the name of the master file by setting this variable.
If there are multiple levels of nesting, specify the top level file.

If this variable is nil, AUCTeX will query you for the name.

If the variable is t, AUCTeX will assume the file is a master file
itself.

If the variable is 'shared, AUCTeX will query for the name, but not
change the file.

If the variable is 'dwim, AUCTeX will try to avoid querying by
attempting to `do what I mean'; and then change the file.

It is suggested that you use the File Variables (see the info node in
the Emacs manual) to set this variable permanently for each file."
  :group 'TeX-command
  :group 'TeX-parse
  :type '(choice (const :tag "Query" nil)
		 (const :tag "This file" t)
		 (const :tag "Shared" shared)
		 (const :tag "Dwim" dwim)
		 (string :format "%v")))
(make-variable-buffer-local 'TeX-master)
(put 'TeX-master 'safe-local-variable
     '(lambda (x)
	(or (stringp x)
	    (member x (quote (t nil shared dwim))))))

(defcustom TeX-one-master "\\.\\(texi?\\|dtx\\)$"
  "*Regular expression matching ordinary TeX files.

You should set this variable to match the name of all files, where
automatically adding a file variable with the name of the master file
is a good idea.  When AUCTeX adds the name of the master file as a
file variable, it does not need to ask next time you edit the file.

If you dislike AUCTeX automatically modifying your files, you can set
this variable to \"<none>\"."
  :group 'TeX-command
  :type 'regexp)

(defvar TeX-convert-master t
  "*If not nil, automatically convert ``Master:'' lines to file variables.
This will be done when AUCTeX first try to use the master file.")

;; Can be let-bound temporarily in order to inhibit the master file question
;; by using its value instead in case `TeX-master' is nil or 'shared.
(defvar TeX-transient-master nil)

(defun TeX-dwim-master ()
  "Find a likely `TeX-master'."
  (let ((dir default-directory))
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf
	      (and (equal dir default-directory)
		   (stringp TeX-master)))
	(return (with-current-buffer buf TeX-master))))))

(defun TeX-master-file-ask ()
  "Ask for master file, set `TeX-master' and add local variables."
  (interactive)
  (if (TeX-local-master-p)
      (error "Master file already set")
    (let* ((default (TeX-dwim-master))
	   (name (or (and (eq 'dwim TeX-master) default)
		     (condition-case nil
			 (read-file-name (format "Master file: (default %s) "
						 (or default "this file"))
					 nil default)
		       (quit "<quit>")))))
      (cond ((string= name "<quit>")
	     (setq TeX-master t))
	    ((string= name default)
	     (setq TeX-master default)
	     (TeX-add-local-master))
	    ((or
	      ;; Default `read-file-name' proposes and buffer visits a file.
	      (string= (expand-file-name name) (buffer-file-name))
	      ;; Default of `read-file-name' and buffer does not visit a file.
	      (string= name default-directory)
	      ;; User typed <RET> in an empty minibuffer.
	      (string= name ""))
	     (setq TeX-master t)
	     (TeX-add-local-master))
	    (t
	     (setq TeX-master (TeX-strip-extension (file-relative-name name)
						   (list TeX-default-extension)
						   'path))
	     (TeX-add-local-master))))))

(defun TeX-master-file (&optional extension nondirectory ask)
  "Set and return the name of the master file for the current document.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value t means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

If optional third argument ASK is non-nil, ask the user for the
name of master file if it cannot be determined otherwise.

Currently it will check for the presence of a ``Master:'' line in
the beginning of the file, but that feature will be phased out."
  (interactive)
  (if (eq extension t)
      (setq extension TeX-default-extension))
  (let ((my-name (if (buffer-file-name)
		     (TeX-strip-extension nil (list TeX-default-extension) t)
		   "<none>")))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(cond
	 ((and TeX-transient-master
	       (or (not TeX-master) (eq TeX-master 'shared)))
	  (setq TeX-master TeX-transient-master))
	 ;; Special value 't means it is own master (a free file).
	 ((equal TeX-master my-name)
	  (setq TeX-master t))

	 ;; For files shared between many documents.
	 ((and (eq 'shared TeX-master) ask)
	  (setq TeX-master
		(let* ((default (TeX-dwim-master))
		       (name (read-file-name
			      (format "Master file: (default %s) "
				      (or default "this file"))
			      nil default)))
		  (cond ((string= name default)
			 default)
			((or
			  ;; Default `read-file-name' proposes and
			  ;; buffer visits a file.
			  (string= (expand-file-name name)
				   (buffer-file-name))
			  ;; Default of `read-file-name' and
			  ;; buffer does not visit a file.
			  (string= name default-directory)
			  ;; User typed <RET> in an empty minibuffer.
			  (string= name ""))
			 t)
			(t
			 (TeX-strip-extension
			  name (list TeX-default-extension) 'path))))))

	 ;; We might already know the name.
	 ((or (eq TeX-master t) (stringp TeX-master)) TeX-master)

	 ;; Support the ``Master:'' line (under protest!)
	 ((re-search-forward
	   "^%% *[Mm]aster:?[ \t]*\\([^ \t\n]+\\)" 500 t)
	  (setq TeX-master
		(TeX-strip-extension (TeX-match-buffer 1)
				     (list TeX-default-extension)))
	  (if TeX-convert-master
	      (progn
		(beginning-of-line)
		(kill-line 1)
		(TeX-add-local-master))))

	 ;; Ask the user (but add it as a local variable).
	 (ask (TeX-master-file-ask)))))

    (let ((name (if (stringp TeX-master)
		    TeX-master
		  my-name)))

      (if (TeX-match-extension name)
	  ;; If it already has an extension...
	  (if (equal extension TeX-default-extension)
	      ;; Use instead of the default extension
	      (setq extension nil)
	    ;; Otherwise drop it.
	    (setq name (TeX-strip-extension name))))

      ;; Remove directory if needed.
      (if nondirectory
	  (setq name (file-name-nondirectory name)))

      (if extension
	  (concat name "." extension)
	name))))

(defun TeX-master-directory ()
  "Directory of master file."
  (file-name-as-directory
   (abbreviate-file-name
    (substitute-in-file-name
     (expand-file-name
      (let ((dir (file-name-directory (TeX-master-file))))
	(if dir (directory-file-name dir) "."))
      (and buffer-file-name
	   (file-name-directory buffer-file-name)))))))

(defun TeX-add-local-master ()
  "Add local variable for `TeX-master'."
  (when (and (buffer-file-name)
	     (string-match TeX-one-master
			   (file-name-nondirectory (buffer-file-name)))
	     (not buffer-read-only))
    (goto-char (point-max))
    (if (re-search-backward (concat "^\\([^\n]+\\)Local " "Variables:")
			    (- (point-max) 3000) t)
	(let ((prefix (TeX-match-buffer 1)))
	  (re-search-forward (regexp-quote (concat prefix
						   "End:")))
	  (beginning-of-line 1)
	  (insert prefix "TeX-master: " (prin1-to-string TeX-master) "\n"))
      (let ((comment-prefix (cond ((eq major-mode 'texinfo-mode) "@c ")
				  ((eq major-mode 'doctex-mode) "% ")
				  (t "%%% ")))
	    (mode (concat (and (boundp 'japanese-TeX-mode) japanese-TeX-mode
			       "japanese-")
			  (substring (symbol-name major-mode) 0 -5))))
	(newline)
	(when (eq major-mode 'doctex-mode)
	  (insert comment-prefix TeX-esc "endinput\n"))
	(insert
	 comment-prefix "Local " "Variables: \n"
	 comment-prefix "mode: " mode "\n"
	 comment-prefix "TeX-master: " (prin1-to-string TeX-master) "\n"
	 comment-prefix "End: \n")))))

(defun TeX-local-master-p ()
  "Return non-nil if there is a `TeX-master' entry in local variables spec.
Return nil otherwise."
  (save-excursion
    ;; XXX: Checking -*- line necessary as well?
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (re-search-forward "^%+ *TeX-master:" nil t)))

;;; Style Paths

(defcustom TeX-style-global (expand-file-name "style" TeX-data-directory)
  "*Directory containing hand generated TeX information.

These correspond to TeX macros shared by all users of a site."
  :group 'TeX-file
  :type 'directory)

(defcustom TeX-auto-local "auto"
  "*Directory containing automatically generated TeX information.

This correspond to TeX macros found in the current directory, and must
be relative to that."
  :group 'TeX-file
  :type 'string)

(defcustom TeX-style-local "style"
  "*Directory containing hand generated TeX information.

These correspond to TeX macros found in the current directory, and must
be relative to that."
  :group 'TeX-file
  :type 'string)

(defun TeX-split-string (regexp string)
  "Return a list of strings.
Given REGEXP the STRING is split into sections which in string was
seperated by REGEXP.

Examples:

      (TeX-split-string \"\:\" \"abc:def:ghi\")
	  -> (\"abc\" \"def\" \"ghi\")

      (TeX-split-string \" +\" \"dvips  -Plw -p3 -c4 testfile.dvi\")

	  -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If REGEXP is nil, or \"\", an error will occur."

  (let ((start 0) result match)
    (while (setq match (string-match regexp string start))
      (push (substring string start match) result)
      (setq start (match-end 0)))
    (push (substring string start) result)
    (nreverse result)))

(defun TeX-parse-path (env)
  "Return a list if private TeX directories found in environment variable ENV."
  (let* ((value (getenv env))
	 (entries (and value
		       (TeX-split-string
			(if (string-match ";" value) ";" ":")
			value)))
	 entry
	 answers)
    (while entries
      (setq entry (car entries))
      (setq entries (cdr entries))
      (setq entry (file-name-as-directory
		   (if (string-match "/?/?\\'" entry)
		       (substring entry 0 (match-beginning 0))
		     entry)))
      (or (not (file-name-absolute-p entry))
	  (member entry (append '("/" "\\") TeX-macro-global))
	  (setq answers (cons entry answers))))
    answers))

(defun TeX-tree-expand (vars program &optional subdirs)
  "Return directories corresponding to the kpathsea variables VARS.
This is done calling `kpsewhich --expand-path' for each variable.
PROGRAM is passed as the parameter for --progname.  SUBDIRS are
subdirectories which are appended to the directories of the TeX
trees.  Only existing directories are returned."
  (let (path-list path exit-status input-dir-list)
    (condition-case nil
	(dolist (var vars)
	  (setq path (with-output-to-string
		       (setq exit-status (call-process
					  "kpsewhich"  nil
					  (list standard-output nil) nil
					  "--progname" program
					  "--expand-path" var))))
	  (when (zerop exit-status)
	    (add-to-list 'path-list path t)))
      (error nil))
    (dolist (elt path-list)
      (let ((separators (if (string-match "^[A-Za-z]:" elt)
			    "[\n\r;]"
			  "[\n\r:]")))
	(dolist (item (condition-case nil
			  (split-string elt separators t)
			;; COMPATIBILITY for XEmacs <= 21.4.15
			(error (delete "" (split-string elt separators)))))
	  (if subdirs
	      (dolist (subdir subdirs)
		(setq path (file-name-as-directory (concat item subdir)))
		(when (file-exists-p path)
		  (add-to-list 'input-dir-list path t)))
	    (setq path (file-name-as-directory item))
	    (when (file-exists-p path)
	      (add-to-list 'input-dir-list path t))))))
    input-dir-list))

(defun TeX-macro-global ()
  "Return directories containing the site's TeX macro and style files."
  (or (TeX-tree-expand '("$SYSTEXMF" "$TEXMFLOCAL" "$TEXMFMAIN" "$TEXMFDIST")
		       "latex" '("/tex/" "/bibtex/bst/"))
      '("/usr/share/texmf/tex/" "/usr/share/texmf/bibtex/bst/")))

(defun TeX-macro-private ()
  "Return directories containing the user's TeX macro and style files."
  (TeX-tree-expand '("$TEXMFHOME") "latex" '("/tex/" "/bibtex/bst/")))

(defcustom TeX-macro-global (TeX-macro-global)
  "Directories containing the site's TeX macro and style files."
  :group 'TeX-file
  :type '(repeat (directory :format "%v")))

(defcustom TeX-macro-private (or (append (TeX-parse-path "TEXINPUTS")
					 (TeX-parse-path "BIBINPUTS"))
				 (TeX-macro-private))
  "Directories where you store your personal TeX macros."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(defcustom TeX-auto-private
  (list (expand-file-name TeX-auto-local
			  (or (and (boundp 'user-emacs-directory)
				   (concat user-emacs-directory "auctex/"))
			      "~/.emacs.d/auctex/")))
  "List of directories containing automatically generated AUCTeX style files.

These correspond to the personal TeX macros."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(if (stringp TeX-auto-private)		;Backward compatibility
    (setq TeX-auto-private (list TeX-auto-private)))

(defcustom TeX-style-private
  (list (expand-file-name TeX-style-local
			  (or (and (boundp 'user-emacs-directory)
				   (concat user-emacs-directory "auctex/"))
			      "~/.emacs.d/auctex/")))
  "List of directories containing hand-generated AUCTeX style files.

These correspond to the personal TeX macros."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(if (stringp TeX-style-private)		;Backward compatibility
    (setq TeX-style-private (list TeX-style-private)))

(defcustom TeX-style-path
  (let ((path))
    ;; Put directories in an order where the more local files can
    ;; override the more global ones.
    (mapcar (lambda (file) (when file (add-to-list 'path file t)))
	    (append (list TeX-auto-global TeX-style-global)
		    TeX-auto-private TeX-style-private
		    (list TeX-auto-local TeX-style-local)))
    path)
  "List of directories to search for AUCTeX style files.
Per default the list is built from the values of the variables
`TeX-auto-global', `TeX-style-global', `TeX-auto-private',
`TeX-style-private', `TeX-auto-local', and `TeX-style-local'."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(defcustom TeX-check-path
  (append (list ".") TeX-macro-private TeX-macro-global)
  "Directory path to search for dependencies.

If nil, just check the current file.
Used when checking if any files have changed."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

;;; Style Files

(defvar TeX-style-hook-list nil
  "List of TeX style hooks currently loaded.

Each entry is a list where the first element is the name of the style,
and the remaining elements are hooks to be run when that style is
active.")

(defcustom TeX-byte-compile nil
  "*Not nil means try to byte compile auto files before loading."
  :group 'TeX-parse
  :type 'boolean)

(defun TeX-load-style (style)
  "Search for and load each definition for STYLE in `TeX-style-path'."
  (cond ((assoc style TeX-style-hook-list)) ; We already found it
	((string-match "\\`\\(.+[/\\]\\)\\([^/\\]*\\)\\'" style) ;Complex path
	 (let* ((dir (substring style (match-beginning 1) (match-end 1)))
		(style (substring style (match-beginning 2) (match-end 2)))
		(master-dir (if (stringp TeX-master)
				(file-name-directory
				 (file-relative-name TeX-master))
			      "./"))
		(TeX-style-path (append (list (expand-file-name
					       TeX-auto-local dir)
					      (expand-file-name
					       TeX-auto-local master-dir)
					      (expand-file-name
					       TeX-style-local dir)
					      (expand-file-name
					       TeX-style-local master-dir))
					TeX-style-path)))
	   (TeX-load-style style)))
	(t				;Relative path
	 ;; Insert empty list to mark the fact that we have searched.
	 (setq TeX-style-hook-list (cons (list style) TeX-style-hook-list))
	 ;; Now check each element of the path
	 (dolist (name TeX-style-path)
	   (TeX-load-style-file (expand-file-name style name))))))

(defun TeX-load-style-file (file)
  "Load FILE checking for a Lisp extensions."
  (let ((el (concat file ".el"))
	(elc (concat file ".elc")))
    (cond ((file-newer-than-file-p el elc)
	   (if (file-readable-p el)
	       (if (and TeX-byte-compile
			(file-writable-p elc)
			(save-excursion
			  ;; `byte-compile-file' switches buffer in Emacs 20.3.
			  (byte-compile-file el))
			(file-readable-p elc))
		   (load-file elc)
		 (load-file el))))
	  ((file-readable-p elc)
	   (load-file elc))
	  ((file-readable-p el)
	   (load-file el)))))

(defun TeX-add-style-hook (style hook)
  "Give STYLE yet another HOOK to run."
  (let ((entry (assoc style TeX-style-hook-list)))
    (cond ((null entry)
	   ;; New style, add entry.
	   (setq TeX-style-hook-list (cons (list style hook)
					   TeX-style-hook-list)))
	  ((member hook entry)
	   ;; Old style, hook already there, do nothing.
	   nil)
	  (t
	   ;; Old style, new hook.
	   (setcdr entry (cons hook (cdr entry)))))))

(defun TeX-unload-style (style)
  "Forget that we once loaded STYLE."
  (cond ((null (assoc style TeX-style-hook-list)))
	((equal (car (car TeX-style-hook-list)) style)
	 (setq TeX-style-hook-list (cdr TeX-style-hook-list)))
	(t
	 (let ((entry TeX-style-hook-list))
	   (while (not (equal (car (car (cdr entry))) style))
	     (setq entry (cdr entry)))
	   (setcdr entry (cdr (cdr entry)))))))

(defcustom TeX-virgin-style (if (and TeX-auto-global
				     (file-directory-p TeX-auto-global))
				"virtex"
			      "NoVirtexSymbols")
  "Style all documents use."
  :group 'TeX-parse
  :type 'string)

(defvar TeX-active-styles nil
  "List of styles currently active in the document.")
 (make-variable-buffer-local 'TeX-active-styles)

(defun TeX-run-style-hooks (&rest styles)
  "Run the TeX style hooks STYLES."
  (mapcar (lambda (style)
	    ;; Avoid recursion.
	    (unless (TeX-member style TeX-active-styles 'string-equal)
	      (setq TeX-active-styles
		    (cons style TeX-active-styles))
	      (TeX-load-style style)
	      (let ((default-directory default-directory))
		;; Complex path.
		(when (string-match "\\`\\(.+[/\\]\\)\\([^/\\]*\\)\\'" style)
		  ;; Set `default-directory' to directory of master
		  ;; file since style files not stored in the fixed
		  ;; style directories are usually located there.
		  (setq default-directory (save-match-data
					    (TeX-master-directory))
			style (substring style
					 (match-beginning 2) (match-end 2))))
		(mapcar 'funcall
			(cdr-safe (assoc style TeX-style-hook-list))))))
	  styles))

(defcustom TeX-parse-self nil
  "Parse file after loading it if no style hook is found for it."
  :group 'TeX-parse
  :type 'boolean)

(defvar TeX-style-hook-applied-p nil
  "Nil, unless the style specific hooks have been applied.")
 (make-variable-buffer-local 'TeX-style-hook-applied-p)

(defvar TeX-update-style-hook nil
  "Hook run as soon as style specific hooks were applied.")

(defun TeX-update-style (&optional force)
  "Run style specific hooks for the current document.

Only do this if it has not been done before, or if optional argument
FORCE is not nil."
  (unless (or (and (boundp 'TeX-auto-update)
		   (eq TeX-auto-update 'BibTeX)) ; Not a real TeX buffer
	      (and (not force)
		   TeX-style-hook-applied-p))
    (setq TeX-style-hook-applied-p t)
    (message "Applying style hooks...")
    (TeX-run-style-hooks (TeX-strip-extension nil nil t))
    ;; Run parent style hooks if it has a single parent that isn't itself.
    (if (or (not (memq TeX-master '(nil t)))
	    (and (buffer-file-name)
		 (string-match TeX-one-master
			       (file-name-nondirectory (buffer-file-name)))))
	(TeX-run-style-hooks (TeX-master-file)))
    (if (and TeX-parse-self
	     (null (cdr-safe (assoc (TeX-strip-extension nil nil t)
				    TeX-style-hook-list))))
	(TeX-auto-apply))
    (run-hooks 'TeX-update-style-hook)
    (message "Applying style hooks... done")))

(defvar TeX-remove-style-hook nil
  "List of hooks to call when we remove the style specific information.")
 (make-variable-buffer-local 'TeX-remove-style-hook)

(defun TeX-remove-style ()
  "Remove all style specific information."
  (setq TeX-style-hook-applied-p nil)
  (run-hooks 'TeX-remove-style-hook)
  (setq TeX-active-styles (list TeX-virgin-style)))

(defun TeX-style-list ()
  "Return a list of all styles (subfiles) used by the current document."
  (TeX-update-style)
  TeX-active-styles)

;;; Special Characters

(defvar TeX-esc "\\" "The TeX escape character.")
 (make-variable-buffer-local 'TeX-esc)

(defvar TeX-grop "{" "The TeX group opening character.")
 (make-variable-buffer-local 'TeX-grop)

(defvar TeX-grcl "}" "The TeX group closing character.")
 (make-variable-buffer-local 'TeX-grcl)

;;; Symbols

;; Must be before keymaps.

(defgroup TeX-macro nil
  "Support for TeX macros in AUCTeX."
  :prefix "TeX-"
  :group 'AUCTeX)

(defcustom TeX-complete-word 'ispell-complete-word
  "*Function to call for completing non-macros in `tex-mode'."
  :group 'TeX-macro)

(defvar TeX-complete-list nil
  "List of ways to complete the preceding text.

Each entry is a list with the following elements:

0. Regexp matching the preceding text.
1. A number indicating the subgroup in the regexp containing the text.
2. A function returning an alist of possible completions.
3. Text to append after a succesful completion.

Or alternatively:

0. Regexp matching the preceding text.
1. Function to do the actual completion.")

(defun TeX-complete-symbol ()
  "Perform completion on TeX/LaTeX symbol preceding point."
  (interactive "*")
  (let ((list TeX-complete-list)
	entry)
    (while list
      (setq entry (car list)
	    list (cdr list))
      (if (TeX-looking-at-backward (car entry) 250)
	  (setq list nil)))
    (if (numberp (nth 1 entry))
	(let* ((sub (nth 1 entry))
	       (close (nth 3 entry))
	       (begin (match-beginning sub))
	       (end (match-end sub))
	       (pattern (TeX-match-buffer 0))
	       (symbol (buffer-substring begin end))
	       (list (funcall (nth 2 entry)))
	       (completion (try-completion symbol list))
	       (buf-name "*Completions*"))
	  (cond ((eq completion t)
		 (and close
		      (not (looking-at (regexp-quote close)))
		      (insert close))
		 (let ((window (get-buffer-window buf-name)))
		   (when window (delete-window window))))
		((null completion)
		 (error "Can't find completion for \"%s\"" pattern))
		((not (string-equal symbol completion))
		 (delete-region begin end)
		 (insert completion)
		 (and close
		      (eq (try-completion completion list) t)
		      (not (looking-at (regexp-quote close)))
		      (insert close))
		 (let ((window (get-buffer-window buf-name)))
		   (when window (delete-window window))))
		(t
		 (if (fboundp 'completion-in-region)
		     (completion-in-region begin end
					   (all-completions symbol list nil))
		   (message "Making completion list...")
		   (let ((list (all-completions symbol list nil)))
		     (with-output-to-temp-buffer buf-name
		       (display-completion-list list)))
		   (set-window-dedicated-p (get-buffer-window buf-name) 'soft)
		   (message "Making completion list...done")))))
      (funcall (nth 1 entry)))))

(defcustom TeX-default-macro "ref"
  "*The default macro when creating new ones with `TeX-insert-macro'."
  :group 'TeX-macro
  :type 'string)

(make-variable-buffer-local 'TeX-default-macro)

(defcustom TeX-insert-braces t
  "*If non-nil, append a empty pair of braces after inserting a macro."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-insert-macro-default-style 'show-optional-args
  "Specifies whether `TeX-insert-macro' will ask for all optional arguments.

If set to the symbol `show-optional-args', `TeX-insert-macro' asks for
optional arguments of TeX marcos.  If set to `mandatory-args-only',
`TeX-insert-macro' asks only for mandatory argument.

When `TeX-insert-macro' is called with \\[universal-argument], it's the other
way round.

Note that for some macros, there are special mechanisms, see e.g.
`LaTeX-includegraphics-options-alist'."
  :group 'TeX-macro
  :type '(choice (const mandatory-args-only)
		 (const show-optional-args)))

(defvar TeX-arg-opening-brace nil
  "String used as an opening brace for argument insertion.
The variable will be temporarily let-bound with the necessary value.")

(defvar TeX-arg-closing-brace nil
  "String used as a closing brace for argument insertion.
The variable will be temporarily let-bound with the necessary value.")

(defvar TeX-after-insert-macro-hook nil
  "A hook run after `TeX-insert-macro'.")

(defvar TeX-macro-history nil)

(defun TeX-insert-macro (symbol)
  "Insert TeX macro SYMBOL with completion.

AUCTeX knows of some macros and may query for extra arguments, depending on
the value of `TeX-insert-macro-default-style' and whether `TeX-insert-macro'
is called with \\[universal-argument]."
  ;; When called with a prefix (C-u), only ask for mandatory arguments,
  ;; i.e. all optional arguments are skipped.  See `TeX-parse-arguments' for
  ;; details.  Note that this behavior may be changed in favor of a more
  ;; flexible solution in the future, therefore we don't document it at the
  ;; moment.
  (interactive (list (completing-read (concat "Macro (default "
					      TeX-default-macro
					      "): "
					      TeX-esc)
				      (TeX-symbol-list) nil nil nil
				      'TeX-macro-history)))
  (cond ((string-equal symbol "")
	 (setq symbol TeX-default-macro))
	((interactive-p)
	 (setq TeX-default-macro symbol)))
  (TeX-parse-macro symbol (cdr-safe (assoc symbol (TeX-symbol-list))))
  (run-hooks 'TeX-after-insert-macro-hook))

(defvar TeX-electric-macro-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " 'minibuffer-complete-and-exit)
    map))

(defun TeX-electric-macro ()
  "Insert TeX macro with completion.

AUCTeX knows of some macros, and may query for extra arguments.
Space will complete and exit."
  (interactive)
  (cond ((eq (preceding-char) ?\\)
	 (call-interactively 'self-insert-command))
	((eq (preceding-char) ?.)
	 (let ((TeX-default-macro " ")
	       (minibuffer-local-completion-map TeX-electric-macro-map))
	   (call-interactively 'TeX-insert-macro)))
	(t
	 (let ((minibuffer-local-completion-map TeX-electric-macro-map))
	   (call-interactively 'TeX-insert-macro)))))

(defun TeX-parse-macro (symbol args)
  "How to parse TeX macros which takes one or more arguments.

First argument SYMBOL is the name of the macro.

If called with no additional arguments, insert macro with point
inside braces.  Otherwise, each argument of this function should
match an argument to the TeX macro.  What is done depend on the
type of ARGS:

  string: Use the string as a prompt to prompt for the argument.

  number: Insert that many braces, leave point inside the first.

  nil: Insert empty braces.

  t: Insert empty braces, leave point between the braces.

  other symbols: Call the symbol as a function.  You can define
  your own hook, or use one of the predefined argument hooks.  If
  you add new hooks, you can assume that point is placed directly
  after the previous argument, or after the macro name if this is
  the first argument.  Please leave point located after the
  argument you are inserting.  If you want point to be located
  somewhere else after all hooks have been processed, set the value
  of `exit-mark'.  It will point nowhere, until the argument hook
  set it.  By convention, these hooks all start with `TeX-arg-'.

  list: If the car is a string, insert it as a prompt and the next
  element as initial input.  Otherwise, call the car of the list
  with the remaining elements as arguments.

  vector: Optional argument.  If it has more than one element,
  parse it as a list, otherwise parse the only element as above.
  Use square brackets instead of curly braces, and is not inserted
  on empty user input."

  (if (and (TeX-active-mark)
	   (> (point) (mark)))
      (exchange-point-and-mark))
  (insert TeX-esc symbol)
  (let ((exit-mark (make-marker))
	(position (point)))
    (TeX-parse-arguments args)
    (cond ((marker-position exit-mark)
	   (goto-char (marker-position exit-mark))
	   (set-marker exit-mark nil))
	  ((and TeX-insert-braces
		;; Do not add braces if the argument is 0 or -1.
		(not (and (= (safe-length args) 1)
			  (numberp (car args))
			  (<= (car args) 0)))
		(equal position (point))
		(string-match "[a-zA-Z]+" symbol)
		(not (texmathp)))
	   (insert TeX-grop)
	   (if (TeX-active-mark)
	       (progn
		 (exchange-point-and-mark)
		 (insert TeX-grcl))
	     (insert TeX-grcl)
	     (backward-char))))))

(defun TeX-arg-string (optional &optional prompt initial-input)
  "Prompt for a string.

If OPTIONAL is not nil then the PROMPT will start with ``(Optional) ''.
INITIAL-INPUT is a string to insert before reading input."
  (TeX-argument-insert
   (if (and (not optional) (TeX-active-mark))
       (let ((TeX-argument (buffer-substring (point) (mark))))
	 (delete-region (point) (mark))
	 TeX-argument)
     (read-string (TeX-argument-prompt optional prompt "Text") initial-input))
   optional))

(defun TeX-parse-arguments (args)
  "Parse TeX macro arguments ARGS.

See `TeX-parse-macro' for details."
  (let ((last-optional-rejected nil)
	skip-opt)
    ;; Maybe get rid of all optional arguments.  See `TeX-insert-macro' for
    ;; more comments.  See `TeX-insert-macro-default-style'.
    (when (or (and (eq TeX-insert-macro-default-style 'show-optional-args)
		   (equal current-prefix-arg '(4)))
	      (and (eq TeX-insert-macro-default-style 'mandatory-args-only)
		   (null (equal current-prefix-arg '(4)))))
      (while (vectorp (car args))
	(setq args (cdr args))))

    (while args
      (if (vectorp (car args))
	  (unless last-optional-rejected
	    (let ((TeX-arg-opening-brace LaTeX-optop)
		  (TeX-arg-closing-brace LaTeX-optcl))
	      (TeX-parse-argument t (if (equal (length (car args)) 1)
					(aref (car args) 0)
				      (append (car args) nil)))))
	(let ((TeX-arg-opening-brace TeX-grop)
	      (TeX-arg-closing-brace TeX-grcl))
	  (setq last-optional-rejected nil)
	  (TeX-parse-argument nil (car args))))
      (setq args (cdr args)))))

(defun TeX-parse-argument (optional arg)
  "Depending on OPTIONAL, insert TeX macro argument ARG.
If OPTIONAL is set, only insert if there is anything to insert, and
then use square brackets instead of curly braces.

See `TeX-parse-macro' for details."
  (let (insert-flag)
    (cond ((stringp arg)
	   (TeX-arg-string optional arg)
	   (setq insert-flag t))
	  ((numberp arg)
	   (cond ((< arg 0)
		  (when (TeX-active-mark)
		    ;; Put both the macro and the marked region in a TeX group.
		    (let ((beg (min (point) (mark)))
			  (end (set-marker (make-marker) (max (point) (mark)))))
		      (insert " ")
		      (goto-char beg)
		      (skip-chars-backward "^\\\\")
		      (backward-char)
		      (insert TeX-arg-opening-brace)
		      (goto-char (marker-position end))
		      (insert TeX-arg-closing-brace)
		      (setq insert-flag t))))
		 ((= arg 0)) ; nop for clarity
		 ((> arg 0)
		  (TeX-parse-argument optional t)
		  (while (> arg 1)
		    (TeX-parse-argument optional nil)
		    (setq arg (- arg 1))))))
	  ((null arg)
	   (insert TeX-arg-opening-brace)
	   (when (and (not optional) (TeX-active-mark))
	     (exchange-point-and-mark))
	   (insert TeX-arg-closing-brace)
	   (setq insert-flag t))
	  ((eq arg t)
	   (insert TeX-arg-opening-brace)
	   (if (and (not optional) (TeX-active-mark))
	       (progn
		 (exchange-point-and-mark))
	     (set-marker exit-mark (point)))
	   (insert TeX-arg-closing-brace)
	   (setq insert-flag t))
	  ((symbolp arg)
	   (funcall arg optional))
	  ((listp arg)
	   (let ((head (car arg))
		 (tail (cdr arg)))
	     (cond ((stringp head)
		    (apply 'TeX-arg-string optional arg))
		   ((symbolp head)
		    (apply head optional tail))
		   (t (error "Unknown list argument type %s"
			     (prin1-to-string head))))))
	  (t (error "Unknown argument type %s" (prin1-to-string arg))))
    (when (and insert-flag (not optional) (TeX-active-mark))
      (TeX-deactivate-mark))))

(defun TeX-argument-insert (name optional &optional prefix)
  "Insert NAME surrounded by curly braces.

If OPTIONAL, only insert it if not empty, and then use square brackets.
If PREFIX is given, insert it before NAME."
  (if (and optional (string-equal name ""))
      (setq last-optional-rejected t)
    (insert TeX-arg-opening-brace)
    (if prefix
	(insert prefix))
    (if (and (string-equal name "")
	     (null (marker-position exit-mark)))
	(set-marker exit-mark (point))
      (insert name))
    (insert TeX-arg-closing-brace)))

(defun TeX-argument-prompt (optional prompt default &optional complete)
  "Return a argument prompt.

If OPTIONAL is not nil then the prompt will start with ``(Optional) ''.

PROMPT will be used if not nil, otherwise use DEFAULT.

Unless optional argument COMPLETE is non-nil, ``: '' will be appended."
  (concat (if optional "(Optional) " "")
	  (if prompt prompt default)
	  (if complete "" ": ")))

(defun TeX-string-divide-number-unit (string)
  "Divide number and unit in STRING.
Return the number as car and unit as cdr."
  (if (string-match "[0-9]*\\.?[0-9]+" string)
      (list (substring string 0 (string-match "[^.0-9]" string))
	    (substring string (if (string-match "[^.0-9]" string)
				  (string-match "[^.0-9]" string)
				(length string))))
    (list "" string)))

(defcustom TeX-default-unit-for-image "cm"
  "Default unit when prompting for an image size."
  :group 'TeX-macro
  :type '(choice (const "cm")
		 (const "in")
		 (const "\\linewidth")
		 (string :tag "Other")))

(defun TeX-arg-maybe (symbol list form)
  "Evaluates FORM, if SYMBOL is an element of LIST."
  (when (memq symbol list)
    (eval form)))

(defun TeX-arg-free (optional &rest args)
  "Parse its arguments but use no braces when they are inserted."
  (let ((TeX-arg-opening-brace "")
	(TeX-arg-closing-brace ""))
    (if (equal (length args) 1)
	(TeX-parse-argument optional (car args))
      (TeX-parse-argument optional args))))

(defun TeX-arg-literal (optional &rest args)
  "Insert its arguments ARGS into the buffer.
Used for specifying extra syntax for a macro."
  ;; FIXME: What is the purpose of OPTIONAL here?  -- rs
  (apply 'insert args))


;;; Font Locking

(defcustom TeX-install-font-lock 'font-latex-setup
  "Function to call to install font lock support.
Choose `ignore' if you don't want AUCTeX to install support for font locking."
  :group 'TeX-misc
  :type '(radio (function-item font-latex-setup)
		(function-item tex-font-setup)
		(function-item ignore)
		(function :tag "Other")))

;;; The Mode

(defvar TeX-format-list
  '(("JLATEX" japanese-latex-mode
     "\\\\\\(documentstyle\\|documentclass\\)[^%\n]*{\\(j[s-]?\\|t\\)\
\\(article\\|report\\|book\\|slides\\)")
    ("JTEX" japanese-plain-tex-mode
     "-- string likely in Japanese TeX --")
    ("AMSTEX" ams-tex-mode
     "\\\\document\\b")
    ("CONTEXT" context-mode
     "\\\\\\(start\\(text\\|tekst\\|proje[ck]t\\|proiect\\|\
produ[ck]t\\|produs\\|environment\\|omgeving\\|umgebung\\|prostredi\\|mediu\\|\
component\\|onderdeel\\|komponent[ea]\\|componenta\\)\
\\|inizia\\(testo\\|progetto\\|prodotto\\|ambiente\\|componente\\)\
\\)\\|%.*?interface=")
    ("LATEX" latex-mode
     "\\\\\\(begin\\|\\(?:sub\\)\\{0,2\\}section\\|chapter\\|documentstyle\\|\
documentclass\\)\\b")
    ("TEX" plain-tex-mode "."))
  "*List of format packages to consider when choosing a TeX mode.

A list with an entry for each format package available at the site.

Each entry is a list with three elements.

1. The name of the format package.
2. The name of the major mode.
3. A regexp typically matched in the beginning of the file.

When entering `tex-mode', each regexp is tried in turn in order to find
the major mode to be used.")

(defcustom TeX-default-mode 'latex-mode
  "*Mode to enter for a new file when it can't be determined otherwise."
  :group 'TeX-misc
  :type '(radio (function-item latex-mode)
		(function-item plain-tex-mode)
		(function :tag "Other")))

(defcustom TeX-force-default-mode nil
  "*If set to nil, try to infer the mode of the file from its content."
  :group 'TeX-misc
  :type 'boolean)

;;;###autoload
(defun TeX-tex-mode ()
  "Major mode in AUCTeX for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) if the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen
   2) If \\documentstyle or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `latex-mode' is selected.
   3) Otherwise, use `plain-tex-mode'"
  (interactive)

  (funcall (if (or (equal (buffer-size) 0)
		   TeX-force-default-mode)
	       TeX-default-mode
	     (save-excursion
	       (goto-char (point-min))
	       (let ((comment-start-skip ;Used by TeX-in-comment
		      (concat
		       "\\(\\(^\\|[^\\\n]\\)\\("
		       (regexp-quote TeX-esc)
		       (regexp-quote TeX-esc)
		       "\\)*\\)\\(%+ *\\)"))
		     (entry TeX-format-list)
		     answer)
		 (while (and entry (not answer))
		   (if (re-search-forward (nth 2 (car entry))
					  10000 t)
		       (if (not (TeX-in-comment))
			   (setq answer (nth 1 (car entry))))
		     (setq entry (cdr entry))))
		 (if answer
		     answer
		   TeX-default-mode))))))

(defun VirTeX-common-initialization ()
  "Perform basic initialization."
  (kill-all-local-variables)
  (setq TeX-mode-p t)
  (setq TeX-output-extension (if TeX-PDF-mode "pdf" "dvi"))
  (setq indent-tabs-mode nil)

  ;; Ispell support
  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex)
  (make-local-variable 'ispell-tex-p)
  (setq ispell-tex-p t)
  
  ;; Redefine some standard variables
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip
	(concat
	 "\\(\\(^\\|[^\\\n]\\)\\("
	 (regexp-quote TeX-esc)
	 (regexp-quote TeX-esc)
	 "\\)*\\)\\(%+[ \t]*\\)"))
  (set (make-local-variable 'comment-end-skip) "[ \t]*\\(\\s>\\|\n\\)")
  (set (make-local-variable 'comment-use-syntax) t)
  ;; `comment-padding' is defined here as an integer for compatibility
  ;; reasons because older Emacsen could not cope with a string.
  (make-local-variable 'comment-padding)
  (setq comment-padding 1)
  ;; Removed as commenting in (La)TeX is done with one `%' not two
  ;; (make-local-variable 'comment-add)
  ;; (setq comment-add 1) ;default to `%%' in comment-region
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'TeX-comment-indent)
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'compile-command)
  (unless (boundp 'compile-command)
    (setq compile-command "make"))
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes nil)

  ;; Make TAB stand out
  ;;  (make-local-variable 'buffer-display-table)
  ;;  (setq buffer-display-table (if standard-display-table
  ;;				 (copy-sequence standard-display-table)
  ;;			       (make-display-table)))
  ;;  (aset buffer-display-table ?\t (apply 'vector (append "<TAB>" nil)))

  ;; Symbol completion.
  (make-local-variable 'TeX-complete-list)
  (setq TeX-complete-list
	(list (list "\\\\\\([a-zA-Z]*\\)"
		    1 'TeX-symbol-list (if TeX-insert-braces "{}"))
	      (list "" TeX-complete-word)))

  (funcall TeX-install-font-lock)

  ;; We want this to be early in the list, so we do not add it before
  ;; we enter TeX mode  the first time.
  (if (boundp 'local-write-file-hooks)
      (add-hook 'local-write-file-hooks 'TeX-safe-auto-write)
    (add-hook 'write-file-hooks 'TeX-safe-auto-write))
  (make-local-variable 'TeX-auto-update)
  (setq TeX-auto-update t)

  ;; Minor modes
  (when TeX-source-correlate-mode
    (TeX-source-correlate-mode 1))

  ;; Let `TeX-master-file' be called after a new file was opened and
  ;; call `TeX-update-style' on any file opened.  (The addition to the
  ;; hook has to be made here because its local value will be deleted
  ;; by `kill-all-local-variables' if it is added e.g. in `tex-mode'.)
  ;;
  ;; `TeX-update-style' has to be called before
  ;; `global-font-lock-mode', which may also be specified in
  ;; `find-file-hooks', gets called.  Otherwise style-based
  ;; fontification will break (in XEmacs).  That means, `add-hook'
  ;; cannot be called with a non-nil value of the APPEND argument.
  ;;
  ;; `(TeX-master-file nil nil t)' has to be called *before*
  ;; `TeX-update-style' as the latter will call `TeX-master-file'
  ;; without the `ask' bit set.
  (when (and (featurep 'xemacs) (not (emacs-version>= 21 5)))
    (make-local-hook 'find-file-hooks))
  (add-hook 'find-file-hooks
	    (lambda ()
	      ;; Check if we are looking at a new or shared file.
	      (when (or (not (file-exists-p (buffer-file-name)))
			(eq TeX-master 'shared))
		(TeX-master-file nil nil t))
	      (TeX-update-style t)) nil t))


;;; Hilighting

(if (boundp 'hilit-patterns-alist)
    (let ((latex-patterns (cdr-safe (assq 'latex-mode hilit-patterns-alist)))
	  (plain-tex-patterns (cdr-safe (assq 'plain-tex-mode
					      hilit-patterns-alist))))
      (if (and latex-patterns plain-tex-patterns)
	  (setq hilit-patterns-alist
		(append (list (cons 'ams-tex-mode plain-tex-patterns))
			hilit-patterns-alist)))))

;;; Parsing

(defgroup TeX-parse nil
  "Parsing TeX files from AUCTeX."
  :group 'AUCTeX)

(defvar TeX-auto-parser '((styles TeX-auto-file TeX-run-style-hooks)))
;; Alist of parsed information.
;; Each entry is a list with the following elements:
;;
;; 0. Name of information type.
;; 1. Name of temporary variable used when parsing.
;; 2. Name of function to add information to add to #3.
;; 3. Name of variable holding buffer local information.
;; 4. Name of variable indicating that #3 has changed.


(defconst TeX-auto-parser-temporary 1)
(defconst TeX-auto-parser-add 2)
(defconst TeX-auto-parser-local 3)
(defconst TeX-auto-parser-change 4)

(defun TeX-auto-add-type (name prefix &optional plural)
  "Add information about NAME to the parser using PREFIX.

Optional third argument PLURAL is the plural form of TYPE.
By default just add an `s'.

This function create a set of variables and functions to maintain a
separate type of information in the parser."
  (let* ((names (or plural (concat name "s")))
	 (tmp (intern (concat prefix "-auto-" name)))
	 (add (intern (concat prefix "-add-" names)))
	 (local (intern (concat prefix "-" name "-list")))
	 (change (intern (concat prefix "-" name "-changed"))))
    (setq TeX-auto-parser
	  (cons (list name tmp add local change) TeX-auto-parser))
    (set local nil)
    (make-variable-buffer-local local)
    (set change nil)
    (make-variable-buffer-local change)
    (fset add `(lambda (&rest entries)
		 ,(concat "Add information about " (upcase name)
			  " to the current buffer.
Generated by `TeX-auto-add-type'.")
		 (TeX-auto-add-information ,name entries)))
    (fset local `(lambda nil
		   ,(concat "List of " names
			    " active in the current buffer.
Generated by `TeX-auto-add-type'.")
		   (TeX-auto-list-information ,name)))
    (add-hook 'TeX-remove-style-hook
	      `(lambda nil (setq ,(symbol-name local) nil)))))

(defun TeX-auto-add-information (name entries)
  "For NAME in `TeX-auto-parser' add ENTRIES."
  (let* ((entry (assoc name TeX-auto-parser))
	 (change (nth TeX-auto-parser-change entry))
	 (change-value (symbol-value change))
	 (local (nth TeX-auto-parser-local entry))
	 (local-value (symbol-value local)))
    (if change-value
	(set local (cons entries local-value))
      (set change t)
      (set local (list entries local-value)))))

(defun TeX-auto-list-information (name)
  "Return information in `TeX-auto-parser' about NAME."
  (TeX-update-style)
  (let* ((entry (assoc name TeX-auto-parser))
	 (change (nth TeX-auto-parser-change entry))
	 (change-value (symbol-value change))
	 (local (nth TeX-auto-parser-local entry)))
    (if (not change-value)
	()
      (set change nil)
      ;; Sort it
      (message "Sorting %s..." name)
      (set local
	   (sort (mapcar 'TeX-listify (apply 'append (symbol-value local)))
		 'TeX-car-string-lessp))
      ;; Make it unique
      (message "Removing duplicates...")
      (let ((entry (symbol-value local)))
	(while (and entry (cdr entry))
	  (let ((this (car entry))
		(next (car (cdr entry))))
	    (if (not (string-equal (car this) (car next)))
		(setq entry (cdr entry))
	      ;; We have two equal symbols.  Use the one with
	      ;; most arguments.
	      (if (> (length next) (length this))
		  (setcdr this (cdr next)))
	      (setcdr entry (cdr (cdr entry)))))))
      (message "Removing duplicates... done"))
    (symbol-value local)))

(TeX-auto-add-type "symbol" "TeX")

(defvar TeX-auto-apply-hook nil
  "Hook run when a buffer is parsed and the information is applied.")

(defun TeX-auto-apply ()
  "Parse and apply TeX information in the current buffer."
  (TeX-auto-parse)
  (run-hooks 'TeX-auto-apply-hook)
  (mapcar 'TeX-auto-apply-entry TeX-auto-parser))

(defun TeX-auto-apply-entry (entry)
  "Apply the information in ENTRY in `TeX-auto-parser'."
  (let ((value (symbol-value (nth TeX-auto-parser-temporary entry)))
	(add (nth TeX-auto-parser-add entry)))
    (if value (apply add value))))

(defun TeX-safe-auto-write ()
  "Call `TeX-auto-write' safely."
  (condition-case name
      (and (boundp 'TeX-auto-update)
	   TeX-auto-update
	   (TeX-auto-write))
    (error nil))
  ;; Continue with the other write file hooks.
  nil)

(defcustom TeX-auto-save nil
  "*Automatically save style information when saving the buffer."
  :group 'TeX-parse
  :type 'boolean)

(defcustom TeX-auto-untabify nil
  "*Automatically untabify when saving the buffer."
  :group 'TeX-parse
  :type 'boolean)

(defun TeX-auto-write ()
  "Save all relevant TeX information from the current buffer."
  (if TeX-auto-untabify
      (untabify (point-min) (point-max)))
  (if (and TeX-auto-save TeX-auto-local)
      (let* ((file (expand-file-name
		    (concat
		     (file-name-as-directory TeX-auto-local)
		     (TeX-strip-extension nil TeX-all-extensions t)
		     ".el")
		    (TeX-master-directory)))
	     (dir (file-name-directory file)))
	;; Create auto directory if possible.
	(if (not (file-exists-p dir))
	    (condition-case name
		(make-directory dir)
	      (error nil)))
	(if (file-writable-p file)
	    (save-excursion
	      (TeX-update-style)
	      (TeX-auto-store file))
	  (message "Can't write style information.")))))

(defcustom TeX-macro-default (car-safe TeX-macro-private)
  "*Default directory to search for TeX macros."
  :group 'TeX-file
  :type 'directory)

(defcustom TeX-auto-default (car-safe TeX-auto-private)
  "*Default directory to place automatically generated TeX information."
  :group 'TeX-file
  :type 'directory)

(defcustom TeX-ignore-file
  "\\(^\\|[/\\]\\)\\(\\.\\|\\.\\.\\|RCS\\|SCCS\\|CVS\\|babel\\..*\\)$"
  "Regular expression matching file names to ignore.

These files or directories will not be considered when searching for
TeX files in a directory."
  :group 'TeX-parse
  :type 'regexp)

(defcustom TeX-file-recurse t
  "Whether to search TeX directories recursively.
nil means do not recurse, a positive integer means go that far deep in the
directory hierarchy, t means recurse indefinitely."
  :group 'TeX-parse
  :type '(choice (const :tag "On" t)
		 (const :tag "Off" nil)
		 (integer :tag "Depth" :value 1)))

;;;###autoload
(defun TeX-auto-generate (tex auto)
  "Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory."
  (interactive (list (setq TeX-macro-default
			   (expand-file-name (read-file-name
					      "TeX file or directory: "
					      TeX-macro-default
					      TeX-macro-default 'confirm)))
		     (setq TeX-auto-default
			   (expand-file-name (read-file-name
					      "AUTO lisp directory: "
					      TeX-auto-default
					      TeX-auto-default 'confirm)))))
  (cond ((not (file-readable-p tex)))
	((string-match TeX-ignore-file tex))
	((file-directory-p tex)
	 (let ((files (directory-files (expand-file-name tex)))
	       (default-directory (file-name-as-directory
				   (expand-file-name tex)))
	       (TeX-file-recurse (cond ((symbolp TeX-file-recurse)
					TeX-file-recurse)
				       ((zerop TeX-file-recurse)
					nil)
				       ((1- TeX-file-recurse)))))
	   (mapcar (lambda (file)
		     (if (or TeX-file-recurse
			     (not (file-directory-p file)))
			 (TeX-auto-generate file auto)))
		   files)))
	((not (file-newer-than-file-p
	       tex
	       (concat (file-name-as-directory auto)
		       (TeX-strip-extension tex TeX-all-extensions t)
		       ".el"))))
	((TeX-match-extension tex (TeX-delete-duplicate-strings
				   (append TeX-file-extensions
					   BibTeX-file-extensions
					   TeX-Biber-file-extensions)))
	 (save-excursion
	   (set-buffer (let (enable-local-eval)
			 (find-file-noselect tex)))
	   (message "Parsing %s..." tex)
	   (TeX-auto-store (concat (file-name-as-directory auto)
				   (TeX-strip-extension tex
							TeX-all-extensions
							t)
				   ".el"))
	   (kill-buffer (current-buffer))
	   (message "Parsing %s... done" tex)))))

;;;###autoload
(defun TeX-auto-generate-global ()
  "Create global auto directory for global TeX macro definitions."
  (interactive)
  (unless (file-directory-p TeX-auto-global)
    (make-directory TeX-auto-global))
  (let ((TeX-file-extensions '("cls" "sty"))
	(BibTeX-file-extensions nil)
	(TeX-Biber-file-extensions nil))
    (mapc (lambda (macro) (TeX-auto-generate macro TeX-auto-global))
	  TeX-macro-global))
  (byte-recompile-directory TeX-auto-global 0))

(defun TeX-auto-store (file)
  "Extract information for AUCTeX from current buffer and store it in FILE."
  (TeX-auto-parse)

  (if (member nil (mapcar 'TeX-auto-entry-clear-p TeX-auto-parser))
      (let ((style (TeX-strip-extension nil TeX-all-extensions t)))
	(TeX-unload-style style)
	(save-excursion
	  (set-buffer (generate-new-buffer file))
	  (erase-buffer)
	  (insert "(TeX-add-style-hook \"" style "\"\n"
		  " (lambda ()")
	  (mapc (lambda (el) (TeX-auto-insert el style))
		TeX-auto-parser)
	  (insert "))\n\n")
	  (write-region (point-min) (point-max) file nil 'silent)
	  (kill-buffer (current-buffer))))
    (if (file-exists-p (concat file "c"))
	(delete-file (concat file "c")))
    (if (file-exists-p file)
	(delete-file file))))

(defun TeX-auto-entry-clear-p (entry)
  "Check if the temporary for `TeX-auto-parser' entry ENTRY is clear."
  ;; FIXME: This doc-string isn't clear to me.  -- rs
  (null (symbol-value (nth TeX-auto-parser-temporary entry))))

(defun TeX-auto-insert (entry &optional skip)
  "Insert code to initialize ENTRY from `TeX-auto-parser'.

If SKIP is not-nil, don't insert code for SKIP."
  (let ((name (symbol-name (nth TeX-auto-parser-add entry)))
	(list (symbol-value (nth TeX-auto-parser-temporary entry))))
    (unless (null list)
      (insert "\n    (" name)
      (dolist (el list)
	(cond ((and (stringp el) (not (string= el skip)))
	       (insert "\n     ")
	       (insert (prin1-to-string el)))
	      ((not (stringp el))
	       (insert "\n     ")
	       (insert "'" (prin1-to-string el)))))
      (insert ")"))))

(defvar TeX-auto-ignore
  '("csname" "filedate" "fileversion" "docdate" "next" "labelitemi"
    "labelitemii" "labelitemiii" "labelitemiv" "labelitemv"
    "labelenumi" "labelenumii" "labelenumiii" "labelenumiv"
    "labelenumv" "theenumi" "theenumii" "theenumiii" "theenumiv"
    "theenumv" "document" "par" "do" "expandafter")
  "List of symbols to ignore when scanning a TeX style file.")

(defcustom TeX-auto-regexp-list 'TeX-auto-full-regexp-list
  "List of regular expressions used for parsing the current file."
  :type '(radio (variable-item TeX-auto-empty-regexp-list)
		(variable-item TeX-auto-full-regexp-list)
		(variable-item plain-TeX-auto-regexp-list)
		(variable-item LaTeX-auto-minimal-regexp-list)
		(variable-item LaTeX-auto-label-regexp-list)
		(variable-item LaTeX-auto-regexp-list)
		(symbol :tag "Other")
		(repeat :tag "Specify"
			(group (regexp :tag "Match")
			       (sexp :tag "Groups")
			       symbol)))
  :group 'TeX-parse)
  (make-variable-buffer-local 'TeX-auto-regexp-list)

(defun TeX-auto-add-regexp (regexp)
  "Add REGEXP to `TeX-auto-regexp-list' if not already a member."
  (if (symbolp TeX-auto-regexp-list)
      (setq TeX-auto-regexp-list (symbol-value TeX-auto-regexp-list)))
  (or (memq regexp TeX-auto-regexp-list)
      (setq TeX-auto-regexp-list (cons regexp TeX-auto-regexp-list))))

(defvar TeX-auto-empty-regexp-list
  '(("<IMPOSSIBLE>\\(\\'\\`\\)" 1 ignore))
  "List of regular expressions guaranteed to match nothing.")

(defvar TeX-token-char
  (if (featurep 'mule)
      "\\(?:[a-zA-Z]\\|\\cj\\)"
    "[a-zA-Z]")
  "Regexp matching a character in a TeX macro.

Please use a shy group if you use a grouping construct, because
the functions/variables which use `TeX-token-char' expect not to
alter the numbering of any ordinary, non-shy groups.")

(defvar plain-TeX-auto-regexp-list
  (let ((token TeX-token-char))
    `((,(concat "\\\\def\\\\\\(" token "+\\)[^a-zA-Z@]")
       1 TeX-auto-symbol-check)
      (,(concat "\\\\let\\\\\\(" token "+\\)[^a-zA-Z@]")
       1 TeX-auto-symbol-check)
      (,(concat "\\\\font\\\\\\(" token "+\\)[^a-zA-Z@]") 1 TeX-auto-symbol)
      (,(concat "\\\\chardef\\\\\\(" token "+\\)[^a-zA-Z@]") 1 TeX-auto-symbol)
      (,(concat "\\\\new\\(?:count\\|dimen\\|muskip\\|skip\\)\\\\\\(" token
		"+\\)[^a-zA-Z@]")
       1 TeX-auto-symbol)
      (,(concat "\\\\newfont{?\\\\\\(" token "+\\)}?") 1 TeX-auto-symbol)
      (,(concat "\\\\typein\\[\\\\\\(" token "+\\)\\]") 1 TeX-auto-symbol)
      ("\\\\input +\\(\\.*[^#%\\\\\\.\n\r]+\\)\\(\\.[^#%\\\\\\.\n\r]+\\)?"
       1 TeX-auto-file)
      (,(concat "\\\\mathchardef\\\\\\(" token "+\\)[^a-zA-Z@]")
       1 TeX-auto-symbol)))
  "List of regular expression matching common LaTeX macro definitions.")

(defvar TeX-auto-full-regexp-list plain-TeX-auto-regexp-list
  "Full list of regular expression matching TeX macro definitions.")

(defvar TeX-auto-prepare-hook nil
  "List of hooks to be called before parsing a TeX file.")

(defvar TeX-auto-cleanup-hook nil
  "List of hooks to be called after parsing a TeX file.")

(defcustom TeX-auto-parse-length 999999
  "Maximal length of TeX file (in characters) that will be parsed."
  :group 'TeX-parse
  :type 'integer)
  (make-variable-buffer-local 'TeX-auto-parse-length)

(defcustom TeX-auto-x-parse-length 0
  "Maximum length of TeX file that will be parsed additionally.
Use `TeX-auto-x-regexp-list' for parsing the region between
`TeX-auto-parse-length' and this value."
  :group 'TeX-parse
  :type 'integer)
  (make-variable-buffer-local 'TeX-auto-x-parse-length)

(defcustom TeX-auto-x-regexp-list 'LaTeX-auto-label-regexp-list
  "List of regular expressions used for additional parsing.
See `TeX-auto-x-parse-length'."
  :type '(radio (variable-item TeX-auto-empty-regexp-list)
		(variable-item TeX-auto-full-regexp-list)
		(variable-item plain-TeX-auto-regexp-list)
		(variable-item LaTeX-auto-minimal-regexp-list)
		(variable-item LaTeX-auto-label-regexp-list)
		(variable-item LaTeX-auto-regexp-list)
		(symbol :tag "Other")
		(repeat :tag "Specify"
			(group (regexp :tag "Match")
			       (sexp :tag "Groups")
			       symbol)))
  :group 'TeX-parse)
  (make-variable-buffer-local 'TeX-auto-x-regexp-list)

(defun TeX-regexp-group-count (regexp)
  "Return number of groups in a REGEXP.  This is not foolproof:
you should not use something like `[\\(]' for a character range."
  (let (start (n 0))
    (while (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\([^?]"
			 regexp start)
      (setq start (- (match-end 0) 2)
	    n (1+ n)))
    n))

(defun TeX-auto-parse-region (regexp-list beg end)
  "Parse TeX information according to REGEXP-LIST between BEG and END."
  (if (symbolp regexp-list)
      (setq regexp-list (and (boundp regexp-list) (symbol-value regexp-list))))
   (if regexp-list
       ;; Extract the information.
       (let* (groups
	      (count 1)
	      (regexp (concat "\\("
			      (mapconcat
			       (lambda(x)
				 (push (cons count x) groups)
				 (setq count
				       (+ 1 count
					  (TeX-regexp-group-count (car x))))
				 (car x))
			       regexp-list "\\)\\|\\(")
			      "\\)"))
	      syms
	      lst)
	 (setq count 0)
	 (goto-char (if end (min end (point-max)) (point-max)))
	 (while (re-search-backward regexp beg t)
	   (let* ((entry (cdr (TeX-member nil groups
					  (lambda (a b)
					    (match-beginning (car b))))))
		  (symbol (nth 2 entry))
		  (match (nth 1 entry)))
	     (unless (TeX-in-comment)
	       (looking-at (nth 0 entry))
	       (if (fboundp symbol)
		   (funcall symbol match)
		 (puthash (if (listp match)
			      (mapcar #'TeX-match-buffer match)
			    (TeX-match-buffer match))
			  (setq count (1- count))
			  (cdr (or (assq symbol syms)
				   (car (push
					 (cons symbol
					       (make-hash-table :test 'equal))
					 syms)))))))))
	 (setq count 0)
	 (dolist (symbol syms)
	   (setq lst (symbol-value (car symbol)))
	   (while lst
	     (puthash (pop lst)
		      (setq count (1+ count))
		      (cdr symbol)))
	   (maphash (lambda (key value)
		      (push (cons value key) lst))
		    (cdr symbol))
	   (clrhash (cdr symbol))
	   (set (car symbol) (mapcar #'cdr (sort lst #'car-less-than-car)))))))


(defun TeX-auto-parse ()
  "Parse TeX information in current buffer.

Call the functions in `TeX-auto-prepare-hook' before parsing, and the
functions in `TeX-auto-cleanup-hook' after parsing."

  (let ((case-fold-search nil))

    (mapc 'TeX-auto-clear-entry TeX-auto-parser)
    (run-hooks 'TeX-auto-prepare-hook)

    (save-excursion
      (and (> TeX-auto-x-parse-length TeX-auto-parse-length)
	   (> (point-max) TeX-auto-parse-length)
	   (TeX-auto-parse-region TeX-auto-x-regexp-list
				  TeX-auto-parse-length
				  TeX-auto-x-parse-length))
      (TeX-auto-parse-region TeX-auto-regexp-list
			     nil TeX-auto-parse-length))

    ;; Cleanup ignored symbols.

    ;; NOTE: This is O(N M) where it could be O(N log N + M log M) if we
    ;; sorted the lists first.
    (while (member (car TeX-auto-symbol) TeX-auto-ignore)
      (setq TeX-auto-symbol (cdr TeX-auto-symbol)))
    (let ((list TeX-auto-symbol))
      (while (and list (cdr list))
	(if (member (car (cdr list)) TeX-auto-ignore)
	    (setcdr list (cdr (cdr list)))
	  (setq list (cdr list)))))

    (run-hooks 'TeX-auto-cleanup-hook)))

(defun TeX-auto-clear-entry (entry)
  "Set the temporary variable in ENTRY to nil."
  (set (nth TeX-auto-parser-temporary entry) nil))

(defvar LaTeX-auto-end-symbol nil)

(defun TeX-auto-symbol-check (match)
  "Add MATCH to TeX-auto-symbols.
Check for potential LaTeX environments."
  (let ((symbol (if (listp match)
		    (mapcar 'TeX-match-buffer match)
		  (TeX-match-buffer match))))
    (if (and (stringp symbol)
	     (string-match "^end\\(.+\\)$" symbol))
	(add-to-list 'LaTeX-auto-end-symbol
		     (substring symbol (match-beginning 1) (match-end 1)))
      (if (listp symbol)
	  (dolist (elt symbol)
	    (add-to-list 'TeX-auto-symbol elt))
	(add-to-list 'TeX-auto-symbol symbol)))))


;;; File Extensions

(defgroup TeX-file-extension nil
  "File extensions recognized by AUCTeX."
  :group 'TeX-file)

(defcustom TeX-file-extensions '("tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx")
  "*File extensions used by manually generated TeX files."
  :group 'TeX-file-extension
  :type '(repeat (string :format "%v")))

(defcustom TeX-all-extensions '("[^.\n]+")
  "All possible file extensions."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defcustom TeX-default-extension "tex"
  "*Default extension for TeX files."
  :group 'TeX-file-extension
  :type 'string)

  (make-variable-buffer-local 'TeX-default-extension)

(defvar TeX-doc-extensions
  '("dvi" "pdf" "ps" "txt" "html" "dvi.gz" "pdf.gz" "ps.gz" "txt.gz" "html.gz"
    "dvi.bz2" "pdf.bz2" "ps.bz2" "txt.bz2" "html.bz2")
  "File extensions of documentation files.")

(defcustom docTeX-default-extension "dtx"
  "*Default extension for docTeX files."
  :group 'TeX-file-extension
  :type 'string)

(defvar TeX-output-extension nil
  "Extension of TeX output file.
This is either a string or a list with
a string as element.  Its value is obtained from `TeX-command-output-list'.
Access to the value should be through the function `TeX-output-extension'.")

  (make-variable-buffer-local 'TeX-output-extension)

(defcustom TeX-Biber-file-extensions '("bib" "ris" "xml")
  "Valid file extensions for Biber files."
  :group 'TeX-file-extension
  :type '(repeat (string :format "%v")))

(defcustom BibTeX-file-extensions '("bib")
  "Valid file extensions for BibTeX files."
  :group 'TeX-file-extension
  :type '(repeat (string :format "%v")))

(defcustom BibTeX-style-extensions '("bst")
  "Valid file extensions for BibTeX styles."
  :group 'TeX-file-extension
  :type '(repeat (string :format "%v")))

(defun TeX-match-extension (file &optional extensions)
  "Return non-nil if FILE has one of EXTENSIONS.

If EXTENSIONS is not specified or nil, the value of
`TeX-file-extensions' is used instead."

  (if (null extensions)
      (setq extensions TeX-file-extensions))

  (let ((regexp (concat "\\.\\("
			(mapconcat 'identity extensions "\\|")
			"\\)$"))
	(case-fold-search t))
    (string-match regexp file)))

(defun TeX-strip-extension (&optional string extensions nodir nostrip)
  "Return STRING without any trailing extension in EXTENSIONS.
If NODIR is t, also remove directory part of STRING.
If NODIR is `path', remove directory part of STRING if it is equal to
the current directory, `TeX-macro-private' or `TeX-macro-global'.
If NOSTRIP is set, do not remove extension after all.
STRING defaults to the name of the current buffer.
EXTENSIONS defaults to `TeX-file-extensions'."

  (if (null string)
      (setq string (or (buffer-file-name) "<none>")))

  (if (null extensions)
      (setq extensions TeX-file-extensions))

  (let* ((strip (if (and (not nostrip)
			 (TeX-match-extension string extensions))
		    (substring string 0 (match-beginning 0))
		  string))
	 (dir (expand-file-name (or (file-name-directory strip) "./"))))
    (if (or (eq nodir t)
	    (string-equal dir (expand-file-name "./"))
	    (member dir TeX-macro-global)
	    (member dir TeX-macro-private))
	(file-name-nondirectory strip)
      strip)))


;;; File Searching

(defun TeX-tree-roots ()
  "Return a list of available TeX tree roots."
  (let (list)
    (dolist (dir (TeX-tree-expand '("$TEXMFHOME" "$TEXMFMAIN" "$TEXMFLOCAL"
				    "$TEXMFDIST") "latex"))
      (when (file-readable-p dir)
	(add-to-list 'list dir t)))
    list))

(defcustom TeX-tree-roots (TeX-tree-roots)
  "List of all available TeX tree root directories."
  :group 'TeX-file
  :type '(repeat directory))

(defcustom TeX-kpathsea-path-delimiter t
  "Path delimiter for kpathsea output.
t means autodetect, nil means kpathsea is disabled."
  :group 'TeX-file
  :type '(choice (const ":")
		 (const ";")
		 (const :tag "Autodetect" t)
		 (const :tag "Off" nil)))

;; We keep this function in addition to `TeX-search-files' because it
;; is faster.  Since it does not look further into subdirectories,
;; this comes at the price of finding a smaller number of files.
(defun TeX-search-files-kpathsea (var extensions scope nodir strip)
  "Return a list of files in directories determined by expanding VAR.
Only files which match EXTENSIONS are returned.  SCOPE defines
the scope for the search and can be `local' or `global' besides
nil.  If NODIR is non-nil, remove directory part.  If STRIP is
non-nil, remove file extension."
  (and TeX-kpathsea-path-delimiter
       (catch 'no-kpathsea
	 (let* ((dirs (if (eq scope 'local)
			  "."
			(with-output-to-string
			  (unless (zerop (call-process
					  "kpsewhich" nil
					  (list standard-output nil) nil
					  (concat "-expand-path=" var)))
			    (if (eq TeX-kpathsea-path-delimiter t)
				(throw 'no-kpathsea
				       (setq TeX-kpathsea-path-delimiter nil))
			      (error "kpsewhich error"))))))
		result)
	   (when (eq TeX-kpathsea-path-delimiter t)
	     (setq TeX-kpathsea-path-delimiter
		   (if (string-match ";" dirs) ";" ":")))
	   (unless TeX-kpathsea-path-delimiter
	     (throw 'no-kpathsea nil))
	   (setq dirs (delete "" (split-string
				  dirs (concat "[\n\r"
					       TeX-kpathsea-path-delimiter
					       "]+"))))
	   (if (eq scope 'global)
	       (delete "." dirs))
	   (setq extensions (concat "\\." (regexp-opt extensions t) "\\'")
		 result (apply #'append (mapcar (lambda (x)
						  (when (file-readable-p x)
						    (directory-files
						     x (not nodir) extensions)))
						dirs)))
	   (if strip
	       (mapcar (lambda(x)
			 (if (string-match extensions x)
			     (substring x 0 (match-beginning 0))
			   x))
		       result)
	     result)))))

(defun TeX-search-files (&optional directories extensions nodir strip)
  "Return a list of all reachable files in DIRECTORIES ending with EXTENSIONS.
If optional argument NODIR is set, remove directory part.
If optional argument STRIP is set, remove file extension.
If optional argument DIRECTORIES is set, search in those directories.
Otherwise, search in all TeX macro directories.
If optional argument EXTENSIONS is not set, use `TeX-file-extensions'"
  (when (null extensions)
    (setq extensions TeX-file-extensions))
  (when (null directories)
    (setq directories (cons "./" (append TeX-macro-private TeX-macro-global))))
  (let (match
	(TeX-file-recurse (cond ((symbolp TeX-file-recurse)
				 TeX-file-recurse)
				((zerop TeX-file-recurse)
				 nil)
				((1- TeX-file-recurse)))))
    (while directories
      (let* ((directory (car directories))
	     (content (and directory
			   (file-readable-p directory)
			   (file-directory-p directory)
			   (directory-files directory))))
	(setq directories (cdr directories))
	(while content
	  (let ((file (concat directory (car content))))
	    (setq content (cdr content))
	    (cond ((string-match TeX-ignore-file file))
		  ((not (file-readable-p file)))
		  ((file-directory-p file)
		   (if TeX-file-recurse
		       (setq match
			     (append match
				     (TeX-search-files
				      (list (file-name-as-directory file))
				      extensions nodir strip)))))
		  ((TeX-match-extension file extensions)
		   (setq match (cons (TeX-strip-extension
				      file extensions nodir (not strip))
				     match))))))))
    match))

;; The variables `TeX-macro-private' and `TeX-macro-global' are not
;; used for specifying the directories because the number of
;; directories to be searched should be limited as much as possible
;; and the TeX-macro-* variables are just too broad for this.
(defvar TeX-search-files-type-alist
  '((texinputs "${TEXINPUTS}" ("tex/") TeX-file-extensions)
    (docs "${TEXDOCS}" ("doc/") TeX-doc-extensions)
    (graphics "${TEXINPUTS}" ("tex/") LaTeX-includegraphics-extensions)
    (bibinputs "${BIBINPUTS}" ("bibtex/bib/") BibTeX-file-extensions)
    (bstinputs "${BSTINPUTS}" ("bibtex/bst/") BibTeX-style-extensions))
  "Alist of filetypes with locations and file extensions.
Each element of the alist consists of a symbol expressing the
filetype, a variable which can be expanded on kpathsea-based
systems into the directories where files of the given type
reside, a list of absolute directories, relative directories
below the root of a TDS-compliant TeX tree or a list of variables
with either type of directories as an alternative for
non-kpathsea-based systems and a list of extensions to be matched
upon a file search.  Note that the directories have to end with a
directory separator.

Each AUCTeX mode should set the variable buffer-locally with a
more specific value.  See `LateX-search-files-type-alist' for an
example.")

(defun TeX-search-files-by-type (filetype &optional scope nodir strip)
  "Return a list of files in TeX's search path with type FILETYPE.
FILETYPE is a symbol used to choose the search paths and
extensions.  See `TeX-search-file-type-alist' for supported
symbols.

The optional argument SCOPE sets the scope for the search.
Besides nil the symbols `local' and `global' are accepted.
`local' means to search in the current directory only, `global'
in the global directories only and nil in both.

If optional argument NODIR is non-nil, remove directory part.

If optional argument STRIP is non-nil, remove file extension."
  (let* ((spec (assq filetype TeX-search-files-type-alist))
	 (kpse-var (nth 1 spec))
	 (rawdirs (nth 2 spec))
	 (exts (nth 3 spec))
	 expdirs dirs local-files)
    (setq exts (if (symbolp exts) (eval exts) exts))
    (or (TeX-search-files-kpathsea kpse-var exts scope nodir strip)
	(progn
	  (unless (eq scope 'global)
	    (setq local-files
		  (let ((TeX-file-recurse nil))
		    (TeX-search-files '("./") exts nodir strip))))
	  (if (eq scope 'local)
	      local-files
	    (if (null TeX-tree-roots)
		(error "No TeX trees available; configure `TeX-tree-roots'")
	      ;; Expand variables.
	      (dolist (rawdir rawdirs)
		(if (symbolp rawdir)
		    (setq expdirs (append expdirs (eval rawdir)))
		  (add-to-list 'expdirs rawdir t)))
	      (delete-dups expdirs)
	      ;; Assumption: Either all paths are absolute or all are relative.
	      (if (file-name-absolute-p (car expdirs))
		  (setq dirs expdirs)
		;; Append relative TDS subdirs to all TeX tree roots.
		(dolist (root TeX-tree-roots)
		  (dolist (dir expdirs)
		    (add-to-list 'dirs (concat (file-name-as-directory root)
					       dir) t)))))
	    (append local-files (TeX-search-files dirs exts nodir strip)))))))


;;; Utilities
;;
;; Some of these functions has little to do with TeX, but nonetheless we
;; should use the "TeX-" prefix to avoid name clashes.

(defun TeX-car-string-lessp (s1 s2)
  "Compare the cars of S1 and S2 in lexicographic order.
Return t if first is less than second in lexicographic order."
  (string-lessp (car s1) (car s2)))

(defun TeX-listify (elt)
  "Return a newly created list with element ELT.
If ELT already is a list, return ELT."
  (if (listp elt) elt (list elt)))

(defun TeX-member (elt list how)
  "Return the member ELT in LIST.  Comparison done with HOW.
Return nil if ELT is not a member of LIST."
  (while (and list (not (funcall how elt (car list))))
    (setq list (cdr list)))
  (car-safe list))

(defun TeX-elt-of-list-member (elts list)
  "Return non-nil if an element of ELTS is a member of LIST."
  (catch 'found
    (dolist (elt elts)
      (when (member elt list)
	(throw 'found t)))))

(defun TeX-assoc (key list)
  "Return non-nil if KEY is `equal' to the car of an element of LIST.
Like assoc, except case insensitive."
  (let ((case-fold-search t))
    (TeX-member key list
		(lambda (a b)
		  (string-match (concat "^" (regexp-quote a) "$")
				(car b))))))

(defun TeX-match-buffer (n)
  "Return the substring corresponding to the N'th match.
See `match-data' for details."
  (if (match-beginning n)
      (buffer-substring-no-properties (match-beginning n) (match-end n))
    ""))

(defun TeX-function-p (arg)
  "Return non-nil if ARG is callable as a function."
  (or (and (fboundp 'byte-code-function-p)
	   (byte-code-function-p arg))
      (and (listp arg)
	   (eq (car arg) 'lambda))
      (and (symbolp arg)
	   (fboundp arg))))

(defun TeX-booleanp (arg)
  "Return non-nil if ARG is t or nil."
  (memq arg '(t nil)))

(defun TeX-looking-at-backward (regexp &optional limit)
  "Return non-nil if the text before point matches REGEXP.
Optional second argument LIMIT gives a max number of characters
to look backward for."
  (let ((pos (point)))
    (save-excursion
      (and (re-search-backward regexp
			       (if limit (max (point-min) (- (point) limit)))
			       t)
	   (eq (match-end 0) pos)))))

(defun TeX-current-line ()
  "The current line number."
  (format "%d" (1+ (TeX-current-offset))))

(defun TeX-current-file-name-master-relative ()
  "Return current filename, relative to master directory."
  (file-relative-name
   (buffer-file-name)
   (TeX-master-directory)))

(defun TeX-near-bobp ()
  "Return t iff there's nothing but whitespace between (bob) and (point)."
  (save-excursion
    (skip-chars-backward " \t\n")
    (bobp)))

(defun TeX-deactivate-mark ()
  "Deactivate the mark.
This is a compatibility function which works both in Emacs and
XEmacs.  In XEmacs the region is deactivated instead of the
mark which is sort of equivalent."
  (if (featurep 'xemacs)
      (zmacs-deactivate-region)
    (deactivate-mark)))

(defalias 'TeX-run-mode-hooks
  (if (fboundp 'run-mode-hooks) 'run-mode-hooks 'run-hooks))


;;; Syntax Table

(defvar TeX-mode-syntax-table (make-syntax-table)
  "Syntax table used while in TeX mode.")

 (make-variable-buffer-local 'TeX-mode-syntax-table)

(progn ; Define TeX-mode-syntax-table.
  (modify-syntax-entry (string-to-char TeX-esc)
			   "\\" TeX-mode-syntax-table)
  (modify-syntax-entry ?\f ">"  TeX-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  TeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char TeX-grop)
			   (concat "(" TeX-grcl)
				TeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char TeX-grcl)
			   (concat ")" TeX-grop)
				TeX-mode-syntax-table)
  (modify-syntax-entry ?%  "<"  TeX-mode-syntax-table)
  (modify-syntax-entry ?\" "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?&  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?_  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?@  "_"  TeX-mode-syntax-table)
  (modify-syntax-entry ?~  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?$  "$"  TeX-mode-syntax-table)
  (modify-syntax-entry ?'  "w"  TeX-mode-syntax-table)
  (modify-syntax-entry ?«  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?»  "."  TeX-mode-syntax-table))

;;; Menu Support

(defvar TeX-command-current 'TeX-command-master
  "Specify whether to run command on master, buffer or region.")
;; Function used to run external command.

(defun TeX-command-select-master ()
  "Determine that the next command will be on the master file."
  (interactive)
  (message "Next command will be on the master file.")
  (setq TeX-command-current 'TeX-command-master))

(defun TeX-command-select-buffer ()
  "Determine that the next command will be on the buffer."
  (interactive)
  (message "Next command will be on the buffer")
  (setq TeX-command-current 'TeX-command-buffer))

(defun TeX-command-select-region ()
  "Determine that the next command will be on the region."
  (interactive)
  (message "Next command will be on the region")
  (setq TeX-command-current 'TeX-command-region))

(defvar TeX-command-force nil)
;; If non-nil, TeX-command-query will return the value of this
;; variable instead of quering the user.

(defun TeX-command-menu (name)
  "Execute `TeX-command-list' NAME from a menu."
  (let ((TeX-command-force name))
    (funcall TeX-command-current)))

(defun TeX-command-menu-print (printer command name)
  "Print on PRINTER using method COMMAND to run NAME."
  (let ((TeX-printer-default (unless (string= printer "Other") printer))
	(TeX-printer-list (and (string= printer "Other") TeX-printer-list))
	(TeX-print-command command)
	(TeX-queue-command command))
    (TeX-command-menu name)))

(defun TeX-command-menu-printer-entry (entry lookup command name)
  "Return `TeX-printer-list' ENTRY as a menu item."
  (vector (nth 0 entry)
	  (list 'TeX-command-menu-print
		(nth 0 entry)
		(or (nth lookup entry) command)
		name)))

(defun TeX-command-menu-entry (entry)
  "Return `TeX-command-list' ENTRY as a menu item."
  (let ((name (car entry)))
    (cond ((and (string-equal name TeX-command-Print)
		TeX-printer-list)
	   (cons TeX-command-Print
		 (mapcar (lambda (entry)
			   (TeX-command-menu-printer-entry
			    entry 1 TeX-print-command name))
			 (append TeX-printer-list '(("Other"))))))
	  ((and (string-equal name TeX-command-Queue)
		TeX-printer-list)
	   (cons TeX-command-Queue
		 (mapcar (lambda (entry)
			   (TeX-command-menu-printer-entry
			    entry 2 TeX-queue-command name))
			 (append TeX-printer-list '(("Other"))))))
	  (t
	   (vconcat `(,name (TeX-command-menu ,name))
		    (nthcdr 5 entry))))))

(defconst TeX-command-menu-name "Command"
  "Name to be displayed for the command menu in all modes defined by AUCTeX.")

;;; Keymap

(defcustom TeX-electric-escape nil
  "If non-nil, ``\\'' will be bound to `TeX-electric-macro'."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-electric-sub-and-superscript nil
  "If non-nil, insert braces after typing `^' and `_' in math mode."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-newline-function 'newline
  "Function to be called upon pressing `RET'."
  :group 'TeX-indentation
  :type '(choice (const newline)
		 (const newline-and-indent)
		 (const reindent-then-newline-and-indent)
		 (sexp :tag "Other")))

(defun TeX-insert-backslash (arg)
  "Either insert typed key ARG times or call `TeX-electric-macro'.
`TeX-electric-macro' will be called if `TeX-electric-escape' is non-nil."
  (interactive "*p")
  (if TeX-electric-escape
      (TeX-electric-macro)
    (self-insert-command arg)))

(defun TeX-insert-sub-or-superscript (arg)
  "Insert typed key ARG times and possibly a pair of braces.
Brace insertion is only done if point is in a math construct and
`TeX-electric-sub-and-superscript' has a non-nil value."
  (interactive "*p")
  (self-insert-command arg)
  (when (and TeX-electric-sub-and-superscript (texmathp))
    (insert (concat TeX-grop TeX-grcl))
    (backward-char)))

(defun TeX-newline ()
  "Call the function specified by the variable `TeX-newline-function'."
  (interactive) (funcall TeX-newline-function))

(defvar TeX-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Standard
    ;; (define-key map "\177"     'backward-delete-char-untabify)
    (define-key map "\C-c}"    'up-list)
    (define-key map "\C-c#"    'TeX-normal-mode)
    (define-key map "\C-c\C-n" 'TeX-normal-mode)
    (define-key map "\C-c?"    'TeX-doc)
    (define-key map "\C-c\C-i" 'TeX-goto-info-page)
    (define-key map "\r"       'TeX-newline)
    
    ;; From tex.el
    (define-key map "\""       'TeX-insert-quote)
    (define-key map "$"        'TeX-insert-dollar)
    ;; Removed because LaTeX 2e have a better solution to italic correction.
    ;; (define-key map "."        'TeX-insert-punctuation)
    ;; (define-key map ","        'TeX-insert-punctuation)
    (define-key map "\C-c{"    'TeX-insert-braces)
    (define-key map "\C-c\C-f" 'TeX-font)
    (define-key map "\C-c\C-m" 'TeX-insert-macro)
    (define-key map "\\"       'TeX-insert-backslash)
    (define-key map "^"        'TeX-insert-sub-or-superscript)
    (define-key map "_"        'TeX-insert-sub-or-superscript)
    (define-key map "\e\t"     'TeX-complete-symbol) ;*** Emacs 19 way
    
    (define-key map "\C-c'"    'TeX-comment-or-uncomment-paragraph) ;*** Old way
    (define-key map "\C-c:"    'TeX-comment-or-uncomment-region) ;*** Old way
    (define-key map "\C-c\""   'TeX-uncomment) ;*** Old way
    
    (define-key map "\C-c;"    'TeX-comment-or-uncomment-region)
    (define-key map "\C-c%"    'TeX-comment-or-uncomment-paragraph)
    
    (define-key map "\C-c\C-t\C-p"   'TeX-PDF-mode)
    (define-key map "\C-c\C-t\C-i"   'TeX-interactive-mode)
    (define-key map "\C-c\C-t\C-s"   'TeX-source-correlate-mode)
    (define-key map "\C-c\C-t\C-r"   'TeX-pin-region)
    (define-key map "\C-c\C-w"       'TeX-toggle-debug-bad-boxes); to be removed
    (define-key map "\C-c\C-t\C-b"   'TeX-toggle-debug-bad-boxes)
    (define-key map "\C-c\C-t\C-w"   'TeX-toggle-debug-warnings)
    (define-key map "\C-c\C-v" 'TeX-view)
    ;; From tex-buf.el
    (define-key map "\C-c\C-d" 'TeX-save-document)
    (define-key map "\C-c\C-r" 'TeX-command-region)
    (define-key map "\C-c\C-b" 'TeX-command-buffer)
    (define-key map "\C-c\C-c" 'TeX-command-master)
    (define-key map "\C-c\C-k" 'TeX-kill-job)
    (define-key map "\C-c\C-l" 'TeX-recenter-output-buffer)
    (define-key map "\C-c^" 'TeX-home-buffer)
    (define-key map "\C-c`"    'TeX-next-error)
    ;; Remap bindings of `next-error'
    (if (featurep 'xemacs)
	(substitute-key-definition 'next-error 'TeX-next-error map global-map)
      (define-key map [remap next-error] 'TeX-next-error))
    ;; Remap bindings of `previous-error'
    (if (featurep 'xemacs)
	(substitute-key-definition 'previous-error 'TeX-previous-error
				   map global-map)
      (define-key map [remap previous-error] 'TeX-previous-error))
    ;; From tex-fold.el
    (define-key map "\C-c\C-o\C-f" 'TeX-fold-mode)

    ;; Multifile
    (define-key map "\C-c_" 'TeX-master-file-ask)  ;*** temporary
    map)
  "Keymap for common TeX and LaTeX commands.")

(defun TeX-mode-specific-command-menu (mode)
  "Return a Command menu specific to the major MODE."
  ;; COMPATIBILITY for Emacs < 21
  (if (and (not (featurep 'xemacs))
	   (= emacs-major-version 20))
      (cons TeX-command-menu-name
	    (TeX-mode-specific-command-menu-entries mode))
    (list TeX-command-menu-name
	  :filter `(lambda (&rest ignored)
		     (TeX-mode-specific-command-menu-entries ',mode))
	  "Bug.")))

(defun TeX-mode-specific-command-menu-entries (mode)
  "Return the entries for a Command menu specific to the major MODE."
  (append
   (TeX-menu-with-help
    `("Command on"
      [ "Master File" TeX-command-select-master
	:keys "C-c C-c" :style radio
	:selected (eq TeX-command-current 'TeX-command-master)
	:help "Commands in this menu work on the Master File"]
      [ "Buffer" TeX-command-select-buffer
	:keys "C-c C-b" :style radio
	:selected (eq TeX-command-current 'TeX-command-buffer)
	:help "Commands in this menu work on the current buffer"]
      [ "Region" TeX-command-select-region
	:keys "C-c C-r" :style radio
	:selected (eq TeX-command-current 'TeX-command-region)
	:help "Commands in this menu work on the region"]
      [ "Fix the Region" TeX-pin-region
	:active (or (if prefix-arg
			(<= (prefix-numeric-value prefix-arg) 0)
		      (and (boundp 'TeX-command-region-begin)
			   (markerp TeX-command-region-begin)))
		    (TeX-mark-active))
	;;:visible (eq TeX-command-current 'TeX-command-region)
	:style toggle
	:selected (and (boundp 'TeX-command-region-begin)
		       (markerp TeX-command-region-begin))
	:help "Fix the region for \"Command on Region\""]
      "-"
      ["Recenter Output Buffer" TeX-recenter-output-buffer
       :help "Show the output of current TeX process"]
      ["Kill Job" TeX-kill-job
       :help "Kill the current TeX process"]
      ["Next Error" TeX-next-error
       :help "Jump to the next error of the last TeX run"]
      ["Quick View" TeX-view
       :help "Start a viewer without prompting"]
      "-"
      ("TeXing Options"
       ,@(mapcar (lambda (x)
		   (let ((symbol (car x)) (name (nth 1 x)))
		     `[ ,(format "Use %s engine" name) (TeX-engine-set ',symbol)
			:style radio :selected (eq TeX-engine ',symbol)
			:help ,(format "Use %s engine for compiling" name) ]))
		 (TeX-engine-alist))
       "-"
       [ "Generate PDF" TeX-PDF-mode
	 :style toggle :selected TeX-PDF-mode
	 :active (not (eq TeX-engine 'omega))
	 :help "Use PDFTeX to generate PDF instead of DVI"]
       [ "Run Interactively" TeX-interactive-mode
	 :style toggle :selected TeX-interactive-mode :keys "C-c C-t C-i"
	 :help "Stop on errors in a TeX run"]
       [ "Correlate I/O" TeX-source-correlate-mode
	 :style toggle :selected TeX-source-correlate-mode
	 :help "Enable forward and inverse search in the previewer"]
       ["Debug Bad Boxes" TeX-toggle-debug-bad-boxes
	:style toggle :selected TeX-debug-bad-boxes :keys "C-c C-t C-b"
	:help "Make \"Next Error\" show overfull and underfull boxes"]
       ["Debug Warnings" TeX-toggle-debug-warnings
	:style toggle :selected TeX-debug-warnings
	:help "Make \"Next Error\" show warnings"])))
   (let ((file 'TeX-command-on-current));; is this actually needed?
     (TeX-maybe-remove-help
      (delq nil
	    (mapcar 'TeX-command-menu-entry
		    (TeX-mode-specific-command-list mode)))))))

(defun TeX-mode-specific-command-list (mode)
  "Return the list of commands available in the given MODE."
  (let ((full-list TeX-command-list)
	out-list
	entry)
    (while (setq entry (pop full-list))
      ;; `(nth 4 entry)' may be either an atom in case of which the
      ;; entry should be present in any mode or a list of major modes.
      (if (or (atom (nth 4 entry))
	      (memq mode (nth 4 entry)))
	  (push entry out-list)))
    (nreverse out-list)))

(defvar TeX-fold-menu
  (TeX-menu-with-help
   '("Show/Hide"
     ["Fold Mode" TeX-fold-mode
      :style toggle
      :selected (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Toggle folding mode"]
     "-"
     ["Hide All in Current Buffer" TeX-fold-buffer
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide all configured TeX constructs in the current buffer"]
     ["Hide All in Current Region" TeX-fold-region
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide all configured TeX constructs in the marked region"]
     ["Hide All in Current Paragraph" TeX-fold-paragraph
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide all configured TeX constructs in the paragraph containing point"]
     ["Hide Current Macro" TeX-fold-macro
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide the macro containing point"]
     ["Hide Current Environment" TeX-fold-env
      :visible (not (eq major-mode 'plain-tex-mode))
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide the environment containing point"]
     ["Hide Current Comment" TeX-fold-comment
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide the comment containing point"]
     "-"
     ["Show All in Current Buffer" TeX-fold-clearout-buffer
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Permanently show all folded content again"]
     ["Show All in Current Region" TeX-fold-clearout-region
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Permanently show all folded content in marked region"]
     ["Show All in Current Paragraph" TeX-fold-clearout-paragraph
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Permanently show all folded content in paragraph containing point"]
     ["Show Current Item" TeX-fold-clearout-item
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Permanently show the item containing point"]
     "-"
     ["Hide or Show Current Item" TeX-fold-dwim
      :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
      :help "Hide or show the item containing point"]))
   "Menu definition for commands from tex-fold.el.")

(defvar TeX-customization-menu nil)

(defvar TeX-common-menu-entries
  (TeX-menu-with-help
   `(("Multifile/Parsing"
      ["Switch to Master File" TeX-home-buffer
       :help "Switch to buffer of Master File, or buffer of last TeX command"]
      ["Save Document" TeX-save-document
       :help "Save all buffers associated with the current Master File"]
      ["Set Master File" TeX-master-file-ask
       :active (not (TeX-local-master-p))
       :help "Set the main file to run TeX commands on"]
      ["Reset Buffer" TeX-normal-mode
       :help "Save and reparse the current buffer for style information"]
      ["Reset AUCTeX" (TeX-normal-mode t) :keys "C-u C-c C-n"
       :help "Reset buffer and reload AUCTeX style files"])
     ["Find Documentation..." TeX-doc
      :help "Get help on commands, packages, or TeX-related topics in general"]
     ["Read the AUCTeX Manual" TeX-goto-info-page
      :help "Everything worth reading"]
     ("Customize AUCTeX"
      ["Browse Options"
       (customize-group 'AUCTeX)
       :help "Open the customization buffer for AUCTeX"]
      ["Extend this Menu"
       (progn
	 (easy-menu-add-item
	  nil
	  ;; Ugly hack because docTeX mode uses the LaTeX menu.
	  (list (if (eq major-mode 'doctex-mode) "LaTeX" TeX-base-mode-name))
	  (or TeX-customization-menu
	      (setq TeX-customization-menu
		    (customize-menu-create 'AUCTeX "Customize AUCTeX")))))
       :help "Make this menu a full-blown customization menu"])
     ["Report AUCTeX Bug" TeX-submit-bug-report
      :help ,(format "Problems with AUCTeX %s? Mail us!"
		     AUCTeX-version)])))


;;; Verbatim constructs

(defvar TeX-verbatim-p-function nil
  "Mode-specific function to be called by `TeX-verbatim-p'.")
(make-variable-buffer-local 'TeX-verbatim-p-function)

;; XXX: We only have an implementation for LaTeX mode at the moment (Oct 2009).
(defun TeX-verbatim-p (&optional pos)
  "Return non-nil if position POS is in a verbatim-like construct.
A mode-specific implementation is required.  If it is not
available, the function always returns nil."
  (when TeX-verbatim-p-function
    (funcall TeX-verbatim-p-function)))


;;; Comments

(defvar TeX-comment-start-regexp "%"
  "Regular expression matching a comment starter.
Unlike the variable `comment-start-skip' it should not match any
whitespace after the comment starter or any character before it.")
(make-variable-buffer-local 'TeX-comment-start-regexp)

(defun TeX-comment-region (beg end &optional arg)
  "Comment each line in the region from BEG to END.
Numeric prefix arg ARG means use ARG comment characters.
If ARG is negative, delete that many comment characters instead."
  (interactive "*r\nP")
  ;; `comment-padding' will not be recognized in XEmacs' (21.4)
  ;; `comment-region', so we temporarily modify `comment-start' to get
  ;; proper spacing.  Unfortunately we have to check for the XEmacs
  ;; version and cannot test if `comment-padding' is bound as this
  ;; gets initialized in `VirTeX-common-initialization'.
  (let ((comment-start (if (and (featurep 'xemacs)
				(= emacs-major-version 21)
				(<= emacs-minor-version 4))
			   (concat comment-start (TeX-comment-padding-string))
			 comment-start)))
    (comment-region beg end arg)))

(eval-and-compile
  ;; COMPATIBILITY for Emacs <= 21.3
  (if (fboundp 'comment-or-uncomment-region)
      (defalias 'TeX-comment-or-uncomment-region 'comment-or-uncomment-region)
    ;; The following function was copied from `newcomment.el' on
    ;; 2004-01-30 and adapted accordingly
    (defun TeX-comment-or-uncomment-region (beg end &optional arg)
      "Comment or uncomment a the region from BEG to END.
Call `TeX-comment-region', unless the region only consists of
comments, in which case call `TeX-uncomment-region'.  If a prefix
arg ARG is given, it is passed on to the respective function."
      (interactive "*r\nP")
      (funcall (if (save-excursion ;; check for already commented region
		     (goto-char beg)
		     (TeX-comment-forward (point-max))
		     (<= end (point)))
		   'TeX-uncomment-region 'TeX-comment-region)
	       beg end arg)))

  ;; COMPATIBILITY for Emacs <= 20.  (Introduced in 21.1?)
  (if (fboundp 'uncomment-region)
      (defalias 'TeX-uncomment-region 'uncomment-region)
    (defun TeX-uncomment-region (beg end &optional arg)
      "Remove comment characters from the beginning of each line
in the region from BEG to END.  Numeric prefix arg ARG means use
ARG comment characters.  If ARG is negative, delete that many
comment characters instead."
      (interactive "*r\nP")
      (or arg
	  ;; Determine the number of comment characters at the
	  ;; beginning of the first commented line.
	  (setq arg
		(save-excursion
		  (goto-char beg)
		  (re-search-forward
		   (concat "^" TeX-comment-start-regexp "+") end t)
		  (length (match-string 0)))))
      (comment-region beg end (- arg)))))

(defun TeX-uncomment ()
  "Delete comment characters from the beginning of each line in a comment."
  (interactive)
  (save-excursion
    ;; Find first comment line
    (beginning-of-line)
    (while (and (looking-at (concat "^[ \t]*" TeX-comment-start-regexp))
		(not (bobp)))
      (forward-line -1))
    (let ((beg (point)))
      (forward-line 1)
      ;; Find last comment line
      (while (and (looking-at (concat "^[ \t]*" TeX-comment-start-regexp))
		  (not (eobp)))
	(forward-line 1))
      ;; Uncomment region
      (TeX-uncomment-region beg (point)))))

(defun TeX-comment-or-uncomment-paragraph ()
  "Comment or uncomment current paragraph."
  (interactive)
  (if (TeX-in-commented-line)
      (TeX-uncomment)
    (save-excursion
      (beginning-of-line)
      ;; Don't do anything if we are in an empty line.  If this line
      ;; is followed by a lot of commented lines, this shall prevent
      ;; that mark-paragraph skips over these lines and marks a
      ;; paragraph outside the visible window which might get
      ;; commented without the user noticing.
      (unless (looking-at "^[ \t]*$")
	(mark-paragraph)
	(TeX-comment-region (point) (mark))))))

(defun TeX-in-comment ()
  "Return non-nil if point is in a comment."
  (if (or (bolp)
	  (null comment-start-skip)
	  (eq (preceding-char) ?\r))
      nil
    (save-excursion
      (save-match-data
	(let ((pos (point)))
	  (beginning-of-line)
	  (and (or (looking-at comment-start-skip)
		   (re-search-forward comment-start-skip pos t))
	       (not (TeX-verbatim-p))))))))

(defun TeX-in-commented-line ()
  "Return non-nil if point is in a line consisting only of a comment.
The comment can be preceded by whitespace.  This means that
`TeX-in-commented-line' is more general than `TeX-in-line-comment'
which will not match commented lines with leading whitespace.  But
`TeX-in-commented-line' will match commented lines without leading
whitespace as well."
  (save-excursion
    (forward-line 0)
    (skip-chars-forward " \t")
    (string= (buffer-substring-no-properties
	      (point) (min (point-max) (+ (point) (length comment-start))))
	     comment-start)))

(defun TeX-in-line-comment ()
  "Return non-nil if point is in a line comment.
A line comment is a comment starting in column one, i.e. there is
no whitespace before the comment sign."
  (save-excursion
    (forward-line 0)
    (string= (buffer-substring-no-properties
	      (point) (min (point-max) (+ (point) (length comment-start))))
	     comment-start)))

(defun TeX-comment-prefix ()
  "Return the comment prefix of the current line.
If there are no comment starters after potential whitespace at
the beginning of the line, return nil."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (when (looking-at (concat "\\([ \t]*" TeX-comment-start-regexp "+\\)+"))
	(match-string 0)))))

(defun TeX-forward-comment-skip (&optional count limit)
  "Move forward to the next comment skip.
This may be a switch between commented and not commented adjacent
lines or between lines with different comment prefixes.  With
argument COUNT do it COUNT times.  If argument LIMIT is given, do
not move point further than this value."
  (unless count (setq count 1))
  ;; A value of 0 is nonsense.
  (when (= count 0) (setq count 1))
  (unless limit (setq limit (point-max)))
  (dotimes (i (abs count))
    (if (< count 0)
	(forward-line -1)
      (beginning-of-line))
    (let ((prefix (when (looking-at (concat "\\([ \t]*"
					    TeX-comment-start-regexp "+\\)+"))
		    (buffer-substring (+ (line-beginning-position)
					 (current-indentation))
				      (match-end 0)))))
      (while (save-excursion
	       (and (if (> count 0)
			(<= (point) limit)
		      (>= (point) limit))
		    (zerop (if (> count 0)
			       (forward-line 1)
			     (forward-line -1)))
		    (if prefix
			(if (looking-at (concat "\\([ \t]*"
						TeX-comment-start-regexp
						"+\\)+"))
			    ;; If the preceding line is a commented line
			    ;; as well, check if the prefixes are
			    ;; identical.
			    (string= prefix
				     (buffer-substring
				      (+ (line-beginning-position)
					 (current-indentation))
				      (match-end 0)))
			  nil)
		      (not (looking-at (concat "[ \t]*"
					       TeX-comment-start-regexp))))))
	(if (> count 0)
	    (forward-line 1)
	  (forward-line -1)))
      (if (> count 0)
	  (forward-line 1)))))

(defun TeX-backward-comment-skip (&optional count limit)
  "Move backward to the next comment skip.
This may be a switch between commented and not commented adjacent
lines or between lines with different comment prefixes.  With
argument COUNT do it COUNT times.  If argument LIMIT is given, do
not move point to a position less than this value."
  (unless count (setq count 1))
  (when (= count 0) (setq count 1))
  (unless limit (setq limit (point-min)))
  (TeX-forward-comment-skip (- count) limit))

;; Taken from `comment-forward' in Emacs' CVS on 2006-12-26.  Used as
;; a compatibility function for XEmacs 21.4.
(defun TeX-comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment' but only for positive N
and can use regexps instead of syntax."
  (when (fboundp 'comment-normalize-vars)
    (comment-normalize-vars))
  (if (fboundp 'comment-forward)
      (comment-forward n)
    (setq n (or n 1))
    (if (< n 0) (error "No comment-backward")
      (if comment-use-syntax (forward-comment n)
	(while (> n 0)
	  (setq n
		(if (or (forward-comment 1)
			(and (looking-at comment-start-skip)
			     (goto-char (match-end 0))
			     (re-search-forward comment-end-skip nil 'move)))
		    (1- n) -1)))
	(= n 0)))))

(defun TeX-comment-padding-string ()
  "Return  comment padding as a string.
The variable `comment-padding' can hold an integer or a string.
This function will return the appropriate string representation
regardless of its data type."
  (if (integerp comment-padding)
      (make-string comment-padding ? )
    comment-padding))


;;; Indentation

(defgroup TeX-indentation nil
  "Indentation of TeX buffers in AUCTeX."
  :group 'AUCTeX)

(defcustom TeX-brace-indent-level 2
  "*The level of indentation produced by an open brace."
  :group 'TeX-indentation
  :type 'integer)

(defun TeX-comment-indent ()
  "Determine the indentation of a comment."
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

(defun TeX-brace-count-line ()
  "Count number of open/closed braces."
  (save-excursion
    (let ((count 0) (limit (line-end-position)) char)
      (while (progn
	       (skip-chars-forward "^{}\\\\" limit)
	       (when (and (< (point) limit) (not (TeX-in-comment)))
		 (setq char (char-after))
		 (forward-char)
		 (cond ((eq char ?\{)
			(setq count (+ count TeX-brace-indent-level)))
		       ((eq char ?\})
			(setq count (- count TeX-brace-indent-level)))
		       ((eq char ?\\)
			(when (< (point) limit)
			  (forward-char)
			  t))))))
      count)))

;;; Navigation

(defvar TeX-search-syntax-table
  (let ((table (make-syntax-table (make-char-table (if (featurep 'xemacs)
						       'syntax
						     'syntax-table)))))
    ;; Preset mode-independent syntax entries.  (Mode-dependent
    ;; entries are set in the function `TeX-search-syntax-table'.)
    ;; ?\", ?\( and ?\) explicitely get whitespace syntax because
    ;; Emacs 21.3 and XEmacs don't generate a completely empty syntax
    ;; table.
    (dolist (elt '((?\f . ">") (?\n . ">") (?\" . " ") (?\( . " ") (?\) . " ")))
      (modify-syntax-entry (car elt) (cdr elt) table))
    table)
  "Syntax table used for searching purposes.
It should be accessed through the function `TeX-search-syntax-table'.")

(defun TeX-search-syntax-table (&rest args)
  "Return a syntax table for searching purposes.
ARGS may be a list of characters.  For each of them the
respective predefined syntax is set.  Currently the parenthetical
characters ?{, ?}, ?[, ?], ?\(, ?\), ?<, and ?> are supported.
The syntax of each of these characters not specified will be
reset to \" \"."
  (let ((char-syntax-alist '((?\{ . "(}") (?\} . "){")
			     (?\[ . "(]") (?\] . ")[")
			     (?\( . "()") (?\) . ")(")
			     (?\< . "(>") (?\> . ")<"))))
    ;; Clean entries possibly set before.
    (modify-syntax-entry ?\\ " " TeX-search-syntax-table)
    (modify-syntax-entry ?@ " " TeX-search-syntax-table)
    (modify-syntax-entry ?\% " " TeX-search-syntax-table)
    ;; Preset mode-dependent syntax entries.  (Mode-independent entries
    ;; are set when the variable `TeX-search-syntax-table' is created.)
    (modify-syntax-entry (string-to-char TeX-esc) "\\" TeX-search-syntax-table)
    (unless (eq major-mode 'texinfo-mode)
      (modify-syntax-entry ?\% "<" TeX-search-syntax-table))
    ;; Clean up the entries which can be specified as arguments.
    (dolist (elt char-syntax-alist)
      (modify-syntax-entry (car elt) " " TeX-search-syntax-table))
    ;; Now set what we got.
    (dolist (elt args)
      (unless (assoc elt char-syntax-alist) (error "Char not supported"))
      (modify-syntax-entry elt (cdr (assoc elt char-syntax-alist))
			   TeX-search-syntax-table))
    ;; Return the syntax table.
    TeX-search-syntax-table))

(defun TeX-find-balanced-brace (&optional count depth limit)
  "Return the position of a balanced brace in a TeX group.
The function scans forward COUNT parenthetical groupings.
Default is 1.  If COUNT is negative, it searches backwards.  With
optional DEPTH>=1, find that outer level.  If LIMIT is non-nil,
do not search further than this position in the buffer."
  (let ((count (if count
		   (if (= count 0) (error "COUNT has to be <> 0") count)
		 1))
	(depth (if depth
		   (if (< depth 1) (error "DEPTH has to be > 0") depth)
		 1)))
    (save-restriction
      (when limit
	(if (> count 0)
	    (narrow-to-region (point-min) limit)
	  (narrow-to-region limit (point-max))))
      (with-syntax-table (TeX-search-syntax-table ?\{ ?\})
	(condition-case nil
	    (scan-lists (point) count depth)
	  (error nil))))))

(defun TeX-find-closing-brace (&optional depth limit)
  "Return the position of the closing brace in a TeX group.
The function assumes that point is inside the group, i.e. after
an opening brace.  With optional DEPTH>=1, find that outer level.
If LIMIT is non-nil, do not search further down than this
position in the buffer."
  (TeX-find-balanced-brace 1 depth limit))

(defun TeX-find-opening-brace (&optional depth limit)
  "Return the position of the opening brace in a TeX group.
The function assumes that point is inside the group, i.e. before
a closing brace.  With optional DEPTH>=1, find that outer level.
If LIMIT is non-nil, do not search further up than this position
in the buffer."
  (TeX-find-balanced-brace -1 depth limit))

(defun TeX-find-macro-boundaries (&optional lower-bound)
  "Return a list containing the start and end of a macro.
If LOWER-BOUND is given, do not search backward further than this
point in buffer.  Arguments enclosed in brackets or braces are
considered part of the macro."
  (save-restriction
    (when lower-bound
      (narrow-to-region lower-bound (point-max)))
    (let ((orig-point (point))
	  start-point)
      ;; Point is located directly at the start of a macro. (-!-\foo{bar})
      (when (and (eq (char-after) (aref TeX-esc 0))
		 (not (TeX-escaped-p)))
	(setq start-point (point)))
      ;; Point is located on a macro. (\fo-!-o{bar})
      (unless start-point
	(save-excursion
	  (skip-chars-backward "A-Za-z@*")
	  (when (and (eq (char-before) (aref TeX-esc 0))
		     (not (TeX-escaped-p (1- (point)))))
	    (setq start-point (1- (point))))))
      ;; Point is located in the argument of a macro. (\foo{ba-!-r})
      (unless start-point
	(save-excursion
	  (catch 'abort
	    (let ((parse-sexp-ignore-comments t))
	      (when (condition-case nil (progn (up-list) t) (error nil))
		(while (progn
			 (condition-case nil (backward-sexp)
			   (error (throw 'abort nil)))
			 (forward-comment -1)
			 (and (memq (char-before) '(?\] ?\}))
			      (not (TeX-escaped-p (1- (point)))))))
		(skip-chars-backward "A-Za-z@*")
		(when (and (eq (char-before) (aref TeX-esc 0))
			   (not (TeX-escaped-p (1- (point)))))
		  (setq start-point (1- (point)))))))))
      ;; Search forward for the end of the macro.
      (when start-point
	(save-excursion
	  (goto-char (TeX-find-macro-end-helper start-point))
	  (if (< orig-point (point))
	      (cons start-point (point))
	    nil))))))

(defun TeX-find-macro-end-helper (start)
  "Find the end of a macro given its START.
START is the position just before the starting token of the macro.
If the macro is followed by square brackets or curly braces,
those will be considered part of it."
  (save-excursion
    (save-match-data
      (catch 'found
	(goto-char (1+ start))
	(if (zerop (skip-chars-forward "A-Za-z@"))
	    (forward-char)
	  (skip-chars-forward "*"))
	(while (not (eobp))
	  (cond
	   ;; Skip over pairs of square brackets
	   ((or (looking-at "[ \t]*\n?\\(\\[\\)") ; Be conservative: Consider
					; only consecutive lines.
		(and (looking-at (concat "[ \t]*" TeX-comment-start-regexp))
		     (save-excursion
		       (forward-line 1)
		       (looking-at "[ \t]*\\(\\[\\)"))))
	    (goto-char (match-beginning 1))
	    (condition-case nil
		(forward-sexp)
	      (scan-error (throw 'found (point)))))
	   ;; Skip over pairs of curly braces
	   ((or (looking-at "[ \t]*\n?{") ; Be conservative: Consider
					; only consecutive lines.
		(and (looking-at (concat "[ \t]*" TeX-comment-start-regexp))
		     (save-excursion
		       (forward-line 1)
		       (looking-at "[ \t]*{"))))
	    (goto-char (match-end 0))
	    (goto-char (or (TeX-find-closing-brace)
			   ;; If we cannot find a regular end, use the
			   ;; next whitespace.
			   (save-excursion (skip-chars-forward "^ \t\n")
					   (point))))
	    (when (eobp) (throw 'found (point))))
	   (t
	    (throw 'found (point)))))))))

(defun TeX-find-macro-start (&optional limit)
  "Return the start of a macro.
If LIMIT is given, do not search backward further than this point
in buffer.  Arguments enclosed in brackets or braces are
considered part of the macro."
  (car (TeX-find-macro-boundaries limit)))

(defun TeX-find-macro-end ()
  "Return the end of a macro.
Arguments enclosed in brackets or braces are considered part of
the macro."
  (cdr (TeX-find-macro-boundaries)))

(defun TeX-search-forward-unescaped (string &optional bound noerror)
  "Search forward from point for unescaped STRING.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (TeX-search-unescaped string 'forward nil bound noerror))

(defun TeX-search-backward-unescaped (string &optional bound noerror)
  "Search backward from point for unescaped STRING.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (TeX-search-unescaped string 'backward nil bound noerror))

(defun TeX-re-search-forward-unescaped (regexp &optional bound noerror)
  "Search forward from point for unescaped regular expression REGEXP.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (TeX-search-unescaped regexp 'forward t bound noerror))

(defun TeX-search-unescaped (pattern
			     &optional direction regexp-flag bound noerror)
  "Search for unescaped PATTERN in a certain DIRECTION.
DIRECTION can be indicated by the symbols 'forward and 'backward.
If DIRECTION is omitted, a forward search is carried out.
If REGEXP-FLAG is non-nil, PATTERN may be a regular expression,
otherwise a string.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (let ((search-fun (if (eq direction 'backward)
			(if regexp-flag 're-search-backward 'search-backward)
		      (if regexp-flag 're-search-forward 'search-forward))))
    (catch 'found
      (while (funcall search-fun pattern bound noerror)
	(when (not (TeX-escaped-p (match-beginning 0)))
	  (throw 'found (point)))))))

(defun TeX-escaped-p (&optional pos)
  "Return t if the character at position POS is escaped.
If POS is omitted, examine the character at point.
A character is escaped if it is preceded by an odd number of
escape characters, such as \"\\\" in LaTeX."
  (save-excursion
    (when pos (goto-char pos))
    (not (zerop (mod (skip-chars-backward (regexp-quote TeX-esc)) 2)))))

(defun TeX-current-macro ()
  "Return the name of the macro containing point, nil if there is none."
  (let ((macro-start (TeX-find-macro-start)))
    (when macro-start
      (save-excursion
	(goto-char macro-start)
	(forward-char (length TeX-esc))
	(buffer-substring-no-properties
	 (point) (progn (skip-chars-forward "@A-Za-z") (point)))))))

(defvar TeX-search-forward-comment-start-function nil
  "Function to find the start of a comment.
The function should accept an optional argument for specifying
the limit of the search.  It should return the position just
before the comment if one is found and nil otherwise.  Point
should not be moved.")
(make-variable-buffer-local 'TeX-search-forward-comment-start-function)

(defun TeX-search-forward-comment-start (&optional limit)
  "Search forward for a comment start from current position till LIMIT.
If LIMIT is omitted, search till the end of the buffer.

The search relies on `TeX-comment-start-regexp' being set
correctly for the current mode.

Set `TeX-search-forward-comment-start-defun' in order to override
the default implementation."
  (if TeX-search-forward-comment-start-function
      (funcall TeX-search-forward-comment-start-function limit)
    (setq limit (or limit (point-max)))
    (when (TeX-re-search-forward-unescaped TeX-comment-start-regexp limit t)
      (match-beginning 0))))

;;; Fonts

(defcustom TeX-font-list '((?\C-b "{\\bf " "}")
			   (?\C-c "{\\sc " "}")
			   (?\C-e "{\\em " "\\/}")
			   (?\C-i "{\\it " "\\/}")
			   (?\C-r "{\\rm " "}")
			   (?\C-s "{\\sl " "\\/}")
			   (?\C-t "{\\tt " "}")
			   (?\C-d "" "" t))
  "List of fonts used by `TeX-font'.

Each entry is a list.
The first element is the key to activate the font.
The second element is the string to insert before point, and the third
element is the string to insert after point.
If the fourth and fifth element are strings, they specify the prefix and
suffix to be used in math mode.
An optional fourth (or sixth) element means always replace if t."
  :group 'TeX-macro
  :type '(repeat
	   (group
	    :value (?\C-a "" "")
	    (character :tag "Key")
	    (string :tag "Prefix")
	    (string :tag "Suffix")
	    (option (group
		     :inline t
		     (string :tag "Math Prefix")
		     (string :tag "Math Suffix")))
	    (option (sexp :format "Replace\n" :value t)))))

(defvar TeX-font-replace-function 'TeX-font-replace
  "Determines the function which is called when a font should be replaced.")

(defun TeX-describe-font-entry (entry)
  "A textual description of an ENTRY in `TeX-font-list'."
  (concat (format "%16s  " (key-description (char-to-string (nth 0 entry))))
	  (if (or (eq t (nth 3 entry)) (eq t (nth 5 entry)))
	      "-- delete font"
	    (format "%14s %-3s %14s %-3s"
		    (nth 1 entry) (nth 2 entry)
		    (if (stringp (nth 3 entry)) (nth 3 entry) "")
		    (if (stringp (nth 4 entry)) (nth 4 entry) "")))))

(defun TeX-font (replace what)
  "Insert template for font change command.
If REPLACE is not nil, replace current font.  WHAT determines the font
to use, as specified by `TeX-font-list'."
  (interactive "*P\nc")
  (TeX-update-style)
  (let* ((entry (assoc what TeX-font-list))
	 (in-math (texmathp))
	 (before (nth 1 entry))
	 (after (nth 2 entry)))
    (setq replace (or replace (eq t (nth 3 entry)) (eq t (nth 5 entry))))
    (if (and in-math (stringp (nth 3 entry)))
	(setq before (nth 3 entry)
	      after (nth 4 entry)))
    (cond ((null entry)
	   (let ((help (concat
			"Font list:   "
			"KEY        TEXTFONT           MATHFONT\n\n"
			(mapconcat 'TeX-describe-font-entry
				   TeX-font-list "\n"))))
	     (with-output-to-temp-buffer "*Help*"
	       (set-buffer "*Help*")
	       (insert help))))
	  (replace
	   (funcall TeX-font-replace-function before after))
	  ((TeX-active-mark)
	   (save-excursion
	     (cond ((> (mark) (point))
		    (insert before)
		    (goto-char (mark))
		    (insert after))
		   (t
		    (insert after)
		    (goto-char (mark))
		    (insert before)))))
	  (t
	   (insert before)
	   (save-excursion
	     (insert after))))))

(defun TeX-font-replace (start end)
  "Replace font specification around point with START and END.
For modes with font specifications like `{\\font text}'.
See also `TeX-font-replace-macro' and `TeX-font-replace-function'."
  (save-excursion
    (while (not (looking-at "{\\\\[a-zA-Z]+ "))
      (up-list -1))
    (forward-sexp)
    (save-excursion
      (replace-match start t t))
    (if (save-excursion
	  (backward-char 3)
	  (if (looking-at (regexp-quote "\\/}"))
	      (progn
		(delete-char 3)
		nil)
	    t))
	(delete-backward-char 1))
    (insert end)))

(defun TeX-font-replace-macro (start end)
  "Replace font specification around point with START and END.
For modes with font specifications like `\\font{text}'.
See also `TeX-font-replace' and `TeX-font-replace-function'."
  (let ((font-list TeX-font-list)
	cmds strings regexp)
    (while font-list
      (setq strings (cdr (car font-list))
	    font-list (cdr font-list))
      (and (stringp (car strings)) (null (string= (car strings) ""))
	   (setq cmds (cons (car strings) cmds)))
      (setq strings (cdr (cdr strings)))
      (and (stringp (car strings)) (null (string= (car strings) ""))
	   (setq cmds (cons (car strings) cmds))))
    (setq regexp (mapconcat 'regexp-quote cmds "\\|"))
    (save-excursion
      (catch 'done
	(while t
	  (if (/= ?\\ (following-char))
	      (skip-chars-backward "a-zA-Z "))
	  (skip-chars-backward (regexp-quote TeX-esc))
	  (if (looking-at regexp)
	      (throw 'done t)
	    (up-list -1))))
      ;; Use stripped syntax table in order to get stuff like "\emph{(}" right.
      (with-syntax-table (TeX-search-syntax-table ?\{ ?\})
	(forward-sexp 2))
      (save-excursion
	(replace-match start t t))
      (delete-backward-char 1)
      (insert end))))

;;; Dollars
;;
;; Rewritten from scratch with use of `texmathp' by
;; Carsten Dominik <dominik@strw.leidenuniv.nl>

(defvar TeX-symbol-marker nil)

(defvar TeX-symbol-marker-pos 0)

;; The following constants are no longer used, but kept in case some
;; foreign code uses any of them.
(defvar TeX-dollar-sign ?$
  "*Character used to enter and leave math mode in TeX.")
(defconst TeX-dollar-string (char-to-string TeX-dollar-sign))
(defconst TeX-dollar-regexp
  (concat "^" (regexp-quote TeX-dollar-string) "\\|[^" TeX-esc "]"
	  (regexp-quote TeX-dollar-string)))

(defcustom TeX-math-toggle-off-input-method t
  "*If non-nil, auto toggle off CJK input methods when entering math mode."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-math-close-double-dollar nil
  "If non-nil close double dollar math by typing a single `$'."
  :group 'TeX-macro
  :type 'boolean)

(defun TeX-insert-dollar (&optional arg)
  "Insert dollar sign.

If current math mode was not entered with a dollar, refuse to
insert one.  Show matching dollar sign if this dollar sign ends
the TeX math mode and `blink-matching-paren' is non-nil.  Ensure
double dollar signs match up correctly by inserting extra dollar
signs when needed.

With raw \\[universal-argument] prefix, insert exactly one dollar
sign.  With optional ARG, insert that many dollar signs."
  (interactive "P")
  (cond
   ((and arg (listp arg))
    ;; C-u always inserts one
    (insert "$"))
   (arg
    ;; Numerical arg inserts that many
    (insert (make-string (prefix-numeric-value arg) ?\$)))
   ((TeX-escaped-p)
    ;; This is escaped with `\', so just insert one.
    (insert "$"))
   ((texmathp)
    ;; We are inside math mode
    (if (and (stringp (car texmathp-why))
	     (string-equal (substring (car texmathp-why) 0 1) "\$"))
	;; Math mode was turned on with $ or $$ - so finish it accordingly.
	(progn
	  (if TeX-math-close-double-dollar
	      (insert (car texmathp-why))
	    (insert "$"))
	  (when (and blink-matching-paren
		     (or (string= (car texmathp-why) "$")
			 (zerop (mod (save-excursion
				       (skip-chars-backward "$")) 2))))
	    (save-excursion
	      (goto-char (cdr texmathp-why))
	      (if (pos-visible-in-window-p)
		  (sit-for 1)
		(message "Matches %s"
			 (buffer-substring
			  (point) (progn (end-of-line) (point))))))))
      ;; Math mode was not entered with dollar - we cannot finish it with one.
      (message "Math mode started with `%s' cannot be closed with dollar"
	       (car texmathp-why))
      (insert "$")))
   (t
    ;; Just somewhere in the text.
    (insert "$")))
  (TeX-math-input-method-off))

(defvar TeX-math-input-method-off-regexp
  "^\\(chinese\\|japanese\\|korean\\|bulgarian\\|russian\\)"
  "Regexp matching input methods to be deactivated when entering math mode.")

(defun TeX-math-input-method-off ()
  "Toggle off input method when entering math mode."
  (and TeX-math-toggle-off-input-method
       (texmathp)
       (boundp 'current-input-method) current-input-method
       (string-match TeX-math-input-method-off-regexp current-input-method)
       (inactivate-input-method)))

;;; Simple Commands

(defun TeX-normal-mode (arg)
  "Remove all information about this buffer, and apply the style hooks again.
Save buffer first including style information.
With optional argument ARG, also reload the style hooks."
  ;; FIXME: Shouldn't it be (&optional arg)?  -- rs
  (interactive "*P")
  (if arg
      (setq TeX-style-hook-list nil
	    BibTeX-global-style-files nil
	    BibTeX-global-files nil
	    TeX-Biber-global-files nil
	    TeX-global-input-files nil))
  (let ((TeX-auto-save t))
    (if (buffer-modified-p)
	(save-buffer)
      (TeX-auto-write)))
  (normal-mode)
  ;; See also addition to `find-file-hooks' in `VirTeX-common-initialization'.
  (when (eq TeX-master 'shared) (TeX-master-file nil nil t))
  (TeX-update-style t))

(defgroup TeX-quote nil
  "Quoting in AUCTeX."
  :group 'AUCTeX)

(defcustom TeX-open-quote "``"
  "String inserted by typing \\[TeX-insert-quote] to open a quotation."
  :group 'TeX-quote
  :type 'string)

(defcustom TeX-close-quote "''"
  "String inserted by typing \\[TeX-insert-quote] to close a quotation."
  :group 'TeX-quote
  :type 'string)

(defcustom TeX-quote-after-quote nil
  "Behaviour of \\[TeX-insert-quote].
Nil means standard behaviour; when non-nil, opening and closing
quotes are inserted only after \"."
  :group 'TeX-quote
  :type 'boolean)

(defcustom TeX-quote-language-alist nil
  "Alist for overriding the default language-specific quote insertion.
First element in each item is the name of the language as set by
the language style file as a string.  Second element is the
opening quotation mark.  Third elemxent is the closing quotation
mark.  Opening and closing quotation marks can be specified
directly as strings or as functions returning a string.  Fourth
element is a boolean specifying insertion behavior, overriding
`TeX-quote-after-quote'.  See Info node `(auctex)European' for
valid languages."
  :group 'TeX-quote
  :link '(custom-manual "(auctex)European")
  :type '(repeat (group (choice
			 (const "czech")
			 (const "danish")
			 (const "dutch")
			 (const "german")
			 (const "ngerman")
			 (const "french") ;; not frenchb or francais
			 (const "italian")
			 (const "polish")
			 (const "slovak")
			 (const "swedish")
			 (string :tag "Other Language"))
			(choice :tag "Opening quotation mark" string function)
			(choice :tag "Closing quotation mark" string function)
			(boolean :tag "Insert plain quote first" :value t))))

(defvar TeX-quote-language nil
  "If non-nil determines behavior of quote insertion.
It is usually set by language-related style files.  Its value has
the same structure as the elements of `TeX-quote-language-alist'.
The symbol 'override can be used as its car in order to override
the settings of style files.  Style files should therefore check
if this symbol is present and not alter `TeX-quote-language' if
it is.")
(make-variable-buffer-local 'TeX-quote-language)

(defun TeX-insert-quote (force)
  "Insert the appropriate quotation marks for TeX.
Inserts the value of `TeX-open-quote' (normally ``) or `TeX-close-quote'
\(normally '') depending on the context.  If `TeX-quote-after-quote'
is non-nil, this insertion works only after \".
With prefix argument FORCE, always inserts \" characters."
  (interactive "*P")
  (if (or force
	  ;; Do not insert TeX quotes in verbatim, math or comment constructs.
	  (and (fboundp 'font-latex-faces-present-p)
	       (font-latex-faces-present-p '(font-latex-verbatim-face
					     font-latex-math-face
					     font-lock-comment-face))
	       (font-latex-faces-present-p '(font-latex-verbatim-face
					     font-latex-math-face
					     font-lock-comment-face)
					   (1- (point))))
	  (texmathp)
	  (and (TeX-in-comment) (not (eq major-mode 'doctex-mode))))
      (self-insert-command (prefix-numeric-value force))
    (TeX-update-style)
    (let* ((lang-override (if (eq (car TeX-quote-language) 'override)
			      TeX-quote-language
			    (assoc (car TeX-quote-language)
				   TeX-quote-language-alist)))
	   (lang (or lang-override TeX-quote-language))
	   (open-quote (if lang (nth 1 lang) TeX-open-quote))
	   (close-quote (if lang (nth 2 lang) TeX-close-quote))
	   (q-after-q (if lang (nth 3 lang) TeX-quote-after-quote)))
      (when (functionp open-quote)
	(setq open-quote (funcall open-quote)))
      (when (functionp close-quote)
	(setq close-quote (funcall close-quote)))
      (if q-after-q
	  (insert (cond ((bobp)
			 ?\")
			((save-excursion
			   (TeX-looking-at-backward
			    (concat (regexp-quote open-quote) "\\|"
				    (regexp-quote close-quote))
			    (max (length open-quote) (length close-quote))))
			 (delete-backward-char (length (match-string 0)))
			 "\"\"")
			((< (save-excursion (skip-chars-backward "\"")) -1)
			 ?\")
			((not (= (preceding-char) ?\"))
			 ?\")
			((save-excursion
			   (forward-char -1)
			   (bobp))
			 (delete-backward-char 1)
			 open-quote)
			((save-excursion
			   (forward-char -2) ;;; at -1 there is double quote
			   (looking-at "[ \t\n]\\|\\s("))
			 (delete-backward-char 1)
			 open-quote)
			(t
			 (delete-backward-char 1)
			 close-quote)))
	(insert (cond ((bobp)
		       open-quote)
		      ((= (preceding-char) (string-to-char TeX-esc))
		       ?\")
		      ((= (preceding-char) ?\")
		       ?\")
		      ((save-excursion
			 (forward-char (- (length open-quote)))
			 (looking-at (regexp-quote open-quote)))
		       (delete-backward-char (length open-quote))
		       ?\")
		      ((save-excursion
			 (forward-char (- (length close-quote)))
			 (looking-at (regexp-quote close-quote)))
		       (delete-backward-char (length close-quote))
		       ?\")
		      ((save-excursion
			 (forward-char -1)
			 (looking-at "[ \t\n]\\|\\s("))
		       open-quote)
		      (t
		       close-quote)))))))

(defun TeX-insert-punctuation ()
  "Insert point or comma, cleaning up preceding space."
  (interactive)
  (expand-abbrev)
  (if (TeX-looking-at-backward "\\\\/\\(}+\\)" 50)
      (replace-match "\\1" t))
  (call-interactively 'self-insert-command))

(defun TeX-insert-braces (arg)
  "Make a pair of braces around next ARG sexps and leave point inside.
No argument is equivalent to zero: just insert braces and leave point
between.

If there is an active region, ARG will be ignored, braces will be
inserted around the region, and point will be left after the
closing brace."
  (interactive "P")
  (if (TeX-active-mark)
      (progn
	(if (< (point) (mark))
	    (exchange-point-and-mark))
	(insert TeX-grcl)
	(save-excursion
	  (goto-char (mark))
	  (insert TeX-grop)))
    (insert TeX-grop)
    (save-excursion
      (if arg (forward-sexp (prefix-numeric-value arg)))
      (insert TeX-grcl))))

;;;###autoload
(defun TeX-submit-bug-report ()
  "Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from (X)Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "bug-auctex@gnu.org"
     AUCTeX-version
     (list 'AUCTeX-date
	   'window-system
	   'LaTeX-version
	   'TeX-style-path
	   'TeX-auto-save
	   'TeX-parse-self
	   'TeX-master
	   'TeX-command-list)
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.

Be sure to consult the FAQ section in the manual before submitting
a bug report.  In addition check if the bug is reproducable with an
up-to-date version of AUCTeX.  So please upgrade to the version
available from http://www.gnu.org/software/auctex/ if your
installation is older than the one available from the web site.

If the bug is triggered by a specific \(La\)TeX file, you should try
to produce a minimal sample file showing the problem and include it
in your report.

Your bug report will be posted to the AUCTeX bug reporting list.
------------------------------------------------------------------------")))


;;; Documentation

(defun TeX-goto-info-page ()
  "Read documentation for AUCTeX in the info system."
  (interactive)
  (info "auctex"))

(autoload 'info-lookup->completions "info-look")

(defvar TeX-doc-backend-alist
  '((texdoc (plain-tex-mode latex-mode doctex-mode ams-tex-mode context-mode)
	    (lambda ()
	      (when (executable-find "texdoc")
		(TeX-search-files
		 ;; Explicitely supply doc directory for
		 ;; non-kpathsea-based TeX systems.
		 (unless (stringp TeX-kpathsea-path-delimiter)
		   (or (TeX-tree-expand
			'("$SYSTEXMF" "$TEXMFLOCAL" "$TEXMFMAIN" "$TEXMFDIST")
			"latex" '("/doc/"))
		       `(,@TeX-macro-global ,@TeX-macro-private)))
		 '("dvi" "pdf" "ps" "txt" "html") t t)))
	    (lambda (doc)
	      ;; texdoc in MiKTeX requires --view in order to start
	      ;; the viewer instead of an intermediate web page.
	      (call-process "texdoc" nil 0 nil "--view" doc)))
    (latex-info (latex-mode)
		(lambda ()
		  (when (condition-case nil
			    (save-window-excursion
			      (let ((buf (generate-new-buffer-name "*info*")))
				(info "latex" buf)
				(kill-buffer buf))
			      t)
			  (error nil))
		    (mapcar (lambda (x)
			      (let ((x (car x)))
				(if (string-match "\\`\\\\" x)
				    (substring x 1) x)))
			    (info-lookup->completions 'symbol 'latex-mode))))
		(lambda (doc)
		  (info-lookup-symbol (concat "\\" doc) 'latex-mode)))
    (texinfo-info (texinfo-mode)
		  (lambda ()
		    (when (condition-case nil
			      (save-window-excursion
				(let ((buf (generate-new-buffer-name "*info*")))
				  (info "texinfo" buf)
				  (kill-buffer buf))
				t)
			    (error nil))
		      (mapcar (lambda (x)
				(let ((x (car x)))
				  (if (string-match "\\`@" x)
				      (substring x 1) x)))
			      (info-lookup->completions 'symbol
							'texinfo-mode))))
		  (lambda (doc)
		    (info-lookup-symbol (concat "@" doc) 'texinfo-mode))))
  "Alist of backends used for looking up documentation.
Each item consists of four elements.

The first is a symbol describing the backend's name.

The second is a list of modes the backend should be activated in.

The third is a function returning a list of documents available
to the backend.  It should return nil if the backend is not
available, e.g. if a required executable is not present on the
system in question.

The fourth is a function for displaying the documentation.  The
function should accept a single argument, the chosen package,
command, or document name.")

(defun TeX-doc (&optional name)
  "Display documentation for string NAME.
NAME may be a package, a command, or a document."
  (interactive)
  (let (docs)
    ;; Build the lists of available documentation used for completion.
    (dolist (elt TeX-doc-backend-alist)
      (when (memq major-mode (nth 1 elt))
	(let ((completions (funcall (nth 2 elt))))
	  (unless (null completions)
	    (add-to-list 'docs (cons completions (nth 0 elt)))))))
    (if (null docs)
	(progn
	  (if (executable-find "texdoc")
	      ;; Fallback if we did not find anything via the backend list.
	      (let ((doc (read-from-minibuffer "Input for `texdoc': ")))
		(when doc (call-process "texdoc" nil 0 nil "--view" doc)))
	    ;; Give up.
	    (message "No documentation found")))
      ;; Ask the user about the package, command, or document.
      (when (and (interactive-p)
		 (or (not name) (string= name "")))
	(let ((symbol (thing-at-point 'symbol))
	      contained completions doc)
	  ;; Is the symbol at point contained in the lists of available
	  ;; documentation?
	  (setq contained (catch 'found
			    (dolist (elt docs)
			      (when (member symbol (car elt))
				(throw 'found t)))))
	  ;; Setup completion list in a format suitable for `completing-read'.
	  (dolist (elt docs)
	    (setq completions (nconc (mapcar 'list (car elt)) completions)))
	  ;; Query user.
	  (setq doc (completing-read
		     (if contained
			 (format "Package, command, or document (default %s): "
				 symbol)
		       "Package, command, or document: ")
		     completions))
	  (setq name (if (string= doc "") symbol doc))))
      (if (not name)
	  (message "No documentation specified")
	;; XXX: Provide way to choose in case a symbol can be found in
	;; more than one backend.
	(let* ((backend (catch 'found
			  (dolist (elt docs)
			    (when (member name (car elt))
			      (throw 'found (cdr elt)))))))
	  (if backend
	      (funcall (nth 3 (assoc backend TeX-doc-backend-alist)) name)
	    (message "Documentation not found")))))))


;;; Ispell Support

;; FIXME: Document those functions and variables.  -- rs

;; The FSF ispell.el use this.
(defun ispell-tex-buffer-p ()
  (and (boundp 'ispell-tex-p) ispell-tex-p))

;; The FSF ispell.el might one day use this.
(setq ispell-enable-tex-parser t)

(defun TeX-run-ispell (command string file)
  "Run ispell on current TeX buffer."
  (cond ((and (string-equal file (TeX-region-file))
	      (fboundp 'ispell-region))
	 (call-interactively 'ispell-region))
	((string-equal file (TeX-region-file))
	 (call-interactively 'spell-region))
	((fboundp 'ispell-buffer)
	 (ispell-buffer))
	((fboundp 'ispell)
	 (ispell))
	(t
	 (spell-buffer))))

(defun TeX-ispell-document (name)
  "Run ispell on all open files belonging to the current document."
  (interactive (list (TeX-master-file)))
  (if (string-equal name "")
      (setq name (TeX-master-file)))

  (let ((found nil)
	(regexp (concat "\\`\\("
			(mapconcat (lambda (dir)
				     (regexp-quote
				      (expand-file-name 
				       (file-name-as-directory dir))))
				   (append (when (file-name-directory name)
					     (list (file-name-directory name)))
					   TeX-check-path)
				   "\\|")
			"\\).*\\("
			(mapconcat 'regexp-quote
				   (cons (file-name-nondirectory name)
					 (TeX-style-list)) "\\|")
			"\\)\\.\\("
			(mapconcat 'regexp-quote TeX-file-extensions "\\|")
			"\\)\\'"))
	(buffers (buffer-list)))
    (while buffers
      (let* ((buffer (car buffers))
	     (name (buffer-file-name buffer)))
	(setq buffers (cdr buffers))
	(if (and name (string-match regexp name))
	    (progn
	      (save-excursion (switch-to-buffer buffer) (ispell-buffer))
	      (setq found t)))))))

;; Some versions of ispell 3 use this.
(defvar ispell-tex-major-modes nil)
(setq ispell-tex-major-modes
      (append '(plain-tex-mode ams-tex-mode latex-mode doctex-mode)
	      ispell-tex-major-modes))


;;; Abbrev mode

(defmacro TeX-abbrev-mode-setup (mode)
  "Set up the abbrev table and variable for MODE."
  (let ((symbol (intern (concat (symbol-name mode) "-abbrev-table")))
	(name (TeX-mode-prefix mode)))
    `(progn
       (defvar ,symbol nil
	 ,(format "Abbrev table for %s mode." name))
       (define-abbrev-table ',symbol nil)
       (when (fboundp 'abbrev-table-put)
	 (abbrev-table-put ,symbol :parents (list text-mode-abbrev-table))))))


;;; Special provisions for other modes and libraries

;; desktop-locals-to-save is broken by design.  Don't have
;; buffer-local values of it.
(eval-after-load "desktop"
  '(progn
     (dolist (elt '(TeX-master))
       (unless (member elt (default-value 'desktop-locals-to-save))
	 (setq-default desktop-locals-to-save
		       (cons elt (default-value 'desktop-locals-to-save)))))
     (add-hook 'desktop-after-read-hook '(lambda ()
					   (TeX-set-mode-name t)))))

;; delsel.el, `delete-selection-mode'
(put 'TeX-newline 'delete-selection t)
(put 'TeX-insert-dollar 'delete-selection t)
(put 'TeX-insert-quote 'delete-selection t)
(put 'TeX-insert-backslash 'delete-selection t)

(provide 'tex)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; tex.el ends here
