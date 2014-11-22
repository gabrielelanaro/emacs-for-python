;;; tex-fold.el --- Fold TeX macros.

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2011-2012
;;   Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-07-04
;; Keywords: tex, wp

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

;; This file provides support for hiding and unhiding TeX, LaTeX,
;; ContTeXt, Texinfo and similar macros and environments inside of
;; AUCTeX.
;;
;; Caveats:
;;
;; The display string of content which should display part of itself
;; is made by copying the text from the buffer together with its text
;; properties.  If fontification has not happened when this is done
;; (e.g. because of lazy or just-in-time font locking) the intended
;; fontification will not show up.  Maybe this could be improved by
;; using some sort of "lazy folding" or refreshing the window upon
;; scrolling.  As a workaround fontification of the whole buffer
;; currently is forced before folding it.

;;; Code:

(when (featurep 'xemacs)
  (require 'overlay))
(require 'tex)
(autoload 'LaTeX-forward-paragraph "latex")
(autoload 'LaTeX-backward-paragraph "latex")
(autoload 'LaTeX-find-matching-begin "latex")
(autoload 'LaTeX-find-matching-end "latex")
(autoload 'ConTeXt-find-matching-start "context")
(autoload 'ConTeXt-find-matching-stop "context")
(autoload 'Texinfo-find-env-start "tex-info")
(autoload 'Texinfo-find-env-end "tex-info")

(defgroup TeX-fold nil
  "Fold TeX macros."
  :group 'AUCTeX)

(defcustom TeX-fold-type-list '(env macro math)
  "List of item types to consider when folding.
Valid items are the symbols 'env for environments, 'macro for
macros, 'math for math macros and 'comment for comments."
  :type '(set (const :tag "Environments" env)
	      (const :tag "Macros" macro)
	      (const :tag "Math Macros" math)
	      (const :tag "Comments" comment))
  :group 'TeX-fold)

(defcustom TeX-fold-macro-spec-list
  `(("[f]" ("footnote" "marginpar"))
    ("[c]" ("cite"))
    ("[l]" ("label"))
    ("[r]" ("ref" "pageref" "eqref"))
    ("[i]" ("index" "glossary"))
    ("[1]:||*" ("item"))
    ("..." ("dots"))
    ("(C)" ("copyright"))
    ("(R)" ("textregistered"))
    ("TM"  ("texttrademark"))
    (1 ("part" "chapter" "section" "subsection" "subsubsection"
	"paragraph" "subparagraph"
	"part*" "chapter*" "section*" "subsection*" "subsubsection*"
	"paragraph*" "subparagraph*"
	"emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
	"textbf" "textsc" "textup")))
  "List of replacement specifiers and macros to fold.

The first element of each item can be a string, an integer or a
function symbol.  The second element is a list of macros two fold
without the leading backslash.

If the first element is a string, it will be used as a display
replacement for the whole macro.  Numbers in braces, brackets,
parens or angle brackets will be replaced by the respective macro
argument.  For example \"{1}\" will be replaced by the first
mandatory argument of the macro.  One can also define
alternatives within the specifier which are used if an argument
is not found.  Alternatives are separated by \"||\".  They are
most useful with optional arguments.  As an example, the default
specifier for \\item is \"[1]:||*\" which means that if there is
an optional argument, its value is shown followed by a colon.  If
there is no optional argument, only an asterisk is used as the
display string.

If the first element is an integer, the macro will be replaced by
the respective macro argument.

If the first element is a function symbol, the function will be
called with all mandatory arguments of the macro and the result
of the function call will be used as a replacement for the macro.

Setting this variable does not take effect immediately.  Use
Customize or reset the mode."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1)
				(function :tag "Function to execute"))
			(repeat :tag "Macros" (string))))
  :group 'TeX-fold)

(defvar TeX-fold-macro-spec-list-internal nil
  "Internal list of display strings and macros to fold.
Is updated when the TeX Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, i.e. contents of both `TeX-fold-macro-spec-list'
and <mode-prefix>-fold-macro-spec-list.")
(make-variable-buffer-local 'TeX-fold-macro-spec-list-internal)

(defcustom TeX-fold-env-spec-list
  '(("[comment]" ("comment")))
  "List of display strings and environments to fold."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Environments" (string))))
  :group 'TeX-fold)

(defvar TeX-fold-env-spec-list-internal nil
  "Internal list of display strings and environments to fold.
Is updated when the TeX Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, i.e. contents of both `TeX-fold-env-spec-list'
and <mode-prefix>-fold-env-spec-list.")
(make-variable-buffer-local 'TeX-fold-env-spec-list-internal)

(defcustom TeX-fold-math-spec-list nil
  "List of display strings and math macros to fold."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Math Macros" (string))))
  :group 'TeX-fold)

(defvar TeX-fold-math-spec-list-internal nil
  "Internal list of display strings and math macros to fold.
Is updated when the TeX Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, i.e. contents of both `TeX-fold-math-spec-list'
and <mode-prefix>-fold-math-spec-list.")
(make-variable-buffer-local 'TeX-fold-math-spec-list-internal)

(defcustom TeX-fold-unspec-macro-display-string "[m]"
  "Display string for unspecified macros.
This string will be displayed if a single macro is being hidden
which is not specified in `TeX-fold-macro-spec-list'."
  :type '(string)
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-env-display-string "[env]"
  "Display string for unspecified environments.
This string will be displayed if a single environment is being
hidden which is not specified in `TeX-fold-env-spec-list'."
  :type '(string)
  :group 'TeX-fold)

(defcustom TeX-fold-unspec-use-name t
  "If non-nil use the name of an unspecified item as display string.
Set it to nil if you want to use the values of the variables
`TeX-fold-unspec-macro-display-string' or
`TeX-fold-unspec-env-display-string' respectively as a display
string for any unspecified macro or environment."
  :type 'boolean
  :group 'TeX-fold)

(defcustom TeX-fold-preserve-comments nil
  "If non-nil do not fold in comments."
  :type 'boolean
  :group 'TeX-fold)

(defcustom TeX-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean
  :group 'TeX-fold)

(defcustom TeX-fold-help-echo-max-length 70
  "Maximum length of help echo message for folded overlays.
Set it to zero in order to disable help echos."
  :type 'integer
  :group 'TeX-fold)

(defcustom TeX-fold-force-fontify t
  "Force the buffer to be fully fontified by folding it."
  :group 'TeX-fold
  :type 'boolean)

(defcustom TeX-fold-auto nil
  "If non-nil, fold macros automatically after `TeX-insert-macro'."
  :group 'TeX-fold
  :type 'boolean)

(defface TeX-fold-folded-face
  '((((class color) (background light))
     (:foreground "SlateBlue"))
    (((class color) (background dark))
     (:foreground "SlateBlue1"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for the display string of folded content."
  :group 'TeX-fold)

(defvar TeX-fold-folded-face 'TeX-fold-folded-face
  "Face for the display string of folded content.")

(defface TeX-fold-unfolded-face
  '((((class color) (background light))
     (:background "#f2f0fd"))
    (((class color) (background dark))
     (:background "#38405d"))
    (((class grayscale) (background light))
     (:background "LightGray"))
    (((class grayscale) (background dark))
     (:background "DimGray"))
    (t (:inverse-video t)))
  "Face for folded content when it is temporarily opened."
  :group 'TeX-fold)

(defvar TeX-fold-unfolded-face 'TeX-fold-unfolded-face
  "Face for folded content when it is temporarily opened.")

(defvar TeX-fold-ellipsis "..."
  "String used as display string for overlays instead of a zero-length string.")

(defvar TeX-fold-open-spots nil)
(make-variable-buffer-local 'TeX-fold-open-spots)

(defcustom TeX-fold-command-prefix "\C-c\C-o"
  "Prefix key to use for commands in TeX Fold mode.
The value of this variable is checked as part of loading TeX Fold mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string
  :group 'TeX-fold)

(defvar TeX-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" 'TeX-fold-dwim)
    (define-key map "\C-b" 'TeX-fold-buffer)
    (define-key map "\C-r" 'TeX-fold-region)
    (define-key map "\C-p" 'TeX-fold-paragraph)
    (define-key map "\C-m" 'TeX-fold-macro)
    (define-key map "\C-e" 'TeX-fold-env)
    (define-key map "\C-c" 'TeX-fold-comment)
    (define-key map "b"    'TeX-fold-clearout-buffer)
    (define-key map "r"    'TeX-fold-clearout-region)
    (define-key map "p"    'TeX-fold-clearout-paragraph)
    (define-key map "i"    'TeX-fold-clearout-item)
    map))


;;; Folding

(defun TeX-fold-dwim ()
  "Hide or show items according to the current context.
If there is folded content, unfold it.  If there is a marked
region, fold all configured content in this region.  If there is
no folded content but a macro or environment, fold it."
  (interactive)
  (cond ((TeX-fold-clearout-item))
	((TeX-active-mark) (TeX-fold-region (mark) (point)))
	((TeX-fold-item 'macro))
	((TeX-fold-item 'math))
	((TeX-fold-item 'env))
	((TeX-fold-comment))))

(defun TeX-fold-buffer ()
  "Hide all configured macros and environments in the current buffer.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and `TeX-fold-math-spec-list', and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (TeX-fold-clearout-region (point-min) (point-max))
  (when (and TeX-fold-force-fontify
	     (boundp 'jit-lock-mode)
	     jit-lock-mode
	     (fboundp 'jit-lock-fontify-now))
    ;; We force fontification here only because it should rarely be
    ;; needed for the other folding commands.
    (jit-lock-fontify-now))
  (TeX-fold-region (point-min) (point-max)))

(defun TeX-fold-paragraph ()
  "Hide all configured macros and environments in the current paragraph.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and `TeX-fold-math-spec-list', and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (save-excursion
    (let ((end (progn (LaTeX-forward-paragraph) (point)))
	  (start (progn (LaTeX-backward-paragraph) (point))))
      (TeX-fold-clearout-region start end)
      (TeX-fold-region start end))))

(defun TeX-fold-region (start end)
  "Fold all items in region from START to END."
  (interactive "r")
  (when (and (memq 'env TeX-fold-type-list)
	     (not (eq major-mode 'plain-tex-mode)))
    (TeX-fold-region-macro-or-env start end 'env))
  (when (memq 'macro TeX-fold-type-list)
    (TeX-fold-region-macro-or-env start end 'macro))
  (when (memq 'math TeX-fold-type-list)
    (TeX-fold-region-macro-or-env start end 'math))
  (when (memq 'comment TeX-fold-type-list)
    (TeX-fold-region-comment start end)))

(defun TeX-fold-region-macro-or-env (start end type)
  "Fold all items of type TYPE in region from START to END.
TYPE can be one of the symbols 'env for environments, 'macro
for macros and 'math for math macros."
  (save-excursion
    (let (fold-list item-list regexp)
      (dolist (item (cond ((eq type 'env) TeX-fold-env-spec-list-internal)
			  ((eq type 'math) TeX-fold-math-spec-list-internal)
			  (t TeX-fold-macro-spec-list-internal)))
	(dolist (i (cadr item))
	  (add-to-list 'fold-list (list i (car item)))
	  (add-to-list 'item-list i)))
      (when item-list
	(setq regexp (cond ((and (eq type 'env)
				 (eq major-mode 'context-mode))
			    (concat (regexp-quote TeX-esc)
				    "start" (regexp-opt item-list t)))
			   ((and (eq type 'env)
				 (eq major-mode 'texinfo-mode))
			    (concat (regexp-quote TeX-esc)
				    (regexp-opt item-list t)))
			   ((eq type 'env)
			    (concat (regexp-quote TeX-esc)
				    "begin[ \t]*{"
				    (regexp-opt item-list t) "}"))
			   (t
			    (concat (regexp-quote TeX-esc)
				    (regexp-opt item-list t)))))
	(save-restriction
	  (narrow-to-region start end)
	  ;; Start from the bottom so that it is easier to prioritize
	  ;; nested macros.
	  (goto-char (point-max))
	  (let ((case-fold-search nil)
		item-name)
	    (while (re-search-backward regexp nil t)
	      (setq item-name (match-string 1))
	      (unless (or (and TeX-fold-preserve-comments
			       (TeX-in-commented-line))
			  ;; Make sure no partially matched macros are
			  ;; folded.  For macros consisting of letters
			  ;; this means there should be none of the
			  ;; characters [A-Za-z@*] after the matched
			  ;; string.  Single-char non-letter macros like
			  ;; \, don't have this requirement.
			  (and (memq type '(macro math))
			       (save-match-data
				 (string-match "[A-Za-z]" item-name))
			       (save-match-data
				 (string-match "[A-Za-z@*]"
					       (string (char-after
							(match-end 0)))))))
		(let* ((item-start (match-beginning 0))
		       (display-string-spec (cadr (assoc item-name
							 fold-list)))
		       (item-end (TeX-fold-item-end item-start type))
		       (ov (TeX-fold-make-overlay item-start item-end type
						  display-string-spec)))
		  (TeX-fold-hide-item ov))))))))))

(defun TeX-fold-region-comment (start end)
  "Fold all comments in region from START to END."
  (save-excursion
    (goto-char start)
    (let (beg)
      (while (setq beg (TeX-search-forward-comment-start end))
	(goto-char beg)
	;; Determine the start of the region to be folded just behind
	;; the comment starter.
	(looking-at TeX-comment-start-regexp)
	(setq beg (match-end 0))
	;; Search for the end of the comment.
	(while (TeX-comment-forward))
	(end-of-line 0)
	;; Hide the whole region.
	(TeX-fold-hide-item (TeX-fold-make-overlay beg (point) 'comment
						   TeX-fold-ellipsis))))))

(defun TeX-fold-macro ()
  "Hide the macro on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'macro)
    (message "No macro found")))

(defun TeX-fold-math ()
  "Hide the math macro on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'math)
    (message "No macro found")))

(defun TeX-fold-env ()
  "Hide the environment on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'env)
    (message "No environment found")))

(defun TeX-fold-comment ()
  "Hide the comment on which point currently is located."
  (interactive)
  (unless (TeX-fold-comment-do)
    (message "No comment found")))

(defun TeX-fold-item (type)
  "Hide the item on which point currently is located.
TYPE specifies the type of item and can be one of the symbols
'env for environments, 'macro for macros or 'math for math
macros.
Return non-nil if an item was found and folded, nil otherwise."
  (if (and (eq type 'env)
	   (eq major-mode 'plain-tex-mode))
      (message
       "Folding of environments is not supported in current mode")
    (let ((item-start (cond ((and (eq type 'env)
				  (eq major-mode 'context-mode))
			     (save-excursion
			       (ConTeXt-find-matching-start) (point)))
			    ((and (eq type 'env)
				  (eq major-mode 'texinfo-mode))
			     (save-excursion
			       (Texinfo-find-env-start) (point)))
			    ((eq type 'env)
			     (condition-case nil
				 (save-excursion
				   (LaTeX-find-matching-begin) (point))
			       (error nil)))
			    (t
			     (TeX-find-macro-start)))))
      (when item-start
	(let* ((item-name (save-excursion
			    (goto-char item-start)
			    (looking-at
			     (cond ((and (eq type 'env)
					 (eq major-mode 'context-mode))
				    (concat (regexp-quote TeX-esc)
					    "start\\([A-Za-z]+\\)"))
				   ((and (eq type 'env)
					 (eq major-mode 'texinfo-mode))
				    (concat (regexp-quote TeX-esc)
					    "\\([A-Za-z]+\\)"))
				   ((eq type 'env)
				    (concat (regexp-quote TeX-esc)
					    "begin[ \t]*{"
					    "\\([A-Za-z]+\\)}"))
				   (t
				    (concat (regexp-quote TeX-esc)
					    "\\([A-Za-z@*]+\\)"))))
			    (if (fboundp 'match-string-no-properties)
				(match-string-no-properties 1)
			      (match-string 1))))
	       (fold-list (cond ((eq type 'env) TeX-fold-env-spec-list-internal)
				((eq type 'math)
				 TeX-fold-math-spec-list-internal)
				(t TeX-fold-macro-spec-list-internal)))
	       fold-item
	       (display-string-spec
		(or (catch 'found
		      (while fold-list
			(setq fold-item (car fold-list))
			(setq fold-list (cdr fold-list))
			(when (member item-name (cadr fold-item))
			  (throw 'found (car fold-item)))))
		    ;; Item is not specified.
		    (if TeX-fold-unspec-use-name
			(concat "[" item-name "]")
		      (if (eq type 'env)
			  TeX-fold-unspec-env-display-string
			TeX-fold-unspec-macro-display-string))))
	       (item-end (TeX-fold-item-end item-start type))
	       (ov (TeX-fold-make-overlay item-start item-end type
					  display-string-spec)))
	  (TeX-fold-hide-item ov))))))

(defun TeX-fold-comment-do ()
  "Hide the comment on which point currently is located.
This is the function doing the work for `TeX-fold-comment'.  It
is an internal function communicating with return values rather
than with messages for the user.
Return non-nil if a comment was found and folded, nil otherwise."
  (if (and (not (TeX-in-comment)) (not (TeX-in-line-comment)))
      nil
    (let (beg)
      (save-excursion
	(while (progn
		 (beginning-of-line 0)
		 (and (TeX-in-line-comment)
		      (not (bobp)))))
	(goto-char (TeX-search-forward-comment-start (line-end-position 2)))
	(looking-at TeX-comment-start-regexp)
	(setq beg (match-end 0))
	(while (TeX-comment-forward))
	(end-of-line 0)
	(when (> (point) beg)
	  (TeX-fold-hide-item (TeX-fold-make-overlay beg (point) 'comment
						     TeX-fold-ellipsis)))))))


;;; Utilities

(defun TeX-fold-make-overlay (ov-start ov-end type display-string-spec)
  "Make a TeX-fold overlay extending from OV-START to OV-END.
TYPE is a symbol which is used to describe the content to hide
and may be 'macro for macros, 'math for math macro and 'env for
environments.
DISPLAY-STRING-SPEC is the original specification of the display
string in the variables `TeX-fold-macro-spec-list' or
`TeX-fold-env-spec-list' and may be a string or an integer."
  ;; Calculate priority before the overlay is instantiated.  We don't
  ;; want `TeX-overlay-prioritize' to pick up a non-prioritized one.
  (let ((priority (TeX-overlay-prioritize ov-start ov-end))
	(ov (make-overlay ov-start ov-end (current-buffer) t nil)))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'TeX-fold-type type)
    (overlay-put ov 'TeX-fold-display-string-spec display-string-spec)
    ov))

(defun TeX-fold-item-end (start type)
  "Return the end of an item of type TYPE starting at START.
TYPE can be either 'env for environments, 'macro for macros or
'math for math macros."
  (save-excursion
    (cond ((and (eq type 'env)
		(eq major-mode 'context-mode))
	   (goto-char start)
	   (ConTeXt-find-matching-stop)
	   (point))
	  ((and (eq type 'env)
		(eq major-mode 'texinfo-mode))
	   (goto-char (1+ start))
	   (Texinfo-find-env-end)
	   (point))
	  ((eq type 'env)
	   (goto-char (1+ start))
	   (LaTeX-find-matching-end)
	   (point))
	  (t
	   (goto-char start)
	   (TeX-find-macro-end)))))

(defun TeX-fold-overfull-p (ov-start ov-end display-string)
  "Return t if an overfull line will result after adding an overlay.
The overlay extends from OV-START to OV-END and will display the
string DISPLAY-STRING."
  (and (not (featurep 'xemacs)) ; Linebreaks in glyphs don't
				; work in XEmacs anyway.
       (save-excursion
	 (goto-char ov-end)
	 (search-backward "\n" ov-start t))
       (not (string-match "\n" display-string))
       (> (+ (- ov-start
		(save-excursion
		  (goto-char ov-start)
		  (line-beginning-position)))
	     (length display-string)
	     (- (save-excursion
		  (goto-char ov-end)
		  (line-end-position))
		ov-end))
	  (current-fill-column))))

(defun TeX-fold-macro-nth-arg (n macro-start &optional macro-end delims)
  "Return a property list of the argument number N of a macro.
The start of the macro to examine is given by MACRO-START, its
end optionally by MACRO-END.  With DELIMS the type of delimiters
can be specified as a cons cell containing the opening char as
the car and the closing char as the cdr.  The chars have to have
opening and closing syntax as defined in
`TeX-search-syntax-table'.

The first item in the returned list is the string specified in
the argument, the second item may be a face if the argument
string was fontified.  In Emacs the string holds text properties
as well, so the second item is always nil.  In XEmacs the string
does not enclose any faces, so these are given in the second item
of the resulting list."
  (save-excursion
    (let* ((macro-end (or macro-end
			  (save-excursion (goto-char macro-start)
					  (TeX-find-macro-end))))
	   (open-char (if delims (car delims) ?{))
	   (open-string (char-to-string open-char))
	   (close-char (if delims (cdr delims) ?}))
	   (close-string (char-to-string close-char))
	   content-start content-end)
      (goto-char macro-start)
      (if (condition-case nil
	      (progn
		(while (> n 0)
		  (skip-chars-forward (concat "^" open-string) macro-end)
		  (when (= (point) macro-end)
		    (error nil))
		  (setq content-start (progn
					(skip-chars-forward
					 (concat open-string " \t"))
					(point)))
		  (goto-char
		   (if delims
		       (with-syntax-table
			   (TeX-search-syntax-table open-char close-char)
			 (scan-lists (point) 1 1))
		     (TeX-find-closing-brace)))
		  (setq content-end (save-excursion
				      (backward-char)
				      (skip-chars-backward " \t")
				      (point)))
		  (setq n (1- n)))
		t)
	    (error nil))
	  (list (TeX-fold-buffer-substring content-start content-end)
		(when (and (featurep 'xemacs)
			   (extent-at content-start))
		  ;; A glyph in XEmacs does not seem to be able to hold more
		  ;; than one face, so we just use the first one we get.
		  (car (extent-property (extent-at content-start) 'face))))
	nil))))

(defun TeX-fold-buffer-substring (start end)
  "Return the contents of buffer from START to END as a string.
Like `buffer-substring' but copy overlay display strings as well."
  ;; Swap values of `start' and `end' if necessary.
  (when (> start end) (let ((tmp start)) (setq start end end tmp)))
  (let ((overlays (overlays-in start end))
	result)
    ;; Get rid of overlays not under our control or not completely
    ;; inside the specified region.
    (dolist (ov overlays)
      (when (or (not (eq (overlay-get ov 'category) 'TeX-fold))
		(< (overlay-start ov) start)
		(> (overlay-end ov) end))
	(setq overlays (remove ov overlays))))
    (if (null overlays)
	(buffer-substring start end)
      ;; Sort list according to ascending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (< (overlay-start a) (overlay-start b)))))
      ;; Get the string from the start of the region up to the first overlay.
      (setq result (buffer-substring start (overlay-start (car overlays))))
      (let (ov)
	(while overlays
	  (setq ov (car overlays)
		overlays (cdr overlays))
	  ;; Add the display string of the overlay.
	  (setq result (concat result (overlay-get ov 'display)))
	  ;; Remove overlays contained in the current one.
	  (dolist (elt overlays)
	    (when (< (overlay-start elt) (overlay-end ov))
	      (setq overlays (remove elt overlays))))
	  ;; Add the string from the end of the current overlay up to
	  ;; the next overlay or the end of the specified region.
	  (setq result (concat result (buffer-substring (overlay-end ov)
							(if overlays
							    (overlay-start
							     (car overlays))
							  end))))))
      result)))

(defun TeX-fold-make-help-echo (start end)
  "Return a string to be used as the help echo of folded overlays.
The text between START and END will be used for this but cropped
to the length defined by `TeX-fold-help-echo-max-length'.  Line
breaks will be replaced by spaces."
  (let* ((spill (+ start TeX-fold-help-echo-max-length))
	 (lines (split-string (buffer-substring start (min end spill)) "\n"))
	 (result (pop lines)))
    (dolist (line lines)
      ;; Strip leading whitespace
      (when (string-match "^[ \t]+" line)
	(setq line (replace-match "" nil nil line)))
      ;; Strip trailing whitespace
      (when (string-match "[ \t]+$" line)
	(setq line (replace-match "" nil nil line)))
      (setq result (concat result " " line)))
    (when (> end spill) (setq result (concat result "...")))
    result))

(defun TeX-fold-update-at-point ()
  "Update all TeX-fold overlays at point displaying computed content."
  (let (overlays)
    ;; Get all overlays at point under our control.
    (dolist (ov (overlays-at (point)))
      (when (and (eq (overlay-get ov 'category) 'TeX-fold)
		 (numberp (overlay-get ov 'TeX-fold-display-string-spec)))
	(add-to-list 'overlays ov)))
    (when overlays
      ;; Sort list according to descending starts.
      (setq overlays (sort (copy-sequence overlays)
			   (lambda (a b)
			     (> (overlay-start a) (overlay-start b)))))
      (dolist (ov overlays)
	(TeX-fold-hide-item ov)))))


;;; Removal

(defun TeX-fold-clearout-buffer ()
  "Permanently show all macros in the buffer."
  (interactive)
  (TeX-fold-clearout-region (point-min) (point-max)))

(defun TeX-fold-clearout-paragraph ()
  "Permanently show all macros in the paragraph point is located in."
  (interactive)
  (save-excursion
    (let ((end (progn (LaTeX-forward-paragraph) (point)))
	  (start (progn (LaTeX-backward-paragraph) (point))))
      (TeX-fold-clearout-region start end))))

(defun TeX-fold-clearout-region (start end)
  "Permanently show all macros in region starting at START and ending at END."
  (interactive "r")
  (let ((overlays (overlays-in start end)))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-clearout-item ()
  "Permanently show the macro on which point currently is located."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-remove-overlays (overlays)
  "Remove all overlays set by TeX-fold in OVERLAYS.
Return non-nil if a removal happened, nil otherwise."
  (let (found)
    (while overlays
      (when (eq (overlay-get (car overlays) 'category) 'TeX-fold)
	(delete-overlay (car overlays))
	(setq found t))
      (setq overlays (cdr overlays)))
    found))


;;; Toggling

(defun TeX-fold-expand-spec (spec ov-start ov-end)
  "Expand instances of {<num>}, [<num>], <<num>>, and (<num>).
Replace them with the respective macro argument."
  (let ((spec-list (split-string spec "||"))
	(delims '((?{ . ?}) (?[ . ?]) (?< . ?>) (?\( . ?\))))
	index success)
    (catch 'success
      ;; Iterate over alternatives.
      (dolist (elt spec-list)
	(setq spec elt
	      index nil)
	;; Find and expand every placeholder.
	(while (and (string-match "\\([[{<]\\)\\([1-9]\\)\\([]}>]\\)" elt index)
		    ;; Does the closing delim match the opening one?
		    (string-equal
		     (match-string 3 elt)
		     (char-to-string
		      (cdr (assq (string-to-char (match-string 1 elt))
				 delims)))))
	  (setq index (match-end 0))
	  (let ((arg (car (save-match-data
			    ;; Get the argument.
			    (TeX-fold-macro-nth-arg
			     (string-to-number (match-string 2 elt))
			     ov-start ov-end
			     (assoc (string-to-char (match-string 1 elt))
				    delims))))))
	    (when arg (setq success t))
	    ;; Replace the placeholder in the string.
	    (setq elt (replace-match (or arg TeX-fold-ellipsis) nil t elt)
		  index (+ index (- (length elt) (length spec)))
		  spec elt)))
	(when success (throw 'success nil))))
    spec))

(defun TeX-fold-hide-item (ov)
  "Hide a single macro or environment.
That means, put respective properties onto overlay OV."
  (let* ((ov-start (overlay-start ov))
	 (ov-end (overlay-end ov))
	 (spec (overlay-get ov 'TeX-fold-display-string-spec))
	 (computed (cond
		    ((stringp spec)
		     (TeX-fold-expand-spec spec ov-start ov-end))
		    ((functionp spec)
		     (let (arg arg-list
			   (n 1))
		       (while (setq arg (TeX-fold-macro-nth-arg
					 n ov-start ov-end))
			 (add-to-list 'arg-list (car arg) t)
			 (setq n (1+ n)))
		       (or (condition-case nil
			       (apply spec arg-list)
			     (error nil))
			   "[Error: No content or function found]")))
		    (t (or (TeX-fold-macro-nth-arg spec ov-start ov-end)
			   "[Error: No content found]"))))
	 (display-string (if (listp computed) (car computed) computed))
	 (face (when (listp computed) (cadr computed))))
    ;; Cater for zero-length display strings.
    (when (string= display-string "") (setq display-string TeX-fold-ellipsis))
    ;; Add a linebreak to the display string and adjust the overlay end
    ;; in case of an overfull line.
    (when (TeX-fold-overfull-p ov-start ov-end display-string)
      (setq display-string (concat display-string "\n"))
      (move-overlay ov ov-start (save-excursion
				  (goto-char ov-end)
				  (skip-chars-forward " \t")
				  (point))))
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'display display-string)
    (if (featurep 'xemacs)
	(let ((glyph (make-glyph (if (listp display-string)
				     (car display-string)
				   display-string))))
	  (overlay-put ov 'invisible t)
	  (when font-lock-mode
	    (if face
		(set-glyph-property glyph 'face face)
	      (set-glyph-property glyph 'face TeX-fold-folded-face)))
	  (set-extent-property ov 'end-glyph glyph))
      (when font-lock-mode
	(overlay-put ov 'face TeX-fold-folded-face))
      (unless (zerop TeX-fold-help-echo-max-length)
	(overlay-put ov 'help-echo (TeX-fold-make-help-echo
				    (overlay-start ov) (overlay-end ov)))))))

(defun TeX-fold-show-item (ov)
  "Show a single LaTeX macro or environment.
Remove the respective properties from the overlay OV."
  (overlay-put ov 'mouse-face nil)
  (if (featurep 'xemacs)
      (progn
	(set-extent-property ov 'end-glyph nil)
	(overlay-put ov 'invisible nil))
    (overlay-put ov 'display nil)
    (overlay-put ov 'help-echo nil)
    (when font-lock-mode
      (overlay-put ov 'face TeX-fold-unfolded-face))))

;; Copy and adaption of `reveal-post-command' from reveal.el in GNU
;; Emacs on 2004-07-04.
(defun TeX-fold-post-command ()
  ;; `with-local-quit' is not supported in XEmacs.
  (condition-case nil
      (let ((inhibit-quit nil))
	(condition-case err
	    (let* ((spots (TeX-fold-partition-list
			   (lambda (x)
			     ;; We refresh any spot in the current
			     ;; window as well as any spots associated
			     ;; with a dead window or a window which
			     ;; does not show this buffer any more.
			     (or (eq (car x) (selected-window))
				 (not (window-live-p (car x)))
				 (not (eq (window-buffer (car x))
					  (current-buffer)))))
			   TeX-fold-open-spots))
		   (old-ols (mapcar 'cdr (car spots))))
	      (setq TeX-fold-open-spots (cdr spots))
	      (when (or (and (boundp 'disable-point-adjustment)
			     disable-point-adjustment)
			(and (boundp 'global-disable-point-adjustment)
			     global-disable-point-adjustment)
			;; See preview.el on how to make this configurable.
			(memq this-command
			      (list (key-binding [left]) (key-binding [right])
				    'backward-char 'forward-char
				    'mouse-set-point)))
		;; Open new overlays.
		(dolist (ol (nconc (when (and TeX-fold-unfold-around-mark
					      (boundp 'mark-active)
					      mark-active)
				     (overlays-at (mark)))
				   (overlays-at (point))))
		  (when (eq (overlay-get ol 'category) 'TeX-fold)
		    (push (cons (selected-window) ol) TeX-fold-open-spots)
		    (setq old-ols (delq ol old-ols))
		    (TeX-fold-show-item ol))))
	      ;; Close old overlays.
	      (dolist (ol old-ols)
		(when (and (eq (current-buffer) (overlay-buffer ol))
			   (not (rassq ol TeX-fold-open-spots))
			   (or (not (featurep 'xemacs))
			       (and (featurep 'xemacs)
				    (not (extent-detached-p ol)))))
		  (if (and (>= (point) (overlay-start ol))
			   (<= (point) (overlay-end ol)))
		      ;; Still near the overlay: keep it open.
		      (push (cons (selected-window) ol) TeX-fold-open-spots)
		    ;; Really close it.
		    (TeX-fold-hide-item ol)))))
	  (error (message "TeX-fold: %s" err))))
    (quit (setq quit-flag t))))


;;; Misc

;; Copy and adaption of `cvs-partition' from pcvs-util.el in GNU Emacs
;; on 2004-07-05 to make tex-fold.el mainly self-contained.
(defun TeX-fold-partition-list (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))


;;; The mode

;; This autoload cookie had to be changed because of XEmacs.  This is
;; very dissatisfactory, because we now don't have the full doc string
;; available to tell people what to expect when using this mode before
;; loading it.

;;;###autoload (autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)
(define-minor-mode TeX-fold-mode
  "Minor mode for hiding and revealing macros and environments.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  nil nil (list (cons TeX-fold-command-prefix TeX-fold-keymap))
  (if TeX-fold-mode
      (progn
	(set (make-local-variable 'search-invisible) t)
	(add-hook 'post-command-hook 'TeX-fold-post-command nil t)
	(add-hook 'LaTeX-fill-newline-hook 'TeX-fold-update-at-point nil t)
	(add-hook 'TeX-after-insert-macro-hook
		  (lambda ()
		    (when (and TeX-fold-mode TeX-fold-auto)
		      (save-excursion
			(backward-char)
			(or (TeX-fold-item 'macro)
			    (TeX-fold-item 'math)
			    (TeX-fold-item 'env))))))
	;; Update the `TeX-fold-*-spec-list-internal' variables.
	(dolist (elt '("macro" "env" "math"))
	  (set (intern (format "TeX-fold-%s-spec-list-internal" elt))
	       ;; Append the value of `TeX-fold-*-spec-list' to the
	       ;; mode-specific `<mode-prefix>-fold-*-spec-list' variable.
	       (append (symbol-value (intern (format "TeX-fold-%s-spec-list"
						     elt)))
		       (let ((symbol (intern (format "%s-fold-%s-spec-list"
						     (TeX-mode-prefix) elt))))
			 (when (boundp symbol)
			   (symbol-value symbol)))))))
    (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook 'TeX-fold-post-command t)
    (remove-hook 'LaTeX-fill-newline-hook 'TeX-fold-update-at-point t)
    (TeX-fold-clearout-buffer))
  (TeX-set-mode-name))

;;;###autoload
(defalias 'tex-fold-mode 'TeX-fold-mode)

(provide 'tex-fold)

;;; tex-fold.el ends here
