;;; font-latex.el --- LaTeX fontification for Font Lock mode.

;; Copyright (C) 1996-2014  Free Software Foundation, Inc.

;; Authors:    Peter S. Galbraith <psg@debian.org>
;;             Simon Marshall <Simon.Marshall@esrin.esa.it>
;; Maintainer: auctex-devel@gnu.org
;; Created:    06 July 1996
;; Keywords:   tex, wp, faces

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package enhances font-lock fontification patterns for LaTeX.
;; font-lock mode is a minor mode that causes your comments to be
;; displayed in one face, strings in another, reserved words in
;; another, and so on.
;;
;; ** Infinite loops !? **
;; If you get an infinite loop, send a bug report!
;; Then set the following in your ~/.emacs file to keep on working:
;;   (setq font-latex-do-multi-line nil)

;;; Code:

(require 'font-lock)
(require 'tex)

(eval-when-compile
  (require 'cl))

(defgroup font-latex nil
  "Font-latex text highlighting package."
  :prefix "font-latex-"
  :group 'faces
  :group 'tex
  :group 'AUCTeX)

(defgroup font-latex-keywords nil
  "Keywords for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defgroup font-latex-highlighting-faces nil
  "Faces for highlighting text in font-latex."
  :prefix "font-latex-"
  :group 'font-latex)

(defvar font-latex-multiline-boundary 5000
  "Size of region to search for the start or end of a multiline construct.")

(defvar font-latex-quote-regexp-beg nil
  "Regexp used to find quotes.")
(make-variable-buffer-local 'font-latex-quote-regexp-beg)

(defvar font-latex-quote-list '(("``" "''") ("<<" ">>" french) ("«" "»" french))
  "List of quote specifiers for quotation fontification.

Each element of the list is either a list consisting of two
strings to be used as opening and closing quotation marks
independently of the value of `font-latex-quotes' or a list with
three elements where the first and second element are strings for
opening and closing quotation marks and the third element being
either the symbol 'german or 'french describing the order of
quotes.

If `font-latex-quotes' specifies a different state, order of the
added quotes will be reversed for fontification.  For example if
'(\"\\\"<\" \"\\\">\" french) is given but `font-latex-quotes'
specifies 'german, quotes will be used like \">foo\"< for
fontification.")

(defvar font-latex-quotes-control nil
  "Internal variable for keeping track if `font-latex-quotes' changed.")
(make-variable-buffer-local 'font-latex-quotes-control)

(defvar font-latex-quotes-internal nil
  "Internal variable for tracking outcome of automatic detection.
If automatic detection is not enabled, it is assigned the value
of `font-latex-quotes'.")
(make-variable-buffer-local 'font-latex-quotes-internal)

(defvar font-latex-quotes-fallback 'french
  "Fallback value for `font-latex-quotes' if automatic detection fails.")

(defvar font-latex-quote-style-list-french
  '("french" "frenchb" "frenchle" "frenchpro" "francais" "canadien"
    "acadian" "italian")
  "List of styles for which French-style quote matching should be activated.")

(defvar font-latex-quote-style-list-german
  '("austrian" "german" "germanb" "naustrian" "ngerman")
  "List of styles for which German-style quote matching should be activated.")

(defcustom font-latex-quotes 'auto
  "Whether to fontify << French quotes >> or >>German quotes<<.
Also selects \"<quote\"> versus \">quote\"<.

If value `auto' is chosen, an attempt is being made in deriving
the type of quotation mark matching from document settings like
the language option supplied to the babel package.

If nil, quoted content will not be fontified."
  :type '(choice (const auto) (const french) (const german) (const nil))
  :group 'font-latex)
(put 'font-latex-quotes 'safe-local-variable
     '(lambda (x) (memq x '(auto french german nil))))

(defun font-latex-add-quotes (quotes)
  "Add QUOTES to `font-latex-quote-list'.
QUOTES has to be a list adhering to the format of an element of
`font-latex-quote-list'."
  (setq font-latex-quotes-control nil)
  (add-to-list (make-local-variable 'font-latex-quote-list) quotes))

(defun font-latex-quotes-set-internal ()
  "Set `font-latex-quotes-internal' according to `font-latex-quotes'.
If `font-latex-quotes' is set to `auto', try to derive the
correct value from document properties."
  (setq font-latex-quotes-internal
	(if (eq font-latex-quotes 'auto)
	    (or (when (TeX-elt-of-list-member
		       font-latex-quote-style-list-french TeX-active-styles)
		  'french)
		(when (TeX-elt-of-list-member
		       font-latex-quote-style-list-german TeX-active-styles)
		  'german)
		font-latex-quotes-fallback)
	  font-latex-quotes)))
;; Update the value of `font-latex-quotes-internal' when the list of
;; styles changes.
(add-hook 'TeX-update-style-hook 'font-latex-quotes-set-internal)

;; The definitions of the title faces were originally taken from
;; info.el (Copyright (C) 1985, 86, 92, 93, 94, 95, 96, 97, 98, 99,
;; 2000, 2001 Free Software Foundation, Inc.) and adapted to the needs
;; of font-latex.el.

(defconst font-latex-sectioning-max 5
  "Highest number for font-latex-sectioning-N-face")
(defface font-latex-sectioning-5-face
  (if (featurep 'xemacs)
      '((((type tty pc) (class color) (background light))
	 (:foreground "blue4" :bold t))
	(((type tty pc) (class color) (background dark))
	 (:foreground "yellow" :bold t))
	(((class color) (background light))
	 (:bold t :foreground "blue4" :family "helvetica"))
	(((class color) (background dark))
	 (:bold t :foreground "yellow" :family "helvetica"))
	(t (:bold t :family "helvetica")))
    '((((type tty pc) (class color) (background light))
       (:foreground "blue4" :weight bold))
      (((type tty pc) (class color) (background dark))
       (:foreground "yellow" :weight bold))
      (((class color) (background light))
       (:weight bold :inherit variable-pitch :foreground "blue4"))
      (((class color) (background dark))
       (:weight bold :inherit variable-pitch :foreground "yellow"))
      (t (:weight bold :inherit variable-pitch))))
  "Face for sectioning commands at level 5."
  :group 'font-latex-highlighting-faces)

(defun font-latex-update-sectioning-faces (&optional max height-scale)
  "Update sectioning commands faces."
  (unless height-scale
    (setq height-scale (if (numberp font-latex-fontify-sectioning)
			   ;; Make sure `height-scale' is a floating point
			   ;; number because `set-face-attribute' treats
			   ;; integers differently from floating points.
			   (float font-latex-fontify-sectioning)
			 1.1)))
  (unless max
    (setq max font-latex-sectioning-max))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
	   (num (- max (1+ num)))
	   (face-name (intern (format "font-latex-sectioning-%s-face" num))))
      (unless (get face-name 'saved-face) ; Do not touch customized faces.
	(if (featurep 'xemacs)
	    (let ((size
		   ;; Multiply with .9 because `face-height' returns a value
		   ;; slightly larger than the actual font size.
		   ;; `make-face-size' takes numeric points according to Aidan
		   ;; Kehoe in <16989.15536.613916.678965@parhasard.net> (not
		   ;; documented).
		   (round (* 0.9
			     (face-height 'default)
			     (expt height-scale (- max 1 num))))))
	      ;; (message "%s - %s" face-name size)
	      (make-face-size face-name size))
	  (set-face-attribute face-name nil :height  height-scale))))))

(defcustom font-latex-fontify-sectioning 1.1
  "Whether to fontify sectioning macros with varying height or a color face.

If it is a number, use varying height faces.  The number is used
for scaling starting from `font-latex-sectioning-5-face'.  Typically
values from 1.05 to 1.3 give best results, depending on your font
setup.  If it is the symbol `color', use `font-lock-type-face'.

Caveats: Customizing the scaling factor applies to all sectioning
faces unless those face have been saved by customize.  Setting
this variable directly does not take effect unless you call
`font-latex-update-sectioning-faces' or restart Emacs.

Switching from `color' to a number or vice versa does not take
effect unless you call \\[font-lock-fontify-buffer] or restart
Emacs."
  ;; Possibly add some words about XEmacs here. :-(
  :type '(choice (number :tag "Scale factor")
                 (const color))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (unless (eq value 'color)
	   (font-latex-update-sectioning-faces font-latex-sectioning-max value)))
  :group 'font-latex)

(defun font-latex-make-sectioning-faces (max &optional height-scale)
  "Build the faces used to fontify sectioning commands."
  (unless max (setq max font-latex-sectioning-max))
  (unless height-scale
    (setq height-scale (if (numberp font-latex-fontify-sectioning)
			   ;; Make sure `height-scale' is a floating point
			   ;; number because the integer type is treated
			   ;; differently.
			   (float font-latex-fontify-sectioning)
			 1.1)))
  (dotimes (num max)
    (let* (;; reverse for XEmacs:
	   (num (- max (1+ num)))
	   (face-name (intern (format "font-latex-sectioning-%s-face" num)))
	   (f-inherit (intern (format "font-latex-sectioning-%s-face" (1+ num))))
	   (size (when (featurep 'xemacs)
		   (round (* 0.9 (face-height 'default)
			     (expt height-scale (- max 1 num)))))))
      (eval
       `(defface ,face-name
	  (if (featurep 'xemacs)
	      '((t (:size ,(format "%spt" size))))
	    '((t (:height ,height-scale :inherit ,f-inherit))))
	  (format "Face for sectioning commands at level %s.

Probably you don't want to customize this face directly.  Better
change the base face `font-latex-sectioning-5-face' or customize the
variable `font-latex-fontify-sectioning'." ',num)
	  :group 'font-latex-highlighting-faces))
      (when (and (featurep 'xemacs)
		 ;; Do not touch customized  faces.
		 (not (get face-name 'saved-face)))
	(set-face-parent face-name f-inherit)
	;; Explicitely set the size again to code around the bug that
	;; `set-face-parent' overwrites the original face size.
	(make-face-size face-name size)))))

(font-latex-make-sectioning-faces font-latex-sectioning-max)


;;; Keywords

(defvar font-latex-keywords-1 nil
  "Subdued level highlighting for LaTeX modes.")

(defvar font-latex-keywords-2 nil
  "High level highlighting for LaTeX modes.")

(defvar font-latex-built-in-keyword-classes
  '(("warning"
     ("nopagebreak" "pagebreak" "newpage" "clearpage" "cleardoublepage"
      "enlargethispage" "nolinebreak" "linebreak" "newline" "-" "\\" "\\*"
      "appendix" "displaybreak" "allowdisplaybreaks" "include")
     'font-latex-warning-face 1 noarg)
    ("variable"
     (("setlength" "|{\\{") ("settowidth" "|{\\{") ("settoheight" "{{")
      ("settodepth" "{{") ("setcounter" "{|{\\")
      ("addtolength" "|{\\{") ("addtocounter" "{|{\\")
      ("stepcounter" "{") ("refstepcounter" "{")
      ("arabic" "{") ("roman" "{") ("Roman" "{") ("alph" "{") ("Alph" "{")
      ("fnsymbol" "{"))
     'font-lock-variable-name-face 2 command)
    ("biblatexnoarg"
     ("newrefsegment" "mancite" "pno" "ppno" "nopp" "psq" "psqq")
     'font-lock-variable-name-face 2 noarg)
    ("biblatex"
     (("newrefsection" "[") ("ExecuteBibliographyOptions" "[{")
      ("printbibliography" "[") ("printshorthands" "[") ("printbibheading" "[")
      ("addbibresource" "[{") ("addglobalbib" "[{") ("addsectionbib" "[{")
      ("bibbysection" "[") ("bibbysegment" "[") ("bibbycategory" "[")
      ("DeclareBibliographyCategory" "{") ("addtocategory" "{{") ("defbibenvironment" "{{{{")
      ("defbibheading" "{[{") ("defbibnote" "{{") ("defbibfilter" "{{") ("defbibcheck" "{{")
      ("defbibentryset" "{{") ("Cite" "[[{") ("parencite" "*[[{") ("Parencite" "[[{")
      ("footcite" "[[{") ("footcitetext" "[[{") ("textcite" "[[{") ("Textcite" "[[{")
      ("smartcite" "[[{") ("Smartcite" "[[{") ("supercite" "{") ("autocite" "*[[{")
      ("Autocite" "*[[{") ("citeauthor" "[[{") ("Citeauthor" "[[{") ("citetitle" "*[[{")
      ("citeyear" "*[[{") ("citedate" "*[[{") ("citeurl" "[[{") ("parentext" "{")
      ("brackettext" "{") ("fullcite" "[[{") ("fullfootcite" "[[{") ("volcite" "[{[[")
      ("Volcite" "[{[[") ("pvolcite" "[{[[") ("Pvolcite" "[{[[") ("fvolcite" "[{[[")
      ("ftvolcite" "[{[[") ("svolcite" "[{[[") ("Svolcite" "[{[[") ("tvolcite" "[{[[")
      ("Tvolcite" "[{[[") ("avolcite" "[{[[") ("Avolcite" "[{[[") ("notecite" "[[{")
      ("Notecite" "[[{") ("pnotecite" "[[{") ("Pnotecite" "[[{") ("fnotecite" "[[{")
      ("citename" "[[{[{") ("citelist" "[[{[{") ("citefield" "[[{[{") ("citereset" "*")
      ("RN" "{") ("Rn" "{") ("DefineBibliographyStrings" "{{") ("DefineBibliographyExtras" "{{")
      ("UndefineBibliographyExtras" "{{") ("DefineHyphenationExceptions" "{{")
      ("NewBibliographyString" "{") ("autocites" "(([[{") ("Autocites" "(([[{")
      ("cites" "(([[{") ("Cites" "(([[{") ("parencites" "(([[{") ("Parencites" "(([[{")
      ("footcites" "(([[{") ("footcitetexts" "(([[{") ("smartcites" "(([[{")
      ("Smartcites" "(([[{") ("textcites" "(([[{") ("Textcites" "(([[{") ("supercites" "(([[{"))
     'font-lock-constant-face 2 command)
    ("reference"
     (("nocite" "*{") ("cite" "*[[{") ("label" "{") ("pageref" "{")
      ("vref" "{") ("eqref" "{") ("ref" "{") ("include" "{")
      ("input" "{") ("bibliography" "{") ("index" "{") ("glossary" "{")
      ("footnote" "[{") ("footnotemark" "[") ("footnotetext" "[{"))
     'font-lock-constant-face 2 command)
    ("function"
     (("begin" "{") ("end" "{") ("pagenumbering" "{")
      ("thispagestyle" "{") ("pagestyle" "{") ("nofiles" "")
      ("includeonly" "{") ("bibliographystyle" "{") ("documentstyle" "[{")
      ("documentclass" "[{[") ("newenvironment" "*{[[{{")
      ("newcommand" "*|{\\[[{") ("newlength" "|{\\")
      ("newtheorem" "{[{[")
      ("providecommand" "*|{\\[[{")
      ("newcounter" "{[") ("renewenvironment" "*{[[{{")
      ("renewcommand" "*|{\\[[{") ("renewtheorem" "{[{[")
      ("usepackage" "[{[") ("fbox" "{") ("mbox" "{") ("rule" "[{{")
      ("vspace" "*{") ("hspace" "*{") ("thinspace" "") ("negthinspace" "")
      ;; XXX: Should macros without arguments rather be listed in a
      ;; separate category with 'noarg instead of 'command handling?
      ("enspace" "") ("enskip" "") ("quad" "") ("qquad" "") ("nonumber" "")
      ("centering" "") ("TeX" "") ("LaTeX" ""))
     'font-lock-function-name-face 2 command)
    ("sectioning-0"
     (("part" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
	 'font-lock-type-face
       'font-latex-sectioning-0-face)
     2 command)
    ("sectioning-1"
     (("chapter" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
	 'font-lock-type-face
       'font-latex-sectioning-1-face)
     2 command)
    ("sectioning-2"
     (("section" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
	 'font-lock-type-face
       'font-latex-sectioning-2-face)
     2 command)
    ("sectioning-3"
     (("subsection" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
	 'font-lock-type-face
       'font-latex-sectioning-3-face)
     2 command)
    ("sectioning-4"
     (("subsubsection" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
	 'font-lock-type-face
       'font-latex-sectioning-4-face)
     2 command)
    ("sectioning-5"
     (("paragraph" "*[{") ("subparagraph" "*[{")
      ("subsubparagraph" "*[{"))
     (if (eq font-latex-fontify-sectioning 'color)
	 'font-lock-type-face
       'font-latex-sectioning-5-face)
     2 command)
    ("slide-title" () 'font-latex-slide-title-face 2 command)
    ("textual"
     (("item" "[") ("title" "{") ("author" "{") ("date" "{")
      ("thanks" "{") ("address" "{") ("caption" "[{")
      ("textsuperscript" "{"))
     'font-lock-type-face 2 command)
    ("bold-command"
     (("textbf" "{") ("textsc" "{") ("textup" "{") ("boldsymbol" "{")
      ("pmb" "{"))
     'font-latex-bold-face 1 command)
    ("italic-command"
     (("emph" "{") ("textit" "{") ("textsl" "{"))
     'font-latex-italic-face 1 command)
    ("math-command"
     (("ensuremath" "|{\\"))
     'font-latex-math-face 1 command)
    ("type-command"
     (("texttt" "{") ("textsf" "{") ("textrm" "{") ("textmd" "{"))
     'font-lock-type-face 1 command)
    ("bold-declaration"
     ("bf" "bfseries" "sc" "scshape" "upshape")
     'font-latex-bold-face 1 declaration)
    ("italic-declaration"
     ("em" "it" "itshape" "sl" "slshape")
     'font-latex-italic-face 1 declaration)
    ("type-declaration"
     ("tt" "ttfamily" "sf" "sffamily" "rm" "rmfamily" "mdseries"
      "tiny" "scriptsize" "footnotesize" "small" "normalsize"
      "large" "Large" "LARGE" "huge" "Huge")
     'font-lock-type-face 1 declaration))
  "Built-in keywords and specifications for font locking.

The first element of each item is the name of the keyword class.

The second element is a list of keywords (macros without an
escape character) to highlight or, if the fifth element is the
symbol 'command, a list of lists where the first element of each
item is a keyword and the second a string specifying the macro
syntax.  It can contain \"*\" if the macro has a starred variant,
\"[\" for an optional argument, \"{\" for a mandatory argument,
and \"\\\" for a macro.  A \"|\" means the following two tokens
should be regarded as alternatives.

The third element is the symbol of a face to be used or a Lisp
form returning a face symbol.

The fourth element is the fontification level.

The fifth element is the type of construct to be matched.  It can
be one of 'noarg which will match simple macros without
arguments (like \"\\foo\"), 'declaration which will match macros
inside a TeX group (like \"{\\bfseries foo}\"), or 'command which
will match macros of the form \"\\foo[bar]{baz}\".")

(defcustom font-latex-deactivated-keyword-classes nil
  "List of strings for built-in keyword classes to be deactivated.

Valid entries are \"warning\", \"variable\", \"reference\",
\"function\" , \"sectioning-0\", \"sectioning-1\", \"sectioning-2\",
\"sectioning-3\", \"sectioning-4\", \"sectioning-5\", \"textual\",
\"bold-command\", \"italic-command\", \"math-command\", \"type-command\",
\"bold-declaration\", \"italic-declaration\", \"type-declaration\".

You have to restart Emacs for a change of this variable to take effect."
  :group 'font-latex-keywords
  :type `(set ,@(mapcar
		 (lambda (spec)
		   `(const :tag ,(concat
				  ;; Name of the keyword class
				  (let ((name (split-string (car spec) "-")))
				    (setcar name (capitalize (car name)))
				    (mapconcat 'identity name " "))
				  " keywords in `"
				  ;; Name of the face
				  (symbol-name (eval (nth 2 spec))) "'.\n"
				  ;; List of keywords
				  (with-temp-buffer
				    (insert "  Keywords: "
					    (mapconcat (lambda (x)
							 (if (listp x)
							     (car x)
							   x))
						       (nth 1 spec) ", "))
				    (fill-paragraph nil)
				    (buffer-substring-no-properties
				     (point-min) (point-max))))
			   ,(car spec)))
		 font-latex-built-in-keyword-classes)))

(defun font-latex-make-match-defun (prefix name face type)
  "Return a function definition for keyword matching.
The variable holding the keywords to match are determined by the
strings PREFIX and NAME.  The type of matcher is determined by
the symbol TYPE.

This is a helper function for `font-latex-make-built-in-keywords'
and `font-latex-make-user-keywords' and not intended for general
use."
  ;; Note: The functions are byte-compiled at the end of font-latex.el.
  ;; FIXME: Is the cond-clause possible inside of the defun?

  ;; In an earlier version of font-latex the type could be a list like
  ;; (command 1).  This indicated a macro with one argument.  Provide
  ;; a match function in this case but don't actually support it.
  (cond ((or (eq type 'command) (listp type))
	 (eval `(defun ,(intern (concat prefix name)) (limit)
		  ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex-make-match-defun'.")
		  (when ,(intern (concat prefix name))
		    (font-latex-match-command-with-arguments
		     ,(intern (concat prefix name))
		     (append
		      (when (boundp ',(intern (concat prefix name
						      "-keywords-local")))
			,(intern (concat prefix name "-keywords-local")))
		      ,(intern (concat prefix name "-keywords")))
		     ;; `face' can be a face symbol, a form returning
		     ;; a face symbol, or a list of face attributes.
		     (if (and (listp ,face) (functionp (car ,face)))
			 (eval ,face)
		       ,face)
		     limit)))))
	 ((eq type 'declaration)
	  (eval `(defun ,(intern (concat prefix name)) (limit)
		   ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex-make-match-defun'.")
		   (when ,(intern (concat prefix name))
		     (font-latex-match-command-in-braces
		      ,(intern (concat prefix name)) limit)))))
	 ((eq type 'noarg)
	  (eval `(defun ,(intern (concat prefix name)) (limit)
		   ,(concat "Fontify `" prefix name "' up to LIMIT.

Generated by `font-latex-make-match-defun'.")
		   (when ,(intern (concat prefix name))
		     (re-search-forward
		      ,(intern (concat prefix name)) limit t)))))))

(defun font-latex-keyword-matcher (prefix name face type)
  "Return a matcher and highlighter as required by `font-lock-keywords'.
PREFIX and NAME are strings which are concatenated to form the
respective match function.  FACE is a face name or a list of text
properties that will be applied to the respective part of the
match returned by the match function.  TYPE is the type of
construct to be highlighted.  Currently the symbols 'command,
'sectioning, 'declaration and 'noarg are valid.

This is a helper function for `font-latex-make-built-in-keywords'
and `font-latex-make-user-keywords' and not intended for general
use."
  ;; In an earlier version of font-latex the type could be a list like
  ;; (command 1).  This indicated a macro with one argument.  Provide
  ;; a matcher in this case but don't actually support it.
  (cond ((or (eq type 'command) (listp type))
	 `(,(intern (concat prefix name))
	   (0 (font-latex-matched-face 0) append t)
	   (1 (font-latex-matched-face 1) append t)
	   (2 (font-latex-matched-face 2) append t)
	   (3 (font-latex-matched-face 3) append t)
	   (4 (font-latex-matched-face 4) append t)
	   (5 (font-latex-matched-face 5) append t)
	   (6 (font-latex-matched-face 6) append t)
	   (7 (font-latex-matched-face 7) append t)))
	((eq type 'noarg)
	 `(,(intern (concat prefix name))
	   ;; Quote a list of face properties but do not to quote a face symbol.
	   (0 ,(if (and (listp face) (not (fboundp (car face))))
		   `',face
		 face))))
	((eq type 'declaration)
	 `(,(intern (concat prefix name))
	   (0 'font-latex-warning-face t t)
	   (1 'font-lock-keyword-face append t)
	   (2 ,face append t)))))

(defun font-latex-make-built-in-keywords ()
  "Build defuns, defvars and defcustoms for built-in keyword fontification."
  (dolist (item font-latex-built-in-keyword-classes)
    (let ((prefix "font-latex-match-")
	  (name (nth 0 item))
	  (keywords (nth 1 item))
	  (face (nth 2 item))
	  (level (nth 3 item))
	  (type (nth 4 item)))

      ;; defvar font-latex-match-*-keywords-local
      (eval `(defvar ,(intern (concat prefix name "-keywords-local"))
	       ',keywords
	       ,(concat "Buffer-local keywords to add to `"
			prefix name "-keywords'.

This must be a list where each element is a list consisting of a
keyword string \(not a regular expression\) omitting the leading
backslash and a format specifier as.  The options for the format
specifier are described in the doc string of
`font-latex-user-keyword-classes'.

This is an internal variable which should not be set directly.
Use `font-latex-add-keywords' instead.

Generated by `font-latex-make-built-in-keywords'.")))
      (make-variable-buffer-local
       (intern (concat prefix name "-keywords-local")))

      ;; defun font-latex-match-*-make
      ;; Note: The functions are byte-compiled at the end of font-latex.el.
      (eval `(defun ,(intern (concat prefix name "-make")) ()
	       ,(concat "Make or remake the variable `" prefix name "'.

Generated by `font-latex-make-built-in-keywords'.")
	       (let ((keywords
		      (append
		       (unless (member ,name
				       font-latex-deactivated-keyword-classes)
			 ,(intern (concat prefix name "-keywords-local")))
		       ,(intern (concat prefix name "-keywords"))))
		     multi-char-macros single-char-macros)
		 (dolist (elt keywords)
		   (let ((keyword (if (listp elt) (car elt) elt)))
		     (if (string-match "^[A-Za-z]" keyword)
			 (push keyword multi-char-macros)
		       (push keyword single-char-macros))))
		 (when (or multi-char-macros single-char-macros)
		   (setq ,(intern (concat prefix name))
			 (concat
			  "\\\\\\("
			  (when multi-char-macros
			    (concat
			     "\\(?:" (regexp-opt multi-char-macros) "\\)\\>"))
			  (when single-char-macros
			    (concat
			     (when multi-char-macros "\\|")
			     "\\(?:" (regexp-opt single-char-macros) "\\)"))
			  "\\)"))))))

      ;; defcustom font-latex-match-*-keywords
      (eval `(defcustom ,(intern (concat prefix name "-keywords")) nil
	       ,(concat "List of keywords "
			(when (eq type 'command) "and formats ")
			"for " name " face.\n"
			(if (eq type 'command)
			    "\
Each element has to be a list consisting of the name of a macro
omitting the leading backslash and a format specifier as
described in the doc string of `font-latex-user-keyword-classes'."
			  "\
Each element has to be the name of a macro as a string, omitting
the leading backslash.")
			"\n\n\
Setting this variable directly does not take effect; restart
Emacs.

Generated by `font-latex-make-built-in-keywords'.")
	       :type '(repeat (string :tag "Keyword"))
	       :type '(repeat ,(if (eq type 'command)
				   '(list (string :tag "Keyword")
					  (string :tag "Format"))
				 '(string :tag "Keyword")))
	       :set (lambda (symbol value)
		      (set-default symbol value)
		      (funcall ',(intern (concat prefix name "-make"))))
	       :group 'font-latex-keywords))

      ;; defvar font-latex-match-*
      (eval `(defvar ,(intern (concat prefix name))
	       ,(intern (concat prefix name "-keywords"))))
      (make-variable-buffer-local (intern (concat prefix name)))

      ;; defun font-latex-match-*
      (font-latex-make-match-defun prefix name face type)

      ;; Add matchers and highlighters to `font-latex-keywords-{1,2}'.
      (let ((keywords-entry (font-latex-keyword-matcher
			     prefix name face type)))
	(add-to-list (intern (concat "font-latex-keywords-"
				     (number-to-string level)))
		     keywords-entry t)
	(when (= level 1)
	  (add-to-list (intern (concat "font-latex-keywords-2"))
		       keywords-entry t))))))
(font-latex-make-built-in-keywords)

(defcustom font-latex-user-keyword-classes nil
  "List of user-defined keyword classes for font locking.

Every keyword class consists of four parts, a name, a list of
keywords, a face and a specifier for the type of macro to be
highlighted.

When adding new entries, you have to use unique values for the
class names, i.e. they must not clash with names of the built-in
keyword classes or other names given by you.  Additionally the
names must not contain spaces.

The list of keywords defines which commands and declarations
should be covered by the keyword class.  A keyword can either be
a simple command name omitting the leading backslash or a list
consisting of the command name and a string specifying the syntax
of the command.  The latter is useful if you want to match LaTeX
macros with arguments (see below).  You can specify the occurence
and order of optional (\"[\") and mandatory (\"{\") arguments for
each keyword.  For example for \"documentclass\" you'd use \"[{\"
because the macro has one optional followed by one mandatory
argument.  Optionally starred macros can be indicated with \"*\".
In case an argument is an unbraced macro, use \"\\\".  You can
also specify two alternative arguments by prefixing them with
\"|\".  As an example, the specifier for \\newcommand is
\"*|{\\=\\[[{\".

The face argument can either be an existing face or a font
specification.  (The latter option is not available in XEmacs.)

There are three alternatives for the class type:

A value of `command' indicates commands with arguments
\(\"\\foo[bar]{baz}\").  The mandatory arguments in curly braces
will get the face you specified.

A value of `declaration' indicates declarations inside of TeX
groups (\"{\\foo bar}\").  The content inside the braces,
excluding the command, will get the face you specified.  In case
the braces are missing, the face will be applied to the command
itself.

A value of `noarg' indicates commands without arguments
\(\"\\foo\").  The command itself will get the face you
specified.

Setting this variable directly does not take effect;
use \\[customize] or restart Emacs."
  :group 'font-latex-keywords
  :type `(repeat (list (string :tag "Name")
		       (choice (repeat :tag "Keywords" (string :tag "Keyword"))
			       (repeat
				:tag "Keywords with specs"
				(group (string :tag "Keyword")
				       (string :tag "Format specifier"))))
		       ,(if (featurep 'xemacs)
			    '(face :tag "Face name")
			  '(choice (custom-face-edit :tag "Face attributes")
				   (face :tag "Face name")))
		       (choice :tag "Type"
			       ;; Maps to
			       ;;`font-latex-match-command-with-arguments'
			       (const :tag "Command with arguments"
				      command)
			       ;; Maps to
			       ;;`font-latex-match-command-in-braces'
			       (const :tag "Declaration inside TeX group"
				      declaration)
			       ;; Maps to `re-search-forward'
			       (const :tag "Command without arguments"
				      noarg))))
  :set (lambda (symbol value)
	 (dolist (item value)
	   (when (string-match " " (car item))
	     (error "No spaces allowed in name")))
	 (let (names names-uniq)
	   (dolist (item (append font-latex-built-in-keyword-classes value))
	     (setq names (append names (list (car item)))))
	   (setq names (TeX-sort-strings names))
	   (setq names-uniq (TeX-delete-duplicate-strings names))
	   (dotimes (i (safe-length names-uniq))
	     (unless (string= (nth i names) (nth i names-uniq))
	       (error "Name %S already exists" (nth i names)))))
	 (set-default symbol value)
	 (let ((prefix "font-latex-match-"))
	   (dolist (elt value)
	     (unless (boundp (intern (concat prefix (car elt))))
	       ;; defvar font-latex-match-*
	       (eval `(defvar ,(intern (concat prefix (car elt))) nil)))
	     (let ((keywords (nth 1 elt))
		   single-char-macro-flag)
	       (setq keywords (if (listp (car keywords))
				  (mapcar 'car keywords)
				keywords))
	       (catch 'single-char
		 (dolist (keyword keywords)
		   (unless (string-match "^[A-Za-z]" keyword)
		     (setq single-char-macro-flag t)
		     (throw 'single-char nil))))
	       (set (intern (concat prefix (car elt)))
		    (when (> (safe-length keywords) 0)
		    (concat "\\\\" (let ((max-specpdl-size 1000))
				     (regexp-opt keywords t))
			    (unless single-char-macro-flag "\\>")))))))))

(defun font-latex-make-user-keywords ()
  "Build defuns and defvars for user keyword fontification."
  (let ((keyword-specs font-latex-user-keyword-classes))
    (dolist (item keyword-specs)
      (let ((prefix "font-latex-match-")
	    (name (nth 0 item))
	    (keywords (nth 1 item))
	    (face (nth 2 item))
	    (type (nth 3 item)))

	;; defvar font-latex-match-*-keywords
	(eval `(defvar ,(intern (concat prefix name "-keywords")) ',keywords
		 ,(concat "Font-latex keywords for " name " face.

Generated by `font-latex-make-user-keywords'.")))

	;; defun font-latex-match-*
	(eval `(font-latex-make-match-defun prefix name '',face type))

	;; Add the matcher to `font-latex-keywords-2'.
	(add-to-list 'font-latex-keywords-2
		     (font-latex-keyword-matcher prefix name face type) t))))

  ;; Add the "fixed" matchers and highlighters.
  (dolist (item
	   '(("\\(^\\|[^\\]\\)\\(&+\\)" 2 'font-latex-warning-face)
	     ("\\$\\$\\([^$]+\\)\\$\\$" 1 'font-latex-math-face)
	     (font-latex-match-quotation
	      (0 'font-latex-string-face append)
	      (1 'font-latex-warning-face))
	     ;; Hack to remove the verbatim face from the \ in
	     ;; \end{verbatim} and similar.  The same hack is used in
	     ;; tex-mode.el.
	     ("^[ \t]*\\(\\\\\\)end"
	      (1 (get-text-property (match-end 1) 'face) t))))
    (add-to-list 'font-latex-keywords-1 item)
    (add-to-list 'font-latex-keywords-2 item))
  (dolist (item
	   '((font-latex-match-math-env
	      (0 'font-latex-warning-face t t)
	      (1 'font-latex-math-face append t))
	     (font-latex-match-math-envII
	      (0 'font-latex-math-face append t))
	     (font-latex-match-simple-command
	      (0 'font-latex-sedate-face append))
	     (font-latex-match-script
	      (1 (font-latex-script (match-beginning 0)) append))))
    (add-to-list 'font-latex-keywords-2 item t)))
(font-latex-make-user-keywords)

(defun font-latex-add-keywords (keywords class)
  "Add KEYWORDS to CLASS.
KEYWORDS is a list of keywords or keywords with syntax specs.
CLASS corresponds to a keyword class and can be one of the
symbols 'warning, 'variable, 'reference, 'biblatex, 'function,
'sectioning-0, 'sectioning-1, 'sectioning-2, 'sectioning-3,
'sectioning-4, 'sectioning-5, 'slide-title, 'textual,
'bold-command, 'italic-command, 'math-command, 'type-command,
'bold-declaration, 'italic-declaration or 'type-declaration.

The keywords will be added to the buffer-local list of keywords
of the respective keyword class and necessary updates of the font
locking machinery will be triggered."
  (let* ((class (symbol-name class))
	 (list (intern (format "font-latex-match-%s-keywords-local" class))))
    (dolist (elt keywords)
      (add-to-list list elt))
    (funcall (intern (format "font-latex-match-%s-make" class)))
    (setq font-lock-set-defaults nil)
    (font-lock-set-defaults)))

(defvar font-latex-keywords font-latex-keywords-1
  "Default expressions to highlight in TeX mode.")


;;; Subscript and superscript

(defcustom font-latex-fontify-script (not (featurep 'xemacs))
  "If non-nil, fontify subscript and superscript strings.
This feature does not work in XEmacs."
  :type 'boolean
  :group 'font-latex)
(put 'font-latex-fontify-script 'safe-local-variable 'TeX-booleanp)

(defcustom font-latex-script-display '((raise -0.3) . (raise 0.3))
  "Display specification for subscript and superscript content.
The car is used for subscript, the cdr is used for superscripts."
  :group 'font-latex
  :type '(cons (choice (sexp :tag "Subscript form")
		       (const :tag "No lowering" nil))
	       (choice (sexp :tag "Superscript form")
		       (const :tag "No raising" nil))))


;;; Syntactic keywords

(defvar font-latex-syntactic-keywords nil
  "Syntactic keywords used by `font-latex'.")
(make-variable-buffer-local 'font-latex-syntactic-keywords)

(defvar font-latex-syntactic-keywords-extra nil
  "List of syntactic keywords to add to `font-latex-syntactic-keywords'.
The form should be the same as in `font-lock-syntactic-keywords'.")
(make-variable-buffer-local 'font-latex-syntactic-keywords-extra)

;; Set and updated in `font-latex-set-syntactic-keywords'.
(defvar font-latex-doctex-syntactic-keywords nil)

(defun font-latex-set-syntactic-keywords ()
  "Set the variable `font-latex-syntactic-keywords'.
This function can be used to refresh the variable in case other
variables influencing its value, like `LaTeX-verbatim-environments',
have changed."
  ;; Checks for non-emptiness of lists added in order to cater for
  ;; installations where `(regexp-opt-group nil)' would enter a loop.
  (let ((verb-envs (and (fboundp 'LaTeX-verbatim-environments)
			(LaTeX-verbatim-environments)))
	(verb-macros-with-delims
	 (and (fboundp 'LaTeX-verbatim-macros-with-delims)
	      (LaTeX-verbatim-macros-with-delims)))
	(verb-macros-with-braces
	 (and (fboundp 'LaTeX-verbatim-macros-with-braces)
	      (LaTeX-verbatim-macros-with-braces))))
    (setq verb-envs (and verb-envs (regexp-opt verb-envs))
	  verb-macros-with-delims (and verb-macros-with-delims
				       (regexp-opt verb-macros-with-delims))
	  verb-macros-with-braces (and verb-macros-with-braces
				       (regexp-opt verb-macros-with-braces))
	  font-latex-syntactic-keywords nil)
    (unless (= (length verb-envs) 0)
      (add-to-list 'font-latex-syntactic-keywords
		   `(,(concat
		       "^[ \t]*\\\\begin *{\\(?:" verb-envs "\\)}"
		       ;; Some environments accept an optional and/or mandatory
		       ;; argument that can span over more lines.  Between
		       ;; "\begin{<envname>}" and the optional argument there
		       ;; can be whitespaces and the newline can be commented
		       ;; by a "%" character.
		       "[ \t]*\\(?:%.*\n[ \t]*\\)?"
		       ;; The following line of the regexp matches the optional
		       ;; argument and allows for up to one level of brackets
		       ;; inside the argument (e.g., the dialect of a language
		       ;; in the `lstlisting' environment by the `listings'
		       ;; package).
		       "\\(?:\\[[^\]\[]*\\(?:\\[[^\]\[]*\\][^\]\[]*\\)*\\]\\)?"
		       ;; After the optional argument, there may also be
		       ;; another mandatory argument (e.g. with VerbatimOut or
		       ;; the minted envs).
		       "\\(?:{[^}]+}\\)?"
		       "\\(\n\\)")
		     (1 "|" t)))
      (add-to-list 'font-latex-syntactic-keywords
		   ;; Using the newline character for the syntax
		   ;; property often resulted in fontification
		   ;; problems when text was inserted at the end of
		   ;; the verbatim environment.  That's why we now use
		   ;; the starting backslash of \end.  There is a hack
		   ;; in `font-latex-make-user-keywords' to remove the
		   ;; spurious fontification of the backslash.
		   `(,(concat "^[ \t]*\\(\\\\\\)end *{\\(?:" verb-envs "\\)}")
		     (1 "|" t))))
    (unless (= (length verb-macros-with-delims) 0)
      (add-to-list 'font-latex-syntactic-keywords
		   `(,(concat "\\\\\\(?:" verb-macros-with-delims "\\)"
			      ;; An opening curly brace as delimiter
			      ;; is valid, but allowing it might screw
			      ;; up fontification of stuff like
			      ;; "\url{...} foo \textbf{<--!...}".
			      "\\([^a-z@*\n\f{]\\).*?"
			      ;; Give an escape char at the end of the
			      ;; verbatim construct punctuation syntax.
			      ;; Prevents wrong fontification of stuff
			      ;; like "\verb|foo\|".
			      "\\(" (regexp-quote TeX-esc) "*\\)\\(\\1\\)")
		     (1 "\"") (2 ".") (3 "\""))))
    (unless (= (length verb-macros-with-braces) 0)
      (add-to-list 'font-latex-syntactic-keywords
		   `(,(concat "\\\\\\(?:" verb-macros-with-braces "\\)"
			      "\\({\\).*?[^\\]\\(?:\\\\\\\\\\)*\\(}\\)")
		     (1 "|") (2 "|")))))
  (when font-latex-syntactic-keywords-extra
    (nconc font-latex-syntactic-keywords font-latex-syntactic-keywords-extra))
  ;; Cater for docTeX mode.
  (setq font-latex-doctex-syntactic-keywords
	(append font-latex-syntactic-keywords
		;; For docTeX comment-in-doc.
		`(("\\(\\^\\)\\^A" (1 (font-latex-doctex-^^A)))))))


;;; Syntactic fontification

;; Copy and adaptation of `tex-font-lock-syntactic-face-function' in
;; `tex-mode.el' of CVS Emacs (March 2004)
(defun font-latex-syntactic-face-function (state)
  (let ((char (nth 3 state)))
    (cond
     ((not char) 'font-lock-comment-face)
     ((eq char ?$) 'font-latex-math-face)
     (t
      (when (char-valid-p char)
	;; This is a \verb?...? construct.  Let's find the end and mark it.
	(save-excursion
	  (skip-chars-forward (string ?^ char)) ;; Use `end' ?
	  (when (eq (char-syntax (preceding-char)) ?/)
	    (put-text-property (1- (point)) (point) 'syntax-table '(1)))
	  (unless (eobp)
	    (put-text-property (point) (1+ (point)) 'syntax-table '(7)))))
      'font-latex-verbatim-face))))


;;; Faces

(defface font-latex-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-latex-highlighting-faces)

(defface font-latex-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-latex-highlighting-faces)

(defface font-latex-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-latex-highlighting-faces)

(defface font-latex-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-latex-highlighting-faces)

(defface font-latex-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-latex-highlighting-faces)

(defface font-latex-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-latex-highlighting-faces)

(defface font-latex-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
		       (if (featurep 'xemacs)
			   (find-face 'fixed-pitch)
			 (facep 'fixed-pitch)))
		  '(:inherit fixed-pitch)
		'(:family "courier"))))
    `((((class grayscale) (background light))
	 (:foreground "DimGray" ,@font))
	(((class grayscale) (background dark))
	 (:foreground "LightGray" ,@font))
	(((class color) (background light))
	 (:foreground "SaddleBrown" ,@font))
	(((class color) (background dark))
	 (:foreground "burlywood" ,@font))
	(t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-latex-highlighting-faces)

(defface font-latex-superscript-face
  '((t (:height 0.8)))
  "Face used for superscripts."
  :group 'font-latex-highlighting-faces)

(defface font-latex-subscript-face
  '((t (:height 0.8)))
  "Face used for subscripts."
  :group 'font-latex-highlighting-faces)

(defface font-latex-slide-title-face
  (let* ((scale 1.2)
	 (size (when (featurep 'xemacs)
		 (round (* 0.9 (face-height 'default) scale)))))
    (if (featurep 'xemacs)
	`((t (:bold t :family "helvetica" :size ,size)))
      `((t (:inherit (variable-pitch font-lock-type-face)
		     :weight bold :height ,scale)))))
  "Face for slide titles."
  :group 'font-latex-highlighting-faces)
(when (featurep 'xemacs)
  (set-face-parent 'font-latex-slide-title-face 'font-lock-type-face
		   nil nil 'append))


;;; Setup

(defvar font-lock-comment-start-regexp nil
  "Regexp to match the start of a comment.")

(defvar font-latex-extend-region-functions nil
  "List of functions extending the region for multiline constructs.

Each function should accept two arguments, the begin and end of
the region to be fontified, and return the new region start.  If
no extension is necessary, the original region start should be
returned.

All specified functions will be called and the region extended
backwards to the minimum over their return values.")

(defvar font-latex-syntax-alist
  ;; Use word syntax for @ because we use \> for matching macros and
  ;; we don't want \foo@bar to be found if we search for \foo.
  '((?\( . ".") (?\) . ".") (?$ . "\"") (?@ . "w"))
  "List of specifiers for the syntax alist of `font-lock-defaults'.")

(defun font-latex-add-to-syntax-alist (list)
  "Activate syntactic font locking for the entries in LIST.
The entries are added to `font-latex-syntax-alist' and eventually
end up in `font-lock-defaults'.  Each entry in LIST should be a
cons pair as expected by `font-lock-defaults'.  The function also
triggers Font Lock to recognize the change."
  (make-local-variable 'font-latex-syntax-alist)
  (set (make-local-variable 'font-latex-syntax-alist)
       (append font-latex-syntax-alist list))
  ;; Tell font-lock about the update.
  (setq font-lock-set-defaults nil)
  (font-latex-setup))

;;;###autoload
(defun font-latex-setup ()
  "Setup this buffer for LaTeX font-lock.  Usually called from a hook."
  (font-latex-set-syntactic-keywords)
  ;; Trickery to make $$ fontification be in `font-latex-math-face' while
  ;; strings get whatever `font-lock-string-face' has been set to.
  (when (fboundp 'built-in-face-specifiers)
    ;; Cool patch from Christoph Wedler...
    (let (instance)
      (mapc (lambda (property)
	      (setq instance
		    (face-property-instance 'font-latex-math-face property
					    nil 0 t))
	      (if (numberp instance)
		  (setq instance
			(face-property-instance 'default property nil 0)))
	      (or (numberp instance)
		  (set-face-property 'font-lock-string-face property
				     instance (current-buffer))))
	    (built-in-face-specifiers))))

  ;; Activate multi-line fontification facilities if available.
  (when (boundp 'font-lock-multiline)
    (set (make-local-variable 'font-lock-multiline) t))

  ;; Functions for extending the region.
  (dolist (elt '(font-latex-extend-region-backwards-command-with-args
		 font-latex-extend-region-backwards-command-in-braces
		 font-latex-extend-region-backwards-quotation
		 font-latex-extend-region-backwards-math-env
		 font-latex-extend-region-backwards-math-envII))
    (add-to-list 'font-latex-extend-region-functions elt))

  ;; Tell Font Lock about the support.
  (make-local-variable 'font-lock-defaults)
  ;; The test for `major-mode' currently only works with docTeX mode
  ;; because `TeX-install-font-lock' is called explicitely in
  ;; `doctex-mode'.  In case other modes have to be distinguished as
  ;; well, remove the call to `TeX-install-font-lock' from
  ;; `VirTeX-common-initialization' and place it in the different
  ;; `xxx-mode' calls instead, but _after_ `major-mode' is set.
  (let ((defaults
	 `((font-latex-keywords font-latex-keywords-1 font-latex-keywords-2)
	   nil nil ,font-latex-syntax-alist nil))
	(variables
	 '((font-lock-comment-start-regexp . "%")
	   (font-lock-mark-block-function . mark-paragraph)
	   (font-lock-fontify-region-function
	    . font-latex-fontify-region)
	   (font-lock-unfontify-region-function
	    . font-latex-unfontify-region))))
    ;; Add the mode-dependent stuff to the basic variables defined above.
    (if (eq major-mode 'doctex-mode)
	(progn
	  (setcar defaults (append (car defaults)
				   '(font-latex-doctex-keywords)))
	  (setq variables
		(append variables
			'((font-lock-syntactic-face-function
			   . font-latex-doctex-syntactic-face-function)
			  (font-lock-syntactic-keywords
			   . font-latex-doctex-syntactic-keywords)))))
      (setq variables
	    (append variables
		    '((font-lock-syntactic-face-function
		       . font-latex-syntactic-face-function)
		      (font-lock-syntactic-keywords
		       . font-latex-syntactic-keywords)))))
    ;; Cater for the idiosyncrasies of Emacs and XEmacs.
    (if (featurep 'xemacs)
	(progn
	  ;; XEmacs does not set these variables via `font-lock-defaults'
	  ;; but requires them to be set explicitely.
	  (mapc (lambda (alist)
		  (set (car alist) (cdr alist))) variables)
	  ;; Has to be set to t as otherwise syntax properties will not be
	  ;; be picked up during fontification.
	  (set (make-local-variable 'lookup-syntax-properties) t))
      (setq defaults (append defaults variables)))
    ;; Set the defaults.
    (setq font-lock-defaults defaults)))

(defun font-latex-jit-lock-force-redisplay (buf start end)
  "Compatibility for Emacsen not offering `jit-lock-force-redisplay'."
    ;; The following block is an expansion of `jit-lock-force-redisplay'
    ;; and involved macros taken from CVS Emacs on 2007-04-28.
    (with-current-buffer buf
      (let ((modified (buffer-modified-p)))
	(unwind-protect
	    (let ((buffer-undo-list t)
		  (inhibit-read-only t)
		  (inhibit-point-motion-hooks t)
		  (inhibit-modification-hooks t)
		  deactivate-mark
		  buffer-file-name
		  buffer-file-truename)
	      (put-text-property start end 'fontified t))
	  (unless modified
	    (restore-buffer-modified-p nil))))))

(defun font-latex-fontify-region (beg end &optional loudly)
  "Fontify region from BEG to END.
If optional argument is non-nil, print status messages."
  (let ((extend-list (delq nil (mapcar (lambda (fun) (funcall fun beg end))
				       font-latex-extend-region-functions))))
    (when extend-list
      (let ((orig-beg beg))
	(setq beg (apply 'min extend-list))
	(when (featurep 'jit-lock)
	  ;; Stolen from `jit-lock-fontify-now' (2007-04-27) and
	  ;; adapted.  Without this stanza only the line in which a
	  ;; change happened will refontified.  The rest will only be
	  ;; refontified upon redisplay.
	  (run-with-timer 0 nil 'font-latex-jit-lock-force-redisplay
			  (current-buffer) beg orig-beg))))
    (font-lock-default-fontify-region beg end loudly)))

;; Copy and adaption of `tex-font-lock-unfontify-region' from
;; tex-mode.el in GNU Emacs on 2004-08-04.
;; (XEmacs passes a third argument to the function.)
(defun font-latex-unfontify-region (beg end &rest ignored)
  "Unfontify region from BEG to END."
  (font-lock-default-unfontify-region beg end)
  ;; XEmacs does not provide `font-lock-extra-managed-props', so
  ;; remove the `font-latex-multiline' property manually.  (The
  ;; property is only added if `font-lock-multiline' is bound.)
  (unless (boundp 'font-lock-multiline)
    (remove-text-properties beg end '(font-latex-multiline)))
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
	  (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
	       (member (car-safe (cdr prop))
		       (list (nth 1 (car font-latex-script-display))
			     (nth 1 (cdr font-latex-script-display))))
	       (null (cddr prop)))
	  (put-text-property beg next 'display nil))
      (setq beg next))))

(defadvice font-lock-after-change-function (before font-latex-after-change
						   activate)
  "Extend region for fontification of multiline constructs.
This is only necessary if the editor does not provide multiline
fontification facilities like `font-lock-multiline' itself."
  (unless (boundp 'font-lock-multiline)
    (let ((ad-beg (ad-get-arg 0))
	  (ad-end (ad-get-arg 1)))
      (save-excursion
	(goto-char ad-beg)
	(beginning-of-line)
	(when (get-text-property (point) 'font-latex-multiline)
	  (setq ad-beg (previous-single-property-change (point)
							'font-latex-multiline))
	  (when (numberp ad-beg)
	    (ad-set-arg 0 ad-beg)))
	(goto-char ad-end)
	(end-of-line)
	(when (get-text-property (point) 'font-latex-multiline)
	  (setq ad-end (next-single-property-change (point)
						    'font-latex-multiline))
	  (when (numberp ad-end)
	    (ad-set-arg 1 ad-end)))))))


;;; Utility functions

(defun font-latex-find-matching-close (openchar closechar)
  "Skip over matching pairs of OPENCHAR and CLOSECHAR.
OPENCHAR is the opening character and CLOSECHAR is the closing
character.  Character pairs are usually { } or [ ].  Comments are
ignored during the search."
  (let ((parse-sexp-ignore-comments
	 (not (eq major-mode 'doctex-mode))) ; scan-sexps ignores comments
        (init-point (point))
	(mycount 1)
	(esc-char (or (and (boundp 'TeX-esc) TeX-esc) "\\"))
	;; XXX: Do not look up syntax-table properties since they may
	;; be misleading, e.g. in the case of "{foo}^^A" where the
	;; closing brace gets a comment end syntax.
	(parse-sexp-lookup-properties nil))
    (or
     (condition-case nil
	 (progn
	   (goto-char (with-syntax-table
			  (TeX-search-syntax-table openchar closechar)
			(scan-sexps (point) 1)))
	   ;; No error code.  See if closechar is unquoted
	   (save-excursion
	     (backward-char 1)
	     (zerop (mod (skip-chars-backward (regexp-quote esc-char)) 2))))
       (error nil))
     (save-match-data
       (goto-char (1+ init-point))
       (while (and (> mycount 0)
		   (re-search-forward
		    (string ?\[
			    ;; closechar might be ]
			    ;; and therefor must be first in regexp
			    closechar openchar
			    ?\])
		    nil t))
	 (cond
	  ((font-latex-commented-outp)
	   (forward-line 1))
	  ((save-excursion
	     (backward-char 1)
	     (zerop (mod (skip-chars-backward (regexp-quote esc-char))
			 2)))
	   (setq mycount (+ mycount
			    (if (= (preceding-char) openchar) 1 -1)))))))
     (if (= mycount 0)
	 t
       (goto-char init-point)
       nil))))

(defun font-latex-commented-outp ()
  "Return t if comment character is found between bol and point."
  (save-excursion
    (let ((limit (point))
	  (esc-char (if (and (boundp 'TeX-esc) TeX-esc) TeX-esc "\\")))
      (forward-line 0)
      (if (and (eq (char-after) ?\%)
	       (not (font-latex-faces-present-p 'font-latex-verbatim-face)))
	  (not (eq major-mode 'doctex-mode))
	(catch 'found
	  (while (progn (skip-chars-forward "^%" limit)
			(< (point) limit))
	    (when (and (save-excursion
			 (zerop (mod (skip-chars-backward
				      (regexp-quote esc-char)) 2)))
		       (not (font-latex-faces-present-p
			     'font-latex-verbatim-face)))
	      (throw 'found t))
	    (forward-char)))))))

(defun font-latex-faces-present-p (faces &optional pos)
  "Return t if FACES are present at position POS.
FACES may be a single face or a list of faces.
If POS is omitted, the current position of point is used."
  (let* ((faces (if (listp faces) faces (list faces)))
	 (pos (or pos (point)))
	 (prop (get-text-property pos 'face))
	 (prop-list (if (listp prop) prop (list prop))))
    (catch 'member
      (dolist (item prop-list)
	(when (memq item faces)
	  (throw 'member t))))))

(defun font-latex-forward-comment ()
  "Like `forward-comment' but with special provisions for docTeX mode.
In docTeX mode \"%\" at the start of a line will be treated as whitespace."
  (if (eq major-mode 'doctex-mode)
      ;; XXX: We should probably cater for ^^A as well.
      (progn
	(while (progn (if (bolp) (skip-chars-forward "%"))
		      (> (skip-chars-forward " \t\n") 0)))
	(when (eq (char-after) ?%)
	  (beginning-of-line 2)
	  t))
    (forward-comment 1)))

(defun font-latex-put-multiline-property-maybe (beg end)
  "Add a multiline property if no equivalent is provided by the editor.
The text property is used to find the start or end of a multiline
construct when unfontifying a region.  Emacs adds such a text
property automatically if `font-lock-multiline' is set to t and
extends the region to be unfontified automatically as well.
XEmacs does not do this at the time of this writing."
  (unless (boundp 'font-lock-multiline)
    (put-text-property beg end 'font-latex-multiline t)))


;;; Match functions

(defvar font-latex-matched-faces nil
  "List of faces corresponding to matches in match data.")

(defun font-latex-matched-face (pos)
  "Return face at position POS in `font-latex-matched-faces'."
  (nth pos font-latex-matched-faces))

(defvar font-latex-command-with-args-default-spec nil ; "*[{"
  "Default specifier for keywords without syntax description.
Set this to nil if verification of command syntax is unwanted.")

(defvar font-latex-command-with-args-opt-arg-delims
  '((?[ . ?]) (?< . ?>) (?\( . ?\)))
  "List character pairs used as delimiters for optional arguments.")

(defvar font-latex-syntax-error-modes '(latex-mode)
  "List of modes where syntax errors in macros should be indicated.")

(defun font-latex-match-command-with-arguments (regexp keywords face limit)
  "Search for regexp command KEYWORDS[opt]{arg} before LIMIT.
Returns nil if none of KEYWORDS is found."
  (setq font-latex-matched-faces nil)
  (catch 'match
    (while (re-search-forward regexp limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
					    font-latex-verbatim-face)
					  (match-beginning 0))
	(let* ((beg (match-beginning 0))
	       end		   ; Used for multiline text property.
	       (match-data (list beg))
	       match-beg syntax-error alternative spec
	       error-indicator-pos
	       (spec-list (string-to-list
			   (or (cadr (assoc (match-string 1) keywords))
			       font-latex-command-with-args-default-spec)))
	       (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
	  (goto-char (match-end 0))
	  ;; Check for starred macro if first spec is an asterisk.
	  (when (eq (car spec-list) ?*)
	    (setq spec-list (cdr spec-list))
	    (skip-chars-forward "*" (1+ (point))))
	  ;; Add current point to match data and use keyword face for
	  ;; region from start to point.
	  (nconc match-data (list (point)))
	  (add-to-list 'font-latex-matched-faces 'font-lock-keyword-face)
	  (setq end (point))
	  (catch 'break
	    ;; Walk the list of specs.
	    (while spec-list
	      (setq spec (pop spec-list)
		    error-indicator-pos beg)
	      (while (and (not (eobp)) (font-latex-forward-comment)))
	      ;; Alternative
	      (when (eq spec ?|)
		(setq alternative t)
		(setq spec (pop spec-list)))
	      (cond
	       ;; Macros: \foo
	       ((eq spec ?\\)
		(if (eq (char-after) spec)
		    (progn
		      (nconc match-data
			     (list (point)
				   (progn
				     (forward-char)
				     (if (zerop (skip-chars-forward "A-Za-z@"))
					 (forward-char) ; Single-char macro.
				       (skip-chars-forward "*"))
				     (point))))
		      (nconc font-latex-matched-faces (list face))
		      (setq end (max end (point)))
		      (when alternative (pop spec-list)))
		  (setq syntax-error t)
		  (throw 'break nil)))
	       ;; Mandatory arguments: {...}
	       ((eq spec ?{)
		(if (and (eq (char-after) spec)
			 (setq match-beg (point))
			 (font-latex-find-matching-close ?{ ?}))
		    (progn
		      (nconc match-data (list (1+ match-beg) (1- (point))))
		      (nconc font-latex-matched-faces (list face))
		      (setq end (max end (1- (point))))
		      (when alternative (pop spec-list)))
		  (unless alternative
		    (setq syntax-error t)
		    (when (and match-beg (= match-beg (point)))
		      (setq error-indicator-pos match-beg))
		    (throw 'break nil))))
	       ;; Optional arguments: [...] and others
	       ((eq (char-after) spec)
		(setq match-beg (point))
		(if (font-latex-find-matching-close
		     spec (cdr (assq
				spec
				font-latex-command-with-args-opt-arg-delims)))
		    (progn
		      (nconc match-data (list (1+ match-beg) (1- (point))))
		      (nconc font-latex-matched-faces
			     (list 'font-lock-variable-name-face))
		      (setq end (max end (1- (point)))))
		  (setq syntax-error t
			error-indicator-pos match-beg)
		  (throw 'break nil))))
	      (setq alternative nil)))
	  (when (and syntax-error (memq major-mode
					font-latex-syntax-error-modes))
	    ;; Add the warning face at the front of the list because
	    ;; the matcher uses 'append and the face would otherwise
	    ;; be overridden by the keyword face.
	    (setq match-data (append (list error-indicator-pos
					   (1+ error-indicator-pos))
				     match-data))
	    (push 'font-latex-warning-face font-latex-matched-faces))
	  (font-latex-put-multiline-property-maybe beg end)
	  (store-match-data match-data)
	  (throw 'match t))))))

(defun font-latex-extend-region-backwards-command-with-args (beg end)
  "Return position to extend region backwards for commands with args.
Return nil if region does not have to be extended for a multiline
macro to fit in.  The region between the positions BEG and END
marks boundaries for searching for macro ends."
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (TeX-search-backward-unescaped "}" beg t)
	(let ((macro-start (TeX-find-macro-start
			    (max (point-min)
				 (- beg font-latex-multiline-boundary)))))
	  (when (and macro-start
		     (< macro-start beg))
	    (throw 'extend macro-start))))
      nil)))

(defun font-latex-match-command-in-braces (keywords limit)
  "Search for command like {\\bfseries fubar} before LIMIT.
Sets `match-data' so that:
 subexpression 0 is a warning indicator,
 subexpression 1 is the keyword, and
 subexpression 2 is the rest in the TeX group.
Returns nil if no command is found."
  (catch 'match
    (while (re-search-forward keywords limit t)
      (unless (font-latex-faces-present-p '(font-lock-comment-face
					    font-latex-verbatim-face)
					  (match-beginning 0))
	(let ((kbeg (match-beginning 0)) (kend (match-end 1))
	      (beg  (match-end 0))
	      cbeg cend
	      (parse-sexp-ignore-comments t)) ; scan-sexps ignores comments
	  (goto-char kbeg)
	  (if (not (eq (preceding-char) ?\{))
	      ;; Fontify only the keyword (no argument found).
	      (progn
		(setq cbeg kbeg cend kend)
		(goto-char (match-end 0))
		(store-match-data (list (point) (point)
					(point) (point)
					cbeg cend))
		(throw 'match t))
	    ;; There's an opening bracket
	    (save-restriction
	      ;; Restrict to LIMIT.
	      (narrow-to-region (point-min) limit)
	      (forward-char -1)		; Move on the opening bracket
	      (if (font-latex-find-matching-close ?\{ ?\})
		  (progn
		    (font-latex-put-multiline-property-maybe beg (1- (point)))
		    (store-match-data (list kbeg kbeg
					    kbeg kend
					    beg (1- (point)))))
		(goto-char kend)
		(store-match-data (list (1- kbeg) kbeg
					kbeg kend
					kend kend)))
	      (throw 'match t))))))))

(defun font-latex-extend-region-backwards-command-in-braces (beg end)
  "Return position to extend region backwards for commands in braces.
Return nil if region does not have to be extended for a multiline
group to fit in.  The region between the positions BEG and END
marks boundaries for searching for group ends."
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (TeX-search-backward-unescaped "}" beg t)
	(let ((group-start (TeX-find-opening-brace
			    nil (max (point-min)
				     (- beg font-latex-multiline-boundary)))))
	  (when group-start
	    ;; XXX: Actually we'd have to check if any of the
	    ;; declaration-type macros can be found right after the
	    ;; brace.  If we don't do this (like now) large regions
	    ;; may be refontified for no good reason.  For checking
	    ;; the built-in `font-latex-match-*' variables for
	    ;; declaration-type macros as well as the respective
	    ;; user-defined variables could be concatenated.
	    (goto-char group-start)
	    (when (< group-start beg)
	      (throw 'extend group-start)))))
      nil)))

(defun font-latex-match-simple-command (limit)
  "Search for command like \\foo before LIMIT."
  (TeX-re-search-forward-unescaped "\\\\[@A-Za-z]+" limit t))

(defun font-latex-match-math-env (limit)
  "Match math pattern up to LIMIT.
Used for patterns like:
\\( F = ma \\)
\\[ F = ma \\] but not \\\\ [len]"
  (catch 'match
    (while (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
      (unless (save-excursion
		(goto-char (match-beginning 0))
		;; \\[ does not start a math environment
		(/= (mod (skip-chars-backward "\\\\") 2) 0))
	(let ((beg (match-beginning 0))
	      (open-tag (if (match-beginning 1) "\\(" "\\["))
	      (close-tag (if (match-beginning 1) "\\)" "\\]")))
	  ;; Search for both opening and closing tags in order to be
	  ;; able to avoid erroneously matching stuff like "\(foo \(bar\)".
	  (if (and (re-search-forward (concat "[^\\]\\(?:\\\\\\\\\\)*\\("
					      (regexp-quote open-tag) "\\|"
					      (regexp-quote close-tag) "\\)")
				      limit 'move)
		   (string= (match-string 1) close-tag))
	      ;; Found closing tag.
	      (progn
		(font-latex-put-multiline-property-maybe beg (point))
		(store-match-data (list beg beg beg (point))))
	    ;; Did not find closing tag.
	    (goto-char (+ beg 2))
	    (store-match-data (list beg (point) (point) (point))))
	  (throw 'match t))))))

(defun font-latex-extend-region-backwards-math-env (beg end)
  "Return position to extend region backwards for math environments.
Return nil if region does not have to be extended for a multiline
environment to fit in.  The region between the positions BEG and
END marks boundaries for searching for environment ends."
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (re-search-backward "\\(\\\\)\\)\\|\\(\\\\]\\)" beg t)
	(when (and (zerop (mod (skip-chars-backward "\\\\") 2))
		   (re-search-backward
		    (concat "[^\\]\\(?:\\\\\\\\\\)*\\("
			    (regexp-quote (if (match-beginning 1) "\\(" "\\["))
			    "\\)")
		    (- beg font-latex-multiline-boundary) t)
		   (goto-char (match-beginning 1))
		   (< (point) beg))
	  (throw 'extend (point))))
      nil)))

(defcustom font-latex-math-environments
  '("display" "displaymath" "equation" "eqnarray" "gather" "multline"
    "align" "alignat" "xalignat" "xxalignat" "flalign")
  "List of math environment names for font locking."
  :type '(repeat string)
  :group 'font-latex)

(defun font-latex-match-math-envII (limit)
  "Match math patterns up to LIMIT.
Used for patterns like:
\\begin{equation}
 fontified stuff
\\end{equation}
The \\begin{equation} and \\end{equation} are not fontified here."
  (when (re-search-forward (concat "\\\\begin[ \t]*{"
				   (regexp-opt font-latex-math-environments t)
				   "\\*?}")
			   limit t)
    (let ((beg (match-end 0)) end)
      (if (re-search-forward (concat "\\\\end[ \t]*{"
				     (regexp-quote
				      (buffer-substring-no-properties
				       (match-beginning 1)
				       (match-end 0))))
			     ;; XXX: Should this rather be done by
			     ;; extending the region to be fontified?
			     (+ limit font-latex-multiline-boundary) 'move)
          (setq end (match-beginning 0))
	(goto-char beg)
        (setq end beg))
      (font-latex-put-multiline-property-maybe beg end)
      (store-match-data (list beg end))
      t)))

(defun font-latex-extend-region-backwards-math-envII (beg end)
  "Return position to extend region backwards for math environments.
Return nil if region does not have to be extended for a multiline
environment to fit in.  The region between the positions BEG and
END marks boundaries for searching for environment ends."
  (save-excursion
    (goto-char end)
    (catch 'extend
      (while (re-search-backward
	      (concat "\\\\end[ \t]*{"
		      (regexp-opt font-latex-math-environments t)
		      "\\*?}") beg t)
	(when (and (re-search-backward (concat  "\\\\begin[ \t]*{"
						(buffer-substring-no-properties
						 (match-beginning 1)
						 (match-end 0)))
				       (- beg font-latex-multiline-boundary) t)
		   (< (point) beg))
	  (throw 'extend (point))))
      nil)))

(defun font-latex-update-quote-list ()
  "Update quote list and regexp if value of `font-latex-quotes' changed."
  (unless (eq font-latex-quotes-control font-latex-quotes)
    (setq font-latex-quotes-control font-latex-quotes)
    (font-latex-quotes-set-internal)
    ;; Set order of each entry in `font-latex-quote-list' according to
    ;; setting of `font-latex-quotes-internal'.
    (let ((tail font-latex-quote-list)
	  elt)
      (while tail
	(setq elt (car tail))
	(when (and (> (safe-length elt) 2)
		   (not (eq (nth 2 elt) font-latex-quotes-internal)))
	  (setcar tail (list (nth 1 elt) (nth 0 elt)
			     font-latex-quotes-internal)))
	(setq tail (cdr tail))))
    (setq font-latex-quote-regexp-beg
	  (regexp-opt (mapcar 'car font-latex-quote-list) t))))

(defun font-latex-match-quotation (limit)
  "Match quote patterns up to LIMIT.
Used for patterns like:
``this is a normal quote'' and these are multilingual quoted strings:
\"< french \"> and \"`german\"' quotes.
The quotes << french >> and 8-bit french are used if `font-latex-quotes' is
set to french, and >>german<< (and 8-bit) are used if set to german."
  (when font-latex-quotes
    (font-latex-update-quote-list)
    ;; Search for matches.
    (catch 'match
      (while (TeX-re-search-forward-unescaped
	      font-latex-quote-regexp-beg limit t)
	(unless (font-latex-faces-present-p '(font-lock-comment-face
					      font-latex-verbatim-face
					      font-latex-math-face)
					    (match-beginning 0))
	  (let* ((beg (match-beginning 0))
		 (after-beg (match-end 0))
		 (opening-quote (match-string 0))
		 (closing-quote
		  (nth 1 (assoc (if (fboundp 'string-make-multibyte)
				    (string-make-multibyte (match-string 0))
				  (match-string 0))
				font-latex-quote-list)))
		 (nest-count 0)
		 (point-of-surrender (+ beg font-latex-multiline-boundary)))
	    ;; Find closing quote taking nested quotes into account.
	    (while (progn
		     (re-search-forward
		      (concat opening-quote "\\|" closing-quote)
		      point-of-surrender 'move)
		     (when (and (< (point) point-of-surrender) (not (eobp)))
		       (if (string= (match-string 0) opening-quote)
			   (setq nest-count (1+ nest-count))
			 (when (/= nest-count 0)
			   (setq nest-count (1- nest-count)))))))
	    ;; If no closing quote was found, set the second match which
	    ;; will be marked with warning color, if one was found, set
	    ;; the first match which will be marked with string color.
	    (if (or (= (point) point-of-surrender) (eobp))
		(progn
		  (goto-char after-beg)
		  (store-match-data (list after-beg after-beg beg after-beg)))
	      (font-latex-put-multiline-property-maybe beg (point))
	      (store-match-data (list beg (point) (point) (point))))
	    (throw 'match t)))))))

(defun font-latex-extend-region-backwards-quotation (beg end)
  "Return position to extend region backwards for quotations.
Return nil if region does not have to be extended for a multiline
quotation to fit in.  The region between the positions BEG and
END marks boundaries for searching for quotation ends."
  (if font-latex-quotes
      (progn
	(font-latex-update-quote-list)
	(let ((regexp-end (regexp-opt (mapcar 'cadr font-latex-quote-list) t)))
	  (save-excursion
	    (goto-char end)
	    (catch 'extend
	      (while (re-search-backward regexp-end beg t)
		(let ((closing-quote (match-string 0))
		      (nest-count 0)
		      (point-of-surrender (- beg font-latex-multiline-boundary))
		      opening-quote)
		  (catch 'found
		    (dolist (elt font-latex-quote-list)
		      (when (string= (cadr elt) closing-quote)
			(setq opening-quote (car elt))
			(throw 'found nil))))
		  ;; Find opening quote taking nested quotes into account.
		  (while (progn
			   (re-search-backward (concat opening-quote "\\|"
						       closing-quote)
					       point-of-surrender 'move)
			   (when (and (> (point) point-of-surrender)
				      (not (bobp)))
			     (if (string= (match-string 0) closing-quote)
				 (setq nest-count (1+ nest-count))
			       (when (/= nest-count 0)
				 (setq nest-count (1- nest-count)))))))
		  (when (< (point) beg)
		    (throw 'extend (point)))))
	      nil))))
    nil))

(defun font-latex-match-script (limit)
  "Match subscript and superscript patterns up to LIMIT."
  (when (and font-latex-fontify-script
	     (re-search-forward "[_^] *\\([^\n\\{}]\\|\
\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|\\({\\)\\)" limit t))
    (if (font-latex-faces-present-p '(font-latex-subscript-face
				      font-latex-superscript-face))
	;; Apply subscript and superscript highlighting only once in
	;; order to prevent the font size becoming too small.  We set
	;; an empty match to do that.
	(let ((point (point)))
	  (store-match-data (list point point point point)))
      (when (match-end 3)
	(let ((beg (match-beginning 3))
	      (end (TeX-find-closing-brace
		    ;; Don't match groups spanning more than one line
		    ;; in order to avoid visually wrong indentation in
		    ;; subsequent lines.
		    nil (line-end-position))))
	  (store-match-data (if end
				(list (match-beginning 0) end beg end)
			      (list beg beg beg beg))))))
    t))

;; Copy and adaption of `tex-font-lock-suscript' from tex-mode.el in
;; GNU Emacs on 2004-07-07.
(defun font-latex-script (pos)
  "Return face and display spec for subscript and superscript content."
  (when (and (font-latex-faces-present-p 'font-latex-math-face pos)
	     (not (font-latex-faces-present-p '(font-lock-constant-face
						font-lock-builtin-face
						font-lock-comment-face
						font-latex-verbatim-face) pos))
	     ;; Check for backslash quoting
	     (not (let ((odd nil)
			(pos pos))
		    (while (eq (char-before pos) ?\\)
		      (setq pos (1- pos) odd (not odd)))
		    odd)))
    ;; Adding other text properties than `face' is supported by
    ;; `font-lock-apply-highlight' in CVS Emacsen since 2001-10-28.
    ;; With the introduction of this feature the variable
    ;; `font-lock-extra-managed-props' was introduced and serves here
    ;; for feature checking.  XEmacs (CVS and 21.4.15) currently
    ;; (2004-08-18) does not support this feature.
    (let ((extra-props-flag (boundp 'font-lock-extra-managed-props)))
      (if (eq (char-after pos) ?_)
	  (if extra-props-flag
	      `(face font-latex-subscript-face display
		     ,(car font-latex-script-display))
	    'font-latex-subscript-face)
	(if extra-props-flag
	    `(face font-latex-superscript-face display
		   ,(cdr font-latex-script-display))
	  'font-latex-superscript-face)))))


;;; docTeX

(defvar font-latex-doctex-preprocessor-face
  'font-latex-doctex-preprocessor-face
  "Face used to highlight preprocessor directives in docTeX mode.")

(defface font-latex-doctex-preprocessor-face
  '((t (:inherit (font-latex-doctex-documentation-face
		  font-lock-builtin-face ; Emacs 21 does not provide
					 ; the preprocessor face.
		  font-lock-preprocessor-face))))
  "Face used to highlight preprocessor directives in docTeX mode."
  :group 'font-latex-highlighting-faces)

(defvar font-latex-doctex-documentation-face
  'font-latex-doctex-documentation-face
  "Face used to highlight the documentation in docTeX mode.")

(defface font-latex-doctex-documentation-face
  '((((class mono)) (:inverse-video t))
    (((class grayscale) (background dark)) (:background "#333"))
    (((class color) (background dark)) (:background "#333"))
    (t (:background "#eeeeee")))
  "Face used to highlight the documentation parts in docTeX mode."
  :group 'font-latex-highlighting-faces)

(defvar font-latex-doctex-keywords
  (append font-latex-keywords-2
	  '(("^%<[^>]*>" (0 font-latex-doctex-preprocessor-face t)))))

;; Copy and adaptation of `doctex-font-lock-^^A' in `tex-mode.el' of
;; CVS Emacs (March 2004)
(defun font-latex-doctex-^^A ()
  (if (eq (char-after (line-beginning-position)) ?\%)
      (progn
	(put-text-property
	 (1- (match-beginning 1)) (match-beginning 1) 'syntax-table
	 (if (= (1+ (line-beginning-position)) (match-beginning 1))
	     ;; The `%' is a single-char comment, which Emacs
	     ;; syntax-table can't deal with.  We could turn it
	     ;; into a non-comment, or use `\n%' or `%^' as the comment.
	     ;; Instead, we include it in the ^^A comment.
	     ;; COMPATIBILITY for Emacs 20 and XEmacs
	     (eval-when-compile (if (fboundp 'string-to-syntax)
				    (string-to-syntax "< b")
				  '(2097163)))
	   ;; COMPATIBILITY for Emacs 20 and XEmacs
	   (eval-when-compile (if (fboundp 'string-to-syntax)
				  (string-to-syntax ">")
				'(12)))))
	(let ((end (line-end-position)))
	  (if (< end (point-max))
	      (put-text-property end (1+ end) 'syntax-table
				    ;; COMPATIBILITY for Emacs 20 and XEmacs
				    (eval-when-compile
				      (if (fboundp 'string-to-syntax)
					  (string-to-syntax "> b")
					'(2097164))))))
	;; COMPATIBILITY for Emacs 20 and XEmacs
	(eval-when-compile (if (fboundp 'string-to-syntax)
			       (string-to-syntax "< b")
			     '(2097163))))))

;; Copy and adaptation of `doctex-font-lock-syntactic-face-function'
;; in `tex-mode.el' of CVS Emacs (March 2004)
(defun font-latex-doctex-syntactic-face-function (state)
  ;; Mark docTeX documentation, which is parsed as a style A comment
  ;; starting in column 0.
  (if (or (nth 3 state) (nth 7 state)
	  (not (memq (char-before (nth 8 state))
		     '(?\n nil))))
      ;; Anything else is just as for LaTeX.
      (font-latex-syntactic-face-function state)
    font-latex-doctex-documentation-face))


;;; Installation in non-AUCTeX LaTeX mode

(add-hook 'latex-mode-hook 'font-latex-setup)
;; If font-latex is loaded using a latex-mode-hook, then the add-hook above
;; won't be called this time around.  Check for this now:
(if (eq major-mode 'latex-mode)
    (font-latex-setup))


;;; Byte-compilation of generated functions

(when (byte-code-function-p
       (symbol-function 'font-latex-make-built-in-keywords))
  (dolist (elt font-latex-built-in-keyword-classes)
    (let ((name (nth 0 elt)))
      (byte-compile (intern (concat "font-latex-match-" name)))
      (byte-compile (intern (concat "font-latex-match-" name "-make"))))))


;; Provide ourselves:
(provide 'font-latex)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; font-latex.el ends here
