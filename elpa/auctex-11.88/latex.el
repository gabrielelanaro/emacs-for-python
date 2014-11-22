;;; latex.el --- Support for LaTeX documents.

;; Copyright (C) 1991, 1993-2014 Free Software Foundation, Inc.

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

;; This file provides AUCTeX support for LaTeX.

;;; Code:

(require 'tex)
(require 'tex-style)

;;; Syntax

(defvar LaTeX-optop "["
  "The LaTeX optional argument opening character.")

(defvar LaTeX-optcl "]"
  "The LaTeX optional argument closeing character.")

;;; Style

(defcustom LaTeX-default-style "article"
  "*Default when creating new documents."
  :group 'LaTeX-environment
  :type 'string)

(defcustom LaTeX-default-options nil
  "Default options to documentclass.
A comma-seperated list of strings."
  :group 'LaTeX-environment
  :type '(repeat (string :format "%v")))

(make-variable-buffer-local 'LaTeX-default-options)

(defcustom LaTeX-insert-into-comments t
  "*Whether insertion commands stay in comments.
This allows using the insertion commands even when
the lines are outcommented, like in dtx files."
  :group 'LaTeX-environment
  :type 'boolean)

(defun LaTeX-newline ()
  "Start a new line potentially staying within comments.
This depends on `LaTeX-insert-into-comments'."
  (if LaTeX-insert-into-comments
      (cond ((and (save-excursion (skip-chars-backward " \t") (bolp))
		  (save-excursion
		    (skip-chars-forward " \t")
		    (looking-at (concat TeX-comment-start-regexp "+"))))
	     (beginning-of-line)
	     (insert (buffer-substring-no-properties
		      (line-beginning-position) (match-end 0)))
	     (newline))
	    ((and (not (bolp))
		  (save-excursion
		    (skip-chars-forward " \t") (not (TeX-escaped-p)))
		  (looking-at
		   (concat "[ \t]*" TeX-comment-start-regexp "+[ \t]*")))
	     (delete-region (match-beginning 0) (match-end 0))
	     (indent-new-comment-line))
	    ;; `indent-new-comment-line' does nothing when
	    ;; `comment-auto-fill-only-comments' is non-il, so we must be sure
	    ;; to be in a comment before calling it.  In any other case
	    ;; `newline' is used.
	    ((TeX-in-comment)
	     (indent-new-comment-line))
	    (t
	     (newline)))
    (newline)))


;;; Syntax Table

(defvar LaTeX-mode-syntax-table (copy-syntax-table TeX-mode-syntax-table)
  "Syntax table used in LaTeX mode.")

(progn ; set [] to match for LaTeX.
  (modify-syntax-entry (string-to-char LaTeX-optop)
		       (concat "(" LaTeX-optcl)
		       LaTeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char LaTeX-optcl)
		       (concat ")" LaTeX-optop)
		       LaTeX-mode-syntax-table))

;;; Sections

(defun LaTeX-section (arg)
  "Insert a template for a LaTeX section.
Determine the type of section to be inserted, by the argument ARG.

If ARG is nil or missing, use the current level.
If ARG is a list (selected by \\[universal-argument]), go downward one level.
If ARG is negative, go up that many levels.
If ARG is positive or zero, use absolute level:

  0 : part
  1 : chapter
  2 : section
  3 : subsection
  4 : subsubsection
  5 : paragraph
  6 : subparagraph

The following variables can be set to customize:

`LaTeX-section-hook'	Hooks to run when inserting a section.
`LaTeX-section-label'	Prefix to all section labels."

  (interactive "*P")
  (let* ((val (prefix-numeric-value arg))
	 (level (cond ((null arg)
		       (LaTeX-current-section))
		      ((listp arg)
		       (LaTeX-down-section))
		      ((< val 0)
		       (LaTeX-up-section (- val)))
		      (t val)))
	 (name (LaTeX-section-name level))
	 (toc nil)
	 (title (if (TeX-active-mark)
		    (buffer-substring (region-beginning)
				      (region-end))
		  ""))
	 (done-mark (make-marker)))
    (run-hooks 'LaTeX-section-hook)
    (LaTeX-newline)
    (if (marker-position done-mark)
	(goto-char (marker-position done-mark)))
    (set-marker done-mark nil)))

(defun LaTeX-current-section ()
  "Return the level of the section that contain point.
See also `LaTeX-section' for description of levels."
  (save-excursion
    (max (LaTeX-largest-level)
	 (if (re-search-backward (LaTeX-outline-regexp) nil t)
	     (- (LaTeX-outline-level) (LaTeX-outline-offset))
	   (LaTeX-largest-level)))))

(defun LaTeX-down-section ()
  "Return the value of a section one level under the current.
Tries to find what kind of section that have been used earlier in the
text, if this fail, it will just return one less than the current
section."
  (save-excursion
    (let ((current (LaTeX-current-section))
	  (next nil)
	  (regexp (LaTeX-outline-regexp)))
      (if (not (re-search-backward regexp nil t))
	  (1+ current)
	(while (not next)
	  (cond
	   ((eq (LaTeX-current-section) current)
	    (if (re-search-forward regexp nil t)
		(if (<= (setq next (LaTeX-current-section)) current) ;Wow!
		    (setq next (1+ current)))
	      (setq next (1+ current))))
	   ((not (re-search-backward regexp nil t))
	    (setq next (1+ current)))))
	next))))

(defun LaTeX-up-section (arg)
  "Return the value of the section ARG levels above this one."
  (save-excursion
    (if (zerop arg)
	(LaTeX-current-section)
      (let ((current (LaTeX-current-section)))
	(while (and (>= (LaTeX-current-section) current)
		    (re-search-backward (LaTeX-outline-regexp)
					nil t)))
	(LaTeX-up-section (1- arg))))))

(defvar LaTeX-section-list '(("part" 0)
			     ("chapter" 1)
			     ("section" 2)
			     ("subsection" 3)
			     ("subsubsection" 4)
			     ("paragraph" 5)
			     ("subparagraph" 6))
  "List which elements is the names of the sections used by LaTeX.")

(defun LaTeX-section-list-add-locally (sections &optional clean)
  "Add SECTIONS to `LaTeX-section-list'.
SECTIONS can be a single list containing the section macro name
as a string and the the level as an integer or a list of such
lists.

If optional argument CLEAN is non-nil, remove any existing
entries from `LaTeX-section-list' before adding the new ones.

The function will make `LaTeX-section-list' buffer-local and
invalidate the section submenu in order to let the menu filter
regenerate it.  It is mainly a convenience function which can be
used in style files."
  (when (stringp (car sections))
    (setq sections (list sections)))
  (make-local-variable 'LaTeX-section-list)
  (when clean (setq LaTeX-section-list nil))
  (dolist (elt sections) (add-to-list 'LaTeX-section-list elt t))
  (setq LaTeX-section-list
	(sort (copy-sequence LaTeX-section-list)
	      (lambda (a b) (< (nth 1 a) (nth 1 b)))))
  (setq LaTeX-section-menu nil))

(defun LaTeX-section-name (level)
  "Return the name of the section corresponding to LEVEL."
  (let ((entry (TeX-member level LaTeX-section-list
			   (lambda (a b) (equal a (nth 1 b))))))
    (if entry
	(nth 0 entry)
      nil)))

(defun LaTeX-section-level (name)
  "Return the level of the section NAME."
  (let ((entry (TeX-member name LaTeX-section-list
			   (lambda (a b) (equal a (nth 0 b))))))

    (if entry
	(nth 1 entry)
      nil)))

(defcustom TeX-outline-extra nil
  "List of extra TeX outline levels.

Each element is a list with two entries.  The first entry is the
regular expression matching a header, and the second is the level of
the header.  See `LaTeX-section-list' for existing header levels."
  :group 'LaTeX
  :type '(repeat (group (regexp :tag "Match")
			(integer :tag "Level"))))

(defun LaTeX-outline-regexp (&optional anywhere)
  "Return regexp for LaTeX sections.

If optional argument ANYWHERE is not nil, do not require that the
header is at the start of a line."
  (concat (if anywhere "" "^")
	  "[ \t]*"
	  (regexp-quote TeX-esc)
	  "\\(appendix\\|documentstyle\\|documentclass\\|"
	  (mapconcat 'car LaTeX-section-list "\\|")
	  "\\)\\b"
	  (if TeX-outline-extra
	      "\\|"
	    "")
	  (mapconcat 'car TeX-outline-extra "\\|")
	  "\\|" TeX-header-end
	  "\\|" TeX-trailer-start))

(defvar LaTeX-largest-level nil
  "Largest sectioning level with current document class.")

(make-variable-buffer-local 'LaTeX-largest-level)

(defun LaTeX-largest-level ()
  "Return largest sectioning level with current document class.
Run style hooks before it has not been done."
  (TeX-update-style)
  LaTeX-largest-level)

(defun LaTeX-largest-level-set (section)
  "Set `LaTeX-largest-level' to the level of SECTION.
SECTION has to be a string contained in `LaTeX-section-list'.
Additionally the function will invalidate the section submenu in
order to let the menu filter regenerate it."
  (setq LaTeX-largest-level (LaTeX-section-level section))
  (let ((offset (LaTeX-outline-offset)))
    (when (and (> offset 0)
	       ;; XEmacs does not know `outline-heading-alist'.
	       (boundp 'outline-heading-alist))
      (let (lst)
	(dolist (tup outline-heading-alist)
	  (setq lst (cons (cons (car tup)
				(+ offset (cdr tup)))
			  lst)))
	(setq outline-heading-alist (nreverse lst)))))
  (setq LaTeX-section-menu nil))

(defun LaTeX-outline-offset ()
  "Offset to add to `LaTeX-section-list' levels to get outline level."
  (- 2 (LaTeX-largest-level)))

(defun TeX-look-at (list)
  "Check if we are looking at the first element of a member of LIST.
If so, return the second element, otherwise return nil."
  (while (and list
	      (not (looking-at (nth 0 (car list)))))
    (setq list (cdr list)))
  (if list
      (nth 1 (car list))
    nil))

(defun LaTeX-outline-level ()
  "Find the level of current outline heading in an LaTeX document."
  (cond ((looking-at LaTeX-header-end) 1)
	((looking-at LaTeX-trailer-start) 1)
	((TeX-look-at TeX-outline-extra)
	 (max 1 (+ (TeX-look-at TeX-outline-extra)
		   (LaTeX-outline-offset))))
	(t
	 (save-excursion
	  (skip-chars-forward " \t")
	  (forward-char 1)
	  (cond ((looking-at "appendix") 1)
		((looking-at "documentstyle") 1)
		((looking-at "documentclass") 1)
		((TeX-look-at LaTeX-section-list)
		 (max 1 (+ (TeX-look-at LaTeX-section-list)
			   (LaTeX-outline-offset))))
		(t (outline-level)))))))

(defun LaTeX-outline-name ()
  "Guess a name for the current header line."
  (save-excursion
    (if (re-search-forward "{\\([^\}]*\\)}" (+ (point) fill-column 10) t)
	(match-string 1)
      (buffer-substring (point) (min (point-max) (+ 20 (point)))))))

(add-hook 'TeX-remove-style-hook
	  (lambda () (setq LaTeX-largest-level nil)))

(defcustom LaTeX-section-hook
  '(LaTeX-section-heading
    LaTeX-section-title
;; LaTeX-section-toc		; Most people won't want this
    LaTeX-section-section
    LaTeX-section-label)
  "List of hooks to run when a new section is inserted.

The following variables are set before the hooks are run

level - numeric section level, see the documentation of `LaTeX-section'.
name - name of the sectioning command, derived from `level'.
title - The title of the section, default to an empty string.
toc - Entry for the table of contents list, default nil.
done-mark - Position of point afterwards, default nil (meaning end).

The following standard hook exist -

LaTeX-section-heading: Query the user about the name of the
sectioning command.  Modifies `level' and `name'.

LaTeX-section-title: Query the user about the title of the
section.  Modifies `title'.

LaTeX-section-toc: Query the user for the toc entry.  Modifies
`toc'.

LaTeX-section-section: Insert LaTeX section command according to
`name', `title', and `toc'.  If `toc' is nil, no toc entry is
inserted.  If `toc' or `title' are empty strings, `done-mark' will be
placed at the point they should be inserted.

LaTeX-section-label: Insert a label after the section command.
Controled by the variable `LaTeX-section-label'.

To get a full featured `LaTeX-section' command, insert

 (setq LaTeX-section-hook
       '(LaTeX-section-heading
	 LaTeX-section-title
	 LaTeX-section-toc
	 LaTeX-section-section
	 LaTeX-section-label))

in your .emacs file."
  :group 'LaTeX-macro
  :type 'hook
  :options '(LaTeX-section-heading
	     LaTeX-section-title
	     LaTeX-section-toc
	     LaTeX-section-section
	     LaTeX-section-label))


(defcustom LaTeX-section-label
  '(("part" . "part:")
    ("chapter" . "chap:")
    ("section" . "sec:")
    ("subsection" . "sec:")
    ("subsubsection" . "sec:"))
  "Default prefix when asking for a label.

Some LaTeX packages \(such as `fancyref'\) look at the prefix to generate some
text around cross-references automatically.  When using those packages, you
should not change this variable.

If it is a string, it it used unchanged for all kinds of sections.
If it is nil, no label is inserted.
If it is a list, the list is searched for a member whose car is equal
to the name of the sectioning command being inserted.  The cdr is then
used as the prefix.  If the name is not found, or if the cdr is nil,
no label is inserted."
  :group 'LaTeX-label
  :type '(choice (const :tag "none" nil)
		 (string :format "%v" :tag "Common")
		 (repeat :menu-tag "Level specific"
			 :format "\n%v%i"
			 (cons :format "%v"
			       (string :tag "Type")
			       (choice :tag "Prefix"
				       (const :tag "none" nil)
				       (string  :format "%v"))))))

;;; Section Hooks.

(defun LaTeX-section-heading ()
  "Hook to prompt for LaTeX section name.
Insert this hook into `LaTeX-section-hook' to allow the user to change
the name of the sectioning command inserted with `\\[LaTeX-section]'."
  (let ((string (completing-read
		 (concat "Level: (default " name ") ")
		 LaTeX-section-list
		 nil nil nil)))
    ; Update name
    (if (not (zerop (length string)))
	(setq name string))
    ; Update level
    (setq level (LaTeX-section-level name))))

(defun LaTeX-section-title ()
  "Hook to prompt for LaTeX section title.
Insert this hook into `LaTeX-section-hook' to allow the user to change
the title of the section inserted with `\\[LaTeX-section]."
  (setq title (read-string "Title: " title))
  (let ((region (and (TeX-active-mark)
		     (cons (region-beginning) (region-end)))))
    (when region (delete-region (car region) (cdr region)))))

(defun LaTeX-section-toc ()
  "Hook to prompt for the LaTeX section entry in the table of content .
Insert this hook into `LaTeX-section-hook' to allow the user to insert
a different entry for the section in the table of content."
  (setq toc (read-string "Toc Entry: "))
  (if (zerop (length toc))
      (setq toc nil)))

(defun LaTeX-section-section ()
  "Hook to insert LaTeX section command into the file.
Insert this hook into `LaTeX-section-hook' after those hooks that set
the `name', `title', and `toc' variables, but before those hooks that
assume that the section is already inserted."
  ;; insert a new line if the current line and the previous line are
  ;; not empty (except for whitespace), with one exception: do not
  ;; insert a new line if the previous (or current, sigh) line starts
  ;; an environment (i.e., starts with `[optional whitespace]\begin')
  (unless (save-excursion
	    (re-search-backward
	     (concat "^\\s-*\n\\s-*\\=\\|^\\s-*" (regexp-quote TeX-esc)
		     "begin")
	     (line-beginning-position 0) t))
    (LaTeX-newline))
  (insert TeX-esc name)
  (cond ((null toc))
	((zerop (length toc))
	 (insert LaTeX-optop)
	 (set-marker done-mark (point))
	 (insert LaTeX-optcl))
	(t
	 (insert LaTeX-optop toc LaTeX-optcl)))
  (insert TeX-grop)
  (if (zerop (length title))
      (set-marker done-mark (point)))
  (insert title TeX-grcl)
  (LaTeX-newline)
  ;; If RefTeX is available, tell it that we've just made a new section
  (and (fboundp 'reftex-notice-new-section)
       (reftex-notice-new-section)))

(defun LaTeX-section-label ()
  "Hook to insert a label after the sectioning command.
Insert this hook into `LaTeX-section-hook' to prompt for a label to be
inserted after the sectioning command.

The behaviour of this hook is controlled by variable `LaTeX-section-label'."
  (and (LaTeX-label name 'section)
       (LaTeX-newline)))

;;; Environments

(defgroup LaTeX-environment nil
  "Environments in AUCTeX."
  :group 'LaTeX-macro)

(defcustom LaTeX-default-environment "itemize"
  "*The default environment when creating new ones with `LaTeX-environment'.
It is overridden by `LaTeX-default-document-environment' when it
is non-nil and the current environment is \"document\"."
  :group 'LaTeX-environment
  :type 'string)
(make-variable-buffer-local 'LaTeX-default-environment)

(defvar LaTeX-default-document-environment nil
  "The default environment when creating new ones with
`LaTeX-environment' and the current one is \"document\".  This
variable overrides `LaTeX-default-environment'.")
(make-variable-buffer-local 'LaTeX-default-document-environment)

(defvar LaTeX-default-tabular-environment "tabular"
  "The default tabular-like environment used when inserting a table env.
Styles such as tabularx may set it according to their needs.")
(make-variable-buffer-local 'LaTeX-default-tabular-environment)

(defvar LaTeX-environment-history nil)

;; Variable used to cache the current environment, e.g. for repeated
;; tasks in an environment, like indenting each line in a paragraph to
;; be filled.  It must not have a non-nil value in general.  That
;; means it is usually let-bound for such operations.
(defvar LaTeX-current-environment nil)

(defun LaTeX-environment (arg)
  "Make LaTeX environment (\\begin{...}-\\end{...} pair).
With optional ARG, modify current environment.

It may be customized with the following variables:

`LaTeX-default-environment'       Your favorite environment.
`LaTeX-default-style'             Your favorite document class.
`LaTeX-default-options'           Your favorite document class options.
`LaTeX-float'                     Where you want figures and tables to float.
`LaTeX-table-label'               Your prefix to labels in tables.
`LaTeX-figure-label'              Your prefix to labels in figures.
`LaTeX-default-format'            Format for array and tabular.
`LaTeX-default-width'             Width for minipage and tabular*.
`LaTeX-default-position'          Position for array and tabular."

  (interactive "*P")
  (let* ((default (cond
		   ((TeX-near-bobp) "document")
		   ((and LaTeX-default-document-environment
			 (string-equal (LaTeX-current-environment) "document"))
		    LaTeX-default-document-environment)
		   (t LaTeX-default-environment)))
	 (environment (completing-read (concat "Environment type: (default "
					       default ") ")
				       (LaTeX-environment-list-filtered) nil nil
				       nil 'LaTeX-environment-history default)))
    ;; Use `environment' as default for the next time only if it is different
    ;; from the current default.
    (unless (equal environment default)
      (setq LaTeX-default-environment environment))

    (let ((entry (assoc environment (LaTeX-environment-list))))
      (if (null entry)
	  (LaTeX-add-environments (list environment)))

      (if arg
	  (LaTeX-modify-environment environment)
	(LaTeX-environment-menu environment)))))

(defun LaTeX-environment-menu (environment)
  "Insert ENVIRONMENT around point or region."
  (let ((entry (assoc environment (LaTeX-environment-list))))
    (cond ((not (and entry (nth 1 entry)))
	   (LaTeX-insert-environment environment))
	  ((numberp (nth 1 entry))
	   (let ((count (nth 1 entry))
		 (args ""))
	     (while (> count 0)
	       (setq args (concat args TeX-grop TeX-grcl))
	       (setq count (- count 1)))
	     (LaTeX-insert-environment environment args)))
	  ((or (stringp (nth 1 entry)) (vectorp (nth 1 entry)))
	   (let ((prompts (cdr entry))
		 (args ""))
	     (dolist (elt prompts)
	       (let* ((optional (vectorp elt))
		      (elt (if optional (elt elt 0) elt))
		      (arg (read-string (concat (when optional "(Optional) ")
						elt ": "))))
		 (setq args (concat args
				    (cond ((and optional (> (length arg) 0))
					   (concat LaTeX-optop arg LaTeX-optcl))
					  ((not optional)
					   (concat TeX-grop arg TeX-grcl)))))))
	     (LaTeX-insert-environment environment args)))
	  (t
	   (apply (nth 1 entry) environment (nthcdr 2 entry))))))

(defun LaTeX-close-environment (&optional reopen)
  "Create an \\end{...} to match the current environment.
With prefix-argument, reopen environment afterwards."
  (interactive "*P")
  (if (> (point)
	 (save-excursion
	   (beginning-of-line)
	   (when LaTeX-insert-into-comments
	     (if (looking-at comment-start-skip)
		 (goto-char (match-end 0))))
	   (skip-chars-forward " \t")
	   (point)))
      (LaTeX-newline))
  (let ((environment (LaTeX-current-environment 1)) marker)
    (insert "\\end{" environment "}")
    (indent-according-to-mode)
    (if (or (not (looking-at "[ \t]*$"))
	    (and (TeX-in-commented-line)
		 (save-excursion (beginning-of-line 2)
				 (not (TeX-in-commented-line)))))
	(LaTeX-newline)
      (unless (= (forward-line 1) 0)
      	(insert "\n")))
    (indent-according-to-mode)
    (when reopen
      (save-excursion
	(setq marker (point-marker))
	(set-marker-insertion-type marker t)
	(LaTeX-environment-menu environment)
	(delete-region (point)
		       (if (save-excursion (goto-char marker)
					   (bolp))
			   (1- marker)
			 marker))
	(move-marker marker nil)))))

(defvar LaTeX-after-insert-env-hooks nil
  "List of functions to be run at the end of `LaTeX-insert-environment'.
Each function is called with three arguments: the name of the
environment just inserted, the buffer position just before
\\begin and the position just before \\end.")

(defun LaTeX-insert-environment (environment &optional extra)
  "Insert LaTeX ENVIRONMENT with optional argument EXTRA."
  (let ((active-mark (and (TeX-active-mark) (not (eq (mark) (point)))))
	prefix content-start env-start env-end)
    (when (and active-mark (< (mark) (point))) (exchange-point-and-mark))
    ;; Compute the prefix.
    (when (and LaTeX-insert-into-comments (TeX-in-commented-line))
      (save-excursion
	(beginning-of-line)
	(looking-at
	 (concat "^\\([ \t]*" TeX-comment-start-regexp "+\\)+[ \t]*"))
	(setq prefix (match-string 0))))
    ;; What to do with the line containing point.
    (cond ((save-excursion (beginning-of-line)
			   (looking-at (concat prefix "[ \t]*$")))
	   (delete-region (match-beginning 0) (match-end 0)))
	  ((TeX-looking-at-backward (concat "^" prefix "[ \t]*")
				    (line-beginning-position))
	   (beginning-of-line)
	   (newline)
	   (beginning-of-line 0))
	  ((bolp)
	   (delete-horizontal-space)
	   (newline)
	   (beginning-of-line 0))
	  (t
	   (delete-horizontal-space)
	   (newline 2)
	   (when prefix (insert prefix))
	   (beginning-of-line 0)))
    ;; What to do with the line containing mark.
    (when active-mark
      (save-excursion
	(goto-char (mark))
	(cond ((save-excursion (beginning-of-line)
			       (or (looking-at (concat prefix "[ \t]*$"))
				   (looking-at "[ \t]*$")))
	       (delete-region (match-beginning 0) (match-end 0)))
	      ((TeX-looking-at-backward (concat "^" prefix "[ \t]*")
					(line-beginning-position))
	       (beginning-of-line)
	       (newline)
	       (beginning-of-line 0))
	      (t
	       (delete-horizontal-space)
	       (insert-before-markers "\n")
	       (newline)
	       (when prefix (insert prefix))))))
    ;; Now insert the environment.
    (when prefix (insert prefix))
    (setq env-start (point))
    (insert TeX-esc "begin" TeX-grop environment TeX-grcl)
    (indent-according-to-mode)
    (when extra (insert extra))
    (setq content-start (line-beginning-position 2))
    (unless active-mark
      (newline)
      (when prefix (insert prefix))
      (newline))
    (when active-mark (goto-char (mark)))
    (when prefix (insert prefix))
    (insert TeX-esc "end" TeX-grop environment TeX-grcl)
    (end-of-line 0)
    (if active-mark
	(progn
	  (or (assoc environment LaTeX-indent-environment-list)
	      (LaTeX-fill-region content-start (line-beginning-position 2)))
	  (set-mark content-start))
      (indent-according-to-mode))
    (save-excursion (beginning-of-line 2) (indent-according-to-mode))
    (TeX-math-input-method-off)
    (setq env-end (save-excursion
		    (search-forward
		     (concat TeX-esc "end" TeX-grop
			     environment TeX-grcl))
		    (match-beginning 0)))
    (run-hook-with-args 'LaTeX-after-insert-env-hooks
			environment env-start env-end)))

(defun LaTeX-modify-environment (environment)
  "Modify current ENVIRONMENT."
  (save-excursion
    (LaTeX-find-matching-end)
    (re-search-backward (concat (regexp-quote TeX-esc)
				"end"
				(regexp-quote TeX-grop)
				" *\\([a-zA-Z*]*\\)"
				(regexp-quote TeX-grcl))
			(save-excursion (beginning-of-line 1) (point)))
    (replace-match (concat TeX-esc "end" TeX-grop environment TeX-grcl) t t)
    (beginning-of-line 1)
    (LaTeX-find-matching-begin)
    (re-search-forward (concat (regexp-quote TeX-esc)
			       "begin"
			       (regexp-quote TeX-grop)
			       " *\\([a-zA-Z*]*\\)"
			       (regexp-quote TeX-grcl))
		       (save-excursion (end-of-line 1) (point)))
    (replace-match (concat TeX-esc "begin" TeX-grop environment TeX-grcl) t t)))

(defun LaTeX-current-environment (&optional arg)
  "Return the name (a string) of the enclosing LaTeX environment.
With optional ARG>=1, find that outer level.

If function is called inside a comment and
`LaTeX-syntactic-comments' is enabled, try to find the
environment in commented regions with the same comment prefix.

The functions `LaTeX-find-matching-begin' and `LaTeX-find-matching-end'
work analogously."
  (setq arg (if arg (if (< arg 1) 1 arg) 1))
  (let* ((in-comment (TeX-in-commented-line))
	 (comment-prefix (and in-comment (TeX-comment-prefix)))
	 (case-fold-search nil))
    (save-excursion
      (while (and (/= arg 0)
		  (re-search-backward
		   "\\\\\\(begin\\|end\\) *{ *\\([A-Za-z*]+\\) *}" nil t))
	(when (or (and LaTeX-syntactic-comments
		       (eq in-comment (TeX-in-commented-line))
		       (or (not in-comment)
			   ;; Consider only matching prefixes in the
			   ;; commented case.
			   (string= comment-prefix (TeX-comment-prefix))))
		  (and (not LaTeX-syntactic-comments)
		       (not (TeX-in-commented-line))))
	  (setq arg (if (string= (match-string 1) "end") (1+ arg) (1- arg)))))
      (if (/= arg 0)
	  "document"
	(match-string-no-properties 2)))))

(defun docTeX-in-macrocode-p ()
  "Determine if point is inside a macrocode environment."
  (let ((case-fold-search nil))
    (save-excursion
      (re-search-backward
       (concat "^%    " (regexp-quote TeX-esc)
	       "\\(begin\\|end\\)[ \t]*{macrocode\\*?}") nil 'move)
      (not (or (bobp)
	       (= (char-after (match-beginning 1)) ?e))))))


;;; Environment Hooks

(defvar LaTeX-document-style-hook nil
  "List of hooks to run when inserting a document environment.

To insert a hook here, you must insert it in the appropiate style file.")

(defun LaTeX-env-document (&optional ignore)
  "Create new LaTeX document.
Also inserts a \\documentclass macro if there's none already and
prompts for the insertion of \\usepackage macros.

The compatibility argument IGNORE is ignored."
  ;; just assume a single valid \\documentclass, i.e., one not in a
  ;; commented line
  (let ((found nil))
    (save-excursion
      (while (and (not found)
		  (re-search-backward
		   "\\\\documentclass\\(\\[[a-z0-9A-Z\-\_,]*\\]\\)?\\({[^}]+}\\)"
		   nil t))
	(and (not (TeX-in-commented-line))
	     (setq found t))))
    (when (not found)
      (TeX-insert-macro "documentclass")
      (LaTeX-newline)
      (LaTeX-newline)
      ;; Add a newline only if some `\usepackage' has been inserted.
      (if (LaTeX-insert-usepackages)
	  (LaTeX-newline))
      (LaTeX-newline)
      (end-of-line 0)))
  (LaTeX-insert-environment "document")
  (run-hooks 'LaTeX-document-style-hook)
  (setq LaTeX-document-style-hook nil))

(defcustom LaTeX-float ""
  "Default float position for figures and tables.
If nil, act like the empty string is given, but do not prompt.
\(The standard LaTeX classes use [tbp] as float position if the
optional argument is omitted.)"
  :group 'LaTeX-environment
  :type '(choice (const :tag "Do not prompt" nil)
		 (const :tag "Empty" "")
		 (string :format "%v")))
(make-variable-buffer-local 'LaTeX-float)

(defcustom LaTeX-top-caption-list nil
  "*List of float environments with top caption."
  :group 'LaTeX-environment
  :type '(repeat (string :format "%v")))

(defgroup LaTeX-label nil
  "Adding labels for LaTeX commands in AUCTeX."
  :group 'LaTeX)

(defcustom LaTeX-label-function nil
  "*A function inserting a label at point.
Sole argument of the function is the environment.  The function has to return
the label inserted, or nil if no label was inserted."
  :group 'LaTeX-label
  :type 'function)

(defcustom LaTeX-figure-label "fig:"
  "*Default prefix to figure labels."
  :group 'LaTeX-label
  :group 'LaTeX-environment
  :type 'string)

(defcustom LaTeX-table-label "tab:"
  "*Default prefix to table labels."
  :group 'LaTeX-label
  :group 'LaTeX-environment
  :type 'string)

(defcustom LaTeX-default-format ""
  "Default format for array and tabular environments."
  :group 'LaTeX-environment
  :type 'string)
(make-variable-buffer-local 'LaTeX-default-format)

(defcustom LaTeX-default-width "1.0\\linewidth"
  "Default width for minipage and tabular* environments."
  :group 'LaTeX-environment
  :type 'string)
(make-variable-buffer-local 'LaTeX-default-width)

(defcustom LaTeX-default-position ""
  "Default position for array and tabular environments.
If nil, act like the empty string is given, but do not prompt."
  :group 'LaTeX-environment
  :type '(choice (const :tag "Do not prompt" nil)
		 (const :tag "Empty" "")
		 string))
(make-variable-buffer-local 'LaTeX-default-position)

(defcustom LaTeX-equation-label "eq:"
  "*Default prefix to equation labels."
  :group 'LaTeX-label
  :type 'string)

(defcustom LaTeX-eqnarray-label LaTeX-equation-label
  "*Default prefix to eqnarray labels."
  :group 'LaTeX-label
  :type 'string)

(defun LaTeX-env-item (environment)
  "Insert ENVIRONMENT and the first item."
  (LaTeX-insert-environment environment)
  (if (TeX-active-mark)
      (progn
	(LaTeX-find-matching-begin)
	(end-of-line 1))
    (end-of-line 0))
  (delete-char 1)
  (when (looking-at (concat "^[ \t]+$\\|"
			    "^[ \t]*" TeX-comment-start-regexp "+[ \t]*$"))
    (delete-region (point) (line-end-position)))
  (delete-horizontal-space)
  ;; Deactivate the mark here in order to prevent `TeX-parse-macro'
  ;; from swapping point and mark and the \item ending up right after
  ;; \begin{...}.
  (TeX-deactivate-mark)
  (LaTeX-insert-item)
  ;; The inserted \item may have outdented the first line to the
  ;; right.  Fill it, if appropriate.
  (when (and (not (looking-at "$"))
	     (not (assoc environment LaTeX-indent-environment-list))
	     (> (- (line-end-position) (line-beginning-position))
		(current-fill-column)))
    (LaTeX-fill-paragraph nil)))

(defcustom LaTeX-label-alist
  '(("figure" . LaTeX-figure-label)
    ("table" . LaTeX-table-label)
    ("figure*" . LaTeX-figure-label)
    ("table*" . LaTeX-table-label)
    ("equation" . LaTeX-equation-label)
    ("eqnarray" . LaTeX-eqnarray-label))
  "Lookup prefixes for labels.
An alist where the CAR is the environment name, and the CDR
either the prefix or a symbol referring to one."
  :group 'LaTeX-label
  :type '(repeat (cons (string :tag "Environment")
		       (choice (string :tag "Label prefix")
			       (symbol :tag "Label prefix symbol")))))

(make-variable-buffer-local 'LaTeX-label-alist)

(defun LaTeX-label (name type)
  "Insert a label for NAME at point.
TYPE can be either environment or section.  If
`LaTeX-label-function' is a valid function, LaTeX label will
transfer the job to this function."
  (let ((prefix (cond
		 ((eq type 'environment)
		  (cdr (assoc name LaTeX-label-alist)))
		 ((eq type 'section)
		  (if (assoc name LaTeX-section-list)
		      (if (stringp LaTeX-section-label)
			  LaTeX-section-label
			(and (listp LaTeX-section-label)
			     (cdr (assoc name LaTeX-section-label))))
		    ""))))
	label)
    (when (symbolp prefix)
      (setq prefix (symbol-value prefix)))
    (when prefix
      (if (and (boundp 'LaTeX-label-function)
	       LaTeX-label-function
	       (fboundp LaTeX-label-function))
	  (setq label (funcall LaTeX-label-function name))
	;; Use completing-read as we do with `C-c C-m \label RET'
	(setq label (completing-read
		     (TeX-argument-prompt t nil "What label")
		     (LaTeX-label-list) nil nil prefix))
	;; No label or empty string entered?
	(if (or (string= prefix label)
		(string= "" label))
	    (setq label nil)
	  (insert TeX-esc "label" TeX-grop label TeX-grcl))
	(if label
	    (progn
	      (LaTeX-add-labels label)
	      label)
	  nil)))))

(defun LaTeX-env-figure (environment)
  "Create ENVIRONMENT with \\caption and \\label commands."
  (let ((float (and LaTeX-float		; LaTeX-float can be nil, i.e.
					; do not prompt
		    (read-string "(Optional) Float position: " LaTeX-float)))
	(caption (read-string "Caption: "))
	(center (y-or-n-p "Center? "))
	(active-mark (and (TeX-active-mark)
			  (not (eq (mark) (point)))))
	start-marker end-marker)
    (when active-mark
      (if (< (mark) (point))
	  (exchange-point-and-mark))
      (setq start-marker (point-marker))
      (set-marker-insertion-type start-marker t)
      (setq end-marker (copy-marker (mark))))
    (setq LaTeX-float float)
    (LaTeX-insert-environment environment
			      (unless (zerop (length float))
				(concat LaTeX-optop float
					LaTeX-optcl)))
    (when active-mark (goto-char start-marker))
    (when center
      (insert TeX-esc "centering")
      (indent-according-to-mode)
      (LaTeX-newline))
    ;; Insert caption and ask for a label, do nothing if user skips caption
    (unless (zerop (length caption))
      (if (member environment LaTeX-top-caption-list)
	  ;; top caption
	  (progn
	    (insert TeX-esc "caption" TeX-grop caption TeX-grcl)
	    ;; If `auto-fill-mode' is active, fill the caption.
	    (if auto-fill-function (LaTeX-fill-paragraph))
	    (LaTeX-newline)
	    (indent-according-to-mode)
	    ;; ask for a label and insert a new line only if a label is
	    ;; actually inserted
	    (when (LaTeX-label environment 'environment)
	      (LaTeX-newline)
	      (indent-according-to-mode)))
	;; bottom caption (default)
	(when active-mark (goto-char end-marker))
	(save-excursion
	  (LaTeX-newline)
	  (indent-according-to-mode)
	  ;; If there is an active region point is before the backslash of
	  ;; "\end" macro, go one line upwards.
	  (when active-mark (forward-line -1) (indent-according-to-mode))
	  (insert TeX-esc "caption" TeX-grop caption TeX-grcl)
	  ;; If `auto-fill-mode' is active, fill the caption.
	  (if auto-fill-function (LaTeX-fill-paragraph))
	  ;; ask for a label and if necessary insert a new line between caption
	  ;; and label
	  (when (save-excursion (LaTeX-label environment 'environment))
	    (LaTeX-newline)
	    (indent-according-to-mode)))
	;; Insert an empty line between caption and marked region, if any.
	(when active-mark (LaTeX-newline) (forward-line -1))
	(indent-according-to-mode)))
    (when (and (member environment '("table" "table*"))
	       ;; Suppose an existing tabular environment should just
	       ;; be wrapped into a table if there is an active region.
	       (not active-mark))
      (LaTeX-environment-menu LaTeX-default-tabular-environment))))

(defun LaTeX-env-array (environment)
  "Insert ENVIRONMENT with position and column specifications.
Just like array and tabular."
  (let ((pos (and LaTeX-default-position ; LaTeX-default-position can
					; be nil, i.e. do not prompt
		  (read-string "(Optional) Position: " LaTeX-default-position)))
	(fmt (read-string "Format: " LaTeX-default-format)))
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			       (unless (zerop (length pos))
				 (concat LaTeX-optop pos LaTeX-optcl))
			       (concat TeX-grop fmt TeX-grcl)))
    (LaTeX-item-array t)))

(defun LaTeX-env-label (environment)
  "Insert ENVIRONMENT and prompt for label."
  (LaTeX-insert-environment environment)
  (when (LaTeX-label environment 'environment)
    (LaTeX-newline)
    (indent-according-to-mode)))

(defun LaTeX-env-list (environment)
  "Insert ENVIRONMENT and the first item."
  (let ((label (read-string "Default Label: ")))
    (LaTeX-insert-environment environment
			      (format "{%s}{}" label))
    (end-of-line 0)
    (delete-char 1)
    (delete-horizontal-space))
  (LaTeX-insert-item))

(defun LaTeX-env-minipage (environment)
  "Create new LaTeX minipage or minipage-like ENVIRONMENT."
  (let ((pos (and LaTeX-default-position ; LaTeX-default-position can
					; be nil, i.e. do not prompt
		  (read-string "(Optional) Position: " LaTeX-default-position)))
	(width (read-string "Width: " LaTeX-default-width)))
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-width width)
    (LaTeX-insert-environment environment
			      (concat
			       (unless (zerop (length pos))
				 (concat LaTeX-optop pos LaTeX-optcl))
			       (concat TeX-grop width TeX-grcl)))))

(defun LaTeX-env-tabular* (environment)
  "Insert ENVIRONMENT with width, position and column specifications."
  (let ((width (read-string "Width: " LaTeX-default-width))
	(pos (and LaTeX-default-position ; LaTeX-default-position can
					; be nil, i.e. do not prompt
		  (read-string "(Optional) Position: " LaTeX-default-position)))
	(fmt (read-string "Format: " LaTeX-default-format)))
    (setq LaTeX-default-width width)
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			       (concat TeX-grop width TeX-grcl) ;; not optional!
			       (unless (zerop (length pos))
				 (concat LaTeX-optop pos LaTeX-optcl))
			       (concat TeX-grop fmt TeX-grcl)))
    (LaTeX-item-tabular* t)))

(defun LaTeX-env-picture (environment)
  "Insert ENVIRONMENT with width, height specifications."
  (let ((width (read-string "Width: "))
	(height (read-string "Height: "))
	(x-offset (read-string "X Offset: "))
	(y-offset (read-string "Y Offset: ")))
    (if (zerop (length x-offset))
	(setq x-offset "0"))
    (if (zerop (length y-offset))
	(setq y-offset "0"))
    (LaTeX-insert-environment environment
			      (concat
			       (format "(%s,%s)" width height)
			       (if (not (and (string= x-offset "0")
					     (string= y-offset "0")))
				   (format "(%s,%s)" x-offset y-offset))))))

(defun LaTeX-env-bib (environment)
  "Insert ENVIRONMENT with label for bibitem."
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "Label for BibItem: " "99")
				    TeX-grcl))
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(defun LaTeX-env-contents (environment)
  "Insert ENVIRONMENT with filename for contents."
  (save-excursion
    (when (re-search-backward LaTeX-header-end nil t)
      (error "Put %s environment before \\begin{document}" environment)))
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "File: ")
				    TeX-grcl))
  (delete-horizontal-space))

(defun LaTeX-env-args (environment &rest args)
  "Insert ENVIRONMENT and arguments defined by ARGS."
  (LaTeX-insert-environment environment)
  (save-excursion
    (LaTeX-find-matching-begin)
    (end-of-line)
    (TeX-parse-arguments args)))

;;; Item hooks

(defvar LaTeX-item-list nil
  "A list of environments where items have a special syntax.
The cdr is the name of the function, used to insert this kind of items.")

(defun LaTeX-insert-item ()
  "Insert a new item in an environment.
You may use `LaTeX-item-list' to change the routines used to insert the item."
  (interactive "*")
  (let ((environment (LaTeX-current-environment)))
    (when (and (TeX-active-mark)
	       (> (point) (mark)))
      (exchange-point-and-mark))
    (unless (bolp) (LaTeX-newline))
    (if (assoc environment LaTeX-item-list)
	(funcall (cdr (assoc environment LaTeX-item-list)))
      (TeX-insert-macro "item"))
    (indent-according-to-mode)))

(defun LaTeX-item-argument ()
  "Insert a new item with an optional argument."
  (let ((TeX-arg-item-label-p t))
    (TeX-insert-macro "item")))

(defun LaTeX-item-bib ()
  "Insert a new bibitem."
  (TeX-insert-macro "bibitem"))

(defvar LaTeX-array-skipping-regexp (regexp-opt '("[t]" "[b]" ""))
   "Regexp matching between \\begin{xxx} and column specification.
For array and tabular environments.  See `LaTeX-insert-ampersands' for
detail.")

(defvar LaTeX-tabular*-skipping-regexp
  ;; Assume width specification contains neither nested curly brace
  ;; pair nor escaped "}".
  (concat "{[^}]*}[ \t]*" (regexp-opt '("[t]" "[b]" "")))
   "Regexp matching between \\begin{tabular*} and column specification.
For tabular* environment only.  See `LaTeX-insert-ampersands' for detail.")

(defun LaTeX-item-array (&optional suppress)
  "Insert line break macro on the last line and suitable number of &'s.
For array and tabular environments.

If SUPPRESS is non-nil, do not insert line break macro."
  (unless suppress
    (save-excursion
      (end-of-line 0)
      (just-one-space)
      (TeX-insert-macro "\\")))
  (LaTeX-insert-ampersands
   LaTeX-array-skipping-regexp 'LaTeX-array-count-columns))

(defun LaTeX-item-tabular* (&optional suppress)
  "Insert line break macro on the last line and suitable number of &'s.
For tabular* environment only.

If SUPPRESS is non-nil, do not insert line break macro."
  (unless suppress
    (save-excursion
      (end-of-line 0)
      (just-one-space)
      (TeX-insert-macro "\\")))
  (LaTeX-insert-ampersands
   LaTeX-tabular*-skipping-regexp 'LaTeX-array-count-columns))

(defun LaTeX-insert-ampersands (regexp func)
  "Insert suitable number of ampersands for the current environment.
The number is calculated from REGEXP and FUNC.

Example 1:
Consider the case that the current environment begins with
\\begin{array}[t]{|lcr|}
.  REGEXP must be chosen to match \"[t]\", i.e., the text between just
after \"\\begin{array}\" and just before \"{|lcr|}\", which encloses
the column specification.  FUNC must return the number of ampersands to
be inserted, which is 2 since this example specifies three columns.
FUNC is called with two arguments START and END, which spans the column
specification (without enclosing braces.)  REGEXP is used to determine
these START and END.

Example 2:
This time the environment begins with
\\begin{tabular*}{1.0\\linewidth}[b]{c@{,}p{5ex}}
.  REGEXP must match \"{1.0\\linewidth}[b]\" and FUNC must return 1 from
the text \"c@{,}p{5ex}\" between START and END specified two columns.

FUNC should return nil if it cannot determine the number of ampersands."
  (let* ((cur (point))
	 (num
	  (save-excursion
	    (ignore-errors
	      (LaTeX-find-matching-begin)
	      ;; Skip over "\begin{xxx}" and possible whitespaces.
	      (forward-list 1)
	      (skip-chars-forward " \t")
	      ;; Skip over the text specified by REGEXP and whitespaces.
	      (when (let ((case-fold-search nil))
		      (re-search-forward regexp cur))
		(skip-chars-forward " \t")
		(when (eq (following-char) ?{)
		  ;; We have reached the target "{yyy}" part.
		  (forward-char 1)
		  ;; The next line doesn't move point, so point
		  ;; is left just after the opening brace.
		  (let ((pos (TeX-find-closing-brace)))
		    (if pos
			;; Calculate number of ampersands to be inserted.
			(funcall func (point) (1- pos))))))))))
    (if (natnump num)
	(save-excursion (insert (make-string num ?&))))))

(defvar LaTeX-array-column-letters "clrp"
  "Column letters for array-like environments.
See `LaTeX-array-count-columns' for detail.")

(defun LaTeX-array-count-columns (start end)
  "Count number of ampersands to be inserted.
The columns are specified by the letters found in the string
`LaTeX-array-column-letters' and the number of those letters within the
text between START and END is basically considered to be the number of
columns.  The arguments surrounded between braces such as p{30pt} do not
interfere the count of columns.

Return one less number than the columns, or nil on failing to count the
right number."
  (save-excursion
    (let (p (cols 0))
      (goto-char start)
      (while (< (setq p (point)) end)

	;; The below block accounts for one unit of move for
	;; one column.
	(setq cols (+ cols (skip-chars-forward
			    LaTeX-array-column-letters end)))
	(skip-chars-forward (concat
			     "^" LaTeX-array-column-letters
			     TeX-grop) end)
	(if (eq (following-char) ?{) (forward-list 1))

	;; Not sure whether this is really necessary or not, but
	;; prepare for possible infinite loop anyway.
	(when (eq p (point))
	  (setq cols nil)
	  (goto-char end)))
      ;; The number of ampersands is one less than column.
      (if cols (1- cols)))))

;;; Parser

(defvar LaTeX-auto-style nil)
(defvar LaTeX-auto-arguments nil)
(defvar LaTeX-auto-optional nil)
(defvar LaTeX-auto-env-args nil)

(TeX-auto-add-type "label" "LaTeX")
(TeX-auto-add-type "bibitem" "LaTeX")
(TeX-auto-add-type "environment" "LaTeX")
(TeX-auto-add-type "bibliography" "LaTeX" "bibliographies")
(TeX-auto-add-type "index-entry" "LaTeX" "index-entries")
(TeX-auto-add-type "pagestyle" "LaTeX")
(TeX-auto-add-type "counter" "LaTeX")
(TeX-auto-add-type "length" "LaTeX")
(TeX-auto-add-type "savebox" "LaTeX" "saveboxes")

(defvar LaTeX-auto-minimal-regexp-list
  '(("\\\\document\\(style\\|class\\)\
\\(\\[\\(\\([^#\\%]\\|%[^\n\r]*[\n\r]\\)*\\)\\]\\)?\
{\\([^#\\.\n\r]+?\\)}"
     (3 5 1) LaTeX-auto-style)
    ("\\\\use\\(package\\)\\(\\[\\([^\]\\]*\\)\\]\\)?\
{\\(\\([^#}\\.%]\\|%[^\n\r]*[\n\r]\\)+?\\)}"
     (3 4 1) LaTeX-auto-style))
  "Minimal list of regular expressions matching LaTeX macro definitions.")

(defvar LaTeX-auto-label-regexp-list
  '(("\\\\label{\\([^\n\r%\\{}]+\\)}" 1 LaTeX-auto-label))
  "List of regular expression matching LaTeX labels only.")

(defvar LaTeX-auto-index-regexp-list
   '(("\\\\\\(index\\|glossary\\){\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}"
	2 LaTeX-auto-index-entry))
   "List of regular expression matching LaTeX index/glossary entries only.
Regexp allows for up to 3 levels of parenthesis inside the index argument.
This is necessary since index entries may contain commands and stuff.")

(defvar LaTeX-auto-class-regexp-list
  '(;; \RequirePackage[<options>]{<package>}[<date>]
    ("\\\\Require\\(Package\\)\\(\\[\\([^#\\.%]*?\\)\\]\\)?\
{\\([^#\\.\n\r]+?\\)}"
     (3 4 1) LaTeX-auto-style)
    ;; \RequirePackageWithOptions{<package>}[<date>],
    ("\\\\Require\\(Package\\)WithOptions\\(\\){\\([^#\\.\n\r]+?\\)}"
     (2 3 1) LaTeX-auto-style)
    ;; \LoadClass[<options>]{<package>}[<date>]
    ("\\\\Load\\(Class\\)\\(\\[\\([^#\\.%]*?\\)\\]\\)?{\\([^#\\.\n\r]+?\\)}"
     (3 4 1) LaTeX-auto-style)
    ;; \LoadClassWithOptions{<package>}[<date>]
    ("\\\\Load\\(Class\\)WithOptions\\(\\){\\([^#\\.\n\r]+?\\)}"
     (2 3 1) LaTeX-auto-style)
    ;; \DeclareRobustCommand{<cmd>}[<num>][<default>]{<definition>},
    ;; \DeclareRobustCommand*{<cmd>}[<num>][<default>]{<definition>}
    ("\\\\DeclareRobustCommand\\*?{?\\\\\\([A-Za-z]+\\)}?\
\\[\\([0-9]+\\)\\]\\[\\([^\n\r]*?\\)\\]"
     (1 2 3) LaTeX-auto-optional)
    ("\\\\DeclareRobustCommand\\*?{?\\\\\\([A-Za-z]+\\)}?\\[\\([0-9]+\\)\\]"
     (1 2) LaTeX-auto-arguments)
    ("\\\\DeclareRobustCommand\\*?{?\\\\\\([A-Za-z]+\\)}?"
     1 TeX-auto-symbol)
    ;; Patterns for commands described in "LaTeX2e font selection" (fntguide)
    ("\\\\DeclareMath\\(?:Symbol\\|Delimiter\\|Accent\\|Radical\\)\
{?\\\\\\([A-Za-z]+\\)}?"
     1 TeX-auto-symbol)
    ("\\\\\\(Declare\\|Provide\\)Text\
\\(?:Command\\|Symbol\\|Accent\\|Composite\\){?\\\\\\([A-Za-z]+\\)}?"
     1 TeX-auto-symbol)
    ("\\\\Declare\\(?:Text\\|Old\\)FontCommand{?\\\\\\([A-Za-z]+\\)}?"
     1 TeX-auto-symbol))
  "List of regular expressions matching macros in LaTeX classes and packages.")

(defvar LaTeX-auto-pagestyle-regexp-list
  '(("\\\\ps@\\([A-Za-z]+\\)" 1 LaTeX-auto-pagestyle))
  "List of regular expressions matching LaTeX pagestyles only.")

(defvar LaTeX-auto-counter-regexp-list
  '(("\\\\newcounter *{\\([A-Za-z]+\\)}" 1 LaTeX-auto-counter)
    ("\\\\@definecounter{\\([A-Za-z]+\\)}" 1 LaTeX-auto-counter))
  "List of regular expressions matching LaTeX counters only.")

(defvar LaTeX-auto-length-regexp-list
  '(("\\\\newlength *{?\\\\\\([A-Za-z]+\\)}?" 1 LaTeX-auto-length))
  "List of regular expressions matching LaTeX lengths only.")

(defvar LaTeX-auto-savebox-regexp-list
  '(("\\\\newsavebox *{?\\\\\\([A-Za-z]+\\)}?" 1 LaTeX-auto-savebox))
  "List of regular expressions matching LaTeX saveboxes only.")

(defvar LaTeX-auto-regexp-list
  (append
   (let ((token TeX-token-char))
     `((,(concat "\\\\\\(?:new\\|provide\\)command\\*?{?\\\\\\(" token "+\\)}?\\[\\([0-9]+\\)\\]\\[\\([^\n\r]*\\)\\]")
	(1 2 3) LaTeX-auto-optional)
       (,(concat "\\\\\\(?:new\\|provide\\)command\\*?{?\\\\\\(" token "+\\)}?\\[\\([0-9]+\\)\\]")
	(1 2) LaTeX-auto-arguments)
       (,(concat "\\\\\\(?:new\\|provide\\)command\\*?{?\\\\\\(" token "+\\)}?")
	1 TeX-auto-symbol)
       (,(concat "\\\\newenvironment\\*?{?\\(" token "+\\)}?\\[\\([0-9]+\\)\\]\\[")
	1 LaTeX-auto-environment)
       (,(concat "\\\\newenvironment\\*?{?\\(" token "+\\)}?\\[\\([0-9]+\\)\\]")
	(1 2) LaTeX-auto-env-args)
       (,(concat "\\\\newenvironment\\*?{?\\(" token "+\\)}?")
	1 LaTeX-auto-environment)
       (,(concat "\\\\newtheorem{\\(" token "+\\)}") 1 LaTeX-auto-environment)
       ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
	1 TeX-auto-file)
       ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
	1 TeX-auto-file)
       (, (concat "\\\\bibitem{\\(" token "[^, \n\r\t%\"#'()={}]*\\)}")
	  1 LaTeX-auto-bibitem)
       (, (concat "\\\\bibitem\\[[^][\n\r]+\\]{\\(" token "[^, \n\r\t%\"#'()={}]*\\)}")
	  1 LaTeX-auto-bibitem)
       ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography)
       ("\\\\addbibresource\\(?:\\[[^]]+\\]\\)?{\\([^#}\\\\\n\r\.]+\\)\\..+}"
	1 LaTeX-auto-bibliography)
       ("\\\\add\\(?:global\\|section\\)bib\\(?:\\[[^]]+\\]\\)?{\\([^#}\\\\\n\r\.]+\\)\\(?:\\..+\\)?}" 1 LaTeX-auto-bibliography)
       ("\\\\newrefsection\\[\\([^]]+\\)\\]" 1 LaTeX-split-bibs)
       ("\\\\begin{refsection}\\[\\([^]]+\\)\\]" 1 LaTeX-split-bibs)))
   LaTeX-auto-class-regexp-list
   LaTeX-auto-label-regexp-list
   LaTeX-auto-index-regexp-list
   LaTeX-auto-minimal-regexp-list
   LaTeX-auto-pagestyle-regexp-list
   LaTeX-auto-counter-regexp-list
   LaTeX-auto-length-regexp-list
   LaTeX-auto-savebox-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defun LaTeX-split-bibs (match)
  "Extract bibliography resources from MATCH.
Split the string at commas and remove Biber file extensions."
  (let ((bibs (TeX-split-string " *, *" (TeX-match-buffer match))))
    (dolist (bib bibs)
      (LaTeX-add-bibliographies (replace-regexp-in-string
				 (concat "\\(?:\\."
					 (mapconcat 'regexp-quote
						    TeX-Biber-file-extensions
						    "\\|\\.")
					 "\\)")
				 "" bib)))))

(defun LaTeX-auto-prepare ()
  "Prepare for LaTeX parsing."
  (setq LaTeX-auto-arguments nil
	LaTeX-auto-optional nil
	LaTeX-auto-env-args nil
	LaTeX-auto-style nil
	LaTeX-auto-end-symbol nil))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-auto-prepare)

(defun LaTeX-listify-package-options (options)
  "Return a list from a comma-separated string of package OPTIONS.
The input string may include LaTeX comments and newlines."
  ;; We jump through all those hoops and don't just use `split-string'
  ;; or the like in order to be able to deal with key=value package
  ;; options which can look like this: "pdftitle={A Perfect Day},
  ;; colorlinks=false"
  (let (opts match start)
    (with-temp-buffer
      (set-syntax-table LaTeX-mode-syntax-table)
      (insert options)
      (newline) ; So that the last entry can be found.
      (goto-char (point-min))
      (setq start (point))
      (while (re-search-forward "[{ ,%\n\r]" nil t)
	(setq match (match-string 0))
	(cond
	 ;; Step over groups.  (Let's hope nobody uses escaped braces.)
	 ((string= match "{")
	  (up-list))
	 ;; Get rid of whitespace.
	 ((string= match " ")
	  (delete-region (1- (point))
			 (save-excursion
			   (skip-chars-forward " ")
			   (point))))
	 ;; Add entry to output.
	 ((or (string= match ",") (= (point) (point-max)))
	  (add-to-list 'opts (buffer-substring-no-properties
			      start (1- (point))) t)
	  (setq start (point)))
	 ;; Get rid of comments.
	 ((string= match "%")
	  (delete-region (1- (point))
			 (line-beginning-position 2)))
	 ;; Get rid of newlines.
	 ((or (string= match "\n") (string= match "\r"))
	  (delete-char -1)))))
    opts))

(defvar LaTeX-provided-class-options nil
  "Alist of options provided to LaTeX classes.
For each element, the CAR is the name of the class, the CDR is
the list of options provided to it.

E.g., its value will be
  \(\(\"book\" \"a4paper\" \"11pt\" \"openany\" \"fleqn\"\)
   ...\)
See also `LaTeX-provided-package-options'.")
(make-variable-buffer-local 'LaTeX-provided-class-options)

(defun LaTeX-provided-class-options-member (class option)
  "Return non-nil if OPTION has been given to CLASS at load time.
The value is actually the tail of the list of options given to CLASS."
  (member option (cdr (assoc class LaTeX-provided-class-options))))

(defvar LaTeX-provided-package-options nil
  "Alist of options provided to LaTeX packages.
For each element, the CAR is the name of the package, the CDR is
the list of options provided to it.

E.g., its value will be
  \(\(\"babel\" \"german\"\)
   \(\"geometry\" \"a4paper\" \"top=2cm\" \"bottom=2cm\" \"left=2.5cm\" \"right=2.5cm\"\)
   ...\)
See also `LaTeX-provided-class-options'.")
(make-variable-buffer-local 'LaTeX-provided-package-options)

(defun LaTeX-provided-package-options-member (package option)
  "Return non-nil if OPTION has been given to PACKAGE at load time.
The value is actually the tail of the list of options given to PACKAGE."
  (member option (cdr (assoc package LaTeX-provided-package-options))))

(defun LaTeX-auto-cleanup ()
  "Cleanup after LaTeX parsing."

  ;; Cleanup BibTeX/Biber files
  (setq LaTeX-auto-bibliography
	(apply 'append (mapcar (lambda (arg)
				 (TeX-split-string "," arg))
			       LaTeX-auto-bibliography)))

  ;; Reset class and packages options for the current buffer
  (setq LaTeX-provided-class-options nil)
  (setq LaTeX-provided-package-options nil)

  ;; Cleanup document classes and packages
  (unless (null LaTeX-auto-style)
    (while LaTeX-auto-style
      (let* ((entry (car LaTeX-auto-style))
	     (options (nth 0 entry))
	     (style (nth 1 entry))
	     (class (nth 2 entry)))

	;; Next document style.
	(setq LaTeX-auto-style (cdr LaTeX-auto-style))

	;; Get the options.
	(setq options (LaTeX-listify-package-options options))

        ;; Treat documentclass/documentstyle specially.
        (if (or (string-equal "package" class)
                (string-equal "Package" class))
            (dolist (elt (TeX-split-string
                          "\\([ \t\r\n]\\|%[^\n\r]*[\n\r]\\|,\\)+" style))
	      ;; Append style to the style list.
	      (add-to-list 'TeX-auto-file elt t)
              ;; Append to `LaTeX-provided-package-options' the name of the
              ;; package and the options provided to it at load time.
	      (unless (equal options '(""))
		(TeX-add-to-alist 'LaTeX-provided-package-options
				  (list (cons elt options)))))
	  ;; And a special "art10" style file combining style and size.
	  (add-to-list 'TeX-auto-file style t)
	  (add-to-list 'TeX-auto-file
		       (concat
			(cond ((string-equal "article" style)
			       "art")
			      ((string-equal "book" style)
			       "bk")
			      ((string-equal "report" style)
			       "rep")
			      ((string-equal "jarticle" style)
			       "jart")
			      ((string-equal "jbook" style)
			       "jbk")
			      ((string-equal "jreport" style)
			       "jrep")
			      ((string-equal "j-article" style)
			       "j-art")
			      ((string-equal "j-book" style)
			       "j-bk")
			      ((string-equal "j-report" style )
			       "j-rep")
			      (t style))
			(cond ((member "11pt" options)
			       "11")
			      ((member "12pt" options)
			       "12")
			      (t
			       "10"))) t)
	  (unless (equal options '(""))
	    (TeX-add-to-alist 'LaTeX-provided-class-options
			      (list (cons style options)))))

	;; The third argument if "class" indicates LaTeX2e features.
	(cond ((equal class "class")
	       (add-to-list 'TeX-auto-file "latex2e"))
	      ((equal class "style")
	       (add-to-list 'TeX-auto-file "latex2"))))))

  ;; Cleanup optional arguments
  (mapc (lambda (entry)
	  (add-to-list 'TeX-auto-symbol
		       (list (nth 0 entry)
			     (string-to-number (nth 1 entry)))))
	LaTeX-auto-arguments)

  ;; Cleanup default optional arguments
  (mapc (lambda (entry)
	  (add-to-list 'TeX-auto-symbol
		       (list (nth 0 entry)
			     (vector "argument")
			     (1- (string-to-number (nth 1 entry))))))
	LaTeX-auto-optional)

  ;; Cleanup environments arguments
  (mapc (lambda (entry)
	  (add-to-list 'LaTeX-auto-environment
		       (list (nth 0 entry)
			     (string-to-number (nth 1 entry)))))
	LaTeX-auto-env-args)

  ;; Cleanup use of def to add environments
  ;; NOTE: This uses an O(N^2) algorithm, while an O(N log N)
  ;; algorithm is possible.
  (mapc (lambda (symbol)
	  (if (not (TeX-member symbol TeX-auto-symbol 'equal))
	      ;; No matching symbol, insert in list
	      (add-to-list 'TeX-auto-symbol (concat "end" symbol))
	    ;; Matching symbol found, remove from list
	    (if (equal (car TeX-auto-symbol) symbol)
		;; Is it the first symbol?
		(setq TeX-auto-symbol (cdr TeX-auto-symbol))
	      ;; Nope!  Travel the list
	      (let ((list TeX-auto-symbol))
		(while (consp (cdr list))
		  ;; Until we find it.
		  (if (equal (car (cdr list)) symbol)
		      ;; Then remove it.
		      (setcdr list (cdr (cdr list))))
		  (setq list (cdr list)))))
	    ;; and add the symbol as an environment.
	    (add-to-list 'LaTeX-auto-environment symbol)))
	LaTeX-auto-end-symbol))

(add-hook 'TeX-auto-cleanup-hook 'LaTeX-auto-cleanup)

(defadvice LaTeX-add-bibliographies (after run-bib-style-hooks (&rest bibliographies) activate)
  "Add BIBLIOGRAPHIES to the list of known bibliographies and style files."
  (apply 'TeX-run-style-hooks bibliographies))

;;; Biber support

(defvar LaTeX-using-Biber nil
  "Used to track whether Biber is in use.")
(make-variable-buffer-local 'LaTeX-using-Biber)

;;; BibTeX

;;;###autoload
(defun BibTeX-auto-store ()
  "This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file."
  ;; We want this to be early in the list, so we do not
  ;; add it before we enter BibTeX mode the first time.
  (if (boundp 'local-write-file-hooks)
      (add-hook 'local-write-file-hooks 'TeX-safe-auto-write)
    (add-hook 'write-file-hooks 'TeX-safe-auto-write))
  (set (make-local-variable 'TeX-auto-update) 'BibTeX)
  (set (make-local-variable 'TeX-auto-untabify) nil)
  (set (make-local-variable 'TeX-auto-parse-length) 999999)
  (set (make-local-variable 'TeX-auto-regexp-list) BibTeX-auto-regexp-list)
  (set (make-local-variable 'TeX-master) t))

(defvar BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\([^, \n\r\t%\"#'()={}]*\\)" 1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

;;; Macro Argument Hooks

(defun TeX-arg-conditional (optional expr then else)
  "Implement if EXPR THEN ELSE.

If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.

If EXPR evaluate to true, parse THEN as an argument list, else parse
ELSE as an argument list."
  (TeX-parse-arguments (if (eval expr) then else)))

(defun TeX-arg-eval (optional &rest args)
  "Evaluate ARGS and insert value in buffer.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one."
  (TeX-argument-insert (eval args) optional))

(defun TeX-arg-label (optional &optional prompt definition)
  "Prompt for a label completing with known labels.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen label to the
list of defined labels."
  (let ((label (completing-read (TeX-argument-prompt optional prompt "Key")
				(LaTeX-label-list))))
    (if (and definition (not (string-equal "" label)))
	(LaTeX-add-labels label))
    (TeX-argument-insert label optional optional)))

(defun TeX-arg-ref (optional &optional prompt definition)
  "Let-bind `reftex-ref-macro-prompt' to nil and pass arguments
to `TeX-arg-label'.

See the documentation of `TeX-arg-label' for details on the
arguments: OPTIONAL, PROMPT, and DEFINITION."
  (let ((reftex-ref-macro-prompt nil))
    (TeX-arg-label optional prompt definition)))

(defun TeX-arg-index-tag (optional &optional prompt &rest args)
  "Prompt for an index tag.
This is the name of an index, not the entry.

If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  ARGS is unused."
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt "Index tag")) optional))

(defun TeX-arg-index (optional &optional prompt &rest args)
  "Prompt for an index entry completing with known entries.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  ARGS is unused."
  (let ((entry (completing-read (TeX-argument-prompt optional prompt "Key")
				(LaTeX-index-entry-list))))
    (if (and (not (string-equal "" entry))
	     (not (member (list entry) (LaTeX-index-entry-list))))
	(LaTeX-add-index-entries entry))
    (TeX-argument-insert entry optional optional)))

(defalias 'TeX-arg-define-index 'TeX-arg-index)

(defun TeX-arg-macro (optional &optional prompt definition)
  "Prompt for a TeX macro with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen macro to the
list of defined macros."
  (let ((macro (completing-read (TeX-argument-prompt optional prompt
						     (concat "Macro: "
							     TeX-esc)
						     t)
				(TeX-symbol-list))))
    (if (and definition (not (string-equal "" macro)))
	(TeX-add-symbols macro))
    (TeX-argument-insert macro optional TeX-esc)))

(defun TeX-arg-environment (optional &optional prompt definition)
  "Prompt for a LaTeX environment with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen environment to
the list of defined environments."
  (let ((environment (completing-read (TeX-argument-prompt optional prompt
							   "Environment")
				      (LaTeX-environment-list))))
    (if (and definition (not (string-equal "" environment)))
	(LaTeX-add-environments environment))

    (TeX-argument-insert environment optional)))

;; Why is DEFINITION unused?
(defun TeX-arg-cite (optional &optional prompt definition)
  "Prompt for a BibTeX citation with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  DEFINITION is unused."
  (let ((items (multi-prompt "," t (TeX-argument-prompt optional prompt "Key")
			     (LaTeX-bibitem-list))))
    (apply 'LaTeX-add-bibitems items)
    (TeX-argument-insert (mapconcat 'identity items ",") optional optional)))

(defun TeX-arg-counter (optional &optional prompt definition)
  "Prompt for a LaTeX counter.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen counter to
the list of defined counters."
  (let ((counter (completing-read (TeX-argument-prompt optional prompt
						       "Counter")
				  (LaTeX-counter-list))))
    (if (and definition (not (string-equal "" counter)))
	(LaTeX-add-counters counter))
    (TeX-argument-insert counter optional)))

(defun TeX-arg-savebox (optional &optional prompt definition)
  "Prompt for a LaTeX savebox.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If definition is non-nil, the savebox is added to the
list of defined saveboxes."
  (let ((savebox (completing-read (TeX-argument-prompt optional prompt
						       (concat "Savebox: "
							       TeX-esc) t)
                                   (LaTeX-savebox-list))))
    (if (and definition (not (zerop (length savebox))))
        (LaTeX-add-saveboxes savebox))
    (TeX-argument-insert savebox optional TeX-esc)))

(defun TeX-arg-length (optional &optional prompt initial-input definition)
  "Prompt for a LaTeX length.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If INITIAL-INPUT is non-nil, insert it in the minibuffer
initially, with point positioned at the end.  If DEFINITION is
non-nil, the length is added to the list of defined length."
  (let ((length (completing-read (TeX-argument-prompt optional prompt "Length")
				 ;; A valid length can be a macro or a length of
				 ;; the form <value><dimension>.  Input starting
				 ;; with a `\' can be completed with length
				 ;; macros.
				 (mapcar (lambda(elt) (concat TeX-esc (car elt)))
					 (LaTeX-length-list))
				 ;; Some macros takes as argument only a length
				 ;; macro (e.g., `\setlength' in its first
				 ;; argument, and `\newlength'), in this case is
				 ;; convenient to set `\\' as initial input.
				 nil nil initial-input)))
    (if (and definition (not (zerop (length length))))
	;; Strip leading TeX-esc from macro name
        (LaTeX-add-lengths (substring length 1)))
    (TeX-argument-insert length optional)))

(defun TeX-arg-file (optional &optional prompt)
  "Prompt for a filename in the current directory.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-argument-insert (read-file-name (TeX-argument-prompt optional
							    prompt "File")
				       "" "" nil)
		       optional))

(defun TeX-arg-file-name (optional &optional prompt)
  "Prompt for a file name.
Initial input is the name of the file being visited in the
current buffer, with extension.  If OPTIONAL is non-nil, insert
it as an optional argument.  Use PROMPT as the prompt string."
  (TeX-argument-insert
   (TeX-read-string
    (TeX-argument-prompt optional prompt "Name")
    (file-name-nondirectory buffer-file-name))
   optional))

(defun TeX-arg-file-name-sans-extension (optional &optional prompt)
  "Prompt for a file name.
Initial input is the name of the file being visited in the
current buffer, without extension.  If OPTIONAL is non-nil,
insert it as an optional argument.  Use PROMPT as the prompt
string."
  (TeX-argument-insert
   (TeX-read-string
    (TeX-argument-prompt optional prompt "Name")
    (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
   optional))

(defun TeX-arg-define-label (optional &optional prompt)
  "Prompt for a label completing with known labels.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-label optional prompt t))

(defun TeX-arg-define-macro (optional &optional prompt)
  "Prompt for a TeX macro with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-macro optional prompt t))

(defun TeX-arg-define-environment (optional &optional prompt)
  "Prompt for a LaTeX environment with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-environment optional prompt t))

(defun TeX-arg-define-cite (optional &optional prompt)
  "Prompt for a BibTeX citation.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-cite optional prompt t))

(defun TeX-arg-define-counter (optional &optional prompt)
  "Prompt for a LaTeX counter.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-counter optional prompt t))

(defun TeX-arg-define-savebox (optional &optional prompt)
  "Prompt for a LaTeX savebox.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-savebox optional prompt t))

(defun TeX-arg-define-length (optional &optional prompt)
  "Prompt for a LaTeX length.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-arg-length optional prompt "\\" t))

(defcustom LaTeX-style-list '(("amsart")
			      ("amsbook")
			      ("article")
			      ("beamer")
			      ("book")
			      ("dinbrief")
			      ("foils")
			      ("letter")
			      ("memoir")
			      ("minimal")
			      ("prosper")
			      ("report")
			      ("scrartcl")
			      ("scrbook")
			      ("scrlttr2")
			      ("scrreprt")
			      ("slides"))
  "List of document classes offered when inserting a document environment.

If `TeX-arg-input-file-search' is set to `t', you will get
completion with all LaTeX classes available in your distribution
and this variable will be ignored."
  :group 'LaTeX-environment
  :type '(repeat (group (string :format "%v"))))

(defvar LaTeX-global-class-files nil
  "List of the LaTeX class files.
Initialized once at the first time you prompt for a LaTeX class.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

(defvar TeX-after-document-hook nil
  "List of functions to be run at the end of `TeX-arg-document'.

To insert a hook here, you must insert it in the appropiate style file.")

(defun TeX-arg-document (optional &optional ignore)
  "Insert arguments to documentclass.
OPTIONAL and IGNORE are ignored."
  (let* ((TeX-file-extensions '("cls"))
	 (crm-separator ",")
	 style var options)
    (unless LaTeX-global-class-files
      (if (if (eq TeX-arg-input-file-search 'ask)
	      (not (y-or-n-p "Find class yourself? "))
	    TeX-arg-input-file-search)
	  (progn
	    (message "Searching for LaTeX classes...")
	    (setq LaTeX-global-class-files
		  (mapcar 'identity (TeX-search-files-by-type 'texinputs 'global t t))))
	LaTeX-style-list))
    (setq style (completing-read
		 (concat "Document class: (default " LaTeX-default-style ") ")
		 LaTeX-global-class-files nil nil nil nil LaTeX-default-style))
    ;; Clean up hook before use.
    (setq TeX-after-document-hook nil)
    (TeX-run-style-hooks style)
    (setq var (intern (format "LaTeX-%s-class-options" style)))
    (if (or (and (boundp var)
		 (listp (symbol-value var)))
	    (fboundp var))
	(if (functionp var)
	    (setq options (funcall var))
	  (when (symbol-value var)
	    (setq options
		  (mapconcat 'identity
			     (TeX-completing-read-multiple
			      "Options: " (mapcar 'list (symbol-value var)) nil nil
			      (if (stringp LaTeX-default-options)
				  LaTeX-default-options
				(mapconcat 'identity LaTeX-default-options ",")))
			     ","))))
      (setq options (read-string "Options: ")))
    (unless (zerop (length options))
      (insert LaTeX-optop options LaTeX-optcl)
      (let ((opts (LaTeX-listify-package-options options)))
	(TeX-add-to-alist 'LaTeX-provided-class-options
			  (list (cons style opts)))))
    (insert TeX-grop style TeX-grcl))

  ;; remove old information
  (TeX-remove-style)

  ;; defined in individual style hooks
  (TeX-update-style)
  (run-hooks 'TeX-after-document-hook))

(defvar LaTeX-after-usepackage-hook nil
  "List of functions to be run at the end of `LaTeX-arg-usepackage'.

To insert a hook here, you must insert it in the appropiate style file.")

(defun LaTeX-arg-usepackage-read-packages-with-options ()
  "Read the packages and the options for the usepackage macro.

If at least one package is provided, this function returns a cons
cell, whose CAR is the list of packages and the CDR is the string
of the options, nil otherwise."
  (let* ((TeX-file-extensions '("sty"))
	 (crm-separator ",")
	 packages var options)
    (unless TeX-global-input-files
      (if (if (eq TeX-arg-input-file-search 'ask)
	      (not (y-or-n-p "Find packages yourself? "))
	    TeX-arg-input-file-search)
	  (progn
	    (message "Searching for LaTeX packages...")
	    (setq TeX-global-input-files
		  (mapcar 'list (TeX-search-files-by-type
				 'texinputs 'global t t))))))
    (setq packages (TeX-completing-read-multiple
		    "Packages: " TeX-global-input-files))
    ;; Clean up hook before use in `LaTeX-arg-usepackage-insert'.
    (setq LaTeX-after-usepackage-hook nil)
    (mapc 'TeX-run-style-hooks packages)
    ;; Prompt for options only if at least one package has been supplied, return
    ;; nil otherwise.
    (when packages
      (setq var (if (= 1 (length packages))
		    (intern (format "LaTeX-%s-package-options" (car packages)))
		  ;; Something like `\usepackage[options]{pkg1,pkg2,pkg3,...}' is
		  ;; allowed (provided that pkg1, pkg2, pkg3, ... accept same
		  ;; options).  When there is more than one package, set `var' to
		  ;; a dummy value so next `if' enters else form.
		  t))
      (if (or (and (boundp var)
		   (listp (symbol-value var)))
	      (fboundp var))
	  (if (functionp var)
	      (setq options (funcall var))
	    (when (symbol-value var)
	      (setq options
		    (mapconcat 'identity
			       (TeX-completing-read-multiple
				"Options: " (mapcar 'list (symbol-value var)))
			       ","))))
	(setq options (read-string "Options: ")))
      (cons packages options))))

(defun LaTeX-arg-usepackage-insert (packages options)
  "Actually insert arguments to usepackage."
  (unless (zerop (length options))
    (let ((opts (LaTeX-listify-package-options options)))
      (mapc (lambda (elt)
	      (TeX-add-to-alist 'LaTeX-provided-package-options
				(list (cons elt opts))))
	    packages))
    (insert LaTeX-optop options LaTeX-optcl))
  (insert TeX-grop (mapconcat 'identity packages ",") TeX-grcl)
  (run-hooks 'LaTeX-after-usepackage-hook))

(defun LaTeX-arg-usepackage (optional)
  "Insert arguments to usepackage.
OPTIONAL is ignored."
  (let* ((packages-options (LaTeX-arg-usepackage-read-packages-with-options))
	 (packages (car packages-options))
	 (options (cdr packages-options)))
    (LaTeX-arg-usepackage-insert packages options)))

(defun LaTeX-insert-usepackages ()
  "Prompt for the insertion of usepackage macros until empty
input is reached.

Return t if at least one \\usepackage has been inserted, nil
otherwise."
  (let (packages-options packages options (inserted nil))
    (while (setq packages-options
		 (LaTeX-arg-usepackage-read-packages-with-options))
      (setq packages (car packages-options))
      (setq options (cdr packages-options))
      (insert TeX-esc "usepackage")
      (LaTeX-arg-usepackage-insert packages options)
      (LaTeX-newline)
      (setq inserted t))
    inserted))

(defcustom LaTeX-search-files-type-alist
  '((texinputs "${TEXINPUTS.latex}" ("tex/generic/" "tex/latex/")
	       TeX-file-extensions)
    (docs "${TEXDOCS}" ("doc/") TeX-doc-extensions)
    (graphics "${TEXINPUTS}" ("tex/") LaTeX-includegraphics-extensions)
    (bibinputs "${BIBINPUTS}" ("bibtex/bib/") BibTeX-file-extensions)
    (bstinputs "${BSTINPUTS}" ("bibtex/bst/") BibTeX-style-extensions)
    (bbxinputs "" ("tex/latex/") BibLaTeX-style-extensions)
    (biberinputs "${BIBINPUTS}" ("bibtex/bib/") TeX-Biber-file-extensions))
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

Reset the mode for a change of this variable to take effect."
  :group 'TeX-file
  :type '(alist :key-type symbol
		:value-type
		(group (string :tag "Kpathsea variable")
		       (choice :tag "Directories"
			       (repeat :tag "TDS subdirectories" string)
			       (repeat :tag "Absolute directories" directory)
			       (repeat :tag "Variables" variable))
		       (choice :tag "Extensions"
			       variable (repeat string)))))

(defcustom TeX-arg-input-file-search t
  "If `TeX-arg-input-file' should search for files.
If the value is t, files in TeX's search path are searched for
and provided for completion.  The file name is then inserted
without directory and extension.  If the value is nil, the file
name can be specified manually and is inserted with a path
relative to the directory of the current buffer's file and with
extension.  If the value is `ask', you are asked for the method
to use every time `TeX-arg-input-file' is called."
  :group 'LaTeX-macro
  :type '(choice (const t) (const nil) (const ask)))

(defvar TeX-global-input-files nil
  "List of the non-local TeX input files.
Initialized once at the first time you prompt for an input file.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

(defun TeX-arg-input-file (optional &optional prompt local)
  "Prompt for a tex or sty file.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  PROMPT is the prompt,
LOCAL is a flag.  If the flag is set, only complete with local
files."
  (let ((search (if (eq TeX-arg-input-file-search 'ask)
		    (not (y-or-n-p "Find file yourself? "))
		  TeX-arg-input-file-search))
	file style)
    (if search
	(progn
	  (unless (or TeX-global-input-files local)
	    (message "Searching for files...")
	    (setq TeX-global-input-files
		  (mapcar 'list (TeX-search-files-by-type
				 'texinputs 'global t t))))
	  (setq file (completing-read
		      (TeX-argument-prompt optional prompt "File")
		      (TeX-delete-dups-by-car
		       (append (mapcar 'list (TeX-search-files-by-type
					      'texinputs 'local t t))
			       (unless local
				 TeX-global-input-files))))
		style file))
      (setq file (read-file-name
		  (TeX-argument-prompt optional prompt "File") nil ""))
      (unless (string-equal file "")
	(setq file (file-relative-name file)))
      (setq style (file-name-sans-extension (file-name-nondirectory file))))
    (unless (string-equal "" style)
      (TeX-run-style-hooks style))
    (TeX-argument-insert file optional)))

(defvar BibTeX-global-style-files nil
  "Association list of BibTeX style files.

Initialized once at the first time you prompt for an input file.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

(defvar BibLaTeX-global-style-files nil
  "Association list of BibLaTeX style files.

Initialized once at the first time you prompt for a BibLaTeX
style.  May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

(defun TeX-arg-bibstyle (optional &optional prompt)
  "Prompt for a BibTeX style file.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (message "Searching for BibTeX styles...")
  (or BibTeX-global-style-files
      (setq BibTeX-global-style-files
	    (mapcar 'list (TeX-search-files-by-type 'bstinputs 'global t t))))
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "BibTeX style")
		    (append (mapcar 'list (TeX-search-files-by-type
					   'bstinputs 'local t t))
			    BibTeX-global-style-files))
   optional))

(defvar BibTeX-global-files nil
  "Association list of BibTeX files.

Initialized once at the first time you prompt for a BibTeX file.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

(defvar TeX-Biber-global-files nil
  "Association list of Biber files.

Initialized once at the first time you prompt for an Biber file.
May be reset with `\\[universal-argument] \\[TeX-normal-mode]'.")

(defun TeX-arg-bibliography (optional &optional prompt)
  "Prompt for a BibTeX database file.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (message "Searching for BibTeX files...")
  (or BibTeX-global-files
      (setq BibTeX-global-files
	    (mapcar 'list (TeX-search-files-by-type 'bibinputs 'global t t))))
  (let ((styles (multi-prompt
		 "," t
		 (TeX-argument-prompt optional prompt "BibTeX files")
		 (append (mapcar 'list (TeX-search-files-by-type
					'bibinputs 'local t t))
			 BibTeX-global-files))))
    (apply 'LaTeX-add-bibliographies styles)
    ;; Run style files associated to the bibliography database files in order to
    ;; immediately fill `LaTeX-bibitem-list'.
    (mapc 'TeX-run-style-hooks styles)
    (TeX-argument-insert (mapconcat 'identity styles ",") optional)))

(defun TeX-arg-corner (optional &optional prompt)
  "Prompt for a LaTeX side or corner position with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("l") ("r") ("t") ("b") ("tl") ("tr") ("bl") ("br"))
		    nil t)
   optional))

(defun TeX-arg-lr (optional &optional prompt)
  "Prompt for a LaTeX side with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("l") ("r"))
		    nil t)
   optional))

(defun TeX-arg-tb (optional &optional prompt)
  "Prompt for a LaTeX side with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("t") ("b"))
		    nil t)
   optional))

(defcustom TeX-date-format "%Y/%m/%d"
  "The default date format prompted by `TeX-arg-date'."
  :group 'LaTeX-macro
  :type 'string)

(defun TeX-arg-date (optional &optional prompt)
  "Prompt for a date, defaulting to the current date.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (let ((default (format-time-string TeX-date-format (current-time))))
    (TeX-argument-insert
     (TeX-read-string (TeX-argument-prompt
		       optional prompt (format "Date (default %s)" default))
		      nil nil default)
     optional)))

(defun TeX-arg-version (optional &optional prompt)
  "Prompt for the version of a file.
Use as initial input the current date.  If OPTIONAL is non-nil,
insert the resulting value as an optional argument, otherwise as
a mandatory one.  Use PROMPT as the prompt string."
  (TeX-argument-insert
   (TeX-read-string (TeX-argument-prompt optional prompt "Version")
		    (format-time-string "%Y/%m/%d" (current-time)))
   optional))

(defun TeX-arg-pagestyle (optional &optional prompt definition)
  "Prompt for a LaTeX pagestyle with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen pagestyle to
the list of defined pagestyles."
  (let ((pagestyle (completing-read (TeX-argument-prompt optional prompt
							 "Pagestyle")
				    (LaTeX-pagestyle-list))))
    (if (and definition (not (string-equal "" pagestyle)))
	(LaTeX-add-pagestyles pagestyle))
    (TeX-argument-insert pagestyle optional)))

(defcustom LaTeX-default-verb-delimiter ?|
  "Default delimiter for `\\verb' macros."
  :group 'LaTeX-macro
  :type 'character)

(defun TeX-arg-verb (optional &optional ignore)
  "Prompt for delimiter and text.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  IGNORE is ignored."
  (let ((del (read-quoted-char
	      (concat "Delimiter: (default "
		      (char-to-string LaTeX-default-verb-delimiter) ") "))))
    (when (<= del ?\ ) (setq del LaTeX-default-verb-delimiter))
    (if (TeX-active-mark)
	(progn
	  (insert del)
	  (goto-char (mark))
	  (insert del))
      (insert del (read-from-minibuffer "Text: ") del))
    (setq LaTeX-default-verb-delimiter del)))

(defun TeX-arg-pair (optional first second)
  "Insert a pair of number, prompted by FIRST and SECOND.

The numbers are surounded by parenthesizes and separated with a
comma.

If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one."
  (insert "(" (read-string (concat first  ": ")) ","
	      (read-string (concat second ": ")) ")"))

(defun TeX-arg-size (optional)
  "Insert width and height as a pair.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one."
  (TeX-arg-pair optional "Width" "Height"))

(defun TeX-arg-coordinate (optional)
  "Insert x and y coordinate as a pair.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one."
 (TeX-arg-pair optional "X position" "Y position"))

(defconst TeX-braces-default-association
  '(("[" . "]")
    ("\\{" . "\\}")
    ("(" . ")")
    ("|" . "|")
    ("\\|" . "\\|")
    ("/" . "/")
    ("\\backslash" . "\\backslash")
    ("\\lfloor" . "\\rfloor")
    ("\\lceil" . "\\rceil")
    ("\\langle" . "\\rangle")))

(defcustom TeX-braces-user-association nil
  "A list of your personal association of brace symbols.
These are used for \\left and \\right.

The car of each entry is the brace used with \\left,
the cdr is the brace used with \\right."
  :group 'LaTeX-macro
  :group 'LaTeX-math
  :type '(repeat (cons :format "%v"
		       (string :tag "Left")
		       (string :tag "Right"))))

(defvar TeX-braces-association
  (append TeX-braces-user-association
	  TeX-braces-default-association)
    "A list of association of brace symbols for \\left and \\right.
The car of each entry is the brace used with \\left,
the cdr is the brace used with \\right.")

(defcustom LaTeX-electric-left-right-brace nil
  "If non-nil, insert right brace with suitable macro after typing left brace."
  :group 'LaTeX-macro
  :type 'boolean)

(defvar TeX-left-right-braces
  '(("[") ("]") ("\\{") ("\\}") ("(") (")") ("|") ("\\|")
    ("/") ("\\backslash") ("\\lfloor") ("\\rfloor")
    ("\\lceil") ("\\rceil") ("\\langle") ("\\rangle")
    ("\\uparrow") ("\\Uparrow") ("\\downarrow") ("\\Downarrow")
    ("\\updownarrow") ("\\Updownarrow") ("."))
  "List of symbols which can follow the \\left or \\right command.")

(defvar LaTeX-left-right-macros-association
  '(("left" . "right")
    ("bigl" . "bigr") ("Bigl" . "Bigr")
    ("biggl" . "biggr") ("Biggl" . "Biggr"))
  "Alist of macros for adjusting size of left and right braces.
The car of each entry is for left brace and the cdr is for right brace.")

(defun TeX-arg-insert-braces (optional &optional prompt)
  "Prompt for a brace for \\left and insert the corresponding \\right.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (let (left-macro)
    (save-excursion
      ;; Obtain macro name such as "left", "bigl" etc.
      (setq left-macro (buffer-substring-no-properties
                        (point)
                        (progn (backward-word 1) (point))))
      (backward-char)
      (LaTeX-newline)
      (indent-according-to-mode)
      ;; Delete possibly produced blank line.
      (beginning-of-line 0)
      (if (looking-at "^[ \t]*$")
          (progn (delete-horizontal-space)
                 (delete-char 1))))
    (let ((left-brace (completing-read
                       (TeX-argument-prompt optional prompt
					    "Which brace")
                       TeX-left-right-braces)))
      (insert left-brace)
      (LaTeX-newline)
      (save-excursion
	(if (TeX-active-mark)
	    (goto-char (mark)))
        (LaTeX-newline)
        (LaTeX-insert-corresponding-right-macro-and-brace
         left-macro left-brace optional prompt)
        (indent-according-to-mode))
      (indent-according-to-mode))))

(defun TeX-arg-insert-right-brace-maybe (optional)
  "Insert the suitable right brace macro such as \\rangle.
Insertion is done when `TeX-arg-right-insert-p' is non-nil.
If the left brace macro is preceeded by \\left, \\bigl etc.,
supply the corresponding macro such as \\right before the right brace macro.
OPTIONAL is ignored."
  ;; Nothing is done when TeX-arg-right-insert-p is nil.
  (when TeX-arg-right-insert-p
    (let (left-brace left-macro)
      (save-excursion
	;; Obtain left brace macro name such as "\langle".
	(setq left-brace (buffer-substring-no-properties
			  (point)
			  (progn (backward-word) (backward-char)
				 (point)))
	      ;; Obtain the name of preceeding left macro, if any,
	      ;; such as "left", "bigl" etc.
	      left-macro (LaTeX-find-preceeding-left-macro-name)))
      (save-excursion
	(if (TeX-active-mark)
	    (goto-char (mark)))
	(LaTeX-insert-corresponding-right-macro-and-brace
	 left-macro left-brace optional)))))

(defun LaTeX-insert-left-brace (arg)
  "Insert typed left brace ARG times and possibly a correspondig right brace.
Automatic right brace insertion is done only if no prefix ARG is given and
`LaTeX-electric-left-right-brace' is non-nil.
Normally bound to keys \(, { and [."
  (interactive "*P")
  (let ((auto-p (and LaTeX-electric-left-right-brace (not arg))))
    (if (and auto-p
	     (TeX-active-mark)
	     (> (point) (mark)))
	(exchange-point-and-mark))
    (self-insert-command (prefix-numeric-value arg))
    (if auto-p
      (let ((lbrace (char-to-string last-command-event)) lmacro skip-p)
        (save-excursion
          (backward-char)
	  ;; The brace "{" is exceptional in two aspects.
	  ;; 1. "\{" should be considered as a single brace
	  ;;    like "(" and "[".
	  ;; 2. "\left{" is nonsense while "\left\{" and
	  ;;    "\left(" are not.
	  (if (string= lbrace TeX-grop)
	      ;; If "{" follows "\", set lbrace to "\{".
	      (if (TeX-escaped-p)
		  (progn
		    (backward-char)
		    (setq lbrace (concat TeX-esc TeX-grop)))
		;; Otherwise, don't search for left macros.
		(setq skip-p t)))
	  (unless skip-p
	    ;; Obtain the name of preceeding left macro, if any,
	    ;; such as "left", "bigl" etc.
	    (setq lmacro (LaTeX-find-preceeding-left-macro-name))))
        (let ((TeX-arg-right-insert-p t)
              ;; "{" and "}" are paired temporally so that typing
	      ;; a single "{" should insert a pair "{}".
              (TeX-braces-association
               (cons (cons TeX-grop TeX-grcl) TeX-braces-association)))
	  (save-excursion
	    (if (TeX-active-mark)
		(goto-char (mark)))
	    (LaTeX-insert-corresponding-right-macro-and-brace
	     lmacro lbrace)))))))

(defun LaTeX-insert-corresponding-right-macro-and-brace
  (lmacro lbrace &optional optional prompt)
  "Insert right macro and brace correspoinding to LMACRO and LBRACE.
Left-right association is determined through
`LaTeX-left-right-macros-association' and `TeX-braces-association'.

If brace association can't be determined or `TeX-arg-right-insert-p'
is nil, consult user which brace should be used."
  ;; This function is called with LMACRO being one of the following
  ;; possibilities.
  ;;  (1) nil, which means LBRACE is isolated.
  ;;  (2) null string, which means LBRACE follows right after "\" to
  ;;      form "\(" or "\[".
  ;;  (3) a string in CARs of `LaTeX-left-right-macros-association'.
  (let ((rmacro (cdr (assoc lmacro LaTeX-left-right-macros-association)))
	(rbrace (cdr (assoc lbrace TeX-braces-association))))
    ;; Since braces like "\(" and "\)" should be paired, RMACRO
    ;; should be considered as null string in the case (2).
    (if (string= lmacro "")
	(setq rmacro ""))
    ;; Insert right macros such as "\right", "\bigr" etc., if necessary.
    ;; Even single "\" will be inserted so that "\)" or "\]" is
    ;; inserted after "\(", "\[".
    (if rmacro
	(insert TeX-esc rmacro))
    (cond
     ((and TeX-arg-right-insert-p rbrace)
      (insert rbrace))
     (rmacro
      (insert (completing-read
	       (TeX-argument-prompt
		optional prompt
		(format "Which brace (default %s)"
			(or rbrace "."))) TeX-left-right-braces
			nil nil nil nil (or rbrace ".")))))))

(defun LaTeX-find-preceeding-left-macro-name ()
  "Return the left macro name just before the point, if any.
If the preceeding macro isn't left macros such as \\left, \\bigl etc.,
return nil.
If the point is just after unescaped `TeX-esc', return the null string."
  ;; \left-!- => "left"
  ;; \-!- => ""
  ;; \infty-!- => nil
  ;; \&-!- => nil
  ;; \mathrm{abc}-!- => nil
  ;; {blah blah blah}-!- => nil
  ;; \\-!- => nil
  (let ((name (buffer-substring-no-properties
	       (point)
	       ;; This is only a helper function, so we do not
	       ;; preserve point by save-excursion.
	       (progn
		 ;; Assume left macro names consist of only A-Z and a-z.
		 (skip-chars-backward "A-Za-z")
		 (point)))))
    (if (and (TeX-escaped-p)
	     (or (string= name "")
		 (assoc name LaTeX-left-right-macros-association)))
	name)))

(defcustom LaTeX-default-author 'user-full-name
  "Initial input to `LaTeX-arg-author' prompt.
If nil, do not prompt at all."
  :group 'LaTeX-macro
  :type '(choice (const :tag "User name in Emacs" user-full-name)
		 (const :tag "Do not prompt" nil)
		 string))

(defun LaTeX-arg-author (optional &optional prompt)
  "Prompt for author name.
Insert the given value as a TeX macro argument.  If OPTIONAL is
non-nil, insert it as an optional argument.  Use PROMPT as the
prompt string.  `LaTeX-default-author' is the initial input."
  (let ((author (if LaTeX-default-author
		    (read-string
		     (TeX-argument-prompt optional prompt "Author(s)")
		     (if (symbolp LaTeX-default-author)
			 (symbol-value LaTeX-default-author)
		       LaTeX-default-author))
		  "")))
    (TeX-argument-insert author optional nil)))

(defun TeX-read-key-val (optional key-val-alist &optional prompt)
  "Prompt for keys and values in KEY-VAL-ALIST and return them.
If OPTIONAL is non-nil, indicate in the prompt that we are
reading an optional argument.  KEY-VAL-ALIST is an alist.  The
car of each element should be a string representing a key and the
optional cdr should be a list with strings to be used as values
for the key.  Use PROMPT as the prompt string."
  (multi-prompt-key-value
   (TeX-argument-prompt optional prompt "Options (k=v)")
   (if (symbolp key-val-alist)
       (eval key-val-alist)
     key-val-alist)))

(defun TeX-arg-key-val (optional key-val-alist &optional prompt)
  "Prompt for keys and values in KEY-VAL-ALIST.
Insert the given value as a TeX macro argument.  If OPTIONAL is
non-nil, insert it as an optional argument.  KEY-VAL-ALIST is an
alist.  The car of each element should be a string representing a
key and the optional cdr should be a list with strings to be used
as values for the key.  Use PROMPT as the prompt string."
  (let ((options (TeX-read-key-val optional key-val-alist prompt)))
    (TeX-argument-insert options optional)))


;;; Verbatim constructs

(defcustom LaTeX-verbatim-macros-with-delims
  '("verb" "verb*")
  "Macros for inline verbatim with arguments in delimiters, like \\foo|...|.

Programs should not use this variable directly but the function
`LaTeX-verbatim-macros-with-delims' which returns a value
including buffer-local keyword additions via
`LaTeX-verbatim-macros-with-delims-local' as well."
  :group 'LaTeX-macro
  :type '(repeat (string)))

(defvar LaTeX-verbatim-macros-with-delims-local nil
  "Buffer-local variable for inline verbatim with args in delimiters.

Style files should add constructs to this variable and not to
`LaTeX-verbatim-macros-with-delims'.

Programs should not use this variable directly but the function
`LaTeX-verbatim-macros-with-delims' which returns a value
including values of the variable
`LaTeX-verbatim-macros-with-delims' as well.")
(make-variable-buffer-local 'LaTeX-verbatim-macros-with-delims-local)

(defcustom LaTeX-verbatim-macros-with-braces nil
  "Macros for inline verbatim with arguments in braces, like \\foo{...}.

Programs should not use this variable directly but the function
`LaTeX-verbatim-macros-with-braces' which returns a value
including buffer-local keyword additions via
`LaTeX-verbatim-macros-with-braces-local' as well."
  :group 'LaTeX-macro
  :type '(repeat (string)))

(defvar LaTeX-verbatim-macros-with-braces-local nil
  "Buffer-local variable for inline verbatim with args in braces.

Style files should add constructs to this variable and not to
`LaTeX-verbatim-macros-with-braces'.

Programs should not use this variable directly but the function
`LaTeX-verbatim-macros-with-braces' which returns a value
including values of the variable
`LaTeX-verbatim-macros-with-braces' as well.")
(make-variable-buffer-local 'LaTeX-verbatim-macros-with-braces-local)

(defcustom LaTeX-verbatim-environments
  '("verbatim" "verbatim*")
  "Verbatim environments.

Programs should not use this variable directly but the function
`LaTeX-verbatim-environments' which returns a value including
buffer-local keyword additions via
`LaTeX-verbatim-environments-local' as well."
  :group 'LaTeX-environment
  :type '(repeat (string)))

(defvar LaTeX-verbatim-environments-local nil
  "Buffer-local variable for inline verbatim environments.

Style files should add constructs to this variable and not to
`LaTeX-verbatim-environments'.

Programs should not use this variable directly but the function
`LaTeX-verbatim-environments' which returns a value including
values of the variable `LaTeX-verbatim-environments' as well.")
(make-variable-buffer-local 'LaTeX-verbatim-environments-local)

(defun LaTeX-verbatim-macros-with-delims ()
  "Return list of verbatim macros with delimiters."
  (append LaTeX-verbatim-macros-with-delims
	  LaTeX-verbatim-macros-with-delims-local))

(defun LaTeX-verbatim-macros-with-braces ()
  "Return list of verbatim macros with braces."
  (append LaTeX-verbatim-macros-with-braces
	  LaTeX-verbatim-macros-with-braces-local))

(defun LaTeX-verbatim-environments ()
  "Return list of verbatim environments."
  (append LaTeX-verbatim-environments
	  LaTeX-verbatim-environments-local))

(defun LaTeX-verbatim-macro-boundaries ()
  "Return boundaries of verbatim macro.
Boundaries are returned as a cons cell where the car is the macro
start and the cdr the macro end.

Only macros which enclose their arguments with special
non-parenthetical delimiters, like \\verb+foo+, are recognized."
  (save-excursion
    (let ((orig (point))
	  (verbatim-regexp (regexp-opt (LaTeX-verbatim-macros-with-delims) t)))
      ;; Search backwards for the macro start, unless we are facing one
      (unless (looking-at (concat (regexp-quote TeX-esc) verbatim-regexp))
	(catch 'found
	  (while (progn
		   (skip-chars-backward (concat "^\n" (regexp-quote TeX-esc))
					(line-beginning-position))
		   (when (looking-at verbatim-regexp) (throw 'found nil))
		   (or (bobp) (forward-char -1))
		   (/= (point) (line-beginning-position))))))
      ;; Search forward for the macro end, unless we failed to find a start
      (unless (bolp)
	(let* ((beg (1- (point)))
	       (macro-end (match-end 0))
	       ;; XXX: Here we assume we are dealing with \verb which
	       ;; expects the delimiter right behind the command.
	       ;; However, \lstinline can also cope with whitespace as
	       ;; well as an optional argument after the command.
	       (delimiter (buffer-substring-no-properties
			   macro-end (1+ macro-end))))
	  ;; Heuristic: If an opening brace is encountered, search for
	  ;; both the opening and the closing brace as an end marker.
	  ;; Like that the function should work for \verb|...| as well
	  ;; as for \url{...}.
	  (when (string= delimiter TeX-grop)
	    (setq delimiter (concat delimiter TeX-grcl)))
	  (goto-char (1+ macro-end))
	  (skip-chars-forward (concat "^" delimiter))
	  (when (<= orig (point))
	    (cons beg (1+ (point)))))))))

(defun LaTeX-current-verbatim-macro ()
  "Return name of verbatim macro containing point, nil if none is present."
  (let ((macro-boundaries (LaTeX-verbatim-macro-boundaries)))
    (when macro-boundaries
      (save-excursion
	(goto-char (car macro-boundaries))
	(forward-char (length TeX-esc))
	(buffer-substring-no-properties
	 (point) (progn (skip-chars-forward "@A-Za-z") (point)))))))

(defun LaTeX-verbatim-p (&optional pos)
  "Return non-nil if position POS is in a verbatim-like construct."
  (when pos (goto-char pos))
  (save-match-data
    (or (when (fboundp 'font-latex-faces-present-p)
	  (font-latex-faces-present-p 'font-latex-verbatim-face))
	(member (LaTeX-current-verbatim-macro)
		(LaTeX-verbatim-macros-with-delims))
	(member (TeX-current-macro) (LaTeX-verbatim-macros-with-braces))
	(member (LaTeX-current-environment) (LaTeX-verbatim-environments)))))


;;; Formatting

(defcustom LaTeX-syntactic-comments t
  "If non-nil comments will be handled according to LaTeX syntax.
This variable influences, among others, the behavior of
indentation and filling which will take LaTeX syntax into
consideration just as is in the non-commented source code."
  :type 'boolean
  :group 'LaTeX)


;;; Indentation

;; We are distinguishing two different types of comments:
;;
;; 1) Comments starting in column one (line comments)
;;
;; 2) Comments starting after column one with only whitespace
;;    preceding it.
;;
;; (There is actually a third type: Comments preceded not only by
;; whitespace but by some code as well; so-called code comments.  But
;; they are not relevant for the following explanations.)
;;
;; Additionally we are distinguishing two different types of
;; indentation:
;;
;; a) Outer indentation: Indentation before the comment character(s).
;;
;; b) Inner indentation: Indentation after the comment character(s)
;;    (taking into account possible comment padding).
;;
;; Comments can be filled syntax-aware or not.
;;
;; In `doctex-mode' line comments should always be indented
;; syntax-aware and the comment character has to be anchored at the
;; first column (unless the appear in a macrocode environment).  Other
;; comments not in the documentation parts always start after the
;; first column and can be indented syntax-aware or not.  If they are
;; indented syntax-aware both the indentation before and after the
;; comment character(s) have to be checked and adjusted.  Indentation
;; should not move the comment character(s) to the first column.  With
;; `LaTeX-syntactic-comments' disabled, line comments should still be
;; indented syntax-aware.
;;
;; In `latex-mode' comments starting in different columns don't have
;; to be handled differently.  They don't have to be anchored in
;; column one.  That means that in any case indentation before and
;; after the comment characters has to be checked and adjusted.

(defgroup LaTeX-indentation nil
  "Indentation of LaTeX code in AUCTeX"
  :group 'LaTeX
  :group 'TeX-indentation)

(defcustom LaTeX-indent-level 2
  "*Indentation of begin-end blocks in LaTeX."
  :group 'LaTeX-indentation
  :type 'integer)

(defcustom LaTeX-item-indent (- LaTeX-indent-level)
  "*Extra indentation for lines beginning with an item."
  :group 'LaTeX-indentation
  :type 'integer)

(defcustom LaTeX-item-regexp "\\(bib\\)?item\\b"
  "*Regular expression matching macros considered items."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-indent-environment-list
  '(("verbatim" current-indentation)
    ("verbatim*" current-indentation)
    ("tabular" LaTeX-indent-tabular)
    ("tabular*" LaTeX-indent-tabular)
    ("align" LaTeX-indent-tabular)
    ("align*" LaTeX-indent-tabular)
    ("array" LaTeX-indent-tabular)
    ("eqnarray" LaTeX-indent-tabular)
    ("eqnarray*" LaTeX-indent-tabular)
    ;; The following should have their own, smart indentation function.
    ;; Some other day.
    ("displaymath")
    ("equation")
    ("equation*")
    ("picture")
    ("tabbing")
    ("table")
    ("table*"))
    "Alist of environments with special indentation.
The second element in each entry is the function to calculate the
indentation level in columns."
    :group 'LaTeX-indentation
    :type '(repeat (list (string :tag "Environment")
			 (option function))))

(defcustom LaTeX-indent-environment-check t
  "*If non-nil, check for any special environments."
  :group 'LaTeX-indentation
  :type 'boolean)

(defcustom LaTeX-document-regexp "document"
  "Regexp matching environments in which the indentation starts at col 0."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-verbatim-regexp "verbatim\\*?"
  "*Regexp matching environments with indentation at col 0 for begin/end."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-begin-regexp "begin\\b"
  "*Regexp matching macros considered begins."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-end-regexp "end\\b"
  "*Regexp matching macros considered ends."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-left-right-indent-level LaTeX-indent-level
  "*The level of indentation produced by a \\left macro."
  :group 'LaTeX-indentation
  :type 'integer)

(defcustom LaTeX-indent-comment-start-regexp "%"
  "*Regexp matching comments ending the indent level count.
This means, we just count the LaTeX tokens \\left, \\right, \\begin,
and \\end up to the first occurence of text matching this regexp.
Thus, the default \"%\" stops counting the tokens at a comment.  A
value of \"%[^>]\" would allow you to alter the indentation with
comments, e.g. with comment `%> \\begin'.
Lines which start with `%' are not considered at all, regardless if this
value."
  :group 'LaTeX-indentation
  :type 'regexp)

(defvar docTeX-indent-inner-fixed
  `((,(concat (regexp-quote TeX-esc)
	     "\\(begin\\|end\\)[ \t]*{macrocode\\*?}") 4 t)
    (,(concat (regexp-quote TeX-esc)
	     "\\(begin\\|end\\)[ \t]*{\\(macro\\|environment\\)\\*?}") 0 nil))
  "List of items which should have a fixed inner indentation.
The items consist of three parts.  The first is a regular
expression which should match the respective string.  The second
is the amount of spaces to be used for indentation.  The third
toggles if comment padding is relevant or not.  If t padding is
part of the amount given, if nil the amount of spaces will be
inserted after potential padding.")

(defun LaTeX-indent-line ()
  "Indent the line containing point, as LaTeX source.
Add `LaTeX-indent-level' indentation in each \\begin{ - \\end{ block.
Lines starting with an item is given an extra indentation of
`LaTeX-item-indent'."
  (interactive)
  (let* ((case-fold-search nil)
	 ;; Compute a fill prefix.  Whitespace after the comment
	 ;; characters will be disregarded and replaced by
	 ;; `comment-padding'.
	 (fill-prefix
	  (and (TeX-in-commented-line)
	       (save-excursion
		 (beginning-of-line)
		 (looking-at
		  (concat "\\([ \t]*" TeX-comment-start-regexp "+\\)+"))
		 (concat (match-string 0) (TeX-comment-padding-string)))))
	 (overlays (when (featurep 'xemacs)
		     ;; Isn't that fun?  In Emacs an `(overlays-at
		     ;; (line-beginning-position))' would do the
		     ;; trick.  How boring.
		     (extent-list
		      nil (line-beginning-position) (line-beginning-position)
		      'all-extents-closed-open 'overlay)))
	 ol-specs)
    ;; XEmacs' `indent-to' function (at least in version 21.4.15) has
    ;; a bug which leads to the insertion of whitespace in front of an
    ;; invisible overlay.  So during indentation we temporarily remove
    ;; the 'invisible property.
    (dolist (ol overlays)
      (when (extent-property ol 'invisible)
	(add-to-list 'ol-specs (list ol (extent-property ol 'invisible)))
	(set-extent-property ol 'invisible nil)))
    (save-excursion
      (cond ((and fill-prefix
		  (TeX-in-line-comment)
		  (eq major-mode 'doctex-mode))
	     ;; If point is in a line comment in `doctex-mode' we only
	     ;; consider the inner indentation.
	     (let ((inner-indent (LaTeX-indent-calculate 'inner)))
	       (when (/= (LaTeX-current-indentation 'inner) inner-indent)
		 (LaTeX-indent-inner-do inner-indent))))
	    ((and fill-prefix
		  LaTeX-syntactic-comments)
	     ;; In any other case of a comment we have to consider
	     ;; outer and inner indentation if we do syntax-aware
	     ;; indentation.
	     (let ((inner-indent (LaTeX-indent-calculate 'inner))
		   (outer-indent (LaTeX-indent-calculate 'outer)))
	       (when (/= (LaTeX-current-indentation 'inner) inner-indent)
		   (LaTeX-indent-inner-do inner-indent))
	       (when (/= (LaTeX-current-indentation 'outer) outer-indent)
		   (LaTeX-indent-outer-do outer-indent))))
	    (t
	     ;; The default is to adapt whitespace before any
	     ;; non-whitespace character, i.e. to do outer
	     ;; indentation.
	     (let ((outer-indent (LaTeX-indent-calculate 'outer)))
	       (when (/= (LaTeX-current-indentation 'outer) outer-indent)
		   (LaTeX-indent-outer-do outer-indent))))))
    ;; Make the overlays invisible again.
    (dolist (ol-spec ol-specs)
      (set-extent-property (car ol-spec) 'invisible (cadr ol-spec)))
    (when (< (current-column) (save-excursion
				(LaTeX-back-to-indentation) (current-column)))
      (LaTeX-back-to-indentation))))

(defun LaTeX-indent-inner-do (inner-indent)
  ;; Small helper function for `LaTeX-indent-line' to perform
  ;; indentation after a comment character.  It requires that
  ;; `LaTeX-indent-line' already set the appropriate variables and
  ;; should not be used outside of `LaTeX-indent-line'.
  (move-to-left-margin)
  (TeX-re-search-forward-unescaped
   (concat "\\(" TeX-comment-start-regexp "+[ \t]*\\)+") (line-end-position) t)
  (delete-region (line-beginning-position) (point))
  (insert fill-prefix)
  (indent-to (+ inner-indent (length fill-prefix))))

(defun LaTeX-indent-outer-do (outer-indent)
  ;; Small helper function for `LaTeX-indent-line' to perform
  ;; indentation of normal lines or before a comment character in a
  ;; commented line.  It requires that `LaTeX-indent-line' already set
  ;; the appropriate variables and should not be used outside of
  ;; `LaTeX-indent-line'.
  (back-to-indentation)
  (delete-region (line-beginning-position) (point))
  (indent-to outer-indent))

(defun LaTeX-indent-calculate (&optional force-type)
  "Return the indentation of a line of LaTeX source.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
  (save-excursion
    (LaTeX-back-to-indentation force-type)
    (let ((i 0)
	  (list-length (safe-length docTeX-indent-inner-fixed))
	  (case-fold-search nil)
	  entry
	  found)
      (cond ((save-excursion (beginning-of-line) (bobp)) 0)
	    ((and (eq major-mode 'doctex-mode)
		  fill-prefix
		  (TeX-in-line-comment)
		  (progn
		    (while (and (< i list-length)
				(not found))
		      (setq entry (nth i docTeX-indent-inner-fixed))
		      (when (looking-at (nth 0 entry))
			(setq found t))
		      (setq i (1+ i)))
		    found))
	     (if (nth 2 entry)
		 (- (nth 1 entry) (if (integerp comment-padding)
				      comment-padding
				    (length comment-padding)))
	       (nth 1 entry)))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "\\(begin\\|end\\){\\("
				 LaTeX-verbatim-regexp
				 "\\)}"))
	     ;; \end{verbatim} must be flush left, otherwise an unwanted
	     ;; empty line appears in LaTeX's output.
	     0)
	    ((and LaTeX-indent-environment-check
		  ;; Special environments.
		  (let ((entry (assoc (or LaTeX-current-environment
					  (LaTeX-current-environment))
				      LaTeX-indent-environment-list)))
		    (and entry
			 (nth 1 entry)
			 (funcall (nth 1 entry))))))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "\\("
				 LaTeX-end-regexp
				 "\\)"))
	     ;; Backindent at \end.
	     (- (LaTeX-indent-calculate-last force-type) LaTeX-indent-level))
	    ((looking-at (concat (regexp-quote TeX-esc) "right\\b"))
	     ;; Backindent at \right.
	     (- (LaTeX-indent-calculate-last force-type)
		LaTeX-left-right-indent-level))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "\\("
				 LaTeX-item-regexp
				 "\\)"))
	     ;; Items.
	     (+ (LaTeX-indent-calculate-last force-type) LaTeX-item-indent))
	    ((looking-at "}")
	     ;; End brace in the start of the line.
	     (- (LaTeX-indent-calculate-last force-type)
		TeX-brace-indent-level))
	    (t (LaTeX-indent-calculate-last force-type))))))

(defun LaTeX-indent-level-count ()
  "Count indentation change caused by all \\left, \\right, \\begin, and
\\end commands in the current line."
  (save-excursion
    (save-restriction
      (let ((count 0))
	(narrow-to-region (point)
			  (save-excursion
			    (re-search-forward
			     (concat "[^" TeX-esc "]"
				     "\\(" LaTeX-indent-comment-start-regexp
				     "\\)\\|\n\\|\\'"))
			    (backward-char)
			    (point)))
	(while (search-forward TeX-esc nil t)
	  (cond
	   ((looking-at "left\\b")
	    (setq count (+ count LaTeX-left-right-indent-level)))
	   ((looking-at "right\\b")
	    (setq count (- count LaTeX-left-right-indent-level)))
	   ((looking-at LaTeX-begin-regexp)
	    (setq count (+ count LaTeX-indent-level)))
	   ((looking-at LaTeX-end-regexp)
	    (setq count (- count LaTeX-indent-level)))
	   ((looking-at (regexp-quote TeX-esc))
	    (forward-char 1))))
	count))))

(defun LaTeX-indent-calculate-last (&optional force-type)
  "Return the correct indentation of a normal line of text.
The point is supposed to be at the beginning of the current line.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
  (let (line-comment-current-flag
	line-comment-last-flag
	comment-current-flag
	comment-last-flag)
    (beginning-of-line)
    (setq line-comment-current-flag (TeX-in-line-comment)
	  comment-current-flag (TeX-in-commented-line))
    (if comment-current-flag
	(skip-chars-backward "%\n\t ")
      (skip-chars-backward "\n\t "))
    (beginning-of-line)
    ;; If we are called in a non-comment line, skip over comment
    ;; lines.  The computation of indentation should in this case
    ;; rather take the last non-comment line into account.
    ;; Otherwise there might arise problems with e.g. multi-line
    ;; code comments.  This behavior is not enabled in docTeX mode
    ;; where large amounts of line comments may have to be skipped
    ;; and indentation should not be influenced by unrelated code in
    ;; other macrocode environments.
    (while (and (not (eq major-mode 'doctex-mode))
		(not comment-current-flag)
		(TeX-in-commented-line)
		(not (bobp)))
      (skip-chars-backward "\n\t ")
      (beginning-of-line))
    (setq line-comment-last-flag (TeX-in-line-comment)
	  comment-last-flag (TeX-in-commented-line))
    (LaTeX-back-to-indentation force-type)
    ;; Separate line comments and other stuff (normal text/code and
    ;; code comments).  Additionally we don't want to compute inner
    ;; indentation when a commented and a non-commented line are
    ;; compared.
    (cond ((or (and (eq major-mode 'doctex-mode)
		    (or (and line-comment-current-flag
			     (not line-comment-last-flag))
			(and (not line-comment-current-flag)
			     line-comment-last-flag)))
	       (and force-type
		    (eq force-type 'inner)
		    (or (and comment-current-flag
			     (not comment-last-flag))
			(and (not comment-current-flag)
			     comment-last-flag))))
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "begin *{\\("
			       LaTeX-document-regexp
			       "\\)}"))
	   ;; I dislike having all of the document indented...
	   (+ (LaTeX-current-indentation force-type)
	      ;; Some people have opening braces at the end of the
	      ;; line, e.g. in case of `\begin{letter}{%'.
	      (TeX-brace-count-line)))
	  ((and (eq major-mode 'doctex-mode)
		(looking-at (concat (regexp-quote TeX-esc)
				    "end[ \t]*{macrocode\\*?}"))
		fill-prefix
		(TeX-in-line-comment))
	   ;; Reset indentation to zero after a macrocode
	   ;; environment.
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "begin *{\\("
			       LaTeX-verbatim-regexp
			       "\\)}"))
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "end *{\\("
			       LaTeX-verbatim-regexp
			       "\\)}"))
	   ;; If I see an \end{verbatim} in the previous line I skip
	   ;; back to the preceding \begin{verbatim}.
	   (save-excursion
	     (if (re-search-backward (concat (regexp-quote TeX-esc)
					     "begin *{\\("
					     LaTeX-verbatim-regexp
					     "\\)}") 0 t)
		 (LaTeX-indent-calculate-last force-type)
	       0)))
	  (t (+ (LaTeX-current-indentation force-type)
		(if (not (and force-type
			      (eq force-type 'outer)
			      (TeX-in-commented-line)))
		    (+ (LaTeX-indent-level-count)
		       (TeX-brace-count-line))
		  0)
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   "\\("
					   LaTeX-end-regexp
					   "\\)"))
		       LaTeX-indent-level)
		      ((looking-at
			(concat (regexp-quote TeX-esc) "right\\b"))
		       LaTeX-left-right-indent-level)
		      ((looking-at (concat (regexp-quote TeX-esc)
					   "\\("
					   LaTeX-item-regexp
					   "\\)"))
		       (- LaTeX-item-indent))
		      ((looking-at "}")
		       TeX-brace-indent-level)
		      (t 0)))))))

(defun LaTeX-current-indentation (&optional force-type)
  "Return the indentation of a line.
FORCE-TYPE can be used to force the calculation of an inner or
outer indentation in case of a commented line.  The symbols
'inner and 'outer are recognized."
  (if (and fill-prefix
	   (or (and force-type
		    (eq force-type 'inner))
	       (and (not force-type)
		    (or
		     ;; If `LaTeX-syntactic-comments' is not enabled,
		     ;; do conventional indentation
		     LaTeX-syntactic-comments
		     ;; Line comments in `doctex-mode' are always
		     ;; indented syntax-aware so we need their inner
		     ;; indentation.
		     (and (TeX-in-line-comment)
			  (eq major-mode 'doctex-mode))))))
      ;; INNER indentation
      (save-excursion
	(beginning-of-line)
	(looking-at (concat "\\(?:[ \t]*" TeX-comment-start-regexp "+\\)+"
			    "\\([ \t]*\\)"))
	(- (length (match-string 1)) (length (TeX-comment-padding-string))))
    ;; OUTER indentation
    (current-indentation)))

(defun LaTeX-back-to-indentation (&optional force-type)
  "Move point to the first non-whitespace character on this line.
If it is commented and comments are formatted syntax-aware move
point to the first non-whitespace character after the comment
character(s).  The optional argument FORCE-TYPE can be used to
force point being moved to the inner or outer indentation in case
of a commented line.  The symbols 'inner and 'outer are
recognized."
  (if (or (and force-type
	       (eq force-type 'inner))
	  (and (not force-type)
	       (or (and (TeX-in-line-comment)
			(eq major-mode 'doctex-mode))
		   (and (TeX-in-commented-line)
			LaTeX-syntactic-comments))))
      (progn
	(beginning-of-line)
	;; Should this be anchored at the start of the line?
	(TeX-re-search-forward-unescaped
	 (concat "\\(?:" TeX-comment-start-regexp "+[ \t]*\\)+")
	 (line-end-position) t))
    (back-to-indentation)))


;;; Filling

(defcustom LaTeX-fill-break-at-separators '(\\\( \\\) \\\[ \\\])
  "List of separators before or after which respectively a line
break will be inserted if they do not fit into one line."
  :group 'LaTeX
  :type '(set :tag "Contents"
	      (const :tag "Opening Brace" \{)
	      (const :tag "Closing Brace" \})
	      (const :tag "Opening Bracket" \[)
	      (const :tag "Opening Inline Math Switches" \\\()
	      (const :tag "Closing Inline Math Switches" \\\))
	      (const :tag "Opening Display Math Switch" \\\[)
	      (const :tag "Closing Display Math Switch" \\\])))

(defcustom LaTeX-fill-break-before-code-comments t
  "If non-nil, a line with some code followed by a comment will
be broken before the last non-comment word in case the comment
does not fit into the line."
  :group 'LaTeX
  :type 'boolean)

(defvar LaTeX-nospace-between-char-regexp
  (if (featurep 'xemacs)
    (if (and (boundp 'word-across-newline) word-across-newline)
	word-across-newline
      ;; NOTE: Ensure not to have a value of nil for such a rare case that
      ;; somebody removes the mule test in `LaTeX-fill-delete-newlines' so that
      ;; it could match only "\n" and this could lead to problem.  XEmacs does
      ;; not have a category `\c|' and `\ct' means `Chinese Taiwan' in XEmacs.
      "\\(\\cj\\|\\cc\\|\\ct\\)")
    "\\c|")
  "Regexp matching a character where no interword space is necessary.
Words formed by such characters can be broken across newlines.")

(defvar LaTeX-fill-newline-hook nil
  "Hook run after `LaTeX-fill-newline' inserted and indented a new line.")

(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph.
Break lines to fit `fill-column', but leave all lines ending with
\\\\ \(plus its optional argument) alone.  Lines with code
comments and lines ending with `\par' are included in filling but
act as boundaries.  Prefix arg means justify too.  From program,
pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (let ((end-marker (save-excursion (goto-char to) (point-marker))))
    (if (or (assoc (LaTeX-current-environment) LaTeX-indent-environment-list)
	    ;; This could be generalized, if there are more cases where
	    ;; a special string at the start of a region to fill should
	    ;; inhibit filling.
	    (progn (save-excursion (goto-char from)
				   (looking-at (concat TeX-comment-start-regexp
						       "+[ \t]*"
						       "Local Variables:")))))
	;; Filling disabled, only do indentation.
	(indent-region from to nil)
      (save-restriction
	(goto-char from)
	(while (< (point) end-marker)
	  (if (re-search-forward
	       (concat "\\("
		       ;; Code comments.
		       "[^\r\n%\\]\\([ \t]\\|\\\\\\\\\\)*"
		       TeX-comment-start-regexp
		       "\\|"
		       ;; Lines ending with `\par'.
		       "\\(\\=\\|[^" TeX-esc "\n]\\)\\("
		       (regexp-quote (concat TeX-esc TeX-esc))
		       "\\)*"
		       (regexp-quote TeX-esc) "par[ \t]*"
		       "\\({[ \t]*}\\)?[ \t]*$"
		       "\\)\\|\\("
		       ;; Lines ending with `\\'.
		       (regexp-quote TeX-esc)
		       (regexp-quote TeX-esc)
		       "\\(\\s-*\\*\\)?"
		       "\\(\\s-*\\[[^]]*\\]\\)?"
		       "\\s-*$\\)")
	       end-marker t)
	      (progn
		(goto-char (line-end-position))
		(delete-horizontal-space)
		;; I doubt very much if we want justify -
		;; this is a line with \\
		;; if you think otherwise - uncomment the next line
		;; (and justify-flag (justify-current-line))
		(forward-char)
		;; keep our position in a buffer
		(save-excursion
		  ;; Code comments and lines ending with `\par' are
		  ;; included in filling.  Lines ending with `\\' are
		  ;; skipped.
		  (if (match-string 1)
		      (LaTeX-fill-region-as-para-do from (point) justify-flag)
		    (LaTeX-fill-region-as-para-do
		     from (line-beginning-position 0) justify-flag)
		    ;; At least indent the line ending with `\\'.
		    (indent-according-to-mode)))
		(setq from (point)))
	    ;; ELSE part follows - loop termination relies on a fact
	    ;; that (LaTeX-fill-region-as-para-do) moves point past
	    ;; the filled region
	    (LaTeX-fill-region-as-para-do from end-marker justify-flag)))))))

;; The content of `LaTeX-fill-region-as-para-do' was copied from the
;; function `fill-region-as-paragraph' in `fill.el' (CVS Emacs,
;; January 2004) and adapted to the needs of AUCTeX.

(defun LaTeX-fill-region-as-para-do (from to &optional justify
					  nosqueeze squeeze-after)
  "Fill the region defined by FROM and TO as one paragraph.
It removes any paragraph breaks in the region and extra newlines at the end,
indents and fills lines between the margins given by the
`current-left-margin' and `current-fill-column' functions.
\(In most cases, the variable `fill-column' controls the width.)
It leaves point at the beginning of the line following the paragraph.

Normally performs justification according to the `current-justification'
function, but with a prefix arg, does full justification instead.

From a program, optional third arg JUSTIFY can specify any type of
justification.  Fourth arg NOSQUEEZE non-nil means not to make spaces
between words canonical before filling.  Fifth arg SQUEEZE-AFTER, if non-nil,
means don't canonicalize spaces before that position.

Return the `fill-prefix' used for filling.

If `sentence-end-double-space' is non-nil, then period followed by one
space does not end a sentence, so don't break a line there."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (region-beginning) (region-end)
		       (if current-prefix-arg 'full))))
  (unless (memq justify '(t nil none full center left right))
    (setq justify 'full))

  ;; Make sure "to" is the endpoint.
  (goto-char (min from to))
  (setq to   (max from to))
  ;; Ignore blank lines at beginning of region.
  (skip-chars-forward " \t\n")

  (let ((from-plus-indent (point))
	(oneleft nil))

    (beginning-of-line)
    (setq from (point))

    ;; Delete all but one soft newline at end of region.
    ;; And leave TO before that one.
    (goto-char to)
    (while (and (> (point) from) (eq ?\n (char-after (1- (point)))))
      (if (and oneleft
	       (not (and use-hard-newlines
			 (get-text-property (1- (point)) 'hard))))
	  (delete-char -1)
	(backward-char 1)
	(setq oneleft t)))
    (setq to (copy-marker (point) t))
    (goto-char from-plus-indent))

  (if (not (> to (point)))
      nil ;; There is no paragraph, only whitespace: exit now.

    (or justify (setq justify (current-justification)))

    ;; Don't let Adaptive Fill mode alter the fill prefix permanently.
    (let ((fill-prefix fill-prefix))
      ;; Figure out how this paragraph is indented, if desired.
      (when (and adaptive-fill-mode
		 (or (null fill-prefix) (string= fill-prefix "")))
	(setq fill-prefix (fill-context-prefix from to))
	;; Ignore a white-space only fill-prefix
	;; if we indent-according-to-mode.
	(when (and fill-prefix fill-indent-according-to-mode
		   (string-match "\\`[ \t]*\\'" fill-prefix))
	  (setq fill-prefix nil)))

      (goto-char from)
      (beginning-of-line)

      (if (not justify)	  ; filling disabled: just check indentation
	  (progn
	    (goto-char from)
	    (while (< (point) to)
	      (if (and (not (eolp))
		       (< (LaTeX-current-indentation) (current-left-margin)))
		  (fill-indent-to-left-margin))
	      (forward-line 1)))

	(when use-hard-newlines
	  (remove-text-properties from to '(hard nil)))
	;; Make sure first line is indented (at least) to left margin...
	(indent-according-to-mode)
	;; COMPATIBILITY for Emacs <= 21.1
	(if (fboundp 'fill-delete-prefix)
	    ;; Delete the fill-prefix from every line.
	    (fill-delete-prefix from to fill-prefix)
	  ;; Delete the comment prefix and any whitespace from every
	  ;; line of the region in concern except the first. (The
	  ;; implementation is heuristic to a certain degree.)
	  (save-excursion
	    (goto-char from)
	    (forward-line 1)
	    (when (< (point) to)
	      (while (re-search-forward (concat "^[ \t]+\\|^[ \t]*"
						TeX-comment-start-regexp
						"+[ \t]*") to t)
		(delete-region (match-beginning 0) (match-end 0))))))

	(setq from (point))

	;; FROM, and point, are now before the text to fill,
	;; but after any fill prefix on the first line.

	(LaTeX-fill-delete-newlines from to justify nosqueeze squeeze-after)

	;; This is the actual FILLING LOOP.
	(goto-char from)
	(let* (linebeg
	       (code-comment-start (save-excursion
				     (LaTeX-back-to-indentation)
				     (TeX-search-forward-comment-start
				      (line-end-position))))
	       (end-marker (save-excursion
			     (goto-char (or code-comment-start to))
			     (point-marker)))
	       (LaTeX-current-environment (LaTeX-current-environment)))
	  ;; Fill until point is greater than the end point.  If there
	  ;; is a code comment, use the code comment's start as a
	  ;; limit.
	  (while (and (< (point) (marker-position end-marker))
		      (or (not code-comment-start)
			  (and code-comment-start
			       (> (- (marker-position end-marker)
				     (line-beginning-position))
				  fill-column))))
	    (setq linebeg (point))
	    (move-to-column (current-fill-column))
	    (if (when (< (point) (marker-position end-marker))
		  ;; Find the position where we'll break the line.
		  (forward-char 1)	; Use an immediately following
					; space, if any.
		  (LaTeX-fill-move-to-break-point linebeg)

		  ;; Check again to see if we got to the end of
		  ;; the paragraph.
		  (skip-chars-forward " \t")
		  (< (point) (marker-position end-marker)))
		;; Found a place to cut.
		(progn
		  (LaTeX-fill-newline)
		  (when justify
		    ;; Justify the line just ended, if desired.
		    (save-excursion
		      (forward-line -1)
		      (justify-current-line justify nil t))))

	      (goto-char end-marker)
	      ;; Justify this last line, if desired.
	      (if justify (justify-current-line justify t t))))

	  ;; Fill a code comment if necessary.  (Enable this code if
	  ;; you want the comment part in lines with code comments to
	  ;; be filled.  Originally it was disabled because the
	  ;; indentation code indented the lines following the line
	  ;; with the code comment to the column of the comment
	  ;; starters.  That means, it would have looked like this:
	  ;; | code code code % comment
	  ;; |                % comment
	  ;; |                code code code
	  ;; This now (2005-07-29) is not the case anymore.  But as
	  ;; filling code comments like this would split a single
	  ;; paragraph into two separate ones, we still leave it
	  ;; disabled.  I leave the code here in case it is useful for
	  ;; somebody.
	  ;; (when (and code-comment-start
	  ;;            (> (- (line-end-position) (line-beginning-position))
	  ;;                  fill-column))
	  ;;   (LaTeX-fill-code-comment justify))

	  ;; The following is an alternative strategy to minimize the
	  ;; occurence of overfull lines with code comments.  A line
	  ;; will be broken before the last non-comment word if the
	  ;; code comment does not fit into the line.
	  (when (and LaTeX-fill-break-before-code-comments
		     code-comment-start
		     (> (- (line-end-position) (line-beginning-position))
			fill-column))
	    (beginning-of-line)
	    (goto-char end-marker)
	    (while (not (looking-at TeX-comment-start-regexp)) (forward-char))
	    (skip-chars-backward " \t")
	    (skip-chars-backward "^ \t\n")
	    (unless (or (bolp)
			;; Comment starters and whitespace.
			(TeX-looking-at-backward
			 (concat "^\\([ \t]*" TeX-comment-start-regexp "+\\)*"
				 "[ \t]*")
			 (line-beginning-position)))
	      (LaTeX-fill-newline)))))
      ;; Leave point after final newline.
      (goto-char to)
      (unless (eobp) (forward-char 1))
      ;; Return the fill-prefix we used
      fill-prefix)))

;; Following lines are copied from `fill.el' (CVS Emacs, March 2005).
;;   The `fill-space' property carries the string with which a newline should be
;;   replaced when unbreaking a line (in fill-delete-newlines).  It is added to
;;   newline characters by fill-newline when the default behavior of
;;   fill-delete-newlines is not what we want.
(unless (featurep 'xemacs)
  ;; COMPATIBILITY for Emacs < 22.1
  (add-to-list 'text-property-default-nonsticky '(fill-space . t)))

(defun LaTeX-fill-delete-newlines (from to justify nosqueeze squeeze-after)
  ;; COMPATIBILITY for Emacs < 22.1 and XEmacs
  (if (fboundp 'fill-delete-newlines)
      (fill-delete-newlines from to justify nosqueeze squeeze-after)
    (if (featurep 'xemacs)
	(when (featurep 'mule)
	  (goto-char from)
	  (let ((unwished-newline (concat LaTeX-nospace-between-char-regexp "\n"
					  LaTeX-nospace-between-char-regexp)))
	    (while (re-search-forward unwished-newline to t)
	      (skip-chars-backward "^\n")
	      (delete-char -1))))
      ;; This else-sentence was copied from the function `fill-delete-newlines'
      ;; in `fill.el' (CVS Emacs, 2005-02-17) and adapted accordingly.
      (while (search-forward "\n" to t)
  	(if (get-text-property (match-beginning 0) 'fill-space)
  	    (replace-match (get-text-property (match-beginning 0) 'fill-space))
	  (let ((prev (char-before (match-beginning 0)))
		(next (following-char)))
	    (when (or (aref (char-category-set next) ?|)
		      (aref (char-category-set prev) ?|))
	      (delete-char -1))))))

    ;; Make sure sentences ending at end of line get an extra space.
    (if (or (not (boundp 'sentence-end-double-space))
	    sentence-end-double-space)
	(progn
	  (goto-char from)
	  (while (re-search-forward "[.?!][]})\"']*$" to t)
	    (insert ? ))))
    ;; Then change all newlines to spaces.
    (let ((point-max (progn
		       (goto-char to)
		       (skip-chars-backward "\n")
		       (point))))
      (subst-char-in-region from point-max ?\n ?\ ))
    (goto-char from)
    (skip-chars-forward " \t")
    ;; Remove extra spaces between words.
    (unless (and nosqueeze (not (eq justify 'full)))
      (canonically-space-region (or squeeze-after (point)) to)
      ;; Remove trailing whitespace.
      (goto-char (line-end-position))
      (delete-char (- (skip-chars-backward " \t"))))))

(defun LaTeX-fill-move-to-break-point (linebeg)
  "Move to the position where the line should be broken."
  ;; COMPATIBILITY for Emacs < 22.1 and XEmacs
  (if (fboundp 'fill-move-to-break-point)
      (fill-move-to-break-point linebeg)
    (if (featurep 'mule)
 	(if (TeX-looking-at-backward
	     (concat LaTeX-nospace-between-char-regexp ".?") 2)
 	    ;; Cancel `forward-char' which is called just before
 	    ;; `LaTeX-fill-move-to-break-point' if the char before point matches
 	    ;; `LaTeX-nospace-between-char-regexp'.
 	    (backward-char 1)
 	  (when (re-search-backward
		 (concat " \\|\n\\|" LaTeX-nospace-between-char-regexp)
		 linebeg 'move)
	    (forward-char 1)))
      (skip-chars-backward "^ \n"))
    ;; Prevent infinite loops: If we cannot find a place to break
    ;; while searching backward, search forward again.
    (when (save-excursion
	    (skip-chars-backward " \t%")
	    (bolp))
      (skip-chars-forward "^ \n" (point-max)))
    ;; This code was copied from the function `fill-move-to-break-point'
    ;; in `fill.el' (CVS Emacs, 2005-02-22) and adapted accordingly.
    (when (and (< linebeg (point))
	       ;; If we are going to break the line after or
	       ;; before a non-ascii character, we may have to
	       ;; run a special function for the charset of the
	       ;; character to find the correct break point.
	       (boundp 'enable-multibyte-characters)
	       enable-multibyte-characters
	       (fboundp 'charset-after) ; Non-MULE XEmacsen don't have this.
	       (not (and (eq (charset-after (1- (point))) 'ascii)
		         (eq (charset-after (point)) 'ascii))))
      ;; Make sure we take SOMETHING after the fill prefix if any.
      (if (fboundp 'fill-find-break-point)
	  (fill-find-break-point linebeg)
	(when (fboundp 'kinsoku-process) ;XEmacs
	  (kinsoku-process)))))
  ;; Prevent line break between 2-byte char and 1-byte char.
  (when (and (featurep 'mule)
	     enable-multibyte-characters
	     (or (and (not (looking-at LaTeX-nospace-between-char-regexp))
		      (TeX-looking-at-backward
		       LaTeX-nospace-between-char-regexp 1))
		 (and (not (TeX-looking-at-backward
			    LaTeX-nospace-between-char-regexp 1))
		      (looking-at LaTeX-nospace-between-char-regexp)))
	     (re-search-backward
	      (concat LaTeX-nospace-between-char-regexp
		      LaTeX-nospace-between-char-regexp
		      LaTeX-nospace-between-char-regexp
		      "\\|"
		      ".\\ca\\s +\\ca") linebeg t))
    (if (looking-at "..\\c>")
	(forward-char 1)
      (forward-char 2)))
  ;; Cater for Japanese Macro
  (when (and (boundp 'japanese-TeX-mode) japanese-TeX-mode
	     (aref (char-category-set (char-after)) ?j)
	     (TeX-looking-at-backward (concat (regexp-quote TeX-esc) TeX-token-char "*")
				      (1- (- (point) linebeg)))
	     (not (TeX-escaped-p (match-beginning 0))))
      (goto-char (match-beginning 0)))
  ;; Cater for \verb|...| (and similar) contructs which should not be
  ;; broken. (FIXME: Make it work with shortvrb.sty (also loaded by
  ;; doc.sty) where |...| is allowed.  Arbitrary delimiters may be
  ;; chosen with \MakeShortVerb{<char>}.)  This could probably be
  ;; handled with `fill-nobreak-predicate', but this is not available
  ;; in XEmacs.
  (let ((final-breakpoint (point))
	(verb-macros (regexp-opt (append (LaTeX-verbatim-macros-with-delims)
					 (LaTeX-verbatim-macros-with-braces)))))
    (save-excursion
      ;; Look for the start of a verbatim macro in the current line.
      (when (re-search-backward (concat (regexp-quote TeX-esc)
					"\\(?:" verb-macros "\\)\\([^a-z@*]\\)")
				(line-beginning-position) t)
	;; Determine start and end of verbatim macro.
	(let ((beg (point))
	      (end (if (not (string-match "[ [{]" (match-string 1)))
		       (cdr (LaTeX-verbatim-macro-boundaries))
		     (TeX-find-macro-end))))
	  ;; Determine if macro end is behind fill column.
	  (when (and end
		     (> (- end (line-beginning-position))
			(current-fill-column))
		     (> end final-breakpoint))
	    ;; Search backwards for place to break before the macro.
	    (goto-char beg)
	    (skip-chars-backward "^ \n")
	    ;; Determine if point ended up at the beginning of the line.
	    (when (save-excursion (skip-chars-backward " \t%") (bolp))
	      ;; Search forward for a place to break after the macro.
	      (goto-char end)
	      (skip-chars-forward "^ \n" (point-max)))
	    (setq final-breakpoint (point))))))
    (goto-char final-breakpoint))
  (when LaTeX-fill-break-at-separators
    (let ((orig-breakpoint (point))
	  (final-breakpoint (point))
	  start-point
	  math-sep)
      (save-excursion
	(beginning-of-line)
	(LaTeX-back-to-indentation)
	(setq start-point (point))
	;; Find occurences of [, $, {, }, \(, \), \[, \] or $$.
	(while (and (= final-breakpoint orig-breakpoint)
		    (TeX-re-search-forward-unescaped
		     (concat "[[{}]\\|\\$\\$?\\|"
			     (regexp-quote TeX-esc) "[][()]")
		     orig-breakpoint t))
	  (let ((match-string (match-string 0)))
	    (cond
	     ;; [ (opening bracket) (The closing bracket should
	     ;; already be handled implicitely by the code for the
	     ;; opening brace.)
	     ((save-excursion
		(and (memq '\[ LaTeX-fill-break-at-separators)
		     (string= match-string "[")
		     (TeX-re-search-forward-unescaped (concat "\\][ \t]*{")
						      (line-end-position) t)
		     (> (- (or (TeX-find-closing-brace)
			       (line-end-position))
			   (line-beginning-position))
			fill-column)))
	      (save-excursion
		(skip-chars-backward "^ \n")
		(when (> (point) start-point)
		  (setq final-breakpoint (point)))))
	     ;; { (opening brace)
	     ((save-excursion
		(and (memq '\{ LaTeX-fill-break-at-separators)
		     (string= match-string "{")
		     (> (- (save-excursion
			     ;; `TeX-find-closing-brace' is not enough
			     ;; if there is no breakpoint in form of
			     ;; whitespace after the brace.
			     (goto-char (or (TeX-find-closing-brace)
					    (line-end-position)))
			     (skip-chars-forward "^ \t\n")
			     (point))
			   (line-beginning-position))
			fill-column)))
	      (save-excursion
		(skip-chars-backward "^ \n")
		;; The following is a primitive and error-prone method
		;; to cope with point probably being inside square
		;; brackets.  A better way would be to use functions
		;; to determine if point is inside an optional
		;; argument and to jump to the start and end brackets.
		(when (save-excursion
			(TeX-re-search-forward-unescaped
			 (concat "\\][ \t]*{") orig-breakpoint t))
		  (TeX-search-backward-unescaped "["
						 (line-beginning-position) t)
		  (skip-chars-backward "^ \n"))
		(when (> (point) start-point)
		  (setq final-breakpoint (point)))))
	     ;; } (closing brace)
	     ((save-excursion
		(and (memq '\} LaTeX-fill-break-at-separators)
		     (string= match-string "}")
		     (save-excursion
		       (backward-char 2)
		       (not (TeX-find-opening-brace
			     nil (line-beginning-position))))))
	      (save-excursion
		(skip-chars-forward "^ \n")
		(when (> (point) start-point)
		  (setq final-breakpoint (point)))))
	     ;; $ or \( or \[ or $$ (opening math)
	     ((save-excursion
		(and (or (and (memq '\\\( LaTeX-fill-break-at-separators)
			      (or (and (string= match-string "$")
				       (texmathp))
				  (string= match-string "\\(")))
			 (and (memq '\\\[ LaTeX-fill-break-at-separators)
			      (or (string= match-string "\\[")
				  (and (string= match-string "$$")
				       (texmathp)))))
		     (> (- (save-excursion
			     (TeX-search-forward-unescaped
			      (cond ((string= match-string "\\(")
				     (concat TeX-esc ")"))
				    ((string= match-string "$") "$")
				    ((string= match-string "$$") "$$")
				    (t (concat TeX-esc "]")))
			      (point-max) t)
			     (skip-chars-forward "^ \n")
			     (point))
			   (line-beginning-position))
			fill-column)))
	      (save-excursion
		(skip-chars-backward "^ \n")
		(when (> (point) start-point)
		  (setq final-breakpoint (point)))))
	     ;; $ or \) or \] or $$ (closing math)
	     ((save-excursion
		(and (or (and (memq '\\\) LaTeX-fill-break-at-separators)
			      (or (and (string= match-string "$")
				       (not (texmathp)))
				  (string= match-string "\\)")))
			 (and (memq '\\\] LaTeX-fill-break-at-separators)
			      (or (string= match-string "\\]")
				  (and (string= match-string "$$")
				       (not (texmathp))))))
		     (if (member match-string '("$" "$$"))
			 (save-excursion
			   (skip-chars-backward "$")
			   (not (TeX-search-backward-unescaped
				 match-string (line-beginning-position) t)))
		       (texmathp-match-switch (line-beginning-position)))))
	      (save-excursion
		(skip-chars-forward "^ \n")
		(when (> (point) start-point)
		  (setq final-breakpoint (point)))))))))
      (goto-char final-breakpoint))))

;; The content of `LaTeX-fill-newline' was copied from the function
;; `fill-newline' in `fill.el' (CVS Emacs, January 2004) and adapted
;; to the needs of AUCTeX.
(defun LaTeX-fill-newline ()
  "Replace whitespace here with one newline and indent the line."
  (skip-chars-backward " \t")
  (newline 1)
  ;; COMPATIBILITY for XEmacs
  (unless (featurep 'xemacs)
    ;; Give newline the properties of the space(s) it replaces
    (set-text-properties (1- (point)) (point)
			 (text-properties-at (point)))
    (and (looking-at "\\( [ \t]*\\)\\(\\c|\\)?")
	 (or (aref (char-category-set (or (char-before (1- (point))) ?\000)) ?|)
	     (match-end 2))
	 ;; When refilling later on, this newline would normally not
	 ;; be replaced by a space, so we need to mark it specially to
	 ;; re-install the space when we unfill.
	 (put-text-property (1- (point)) (point) 'fill-space (match-string 1)))
    ;; COMPATIBILITY for Emacs <= 21.3
    (when (boundp 'fill-nobreak-invisible)
      ;; If we don't want breaks in invisible text, don't insert
      ;; an invisible newline.
      (if fill-nobreak-invisible
	  (remove-text-properties (1- (point)) (point)
				  '(invisible t)))))
  ;; Insert the fill prefix.
  (and fill-prefix (not (equal fill-prefix ""))
       ;; Markers that were after the whitespace are now at point: insert
       ;; before them so they don't get stuck before the prefix.
       (insert-before-markers-and-inherit fill-prefix))
  (indent-according-to-mode)
  (run-hooks 'LaTeX-fill-newline-hook))

(defun LaTeX-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but handle LaTeX comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in.  Code comments, i.e. comments
with uncommented code preceding them in the same line, will not
be filled unless the cursor is placed on the line with the
code comment.

If LaTeX syntax is taken into consideration during filling
depends on the value of `LaTeX-syntactic-comments'."
  (interactive "P")
  (if (save-excursion
	(beginning-of-line)
	(looking-at (concat TeX-comment-start-regexp "*[ \t]*$")))
      ;; Don't do anything if we look at an empty line and let
      ;; `fill-paragraph' think we successfully filled the paragraph.
      t
    (let (;; Non-nil if the current line contains a comment.
	  has-comment
	  ;; Non-nil if the current line contains code and a comment.
	  has-code-and-comment
	  code-comment-start
	  ;; If has-comment, the appropriate fill-prefix for the comment.
	  comment-fill-prefix)

      ;; Figure out what kind of comment we are looking at.
      (cond
       ;; A line only with potential whitespace followed by a
       ;; comment on it?
       ((save-excursion
	  (beginning-of-line)
	  (looking-at (concat "^[ \t]*" TeX-comment-start-regexp
			      "\\(" TeX-comment-start-regexp "\\|[ \t]\\)*")))
	(setq has-comment t
	      comment-fill-prefix (TeX-match-buffer 0)))
       ;; A line with some code, followed by a comment?
       ((and (setq code-comment-start (save-excursion
					(beginning-of-line)
					(TeX-search-forward-comment-start
					 (line-end-position))))
	     (> (point) code-comment-start)
	     (not (TeX-in-commented-line))
	     (save-excursion
	       (goto-char code-comment-start)
	       ;; See if there is at least one non-whitespace character
	       ;; before the comment starts.
	       (re-search-backward "[^ \t\n]" (line-beginning-position) t)))
	(setq has-comment t
	      has-code-and-comment t)))

      (cond
       ;; Code comments.
       (has-code-and-comment
	(save-excursion
	  (when (>= (- code-comment-start (line-beginning-position))
		    fill-column)
	    ;; If start of code comment is beyond fill column, fill it as a
	    ;; regular paragraph before it is filled as a code comment.
	    (let ((end-marker (save-excursion (end-of-line) (point-marker))))
	      (LaTeX-fill-region-as-paragraph (line-beginning-position)
					      (line-beginning-position 2)
					      justify)
	      (goto-char end-marker)
	      (beginning-of-line)))
	  (LaTeX-fill-code-comment justify)))
       ;; Syntax-aware filling:
       ;; * `LaTeX-syntactic-comments' enabled: Everything.
       ;; * `LaTeX-syntactic-comments' disabled: Uncommented code and
       ;;   line comments in `doctex-mode'.
       ((or (or LaTeX-syntactic-comments
		(and (not LaTeX-syntactic-comments)
		     (not has-comment)))
	    (and (eq major-mode 'doctex-mode)
		 (TeX-in-line-comment)))
	(let ((fill-prefix comment-fill-prefix))
	  (save-excursion
	    (let* ((end (progn (LaTeX-forward-paragraph)
			       (or (bolp) (newline 1))
			       (and (eobp) (not (bolp)) (open-line 1))
			       (point)))
		   (start
		    (progn
		      (LaTeX-backward-paragraph)
		      (while (and (looking-at
				   (concat "$\\|[ \t]+$\\|"
					   "[ \t]*" TeX-comment-start-regexp
					   "+[ \t]*$"))
				  (< (point) end))
			(forward-line))
		      (point))))
	      (LaTeX-fill-region-as-paragraph start end justify)))))
	;; Non-syntax-aware filling.
       (t
	(save-excursion
	  (save-restriction
	    (beginning-of-line)
	    (narrow-to-region
	     ;; Find the first line we should include in the region to fill.
	     (save-excursion
	       (while (and (zerop (forward-line -1))
			   (looking-at (concat "^[ \t]*"
					       TeX-comment-start-regexp))))
	       ;; We may have gone too far.  Go forward again.
	       (or (looking-at (concat ".*" TeX-comment-start-regexp))
		   (forward-line 1))
	       (point))
	     ;; Find the beginning of the first line past the region to fill.
	     (save-excursion
	       (while (progn (forward-line 1)
			     (looking-at (concat "^[ \t]*"
						 TeX-comment-start-regexp))))
	       (point)))
	    ;; The definitions of `paragraph-start' and
	    ;; `paragraph-separate' will still make
	    ;; `forward-paragraph' and `backward-paragraph' stop at
	    ;; the respective (La)TeX commands.  If these should be
	    ;; disregarded, the definitions would have to be changed
	    ;; accordingly.  (Lines with only `%' characters on them
	    ;; can be paragraph boundaries.)
	    (let* ((paragraph-start
		    (concat paragraph-start "\\|"
			    "\\(" TeX-comment-start-regexp "\\|[ \t]\\)*$"))
		   (paragraph-separate
		    (concat paragraph-separate "\\|"
			    "\\(" TeX-comment-start-regexp "\\|[ \t]\\)*$"))
		   (fill-prefix comment-fill-prefix)
		   (end (progn (forward-paragraph)
			       (or (bolp) (newline 1))
			       (point)))
		   (beg (progn (backward-paragraph)
			       (point))))
	      (fill-region-as-paragraph
	       beg end
	       justify nil
	       (save-excursion
		 (goto-char beg)
		 (if (looking-at fill-prefix)
		     nil
		   (re-search-forward comment-start-skip nil t)
		   (point)))))))))
      t)))

(defun LaTeX-fill-code-comment (&optional justify-flag)
  "Fill a line including code followed by a comment."
  (let ((beg (line-beginning-position))
	fill-prefix code-comment-start)
    (indent-according-to-mode)
    (when (when (setq code-comment-start (save-excursion
					   (goto-char beg)
					   (TeX-search-forward-comment-start
					    (line-end-position))))
	    (goto-char code-comment-start)
	    (while (not (looking-at TeX-comment-start-regexp)) (forward-char))
	    ;; See if there is at least one non-whitespace character
	    ;; before the comment starts.
	    (save-excursion
	      (re-search-backward "[^ \t\n]" (line-beginning-position) t)))
      (setq fill-prefix
	    (concat
	     (if indent-tabs-mode
		 (concat (make-string (/ (current-column) tab-width) ?\t)
			 (make-string (% (current-column) tab-width) ?\ ))
	       (make-string (current-column) ?\ ))
	     (progn
	       (looking-at (concat TeX-comment-start-regexp "+[ \t]*"))
	       (TeX-match-buffer 0))))
      (fill-region-as-paragraph beg (line-beginning-position 2)
				justify-flag  nil
				(save-excursion
				  (goto-char beg)
				  (if (looking-at fill-prefix)
				      nil
				    (re-search-forward comment-start-skip nil t)
				    (point)))))))

(defun LaTeX-fill-region (from to &optional justify what)
  "Fill and indent the text in region from FROM to TO as LaTeX text.
Prefix arg (non-nil third arg JUSTIFY, if called from program)
means justify as well.  Fourth arg WHAT is a word to be displayed when
formatting."
  (interactive "*r\nP")
  (save-excursion
    (let ((to (set-marker (make-marker) to))
	  (next-par (make-marker)))
      (goto-char from)
      (beginning-of-line)
      (setq from (point))
      (catch 'end-of-buffer
	(while (and (< (point) to))
	  (message "Formatting%s ... %d%%"
		   (or what "")
		   (/ (* 100 (- (point) from)) (- to from)))
	  (save-excursion (LaTeX-fill-paragraph justify))
	  (if (marker-position next-par)
	      (goto-char (marker-position next-par))
	    (LaTeX-forward-paragraph))
	  (when (eobp) (throw 'end-of-buffer t))
	  (LaTeX-forward-paragraph)
	  (set-marker next-par (point))
	  (LaTeX-backward-paragraph)
	  (while (and (not (eobp))
		      (looking-at
		       (concat "^\\($\\|[ \t]+$\\|[ \t]*"
			       TeX-comment-start-regexp "+[ \t]*$\\)")))
	    (forward-line 1))))
      (set-marker to nil)))
  (message "Finished"))

(defun LaTeX-find-matching-end ()
  "Move point to the \\end of the current environment.

If function is called inside a comment and
`LaTeX-syntactic-comments' is enabled, try to find the
environment in commented regions with the same comment prefix."
  (interactive)
  (let* ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
	 (level 1)
	 (in-comment (TeX-in-commented-line))
	 (comment-prefix (and in-comment (TeX-comment-prefix)))
	 (case-fold-search nil))
    (save-excursion
      (skip-chars-backward "a-zA-Z \t{")
      (unless (bolp)
	(backward-char 1)
	(and (looking-at regexp)
	     (char-equal (char-after (1+ (match-beginning 0))) ?e)
	     (setq level 0))))
    (while (and (> level 0) (re-search-forward regexp nil t))
      (when (or (and LaTeX-syntactic-comments
		     (eq in-comment (TeX-in-commented-line))
		     ;; If we are in a commented line, check if the
		     ;; prefix matches the one we started out with.
		     (or (not in-comment)
			 (string= comment-prefix (TeX-comment-prefix))))
		(and (not LaTeX-syntactic-comments)
		     (not (TeX-in-commented-line))))
	(if (= (char-after (1+ (match-beginning 0))) ?b) ;;begin
	    (setq level (1+ level))
	  (setq level (1- level)))))
    (if (= level 0)
	(search-forward "}")
      (error "Can't locate end of current environment"))))

(defun LaTeX-find-matching-begin ()
  "Move point to the \\begin of the current environment.

If function is called inside a comment and
`LaTeX-syntactic-comments' is enabled, try to find the
environment in commented regions with the same comment prefix."
  (interactive)
  (let* ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
	 (level 1)
	 (in-comment (TeX-in-commented-line))
	 (comment-prefix (and in-comment (TeX-comment-prefix)))
	 (case-fold-search nil))
    (skip-chars-backward "a-zA-Z \t{")
    (unless (bolp)
      (backward-char 1)
      (and (looking-at regexp)
	   (char-equal (char-after (1+ (match-beginning 0))) ?b)
	   (setq level 0)))
    (while (and (> level 0) (re-search-backward regexp nil t))
      (when (or (and LaTeX-syntactic-comments
		     (eq in-comment (TeX-in-commented-line))
		     ;; If we are in a commented line, check if the
		     ;; prefix matches the one we started out with.
		     (or (not in-comment)
			 (string= comment-prefix (TeX-comment-prefix))))
		(and (not LaTeX-syntactic-comments)
		     (not (TeX-in-commented-line))))
	(if (= (char-after (1+ (match-beginning 0))) ?e) ;;end
	    (setq level (1+ level))
	  (setq level (1- level)))))
    (or (= level 0)
	(error "Can't locate beginning of current environment"))))

(defun LaTeX-mark-environment (&optional count)
  "Set mark to end of current environment and point to the matching begin.
If prefix argument COUNT is given, mark the respective number of
enclosing environments.  The command will not work properly if
there are unbalanced begin-end pairs in comments and verbatim
environments."
  (interactive "p")
  (setq count (if count (abs count) 1))
  (let ((cur (point)) beg end)
    ;; Only change point and mark after beginning and end were found.
    ;; Point should not end up in the middle of nowhere if the search fails.
    (save-excursion
      (dotimes (c count) (LaTeX-find-matching-end))
      (setq end (line-beginning-position 2))
      (goto-char cur)
      (dotimes (c count) (LaTeX-find-matching-begin))
      (setq beg (point)))
    (push-mark end)
    (goto-char beg)
    (TeX-activate-region)))

(defun LaTeX-fill-environment (justify)
  "Fill and indent current environment as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-mark-environment)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-fill-region (region-beginning) (region-end) justify
		       (concat " environment " (TeX-match-buffer 1)))))

(defun LaTeX-fill-section (justify)
  "Fill and indent current logical section as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-mark-section)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-fill-region (region-beginning) (region-end) justify
		       (concat " section " (TeX-match-buffer 1)))))

(defun LaTeX-mark-section (&optional no-subsections)
  "Set mark at end of current logical section, and point at top.
If optional argument NO-SUBSECTIONS is non-nil, mark only the
region from the current section start to the next sectioning
command.  Thereby subsections are not being marked.

If the function `outline-mark-subtree' is not available,
`LaTeX-mark-section' always behaves like this regardless of the
value of NO-SUBSECTIONS."
  (interactive "P")
  (if (or no-subsections
	  (not (fboundp 'outline-mark-subtree)))
      (progn
	(re-search-forward (concat  "\\(" (LaTeX-outline-regexp)
				    "\\|\\'\\)"))
	(beginning-of-line)
	(push-mark (point) nil t)
	(re-search-backward (concat "\\(" (LaTeX-outline-regexp)
				    "\\|\\`\\)")))
    (outline-mark-subtree)
    (when (and (boundp 'transient-mark-mode)
	       transient-mark-mode
	       (boundp 'mark-active)
	       (not mark-active))
      (setq mark-active t)
      (run-hooks 'activate-mark-hook)))
  (TeX-activate-region))

(defun LaTeX-fill-buffer (justify)
  "Fill and indent current buffer as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-fill-region
     (point-min)
     (point-max)
     justify
     (concat " buffer " (buffer-name)))))


;;; Navigation

(defvar LaTeX-paragraph-commands-internal
  '("[" "]" ; display math
    "appendix" "begin" "caption" "chapter" "end" "include" "includeonly"
    "label" "maketitle" "noindent" "par" "paragraph" "part" "section"
    "subsection" "subsubsection" "tableofcontents" "newpage" "clearpage")
  "Internal list of LaTeX macros that should have their own line.")

(defun LaTeX-paragraph-commands-regexp-make ()
  "Return a regular expression matching defined paragraph commands."
  (concat (regexp-quote TeX-esc) "\\("
	  (regexp-opt (append LaTeX-paragraph-commands
			      LaTeX-paragraph-commands-internal)) "\\)"))

(defcustom LaTeX-paragraph-commands nil
  "List of LaTeX macros that should have their own line.
The list should contain macro names without the leading backslash."
  :group 'LaTeX-macro
  :type '(repeat (string))
  :set (lambda (symbol value)
         (set-default symbol value)
	 (setq LaTeX-paragraph-commands-regexp
	       (LaTeX-paragraph-commands-regexp-make))))

(defvar LaTeX-paragraph-commands-regexp (LaTeX-paragraph-commands-regexp-make)
    "Regular expression matching LaTeX macros that should have their own line.")

(defun LaTeX-set-paragraph-start ()
  "Set `paragraph-start'."
  (setq paragraph-start
	(concat
	 "[ \t]*%*[ \t]*\\("
	 LaTeX-paragraph-commands-regexp "\\|"
	 (regexp-quote TeX-esc) "\\(" LaTeX-item-regexp "\\)\\|"
	 "\\$\\$\\|" ; Plain TeX display math (Some people actually
		     ; use this with LaTeX.  Yuck.)
	 "$\\)")))

(defun LaTeX-paragraph-commands-add-locally (commands)
  "Make COMMANDS be recognized as paragraph commands.
COMMANDS can be a single string or a list of strings which will
be added to `LaTeX-paragraph-commands-internal'.  Additionally
`LaTeX-paragraph-commands-regexp' will be updated and both
variables will be made buffer-local.  This is mainly a
convenience function which can be used in style files."
  (make-local-variable 'LaTeX-paragraph-commands-internal)
  (make-local-variable 'LaTeX-paragraph-commands-regexp)
  (unless (listp commands) (setq commands (list commands)))
  (dolist (elt commands)
    (add-to-list 'LaTeX-paragraph-commands-internal elt))
  (setq LaTeX-paragraph-commands-regexp (LaTeX-paragraph-commands-regexp-make))
  (LaTeX-set-paragraph-start))

(defun LaTeX-forward-paragraph (&optional count)
  "Move forward to end of paragraph.
If COUNT is non-nil, do it COUNT times."
  (or count (setq count 1))
  (dotimes (i count)
    (let* ((macro-start (TeX-find-macro-start))
	   (paragraph-command-start
	    (cond
	     ;; Point is inside of a paragraph command.
	     ((and macro-start
		   (save-excursion
		     (goto-char macro-start)
		     (looking-at LaTeX-paragraph-commands-regexp)))
	      (match-beginning 0))
	     ;; Point is before a paragraph command in the same line.
	     ((looking-at
	       (concat "[ \t]*\\(?:" TeX-comment-start-regexp
		       "\\(?:" TeX-comment-start-regexp "\\|[ \t]\\)*\\)?"
		       "\\(" LaTeX-paragraph-commands-regexp "\\)"))
	      (match-beginning 1))))
	   macro-end)
      ;; If a paragraph command is encountered there are two cases to be
      ;; distinguished:
      ;; 1) If the end of the paragraph command coincides (apart from
      ;;    potential whitespace) with the end of the line, is only
      ;;    followed by a comment or is directly followed by a macro,
      ;;    it is assumed that it should be handled separately.
      ;; 2) If the end of the paragraph command is followed by other
      ;;    code, it is assumed that it should be included with the rest
      ;;    of the paragraph.
      (if (and paragraph-command-start
	       (save-excursion
		 (goto-char paragraph-command-start)
		 (setq macro-end (goto-char (TeX-find-macro-end)))
		 (looking-at (concat (regexp-quote TeX-esc) "[@A-Za-z]+\\|"
				     "[ \t]*\\($\\|"
				     TeX-comment-start-regexp "\\)"))))
	  (progn
	    (goto-char macro-end)
	    ;; If the paragraph command is followed directly by
	    ;; another macro, regard the latter as part of the
	    ;; paragraph command's paragraph.
	    (when (looking-at (concat (regexp-quote TeX-esc) "[@A-Za-z]+"))
	      (goto-char (TeX-find-macro-end)))
	    (forward-line))
	(let (limit)
	  (goto-char (min (save-excursion
			    (forward-paragraph)
			    (setq limit (point)))
			  (save-excursion
			    (TeX-forward-comment-skip 1 limit)
			    (point)))))))))

(defun LaTeX-backward-paragraph (&optional count)
  "Move backward to beginning of paragraph.
If COUNT is non-nil, do it COUNT times."
  (or count (setq count 1))
  (dotimes (i count)
    (let* ((macro-start (TeX-find-macro-start)))
      (if (and macro-start
	       ;; Point really has to be inside of the macro, not before it.
	       (not (= macro-start (point)))
	       (save-excursion
		 (goto-char macro-start)
		 (looking-at LaTeX-paragraph-commands-regexp)))
	  ;; Point is inside of a paragraph command.
	  (progn
	    (goto-char macro-start)
	    (beginning-of-line))
	(let (limit
	      (start (line-beginning-position)))
	  (goto-char
	   (max (save-excursion
		  (backward-paragraph)
		  (setq limit (point)))
		;; Search for possible transitions from commented to
		;; uncommented regions and vice versa.
		(save-excursion
		  (TeX-backward-comment-skip 1 limit)
		  (point))
		;; Search for paragraph commands.
		(save-excursion
		  (let ((end-point 0) macro-bol)
		    (when (setq macro-bol
				(re-search-backward
				 (format "^[ \t]*%s*[ \t]*\\(%s\\)"
					 TeX-comment-start-regexp
					 LaTeX-paragraph-commands-regexp)
				 limit t))
		      (if (and (string= (match-string 1) "\\begin")
			       (progn
				 (goto-char (match-end 1))
				 (skip-chars-forward "{ \t")
				 (member (buffer-substring-no-properties
					  (point) (progn (skip-chars-forward
							  "A-Za-z*") (point)))
					 LaTeX-verbatim-environments)))
			  ;; If inside a verbatim environment, just
			  ;; use the next line.  In such environments
			  ;; `TeX-find-macro-end' could otherwise
			  ;; think brackets or braces belong to the
			  ;; \begin macro.
			  (setq end-point (line-beginning-position 2))
			;; Jump to the macro end otherwise.
			(goto-char (match-beginning 1))
			(goto-char (TeX-find-macro-end))
			;; For an explanation of this distinction see
			;; `LaTeX-forward-paragraph'.
			(if (looking-at (concat (regexp-quote TeX-esc)
						"[@A-Za-z]+\\|[ \t]*\\($\\|"
						TeX-comment-start-regexp "\\)"))
			    (progn
			      (when (string= (buffer-substring-no-properties
					      (point) (+ (point)
							 (length TeX-esc)))
					     TeX-esc)
				(goto-char (TeX-find-macro-end)))
			      (forward-line 1)
			      (when (< (point) start)
				(setq end-point (point))))
			  (setq end-point macro-bol))))
		    end-point)))))))))

(defun LaTeX-search-forward-comment-start (&optional limit)
  "Search forward for a comment start from current position till LIMIT.
If LIMIT is omitted, search till the end of the buffer.

This function makes sure that any comment starters found inside
of verbatim constructs are not considered."
  (setq limit (or limit (point-max)))
  (save-excursion
    (let (start)
      (catch 'found
	(while (progn
		 (when (and (TeX-re-search-forward-unescaped
			     TeX-comment-start-regexp limit 'move)
			    (not (LaTeX-verbatim-p)))
		   (setq start (match-beginning 0))
		   (throw 'found t))
		 (< (point) limit))))
      start)))


;;; Math Minor Mode

(defgroup LaTeX-math nil
  "Mathematics in AUCTeX."
  :group 'LaTeX-macro)

(defvar LaTeX-math-keymap (make-sparse-keymap)
  "Keymap used for `LaTeX-math-mode' commands.")

(defun LaTeX-math-abbrev-prefix ()
  "Make a key definition from the variable `LaTeX-math-abbrev-prefix'."
  (if (stringp LaTeX-math-abbrev-prefix)
      (read-kbd-macro LaTeX-math-abbrev-prefix)
    LaTeX-math-abbrev-prefix))

(defvar LaTeX-math-menu
  '("Math"
    ("Greek Uppercase") ("Greek Lowercase") ("Binary Op") ("Relational")
    ("Arrows") ("Punctuation") ("Misc Symbol") ("Var Symbol") ("Log-like")
    ("Delimiters") ("Constructs") ("Accents") ("AMS"))
  "Menu containing LaTeX math commands.
The menu entries will be generated dynamically, but you can specify
the sequence by initializing this variable.")

(defcustom LaTeX-math-menu-unicode
  (or (string-match "\\<GTK\\>" (emacs-version))
      (eq window-system 'w32))
  "Whether the LaTeX menu should try using Unicode for effect."
  :type 'boolean
  :group 'LaTeX-math)

(defcustom LaTeX-math-abbrev-prefix "`"
  "Prefix key for use in `LaTeX-math-mode'.
This has to be a string representing a key sequence in a format
understood by the `kbd' macro.  This corresponds to the syntax
usually used in the Emacs and Elisp manuals.

Setting this variable directly does not take effect;
use \\[customize]."
  :group 'LaTeX-math
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
	  (define-key LaTeX-math-mode-map (LaTeX-math-abbrev-prefix) t)
	  (set-default symbol value)
	  (define-key LaTeX-math-mode-map
	    (LaTeX-math-abbrev-prefix) LaTeX-math-keymap))
  :type '(string :tag "Key sequence"))

(defun LaTeX-math-initialize ()
  (let ((math (reverse (append LaTeX-math-list LaTeX-math-default)))
	(map LaTeX-math-keymap)
	(unicode (and LaTeX-math-menu-unicode (fboundp 'decode-char))))
    (while math
      (let* ((entry (car math))
	     (key (nth 0 entry))
	     (prefix
	      (and unicode
		   (nth 3 entry)))
	     value menu name)
	(setq math (cdr math))
	(if (and prefix
		 (setq prefix (decode-char 'ucs (nth 3 entry))))
	    (setq prefix (concat (string prefix) " \\"))
	  (setq prefix "\\"))
	(if (listp (cdr entry))
	    (setq value (nth 1 entry)
		  menu (nth 2 entry))
	  (setq value (cdr entry)
		menu nil))
	(if (stringp value)
	    (progn
	      (setq name (intern (concat "LaTeX-math-" value)))
	      (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
			       (list 'LaTeX-math-insert value 'arg))))
	  (setq name value))
	(if key
	    (progn
	      (setq key (cond ((numberp key) (char-to-string key))
			      ((stringp key) (read-kbd-macro key))
			      (t (vector key))))
	      (define-key map key name)))
	(if menu
	    (let ((parent LaTeX-math-menu))
	      (if (listp menu)
		  (progn
		    (while (cdr menu)
		      (let ((sub (assoc (car menu) LaTeX-math-menu)))
			(if sub
			    (setq parent sub)
			  (setcdr parent (cons (list (car menu)) (cdr parent))))
			(setq menu (cdr menu))))
		    (setq menu (car menu))))
	      (let ((sub (assoc menu parent)))
		(if sub
		    (if (stringp value)
			(setcdr sub (cons (vector (concat prefix value)
						  name t)
					  (cdr sub)))
		      (error "Cannot have multiple special math menu items"))
		  (setcdr parent
			  (cons (if (stringp value)
				    (list menu (vector (concat prefix value)
						       name t))
				  (vector menu name t))
				(cdr parent)))))))))
    ;; Make the math prefix char available if it has not been used as a prefix.
    (unless (lookup-key map (LaTeX-math-abbrev-prefix))
      (define-key map (LaTeX-math-abbrev-prefix) 'self-insert-command))))

(defconst LaTeX-dialect :latex
  "Default dialect for use with function `TeX-add-style-hook' for
argument DIALECT-EXPR when the hook is to be run only on LaTeX
file, or any mode derived thereof. See variable
`TeX-style-hook-dialect'." )

(defconst LaTeX-math-default
  '((?a "alpha" "Greek Lowercase" 945) ;; #X03B1
    (?b "beta" "Greek Lowercase" 946) ;; #X03B2
    (?g "gamma" "Greek Lowercase" 947) ;; #X03B3
    (?d "delta" "Greek Lowercase" 948) ;; #X03B4
    (?e "epsilon" "Greek Lowercase" 1013) ;; #X03F5
    (?z "zeta" "Greek Lowercase" 950) ;; #X03B6
    (?h "eta" "Greek Lowercase" 951) ;; #X03B7
    (?j "theta" "Greek Lowercase" 952) ;; #X03B8
    (nil "iota" "Greek Lowercase" 953) ;; #X03B9
    (?k "kappa" "Greek Lowercase" 954) ;; #X03BA
    (?l "lambda" "Greek Lowercase" 955) ;; #X03BB
    (?m "mu" "Greek Lowercase" 956) ;; #X03BC
    (?n "nu" "Greek Lowercase" 957) ;; #X03BD
    (?x "xi" "Greek Lowercase" 958) ;; #X03BE
    (?p "pi" "Greek Lowercase" 960) ;; #X03C0
    (?r "rho" "Greek Lowercase" 961) ;; #X03C1
    (?s "sigma" "Greek Lowercase" 963) ;; #X03C3
    (?t "tau" "Greek Lowercase" 964) ;; #X03C4
    (?u "upsilon" "Greek Lowercase" 965) ;; #X03C5
    (?f "phi" "Greek Lowercase" 981) ;; #X03D5
    (?q "chi" "Greek Lowercase" 967) ;; #X03C7
    (?y "psi" "Greek Lowercase" 968) ;; #X03C8
    (?w "omega" "Greek Lowercase" 969) ;; #X03C9
    ("v e" "varepsilon" "Greek Lowercase" 949) ;; #X03B5
    ("v j" "vartheta" "Greek Lowercase" 977) ;; #X03D1
    ("v p" "varpi" "Greek Lowercase" 982) ;; #X03D6
    ("v r" "varrho" "Greek Lowercase" 1009) ;; #X03F1
    ("v s" "varsigma" "Greek Lowercase" 962) ;; #X03C2
    ("v f" "varphi" "Greek Lowercase" 966) ;; #X03C6
    (?G "Gamma" "Greek Uppercase" 915) ;; #X0393
    (?D "Delta" "Greek Uppercase" 916) ;; #X0394
    (?J "Theta" "Greek Uppercase" 920) ;; #X0398
    (?L "Lambda" "Greek Uppercase" 923) ;; #X039B
    (?X "Xi" "Greek Uppercase" 926) ;; #X039E
    (?P "Pi" "Greek Uppercase" 928) ;; #X03A0
    (?S "Sigma" "Greek Uppercase" 931) ;; #X03A3
    (?U "Upsilon" "Greek Uppercase" 978) ;; #X03D2
    (?F "Phi" "Greek Uppercase" 934) ;; #X03A6
    (?Y "Psi" "Greek Uppercase" 936) ;; #X03A8
    (?W "Omega" "Greek Uppercase" 937) ;; #X03A9
    (?c LaTeX-math-cal "Cal-whatever")
    (nil "pm" "Binary Op" 177) ;; #X00B1
    (nil "mp" "Binary Op" 8723) ;; #X2213
    (?* "times" "Binary Op" 215) ;; #X00D7
    (nil "div" "Binary Op" 247) ;; #X00F7
    (nil "ast" "Binary Op" 8727) ;; #X2217
    (nil "star" "Binary Op" 8902) ;; #X22C6
    (nil "circ" "Binary Op" 8728) ;; #X2218
    (nil "bullet" "Binary Op" 8729) ;; #X2219
    (?. "cdot" "Binary Op" 8901) ;; #X22C5
    (?- "cap" "Binary Op" 8745) ;; #X2229
    (?+ "cup" "Binary Op" 8746) ;; #X222A
    (nil "uplus" "Binary Op" 8846) ;; #X228E
    (nil "sqcap" "Binary Op" 8851) ;; #X2293
    (?| "vee" "Binary Op" 8744) ;; #X2228
    (?& "wedge" "Binary Op" 8743) ;; #X2227
    (?\\ "setminus" "Binary Op" 8726) ;; #X2216
    (nil "wr" "Binary Op" 8768) ;; #X2240
    (nil "diamond" "Binary Op" 8900) ;; #X22C4
    (nil "bigtriangleup" "Binary Op" 9651) ;; #X25B3
    (nil "bigtriangledown" "Binary Op" 9661) ;; #X25BD
    (nil "triangleleft" "Binary Op" 9665) ;; #X25C1
    (nil "triangleright" "Binary Op" 9655) ;; #X25B7
    (nil "lhd" "Binary Op" 8882) ;; #X22B2
    (nil "rhd" "Binary Op" 8883) ;; #X22B3
    (nil "unlhd" "Binary Op" 8884) ;; #X22B4
    (nil "unrhd" "Binary Op" 8885) ;; #X22B5
    (nil "oplus" "Binary Op" 8853) ;; #X2295
    (nil "ominus" "Binary Op" 8854) ;; #X2296
    (nil "otimes" "Binary Op" 8855) ;; #X2297
    (nil "oslash" "Binary Op" 8709) ;; #X2205
    (nil "odot" "Binary Op" 8857) ;; #X2299
    (nil "bigcirc" "Binary Op" 9675) ;; #X25CB
    (nil "dagger" "Binary Op" 8224) ;; #X2020
    (nil "ddagger" "Binary Op" 8225) ;; #X2021
    (nil "amalg" "Binary Op" 10815) ;; #X2A3F
    (?< "leq" "Relational" 8804) ;; #X2264
    (?> "geq" "Relational" 8805) ;; #X2265
    (nil "qed" "Relational" 8718) ;; #X220E
    (nil "equiv" "Relational" 8801) ;; #X2261
    (nil "models" "Relational" 8871) ;; #X22A7
    (nil "prec" "Relational" 8826) ;; #X227A
    (nil "succ" "Relational" 8827) ;; #X227B
    (nil "sim" "Relational" 8764) ;; #X223C
    (nil "perp" "Relational" 10178) ;; #X27C2
    (nil "preceq" "Relational" 10927) ;; #X2AAF
    (nil "succeq" "Relational" 10928) ;; #X2AB0
    (nil "simeq" "Relational" 8771) ;; #X2243
    (nil "mid" "Relational" 8739) ;; #X2223
    (nil "ll" "Relational" 8810) ;; #X226A
    (nil "gg" "Relational" 8811) ;; #X226B
    (nil "asymp" "Relational" 8781) ;; #X224D
    (nil "parallel" "Relational" 8741) ;; #X2225
    (?\{ "subset" "Relational" 8834) ;; #X2282
    (?\} "supset" "Relational" 8835) ;; #X2283
    (nil "approx" "Relational" 8776) ;; #X2248
    (nil "bowtie" "Relational" 8904) ;; #X22C8
    (?\[ "subseteq" "Relational" 8838) ;; #X2286
    (?\] "supseteq" "Relational" 8839) ;; #X2287
    (nil "cong" "Relational" 8773) ;; #X2245
    (nil "Join" "Relational" 10781) ;; #X2A1D
    (nil "sqsubset" "Relational" 8847) ;; #X228F
    (nil "sqsupset" "Relational" 8848) ;; #X2290
    (nil "neq" "Relational" 8800) ;; #X2260
    (nil "smile" "Relational" 8995) ;; #X2323
    (nil "sqsubseteq" "Relational" 8849) ;; #X2291
    (nil "sqsupseteq" "Relational" 8850) ;; #X2292
    (nil "doteq" "Relational" 8784) ;; #X2250
    (nil "frown" "Relational" 8994) ;; #X2322
    (?i "in" "Relational" 8712) ;; #X2208
    (nil "ni" "Relational" 8715) ;; #X220B
    (nil "propto" "Relational" 8733) ;; #X221D
    (nil "vdash" "Relational" 8866) ;; #X22A2
    (nil "dashv" "Relational" 8867) ;; #X22A3
    (?\C-b "leftarrow" "Arrows" 8592) ;; #X2190
    (nil "Leftarrow" "Arrows" 8656) ;; #X21D0
    (?\C-f "rightarrow" "Arrows" 8594) ;; #X2192
    (nil "Rightarrow" "Arrows" 8658) ;; #X21D2
    (nil "leftrightarrow" "Arrows" 8596) ;; #X2194
    (nil "Leftrightarrow" "Arrows" 8660) ;; #X21D4
    (nil "mapsto" "Arrows" 8614) ;; #X21A6
    (nil "hookleftarrow" "Arrows" 8617) ;; #X21A9
    (nil "leftharpoonup" "Arrows" 8636) ;; #X21BC
    (nil "leftharpoondown" "Arrows" 8637) ;; #X21BD
    (nil "longleftarrow" "Arrows" 10229) ;; #X27F5
    (nil "Longleftarrow" "Arrows" 10232) ;; #X27F8
    (nil "longrightarrow" "Arrows" 10230) ;; #X27F6
    (nil "Longrightarrow" "Arrows" 10233) ;; #X27F9
    (nil "longleftrightarrow" "Arrows" 10231) ;; #X27F7
    (nil "Longleftrightarrow" "Arrows" 10234) ;; #X27FA
    (nil "longmapsto" "Arrows" 10236) ;; #X27FC
    (nil "hookrightarrow" "Arrows" 8618) ;; #X21AA
    (nil "rightharpoonup" "Arrows" 8640) ;; #X21C0
    (nil "rightharpoondown" "Arrows" 8641) ;; #X21C1
    (?\C-p "uparrow" "Arrows" 8593) ;; #X2191
    (nil "Uparrow" "Arrows" 8657) ;; #X21D1
    (?\C-n "downarrow" "Arrows" 8595) ;; #X2193
    (nil "Downarrow" "Arrows" 8659) ;; #X21D3
    (nil "updownarrow" "Arrows" 8597) ;; #X2195
    (nil "Updownarrow" "Arrows" 8661) ;; #X21D5
    (nil "nearrow" "Arrows" 8599) ;; #X2197
    (nil "searrow" "Arrows" 8600) ;; #X2198
    (nil "swarrow" "Arrows" 8601) ;; #X2199
    (nil "nwarrow" "Arrows" 8598) ;; #X2196
    (nil "ldots" "Punctuation" 8230) ;; #X2026
    (nil "cdots" "Punctuation" 8943) ;; #X22EF
    (nil "vdots" "Punctuation" 8942) ;; #X22EE
    (nil "ddots" "Punctuation" 8945) ;; #X22F1
    (?: "colon" "Punctuation" 58) ;; #X003A
    (?N "nabla" "Misc Symbol" 8711) ;; #X2207
    (nil "aleph" "Misc Symbol" 8501) ;; #X2135
    (nil "prime" "Misc Symbol" 8242) ;; #X2032
    (?A "forall" "Misc Symbol" 8704) ;; #X2200
    (?I "infty" "Misc Symbol" 8734) ;; #X221E
    (nil "hbar" "Misc Symbol" 8463) ;; #X210F
    (?0 "emptyset" "Misc Symbol" 8709) ;; #X2205
    (?E "exists" "Misc Symbol" 8707) ;; #X2203
    (nil "surd" "Misc Symbol" 8730) ;; #X221A
    (nil "Box" "Misc Symbol" 9633) ;; #X25A1
    (nil "triangle" "Misc Symbol" 9651) ;; #X25B3
    (nil "Diamond" "Misc Symbol" 9671) ;; #X25C7
    (nil "imath" "Misc Symbol" 120484) ;; #X1D6A4
    (nil "jmath" "Misc Symbol" 120485) ;; #X1D6A5
    (nil "ell" "Misc Symbol" 8467) ;; #X2113
    (nil "neg" "Misc Symbol" 172) ;; #X00AC
    (?/ "not" "Misc Symbol" 824) ;; #X0338
    (nil "top" "Misc Symbol" 8868) ;; #X22A4
    (nil "flat" "Misc Symbol" 9837) ;; #X266D
    (nil "natural" "Misc Symbol" 9838) ;; #X266E
    (nil "sharp" "Misc Symbol" 9839) ;; #X266F
    (nil "wp" "Misc Symbol" 8472) ;; #X2118
    (nil "bot" "Misc Symbol" 8869) ;; #X22A5
    (nil "clubsuit" "Misc Symbol" 9827) ;; #X2663
    (nil "diamondsuit" "Misc Symbol" 9826) ;; #X2662
    (nil "heartsuit" "Misc Symbol" 9825) ;; #X2661
    (nil "spadesuit" "Misc Symbol" 9824) ;; #X2660
    (nil "mho" "Misc Symbol" 8487) ;; #X2127
    (nil "Re" "Misc Symbol" 8476) ;; #X211C
    (nil "Im" "Misc Symbol" 8465) ;; #X2111
    (nil "angle" "Misc Symbol" 8736) ;; #X2220
    (nil "partial" "Misc Symbol" 8706) ;; #X2202
    (nil "sum" "Var Symbol" 8721) ;; #X2211
    (nil "prod" "Var Symbol" 8719) ;; #X220F
    (nil "coprod" "Var Symbol" 8720) ;; #X2210
    (nil "int" "Var Symbol" 8747) ;; #X222B
    (nil "oint" "Var Symbol" 8750) ;; #X222E
    (nil "bigcap" "Var Symbol" 8898) ;; #X22C2
    (nil "bigcup" "Var Symbol" 8899) ;; #X22C3
    (nil "bigsqcup" "Var Symbol" 10758) ;; #X2A06
    (nil "bigvee" "Var Symbol" 8897) ;; #X22C1
    (nil "bigwedge" "Var Symbol" 8896) ;; #X22C0
    (nil "bigodot" "Var Symbol" 10752) ;; #X2A00
    (nil "bigotimes" "Var Symbol" 10754) ;; #X2A02
    (nil "bigoplus" "Var Symbol" 10753) ;; #X2A01
    (nil "biguplus" "Var Symbol" 10756) ;; #X2A04
    (nil "arccos" "Log-like")
    (nil "arcsin" "Log-like")
    (nil "arctan" "Log-like")
    (nil "arg" "Log-like")
    (?\C-c "cos" "Log-like")
    (nil "cosh" "Log-like")
    (nil "cot" "Log-like")
    (nil "coth" "Log-like")
    (nil "csc" "Log-like")
    (nil "deg" "Log-like")
    (?\C-d "det" "Log-like")
    (nil "dim" "Log-like")
    (?\C-e "exp" "Log-like")
    (nil "gcd" "Log-like")
    (nil "hom" "Log-like")
    (?\C-_ "inf" "Log-like")
    (nil "ker" "Log-like")
    (nil "lg" "Log-like")
    (?\C-l "lim" "Log-like")
    (nil "liminf" "Log-like")
    (nil "limsup" "Log-like")
    (nil "ln" "Log-like")
    (nil "log" "Log-like")
    (nil "max" "Log-like")
    (nil "min" "Log-like")
    (nil "Pr" "Log-like")
    (nil "sec" "Log-like")
    (?\C-s "sin" "Log-like")
    (nil "sinh" "Log-like")
    (?\C-^ "sup" "Log-like")
    (?\C-t "tan" "Log-like")
    (nil "tanh" "Log-like")
    (nil "{" "Delimiters" ?{)
    (nil "}" "Delimiters" ?})
    (nil "lfloor" "Delimiters" 8970) ;; #X230A
    (nil "rfloor" "Delimiters" 8971) ;; #X230B
    (nil "lceil" "Delimiters" 8968) ;; #X2308
    (nil "rceil" "Delimiters" 8969) ;; #X2309
    (?\( "langle" "Delimiters" 10216) ;; #X27E8
    (?\) "rangle" "Delimiters" 10217) ;; #X27E9
    (nil "rmoustache" "Delimiters" 9137) ;; #X23B1
    (nil "lmoustache" "Delimiters" 9136) ;; #X23B0
    (nil "rgroup" "Delimiters" 9133) ;; #X23AD
    (nil "lgroup" "Delimiters" 9129) ;; #X23A9
    (nil "backslash" "Delimiters" 92) ;; #X005C
    (nil "|" "Delimiters" 8214) ;; #X2016)
    (nil "arrowvert" "Delimiters")
    (nil "Arrowvert" "Delimiters")
    (nil "bracevert" "Delimiters")
    (nil "widetilde" "Constructs" 771) ;; #X0303
    (nil "widehat" "Constructs" 770) ;; #X0302
    (nil "overleftarrow" "Constructs" 8406) ;; #X20D6
    (nil "overrightarrow" "Constructs")
    (nil "overline" "Constructs" 773) ;; #X0305
    (nil "underline" "Constructs" 818) ;; #X0332
    (nil "overbrace" "Constructs" 65079) ;; #XFE37
    (nil "underbrace" "Constructs" 65080) ;; #XFE38
    (nil "sqrt" "Constructs" 8730) ;; #X221A
    (nil "frac" "Constructs")
    (?^ "hat" "Accents" 770) ;; #X0302
    (nil "acute" "Accents" 769) ;; #X0301
    (nil "bar" "Accents" 772) ;; #X0304
    (nil "dot" "Accents" 775) ;; #X0307
    (nil "breve" "Accents" 774) ;; #X0306
    (nil "check" "Accents" 780) ;; #X030C
    (nil "grave" "Accents" 768) ;; #X0300
    (nil "vec" "Accents" 8407) ;; #X20D7
    (nil "ddot" "Accents" 776) ;; #X0308
    (?~ "tilde" "Accents" 771) ;; #X0303
    (nil "mathring" "Accents" 778) ;; #X030A
    (nil "beth" ("AMS" "Hebrew") 8502) ;; #X2136
    (nil "daleth" ("AMS" "Hebrew") 8504) ;; #X2138
    (nil "gimel" ("AMS" "Hebrew") 8503) ;; #X2137
    (nil "digamma" ("AMS" "Greek Lowercase") 989) ;; #X03DD
    ("v k" "varkappa" ("AMS" "Greek Lowercase") 1008) ;; #X03F0
    ("v G" "varGamma" ("AMS" "Greek Uppercase") 120548) ;; #X1D6E4
    ("v D" "varDelta" ("AMS" "Greek Uppercase") 120549) ;; #X1D6E5
    ("v J" "varTheta" ("AMS" "Greek Uppercase") 120553) ;; #X1D6E9
    ("v L" "varLambda" ("AMS" "Greek Uppercase") 120556) ;; #X1D6EC
    ("v X" "varXi" ("AMS" "Greek Uppercase") 120559) ;; #X1D6EF
    ("v P" "varPi" ("AMS" "Greek Uppercase") 120561) ;; #X1D6F1
    ("v S" "varSigma" ("AMS" "Greek Uppercase") 120564) ;; #X1D6F4
    ("v U" "varUpsilon" ("AMS" "Greek Uppercase") 120566) ;; #X1D6F6
    ("v F" "varPhi" ("AMS" "Greek Uppercase") 120567) ;; #X1D6F7
    ("v Y" "varPsi" ("AMS" "Greek Uppercase") 120569) ;; #X1D6F9
    ("v W" "varOmega" ("AMS" "Greek Uppercase") 120570) ;; #X1D6FA
    (nil "dashrightarrow" ("AMS" "Arrows"))
    (nil "dashleftarrow" ("AMS" "Arrows"))
    (nil "leftleftarrows" ("AMS" "Arrows") 8647) ;; #X21C7
    (nil "leftrightarrows" ("AMS" "Arrows") 8646) ;; #X21C6
    (nil "Lleftarrow" ("AMS" "Arrows") 8666) ;; #X21DA
    (nil "twoheadleftarrow" ("AMS" "Arrows") 8606) ;; #X219E
    (nil "leftarrowtail" ("AMS" "Arrows") 8610) ;; #X21A2
    (nil "looparrowleft" ("AMS" "Arrows") 8619) ;; #X21AB
    (nil "leftrightharpoons" ("AMS" "Arrows") 8651) ;; #X21CB
    (nil "curvearrowleft" ("AMS" "Arrows") 8630) ;; #X21B6
    (nil "circlearrowleft" ("AMS" "Arrows") 8634) ;; #X21BA
    (nil "Lsh" ("AMS" "Arrows") 8624) ;; #X21B0
    (nil "upuparrows" ("AMS" "Arrows") 8648) ;; #X21C8
    (nil "upharpoonleft" ("AMS" "Arrows") 8639) ;; #X21BF
    (nil "downharpoonleft" ("AMS" "Arrows") 8643) ;; #X21C3
    (nil "multimap" ("AMS" "Arrows") 8888) ;; #X22B8
    (nil "leftrightsquigarrow" ("AMS" "Arrows") 8621) ;; #X21AD
    (nil "looparrowright" ("AMS" "Arrows") 8620) ;; #X21AC
    (nil "rightleftharpoons" ("AMS" "Arrows") 8652) ;; #X21CC
    (nil "curvearrowright" ("AMS" "Arrows") 8631) ;; #X21B7
    (nil "circlearrowright" ("AMS" "Arrows"))
    (nil "Rsh" ("AMS" "Arrows") 8625) ;; #X21B1
    (nil "downdownarrows" ("AMS" "Arrows") 8650) ;; #X21CA
    (nil "upharpoonright" ("AMS" "Arrows") 8638) ;; #X21BE
    (nil "downharpoonright" ("AMS" "Arrows") 8642) ;; #X21C2
    (nil "rightsquigarrow" ("AMS" "Arrows") 8605) ;; #X219D
    (nil "nleftarrow" ("AMS" "Neg Arrows") 8602) ;; #X219A
    (nil "nrightarrow" ("AMS" "Neg Arrows") 8603) ;; #X219B
    (nil "nLeftarrow" ("AMS" "Neg Arrows") 8653) ;; #X21CD
    (nil "nRightarrow" ("AMS" "Neg Arrows") 8655) ;; #X21CF
    (nil "nleftrightarrow" ("AMS" "Neg Arrows") 8622) ;; #X21AE
    (nil "nLeftrightarrow" ("AMS" "Neg Arrows") 8654) ;; #X21CE
    (nil "leqq" ("AMS" "Relational I") 8806) ;; #X2266
    (nil "leqslant" ("AMS" "Relational I") 10877) ;; #X2A7D
    (nil "eqslantless" ("AMS" "Relational I") 10901) ;; #X2A95
    (nil "lesssim" ("AMS" "Relational I") 8818) ;; #X2272
    (nil "lessapprox" ("AMS" "Relational I") 10885) ;; #X2A85
    (nil "approxeq" ("AMS" "Relational I") 8778) ;; #X224A
    (nil "lessdot" ("AMS" "Relational I") 8918) ;; #X22D6
    (nil "lll" ("AMS" "Relational I") 8920) ;; #X22D8
    (nil "lessgtr" ("AMS" "Relational I") 8822) ;; #X2276
    (nil "lesseqgtr" ("AMS" "Relational I") 8922) ;; #X22DA
    (nil "lesseqqgtr" ("AMS" "Relational I") 10891) ;; #X2A8B
    (nil "doteqdot" ("AMS" "Relational I") 8785) ;; #X2251
    (nil "risingdotseq" ("AMS" "Relational I") 8787) ;; #X2253
    (nil "fallingdotseq" ("AMS" "Relational I") 8786) ;; #X2252
    (nil "backsim" ("AMS" "Relational I") 8765) ;; #X223D
    (nil "backsimeq" ("AMS" "Relational I") 8909) ;; #X22CD
    (nil "subseteqq" ("AMS" "Relational I") 10949) ;; #X2AC5
    (nil "Subset" ("AMS" "Relational I") 8912) ;; #X22D0
    (nil "sqsubset" ("AMS" "Relational I") 8847) ;; #X228F
    (nil "preccurlyeq" ("AMS" "Relational I") 8828) ;; #X227C
    (nil "curlyeqprec" ("AMS" "Relational I") 8926) ;; #X22DE
    (nil "precsim" ("AMS" "Relational I") 8830) ;; #X227E
    (nil "precapprox" ("AMS" "Relational I") 10935) ;; #X2AB7
    (nil "vartriangleleft" ("AMS" "Relational I") 8882) ;; #X22B2
    (nil "trianglelefteq" ("AMS" "Relational I") 8884) ;; #X22B4
    (nil "vDash" ("AMS" "Relational I") 8872) ;; #X22A8
    (nil "Vvdash" ("AMS" "Relational I") 8874) ;; #X22AA
    (nil "smallsmile" ("AMS" "Relational I") 8995) ;; #X2323
    (nil "smallfrown" ("AMS" "Relational I") 8994) ;; #X2322
    (nil "bumpeq" ("AMS" "Relational I") 8783) ;; #X224F
    (nil "Bumpeq" ("AMS" "Relational I") 8782) ;; #X224E
    (nil "geqq" ("AMS" "Relational II") 8807) ;; #X2267
    (nil "geqslant" ("AMS" "Relational II") 10878) ;; #X2A7E
    (nil "eqslantgtr" ("AMS" "Relational II") 10902) ;; #X2A96
    (nil "gtrsim" ("AMS" "Relational II") 8819) ;; #X2273
    (nil "gtrapprox" ("AMS" "Relational II") 10886) ;; #X2A86
    (nil "gtrdot" ("AMS" "Relational II") 8919) ;; #X22D7
    (nil "ggg" ("AMS" "Relational II") 8921) ;; #X22D9
    (nil "gtrless" ("AMS" "Relational II") 8823) ;; #X2277
    (nil "gtreqless" ("AMS" "Relational II") 8923) ;; #X22DB
    (nil "gtreqqless" ("AMS" "Relational II") 10892) ;; #X2A8C
    (nil "eqcirc" ("AMS" "Relational II") 8790) ;; #X2256
    (nil "circeq" ("AMS" "Relational II") 8791) ;; #X2257
    (nil "triangleq" ("AMS" "Relational II") 8796) ;; #X225C
    (nil "thicksim" ("AMS" "Relational II") 8764) ;; #X223C
    (nil "thickapprox" ("AMS" "Relational II") 8776) ;; #X2248
    (nil "supseteqq" ("AMS" "Relational II") 10950) ;; #X2AC6
    (nil "Supset" ("AMS" "Relational II") 8913) ;; #X22D1
    (nil "sqsupset" ("AMS" "Relational II") 8848) ;; #X2290
    (nil "succcurlyeq" ("AMS" "Relational II") 8829) ;; #X227D
    (nil "curlyeqsucc" ("AMS" "Relational II") 8927) ;; #X22DF
    (nil "succsim" ("AMS" "Relational II") 8831) ;; #X227F
    (nil "succapprox" ("AMS" "Relational II") 10936) ;; #X2AB8
    (nil "vartriangleright" ("AMS" "Relational II") 8883) ;; #X22B3
    (nil "trianglerighteq" ("AMS" "Relational II") 8885) ;; #X22B5
    (nil "Vdash" ("AMS" "Relational II") 8873) ;; #X22A9
    (nil "shortmid" ("AMS" "Relational II") 8739) ;; #X2223
    (nil "shortparallel" ("AMS" "Relational II") 8741) ;; #X2225
    (nil "between" ("AMS" "Relational II") 8812) ;; #X226C
    (nil "pitchfork" ("AMS" "Relational II") 8916) ;; #X22D4
    (nil "varpropto" ("AMS" "Relational II") 8733) ;; #X221D
    (nil "blacktriangleleft" ("AMS" "Relational II") 9664) ;; #X25C0
    (nil "therefore" ("AMS" "Relational II") 8756) ;; #X2234
    (nil "backepsilon" ("AMS" "Relational II") 1014) ;; #X03F6
    (nil "blacktriangleright" ("AMS" "Relational II") 9654) ;; #X25B6
    (nil "because" ("AMS" "Relational II") 8757) ;; #X2235
    (nil "nless" ("AMS" "Neg Rel I") 8814) ;; #X226E
    (nil "nleq" ("AMS" "Neg Rel I") 8816) ;; #X2270
    (nil "nleqslant" ("AMS" "Neg Rel I"))
    (nil "nleqq" ("AMS" "Neg Rel I"))
    (nil "lneq" ("AMS" "Neg Rel I") 10887) ;; #X2A87
    (nil "lneqq" ("AMS" "Neg Rel I") 8808) ;; #X2268
    (nil "lvertneqq" ("AMS" "Neg Rel I"))
    (nil "lnsim" ("AMS" "Neg Rel I") 8934) ;; #X22E6
    (nil "lnapprox" ("AMS" "Neg Rel I") 10889) ;; #X2A89
    (nil "nprec" ("AMS" "Neg Rel I") 8832) ;; #X2280
    (nil "npreceq" ("AMS" "Neg Rel I"))
    (nil "precnsim" ("AMS" "Neg Rel I") 8936) ;; #X22E8
    (nil "precnapprox" ("AMS" "Neg Rel I") 10937) ;; #X2AB9
    (nil "nsim" ("AMS" "Neg Rel I") 8769) ;; #X2241
    (nil "nshortmid" ("AMS" "Neg Rel I") 8740) ;; #X2224
    (nil "nmid" ("AMS" "Neg Rel I") 8740) ;; #X2224
    (nil "nvdash" ("AMS" "Neg Rel I") 8876) ;; #X22AC
    (nil "nvDash" ("AMS" "Neg Rel I") 8877) ;; #X22AD
    (nil "ntriangleleft" ("AMS" "Neg Rel I") 8938) ;; #X22EA
    (nil "ntrianglelefteq" ("AMS" "Neg Rel I") 8940) ;; #X22EC
    (nil "nsubseteq" ("AMS" "Neg Rel I") 8840) ;; #X2288
    (nil "subsetneq" ("AMS" "Neg Rel I") 8842) ;; #X228A
    (nil "varsubsetneq" ("AMS" "Neg Rel I"))
    (nil "subsetneqq" ("AMS" "Neg Rel I") 10955) ;; #X2ACB
    (nil "varsubsetneqq" ("AMS" "Neg Rel I"))
    (nil "ngtr" ("AMS" "Neg Rel II") 8815) ;; #X226F
    (nil "ngeq" ("AMS" "Neg Rel II") 8817) ;; #X2271
    (nil "ngeqslant" ("AMS" "Neg Rel II"))
    (nil "ngeqq" ("AMS" "Neg Rel II"))
    (nil "gneq" ("AMS" "Neg Rel II") 10888) ;; #X2A88
    (nil "gneqq" ("AMS" "Neg Rel II") 8809) ;; #X2269
    (nil "gvertneqq" ("AMS" "Neg Rel II"))
    (nil "gnsim" ("AMS" "Neg Rel II") 8935) ;; #X22E7
    (nil "gnapprox" ("AMS" "Neg Rel II") 10890) ;; #X2A8A
    (nil "nsucc" ("AMS" "Neg Rel II") 8833) ;; #X2281
    (nil "nsucceq" ("AMS" "Neg Rel II"))
    (nil "succnsim" ("AMS" "Neg Rel II") 8937) ;; #X22E9
    (nil "succnapprox" ("AMS" "Neg Rel II") 10938) ;; #X2ABA
    (nil "ncong" ("AMS" "Neg Rel II") 8775) ;; #X2247
    (nil "nshortparallel" ("AMS" "Neg Rel II") 8742) ;; #X2226
    (nil "nparallel" ("AMS" "Neg Rel II") 8742) ;; #X2226
    (nil "nvDash" ("AMS" "Neg Rel II") 8877) ;; #X22AD
    (nil "nVDash" ("AMS" "Neg Rel II") 8879) ;; #X22AF
    (nil "ntriangleright" ("AMS" "Neg Rel II") 8939) ;; #X22EB
    (nil "ntrianglerighteq" ("AMS" "Neg Rel II") 8941) ;; #X22ED
    (nil "nsupseteq" ("AMS" "Neg Rel II") 8841) ;; #X2289
    (nil "nsupseteqq" ("AMS" "Neg Rel II"))
    (nil "supsetneq" ("AMS" "Neg Rel II") 8843) ;; #X228B
    (nil "varsupsetneq" ("AMS" "Neg Rel II"))
    (nil "supsetneqq" ("AMS" "Neg Rel II") 10956) ;; #X2ACC
    (nil "varsupsetneqq" ("AMS" "Neg Rel II"))
    (nil "dotplus" ("AMS" "Binary Op") 8724) ;; #X2214
    (nil "smallsetminus" ("AMS" "Binary Op") 8726) ;; #X2216
    (nil "Cap" ("AMS" "Binary Op") 8914) ;; #X22D2
    (nil "Cup" ("AMS" "Binary Op") 8915) ;; #X22D3
    (nil "barwedge" ("AMS" "Binary Op") 8892) ;; #X22BC
    (nil "veebar" ("AMS" "Binary Op") 8891) ;; #X22BB
    (nil "doublebarwedge" ("AMS" "Binary Op") 8966) ;; #X2306
    (nil "boxminus" ("AMS" "Binary Op") 8863) ;; #X229F
    (nil "boxtimes" ("AMS" "Binary Op") 8864) ;; #X22A0
    (nil "boxdot" ("AMS" "Binary Op") 8865) ;; #X22A1
    (nil "boxplus" ("AMS" "Binary Op") 8862) ;; #X229E
    (nil "divideontimes" ("AMS" "Binary Op") 8903) ;; #X22C7
    (nil "ltimes" ("AMS" "Binary Op") 8905) ;; #X22C9
    (nil "rtimes" ("AMS" "Binary Op") 8906) ;; #X22CA
    (nil "leftthreetimes" ("AMS" "Binary Op") 8907) ;; #X22CB
    (nil "rightthreetimes" ("AMS" "Binary Op") 8908) ;; #X22CC
    (nil "curlywedge" ("AMS" "Binary Op") 8911) ;; #X22CF
    (nil "curlyvee" ("AMS" "Binary Op") 8910) ;; #X22CE
    (nil "circleddash" ("AMS" "Binary Op") 8861) ;; #X229D
    (nil "circledast" ("AMS" "Binary Op") 8859) ;; #X229B
    (nil "circledcirc" ("AMS" "Binary Op") 8858) ;; #X229A
    (nil "centerdot" ("AMS" "Binary Op"))
    (nil "intercal" ("AMS" "Binary Op") 8890) ;; #X22BA
    (nil "hbar" ("AMS" "Misc") 8463) ;; #X210F
    (nil "hslash" ("AMS" "Misc") 8463) ;; #X210F
    (nil "vartriangle" ("AMS" "Misc") 9653) ;; #X25B5
    (nil "triangledown" ("AMS" "Misc") 9663) ;; #X25BF
    (nil "square" ("AMS" "Misc") 9633) ;; #X25A1
    (nil "lozenge" ("AMS" "Misc") 9674) ;; #X25CA
    (nil "circledS" ("AMS" "Misc") 9416) ;; #X24C8
    (nil "angle" ("AMS" "Misc") 8736) ;; #X2220
    (nil "measuredangle" ("AMS" "Misc") 8737) ;; #X2221
    (nil "nexists" ("AMS" "Misc") 8708) ;; #X2204
    (nil "mho" ("AMS" "Misc") 8487) ;; #X2127
    (nil "Finv" ("AMS" "Misc") 8498) ;; #X2132
    (nil "Game" ("AMS" "Misc") 8513) ;; #X2141
    (nil "Bbbk" ("AMS" "Misc") 120156) ;; #X1D55C
    (nil "backprime" ("AMS" "Misc") 8245) ;; #X2035
    (nil "varnothing" ("AMS" "Misc") 8709) ;; #X2205
    (nil "blacktriangle" ("AMS" "Misc") 9652) ;; #X25B4
    (nil "blacktriangledown" ("AMS" "Misc") 9662) ;; #X25BE
    (nil "blacksquare" ("AMS" "Misc") 9632) ;; #X25A0
    (nil "blacklozenge" ("AMS" "Misc") 10731) ;; #X29EB
    (nil "bigstar" ("AMS" "Misc") 9733) ;; #X2605
    (nil "sphericalangle" ("AMS" "Misc") 8738) ;; #X2222
    (nil "complement" ("AMS" "Misc") 8705) ;; #X2201
    (nil "eth" ("AMS" "Misc") 240) ;; #X00F0
    (nil "diagup" ("AMS" "Misc") 9585) ;; #X2571
    (nil "diagdown" ("AMS" "Misc") 9586) ;; #X2572
    (nil "dddot" ("AMS" "Accents") 8411) ;; #X20DB
    (nil "ddddot" ("AMS" "Accents") 8412) ;; #X20DC
    (nil "bigl" ("AMS" "Delimiters"))
    (nil "bigr" ("AMS" "Delimiters"))
    (nil "Bigl" ("AMS" "Delimiters"))
    (nil "Bigr" ("AMS" "Delimiters"))
    (nil "biggl" ("AMS" "Delimiters"))
    (nil "biggr" ("AMS" "Delimiters"))
    (nil "Biggl" ("AMS" "Delimiters"))
    (nil "Biggr" ("AMS" "Delimiters"))
    (nil "lvert" ("AMS" "Delimiters"))
    (nil "rvert" ("AMS" "Delimiters"))
    (nil "lVert" ("AMS" "Delimiters"))
    (nil "rVert" ("AMS" "Delimiters"))
    (nil "ulcorner" ("AMS" "Delimiters") 8988) ;; #X231C
    (nil "urcorner" ("AMS" "Delimiters") 8989) ;; #X231D
    (nil "llcorner" ("AMS" "Delimiters") 8990) ;; #X231E
    (nil "lrcorner" ("AMS" "Delimiters") 8991) ;; #X231F
    (nil "nobreakdash" ("AMS" "Special"))
    (nil "leftroot" ("AMS" "Special"))
    (nil "uproot" ("AMS" "Special"))
    (nil "accentedsymbol" ("AMS" "Special"))
    (nil "xleftarrow" ("AMS" "Special"))
    (nil "xrightarrow" ("AMS" "Special"))
    (nil "overset" ("AMS" "Special"))
    (nil "underset" ("AMS" "Special"))
    (nil "dfrac" ("AMS" "Special"))
    (nil "genfrac" ("AMS" "Special"))
    (nil "tfrac" ("AMS" "Special"))
    (nil "binom" ("AMS" "Special"))
    (nil "dbinom" ("AMS" "Special"))
    (nil "tbinom" ("AMS" "Special"))
    (nil "smash" ("AMS" "Special"))
    (nil "eucal" ("AMS" "Special"))
    (nil "boldsymbol" ("AMS" "Special"))
    (nil "text" ("AMS" "Special"))
    (nil "intertext" ("AMS" "Special"))
    (nil "substack" ("AMS" "Special"))
    (nil "subarray" ("AMS" "Special"))
    (nil "sideset" ("AMS" "Special")))
  "Alist of LaTeX math symbols.

Each entry should be a list with upto four elements, KEY, VALUE,
MENU and CHARACTER, see `LaTeX-math-list' for details.")

(defcustom LaTeX-math-list nil
  "Alist of your personal LaTeX math symbols.

Each entry should be a list with up to four elements, KEY, VALUE,
MENU and CHARACTER.

KEY is the key (after `LaTeX-math-abbrev-prefix') to be redefined
in math minor mode.  If KEY is nil, the symbol has no associated
keystroke \(it is available in the menu, though\).

VALUE can be a string with the name of the macro to be inserted,
or a function to be called.  The macro must be given without the
leading backslash.

The third element MENU is the name of the submenu where the
command should be added.  MENU can be either a string
\(e.g. \"greek\"\), a list (e.g. \(\"AMS\" \"Delimiters\"\)\) or
nil.  If MENU is nil, no menu item will be created.

The fourth element CHARACTER is a Unicode character position for
menu display.  When nil, no character is shown.

See also `LaTeX-math-menu'."
  :group 'LaTeX-math
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (LaTeX-math-initialize))
  :type '(repeat (group (choice :tag "Key"
				(const :tag "none" nil)
				(choice (character)
					(string :tag "Key sequence")))
			(choice :tag "Value"
				(string :tag "Macro")
				(function))
			(choice :tag "Menu"
				(string :tag "Top level menu" )
				(repeat :tag "Submenu"
					(string :tag "Menu")))
			(choice :tag "Unicode character"
				(const :tag "none" nil)
				(integer :tag "Number")))))

(define-minor-mode LaTeX-math-mode
  "A minor mode with easy access to TeX math macros.

Easy insertion of LaTeX math symbols.  If you give a prefix argument,
the symbols will be surrounded by dollar signs.  The following
commands are defined:

\\{LaTeX-math-mode-map}"
  nil nil (list (cons (LaTeX-math-abbrev-prefix) LaTeX-math-keymap))
  (if LaTeX-math-mode
      (easy-menu-add LaTeX-math-mode-menu LaTeX-math-mode-map)
    (easy-menu-remove LaTeX-math-mode-menu))
  (TeX-set-mode-name))
(defalias 'latex-math-mode 'LaTeX-math-mode)

(easy-menu-define LaTeX-math-mode-menu
    LaTeX-math-mode-map
    "Menu used in math minor mode."
  LaTeX-math-menu)

(defcustom LaTeX-math-insert-function 'TeX-insert-macro
  "Function called with argument STRING to insert \\STRING."
  :group 'LaTeX-math
  :type 'function)

(defun LaTeX-math-insert (string dollar)
  "Insert \\STRING{}.  If DOLLAR is non-nil, put $'s around it.
If `TeX-electric-math' is non-nil wrap that symbols around the
string."
  (if dollar (insert (or (car TeX-electric-math) "$")))
  (funcall LaTeX-math-insert-function string)
  (if dollar (insert (or (cdr TeX-electric-math) "$"))))

(defun LaTeX-math-cal (char dollar)
  "Insert a {\\cal CHAR}.  If DOLLAR is non-nil, put $'s around it.
If `TeX-electric-math' is non-nil wrap that symbols around the
char."
  (interactive "*c\nP")
  (if dollar (insert (or (car TeX-electric-math) "$")))
  (if (member "latex2e" (TeX-style-list))
      (insert "\\mathcal{" (char-to-string char) "}")
    (insert "{\\cal " (char-to-string char) "}"))
  (if dollar (insert (or (cdr TeX-electric-math) "$"))))


;;; Folding

(defcustom LaTeX-fold-macro-spec-list nil
  "List of display strings and macros to fold in LaTeX mode."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Macros" (string))))
  :group 'TeX-fold)

(defcustom LaTeX-fold-env-spec-list nil
  "List of display strings and environments to fold in LaTeX mode."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Environments" (string))))
  :group 'TeX-fold)

(defcustom LaTeX-fold-math-spec-list
  (delete nil
	  (mapcar (lambda (elt)
		    (let ((tex-token (nth 1 elt))
			  (submenu   (nth 2 elt))
			  (unicode   (nth 3 elt))
			  uchar noargp)
		      (when (and (fboundp 'decode-char) (integerp unicode))
			(setq uchar (decode-char 'ucs unicode)))
		      (when (listp submenu) (setq submenu (nth 1 submenu)))
		      (setq noargp
			    (not (string-match
				  (concat "^" (regexp-opt '("Constructs"
							    "Accents")))
				  submenu)))
		      (when (and (stringp tex-token) (integerp uchar) noargp)
			`(,(char-to-string uchar) (,tex-token)))))
		  `((nil "to" "" 8594)
		    (nil "gets" "" 8592)
		    ,@LaTeX-math-default)))
  "List of display strings and math macros to fold in LaTeX mode."
  :type '(repeat (group (choice (string :tag "Display String")
				(integer :tag "Number of argument" :value 1))
			(repeat :tag "Math Macros" (string))))
  :group 'TeX-fold)

;;; Narrowing

(defun LaTeX-narrow-to-environment (&optional count)
  "Make text outside current environment invisible.
With optional COUNT keep visible that number of enclosing
environmens."
  (interactive "p")
  (setq count (if count (abs count) 1))
  (save-excursion
    (widen)
    (let ((opoint (point))
	  beg end)
      (dotimes (c count) (LaTeX-find-matching-end))
      (setq end (point))
      (goto-char opoint)
      (dotimes (c count) (LaTeX-find-matching-begin))
      (setq beg (point))
      (narrow-to-region beg end))))
(put 'LaTeX-narrow-to-environment 'disabled t)

;;; Keymap

(defvar LaTeX-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-mode-map)

    ;; Standard
    (define-key map "\n"      'reindent-then-newline-and-indent)

    ;; From latex.el
    ;; We now set `fill-paragraph-function' instead.
    ;; (define-key map "\eq"     'LaTeX-fill-paragraph) ;*** Alias
    ;; This key is now used by Emacs for face settings.
    ;; (define-key map "\eg"     'LaTeX-fill-region) ;*** Alias
    (define-key map "\e\C-e"  'LaTeX-find-matching-end)
    (define-key map "\e\C-a"  'LaTeX-find-matching-begin)

    (define-key map "\C-c\C-q\C-p" 'LaTeX-fill-paragraph)
    (define-key map "\C-c\C-q\C-r" 'LaTeX-fill-region)
    (define-key map "\C-c\C-q\C-s" 'LaTeX-fill-section)
    (define-key map "\C-c\C-q\C-e" 'LaTeX-fill-environment)

    (define-key map "\C-c."    'LaTeX-mark-environment) ;*** Dubious
    (define-key map "\C-c*"    'LaTeX-mark-section) ;*** Dubious

    (define-key map "\C-c\C-e" 'LaTeX-environment)
    (define-key map "\C-c\n"   'LaTeX-insert-item)
    (or (key-binding "\e\r")
	(define-key map "\e\r"    'LaTeX-insert-item)) ;*** Alias
    (define-key map "\C-c]" 'LaTeX-close-environment)
    (define-key map "\C-c\C-s" 'LaTeX-section)

    (define-key map "\C-c~"    'LaTeX-math-mode) ;*** Dubious

    (define-key map "-" 'LaTeX-babel-insert-hyphen)
    (define-key map "(" 'LaTeX-insert-left-brace)
    (define-key map "{" 'LaTeX-insert-left-brace)
    (define-key map "[" 'LaTeX-insert-left-brace)
    map)
  "Keymap used in `LaTeX-mode'.")

(defvar LaTeX-environment-menu-name "Insert Environment  (C-c C-e)")

(defun LaTeX-environment-menu-entry (entry)
  "Create an entry for the environment menu."
  (vector (car entry) (list 'LaTeX-environment-menu (car entry)) t))

(defvar LaTeX-environment-modify-menu-name "Change Environment  (C-u C-c C-e)")

(defun LaTeX-environment-modify-menu-entry (entry)
  "Create an entry for the change environment menu."
  (vector (car entry) (list 'LaTeX-modify-environment (car entry)) t))

(defun LaTeX-section-enable-symbol (LEVEL)
  "Symbol used to enable section LEVEL in the menu bar."
  (intern (concat "LaTeX-section-" (int-to-string (nth 1 entry)) "-enable")))

(defun LaTeX-section-enable (entry)
  "Enable or disable section ENTRY from `LaTeX-section-list'."
  (let* ((level (nth 1 entry))
	 (symbol (LaTeX-section-enable-symbol level)))
    (set symbol (or (= level 0) (>= level LaTeX-largest-level)))
    (make-variable-buffer-local symbol)))

(defun LaTeX-section-menu (level)
  "Insert section from menu."
  (let ((LaTeX-section-hook (delq 'LaTeX-section-heading
				  (copy-sequence LaTeX-section-hook))))
    (LaTeX-section level)))

(defun LaTeX-section-menu-entry (entry)
  "Create an ENTRY for the section menu."
  (let ((enable (LaTeX-section-enable-symbol (nth 1 entry))))
    (vector (car entry) (list 'LaTeX-section-menu (nth 1 entry)) enable)))

(defcustom LaTeX-menu-max-items 25
  "*Maximum number of items in the menu for LaTeX environments.
If number of entries in a menu is larger than this value, split menu
into submenus of nearly equal length.  If nil, never split menu into
submenus."
  :group 'LaTeX-environment
  :type '(choice (const :tag "no submenus" nil)
		 (integer)))

(defcustom LaTeX-submenu-name-format "%-12.12s ... %.12s"
  "*Format specification of the submenu name.
Used by `LaTeX-split-long-menu' if the number of entries in a menu is
larger than `LaTeX-menu-max-items'.
This string should contain one %s for the name of the first entry and
one %s for the name of the last entry in the submenu.
If the value is a function, it should return the submenu name.  The
function is called with two arguments, the names of the first and
the last entry in the menu."
  :group 'LaTeX-environment
  :type '(choice (string :tag "Format string")
		 (function)))

(defun LaTeX-split-long-menu (menu)
  "Split MENU according to `LaTeX-menu-max-items'."
  (let ((len (length menu)))
    (if (or (null LaTeX-menu-max-items)
	    (null (featurep 'lisp-float-type))
	    (<= len LaTeX-menu-max-items))
	menu
      ;; Submenu is max 2 entries longer than menu, never shorter, number of
      ;; entries in submenus differ by at most one (with longer submenus first)
      (let* ((outer (floor (sqrt len)))
	     (inner (/ len outer))
	     (rest (% len outer))
	     (result nil))
	(setq menu (reverse menu))
	(while menu
	  (let ((in inner)
		(sub nil)
		(to (car menu)))
	    (while (> in 0)
	      (setq in   (1- in)
		    sub  (cons (car menu) sub)
		    menu (cdr menu)))
	    (setq result
		  (cons (cons (if (stringp LaTeX-submenu-name-format)
				  (format LaTeX-submenu-name-format
					  (aref (car sub) 0) (aref to 0))
				(funcall LaTeX-submenu-name-format
					 (aref (car sub) 0) (aref to 0)))
			      sub)
			result)
		  rest  (1+ rest))
	    (if (= rest outer) (setq inner (1+ inner)))))
	result))))

(defvar LaTeX-section-menu nil)
(make-variable-buffer-local 'LaTeX-section-menu)
(defun LaTeX-section-menu-filter (ignored)
  "Filter function for the section submenu in the mode menu.
The argument IGNORED is not used in any way."
  (TeX-update-style)
  (or LaTeX-section-menu
      (progn
	(setq LaTeX-section-list-changed nil)
	(mapc 'LaTeX-section-enable LaTeX-section-list)
	(setq LaTeX-section-menu
	      (mapcar 'LaTeX-section-menu-entry LaTeX-section-list)))))

(defvar LaTeX-environment-menu nil)
(make-variable-buffer-local 'LaTeX-environment-menu)
(defvar LaTeX-environment-modify-menu nil)
(make-variable-buffer-local 'LaTeX-environment-modify-menu)
(defun LaTeX-environment-menu-filter (menu)
  "Filter function for the environment submenus in the mode menu.
The argument MENU is the name of the submenu in concern and
corresponds to the variables `LaTeX-environment-menu-name' and
`LaTeX-environment-modify-menu-name'."
  (TeX-update-style)
  (cond
   ((string= menu LaTeX-environment-menu-name)
    (or LaTeX-environment-menu
	(setq LaTeX-environment-menu
	      (LaTeX-split-long-menu
	       (mapcar 'LaTeX-environment-menu-entry
		       (LaTeX-environment-list))))))
   ((string= menu LaTeX-environment-modify-menu-name)
    (or LaTeX-environment-modify-menu
	(setq LaTeX-environment-modify-menu
	      (LaTeX-split-long-menu
	       (mapcar 'LaTeX-environment-modify-menu-entry
		       (LaTeX-environment-list))))))))

(defadvice LaTeX-add-environments (after LaTeX-invalidate-environment-menu (&rest environments) activate)
  "Add ENVIRONMENTS to the list of known environments.
Additionally invalidate the environment submenus to let them be
regenerated by the respective menu filter."
  (setq LaTeX-environment-menu nil)
  (setq LaTeX-environment-modify-menu nil))

(easy-menu-define LaTeX-mode-command-menu
    LaTeX-mode-map
    "Command menu used in LaTeX mode."
    (TeX-mode-specific-command-menu 'latex-mode))

(easy-menu-define LaTeX-mode-menu
  LaTeX-mode-map
  "Menu used in LaTeX mode."
  (TeX-menu-with-help
   `("LaTeX"
     ("Section  (C-c C-s)" :filter LaTeX-section-menu-filter)
     ["Macro..." TeX-insert-macro
      :help "Insert a macro and possibly arguments"]
     ["Complete Macro" TeX-complete-symbol
      :help "Complete the current macro or environment name"]
     ,(list LaTeX-environment-menu-name
	    :filter (lambda (ignored) (LaTeX-environment-menu-filter
				       LaTeX-environment-menu-name)))
     ,(list LaTeX-environment-modify-menu-name
	    :filter (lambda (ignored) (LaTeX-environment-menu-filter
				       LaTeX-environment-modify-menu-name)))
     ["Close Environment" LaTeX-close-environment
      :help "Insert the \\end part of the current environment"]
     ["Item" LaTeX-insert-item
      :help "Insert a new \\item into current environment"]
     "-"
     ("Insert Font"
      ["Emphasize"  (TeX-font nil ?\C-e) :keys "C-c C-f C-e"]
      ["Bold"       (TeX-font nil ?\C-b) :keys "C-c C-f C-b"]
      ["Typewriter" (TeX-font nil ?\C-t) :keys "C-c C-f C-t"]
      ["Small Caps" (TeX-font nil ?\C-c) :keys "C-c C-f C-c"]
      ["Sans Serif" (TeX-font nil ?\C-f) :keys "C-c C-f C-f"]
      ["Italic"     (TeX-font nil ?\C-i) :keys "C-c C-f C-i"]
      ["Slanted"    (TeX-font nil ?\C-s) :keys "C-c C-f C-s"]
      ["Roman"      (TeX-font nil ?\C-r) :keys "C-c C-f C-r"]
      ["Calligraphic" (TeX-font nil ?\C-a) :keys "C-c C-f C-a"])
     ("Replace Font"
      ["Emphasize"  (TeX-font t ?\C-e) :keys "C-u C-c C-f C-e"]
      ["Bold"       (TeX-font t ?\C-b) :keys "C-u C-c C-f C-b"]
      ["Typewriter" (TeX-font t ?\C-t) :keys "C-u C-c C-f C-t"]
      ["Small Caps" (TeX-font t ?\C-c) :keys "C-u C-c C-f C-c"]
      ["Sans Serif" (TeX-font t ?\C-f) :keys "C-u C-c C-f C-f"]
      ["Italic"     (TeX-font t ?\C-i) :keys "C-u C-c C-f C-i"]
      ["Slanted"    (TeX-font t ?\C-s) :keys "C-u C-c C-f C-s"]
      ["Roman"      (TeX-font t ?\C-r) :keys "C-u C-c C-f C-r"]
      ["Calligraphic" (TeX-font t ?\C-a) :keys "C-u C-c C-f C-a"])
     ["Delete Font" (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
     "-"
     ["Comment or Uncomment Region"
      TeX-comment-or-uncomment-region
      :help "Make the selected region outcommented or active again"]
     ["Comment or Uncomment Paragraph"
      TeX-comment-or-uncomment-paragraph
      :help "Make the current paragraph outcommented or active again"]
     ("Formatting and Marking"
      ["Format Environment" LaTeX-fill-environment
       :help "Fill and indent the current environment"]
      ["Format Paragraph" LaTeX-fill-paragraph
       :help "Fill and ident the current paragraph"]
      ["Format Region" LaTeX-fill-region
       :help "Fill and indent the currently selected region"]
      ["Format Section" LaTeX-fill-section
       :help "Fill and indent the current section"]
      "-"
      ["Mark Environment" LaTeX-mark-environment
       :help "Mark the current environment"]
      ["Mark Section" LaTeX-mark-section
       :help "Mark the current section"]
      "-"
      ["Beginning of Environment" LaTeX-find-matching-begin
       :help "Move point to the beginning of the current environment"]
      ["End of Environment" LaTeX-find-matching-end
       :help "Move point to the end of the current environment"])
     ,TeX-fold-menu
     ["Math Mode" LaTeX-math-mode
      :style toggle :selected LaTeX-math-mode
      :help "Toggle math mode"]
     "-"
      [ "Convert 209 to 2e" LaTeX-209-to-2e
        :visible (member "latex2" (TeX-style-list)) ]
      . ,TeX-common-menu-entries)))

(defcustom LaTeX-font-list
  '((?\C-a ""              ""  "\\mathcal{"    "}")
    (?\C-b "\\textbf{"     "}" "\\mathbf{"     "}")
    (?\C-c "\\textsc{"     "}")
    (?\C-e "\\emph{"       "}")
    (?\C-f "\\textsf{"     "}" "\\mathsf{"     "}")
    (?\C-i "\\textit{"     "}" "\\mathit{"     "}")
    (?\C-m "\\textmd{"     "}")
    (?\C-n "\\textnormal{" "}" "\\mathnormal{" "}")
    (?\C-r "\\textrm{"     "}" "\\mathrm{"     "}")
    (?\C-s "\\textsl{"     "}" "\\mathbb{"     "}")
    (?\C-t "\\texttt{"     "}" "\\mathtt{"     "}")
    (?\C-u "\\textup{"     "}")
    (?\C-d "" "" t))
  "Font commands used with LaTeX2e.  See `TeX-font-list'."
  :group 'LaTeX-macro
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


;;; Simple Commands

(defcustom LaTeX-babel-hyphen "\"="
  "String to be used when typing `-'.
This usually is a hyphen alternative or hyphenation aid, like
\"=, \"~ or \"-, provided by babel and the related language style
files.

Set it to an empty string or nil in order to disable this
feature.  Alter `LaTeX-babel-hyphen-language-alist' in case you
want to change the behavior for a specific language only."
  :group 'LaTeX-macro
  :type 'string)

(defcustom LaTeX-babel-hyphen-after-hyphen t
  "Control insertion of hyphen strings.
If non-nil insert normal hyphen on first key press and swap it
with the language-specific hyphen string specified in the
variable `LaTeX-babel-hyphen' on second key press.  If nil do it
the other way round."
  :group 'LaTeX-macro
  :type 'boolean)

(defcustom LaTeX-babel-hyphen-language-alist nil
  "Alist controlling hyphen insertion for specific languages.
It may be used to override the defaults given by `LaTeX-babel-hyphen'
and `LaTeX-babel-hyphen-after-hyphen' respectively.  The first item
in each element is a string specifying the language as set by the
language-specific style file.  The second item is the string to be
used instead of `LaTeX-babel-hyphen'.  The third element is the
value overriding `LaTeX-bybel-hyphen-after-hyphen'."
  :group 'LaTeX-macro
  :type '(alist :key-type (string :tag "Language")
		:value-type (group (string :tag "Hyphen string")
				   (boolean :tag "Insert plain hyphen first"
					    :value t))))

(defvar LaTeX-babel-hyphen-language nil
  "String determining language-specific behavior of hyphen insertion.
It serves as an indicator that the babel hyphenation string
should be used and as a means to find a potential customization
in `LaTeX-babel-hyphen-language-alist' related to the active
language.  It is usually set by language-related style files.")
(make-variable-buffer-local 'LaTeX-babel-hyphen-language)

(defun LaTeX-babel-insert-hyphen (force)
  "Insert a hyphen string.
The string can be either a normal hyphen or the string specified
in `LaTeX-babel-hyphen'.  Wether one or the other is chosen
depends on the value of `LaTeX-babel-hyphen-after-hyphen' and
the buffer context.
If prefix argument FORCE is non-nil, always insert a regular hyphen."
  (interactive "*P")
  (if (or force
	  (zerop (length LaTeX-babel-hyphen))
	  (not LaTeX-babel-hyphen-language)
	  ;; FIXME: It would be nice to check for verbatim constructs in the
	  ;; non-font-locking case, but things like `LaTeX-current-environment'
	  ;; are rather expensive in large buffers.
	  (and (fboundp 'font-latex-faces-present-p)
	       (font-latex-faces-present-p '(font-latex-verbatim-face
					     font-latex-math-face
					     font-lock-comment-face)))
	  (texmathp)
	  (TeX-in-comment))
      (call-interactively 'self-insert-command)
    (let* ((lang (assoc LaTeX-babel-hyphen-language
			LaTeX-babel-hyphen-language-alist))
	   (hyphen (if lang (nth 1 lang) LaTeX-babel-hyphen))
	   (h-after-h (if lang (nth 2 lang) LaTeX-babel-hyphen-after-hyphen))
	   (hyphen-length (length hyphen)))
      (cond
       ;; "= --> -- / -
       ((string= (buffer-substring (max (- (point) hyphen-length) (point-min))
				   (point))
		 hyphen)
	(if h-after-h
	    (progn (delete-char (- hyphen-length))
		   (insert "--"))
	  (delete-char (- hyphen-length))
	  (call-interactively 'self-insert-command)))
       ;; -- --> [+]-
       ((string= (buffer-substring (max (- (point) 2) (point-min))
				   (point))
		 "--")
	(call-interactively 'self-insert-command))
       ;; - --> "= / [+]-
       ((eq (char-before) ?-)
	(if h-after-h
	    (progn (delete-char -1)
		   (insert hyphen))
	  (call-interactively 'self-insert-command)))
       (h-after-h
	(call-interactively 'self-insert-command))
       (t (insert hyphen))))))
;; Cater for Delete Selection mode
(put 'LaTeX-babel-insert-hyphen 'delete-selection t)

(defcustom LaTeX-enable-toolbar t
  "Enable LaTeX tool bar."
  :group 'TeX-tool-bar
  :type 'boolean)

(defun LaTeX-maybe-install-toolbar ()
  "Conditionally install tool bar buttons for LaTeX mode.
Install tool bar if `LaTeX-enable-toolbar' is non-nil."
  (when LaTeX-enable-toolbar
    ;; Defined in `tex-bar.el':
    (LaTeX-install-toolbar)))

;;; Mode

(defgroup LaTeX-macro nil
  "Special support for LaTeX macros in AUCTeX."
  :prefix "TeX-"
  :group 'LaTeX
  :group 'TeX-macro)

(defcustom TeX-arg-cite-note-p nil
  "*If non-nil, ask for optional note in citations."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom TeX-arg-footnote-number-p nil
  "*If non-nil, ask for optional number in footnotes."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom TeX-arg-item-label-p nil
  "*If non-nil, always ask for optional label in items.
Otherwise, only ask in description environments."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom TeX-arg-right-insert-p t
  "*If non-nil, always insert automatically the corresponding \\right.
This happens when \\left is inserted."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom LaTeX-mode-hook nil
  "A hook run in LaTeX mode buffers."
  :type 'hook
  :group 'LaTeX)

(TeX-abbrev-mode-setup latex-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.drv\\'" . latex-mode))

;;;###autoload
(defun TeX-latex-mode ()
  "Major mode in AUCTeX for editing LaTeX files.
See info under AUCTeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'."
  (interactive)
  (LaTeX-common-initialization)
  (setq TeX-base-mode-name "LaTeX")
  (setq major-mode 'latex-mode)
  (setq TeX-command-default "LaTeX")
  (setq TeX-sentinel-default-function 'TeX-LaTeX-sentinel)
  (add-hook 'tool-bar-mode-on-hook 'LaTeX-maybe-install-toolbar nil t)
  (when (if (featurep 'xemacs)
	    (featurep 'toolbar)
	  (and (boundp 'tool-bar-mode) tool-bar-mode))
    (LaTeX-maybe-install-toolbar))
  ;; Set the value of `LaTeX-using-Biber' based on the local value of
  ;; `LaTeX-biblatex-use-Biber'.  This should be run within
  ;; `TeX-update-style-hook' before toolbarx-refresh, otherwise the bibliography
  ;; button could be wrongly set.
  (add-hook 'TeX-update-style-hook
	    (lambda ()
	      (if (local-variable-p 'LaTeX-biblatex-use-Biber (current-buffer))
		  (setq LaTeX-using-Biber LaTeX-biblatex-use-Biber))) nil t)
  (TeX-run-mode-hooks 'text-mode-hook 'TeX-mode-hook 'LaTeX-mode-hook)
  (TeX-set-mode-name)
  ;; Defeat filladapt
  (if (and (boundp 'filladapt-mode)
	   filladapt-mode)
      (turn-off-filladapt-mode)))

(TeX-abbrev-mode-setup doctex-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dtx\\'" . doctex-mode))

;;;###autoload
(define-derived-mode docTeX-mode TeX-latex-mode "docTeX"
  "Major mode in AUCTeX for editing .dtx files derived from `LaTeX-mode'.
Runs `LaTeX-mode', sets a few variables and
runs the hooks in `docTeX-mode-hook'."
  :abbrev-table doctex-mode-abbrev-table
  (setq major-mode 'doctex-mode)
  (set (make-local-variable 'LaTeX-insert-into-comments) t)
  (set (make-local-variable 'LaTeX-syntactic-comments) t)
  (setq TeX-default-extension docTeX-default-extension)
  ;; Make filling and indentation aware of DocStrip guards.
  (setq paragraph-start (concat paragraph-start "\\|%<")
	paragraph-separate (concat paragraph-separate "\\|%<")
	TeX-comment-start-regexp "\\(?:%\\(?:<[^>]+>\\)?\\)")
  (setq TeX-base-mode-name "docTeX")
  (TeX-set-mode-name)
  (funcall TeX-install-font-lock))

;;This is actually a mess: to fit the scheme properly, our derived
;;mode definition would have had to be made for TeX-doctex-mode in the
;;first place, but then we could not have used define-derived-mode, or
;;all mode-specific variables would have gotten non-AUCTeX names.
;;This solution has the advantage that documentation strings are
;;provided in the autoloads, and has the disadvantage that docTeX-mode
;;is not aliased to doctex-mode (not even when the AUCTeX version is
;;disabled) as would be normal for our scheme.

;;;###autoload
(defalias 'TeX-doctex-mode 'docTeX-mode)

(defcustom docTeX-clean-intermediate-suffixes
  TeX-clean-default-intermediate-suffixes
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defcustom docTeX-clean-output-suffixes TeX-clean-default-output-suffixes
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defvar LaTeX-header-end
  (concat "^[^%\n]*" (regexp-quote TeX-esc) "begin *"
	  TeX-grop "document" TeX-grcl)
  "Default end of header marker for LaTeX documents.")

(defvar LaTeX-trailer-start
  (concat "^[^%\n]*" (regexp-quote TeX-esc) "end *"
	  TeX-grop "document" TeX-grcl)
  "Default start of trailer marker for LaTeX documents.")

(defcustom LaTeX-clean-intermediate-suffixes
  TeX-clean-default-intermediate-suffixes
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defcustom LaTeX-clean-output-suffixes TeX-clean-default-output-suffixes
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defun LaTeX-common-initialization ()
  "Common initialization for LaTeX derived modes."
  (VirTeX-common-initialization)
  (set-syntax-table LaTeX-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'LaTeX-indent-line)

  (setq local-abbrev-table latex-mode-abbrev-table)

  ;; Filling
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'LaTeX-fill-paragraph)
  (set (make-local-variable 'adaptive-fill-mode) nil)

  (or LaTeX-largest-level
      (setq LaTeX-largest-level (LaTeX-section-level "section")))

  (setq TeX-header-end LaTeX-header-end
	TeX-trailer-start LaTeX-trailer-start)
  (set (make-local-variable 'TeX-style-hook-dialect) :latex)

  (require 'outline)
  (set (make-local-variable 'outline-level) 'LaTeX-outline-level)
  (set (make-local-variable 'outline-regexp) (LaTeX-outline-regexp t))
  (when (boundp 'outline-heading-alist)
    (setq outline-heading-alist
	  (mapcar (lambda (x)
		    (cons (concat "\\" (nth 0 x)) (nth 1 x)))
		  LaTeX-section-list)))

  (set (make-local-variable 'TeX-auto-full-regexp-list)
	(append LaTeX-auto-regexp-list plain-TeX-auto-regexp-list))

  (LaTeX-set-paragraph-start)
  (setq paragraph-separate
	(concat
	 "[ \t]*%*[ \t]*\\("
	 "\\$\\$"			; Plain TeX display math
	 "\\|$\\)"))

  (setq TeX-verbatim-p-function 'LaTeX-verbatim-p)
  (setq TeX-search-forward-comment-start-function
	'LaTeX-search-forward-comment-start)
  (set (make-local-variable 'TeX-search-files-type-alist)
       LaTeX-search-files-type-alist)

  (set (make-local-variable 'LaTeX-item-list) '(("description" . LaTeX-item-argument)
			  ("thebibliography" . LaTeX-item-bib)
			  ("array" . LaTeX-item-array)
			  ("tabular" . LaTeX-item-array)
			  ("tabular*" . LaTeX-item-tabular*)))

  (setq TeX-complete-list
	(append '(("\\\\cite\\[[^]\n\r\\%]*\\]{\\([^{}\n\r\\%,]*\\)"
		   1 LaTeX-bibitem-list "}")
		  ("\\\\cite{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
		  ("\\\\cite{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
		   2 LaTeX-bibitem-list)
		  ("\\\\nocite{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
		  ("\\\\nocite{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
		   2 LaTeX-bibitem-list)
		  ("\\\\ref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\eqref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\pageref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\\\(index\\|glossary\\){\\([^{}\n\r\\%]*\\)"
		   2 LaTeX-index-entry-list "}")
		  ("\\\\begin{\\([A-Za-z]*\\)" 1 LaTeX-environment-list-filtered "}")
		  ("\\\\end{\\([A-Za-z]*\\)" 1 LaTeX-environment-list-filtered "}")
		  ("\\\\renewcommand\\*?{\\\\\\([A-Za-z]*\\)"
		   1 TeX-symbol-list-filtered "}")
		  ("\\\\renewenvironment\\*?{\\([A-Za-z]*\\)"
		   1 LaTeX-environment-list-filtered "}")
                  ("\\\\\\(this\\)?pagestyle{\\([A-Za-z]*\\)"
		   1 LaTeX-pagestyle-list "}"))
		TeX-complete-list))

  (LaTeX-add-environments
   '("document" LaTeX-env-document)
   '("enumerate" LaTeX-env-item)
   '("itemize" LaTeX-env-item)
   '("list" LaTeX-env-list)
   '("trivlist" LaTeX-env-item)
   '("picture" LaTeX-env-picture)
   '("tabular" LaTeX-env-array)
   '("tabular*" LaTeX-env-tabular*)
   '("array" LaTeX-env-array)
   '("eqnarray" LaTeX-env-label)
   '("equation" LaTeX-env-label)
   '("minipage" LaTeX-env-minipage)

   ;; The following have no special support, but are included in
   ;; case the auto files are missing.

   "sloppypar" "picture" "tabbing" "verbatim" "verbatim*"
   "flushright" "flushleft" "displaymath" "math" "quote" "quotation"
   "abstract" "center" "titlepage" "verse" "eqnarray*"

   ;; The following are not defined in latex.el, but in a number of
   ;; other style files.  I'm to lazy to copy them to all the
   ;; corresponding .el files right now.

   ;; This means that AUCTeX will complete e.g.
   ;; ``thebibliography'' in a letter, but I guess we can live with
   ;; that.

   '("description" LaTeX-env-item)
   '("figure" LaTeX-env-figure)
   '("figure*" LaTeX-env-figure)
   '("table" LaTeX-env-figure)
   '("table*" LaTeX-env-figure)
   '("thebibliography" LaTeX-env-bib)
   '("theindex" LaTeX-env-item))

  ;; `latex.ltx' defines `plain' and `empty' pagestyles
  (LaTeX-add-pagestyles "plain" "empty")

  ;; `latex.ltx' defines the following counters
  (LaTeX-add-counters "page" "equation" "enumi" "enumii" "enumiii"
		      "enumiv" "footnote" "mpfootnote")

  (LaTeX-add-lengths "baselineskip" "baselinestretch" "columnsep"
		     "columnwidth" "evensidemargin" "linewidth" "oddsidemargin"
		     "paperwidth" "paperheight" "parindent" "parskip"
		     "tabcolsep" "textheight" "textwidth" "topmargin"
		     "unitlength")

  (TeX-add-symbols
   '("addtocounter" TeX-arg-counter "Value")
   '("alph" TeX-arg-counter)
   '("arabic" TeX-arg-counter)
   '("fnsymbol" TeX-arg-counter)
   '("newcounter" TeX-arg-define-counter
     [ TeX-arg-counter "Within counter" ])
   '("roman" TeX-arg-counter)
   '("setcounter" TeX-arg-counter "Value")
   '("usecounter" TeX-arg-counter)
   '("value" TeX-arg-counter)
   '("stepcounter" TeX-arg-counter)
   '("refstepcounter" TeX-arg-counter)
   '("label" TeX-arg-define-label)
   '("pageref" TeX-arg-ref)
   '("ref" TeX-arg-ref)
   '("newcommand" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("renewcommand" TeX-arg-macro [ "Number of arguments" ] t)
   '("newenvironment" TeX-arg-define-environment
     [ "Number of arguments"] t t)
   '("renewenvironment" TeX-arg-environment
     [ "Number of arguments"] t t)
   '("providecommand" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("providecommand*" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("newcommand*" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("renewcommand*" TeX-arg-macro [ "Number of arguments" ] t)
   '("newenvironment*" TeX-arg-define-environment
     [ "Number of arguments"] t t)
   '("renewenvironment*" TeX-arg-environment
     [ "Number of arguments"] t t)
   '("newtheorem" TeX-arg-define-environment
     [ TeX-arg-environment "Numbered like" ]
     t [ (TeX-arg-eval progn (if (eq (save-excursion
				       (backward-char 2)
				       (preceding-char)) ?\])
				 ()
			       (TeX-arg-counter t "Within counter"))
		       "") ])
   '("newfont" TeX-arg-define-macro t)
   '("circle" "Diameter")
   '("circle*" "Diameter")
   '("dashbox" "Dash Length" TeX-arg-size
     [ TeX-arg-corner ] t)
   '("frame" t)
   '("framebox" (TeX-arg-conditional
		 (string-equal (LaTeX-current-environment) "picture")
		 (TeX-arg-size [ TeX-arg-corner ] t)
		 ([ "Length" ] [ TeX-arg-lr ] t)))
   '("line" (TeX-arg-pair "X slope" "Y slope") "Length")
   '("linethickness" "Dimension")
   '("makebox" (TeX-arg-conditional
		(string-equal (LaTeX-current-environment) "picture")
		(TeX-arg-size [ TeX-arg-corner ] t)
		([ "Length" ] [ TeX-arg-lr ] t)))
   '("multiput"
     TeX-arg-coordinate
     (TeX-arg-pair "X delta" "Y delta")
     "Number of copies"
     t)
   '("oval" TeX-arg-size [ TeX-arg-corner "Portion" ])
   '("put" TeX-arg-coordinate t)
   '("savebox" TeX-arg-savebox
     (TeX-arg-conditional
      (string-equal (LaTeX-current-environment) "picture")
      (TeX-arg-size [ TeX-arg-corner ] t)
      ([ "Length" ] [ TeX-arg-lr ] t)))
   '("shortstack" [ TeX-arg-lr ] t)
   '("vector" (TeX-arg-pair "X slope" "Y slope") "Length")
   '("cline" "Span `i-j'")
   '("multicolumn" "Columns" "Format" t)
   '("item"
     (TeX-arg-conditional (or TeX-arg-item-label-p
			      (string-equal (LaTeX-current-environment)
					    "description"))
			  ([ "Item label" ])
			  ())
     (TeX-arg-literal " "))
   '("bibitem" [ "Bibitem label" ] TeX-arg-define-cite)
   '("cite"
     (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) ())
     TeX-arg-cite)
   '("nocite" TeX-arg-cite)
   '("bibliographystyle" TeX-arg-bibstyle)
   '("bibliography" TeX-arg-bibliography)
   '("footnote"
     (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
     t)
   '("footnotetext"
     (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
     t)
   '("footnotemark"
     (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil))
   '("newlength" (TeX-arg-define-length "Length macro"))
   '("setlength" (TeX-arg-length "Length macro" "\\")
     (TeX-arg-length "Length value"))
   '("addtolength" (TeX-arg-length "Length macro" "\\")
     (TeX-arg-length "Length to add"))
   '("settowidth" (TeX-arg-length "Length macro" "\\") "Text")
   '("settoheight" (TeX-arg-length "Length macro" "\\") "Text")
   '("settodepth" (TeX-arg-length "Length macro" "\\") "Text")
   '("\\" [ "Space" ])
   '("\\*" [ "Space" ])
   '("hyphenation" t)
   '("linebreak" [ "How much [0 - 4]" ])
   '("nolinebreak" [ "How much [0 - 4]" ])
   '("nopagebreak" [ "How much [0 - 4]" ])
   '("pagebreak" [ "How much [0 - 4]" ])
   '("stackrel" t nil)
   '("frac" t nil)
   '("lefteqn" t)
   '("overbrace" t)
   '("overline" t)
   '("overleftarrow" t)
   '("overrightarrow" t)
   '("sqrt" [ "Root" ] t)
   '("underbrace" t)
   '("underline" t)
   '("acute" t) '("grave" t) '("ddot" t) '("tilde" t) '("bar" t)
   '("breve" t) '("check" t) '("hat" t) '("vec" t) '("dot" t)
   '("widetilde" t) '("widehat" t)
   '("author" LaTeX-arg-author)
   '("date" TeX-arg-date)
   '("thanks" t)
   '("title" t)
   '("pagenumbering" (TeX-arg-eval
		      completing-read "Numbering style: "
		      '(("arabic") ("roman") ("Roman") ("alph") ("Alph"))))
   '("pagestyle" TeX-arg-pagestyle)
   '("markboth" t nil)
   '("markright" t)
   '("thispagestyle" TeX-arg-pagestyle)
   '("addvspace" TeX-arg-length)
   '("fbox" t)
   '("hspace*" TeX-arg-length)
   '("hspace" TeX-arg-length)
   '("mbox" t)
   '("newsavebox" TeX-arg-define-savebox)
   '("parbox" [ TeX-arg-tb ] [ "Height" ] [ TeX-arg-tb "Inner position" ]
     "Width" t)
   '("raisebox" "Raise" [ "Height above" ] [ "Depth below" ] t)
   '("rule" [ "Raise" ] "Width" "Thickness")
   '("sbox" TeX-arg-savebox t)
   '("usebox" TeX-arg-savebox)
   '("vspace*" TeX-arg-length)
   '("vspace" TeX-arg-length)
   '("documentstyle" TeX-arg-document)
   '("include" (TeX-arg-input-file "File" t))
   '("includeonly" t)
   '("input" TeX-arg-input-file)
   '("addcontentsline"
     (TeX-arg-eval completing-read "File: " '(("toc") ("lof") ("lot")))
     (TeX-arg-eval completing-read "Numbering style: " LaTeX-section-list) t)
   '("addtocontents"
     (TeX-arg-eval completing-read "File: " '(("toc") ("lof") ("lot"))) t)
   '("typeout" t)
   '("typein" [ TeX-arg-define-macro ] t)
   '("verb" TeX-arg-verb)
   '("verb*" TeX-arg-verb)
   '("extracolsep" t)
   '("index" TeX-arg-index)
   '("glossary" TeX-arg-index)
   '("numberline" "Section number" "Heading")
   '("caption" t)
   '("marginpar" [ "Left margin text" ] "Text")
   '("left" TeX-arg-insert-braces)
   ;; The following 4 macros are not specific to amsmath.
   '("bigl" TeX-arg-insert-braces)
   '("Bigl" TeX-arg-insert-braces)
   '("biggl" TeX-arg-insert-braces)
   '("Biggl" TeX-arg-insert-braces)

   '("langle" TeX-arg-insert-right-brace-maybe)
   '("lceil" TeX-arg-insert-right-brace-maybe)
   '("lfloor" TeX-arg-insert-right-brace-maybe)

   ;; These have no special support, but are included in case the
   ;; auto files are missing.

   "TeX" "LaTeX"
   "samepage" "newline"
   "smallskip" "medskip" "bigskip" "fill" "stretch"
   "thinspace" "negthinspace" "enspace" "enskip" "quad" "qquad"
   "nonumber" "centering" "raggedright"
   "raggedleft" "kill" "pushtabs" "poptabs" "protect" "arraystretch"
   "hline" "vline" "cline" "thinlines" "thicklines" "and" "makeindex"
   "makeglossary" "reversemarginpar" "normalmarginpar"
   "raggedbottom" "flushbottom" "sloppy" "fussy" "newpage"
   "clearpage" "cleardoublepage" "twocolumn" "onecolumn"

   "maketitle" "tableofcontents" "listoffigures" "listoftables"
   '("tiny" -1) '("scriptsize" -1) '("footnotesize" -1) '("small" -1)
   '("normalsize" -1) '("large" -1) '("Large" -1) '("LARGE" -1) '("huge" -1)
   '("Huge" -1)
   "pounds" "copyright"
   "hfil" "hfill" "vfil" "vfill" "hrulefill" "dotfill"
   "indent" "noindent" "today"
   "appendix"
   "dots"
   "makeatletter" "makeatother" "jobname")

  (when (string-equal LaTeX-version "2e")
    (LaTeX-add-environments
     '("filecontents" LaTeX-env-contents)
     '("filecontents*" LaTeX-env-contents))

    (TeX-add-symbols
     '("enlargethispage" TeX-arg-length)
     '("enlargethispage*" TeX-arg-length)
     '("tabularnewline" [ TeX-arg-length ])
     '("suppressfloats" [ TeX-arg-tb "Suppress floats position" ])
     '("ensuremath" "Math commands")
     '("textsuperscript" "Text")
     '("textcircled" "Text")
     '("mathring" t)

     "LaTeXe"
     "listfiles" "frontmatter" "mainmatter" "backmatter"
     "textcompwordmark" "textvisiblespace" "textemdash" "textendash"
     "textexclamdown" "textquestiondown" "textquotedblleft"
     "textquotedblright" "textquoteleft" "textquoteright"
     "textbullet" "textperiodcentered" "textasteriskcentered"
     "textbackslash" "textbar" "textless" "textgreater"
     "textasciicircum" "textasciitilde"
     "textregistered" "texttrademark"
     "rmfamily" "sffamily" "ttfamily" "mdseries" "bfseries"
     "itshape" "slshape" "upshape" "scshape"))

  (TeX-run-style-hooks "LATEX")

  (make-local-variable 'TeX-font-list)
  (make-local-variable 'TeX-font-replace-function)
  (if (string-equal LaTeX-version "2")
      ()
    (setq TeX-font-list LaTeX-font-list)
    (setq TeX-font-replace-function 'TeX-font-replace-macro)
    (TeX-add-symbols
     '("newcommand" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("renewcommand" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("providecommand" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("providecommand*" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("newcommand*" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("renewcommand*" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("usepackage" LaTeX-arg-usepackage)
     '("RequirePackage" LaTeX-arg-usepackage)
     '("ProvidesPackage" (TeX-arg-file-name-sans-extension "Package name")
       [ TeX-arg-conditional (y-or-n-p "Insert version? ")
			     ([ TeX-arg-version ]) nil])
     '("ProvidesClass" (TeX-arg-file-name-sans-extension "Class name")
       [ TeX-arg-conditional (y-or-n-p "Insert version? ")
			     ([ TeX-arg-version ]) nil])
     '("ProvidesFile" (TeX-arg-file-name "File name")
       [ TeX-arg-conditional (y-or-n-p "Insert version? ")
			     ([ TeX-arg-version ]) nil ])
     '("documentclass" TeX-arg-document)))

  (TeX-add-style-hook "latex2e"
		      ;; Use new fonts for `\documentclass' documents.
		      (lambda ()
			(setq TeX-font-list LaTeX-font-list)
			(setq TeX-font-replace-function 'TeX-font-replace-macro)
			(run-hooks 'LaTeX2e-hook))
		      LaTeX-dialect)

  (TeX-add-style-hook "latex2"
		      ;; Use old fonts for `\documentstyle' documents.
		      (lambda ()
			(setq TeX-font-list (default-value 'TeX-font-list))
			(setq TeX-font-replace-function
			      (default-value 'TeX-font-replace-function))
			(run-hooks 'LaTeX2-hook))
		      LaTeX-dialect)

  ;; There must be something better-suited, but I don't understand the
  ;; parsing properly.  -- dak
  (TeX-add-style-hook "pdftex" 'TeX-PDF-mode-on LaTeX-dialect)
  (TeX-add-style-hook "pdftricks" 'TeX-PDF-mode-on LaTeX-dialect)
  (TeX-add-style-hook "pst-pdf" 'TeX-PDF-mode-on LaTeX-dialect)
  (TeX-add-style-hook "dvips" 'TeX-PDF-mode-off LaTeX-dialect)
  ;; This is now done in style/pstricks.el because it prevents other
  ;; pstricks style files from being loaded.
  ;;   (TeX-add-style-hook "pstricks" 'TeX-PDF-mode-off)
  (TeX-add-style-hook "psfrag" 'TeX-PDF-mode-off LaTeX-dialect)
  (TeX-add-style-hook "dvipdf" 'TeX-PDF-mode-off LaTeX-dialect)
  (TeX-add-style-hook "dvipdfm" 'TeX-PDF-mode-off LaTeX-dialect)
  ;;  (TeX-add-style-hook "DVIoutput" 'TeX-PDF-mode-off)
  ;;
  ;;  Well, DVIoutput indicates that we want to run PDFTeX and expect to
  ;;  get DVI output.  Ugh.
  (TeX-add-style-hook "ifpdf" (lambda ()
				(TeX-PDF-mode-on)
				(TeX-PDF-mode-off)) LaTeX-dialect)
  ;; ifpdf indicates that we cater for either.  So calling both
  ;; functions will make sure that the default will get used unless the
  ;; user overrode it.

  (set (make-local-variable 'imenu-create-index-function)
       'LaTeX-imenu-create-index-function)

  (use-local-map LaTeX-mode-map)

  ;; Calling `easy-menu-add' may result in the menu filters being
  ;; executed which call `TeX-update-style'.  So this is placed very
  ;; late in mode initialization to assure that all relevant variables
  ;; are properly initialized before style files try to alter them.
  (easy-menu-add LaTeX-mode-menu LaTeX-mode-map)
  (easy-menu-add LaTeX-mode-command-menu LaTeX-mode-map)

  (define-key LaTeX-mode-map "\C-xne" 'LaTeX-narrow-to-environment)

  ;; AUCTeX's brace pairing feature (`LaTeX-electric-left-right-brace') doesn't
  ;; play nice with `electric-pair-mode' which is a global minor mode as of
  ;; emacs 24.4.
  (when (and LaTeX-electric-left-right-brace
	     (boundp 'electric-pair-mode))
    (set (make-local-variable 'electric-pair-mode) nil)))

(defun LaTeX-imenu-create-index-function ()
  "Imenu support function for LaTeX."
  (TeX-update-style)
  (let (entries level
	(regexp (LaTeX-outline-regexp)))
    (goto-char (point-max))
    (while (re-search-backward regexp nil t)
      (let* ((name (LaTeX-outline-name))
	     (level (make-string (1- (LaTeX-outline-level)) ?\ ))
	     (label (concat level level name))
	     (mark (make-marker)))
	(set-marker mark (point))
	(set-text-properties 0 (length label) nil label)
	(setq entries (cons (cons label mark) entries))))
    entries))

(defvar LaTeX-builtin-opts
  '("12pt" "11pt" "10pt" "twocolumn" "twoside" "draft")
  "Built in options for LaTeX standard styles.")

(defun LaTeX-209-to-2e ()
  "Make a stab at changing 2.09 doc header to 2e style."
  (interactive)
  (TeX-home-buffer)
  (let (optstr optlist 2eoptlist 2epackages docline docstyle)
    (goto-char (point-min))
    (if
	(search-forward-regexp
	 "\\documentstyle\\[\\([^]]*\\)\\]{\\([^}]*\\)}"
	 (point-max) t)
	(setq optstr (TeX-match-buffer 1)
	      docstyle (TeX-match-buffer 2)
	      optlist (TeX-split-string "," optstr))
      (if (search-forward-regexp
	   "\\documentstyle{\\([^}]*\\)}"
	   (point-max) t)
	  (setq docstyle (TeX-match-buffer 1))
	(error "No documentstyle defined")))
    (beginning-of-line 1)
    (setq docline (point))
    (insert "%%%")
    (while optlist
      (if (member (car optlist) LaTeX-builtin-opts)
	  (setq 2eoptlist (cons (car optlist) 2eoptlist))
	(setq 2epackages (cons (car optlist) 2epackages)))
      (setq optlist (cdr optlist)))
    ;;(message (format "%S %S" 2eoptlist 2epackages))
    (goto-char docline)
    (forward-line 1)
    (insert "\\documentclass")
    (if 2eoptlist
	(insert "["
		(mapconcat (lambda (x) x)
			   (nreverse 2eoptlist) ",") "]"))
    (insert "{" docstyle "}\n")
    (if 2epackages
	(insert "\\usepackage{"
		(mapconcat (lambda (x) x)
			   (nreverse 2epackages) "}\n\\usepackage{") "}\n"))
    (if (equal docstyle "slides")
      (progn
	(goto-char (point-min))
	(while (re-search-forward "\\\\blackandwhite{" nil t)
      (replace-match "\\\\input{" nil nil)))))
  (TeX-normal-mode nil))

(defun LaTeX-env-beginning-pos-col ()
  "Return a cons: (POINT . COLUMN) for current environment's beginning."
  (save-excursion
    (LaTeX-find-matching-begin)
    (cons (point) (current-column))))

(defun LaTeX-hanging-ampersand-position ()
  "Return indent column for a hanging ampersand (i.e. ^\\s-*&)."
  (destructuring-bind (beg-pos . beg-col)
      (LaTeX-env-beginning-pos-col)
    (let* ((cur-pos (point)))
      (save-excursion
        (if (re-search-backward "\\\\\\\\" beg-pos t)
            (let ((cur-idx (TeX-how-many "[^\\]&" (point) cur-pos)))
              (goto-char beg-pos)
              (re-search-forward "[^\\]&" cur-pos t (+ 1 cur-idx))
              (- (current-column) 1))
          (+ 2 beg-col))))))

(defun LaTeX-indent-tabular ()
  "Return indent column for the current tabular-like line."
  (destructuring-bind
   (beg-pos . beg-col)
   (LaTeX-env-beginning-pos-col)
   (let ((tabular-like-end-regex
	  (format "\\\\end{%s}"
		  (regexp-opt
		   (let (out)
		     (mapcar (lambda (x)
			       (when (eq (cadr x) 'LaTeX-indent-tabular)
				 (push (car x) out)))
			     LaTeX-indent-environment-list)
		     out)))))
     (cond ((looking-at tabular-like-end-regex)
	    beg-col)

	   ((looking-at "\\\\\\\\")
	    (+ 2 beg-col))

	   ((looking-at "&")
	    (LaTeX-hanging-ampersand-position))

	   (t
	    (+ 2
	       (let ((any-col (save-excursion
				(when (re-search-backward "\\\\\\\\\\|&" beg-pos t)
				  (current-column)))))
		 (if (and any-col (string= "&" (match-string 0)))
		     any-col
		   beg-col))))))))

(provide 'latex)

;;; latex.el ends here
