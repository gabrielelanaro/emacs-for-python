;;; tex-bar.el --- toolbar icons on AUCTeX in GNU emacs and XEmacs

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301 USA

;; Author: Miguel V. S. Frasson <frasson@math.leidenuniv.nl>
;; Keywords: tool-bar, tex, latex

;;; Commentary:
;;

;; This package also needs `toolbar-x.el', and `latex.el' for the
;; symbol-toolbar.

;;; Use of this preliminary version:

;; -  Add `LaTeX-install-toolbar' to `LaTeX-mode-hook'.

;; Special requirements for the use of experimental symbol-toolbar:

;; -  Customize `TeX-bar-LaTeX-buttons', adding the label
;;    `LaTeX-symbols-experimental' at the end.

;; -  You should have a folder called "symb-pics" with the pics of the
;;    symbols (xpm format is a good one), and the *parent* of this
;;    folder should be in `load-path'.

;; Did you read carefully this item?  I will say again: the folder
;; "symb-pics" should *not* be in `load-path', but its *parent*.

;; -  each image file is named after the command that it represents in
;;    the following rules: the base name is the name of the command
;;    without the escape character "\", like \delta -> "delta.xpm";
;;    however, since in some OS filenames are case insensitive, all
;;    occurences of capital letter should be replaced by the letter
;;    plus a dash: \Rightarrow -> "R-ightarrow.xpm" --- actually, for
;;    the correct name, apply `TeX-bar-img-filename' to "Rightarrow"
;;     (TeX-bar-img-filename "Rightarrow")
;;            -->  "R-ightarrow"
;;    The function `TeX-bar-img-filename' also treats special commands
;;    like `\{', `\|', etc.

;; You can get the symbol images on (temporary solution)
;;    http://www.math.leidenuniv.nl/~frasson/symb-pics.tar.gz

;;; Code:

(require 'custom)

(require 'toolbar-x)

;; for error handling
(require 'tex-buf)

;; For the symbol toolbar
(require 'latex)

;;; Standard buttons

;; help strings
(defun TeX-bar-help-from-command-list (item)
  "Return the help string of ITEM in `TeX-command-list'.
If there is no help, the empty string is returned."
  (let ((help (nth 1 (memq :help (assoc item TeX-command-list)))))
    (if help help "")))

(defgroup TeX-tool-bar nil
  "Tool bar support in AUCTeX."
  :group 'AUCTeX)

(defcustom TeX-bar-TeX-buttons
  '(new-file open-file dired kill-buffer save-buffer cut copy paste undo
	     [separator nil] tex next-error view bibtex)
  "List of buttons available in `tex-mode'.
It should be a list in the same format of the BUTTONS parameter
in function `toolbarx-install-toolbar', often a symbol that
labels a button or Emacs/XEmacs choice of buttons.

Type `\\[TeX-bar-TeX-buttons]' for a list of available buttons.

Buttons are defined in alists (labels associated to properties
that define a button).  For a list of variables that hold such
alists, see variable `TeX-bar-TeX-all-button-alists'."
  :type '(list (set :inline t
		    (const new-file)
		    (const open-file)
		    (const dired)
		    (const kill-buffer)
		    (const save-buffer)
		    (const write-file)
		    (const undo)
		    (const cut)
		    (const copy)
		    (const paste)
		    (const search-forward)
		    (const print-buffer)
		    (const [separator nil])
		    (const tex)
		    (const next-error)
		    (const view)
		    (const file)
		    (const bibtex)
		    (const clean))
		    ;; (const latex-symbols-experimental)
	       (repeat (choice (symbol :tag "Label")
			       (vector :args ((symbol :tag "Label in Emacs ")
					      (symbol :tag "Label in XEmacs"))
				       :tag "Emacs/XEmacs choice")
			       (sexp :tag "General element"))))
  :group 'TeX-tool-bar)

(defun TeX-bar-TeX-buttons ()
  "Display in a buffer a list of buttons for `tex-bar.el'."
  (interactive)
  (let ((assqs-button-alists)
	(labels))
    (dolist (m-alist TeX-bar-TeX-all-button-alists)
      (setq labels nil)
      (dolist (as (eval m-alist))
	(setq labels (cons (car as) labels)))
      (setq assqs-button-alists (cons (cons m-alist (nreverse labels))
				       assqs-button-alists)))
    (setq assqs-button-alists (nreverse assqs-button-alists))
    ;; displaying results
    (save-excursion
      (set-buffer (get-buffer-create "*TeX tool bar buttons*"))
      (erase-buffer)
      (insert "Available buttons for TeX mode
================================")
      (dolist (i assqs-button-alists)
	(insert (format "\n\n`%s' provides the following buttons:\n  " (car i)))
	(dolist (j (cdr i))
	  (insert (format " %s" j)))
	(fill-region (point-at-bol) (point-at-eol))))
    (display-buffer "*TeX tool bar buttons*" t)))

(defgroup TeX-tool-bar-button-definitions nil
  "Collections of button definitions."
  :group 'TeX-tool-bar)

(defcustom TeX-bar-TeX-all-button-alists
  '(TeX-bar-TeX-button-alist
    toolbarx-default-toolbar-meaning-alist)
  "List of variables that hold buttons properties.
Each element should be a symbol bound to list in the format of
the argument BUTTON-ALIST in function `toolbarx-install-toolbar'."
  :type '(repeat variable)
  :group 'TeX-tool-bar-button-definitions)

(defcustom TeX-bar-TeX-button-alist
  '((tex :image (lambda nil (if TeX-PDF-mode "pdftex" "tex"))
	 :command (progn
		    (TeX-save-document (TeX-master-file))
		    (TeX-command "TeX" 'TeX-master-file -1))
	 :help (lambda (&rest ignored)
		 (TeX-bar-help-from-command-list "TeX")))
    (pdftex :image "pdftex"
	    :command (progn
		       (TeX-save-document (TeX-master-file))
		       (TeX-command "PDFTeX" 'TeX-master-file -1))
	    :help (lambda (&rest ignored)
		    (TeX-bar-help-from-command-list "PDFTeX")))
    (next-error :image "error"
		:command TeX-next-error
		:enable (plist-get TeX-error-report-switches
				   (intern (TeX-master-file)))
		:visible (plist-get TeX-error-report-switches
				    (intern (TeX-master-file))))
    (view :image (lambda nil (if TeX-PDF-mode "viewpdf" "viewdvi"))
	  :command (TeX-command "View" 'TeX-master-file -1)
	  :help (lambda (&rest ignored)
		  (TeX-bar-help-from-command-list "View")))
    (file :image "dvips"
	  :command (TeX-command "File" 'TeX-master-file -1)
	  :visible (not TeX-PDF-mode)
	  :help (lambda (&rest ignored)
		  (TeX-bar-help-from-command-list "File")))
    (bibtex :image "bibtex"
	    :command (TeX-command "BibTeX" 'TeX-master-file -1)
	    :help (lambda (&rest ignored)
		    (TeX-bar-help-from-command-list "BibTeX")))
    (clean  :image "delete"
	    :command (TeX-command "Clean" 'TeX-master-file -1)
	    :help (lambda (&rest ignored)
		    (TeX-bar-help-from-command-list "Clean"))))
  ;; latex-symbols-experimental?
  "Alist for button definitions in TeX bar.
Value should le a list where each element is of format (KEY .
PROPS), where KEY is a symbol that labels the button and PROPS is
a list of properties of the button.  For a description of the
format of PROPS, please see documentation of function
`toolbarx-install-toolbar'.  This custom variable is in the same
format of the argument MEANING-ALIST in the mentioned function."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'TeX-tool-bar-button-definitions)

;;; Installation of the tool bar
;;;###autoload
(defun TeX-install-toolbar ()
  "Install toolbar buttons for TeX mode."
  (interactive)
  (require 'toolbar-x)
  (add-to-list 'toolbarx-image-path
	       (expand-file-name "images" TeX-data-directory))
  (add-hook 'TeX-PDF-mode-hook 'toolbarx-refresh nil t)
  (toolbarx-install-toolbar TeX-bar-TeX-buttons
			    (let ((append-list))
			      (dolist (elt TeX-bar-TeX-all-button-alists)
				(setq append-list (append append-list
							  (eval elt))))
			      append-list)))

(defcustom TeX-bar-LaTeX-buttons
  '(new-file open-file dired kill-buffer save-buffer cut copy paste undo
	      [separator nil] latex next-error view bibtex)
  "List of buttons available in `latex-mode'.
It should be a list in the same format of the BUTTONS parameter
in function `toolbarx-install-toolbar', often a symbol that
labels a button or Emacs/XEmacs choice of buttons.

Type `\\[TeX-bar-LaTeX-buttons]' for a list of available buttons.

Buttons are defined in alists (labels associated to properties
that define a button).  For a list of variables that hold such
alists, see variable `TeX-bar-LaTeX-all-button-alists'."
  :type '(list (set :inline t
		    (const new-file)
		    (const open-file)
		    (const dired)
		    (const kill-buffer)
		    (const save-buffer)
		    (const write-file)
		    (const undo)
		    (const cut)
		    (const copy)
		    (const paste)
		    (const search-forward)
		    (const print-buffer)
		    (const [separator nil])
		    (const latex)
		    (const next-error)
		    (const view)
		    (const file)
		    (const bibtex)
		    (const clean)
		    (const latex-symbols-experimental))
	       (repeat (choice (symbol :tag "Label")
			       (vector :args ((symbol :tag "Label in Emacs ")
					      (symbol :tag "Label in XEmacs"))
				       :tag "Emacs/XEmacs choice")
			       (sexp :tag "General element"))))
  :group 'TeX-tool-bar)

(defun TeX-bar-LaTeX-buttons ()
  "Display in a buffer a list of buttons for `tex-bar.el'."
  (interactive)
  (let ((assqs-button-alists)
	(labels))
    (dolist (m-alist TeX-bar-LaTeX-all-button-alists)
      (setq labels nil)
      (dolist (as (eval m-alist))
	(setq labels (cons (car as) labels)))
      (setq assqs-button-alists (cons (cons m-alist (nreverse labels))
				       assqs-button-alists)))
    (setq assqs-button-alists (nreverse assqs-button-alists))
    ;; displaying results
    (save-excursion
      (set-buffer (get-buffer-create "*TeX tool bar buttons*"))
      (erase-buffer)
      (insert "Available buttons for LaTeX mode
================================")
      (dolist (i assqs-button-alists)
	(insert (format "\n\n`%s' provides the following buttons:\n  " (car i)))
	(dolist (j (cdr i))
	  (insert (format " %s" j)))
	(fill-region (point-at-bol) (point-at-eol))))
    (display-buffer "*TeX tool bar buttons*" t)))

(defgroup TeX-tool-bar-button-definitions nil
  "Collections of button definitions."
  :group 'TeX-tool-bar)

(defcustom TeX-bar-LaTeX-all-button-alists
  '(TeX-bar-LaTeX-button-alist
    toolbarx-default-toolbar-meaning-alist)
  "List of variables that hold buttons properties.
Each element should be a symbol bound to list in the format of
the argument BUTTON-ALIST in function `toolbarx-install-toolbar'."
  :type '(repeat variable)
  :group 'TeX-tool-bar-button-definitions)

(defcustom TeX-bar-LaTeX-button-alist
  '((latex :image (lambda nil (if TeX-PDF-mode "pdftex" "tex"))
	   :command (progn
		      (TeX-save-document (TeX-master-file))
		      (TeX-command "LaTeX" 'TeX-master-file -1))
	   :help (lambda (&rest ignored)
		   (TeX-bar-help-from-command-list "LaTeX")))
    (pdflatex :image "pdftex"
	      :command (progn
			 (TeX-save-document (TeX-master-file))
			 (TeX-command "PDFLaTeX" 'TeX-master-file -1))
	      :help (lambda (&rest ignored)
		      (TeX-bar-help-from-command-list "PDFLaTeX")))
    (next-error :image "error"
		:command TeX-next-error
		:enable (plist-get TeX-error-report-switches
				   (intern (TeX-master-file)))
		:visible (plist-get TeX-error-report-switches
				    (intern (TeX-master-file))))
    (view :image (lambda nil (if TeX-PDF-mode "viewpdf" "viewdvi"))
	  :command (TeX-command "View" 'TeX-master-file -1)
	  :help (lambda (&rest ignored)
		  (TeX-bar-help-from-command-list "View")))
    (file :image "dvips"
	  :command (TeX-command "File" 'TeX-master-file -1)
	  :visible (not TeX-PDF-mode)
	  :help (lambda (&rest ignored)
		  (TeX-bar-help-from-command-list "File")))
    (bibtex :image "bibtex"
	    :command (TeX-command "BibTeX" 'TeX-master-file -1)
	    :help (lambda (&rest ignored)
		    (TeX-bar-help-from-command-list "BibTeX")))
    (clean  :image "delete"
	    :command (TeX-command "Clean" 'TeX-master-file -1)
	    :help (lambda (&rest ignored)
		    (TeX-bar-help-from-command-list "Clean")))
    (latex-symbols-experimental . (:alias :eval-group
					  LaTeX-symbols-toolbar-switch-contents
					  LaTeX-symbols-toolbar-contents)))
  "Alist for button definitions in TeX bar.
Value should le a list where each element is of format (KEY .
PROPS), where KEY is a symbol that labels the button and PROPS is
a list of properties of the button.  For a description of the
format of PROPS, please see documentation of function
`toolbarx-install-toolbar'.  This custom variable is in the same
format of the argument MEANING-ALIST in the mentioned function."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'TeX-tool-bar-button-definitions)

;;; Installation of the tool bar
;;;###autoload
(defun LaTeX-install-toolbar ()
  "Install toolbar buttons for LaTeX mode."
  (interactive)
  (require 'toolbar-x)
  (add-to-list 'toolbarx-image-path
	       (expand-file-name "images" TeX-data-directory))
  (add-hook 'TeX-PDF-mode-hook 'toolbarx-refresh nil t)
  (toolbarx-install-toolbar TeX-bar-LaTeX-buttons
			    (let ((append-list))
			      (dolist (elt TeX-bar-LaTeX-all-button-alists)
				(setq append-list (append append-list
							  (eval elt))))
			      append-list)))

;;; Experimental Symbol Toolbar

;;; symbol toolbar
(defun TeX-bar-img-filename (tex-command)
  "Return the filename (no extension) for the image button of TEX-COMMAND."
  (let ((str-list (append tex-command nil))
	(str-result))
    (dolist (i str-list)
      (cond
       ;; capital letter -> letter + "-"
       ((and (>= i ?A) (<= i ?Z))
	(setq str-result (cons ?- (cons i str-result))))
       ;; lowercase letter -> letter
       ((and (>= i ?a) (<= i ?z))
        (setq str-result (cons i str-result)))
       ;; open curly brackets `{' -> "ocb--"
       ((eq i ?{)
	(setq str-result (cons ?o str-result))
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?b str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; close curly brackets `}' -> "ccb--"
       ((eq i ?})
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?c str-result))
	(setq str-result (cons ?b str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; vertical bar `|' -> "v--"
       ((eq i ?|)
	(setq str-result (cons ?v str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))
       ;; slash `/' -> "s--"
       ((eq i ?/)
	(setq str-result (cons ?s str-result))
	(setq str-result (cons ?- str-result))
	(setq str-result (cons ?- str-result)))))
    (concat (nreverse str-result))))

(let* ((menu-strings-buttons-alist
	;; make a alist os strings with the symbol classes and store it in
	;; `menu-strings-alist'
	(let* ((menu-strings-alist-temp))
	  (dolist (item-external (cdr LaTeX-math-menu)
				 (nreverse menu-strings-alist-temp))
	    (when (listp item-external)
	      ;; if first element is vector, I am supposing that all are
	      ;; vectors as well
	      (if (vectorp (cadr item-external))
		  (let* ((menu-str (car item-external))
			 (menu-buttons))
		    (dolist (button (cdr item-external))
		      (setq menu-buttons
			    (cons (list (intern (TeX-bar-img-filename
						 (aref button 0)))
					:image
					(concat "symb-pics/"
						(TeX-bar-img-filename
						 (aref button 0)))
					:help (aref button 0)
					:command (aref button 1))
				  menu-buttons)))
		    (setq menu-buttons (nreverse menu-buttons))
		    (setq menu-strings-alist-temp
			  (cons (cons menu-str (list menu-buttons))
				menu-strings-alist-temp)))
		;; if another list (therefore, up to second level menu)
		(let ((parent-str (concat (car item-external) " ")))
		  (dolist (item-internal (cdr item-external))
		    (unless (equal (car item-internal) "Special")
		      (let* ((menu-str (concat parent-str
					       (car item-internal)))
			     (menu-buttons))
			(dolist (button (cdr item-internal))
			  (setq menu-buttons
				(cons (list (intern (aref button 0))
					    :image
					    (concat "symb-pics/"
						    (TeX-bar-img-filename
						     (aref button 0)))
					    :help (aref button 0)
					    :command (aref button 1))
				      menu-buttons)))
			(setq menu-buttons (nreverse menu-buttons))
			(setq menu-strings-alist-temp
			      (cons (cons menu-str (list menu-buttons))
				    menu-strings-alist-temp)))))))))))
       (list-strings (let* ((list-str-temp))
		       (dolist (i menu-strings-buttons-alist
				  (nreverse list-str-temp))
			 (setq list-str-temp (cons (car i)
						   list-str-temp))))))
  (defvar LaTeX-symbols-toolbar-visible-flag nil
    "Non-nil means that the LaTeX symbols on toolbar are visible.
Internal variable.")
  (defconst LaTeX-symbols-toolbar-switch-contents
    `(;; the on-off switch button
      (latex-symbols-switch
       :image (lambda nil (if LaTeX-symbols-toolbar-visible-flag
			      "ltx-symb-turn-off"
			    "ltx-symb-turn-on"))
       :command (progn
		  (setq LaTeX-symbols-toolbar-visible-flag
			(not LaTeX-symbols-toolbar-visible-flag))
		  (toolbarx-refresh))
       ;; help message depends on if symb-toolbar is on or off, and in
       ;; the name of the current class of symbols
       :help (lambda (&rest ignore)
	       (concat "Turn "
		       (if LaTeX-symbols-toolbar-visible-flag "off " "on ")
		       "the toolbar of LaTeX symbols (current class: "
		       (nth (1- LaTeX-symbols-active-menuitem)
			    (quote ,list-strings))
		       ")")))
      ;; the dropdown button, that also switch on the symbols
      ,(append '(:dropdown-group)
	       list-strings
	       '(:variable
		 LaTeX-symbols-active-menuitem
		 :save offer
		 :dropdown-prepend-command
		 (setq LaTeX-symbols-toolbar-visible-flag t)
		 :dropdown-help "Select a class of symbols to be displayed"))))
  (defconst LaTeX-symbols-toolbar-contents
    (let* ((ltx-symb)
	   (count 0))
      (dolist (i menu-strings-buttons-alist
		 (append (nreverse ltx-symb)
			 '(:insert
			   LaTeX-symbols-toolbar-visible-flag
			   :toolbar (bottom . top))))
	(setq count (1+ count))
	(setq ltx-symb
	      (cons (append (cdr i)
			    `(:insert (eq LaTeX-symbols-active-menuitem
					  ,count)))
		    ltx-symb))))))

(provide 'tex-bar)

;;; tex-bar.el ends here
