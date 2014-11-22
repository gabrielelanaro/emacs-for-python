;;; tex-site.el - Site specific variables.  Don't edit.

;; Copyright (C) 2005, 2013 Free Software Foundation, Inc.
;;
;; completely rewritten.

;; Author: David Kastrup <dak@gnu.org>
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

;; This file contains startup code, autoloads and variables adapted to
;; the local site configuration.  It is generated and placed by the
;; installation procedure and should not be edited by hand, nor moved
;; to a different place, as some settings may be established relative
;; to the file.

;; All user customization should be done with
;; M-x customize-variable RET

;;; Code:

(if (< emacs-major-version 21)
  (error "AUCTeX requires Emacs 21 or later"))

;; Define here in order for `M-x customize-group <RET> AUCTeX <RET>'
;; to work if the main AUCTeX files are not loaded yet.
(defgroup AUCTeX nil
  "A (La)TeX environment."
  :tag "AUCTeX"
  :link '(custom-manual "(auctex)Top")
  :link '(url-link :tag "Home Page" "http://www.gnu.org/software/auctex/")
  :prefix "TeX-"
  :group 'tex
  :load "tex" :load "latex" :load "tex-style")

(defvar TeX-lisp-directory
  (file-name-directory load-file-name)
  "The directory where most of the AUCTeX lisp files are located.
For the location of lisp files associated with
styles, see the variables TeX-style-* (hand-generated lisp) and
TeX-auto-* (automatically generated lisp).")

(add-to-list 'load-path TeX-lisp-directory)

(defvar TeX-data-directory
  (file-name-directory load-file-name)
  "The directory where the AUCTeX non-Lisp data is located.")

(defcustom TeX-auto-global
    (if (file-writable-p "/usr/local/var/auctex") "/usr/local/var/auctex" "~/.emacs.d/auctex")
  "*Directory containing automatically generated information.
Must end with a directory separator.

For storing automatic extracted information about the TeX macros
shared by all users of a site."
  :group 'TeX-file
  :type 'directory)

(defconst TeX-mode-alist
  '((tex-mode . tex-mode)
    (plain-tex-mode . tex-mode)
    (texinfo-mode . texinfo)
    (latex-mode . tex-mode)
    (doctex-mode . tex-mode))
  "Alist of built-in TeX modes and their load files.")

(defalias 'TeX-load-hack 'ignore)

(add-hook 'tex-site-unload-hook
	  (lambda ()
	    (let ((list after-load-alist))
	      (while list
		;; Adapted copy of the definition of `assq-delete-all'
		;; from Emacs 21 as substitute for
		;; `(assq-delete-all'TeX-modes-set (car list))' which
		;; fails on non-list elements in Emacs 21.
		(let* ((alist (car list))
		       (tail alist)
		       (key 'TeX-modes-set))
		  (while tail
		    (if (and (consp (car tail))
			     (eq (car (car tail)) key))
			(setq alist (delq (car tail) alist)))
		    (setq tail (cdr tail))))
		(setq list (cdr list))))
	    (setq load-path (delq TeX-lisp-directory load-path))))

(defun TeX-modes-set (var value &optional update)
  "Set VAR (which should be `TeX-modes') to VALUE.

This places either the standard or the AUCTeX versions of
functions into the respective function cell of the mode.
If UPDATE is set, a previously saved value for
the non-AUCTeX function gets overwritten with the current
definition."
  (custom-set-default var value)
  (let ((list TeX-mode-alist) elt)
    (while list
      (setq elt (car (pop list)))
      (let ((dst (intern (concat "TeX-" (symbol-name elt)))))
        (if (fboundp 'advice-add)
            (if (memq elt value)
                (advice-add elt :override dst)
              (advice-remove elt dst))
          (when (or update (null (get elt 'tex-saved)))
            (when (fboundp elt)
              (put elt 'tex-saved (symbol-function elt))))
          (defalias elt
            (if (memq elt value)
                dst
              (get elt 'tex-saved))))))))

(defcustom TeX-modes
  (mapcar 'car TeX-mode-alist)
  "List of modes provided by AUCTeX.

This variable can't be set normally; use customize for that, or
set it with `TeX-modes-set'."
  :type (cons 'set
	      (mapcar (lambda(x) (list 'const (car x))) TeX-mode-alist))
  :set 'TeX-modes-set
  :group 'AUCTeX
  :initialize(lambda (var value)
	       (custom-initialize-reset var value)
	       (unless (fboundp 'advice-add)
		 (let ((list TeX-mode-alist))
		   (while list
		     (eval-after-load (cdar list)
		       `(TeX-modes-set ',var ,var t))
		     (setq list (cdr list)))))) )

(defconst AUCTeX-version "11.88"
    "AUCTeX version.
If not a regular release, the date of the last change.")

(defconst AUCTeX-date "2014-10-29"
  "AUCTeX release date using the ISO 8601 format, yyyy-mm-dd.")

;; Store bibitems when saving a BibTeX buffer
(add-hook 'bibtex-mode-hook 'BibTeX-auto-store)

;;; Code specific to ELPA packaging:

;; From preview-latex.el:

(defvar preview-TeX-style-dir
  (expand-file-name "latex" (file-name-directory load-file-name)))

;;; Ensure that loading the autoloads file also loads this file.
;;;###autoload (require 'tex-site)

(provide 'tex-site)
;;; tex-site.el ends here
