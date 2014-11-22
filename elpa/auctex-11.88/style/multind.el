;;; multind.el --- AUCTeX support for multiple indices with multind.sty.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;; Maintainer: auctex-devel@gnu.org

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

;;; Code:

(TeX-add-style-hook "multind"
  (lambda ()

    ;; Commands
    (TeX-add-symbols
     '("makeindex" "Indextag")
     '("index" TeX-arg-index-tag TeX-arg-index)
     '("printindex" TeX-arg-index-tag "Title")
     "printindex" "indexspace")

    ;; Parsing index macros
    (setq LaTeX-auto-regexp-list
	  (append
	   ;; The first regexp is faster, but less accurate
	   ;; '(("\\\\index\\*?{[^{}]*}{\\([^}]*\\)" 1 LaTeX-auto-index-entry))
	   ;; The second regexp is very good, but slower
	   '(("\\\\index\\*?{[^{}]*}{\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}"
	      1 LaTeX-auto-index-entry))
	   LaTeX-auto-regexp-list))

    ;; Completion for index entries in the |see and \index commands
    (setq TeX-complete-list 
	  (append
	   '(("\\\\index{[^{}]*}{\\([^{}\n\r]*\\)" 1 LaTeX-index-entry-list)
	     ("|see{\\([^}]*\\)" 1 LaTeX-index-entry-list))
	   TeX-complete-list))

    ;; RefTeX support
    (and (fboundp 'reftex-add-index-macros)
	 (reftex-add-index-macros '(multind))))
  LaTeX-dialect)

(defvar LaTeX-multind-package-options nil
  "Package options for the multind package.")

;;; multind.el ends here
