;;; bulgarian.el --- AUCTeX style for the `bulgarian' babel option.

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2008-06-28
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

;; Set up AUCTeX for editing Bulgarian text in connection with the
;; `bulgarian' babel option.

;;; Code:

(defvar LaTeX-bulgarian-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `bulgarian.sty'.")

(modify-syntax-entry ?\" "w" LaTeX-bulgarian-mode-syntax-table)

(TeX-add-style-hook
 "bulgarian"
 (lambda ()
   (set-syntax-table LaTeX-bulgarian-mode-syntax-table)
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language
	   `("bulgarian" "\"`" "\"'" ,TeX-quote-after-quote)))
   (setq LaTeX-babel-hyphen-language "bulgarian")
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\"<" "\">" french)))
   (run-hooks 'TeX-language-bg-hook))
 LaTeX-dialect)

;;; bulgarian.el ends here
