;;; polski.el --- AUCTeX style for `polski.sty'.

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-01-11
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

;; Set up AUCTeX for editing Polish text in connection with
;; `polski.sty'.

;;; Code:

(defvar LaTeX-polski-package-options
  '("plmath" "nomathsymbols" "MeX" "T1" "QX" "OT1" "OT4" "prefixinginverb"
    "noprefixinginverb" "roku" "r." "noroku")
  "Package options for polski.sty.")

(TeX-add-style-hook
 "polski"
 (lambda ()
   (TeX-add-symbols
    ;; Dashes
    "dywiz"
    "pauza"
    "ppauza")
   ;; Quotation marks
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language '("polski" ",," "''" t)))
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '(",," "''")))
   (run-hooks 'TeX-language-pl-hook)))

;;; polski.el ends here
