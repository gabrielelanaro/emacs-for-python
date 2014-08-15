;;; frenchb.el --- AUCTeX style for the `frenchb' babel option.

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-10-28
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

;; Set up AUCTeX for editing French text.  In particular for commands
;; provided by the `frenchb' option of the `babel' LaTeX package.  The
;; `frenchb' option is equivalent to the `francais' option and since
;; babel version 3.7j with the `french' option.  `french', however, is
;; ambiguous because another package by that name made by Bernard
;; Gaulle could be loaded.  In order to avoid this, either `frenchb'
;; (or `francais') or `frenchle' (or `frenchPRO') should be used.  See
;; the documentation of `frenchb' at
;; <URL:http://daniel.flipo.free.fr/frenchb/frenchb-doc.pdf>.

;;; Code:

(TeX-add-style-hook
 "frenchb"
 (lambda ()
   (TeX-add-symbols
    "og"
    "fg"
    "up"
    "ier"
    "iere"
    "iers"
    "ieres"
    "ieme"
    "iemes"
    '("bsc" t)
     "primo"
     "secundo"
     "tertio"
     "quarto"
     "No"
     "no"
     "degre"
     "degres"
     "DecimalMathComma"
     "StandardMathComma"
     '("nombre" "Nombre")
     "ThinSpaceInFrenchNumbers"
     "FrenchLayout"
     "StandardLayout")
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language
	   `("french" "\\og "
	     (lambda ()
	       (concat "\\fg"
		       (unless (member "xspace" TeX-active-styles) "{}")))
	     ,TeX-quote-after-quote)))
   (run-hooks 'TeX-language-fr-hook)))

;;; frenchb.el ends here
