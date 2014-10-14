;;; nomencl.el --- AUCTeX style for the nomencl class.

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-10-09
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

;; This file adds support for the nomencl package.

;;; Code:

(defvar LaTeX-nomencl-package-options
  '("refeq" "norefeq" "refpage" "norefpage" "prefix" "noprefix" "cfg" "nocfg"
    "intoc" "notintoq" "compatible" "noncompatible" "croatian" "danish"
    "english" "french" "german" "italian" "polish" "portuguese" "russian"
    "spanish" "ukrainian")
  "Package options for the nomencl package.")

(TeX-add-style-hook
 "nomencl"
 (lambda ()
   (TeX-add-symbols
    '("makenomenclature" 0)
    '("printnomenclature" ["Label width"])
    '("nomenclature" ["Prefix"] "Symbol" "Description")
    "nomrefeq"
    "nomrefpage"
    "nomrefeqpage"
    "nomnorefeq"
    "nomnorefpage"
    "nomnorefeqpage"
    '("nomlabelwidth" 0)
    '("nomname" 0)
    '("nomgroup" 0)
    '("nompreamble" 0)
    '("nompostamble" 0)
    '("nomitemsep" 0)
    '("nomprefix" 0)
    '("nomlabel" 0)
    '("nomentryend" 0)
    '("eqdeclaration" 0)
    '("pagedeclaration" 0))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("nomenclature" "[{{"))
			      'reference))))

;;; nomencl.el ends here
