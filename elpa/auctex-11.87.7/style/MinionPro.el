;;; MinionPro.el -- AUCTeX style for MinionPro.sty

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-11-26
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

;; This file adds support for `MinionPro.sty' (v2.0). 

;;; Code

(TeX-add-style-hook
 "MinionPro"
 (lambda ()
   (TeX-add-symbols
    ;; New symbols
    '("figureversion"
      (TeX-arg-eval completing-read "Figure style: "
		    '(("text") ("osf")
		      ("lining") ("lf")
		      ("tabular") ("tab")
		      ("proportional") ("prop"))))
    '("smallfrac" "Numerator" "Denominator")
    '("slantfrac" "Numerator" "Denominator")
    ;; IMHO they should be added to the other \text.. and \..shape commands
    '("textsw" 1)
    '("textssc" 1)
    "sscshape"
    "swshape")
   ;; Run style hook for amsmath which is loaded via MnSymbol
   (TeX-run-style-hooks "amsmath")
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("smallfrac" "{{")
				("slantfrac" "{{")
				("textsw" "{")
				("textssc" "{"))
			      'textual)
     (font-latex-add-keywords '(("figureversion" "{")) 'variable))))

(defvar LaTeX-MinionPro-package-options
  '("smallfamily" "medfamily" "fullfamily" "noopticals" "opticals"
    "slides" "textosf" "mathosf" "osf" "textlf" "mathlf" "lf"
    "mathtabular" "mnsy" "cmsy" "swash" "abx" "amsbb" "fourierbb"
    "lucidabb" "mixedgreek" "italicgreek" "frenchmath" "minionint"
    "footnotefigures")
"Package options for the MinionPro package.")

;;; MinionPro.el ends here
