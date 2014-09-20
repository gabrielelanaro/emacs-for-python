;;; units.el --- AUCTeX style for the LaTeX package `units.sty' (v0.9b)

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Christian Schlauer <cschl@arcor.de>
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

;; This file adds support for `units.sty'.

;;; Code:

(TeX-add-style-hook
 "units"
 (lambda ()
   (TeX-add-symbols
    '("unit" [ "Value" ] "Unit")
    '("unitfrac" [ "Value" ] "Unit in numerator" "Unit in denominator"))
   ;; units.sty requires the package nicefrac.sty, thus we enable the
   ;; macros of nicefrac.sty, too
   (TeX-run-style-hooks "nicefrac")
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("unit" "[{") ("unitfrac" "[{{")) 'textual))))

(defvar LaTeX-units-package-options '("tight" "loose")
  "Package options for the units package.")

;;; units.el ends here
