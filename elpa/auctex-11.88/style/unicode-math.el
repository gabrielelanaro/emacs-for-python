;;; unicode-math.el --- AUCTeX style for `unicode-math.sty' version 0.7e.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
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

;; This file adds support for `unicode-math.sty' version 0.7e.

;;; Code:

(defvar LaTeX-unicode-math-package-options-list
  '(("math-style" ("ISO" "TeX" "french" "upright" "literal"))
    ("bold-style" ("ISO" "TeX" "upright" "literal"))
    ("sans-style" ("italic" "upright" "literal"))
    ("nabla" ("italic" "upright" "literal"))
    ("partial" ("upright" "italic" "literal"))
    ("vargreek-shape" ("unicode" "TeX"))
    ("colon" ("literal" "TeX"))
    ("slash-delimiter" ("ascii" "frac" "div")))
  "Package options for the unicode-math package.")

(defvar LaTeX-unicode-math-setmathfont-options
  (append LaTeX-unicode-math-package-options-list
	  '(("range")
	    ("script-font")
	    ("script-features")
	    ("sscript-font")
	    ("sscript-features")))
  "Options for the setmathfont macro of the unicode-math package.")

(TeX-add-style-hook
 "unicode-math"
 (lambda ()
   (TeX-run-style-hooks "ifxetex" "ifluatex" "expl3" "xparse" "l3keys2e"
			"fontspec" "catchfile" "fix-cm" "filehook")
   (TeX-add-symbols
    '("setmathfont" [TeX-arg-key-val LaTeX-unicode-math-setmathfont-options]
      "Math font name")
    '("unimathsetup" (TeX-arg-key-val LaTeX-unicode-math-package-options-list)))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setmathfont" "[{")
				("unimathsetup" "{"))
			      'function)))
 LaTeX-dialect)

(defun LaTeX-unicode-math-package-options ()
  "Prompt for package options for the unicode-math package."
  (TeX-read-key-val t LaTeX-unicode-math-package-options-list))

;;; unicode-math.el ends here
