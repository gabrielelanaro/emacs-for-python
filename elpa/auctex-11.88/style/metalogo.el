;;; metalogo.el --- AUCTeX style for `metalogo.sty' version 0.12.

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

;; This file adds support for the `metalogo.sty' version 0.12.

;;; Code:

(TeX-add-style-hook
 "metalogo"
 (lambda ()
   (TeX-add-symbols
    ;; Logos
    '("LaTeXe")
    '("XeTeX")
    '("XeLaTeX")
    '("LuaTeX")
    '("LuaLaTeX")
    ;; Commands
    '("setlogokern"
      (TeX-arg-eval completing-read "Kern parameters: "
		    '(("Te") ("eX") ("La") ("aT") ("Xe") ("eT") ("eL") ("X2")))
      (TeX-arg-length "Dimension"))
    '("setlogodrop"
      [TeX-arg-eval completing-read "Drop parameters: "
		    '(("TeX") ("Xe") ("XeTeX"))]
      (TeX-arg-length "Dimension"))
    '("setLaTeXa" 1)
    '("setLaTeXee" 1)
    '("seteverylogo" 1)
    '("everylogo" 1))

   ;; The main macros of this package are the logos, while fine-tuning commands
   ;; probably will be used only by expert users.
   (TeX-declare-expert-macros
    "metalogo"
    "setlogokern" "setlogodrop" "setLaTeXa" "setLaTeXee"
    "seteverylogo" "everylogo")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '( ;; Logos
				("LaTeXe")
				("XeTeX")
				("XeLaTeX")
				("LuaTeX")
				("LuaLaTeX")
				;; Commands
				("setlogokern" "{{")
				("setlogodrop" "[{")
				("setLaTeXa" "{")
				("setLaTeXee" "{")
				("seteverylogo" "{")
				("everylogo" "{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-metalogo-package-options nil
  "Package options for the metalogo package.")

;;; metalogo.el ends here
