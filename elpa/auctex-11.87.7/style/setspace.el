;;; setspace.el --- AUCTeX style for `setspace.sty'

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2011-04-16
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

;; This file adds support for `setspace.sty'.

;;; Code:

(TeX-add-style-hook
 "setspace"
 (lambda ()
   (TeX-add-symbols
    '("setstretch" "Stretch")
    '("setdisplayskipstretch" "Stretch")
    '("SetSinglespace" "Stretch")
    '("onehalfspacing" 0)
    '("doublespacing" 0)
    '("singlespacing" 0))

   (LaTeX-add-environments 
    '("spacing" "Stretch")
    "singlespace"
    "singlespace*"
    "onehalfspace"
    "doublespace")

   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("singlespacing" "")
				("doublespacing" "")
				("onehalfspacing" ""))
                              'function))))

(defvar LaTeX-setspace-package-options 
  '("doublespacing" "onehalfspacing" "singlespacing" "nodisplayskipstretch")
  "Package options for the setspace package.")

;;; setspace.el ends here
