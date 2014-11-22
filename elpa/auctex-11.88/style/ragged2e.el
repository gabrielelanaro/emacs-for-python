;;; ragged2e.el --- AUCTeX style for `ragged2e.sty'

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

;; This file adds support for `ragged2e.sty'.

;;; Code:

(TeX-add-style-hook
 "ragged2e"
 (lambda ()
   (TeX-add-symbols
    "CenteringLeftskip"
    "RaggedLeftLeftskip"
    "RaggedRightLeftskip"
    "CenteringRightskip"
    "RaggedLeftRightskip"
    "RaggedRightRightskip"
    "CenteringParfillskip"
    "RaggedLeftParfillskip"
    "RaggedRightParfillskip"
    "CenteringParindent"
    "RaggedLeftParindent"
    "RaggedRightParindent"
    "JustifyingParfillskip"
    "JustifyingParindent"
    "ttraggedright"
    "Centering"
    "justifying"
    "RaggedRight"
    "RaggedLeft")

   (LaTeX-add-environments 
    "FlushLeft" "FlushRight" "Center" "justify")

   (TeX-run-style-hooks "footmisc" "everysel"))
 LaTeX-dialect)

(defvar LaTeX-ragged2e-package-options 
  '("originalcommands" "newcommands" "originalparameters" "document"
    "newparameters" "footnotes" "raggedrightboxes")
  "Package options for the ragged2e package.")

;;; ragged2e.el ends here
