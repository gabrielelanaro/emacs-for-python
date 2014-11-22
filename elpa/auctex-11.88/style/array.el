;;; array.el --- AUCTeX style for `array.sty'

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
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

;; This file adds support for `array.sty'

;;; Code:

(TeX-add-style-hook
 "array"
 (lambda ()
   (TeX-add-symbols
    '("newcolumntype" "Column type" [ "Number of arguments" ] t)
    '("showcols" 0)
    '("firsthline" 0)
    '("lasthline" 0))

   ;; `array.sty' adds a couple of new lengths.  They're added here, rather than
   ;; in the `TeX-add-symbols' block.
   (LaTeX-add-lengths "extratabsurround" "extrarowheight")

   ;; `array.sty' adds some new column specification letters.
   (set (make-local-variable 'LaTeX-array-column-letters) "clrpmb"))
 LaTeX-dialect)

(defvar LaTeX-array-package-options nil
  "Package options for array.")

;; array.el ends here
