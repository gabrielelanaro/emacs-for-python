;;; bigstrut.el --- AUCTeX style for `bigstrut.sty'

;; Copyright (C) 2012 Free Software Foundation, Inc.

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

;; This file adds support for `bigstrut.sty'.

;;; Code:

(TeX-add-style-hook
 "bigstrut"
 (lambda ()
   (TeX-add-symbols
    "bigstrutsetup"
    '("bigstrut" [ TeX-arg-bigstrut ])))
 LaTeX-dialect)

(defun TeX-arg-bigstrut (optional &optional prompt)
  "Prompt for the optional argument in \\bigstrut"
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt
     optional "Strut to top (t) or bottom (b)" nil t)
     (mapcar 'list '("t" "b")) nil t) optional))

(defvar LaTeX-bigstrut-package-options nil
  "Package options for the bigstrut package.")

;;; bigstrut.el ends here
