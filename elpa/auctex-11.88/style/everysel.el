;;; everysel.el --- AUCTeX style for `everysel.sty'

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2012-12-25
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

;; This file adds support for `everysel.sty'.

;;; Code:

(TeX-add-style-hook
 "everysel"
 (lambda ()
   (TeX-add-symbols
    ;; adds a hook (the argument code) to be called after \\selectfont
    '("EverySelectfont" 1)
    ;; adds a hook to be called after the next \\selectfont
    '("AtNextSelectont" 1)))
 LaTeX-dialect)

(defvar LaTeX-everysel-package-options nil
  "Package options for the everysel package.")

;;; everysel.el ends here
