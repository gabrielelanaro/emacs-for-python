;;; lscape.el --- AUCTeX style for `lscape.sty'

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2011-05-04
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

;; This file adds support for `lscape.sty'.

;;; Code:

(TeX-add-style-hook
 "lscape"
 (lambda ()
   (LaTeX-add-environments
    "landscape")
   ;; lscape uses graphics internally to handle the work
   (TeX-run-style-hooks "graphics"))
 LaTeX-dialect)

(defvar LaTeX-lscape-package-options '("pdftex")
  "Package options for the lscape package.")

;;; lscape.el ends here
