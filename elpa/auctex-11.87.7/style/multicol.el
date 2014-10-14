;;; multicol.el --- AUCTeX style for `multicol.sty'

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2011-01-24
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

;; This file adds support for `multicol.sty'.

;;; Code:

(TeX-add-style-hook
 "multicol"
 (lambda ()
   (LaTeX-add-environments
    '("multicols" "Number of columns" [ "Text across columns" ]
      [ "Local value for \\premulticols" ])
    '("multicols*" "Number of columns" [ "Text across columns" ]
      [ "Local value for \\premulticols" ]))

   (TeX-add-symbols
    '("multicoltolerance" 0)
    '("multicolpretolerance" 0)
    '("premulticols" 0)
    '("postmulticols" 0)
    '("multicolsep" 0)
    '("multicolbaselineskip" 0)
    '("multicolovershoot" 0)
    '("multicolundershoot" 0)
    '("columnsep" 0)
    '("columnseprule" 0)
    '("columnseprulecolor" 0)
    '("raggedcolumns" 0)
    '("flushcolumns" 0)
    "columnbreak")))

(defvar LaTeX-multicol-package-options
  '("errorshow" "infoshow" "balancingshow" "markshow" "debugshow" "grid")
  "Package options for the multicol package.")

;;; multicol.el ends here
