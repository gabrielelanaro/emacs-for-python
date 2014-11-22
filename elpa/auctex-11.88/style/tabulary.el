;;; tabulary.el --- AUCTeX style for the tabulary package.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2013-07-14
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

;; This file adds support for the tabulary package.

;;; Code:

(defvar LaTeX-tabulary-package-options
  '("debugshow")
  "Package options for the tabulary package.")

(TeX-add-style-hook
 "tabulary"
 (lambda ()
   ;; Make tabulary the default tabular environment
   (setq LaTeX-default-tabular-environment "tabulary")
   ;; Use the enhanced tabular indentation
   (add-to-list 'LaTeX-indent-environment-list
		'("tabulary" LaTeX-indent-tabular))
   ;; New symbols
   (TeX-add-symbols
    "tymax" "tymin" "tyformat")
   ;; New environments
   (LaTeX-add-environments
    ;; TODO: tabulary defines some new column types, but there is no completion
    ;; so far in `LaTeX-env-tabular*'
    '("tabulary" LaTeX-env-tabular*))

   ;; `tabulary' requires the array package
   (TeX-run-style-hooks "array"))
 LaTeX-dialect)

;;; tabulary.el ends here
