;;; amsopn.el --- AUCTeX style for the `amsnopn.sty' AMS-LaTeX package

;; Copyright (C) 1997, 2002, 2005, 2013 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;;         Mads Jensen <mje@inducks.org>
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

;; This file adds support for `amsnopn.sty'

;;; Code:

(TeX-add-style-hook "amsopn"
 (function
  (lambda ()
    (TeX-add-symbols
     '("DeclareMathOperator"  (TeX-arg-define-macro "Math Operator: \\")
       "Expansion text for the math operator")
     '("DeclareMathOperator*" (TeX-arg-define-macro "Math Operator: \\")
       "Expansion text for the math operator")
     '("operatorname" t)
     '("operatorname*" t))

    (add-to-list 'LaTeX-auto-regexp-list
		 '("\\\\DeclareMathOperator\\*?{?\\\\\\([A-Za-z0-9]+\\)}?"
		   1 TeX-auto-symbol))))
 LaTeX-dialect
 )

(defvar LaTeX-amsopn-package-options '("namelimits" "nonamelimits")
  "Package options for the amsopn package.")

;;; amsopn.el ends here.
