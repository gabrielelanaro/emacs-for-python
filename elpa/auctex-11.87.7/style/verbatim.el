;;; verbatim.el --- Style hook for the verbatim package.

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2001/05/01

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

;;  M-x TeX-auto-generate verbatim.sty makes garbages.

;;; Code

(TeX-add-style-hook "verbatim"
 (function
  (lambda ()
    (LaTeX-add-environments
     "comment")
    (TeX-add-symbols
     '("verbatiminput" TeX-arg-file)))))

(defvar LaTeX-verbatim-package-options nil
  "Package options for the verbatim package.")

;;; verbatim.el ends here.
