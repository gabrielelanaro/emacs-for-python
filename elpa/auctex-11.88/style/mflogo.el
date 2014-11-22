;;; mflogo.el --- AUCTeX style for `mflogo.sty'

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2011-02-02
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

;; This file adds support for `mflogo.sty'.

;;; Code:

(TeX-add-style-hook
 "mflogo"
 (lambda ()
   (TeX-add-symbols
    '("textlogo" 1)
    '("logofamily" 1))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("logofamily" "{")
				("textlogo" "{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-mflogo-package-options nil
  "Package options for the mflogo package.")

;;; mflogo.el ends here
