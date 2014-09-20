;;; booktabs.el -- AUCTeX style for booktabs.sty

;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.

;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created:  2003-10-21
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

;; This file adds support for `booktabs.sty'.

;;; Code:

(defun LaTeX-booktabs-arg-paren (optional prompt)
  "Prompt for a value and use parentheses when it is inserted.
If OPTIONAL is non-nil the parameter is labeled as optional.
PROMPT is the value of the prompt to be shown."
  (let ((< "\(")
	(> "\)"))
    (TeX-parse-argument optional prompt)))

(TeX-add-style-hook
 "booktabs"
 (lambda ()

   ;; New symbols
   (TeX-add-symbols
    '("toprule" [ "Thickness" ])
    '("midrule" [ "Thickness" ])
    '("bottomrule" [ "Thickness" ])
    ;; FIXME: The qestion for the trim parameter will only be asked if
    ;; a value for the thickness parameter was given.  Is this a
    ;; feature of `TeX-parse-arguments'?
    '("cmidrule" [ "Thickness" ] [ LaTeX-booktabs-arg-paren "Trim" ]
      "Column(s)")
    '("addlinespace" [ "Height" ])
    '("morecmidrules")
    '("specialrule" "Thickness" "Space above" "Space below"))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("toprule" "[")
				("midrule" "[")
				("bottomrule" "[")
				("cmidrule" "[({")
				("addlinespace" "[")
				("morecmidrules" "")
				("specialrule" "{{{"))
			      'function))))

(defvar LaTeX-booktabs-package-options nil
  "Package options for the booktabs package.")			

;;; booktabs.el ends here
