;;; url.el --- AUCTeX style for `url.sty'

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-13
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

;; This file adds support for `url.sty'.

;;; Code:

(TeX-add-style-hook
 "url"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    "Url"
    "UrlBigBreakPenalty"
    "UrlBigBreaks"
    "UrlBreakPenalty"
    "UrlBreaks"
    "UrlFont"
    "UrlLeft"
    "UrlNoBreaks"
    "UrlOrds"
    "UrlRight"
    "UrlSpecials"
    "path"
    "url"
    "urldef"
    '("urlstyle" TeX-arg-urlstyle))

   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("path" "{") ("url" "{")) 'reference)
     (font-latex-add-keywords '(("Url" "")
				("UrlBigBreakPenalty" "")
				("UrlBigBreaks" "")
				("UrlBreakPenalty" "")
				("UrlBreaks" "")
				("UrlFont" "")
				("UrlLeft" "")
				("UrlNoBreaks" "")
				("UrlOrds" "")
				("UrlRight" "")
				("UrlSpecials" "")
				("urldef" "")
				("urlstyle" "{"))
			      'variable)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults)))
 LaTeX-dialect)

(defun TeX-arg-urlstyle (optional &optional prompt)
  "Prompt for style used in \\urlstyle with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Style")
		    (mapcar 'list '("rm" "same" "sf" "tt"))
		    nil t)
   optional))

(defvar LaTeX-url-package-options '("hyphens" "obeyspaces" "spaces" "LY1"
				    "T1" "allowmove")
  "Package options for the url package.")

;;; url.el ends here
