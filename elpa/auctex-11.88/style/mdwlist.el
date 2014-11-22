;;; mdwlist.el --- AUCTeX style for `mdwlist.sty'

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

;; This file adds support for `mdwlist.sty'.

;;; Code:

(TeX-add-style-hook
 "mdwlist"
 (lambda ()
   (TeX-add-symbols
    '("makecompactlist" "New environment" "Existing environment")
    '("suspend" "Environment") ; this could be done nicer by automatically
    '("resume" "Environment")) ; determining the environment
   (LaTeX-add-environments
    '("enumerate*" LaTeX-env-item)
    '("itemize*" LaTeX-env-item)
    '("description*" LaTeX-env-item))
   ;; Indentation and filling
   (make-local-variable 'LaTeX-begin-regexp)
   (setq LaTeX-begin-regexp (concat LaTeX-begin-regexp "\\|resume\\b"))
   (make-local-variable 'LaTeX-end-regexp)
   (setq LaTeX-end-regexp (concat LaTeX-end-regexp "\\|suspend\\b"))
   (make-local-variable 'paragraph-start)
   (setq paragraph-start (concat paragraph-start
				 "\\|[ \t]*" TeX-comment-start-regexp "*[ \t]*"
				 (regexp-quote TeX-esc)
				 "\\(resume\\b\\|suspend\\b\\)"))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("makecompactlist" "{{")
				("suspend" "[{")
				("resume" "[{["))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-mdwlist-package-options nil
  "Package options for the mdwlist package.")

;;; mdwlist.el ends here
