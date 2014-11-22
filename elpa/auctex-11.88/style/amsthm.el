;;; amsthm.el --- Style hook for the AMS-LaTeX amsthm package.

;; Copyright (C) 1997, 2013 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;; Maintainer: auctex-devel@gnu.org

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

;;; Code:

(defvar LaTeX-amsthm-package-options nil
  "Package options for the amsthm package.")

(TeX-add-style-hook
 "amsthm"
 (lambda ()
   (LaTeX-add-environments
    '("proof" (lambda (env &rest ignore)
		(LaTeX-insert-environment
		 env
		 (let ((heading (read-string "(optional) Heading: ")))
		   (if (string= heading "")
		       ""
		     (format "[%s]" heading))))))
    )
   (TeX-add-symbols
    '("newtheorem*" TeX-arg-define-environment "Heading")
    '("theoremstyle" LaTeX-amsthm-complete-theoremstyle)
    "qedhere"
    "swapnumbers"
    '("newtheoremstyle" "Style name" (TeX-arg-length nil "Space above")
      (TeX-arg-length nil "Space below") "Body font" "Indent amount"
      "Theorem head font" "Punctuation after head"
      (TeX-arg-length nil "Space after head") "Theorem head spec"))

   (TeX-auto-add-regexp
    `(,(concat "\\\\newtheorem\\*{\\(" TeX-token-char "+\\)}")
      1 LaTeX-auto-environment))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newtheorem" "*{[{[")
				("theoremstyle" "{")
				("newtheoremstyle" "{{{{{{{{{"))
			      'function)))
 LaTeX-dialect)

(defun LaTeX-amsthm-complete-theoremstyle (&rest ignore)
  (insert TeX-grop
	  (completing-read  "Style: " '(("plain" . nil)
					("definition" . nil)
					("remark" . nil)))
	  TeX-grcl))

;;; amsthm.el ends here
