;;; italian.el --- Setup AUCTeX for editing Italian text.

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

;; Author: Davide G. M. Salvetti <salve@debian.org>
;; Maintainer: Davide G. M. Salvetti <salve@debian.org>
;; Created: 2004-05-12
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
;;
;; I believe that the Italian correct quoting is achieved with `\"<' and
;; `\">'.  However, I will be glad to see a normative reference. -- DGMS

;;; Code:

(defvar TeX-language-it-hook nil
  "Hook run for Italian texts.")

(TeX-add-style-hook
 "italian"
 (lambda ()
   ;; XXX: Handle former customizations of the now defunct
   ;; Italian-specific variables.  References to the respective
   ;; variables are to be deleted in future versions. (now = 2005-04-01)
   (unless (eq (car TeX-quote-language) 'override)
     (let ((open-quote (if (and (boundp 'LaTeX-italian-open-quote)
				LaTeX-italian-open-quote)
			   LaTeX-italian-open-quote
			 "\"<"))
	   (close-quote (if (and (boundp 'LaTeX-italian-close-quote)
				 LaTeX-italian-close-quote)
			    LaTeX-italian-close-quote
			  "\">")))
       (setq TeX-quote-language
	     `("italian" ,open-quote ,close-quote TeX-quote-after-quote))))
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\"<" "\">" french)))
   (run-hooks 'TeX-language-it-hook)))

;;; italian.el ends here
