;;; fancyref.el --- AUCTeX style file with support for fancyref.sty

;; Copyright (C) 1999 Free Software Foundation, Inc.

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

(TeX-add-style-hook "fancyref"
   (lambda ()
     
     (TeX-add-symbols

      ;; The macros with label arguments
      '("fref" [ TeX-arg-fancyref-format ] TeX-arg-ref)
      '("Fref" [ TeX-arg-fancyref-format ] TeX-arg-ref)

      ;; The macros which define new prefixes and formats
      '("fancyrefchangeprefix" TeX-arg-macro "Prefix")
      '("Frefformat" TeX-arg-fancyref-format TeX-arg-macro "Output")
      '("frefformat" TeX-arg-fancyref-format TeX-arg-macro "Output")

      ;; The delimiter
      "fancyrefargdelim"

      ;; All those names and abbreviations.
      ;; Part
      "fancyrefpartlabelprefix" 
      "Frefpartname" "frefpartname"   
      ;; Chapter
      "fancyrefchalabelprefix"
      "Frefchaname" "frefchaname"   
      ;; Section
      "fancyrefseclabelprefix"
      "Frefsecname" "frefsecname"
      ;; Equation
      "fancyrefeqlabelprefix"
      "Frefeqname" "frefeqname"   
      ;; Figure
      "fancyreffiglabelprefix"
      "Freffigname" "freffigname" "Freffigshortname"
      ;; Footnote
      "fancyreffnlabelprefix"
      "Freffnname" "freffnname"   
      ;; Item
      "fancyrefitemlabelprefix"
      "Frefitemname" "frefitemname" 
      ;; Table
      "fancyreftablabelprefix"
      "Freftabname" "freftabname" "Freftabshortname"
      ;; Page
      "Frefpgname" "frefpgname" "Frefpgshortname"
      ;; On
      "Frefonname" "frefonname" 
      ;; See
      "Frefseename" "frefseename"

      ;; The spacing macros
      "fancyrefloosespacing" "fancyreftightspacing" "fancyrefdefaultspacing"

      ;; And the hook
      "fancyrefhook")

     ;; Insatall completion for labels and formats
     (setq TeX-complete-list
	   (append
	    '(("\\\\[fF]ref\\(\\[[^]]*\\]\\)?{\\([^{}\n\r\\%,]*\\)" 
	       2 LaTeX-label-list "}")
	      ("\\\\[fF]ref\\[\\([^{}\n\r\\%,]*\\)" 
	       1 LaTeX-fancyref-formats "]")
	      ("\\\\[fF]refformat{\\([^{}\n\r\\%,]*\\)"
	       1 LaTeX-fancyref-formats "}"))
	    TeX-complete-list))
     ;; Fontification
     (when (and (featurep 'font-latex)
		(eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("fref" "[{") ("Fref" "[{")) 'reference)))
   LaTeX-dialect)

;; The following list keeps a list of available format names
;; Note that this list is only updated when a format is used, not
;; during buffer parsing.  We could install a regexp to look for
;; formats, but this would not work in multifile documents since the
;; formats are not written out to the auto files.
;; For now, we just leave it at that.
(defvar LaTeX-fancyref-formats '(("plain") ("vario") ("margin") ("main"))
  "List of formats for fancyref.")

(defun LaTeX-fancyref-formats () LaTeX-fancyref-formats)

(defun TeX-arg-fancyref-format (optional &optional prompt definition)
  "Prompt for a fancyref format name.
If the user gives an unknown name, add it to the list."
  (let ((format (completing-read (TeX-argument-prompt optional prompt "Format")
				 LaTeX-fancyref-formats)))
    (if (not (string-equal "" format))
	(add-to-list 'LaTeX-fancyref-formats (list format)))
    (TeX-argument-insert format optional)))

(defvar LaTeX-fancyref-package-options '("english" "german" "loose"
					 "margin" "paren" "plain" "tight"
					 "vario")
  "Package options for the fancyref package.")

;;; fancyref.el ends here
