;;; ulem.el --- AUCTeX style for `ulem.sty'

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
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

;; This file adds support for `ulem.sty'.

;;; Code:

(TeX-add-style-hook
 "ulem"
 (lambda ()
   (TeX-add-symbols
    '("uline" 1)
    '("uuline" 1)
    '("uwave" 1)
    '("sout" 1)
    '("xout" 1)
    ;; can be used with \renewcommand or \setlength
    "ULthickness"
    "ULdepth"
    ;; custom commands can be defined with these commands; see the
    ;; documentation for an example
    "ULon"
    "markoverwith"
    ;; \useunder {underline_command}{font_declaration}{font_command}
    ;; replaces occurences of font_declaration and font_command with the
    ;; underline_command
    '("useunder" TeX-arg-ulem-useunder
      TeX-arg-ulem-fontdecl TeX-arg-ulem-fontcmd))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     ;; Tell font-lock about the update.
     (font-latex-add-keywords '(("useunder" "{{{")) 'function)
     (font-latex-add-keywords '(("uline" "{")
				("uwave" "{")
				("sout" "{")
				("xout" "{")) 'textual)))
 LaTeX-dialect)

(defvar LaTeX-arg-fontdecl
  (mapcar (lambda (str) (concat "\\" str))
	  '("itshape" "bfseries" "scshape"
	    "ttfamily" "upshape" "mdseries"
	    "rmfamily" "sffamily" "slshape"))
  "List of font declaration commands in LaTeX")

(defvar LaTeX-arg-fontcmd
  (mapcar (lambda (str) (concat "\\" str))
	  '("textit" "textbf" "textsc"
	    "texttt" "textup" "textmd"
	    "textrm" "textsf" "textsl"))
  "List of font commands in LaTeX")

(defun TeX-arg-ulem-fontdecl (optional &optional prompt)
  "Prompt for the font-declaration un \\useunder"
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt
		     optional prompt "Font declaration")
		    LaTeX-arg-fontdecl nil t) optional))

(defun TeX-arg-ulem-fontcmd (optional &optional prompt)
  "Prompt for the font-declaration un \\useunder"
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt
		     optional prompt "Font command")
		    LaTeX-arg-fontcmd nil t) optional))

;; adapted from url.el:TeX-arg-urlstyle
(defun TeX-arg-ulem-useunder (optional &optional prompt)
  "Prompt for underline command used in \\useunder"
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Underline command")
		    (mapcar 'list
			    (mapcar (lambda (str) (concat "\\" str))
				    '("uline" "uuline"
				      "uwave" "sout" "xout")))
		    nil t) optional))

(defvar LaTeX-ulem-package-options
  '("UWforbf" "ULforem" "normalbf" "normalem")
  "Package options for the ulem package.")

;;; ulem.el ends here
