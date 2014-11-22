;;; inputenc.el --- AUCTeX style for `inputenc.sty'

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Arne Jørgensen <arne@arnested.dk>
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
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; This file adds support for `inputenc.sty'.

;;; Code:

(defun LaTeX-inputenc-package-options nil
  "Prompt for package options for the inputenc package."
  ;; separate the condition in three to silence the byte compiler
  (if (boundp 'latex-inputenc-coding-alist)
      (when (fboundp 'latexenc-coding-system-to-inputenc)
	(when (fboundp 'latexenc-inputenc-to-coding-system)
	  (let ((default (latexenc-coding-system-to-inputenc
			  (or coding-system-for-write
			      buffer-file-coding-system)))
		(selected 'undecided))
	    (setq selected (completing-read
			    (if default
				(format "Input encoding (default %s): " default)
			      "Input encoding: ")
			    (mapcar 'car latex-inputenc-coding-alist)
			    nil
			    nil
			    nil
			    nil
			    default))

	    ;; if necessary offer to set the coding system for saving
	    ;; this buffer based on the selected input encoding
	    (when (and (null
			(coding-system-equal
			 (coding-system-base
			  (or coding-system-for-write
			      buffer-file-coding-system))
			 (coding-system-base
			  (latexenc-inputenc-to-coding-system selected))))
		       (y-or-n-p "Set coding system for saving this buffer? ")
		       (set-buffer-file-coding-system
			(coding-system-base
			 (latexenc-inputenc-to-coding-system selected)))
		       (message nil)))

	    ;; return selected input encoding
	    selected)))
    (read-string "Input encoding: ")))

(defun LaTeX-arg-inputenc-inputenc (optional)
  "Prompt for input encoding."
  (TeX-argument-insert (LaTeX-inputenc-package-options) nil))

(TeX-add-style-hook
 "inputenc"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("inputencoding" LaTeX-arg-inputenc-inputenc)))
 LaTeX-dialect)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; inputenc.el ends here
