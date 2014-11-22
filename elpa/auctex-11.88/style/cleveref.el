;;; cleveref.el --- Style hook for the `cleveref.sty' package.

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Matthew Leach <matthew@mattleach.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 13/10/2014

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

;;; Code

(defun TeX-arg-cleveref-multiple-labels (optional &optional prompt definition)
  "Prompt for a series of labels completing with known labels.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add each chosen label to the
list of defined labels."
  (let* ((labels (TeX-completing-read-multiple
                  (TeX-argument-prompt optional prompt "Keys")
                  (LaTeX-label-list)))
         (labels-string (mapconcat 'identity labels ",")))
    (when definition
      (apply 'LaTeX-add-labels labels))
    (TeX-argument-insert labels-string optional optional)))

(TeX-add-style-hook
 "cleveref"
 (lambda ()
   (TeX-add-symbols
    '("cref" TeX-arg-cleveref-multiple-labels)
    '("Cref" TeX-arg-cleveref-multiple-labels)
    '("crefrange" (TeX-arg-label "Key (first)") (TeX-arg-label "Key (last)"))
    '("Crefrange" (TeX-arg-label "key (first)") (TeX-arg-label "Key (last)"))
    '("cpageref" TeX-arg-cleveref-multiple-labels)
    '("Cpageref" TeX-arg-cleveref-multiple-labels)
    '("cpagerefrange" (TeX-arg-label "Key (first)") (TeX-arg-label "Key (last)"))
    '("Cpagerefrange" (TeX-arg-label "Key (first)") (TeX-arg-label "Key (last)"))
    '("cref*" TeX-arg-cleveref-multiple-labels)
    '("Cref*" TeX-arg-cleveref-multiple-labels)
    '("crefrange*" (TeX-arg-label "Key (first)") (TeX-arg-label "Key (last)"))
    '("Crefrange*" (TeX-arg-label "Key (first)") (TeX-arg-label "Key (last)"))
    '("namecref" TeX-arg-label)
    '("nameCref" TeX-arg-label)
    '("lnamecref" TeX-arg-label)
    '("namecrefs" TeX-arg-label)
    '("nameCrefs" TeX-arg-label)
    '("lnamecrefs" TeX-arg-label)
    '("labelcref" TeX-arg-cleveref-multiple-labels)
    '("labelcpageref" TeX-arg-cleveref-multiple-labels))

   ;; These macros aren't used particularly often during the course of
   ;; normal referencing.
   (TeX-declare-expert-macros
    "cleveref"
    "namecref" "nameCref" "lnamecref" "namecrefs" "nameCrefs"
    "lnamecrefs" "labelcref" "labelcpageref")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-set-syntactic-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("cref" "*{")
				("Cref" "*{")
				("crefrange" "*{{")
				("Crefrange" "*{{")
                                ("cpageref" "{")
                                ("Cpageref" "{")
                                ("cpagerefrange" "{{")
                                ("Cpagerefrange" "{{")
                                ("namecref" "{")
                                ("nameCref" "{")
                                ("lnamecref" "{")
                                ("namecrefs" "{")
                                ("nameCrefs" "{")
                                ("lnamecrefs" "{")
                                ("labelcref" "{")
                                ("labelcpageref" "{"))
			      'reference)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)))
 LaTeX-dialect)

(defvar LaTeX-cleveref-package-options
  '("capitalise" "nameinlink" "noabbrev" "poorman")
    "Package options for the cleveref package.")

;;; cleveref.el ends here.
