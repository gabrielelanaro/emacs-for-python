;;; acronym.el --- AUCTeX style for `acronym.sty' version 1.38.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
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

;; This file adds support for `acronym.sty' version 1.38.

;;; Code:

(TeX-auto-add-type "acronym" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
(defvar LaTeX-acronym-regexp
  (concat "\\\\\\(?:acro\\|newacro\\|acrodef\\)" "{\\([^\n\r%\\{}]+\\)}")
  "Matches acronyms by `acronym' package.")

(defvar LaTeX-auto-acronym nil
  "Temporary for parsing acronym by `acronym' package.")

(defun LaTeX-acronym-prepare ()
  "Clear `LaTex-auto-acronym' before use."
  (setq LaTeX-auto-acronym nil))

(defun LaTeX-acronym-cleanup ()
  "Move acronyms from `LaTeX-auto-acronym' to `LaTeX-acronym-list'."
  (mapc (lambda (acronym)
	  (add-to-list 'LaTeX-acronym-list (list acronym)))
	LaTeX-auto-acronym))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-acronym-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-acronym-cleanup)

(defvar LaTeX-acronym-acronym-history nil
  "History of acronyms in acronym.")

;; The former `acronym' stands for package name, the latter stands for the
;; argument of the macro calling this function.
(defun LaTeX-arg-acronym-acronym (optional &optional prompt definition)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen acronym to the
list of defined acronyms."
  (let ((acronym (completing-read (TeX-argument-prompt optional prompt "Acronym")
				  (LaTeX-acronym-list) nil nil nil
				  'LaTeX-acronym-acronym-history)))
    (if (and definition (not (string-equal "" acronym)))
	(LaTeX-add-acronyms acronym))
    (TeX-argument-insert acronym optional optional)))

(defun LaTeX-arg-define-acronym-acronym (optional &optional prompt)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-acronym-acronym optional prompt t))

(TeX-add-style-hook
 "acronym"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-acronym-regexp 1 LaTeX-auto-acronym))
   (LaTeX-add-environments
    '("acronym" LaTeX-env-args
      [TeX-arg-string "Longest acronym"]))
   (TeX-add-symbols
    ;; Acronyms in the Text
    '("ac" LaTeX-arg-acronym-acronym)
    '("acresetall" 0)
    '("acf" LaTeX-arg-acronym-acronym)
    '("acs" LaTeX-arg-acronym-acronym)
    '("acl" LaTeX-arg-acronym-acronym)
    '("acp" LaTeX-arg-acronym-acronym)
    '("acfp" LaTeX-arg-acronym-acronym)
    '("acsp" LaTeX-arg-acronym-acronym)
    '("aclp" LaTeX-arg-acronym-acronym)
    '("acfi" LaTeX-arg-acronym-acronym)
    '("acused" LaTeX-arg-acronym-acronym)
    '("acsu" LaTeX-arg-acronym-acronym)
    '("aclu" LaTeX-arg-acronym-acronym)
    '("iac" LaTeX-arg-acronym-acronym)
    '("Iac" LaTeX-arg-acronym-acronym)
    '("ac*" LaTeX-arg-acronym-acronym)
    '("acf*" LaTeX-arg-acronym-acronym)
    '("acs*" LaTeX-arg-acronym-acronym)
    '("acl*" LaTeX-arg-acronym-acronym)
    '("acp*" LaTeX-arg-acronym-acronym)
    '("acfp*" LaTeX-arg-acronym-acronym)
    '("acsp*" LaTeX-arg-acronym-acronym)
    '("aclp*" LaTeX-arg-acronym-acronym)
    '("acfi*" LaTeX-arg-acronym-acronym)
    '("acsu*" LaTeX-arg-acronym-acronym)
    '("aclu*" LaTeX-arg-acronym-acronym)
    '("iac*" LaTeX-arg-acronym-acronym)
    '("Iac*" LaTeX-arg-acronym-acronym)
    ;; Customization
    '("acsfont" 1)
    '("acffont" 1)
    '("acfsfont" 1)
    ;; Defining Acronyms
    '("acro" LaTeX-arg-define-acronym-acronym [ "Short name" ] "Full name")
    '("acroextra" "Additional info")
    '("newacro" LaTeX-arg-define-acronym-acronym [ "Short name" ] "Full name")
    '("acrodef" LaTeX-arg-define-acronym-acronym [ "Short name" ] "Full name")
    ;; Non standard indefinite articles
    '("acroindefinite" LaTeX-arg-acronym-acronym
      "Short indefinite article" "Long indefinite article")
    '("acrodefindefinite" LaTeX-arg-acronym-acronym
      "Short indefinite article" "Long indefinite article")
    '("newacroindefinite" LaTeX-arg-acronym-acronym
      "Short indefinite article" "Long indefinite article")
    ;; Non standard and foreign plural forms
    '("acroplural" LaTeX-arg-acronym-acronym [ "Short plural" ] "Long plural")
    '("acrodefplural" LaTeX-arg-acronym-acronym [ "Short plural" ] "Long plural")
    '("newacroplural" LaTeX-arg-acronym-acronym [ "Short plural" ] "Long plural"))
   (TeX-run-style-hooks
    "relsize"
    "xstring"
    "suffix")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ac" "*{")
				("acf" "*{")
				("acs" "*{")
				("acl" "*{")
				("acp" "*{")
				("acfp" "*{")
				("acsp" "*{")
				("aclp" "*{")
				("acfi" "*{")
				("acused" "{")
				("acsu" "*{")
				("aclu" "*{")
				("iac" "*{")
				("Iac" "*{")
				("acro" "{[{")
				("acroextra" "{")
				("newacro" "{[{")
				("acrodef" "{[{")
				("acroindefinite" "{{{")
				("acrodefindefinite" "{{{")
				("newacroindefinite" "{{{")
				("acroplural" "{[{")
				("acrodefplural" "{[{")
				("newacroplural" "{[{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-acronym-package-options
  '("footnote" "nohyperlinks" "printonlyused" "withpage" "smaller" "dua" "nolist")
  "Package options for the acronym package.")

;; acronym.el ends here
