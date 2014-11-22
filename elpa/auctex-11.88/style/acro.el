;;; acro.el --- AUCTeX style for `acro.sty' version 1.2a.

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

;; This file adds support for `acro.sty' version 1.2a.

;;; Code:

(defvar LaTeX-acro-package-options-list
  '(;; General Options
    ("version" ("0" "1"))
    ("single" ("true" "false"))
    ("hyperref" ("true" "false"))
    ("record-pages" ("true" "false"))
    ("only-used" ("true" "false"))
    ("mark-as-used" ("first" "any"))
    ("macros" ("true" "false"))
    ("xspace" ("true" "false"))
    ("strict" ("true" "false"))
    ("sort" ("true" "false"))
    ("cite" ("all" "first" "none"))
    ("cite-cmd")
    ("cite-space")
    ("index-cmd")
    ("accsupp" ("true" "false"))
    ("uc-cmd")
    ;; Options Regarding Acronyms
    ("short-format")
    ("long-format")
    ("first-long-format")
    ("list-short-format")
    ("list-long-format")
    ("extra-format")
    ("first-style" ("default" "plain" "empty" "square" "short" "reversed"
		    "plain-reversed" "footnote" "sidenote"))
    ("extra-style" ("default" "plain" "comma" "paren" "bracket"))
    ("plural-ending")
    ;; Options Regarding the List
    ("page-ref" ("none" "plain" "comma" "paren"))
    ("page-name")
    ("pages-name")
    ("page-ranges" ("true" "false"))
    ("next-page")
    ("next-pages")
    ("list-type" ("table" "itemize" "description"))
    ("list-style" ("list" "tabular" "longtable" "extra-tabular" "extra-longtable"
		   "extra-tabular-rev" "extra-longtable-rev"))
    ("list-header" ("chapter" "chapter*" "section" "section*" "subsection"
		    "subsection*" "addchap" "addsec"))
    ("list-name")
    ("list-table-width")
    ("list-caps" ("true" "false")))
  "Package options for the acro package.")

(TeX-auto-add-type "acro-acronym" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").
(defvar LaTeX-acro-regexp
  (concat "\\\\DeclareAcronym" "{\\([^\n\r%\\{}]+\\)}")
  "Matches `acro' acronym definitions.")

(defvar LaTeX-auto-acro-acronym nil
  "Temporary for parsing `acro' acronym definitions.")

(defun LaTeX-acro-prepare ()
  "Clear `LaTex-auto-acro-acronym' before use."
  (setq LaTeX-auto-acro-acronym nil))

(defun LaTeX-acro-cleanup ()
  "Move acronyms from `LaTeX-auto-acro-acronym' to
`LaTeX-acro-list' and to `TeX-auto-symbol' if option `macros' is
set to `true'."
  (mapc (lambda (acronym)
	  (add-to-list 'LaTeX-acro-acronym-list (list acronym)))
	LaTeX-auto-acro-acronym)
  (when (or (LaTeX-provided-package-options-member "acro" "macros")
	    (LaTeX-provided-package-options-member "acro" "macros=true"))
    (add-to-list 'TeX-auto-symbol LaTeX-auto-acro-acronym)))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-acro-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-acro-cleanup)

(defvar LaTeX-acro-acronym-history nil
  "History of acronyms in acro.")

(defun LaTeX-arg-acro-acronym (optional &optional prompt definition)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen acronym to the
list of defined acronyms."
  (let ((acronym (completing-read (TeX-argument-prompt optional prompt "Acronym")
				  (LaTeX-acro-acronym-list) nil nil nil
				  'LaTeX-acro-acronym-history)))
    (if (and definition (not (string-equal "" acronym)))
	(LaTeX-add-acro-acronyms acronym))
    (TeX-argument-insert acronym optional optional)))

(defun LaTeX-arg-define-acro-acronym (optional &optional prompt)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-acro-acronym optional prompt t))

(defvar LaTeX-acro-declareacronym-keys
  '(("short") ("long") ("short-plural") ("long-plural") ("long-plural-form")
    ("short-indefinite") ("long-indefinite") ("long-pre") ("long-post") ("alt")
    ("alt-indefinite") ("extra") ("sort") ("class") ("cite") ("short-format")
    ("long-format") ("first-long-format") ("pdfstring") ("accsupp")
    ("index-sort") ("index") ("index-cmd"))
  "List of keys accepted by `\DeclareAcronym' macro of `acro' package
in its second mandatory argument.")

(defvar LaTeX-acro-printacronyms-keys
  '(("include-classes") ("exclude-classes") ("name") ("header"))
  "List of keys accepted by `\printacronyms' macro of `acro' package
in its optional argument.")

(defun LaTeX-arg-acro-key-val (optional prompt key-val-alist)
  "Prompt for keys and values in KEY-VAL-ALIST.
<SPC> key binding in minibuffer is removed temporarily.  Insert
the given value as a TeX macro argument.  If OPTIONAL is non-nil,
insert it as an optional argument.  Use PROMPT as the prompt
string.  KEY-VAL-ALIST is an alist.  The car of each element
should be a string representing a key and the optional cdr should
be a list with strings to be used as values for the key."
  ;; Remove <SPC> key binding from map used in `multi-prompt-key-value' (called
  ;; by `TeX-arg-key-val') with `require-match' set to `nil'.
  (let ((crm-local-completion-map
	 (remove (assoc 32 crm-local-completion-map) crm-local-completion-map)))
    (TeX-arg-key-val optional key-val-alist prompt)))

(TeX-add-style-hook
 "acro"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-acro-regexp 1 LaTeX-auto-acro-acronym))
   (TeX-add-symbols
    ;; Creating New Acronyms
    '("DeclareAcronym" LaTeX-arg-define-acro-acronym
      (LaTeX-arg-acro-key-val "Definition of acronym (k=v)"
			      LaTeX-acro-declareacronym-keys))
    ;; Using the Acronyms
    '("ac" LaTeX-arg-acro-acronym)
    '("ac*" LaTeX-arg-acro-acronym)
    '("Ac" LaTeX-arg-acro-acronym)
    '("Ac*" LaTeX-arg-acro-acronym)
    '("acs" LaTeX-arg-acro-acronym)
    '("acs*" LaTeX-arg-acro-acronym)
    '("acl" LaTeX-arg-acro-acronym)
    '("acl*" LaTeX-arg-acro-acronym)
    '("Acl" LaTeX-arg-acro-acronym)
    '("Acl*" LaTeX-arg-acro-acronym)
    '("aca" LaTeX-arg-acro-acronym)
    '("aca*" LaTeX-arg-acro-acronym)
    '("acf" LaTeX-arg-acro-acronym)
    '("acf*" LaTeX-arg-acro-acronym)
    '("Acf" LaTeX-arg-acro-acronym)
    '("Acf*" LaTeX-arg-acro-acronym)
    '("acp" LaTeX-arg-acro-acronym)
    '("acp*" LaTeX-arg-acro-acronym)
    '("Acp" LaTeX-arg-acro-acronym)
    '("Acp*" LaTeX-arg-acro-acronym)
    '("acsp" LaTeX-arg-acro-acronym)
    '("acsp*" LaTeX-arg-acro-acronym)
    '("aclp" LaTeX-arg-acro-acronym)
    '("aclp*" LaTeX-arg-acro-acronym)
    '("Aclp" LaTeX-arg-acro-acronym)
    '("Aclp*" LaTeX-arg-acro-acronym)
    '("acap" LaTeX-arg-acro-acronym)
    '("acap*" LaTeX-arg-acro-acronym)
    '("acfp" LaTeX-arg-acro-acronym)
    '("acfp*" LaTeX-arg-acro-acronym)
    '("Acfp" LaTeX-arg-acro-acronym)
    '("Acfp*" LaTeX-arg-acro-acronym)
    ;; Indefinite Forms
    '("iac" LaTeX-arg-acro-acronym)
    '("iac*" LaTeX-arg-acro-acronym)
    '("Iac" LaTeX-arg-acro-acronym)
    '("Iac*" LaTeX-arg-acro-acronym)
    '("iacs" LaTeX-arg-acro-acronym)
    '("iacs*" LaTeX-arg-acro-acronym)
    '("Iacs" LaTeX-arg-acro-acronym)
    '("Iacs*" LaTeX-arg-acro-acronym)
    '("iaca" LaTeX-arg-acro-acronym)
    '("iaca*" LaTeX-arg-acro-acronym)
    '("Iaca" LaTeX-arg-acro-acronym)
    '("Iaca*" LaTeX-arg-acro-acronym)
    '("iacl" LaTeX-arg-acro-acronym)
    '("iacl*" LaTeX-arg-acro-acronym)
    '("Iacl" LaTeX-arg-acro-acronym)
    '("Iacl*" LaTeX-arg-acro-acronym)
    '("iacf" LaTeX-arg-acro-acronym)
    '("iacf*" LaTeX-arg-acro-acronym)
    '("Iacf" LaTeX-arg-acro-acronym)
    '("Iacf*" LaTeX-arg-acro-acronym)
    '("iacflike" LaTeX-arg-acro-acronym)
    '("iacflike*" LaTeX-arg-acro-acronym)
    '("Iacflike" LaTeX-arg-acro-acronym)
    '("Iacflike*" LaTeX-arg-acro-acronym)
    ;; Simulating the First Appearance
    '("acflike" LaTeX-arg-acro-acronym)
    '("acflike*" LaTeX-arg-acro-acronym)
    '("acfplike" LaTeX-arg-acro-acronym)
    '("acfplike*" LaTeX-arg-acro-acronym)
    ;; Reset or Mark as Used
    '("acreset" "List of acronyms")
    '("acresetall" 0)
    '("acuse" "List of acronyms")
    '("acuseall" 0)
    ;; PDF bookmarks
    '("acpdfstring" LaTeX-arg-acro-acronym)
    '("acpdfstringplural" LaTeX-arg-acro-acronym)
    ;; Printing the List
    '("printacronyms" [LaTeX-arg-acro-key-val nil LaTeX-acro-printacronyms-keys])
    ;; Customization
    '("acsetup" (TeX-arg-key-val LaTeX-acro-package-options-list)))
   (TeX-run-style-hooks
    "l3sort"
    "xspace"
    "xtemplate"
    "l3keys2e"
    "xparse"
    "expl3")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DeclareAcronym" "{{")
				("ac" "*{")
				("Ac" "*{")
				("acs" "*{")
				("acl" "*{")
				("Acl" "*{")
				("aca" "*{")
				("acf" "*{")
				("Acf" "*{")
				("acp" "*{")
				("Acp" "*{")
				("acsp" "*{")
				("aclp" "*{")
				("Aclp" "*{")
				("acap" "*{")
				("acfp" "*{")
				("Acfp" "*{")
				("acflike" "*{")
				("acfplike" "*{")
				("iac" "*{")
				("Iac" "*{")
				("iacs" "*{")
				("Iacs" "*{")
				("iaca" "*{")
				("Iaca" "*{")
				("iacl" "*{")
				("Iacl" "*{")
				("iacf" "*{")
				("Iacf" "*{")
				("iacflike" "*{")
				("Iacflike" "*{")
				("acuse" "{"))
			      'function)))
 LaTeX-dialect)

(defun LaTeX-acro-package-options ()
  "Prompt for package options for the acro package."
  (TeX-read-key-val t LaTeX-acro-package-options-list))

;;; acro.el ends here
