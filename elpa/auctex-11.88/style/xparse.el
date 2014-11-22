;;; xparse.el --- AUCTeX style for `xparse.sty' version 4467.

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

;; This file adds basic support for `xparse.sty' version 4467.  It
;; doesn't parse argument specification of macros and environments.

;;; Code:

(defvar LaTeX-xparse-macro-regexp
  (concat "\\\\\\(?:Declare\\|New\\|Renew\\|Provide\\|DeclareExpandable\\)"
	  "DocumentCommand[ \t\n\r]*{?[ \t\n\r]*\\\\\\([A-Za-z]+\\)[ \t\n\r]*}?"
	  ;; The following is the opening brace of argument specification and is
	  ;; needed to skip internal macros containing `:' or `_'.
	  "[ \t\n\r]*{")
  "Matches macros by xparse package.")

(defvar LaTeX-xparse-environment-regexp
  (concat "\\\\\\(?:Declare\\|New\\|Renew\\|Provide\\)DocumentEnvironment"
	  "[ \t\n\r]*{[ \t\n\r]*\\([A-Za-z]+\\)[ \t\n\r]*}")
  "Matches environments by xparse package.")

(TeX-add-style-hook
 "xparse"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-xparse-macro-regexp 1 TeX-auto-symbol))
   (TeX-auto-add-regexp
    `(,LaTeX-xparse-environment-regexp 1 LaTeX-auto-environment))
   (TeX-run-style-hooks
    "expl3")
   (TeX-add-symbols
    ;; Declaring commands and environments
    '("DeclareDocumentCommand" TeX-arg-define-macro "Argument specification" t)
    '("NewDocumentCommand" TeX-arg-define-macro "Argument specification" t)
    '("RenewDocumentCommand" TeX-arg-macro "Argument specification" t)
    '("ProvideDocumentCommand" TeX-arg-define-macro "Argument specification" t)
    '("DeclareDocumentEnvironment" TeX-arg-define-environment
      "Argument specification" t t)
    '("NewDocumentEnvironment" TeX-arg-define-environment
      "Argument specification" t t)
    '("RenewDocumentEnvironment" TeX-arg-environment
      "Argument specification" t t)
    '("ProvideDocumentEnvironment" TeX-arg-define-environment
      "Argument specification" t t)
    ;; Fully-expandable document commands
    '("DeclareExpandableDocumentCommand"
      TeX-arg-define-macro "Argument specification" t)
    ;; Testing special values
    '("IfBooleanTF" 3)
    '("IfBooleanT" 3)
    '("IfBooleanF" 3)
    '("IfNoValueTF" 3)
    '("IfNoValueT" 3)
    '("IfNoValueF" 3)
    '("IfValueTF" 3)
    '("IfValueT" 3)
    '("IfValueF" 3)
    "BooleanTrue"
    "BooleanFalse"
    ;; Argument processors
    "ProcessedArgument"
    "ReverseBoolean"
    '("SplitArgument" "Number" "Token")
    "SplitList"
    "TrimSpaces"
    '("ProcessList" "List" "Functiom")
    ;; Access to the argument specification
    '("GetDocumentCommandArgSpec" TeX-arg-macro)
    '("GetDocumentEnvironmmentArgSpec" TeX-arg-environment)
    '("ShowDocumentCommandArgSpec" TeX-arg-macro)
    '("ShowDocumentEnvironmentArgSpec" TeX-arg-environment))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DeclareDocumentCommand" "|{{{")
				("NewDocumentCommand" "|{{{")
				("RenewDocumentCommand" "|{{{")
				("ProvideDocumentCommand" "|{{{")
				("DeclareExpandableDocumentCommand" "|{{{")
				("DeclareDocumentEnvironment" "{{{{")
				("NewDocumentEnvironment" "{{{{")
				("RenewDocumentEnvironment" "{{{{")
				("ProvideDocumentEnvironment" "{{{{"))
			      'function)))
 LaTeX-dialect)

(defun LaTeX-xparse-package-options ()
  "Read the xparse package options from the user."
  (TeX-read-key-val t '(("log-declarations" ("true" "false")))))

;;; xparse.el ends here
