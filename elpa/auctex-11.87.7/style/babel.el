;;; babel.el --- AUCTeX style for `babel.sty'

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2005-05-29
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

;; This file adds support for `babel.sty'.

;;; Code:

(defvar LaTeX-babel-language-list
  '("acadian" "afrikaans" "american" "austrian""bahasa" "basque" "brazil"
    "brazilian" "breton" "british" "bulgarian" "canadian" "canadien"
    "catalan" "croatian" "czech" "danish" "dutch" "english" "esperanto"
    "estonian" "finnish" "francais" "frenchb" "french" "galician"
    "german" "germanb" "greek" "polutonikogreek" "hebrew" "hungarian"
    "icelandic" "irish" "italian" "latin" "lowersorbian" "magyar"
    "naustrian" "ngerman" "norsk" "samin" "nynorsk" "polish" "portuges"
    "portuguese" "romanian" "russian" "scottish" "serbian" "slovak"
    "slovene" "spanish" "swedish" "turkish" "ukrainian" "uppersorbian"
    "welsh" "UKenglish" "USenglish")
  "List of languages supported by the babel LaTeX package.")

(if (fboundp 'defvaralias)
    (defvaralias 'LaTeX-babel-package-options 'LaTeX-babel-language-list)
  (defvar LaTeX-babel-package-options LaTeX-babel-language-list
    "Package options for the babel package."))

(defun LaTeX-babel-active-languages ()
  "Return a list of languages used in the document."
  (let (active-languages)
    (dolist (elt LaTeX-babel-language-list)
      (when (member elt TeX-active-styles)
	(add-to-list 'active-languages (list elt))))
    active-languages))

(defun TeX-arg-babel-lang (optional &optional prompt)
  "Prompt for a language with completion and insert it as an argument."
  (TeX-argument-insert
   (completing-read "Language: " (LaTeX-babel-active-languages)) nil))

(defun LaTeX-env-babel-lang (env)
  "Prompt for a language and insert it as an argument of ENV."
  (LaTeX-insert-environment
   env (format "{%s}" (completing-read "Language: "
				       (LaTeX-babel-active-languages)))))

(TeX-add-style-hook
 "babel"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("selectlanguage" TeX-arg-babel-lang)
    '("foreignlanguage" TeX-arg-babel-lang t)
    "languagename"
    '("iflanguage" TeX-arg-babel-lang t nil)
    '("useshorthands" t)
    '("defineshorthand" t nil)
    '("aliasshorthand" t nil)
    '("languageshorthands" TeX-arg-babel-lang)
    '("shorthandon" t)
    '("shorthandoff" t)
    '("languageattribute" TeX-arg-babel-lang t))
   ;; New environments
   (LaTeX-add-environments
    '("otherlanguage" LaTeX-env-babel-lang)
    '("otherlanguage*" LaTeX-env-babel-lang)
    '("hyphenrules" LaTeX-env-babel-lang))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("selectlanguage" "{")
				("foreignlanguage" "{{")
				("iflanguage" "{{{")
				("languagename" "")
				("useshorthands" "{")
				("languageshorthands" "{")
				("shorthandon" "{")
				("shorthandoff" "{"))
			      'function)
     (font-latex-add-keywords '(("defineshorthand" "{{")
				("aliasshorthand" "{{")
				("languageattribute" "{{"))
			      'variable))))

;;; babel.el ends here
