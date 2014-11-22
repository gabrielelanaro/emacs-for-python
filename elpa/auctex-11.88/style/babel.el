;;; babel.el --- AUCTeX style for `babel.sty' version 3.9h.

;; Copyright (C) 2005, 2007, 2013-2014 Free Software Foundation, Inc.

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

;; This file adds support for `babel.sty' version 3.9h.

;;; Code:

(defvar LaTeX-babel-language-list
  '("afrikaans"
    "bahasa" "indonesian" "indon" "bahasai" "bahasam" "malay" "meyalu"
    "basque"
    "breton"
    "bulgarian"
    "catalan"
    "croatian"
    "czech"
    "danish"
    "dutch"
    "english" "USenglish" "american" "UKenglish" "british"  "canadian"
    "australian" "newzealand"
    "esperanto"
    "estonian"
    "finnish"
    "french" "francais" "canadien" "acadian"
    "galician"
    "austrian" "german" "germanb" "ngerman" "naustrian"
    "greek" "polutonikogreek"
    "hebrew"
    "icelandic"
    "interlingua"
    "irish"
    "italian"
    "latin"
    "lowersorbian"
    "samin"
    "norsk" "nynorsk"
    "polish"
    "portuges" "portuguese" "brazilian" "brazil"
    "romanian"
    "russian"
    "scottish"
    "spanish"
    "slovak"
    "slovene"
    "swedish"
    "serbian"
    "turkish"
    "ukrainian"
    "uppersorbian"
    "welsh"
    ;; Extra languages mentioned in the `babel' manual.
    "albanian" "hindi" "thai" "thaicjk" "latvian" "turkmen" "hungarian" "magyar"
    "mongolian" "romansh" "lithuanian" "spanglish" "vietnamese" "japanese"
    "pinyin" "arabinc" "farsi" "ibygreek" "bgreek" "serbianic" "frenchle"
    "ethiop" "friulan" "frenchb")
  "List of languages supported by the babel LaTeX package.")

(defun LaTeX-babel-active-languages ()
  "Return a list of languages used in the document."
  (let (main-language active-languages)
    ;; Loop over options provided to class and `babel' package at load time.
    (dolist (elt (append
		  ;; In most cases there is only one element in the alist, if
		  ;; there is more than one element, the first one should
		  ;; contain the class options of the current buffer.  So we can
		  ;; take the car of `LaTeX-provided-class-options'.
		  (cdr (car LaTeX-provided-class-options))
		  (cdr (assoc "babel" LaTeX-provided-package-options))))
      (setq elt (TeX-split-string "=" elt))
      (if (equal (car elt) "main")
	  ;; Starting from version 3.9 of `babel' package, languages can be set
	  ;; with the following syntax:
	  ;;   \usepackage[latin.medieval,main=danish,spanish.notilde]{babel}
	  ;; with `danish' being the default language.  When the default
	  ;; language is set with the `main' option, we record it and append to
	  ;; the list at the end.
	  (setq main-language (car (cdr elt)))
	;; Get rid of the modifiers (`medieval' and `notilde' in the above
	;; example).
	(setq elt (car (TeX-split-string "\\." (car elt))))
	(if (member elt LaTeX-babel-language-list)
	    ;; Append element to `active-languages' to respect loading order.
	    ;; `babel' package uses as default language the last loaded one,
	    ;; except if it is set with the `main' option.
	    (add-to-list 'active-languages elt t))))
    (if main-language
	(add-to-list 'active-languages main-language t))
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

(defun LaTeX-babel-load-languages ()
  "Load style files of babel active languages."
  ;; Run style hooks for every active language in loading order, so
  ;; `TeX-quote-language' will be correctly set.
  (mapc 'TeX-run-style-hooks (LaTeX-babel-active-languages)))

(TeX-add-style-hook
 "babel"
 (lambda ()
   (LaTeX-babel-load-languages)
   (add-hook 'LaTeX-after-usepackage-hook 'LaTeX-babel-load-languages nil t)
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
			      'variable)))
 LaTeX-dialect)

(defun LaTeX-babel-package-options ()
  "Prompt for package options for the babel package."
  (TeX-read-key-val
   t
   (append
    '(("KeepShorthandsActive")
      ("activeacute")
      ("activegrave")
      ("shorthands")
      ("safe" ("none" "ref" "bib"))
      ("math" ("active" "normal"))
      ("config")
      ("main" LaTeX-babel-language-list)
      ("headfoot" LaTeX-babel-language-list)
      ("noconfigs")
      ("showlanguages")
      ("strings" ("generic" "unicode" "encoded"))
      ("hyphenmap" ("off" "main" "select" "other" "other*"))
      ("base"))
    (mapcar 'list LaTeX-babel-language-list))))

;;; babel.el ends here
