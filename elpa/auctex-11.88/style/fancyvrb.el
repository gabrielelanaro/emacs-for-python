;;; fancyvrb.el --- AUCTeX style for `fancyvrb.sty' version 2.8.

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

;; This file adds support for `fancyvrb.sty' version 2.8.

;;; Code:

(defvar LaTeX-fancyvrb-macro-regexp
  (concat "\\\\\\(?:Re\\|\\)CustomVerbatimCommand"
	  "[ \t\n\r]*{?[ \t\n\r]*\\\\\\([A-Za-z]+\\)[ \t\n\r]*}?")
  "Matches macros by fancyvrb package.")

(defvar LaTeX-fancyvrb-environment-regexp
  (concat "\\\\\\(?:Define\\|Custom\\|Recustom\\)VerbatimEnvironment"
	  "[ \t\n\r]*{[ \t\n\r]*\\([A-Za-z]+\\)[ \t\n\r]*}")
  "Matches environments by fancyvrb package.")

(defvar LaTeX-fancyvrb-key-val-options
  '(("commentchar")
    ("gobble")
    ("formatcom")
    ("fontfamily" ("tt" "courier" "helvetica"))
    ("fontsize")
    ("fontshape")
    ("fontseries")
    ("frame" ("none" "leftline" "topline" "bottomline" "lines" "single"))
    ("framerule")
    ("framesep")
    ("rulecolor")
    ("fillcolor")
    ("label")
    ("labelposition" ("none" "topline" "bottomline" "all"))
    ("numbers" ("none" "left" "right"))
    ("numbersep")
    ("firstnumber" ("auto" "last" "integer"))
    ("stepnumber")
    ("numberblanklines" ("true" "false"))
    ("firstline")
    ("lastline")
    ("showspaces" ("true" "false"))
    ("showtabs" ("true" "false"))
    ("obeytabs" ("true" "false"))
    ("tabsize")
    ("baselinestretch" ("auto" "dimension"))
    ("commandchars")
    ("xleftmargin")
    ("xrightmargin")
    ("resetmargins" ("true" "false"))
    ("hfuzz")
    ("samepage" ("true" "false"))
    ("codes")
    ("defineactive")
    ;; Actually, the following options are used only by the `BVerbatim'
    ;; environment.
    ("boxwidth" ("auto" "dimension"))
    ("baseline" ("b" "c" "t")))
  "Key=value options for fancyvrb macros and environments.")

(TeX-add-style-hook
 "fancyvrb"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-fancyvrb-macro-regexp 1 TeX-auto-symbol))
   (TeX-auto-add-regexp
    `(,LaTeX-fancyvrb-environment-regexp 1 LaTeX-auto-environment))
   (TeX-run-style-hooks
    "keyval")
   (TeX-add-symbols
    ;;; Verbatim material in footnotes
    "VerbatimFootnotes"
    ;;; Improved verbatim commands
    '("Verb" [TeX-arg-key-val LaTeX-fancyvrb-key-val-options] TeX-arg-verb)
    "DefineShortVerb"
    "UndefineShortVerb"
    ;;; Verbatim environments
    '("fvset" (TeX-arg-key-val LaTeX-fancyvrb-key-val-options))
    ;; Changing individual line formatting
    "FancyVerbFormatLine"
    ;; Line numbering
    "theFancyVerbLine"
    ;; Selection of lines to print
    "FancyVerbStartString"
    "FancyVerbStopString"
    ;; Personalized environments
    '("CustomVerbatimEnvironment"
      (TeX-arg-define-environment "New verbatim environment")
      (TeX-arg-environment "Basend on environment")
      (TeX-arg-key-val LaTeX-fancyvrb-key-val-options))
    '("RecustomVerbatimEnvironment"
      (TeX-arg-environment "New verbatim environment")
      (TeX-arg-environment "Basend on environment")
      (TeX-arg-key-val LaTeX-fancyvrb-key-val-options))
    '("DefineVerbatimEnvironment"
      (TeX-arg-define-environment "New verbatim environment")
      (TeX-arg-environment "Basend on environment")
      (TeX-arg-key-val LaTeX-fancyvrb-key-val-options))
    '("CustomVerbatimCommand" (TeX-arg-define-macro "New verbatim macro: ")
      (TeX-arg-eval completing-read "Based on macro: " (TeX-symbol-list))
      (TeX-arg-key-val LaTeX-fancyvrb-key-val-options))
    '("RecustomVerbatimCommand" (TeX-arg-macro "New verbatim macro: ")
      (TeX-arg-eval completing-read "Based on macro: " (TeX-symbol-list))
      (TeX-arg-key-val LaTeX-fancyvrb-key-val-options))
    ;;; Saving and restoring verbatim text and environments
    "SaveVerb"
    "UseVerb"
    "SaveVerbatim"
    "UseVerbatim"
    "LUseVerbatim"
    "BUseVerbatim"
    ;;; Writing and reading verbatim files
    '("VerbatimInput" [TeX-arg-key-val LaTeX-fancyvrb-key-val-options]
      (TeX-arg-file))
    '("BVerbatimInput" [TeX-arg-key-val LaTeX-fancyvrb-key-val-options]
      (TeX-arg-file))
    '("LVerbatimInput" [TeX-arg-key-val LaTeX-fancyvrb-key-val-options]
      (TeX-arg-file)))
   (LaTeX-add-environments
    '("Verbatim" LaTeX-env-args
      [TeX-arg-key-val LaTeX-fancyvrb-key-val-options])
    '("BVerbatim" LaTeX-env-args
      [TeX-arg-key-val LaTeX-fancyvrb-key-val-options])
    '("LVerbatim" LaTeX-env-args
      [TeX-arg-key-val LaTeX-fancyvrb-key-val-options])
    '("SaveVerbatim" LaTeX-env-args
      [TeX-arg-key-val LaTeX-fancyvrb-key-val-options])
    '("VerbatimOut"
      (lambda (env)
	(let ((options (TeX-read-key-val t LaTeX-fancyvrb-key-val-options))
	      (file (TeX-read-string "Output file: ")))
	  (LaTeX-insert-environment
	   env
	   (concat (unless (zerop (length options))
		     (concat LaTeX-optop options LaTeX-optcl))
		   (concat TeX-grop file TeX-grcl)))))))
   (LaTeX-add-counters
    "FancyVerbLine")

   ;; Filling
   (set (make-local-variable 'LaTeX-verbatim-regexp)
   	 (concat
   	  LaTeX-verbatim-regexp
   	  "\\|Verbatim\\|BVerbatim\\|LVerbatim\\|SaveVerbatim\\|VerbatimOut"))
   (add-to-list 'LaTeX-verbatim-environments-local "Verbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "BVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "LVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "SaveVerbatim")
   (add-to-list 'LaTeX-verbatim-environments-local "VerbatimOut")
   (make-local-variable 'LaTeX-indent-environment-list)
   (add-to-list 'LaTeX-indent-environment-list '("Verbatim" current-indentation))
   (add-to-list 'LaTeX-indent-environment-list '("BVerbatim" current-indentation))
   (add-to-list 'LaTeX-indent-environment-list '("LVerbatim" current-indentation))
   (add-to-list 'LaTeX-indent-environment-list '("SaveVerbatim" current-indentation))
   (add-to-list 'LaTeX-indent-environment-list '("VerbatimOut" current-indentation))
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "Verb")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "Verb")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-set-syntactic-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DefineVerbatimEnvironment" "{{{")
				("CustomVerbatimEnvironment" "{{{")
				("RecustomVerbatimEnvironment" "{{{"))
			      'function)
     (font-latex-add-keywords '(("VerbatimInput" "[{")
				("BVerbatimInput" "[{")
				("LVerbatimInput" "[{"))
			      'reference)
     (font-latex-add-keywords '(("Verb" "[")) ; The second argument should
					      ; actually be verbatim.
			      'textual)
     (font-latex-add-keywords '(("fvset" "{")) 'variable)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults)))
 LaTeX-dialect)

(defvar LaTeX-fancyvrb-package-options nil
  "Package options for the fancyvrb package.")

;;; fancyvrb.el ends here
