;;; fontspec.el --- AUCTeX style for `fontspec.sty' version 2.3c.

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

;; This file adds support for `fontspec.sty' version 2.3c.

;;; Code:

(defvar LaTeX-fontspec-font-features
  '(;; More control over font shape selection
    ("BoldFont")
    ("ItalicFont")
    ("BoldItalicFont")
    ("SlantedFont")
    ("BoldSlantedFont")
    ("SmallCapsFont")
    ;; Different features for different font shapes
    ("BoldFeatures")
    ("ItalicFeatures")
    ("BoldItalicFeatures")
    ("SlantedFeatures")
    ("BoldSlantedFeatures")
    ("SmallCapsFeatures")
    ;; Different features for different font sizes
    ("SizeFeatures")
    ;; Font independent options
    ("Color")
    ("Scale" ("MatchLowercase" "MatchUppercase"))
    ("WordSpace")
    ("PunctuationSpace")
    ("HyphenChar")
    ("OpticalSize")
    ;; OpenType options
    ("Ligatures" ("Required"
		  "NoRequired"
		  "Common"
		  "NoCommon"
		  "Contextual"
		  "NoContextual"
		  "Rare"
		  "Historic"
		  "TeX"))
    ("Letters" ("Uppercase"
		"SmallCaps"
		"PetiteCaps"
		"UppercaseSmallCaps"
		"UppercasePetiteCaps"
		"Unicase"))
    ("Numbers" ("Lining"
		"OldStyle"
		"Proportional"
		"Monospaced"
		"SlashedZero"
		"Arabic"))
    ("Contextuals" ("Swash"
		    "Alternate"
		    "WordInitial"
		    "WordFinal"
		    "LineFinal"
		    "Inner"))
    ("VerticalPosition" ("Superior"
			 "Inferior"
			 "Numerator"
			 "Denominator"
			 "ScientificInferior"
			 "Ordinal"))
    ("Fraction" ("On" "Alternate"))
    ("StylisticSet")
    ("CharacterVariants")
    ("Alternate")
    ("Style" ("Alternate"
	      "Italic"
	      "Ruby"
	      "Swash"
	      "Historic"
	      "TitlingCaps"
	      "HorizontalKana"
	      "VerticalKana"))
    ("Diacritics" ("MarkToBase"
		   "NoMarkToBase"
		   "MarkToMark"
		   "NoMarkToMark"
		   "AboveBase"
		   "NoAboveBase"
		   "BelowBase"
		   "NoBelowBase"))
    ("Kerning" ("Uppercase" "On" "Off"))
    ("AutoFakeBold")
    ("AutoFakeSlant")
    ("FakeSlant")
    ("FakeStretch")
    ("FakeBold")
    ("Annotation")
    ("CJKShape" ("Traditional"
		 "Simplified"
		 "JIS1978"
		 "JIS1983"
		 "JIS1990"
		 "Expert"
		 "NLC"))
    ("CharacterWidth" ("Proportional"
		       "Full"
		       "Half"
		       "Third"
		       "Quarter"
		       "AlternateProportional"
		       "AlternateHalf"))
    ("Vertical" ("RotatedGlyphs")))
  "Font features options for macros of the fontspec package.")

(defvar LaTeX-fontspec-font-list nil
  "List of the fonts accessible to fontspec.")

(defun LaTeX-fontspec-arg-font (optional &optional prompt)
  "Prompt for a font name with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.

Customize `LaTeX-fontspec-arg-font-search' in order to decide how
to retrieve the list of fonts."
  (unless LaTeX-fontspec-font-list
    (when (if (eq LaTeX-fontspec-arg-font-search 'ask)
	      (not (y-or-n-p "Find font yourself? "))
	    LaTeX-fontspec-arg-font-search)
      (message "Searching for fonts...")
      (with-temp-buffer
	(shell-command "luaotfload-tool --list=basename" t)
	;; Search for the font base names and full names, and add them to
	;; `LaTeX-fontspec-font-list'.  The list is in the form
	;;     <base name><TAB><full name><TAB><version>
	(while
	    (re-search-forward "^\\([^\n\r\t]*\\)\t\\([^\n\r\t]*\\)\t.*$" nil t)
	  (add-to-list 'LaTeX-fontspec-font-list (match-string-no-properties 1))
	  (add-to-list 'LaTeX-fontspec-font-list
		       (match-string-no-properties 2))))))
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Font name")
    (or LaTeX-fontspec-font-list LaTeX-fontspec-font-list-default))
   optional))

(TeX-add-style-hook
 "fontspec"
 (lambda ()
   (TeX-run-style-hooks "expl3" "xparse")
   (TeX-add-symbols
    ;; Font selection
    '("fontspec" [TeX-arg-key-val LaTeX-fontspec-font-features "Font features"]
      LaTeX-fontspec-arg-font)
    ;; Default font families
    '("setmainfont"
      [TeX-arg-key-val LaTeX-fontspec-font-features "Font features"]
      (LaTeX-fontspec-arg-font "Main font name"))
    '("setsansfont"
      [TeX-arg-key-val LaTeX-fontspec-font-features "Font features"]
      (LaTeX-fontspec-arg-font "Sans font name"))
    '("setmonofont"
      [TeX-arg-key-val LaTeX-fontspec-font-features "Font features"]
      (LaTeX-fontspec-arg-font "Mono font name"))
    ;; New commands to select font families
    '("newfontfamily" TeX-arg-define-macro
      [TeX-arg-key-val LaTeX-fontspec-font-features "Font features"]
      LaTeX-fontspec-arg-font)
    '("newfontface" TeX-arg-define-macro
      [TeX-arg-key-val LaTeX-fontspec-font-features "Font features"]
      LaTeX-fontspec-arg-font)
    ;; Math(s) fonts
    '("setmathrm" [ "Font features" ] "Font name")
    '("setmathsf" [ "Font features" ] "Font name")
    '("setmathtt" [ "Font features" ] "Font name")
    '("setboldmathrm" [ "Font features" ] "Font name")
    ;; Emphasis and nested emphasis
    "emshape"
    "eminnershape"
    ;; Default settings
    '("defaultfontfeatures" [ LaTeX-fontspec-arg-font ]
      (TeX-arg-key-val LaTeX-fontspec-font-features "Font features"))
    ;; Changing the currently selected features
    '("addfontfeatures"
      (TeX-arg-key-val LaTeX-fontspec-font-features "Font features"))
    ;; Defining new scripts and languages
    '("newfontscript" "Script name" "OpenType tag")
    '("newfontlanguage" "Language name" "OpenType tag"))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fontspec"    "[{")
				("setmainfont" "[{")
				("setsansfont" "[{")
				("setmonofont" "[{")
				("newfontfamily" "{[{")
				("newfontface" "{[{")
				("setmathrm" "[{")
				("setmathsf" "[{")
				("setmathtt" "[{")
				("setboldmathrm" "[{")
				("emshape")
				("eminnershape")
				("defaultfontfeatures" "[{")
				("addfontfeature" "{")
				("newfontscript" "{{")
				("newfontlanguage" "{{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-fontspec-package-options
  '("math" "no-math" "config" "no-config" "quiet" "silent")
  "Package options for the fontspec package.")

;;; fontspec.el ends here
