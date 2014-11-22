;;; siunitx.el --- AUCTeX style for `siunitx.sty' version 2.5s.

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

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

;; This file adds support for `siunitx.sty' version 2.5s.

;;; Code:

(TeX-auto-add-type "siunitx-unit" "LaTeX")

;; Self Parsing -- see (info "(auctex)Hacking the Parser").  `\\(?:\\[.*\\]\\)?'
;; matches possible options (actually used only by `DeclareSIUnit' macro),
;; wrapped in `[...]'.
(defvar LaTeX-siunitx-regexp
  (concat "\\\\Declare"
	  "\\(?:SIUnit\\|SIPrefix\\|BinaryPrefix\\|SIPostPower\\|SIPrepower\\|"
	  "SIQualifier\\)"
	  "[ \t\n\r]*\\(?:\\[.*\\]\\)?[ \t\n\r]*{?\\\\\\([A-Za-z]+\\)}?")
  "Matches new siunitx unit, prefix, power, and qualifier definitions.")

(defvar LaTeX-auto-siunitx-unit nil
  "Temporary for parsing siunitx macro definitions.")

(defun LaTeX-siunitx-prepare ()
  "Clear `LaTex-auto-siunitx-unit' before use."
  (setq LaTeX-auto-siunitx-unit nil))

(defun LaTeX-siunitx-cleanup ()
  "Move symbols from `LaTeX-auto-siunitx-unit' to `LaTeX-siunitx-unit-list'."
  (mapcar (lambda (symbol)
	    (add-to-list 'LaTeX-siunitx-unit-list (list symbol)))
	  LaTeX-auto-siunitx-unit))

;; FIXME: This does not seem to work unless one does a manual reparse.
(add-hook 'TeX-auto-prepare-hook 'LaTeX-siunitx-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-siunitx-cleanup)

(defvar LaTeX-siunitx-unit-history nil
  "History of units in siunitx.")

(defun LaTeX-arg-siunitx-unit (optional &optional prompt initial-input
					definition prefix)
  "Prompt for siunitx units, prefixes, powers, and qualifiers.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If INITIAL-INPUT is non-nil, insert it in the minibuffer
initially, with point positioned at the end.  If DEFINITION is
non-nil, add the chosen unit to the list of defined units.  If
PREFIX is non-nil, insert it before the given input."
  ;; Remove <SPC> key binding from map used in `TeX-completing-read-multiple'
  ;; with `require-match' set to `nil' (it's `crm-local-completion-map' if
  ;; `completing-read-multiple' is bound, `minibuffer-local-completion-map'
  ;; otherwise) and set completion separator to the TeX escape character.
  (let* ((crm-local-completion-map
	  (remove (assoc 32 crm-local-completion-map) crm-local-completion-map))
	 (minibuffer-local-completion-map
	  (remove (assoc 32 minibuffer-local-completion-map)
		  minibuffer-local-completion-map))
	 (crm-separator (regexp-quote TeX-esc))
	 (unit (mapconcat 'identity
			  (TeX-completing-read-multiple
			   (TeX-argument-prompt optional prompt "Unit: " t)
			   (LaTeX-siunitx-unit-list) nil nil initial-input
			   'LaTeX-siunitx-unit-history)
			  TeX-esc)))
    (if (and definition (not (string-equal "" unit)))
	(LaTeX-add-siunitx-units unit))
    (TeX-argument-insert unit optional prefix)))

(defun LaTeX-arg-define-siunitx-unit (optional &optional prompt)
  "Prompt for a LaTeX siunitx unit, prefix, power, and qualifier.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-siunitx-unit optional
			  (unless prompt (concat "Unit: " TeX-esc))
			  nil t TeX-esc))

(defvar LaTeX-siunitx-package-options
  '(;; Detecting fonts
    ("detect-all")
    ("detect-display-math" ("true" "false"))
    ("detect-family" ("true" "false"))
    ("detect-inline-family" ("math" "text"))
    ("detect-inline-weight" ("math" "text"))
    ("detect-mode" ("true" "false"))
    ("detect-none")
    ("detect-shape" ("true" "false"))
    ("detect-weight" ("true" "false"))
    ;; Font settings
    ("color")
    ("math-rm")
    ("math-sf")
    ("math-tt")
    ("mode" ("math" "text"))
    ("number-color")
    ("text-rm")
    ("text-sf")
    ("text-tt")
    ("unit-color")
    ;; Parsing numbers
    ("input-close-uncertainty")
    ("input-comparators")
    ("input-complex-roots")
    ("input-decimal-markers")
    ("input-digits")
    ("input-exponent-markers")
    ("input-ignore")
    ("input-open-uncertainty")
    ("input-protect-tokens")
    ("input-signs")
    ("input-uncertainty-signs")
    ("input-symbols")
    ("parse-numbers" ("true" "false"))
    ;; Post-processing numbers
    ("add-decimal-zero" ("true" "false"))
    ("add-integer-zero" ("true" "false"))
    ("explicit-sign")
    ("fixed-exponent")
    ("minimum-integer-digits")
    ("omit-uncertainty" ("true" "false"))
    ("retain-explicit-plus" ("true" "false"))
    ("retain-unity-mantissa" ("true" "false"))
    ("retain-zero-exponent" ("true" "false"))
    ("round-half" ("up" "even"))
    ("round-integer-to-decimal" ("true" "false"))
    ("round-minimum")
    ("round-mode" ("off" "figures" "places"))
    ("round-precision")
    ("scientific-notation" ("true" "false" "fixed" "engineering"))
    ("zero-decimal-to-integer" ("true" "false"))
    ;; Printing numbers
    ("bracket-negative-numbers" ("true" "false"))
    ("bracket-numbers" ("true" "false"))
    ("close-bracket")
    ("complex-root-position" ("after-number" "before-number"))
    ("copy-complex-root")
    ("copy-decimal-marker")
    ("exponent-base")
    ("exponent-product")
    ("group-digits" ("true" "false" "decimal" "integer"))
    ("group-minimum-digits")
    ("group-separator")
    ("negative-color")
    ("open-bracket")
    ("output-close-uncertainty")
    ("output-complex-root")
    ("output-decimal-marker")
    ("output-exponent-marker")
    ("output-open-uncertainty")
    ("separate-uncertainty" ("true" "false"))
    ("tight-spacing" ("true" "false"))
    ("uncertainty-separator")
    ;; Multi-part numbers
    ("fraction-function")
    ("input-product")
    ("input-quotient")
    ("output-product")
    ("output-quotient")
    ("quotient-mode" ("symbol" "fraction"))
    ;; Lists and ranges of numbers
    ("list-final-separator")
    ("list-pair-separator")
    ("list-separator")
    ("range-phrase")
    ;; Angles
    ("add-arc-degree-zero" ("true" "false"))
    ("add-arc-minute-zero" ("true" "false"))
    ("add-arc-second-zero" ("true" "false"))
    ("angle-symbol-over-decimal" ("true" "false"))
    ("arc-separator")
    ("number-angle-product")
    ;; Creating units
    ("free-standing-units" ("true" "false"))
    ("overwrite-functions" ("true" "false"))
    ("space-before-unit" ("true" "false"))
    ("unit-optional-argument" ("true" "false"))
    ("use-xspace" ("true" "false"))
    ;; Loading additional units
    ("abbreviations" ("true" "false"))
    ("binary-units" ("true" "false"))
    ("version-1-compatibility" ("true" "false"))
    ;; Using units
    ("bracket-unit-denominator" ("true" "false"))
    ("forbid-literal-units" ("true" "false"))
    ("literal-superscript-as-power" ("true" "false"))
    ("inter-unit-product")
    ("parse-units" ("true" "false"))
    ("per-mode" ("reciprocal" "fraction" "reciprocal-positive-first" "symbol"
		 "repeated-symbol" "symbol-or-fraction"))
    ("per-symbol")
    ("power-font" ("number" "unit"))
    ("prefixes-as-symbols" ("true" "false"))
    ("qualifier-mode" ("subscript" "brackets" "phrase" "space" "text"))
    ("qualifier-phrase")
    ("sticky-per" ("true" "false"))
    ;; Numbers with units
    ("allow-number-unit-breaks" ("true" "false"))
    ("exponent-to-prefix" ("true" "false"))
    ("list-units" ("brackets" "repeat" "single"))
    ("multi-part-units" ("brackets" "repeat" "single"))
    ("number-unit-product")
    ("product-units" ("repeat" "brackets" "brackets-power" "power" "repeat"
		      "single"))
    ("range-units" ("brackets" "repeat" "single"))
    ;; Tabular material
    ("table-align-comparator" ("true" "false"))
    ("table-align-exponent" ("true" "false"))
    ("table-align-text-pre" ("true" "false"))
    ("table-align-text-post" ("true" "false"))
    ("table-align-uncertainty" ("true" "false"))
    ("table-alignment" ("center" "left" "right"))
    ("table-auto-round" ("true" "false"))
    ("table-column-width")
    ("table-comparator" ("true" "false"))
    ("table-figures-decimal")
    ("table-figures-exponent")
    ("table-figures-integer")
    ("table-figures-uncertainty")
    ("table-format")
    ("table-number-alignment" ("center-decimal-marker" "center" "left" "right"))
    ("table-parse-only" ("true" "false"))
    ("table-omit-exponent" ("true" "false"))
    ("table-space-text-pre")
    ("table-space-text-post")
    ("table-sign-exponent" ("true" "false"))
    ("table-sign-mantissa" ("true" "false"))
    ("table-text-alignment" ("center" "left" "right"))
    ("table-unit-alignment" ("center" "left" "right"))
    ;; Symbols
    ("math-angstrom")
    ("math-arcminute")
    ("math-arcsecond")
    ("math-celsius")
    ("math-degree")
    ("math-micro")
    ("math-ohm")
    ("redefine-symbols" ("true" "false"))
    ("text-angstrom")
    ("text-arcminute")
    ("text-arcsecond")
    ("text-celsius")
    ("text-degree")
    ("text-micro")
    ("text-ohm")
    ;; Other options
    ("locale" ("FR" "DE" "UK" "US" "ZA"))
    ("strict"))
  "Package options for the siunitx package.")

(TeX-add-style-hook
 "siunitx"
 (lambda ()
   (TeX-auto-add-regexp `(,LaTeX-siunitx-regexp 1 LaTeX-auto-siunitx-unit))
   (TeX-add-symbols
    ;; Numbers
    '("ang" [TeX-arg-key-val LaTeX-siunitx-package-options] "Angle")
    '("num" [TeX-arg-key-val LaTeX-siunitx-package-options] "Number")
    '("numlist" [TeX-arg-key-val LaTeX-siunitx-package-options] "Numbers")
    '("numrange" [TeX-arg-key-val LaTeX-siunitx-package-options]
      "Number 1" "Number 2")
    ;; Units
    '("si" [TeX-arg-key-val LaTeX-siunitx-package-options] LaTeX-arg-siunitx-unit)
    '("SI" [TeX-arg-key-val LaTeX-siunitx-package-options]
      "Value" [ "Pre-unit"] LaTeX-arg-siunitx-unit)
    '("SIlist" [TeX-arg-key-val LaTeX-siunitx-package-options]
      "Values" LaTeX-arg-siunitx-unit)
    '("SIrange" [TeX-arg-key-val LaTeX-siunitx-package-options]
      "Value 1" "Value 2" LaTeX-arg-siunitx-unit)
    ;; Settings
    '("sisetup" (TeX-arg-key-val LaTeX-siunitx-package-options))
    ;; Tabular material
    '("tablenum" [TeX-arg-key-val LaTeX-siunitx-package-options] "Number")
    ;; Creating new macros (`DeclareSIUnitWithOptions' macro is deprecated)
    '("DeclareSIUnit" [TeX-arg-key-val LaTeX-siunitx-package-options]
      (LaTeX-arg-define-siunitx-unit) "Symbol")
    '("DeclareSIPrefix" (LaTeX-arg-define-siunitx-unit "Prefix")
      "Symbol" "Powers of 10")
    '("DeclareBinaryPrefix" (LaTeX-arg-define-siunitx-unit "Prefix")
      "Symbol" "Powers of 2")
    '("DeclareSIPostPower" (LaTeX-arg-define-siunitx-unit "Name") "Power")
    '("DeclareSIPrePower" (LaTeX-arg-define-siunitx-unit "Name") "Power")
    '("DeclareSIQualifier" (LaTeX-arg-define-siunitx-unit "Qualifier") "Symbol")
    ;; Highlighting
    '("highlight" "Color")
    ;; Transferring settings to pgf
    '("SendSettingsToPgf" 0))
    ;;; The unit macros
   ;; SI base units
   (LaTeX-add-siunitx-units
    "ampere"
    "candela"
    "kelvin"
    "kilogram"
    "gram"
    "meter"
    "metre"
    "second"
    ;; Coherent derived units in the SI with special names and symbols
    "becquerel"
    "celsius"
    "degreeCelsius"
    "coulomb"
    "farad"
    "gray"
    "hertz"
    "henry"
    "joule"
    "katal"
    "lumen"
    "lux"
    "newton"
    "ohm"
    "pascal"
    "radian"
    "siemens"
    "sievert"
    "steradian"
    "tesla"
    "volt"
    "watt"
    "weber"
    ;; Non-SI units accepted for use with the International System of Units
    "day"
    "degree"
    "hectare"
    "hour"
    "liter"
    "litre"
    "arcminute"
    "minute"
    "arcsecond"
    "tonne"
    ;; Non-SI units whose values in SI units must be obtained experimentally
    "astronomicalunit"
    "atomicmassunit"
    "bohr"
    "clight"
    "dalton"
    "electronmass"
    "electronvolt"
    "elementarycharge"
    "hartree"
    "planckbar"
    ;; Other non-SI units.
    "angstrom"
    "bar"
    "barn"
    "bel"
    "decibel"
    "knot"
    "mmHg"
    "nauticalmile"
    "neper"
    "percent"
    ;; SI prefixes
    "yocto"
    "zepto"
    "atto"
    "femto"
    "pico"
    "nano"
    "micro"
    "milli"
    "centi"
    "deci"
    "deca"
    "deka"
    "hecto"
    "kilo"
    "mega"
    "giga"
    "tera"
    "peta"
    "exa"
    "zetta"
    "yotta"
    ;; Powers
    "square"
    "squared"
    "cubic"
    "cubed"
    "tothe"
    "raiseto"
    "per"
    "of")
   ;; Abbreviated units (available unless `abbreviations' option is set to `false')
   (unless (LaTeX-provided-package-options-member "siunitx" "abbreviations=false")
     (LaTeX-add-siunitx-units
      "fg"
      "pg"
      "ng"
      "ug"
      "mg"
      "g"
      "kg"
      "amu"
      "pm"
      "nm"
      "um"
      "mm"
      "cm"
      "dm"
      "m"
      "km"
      "as"
      "fs"
      "ps"
      "ns"
      "us"
      "ms"
      "s"
      "fmol"
      "pmol"
      "nmol"
      "umol"
      "mmol"
      "mol"
      "kmol"
      "pA"
      "nA"
      "uA"
      "mA"
      "A"
      "kA"
      "ul"
      "ml"
      "l"
      "hl"
      "uL"
      "mL"
      "L"
      "hL"
      "mHz"
      "Hz"
      "kHz"
      "MHz"
      "GHz"
      "THz"
      "N"
      "mN"
      "kN"
      "MN"
      "Pa"
      "kPa"
      "MPa"
      "GPa"
      "mohm"
      "kohm"
      "Mohm"
      "pV"
      "nV"
      "uV"
      "mV"
      "V"
      "kV"
      "uW"
      "mW"
      "W"
      "kW"
      "MW"
      "GW"
      "J"
      "kJ"
      "meV"
      "keV"
      "MeV"
      "GeV"
      "TeV"
      "kWh"
      "F"
      "fF"
      "pF"
      "K"
      "dB"))
   ;; Binary prefixes and units available when `binary-units' option is used
   (when (or (LaTeX-provided-package-options-member "siunitx" "binary-units")
	     (LaTeX-provided-package-options-member "siunitx" "binary-units=true"))
     (LaTeX-add-siunitx-units
      "kibi"
      "mebi"
      "gibi"
      "tebi"
      "pebi"
      "exbi"
      "zebi"
      "yobi"
      "bit"
      "byte"))
   ;; Symbols
   (LaTeX-add-siunitx-units
    "SIUnitSymbolAngstrom"
    "SIUnitSymbolArcminute"
    "SIUnitSymbolArcsecond"
    "SIUnitSymbolCelsius"
    "SIUnitSymbolDegree"
    "SIUnitSymbolMicro"
    "SIUnitSymbolOhm")
   ;; Macros available when `version-1-compatibility' option is used
   (when (or (LaTeX-provided-package-options-member
	      "siunitx" "version-1-compatibility")
	     (LaTeX-provided-package-options-member
	      "siunitx" "version-1-compatibility=true"))
     (LaTeX-add-siunitx-units
      "Square"
      "ssquare"
      "BAR"
      "bbar"
      "Day"
      "dday"
      "Gray"
      "ggray"
      "atomicmass"
      "arcmin"
      "arcsec"
      "are"
      "curie"
      "gal"
      "millibar"
      "rad"
      "rem"
      "roentgen"
      "micA"
      "micmol"
      "micl"
      "micL"
      "nanog"
      "micg"
      "picm"
      "micm"
      "Sec"
      "mics"
      "cmc"
      "dmc"
      "cms"
      "centimetrecubed"
      "centimetresquared"
      "cubiccentimetre"
      "cubicdecimetre"
      "squarecentimetre"
      "squaremetre"
      "squarekilometre"
      "parsec"
      "lightyear"
      "gmol"
      "kgmol"
      "lbmol"
      "molar"
      "Molar"
      "torr"
      "gon"
      "micron"
      "mrad"
      "gauss"
      "eVperc"
      "nanobarn"
      "picobarn"
      "femtobarn"
      "attobarn"
      "zeptobarn"
      "yoctobarn"
      "nb"
      "pb"
      "fb"
      "ab"
      "zb"
      "yb"))
   (TeX-run-style-hooks "l3keys2e"
			"array"
			"amstext"
			"xparse"
			"expl3")
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("ang" "[{")
				("num" "[{")
				("si" "[{")
				("SI" "[{[{")
				("numlist" "[{")
				("numrange" "[{{")
				("SIlist" "[{{")
				("SIrange" "[{{{")
				("sisetup" "{")
				("tablenum" "[{")
				("DeclareSIUnit" "[|{\\{")
				("DeclareSIPrefix" "|{\\{{")
				("DeclareBinaryPrefix" "|{\\{{")
				("DeclareSIPostPower" "|{\\{")
				("DeclareSIPrePower" "|{\\{")
				("DeclareSIQualifier" "|{\\{")
				("highlight" "{"))
			      'function)))
 LaTeX-dialect)

(defun LaTeX-siunitx-package-options nil
  "Prompt for package options for the siunitx package."
  (TeX-read-key-val t LaTeX-siunitx-package-options))

;; siunitx.el ends here
