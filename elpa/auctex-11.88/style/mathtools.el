;;; mathtools.el --- Style hook for the LaTeX package `mathtools'.

;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2011-02-13
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

;;  This file adds support for `mathtools.sty'

;;; Comments:

;;; This package serves as a wrapper for amsmath, adding more features
;;; and fixing a few bugs in amsmath.  The mathstyle argument for many
;;; of the macros is discussed at
;;; <http://www.tug.org/TUGboat/Articles/tb22-4/tb72perlS.pdf>

;;; Code:

;; amsmath options which can be passed directly to mathtools are
;; appended in the style hook below
(defvar LaTeX-mathtools-package-options
  '("fixamsmath" "donotfixamsmathbugs" "allowspaces" "disallowspaces")
  "Package options for the mathtools package.")

(defvar LaTeX-mathtools-key-val-options
  '(("showonlyrefs")
    ("mathic" ("true" "false"))
    ("showmanualtags" ("true" "false"))
    ("firstline-afterskip")
    ("lastline-preskip")
    ("multlined-pos" ("c" "b" "t"))
    ("multlined-width")
    ("centercolon" ("true" "false"))
    ("prescript-sub-format")
    ("prescript-sup-format")
    ("prescript-arg-format"))
  "Options for the \\mathtoolsset command")

(TeX-add-style-hook
 "mathtools"
 (lambda ()

   ;; mathtools requires amsmath, as some bugs in amsmath are fixed
   (TeX-run-style-hooks "amsmath")

   (dolist (elt LaTeX-amsmath-package-options)
     (add-to-list 'LaTeX-mathtools-package-options elt))

   (LaTeX-add-environments
    '("lgathered" ["Vertical position (t or b)"])
    '("rgathered" ["Vertical position (t or b)"])
    '("multlined" LaTeX-mathtools-env-multlined)
    '("matrix*" LaTeX-mathtools-env-matrix-starred)
    '("pmatrix*" LaTeX-mathtools-env-matrix-starred)
    '("bmatrix*" LaTeX-mathtools-env-matrix-starred)
    '("Bmatrix*" LaTeX-mathtools-env-matrix-starred)
    '("vmatrix*" LaTeX-mathtools-env-matrix-starred)
    '("Vmatrix*" LaTeX-mathtools-env-matrix-starred)
    '("spreadlines" LaTeX-mathtools-env-spreadlines)
    "dcases" "dcases*")

   (TeX-add-symbols
    '("mathtoolsset" (TeX-arg-key-val LaTeX-mathtools-key-val))
    '("mathclap" 1)
    '("mathllap" ["Mathstyle"] t)
    '("mathrlap" ["Mathstyle"] t)
    '("mathclap" ["Mathstyle"] t)
    '("mathmakebox" [TeX-arg-size] [ TeX-arg-size ] 1)
    '("clap" 1)
    '("mathmbox" 1)
    '("cramped" 1)
    '("crampedllap" [ "Mathstye" ] t)
    '("crampedrlap" [ "Mathstyle" ] t)
    '("crampedclap" [ "Mathstyle" ] t)
    '("smashoperator" [ "Position (l, r or lr (default)" ] 2)
    ;; 3.1.4 Adjusting the limits of operators
    ;; explicit argument encapsulation does not seem to be required
    '("adjustlimits" 4)
    ;; 3.2 Controlling tags
    '("newtagform" "Name" ["Inner format"] "Left" "Right")
    '("renewtagform" "Name" ["Inner format"] "Left" "Right")
    '("usetagform" "Name")
    '("xleftrightarrow" ["Below"] "Above")
    '("xLeftarrow" ["Below"] "Above")
    '("xRightarrow" ["Below"] "Above")
    '("xLeftrightarrow" ["Below"] "Above")
    '("xhookleftarrow" ["Below"] "Above")
    '("xhookrightarrow" ["Below"] "Above")
    '("xmapsto" ["Below"] "Above")
    '("xrightharpoondown" ["Below"] "Above")
    '("xrightharpoonup" ["Below"] "Above")
    '("xleftharpoondown" ["Below"] "Above")
    '("xleftharpoonup" ["Below"] "Above")
    '("xrightleftharpoons" ["Below"] "Above")
    '("xleftrightharpoons" ["Below"] "Above")
    '("underbracket" [ "Rule thickness" ] [ "Bracket height" ] t)
    '("overbracket" [ "Rule thickness" ] [ "Bracket height" ] t)
    '("underbrace" 1)
    '("overbrace" 1)
    '("LaTeXunderbrace" 1)
    '("LaTeXoverbrace" 1)
    ;; 3.4.2
    '("shoveleft"  [ TeX-arg-size ] 1)
    '("shoveright" [ TeX-arg-size ] 1)
    ;; don't understand t, but intertext in amsmath.el uses it
    '("shortintertext" t)
    '("DeclarePairedDelimeter" TeX-arg-macro "Left delimeter" "Right delimeter")
    ;; 3.4.4
    '("MoveEqLeft" [ "Number" ])
    '("ArrowBetweenLines" [ TeX-arg-macro ] )
    '("ArrowBetweenLines*" [ TeX-arg-macro ] )
    ;; colon operators
    "vcentcolon" "ordinarycolon" "coloneqq" "Coloneqq"
    "coloneq" "Coloneq" "eqqcolon" "Eqqcolon" "eqcolon"
    "Eqcolon" "colonapprox" "Colonapprox" "colonsim" "Colonsim"
    ;; 3.7.1
    "lparen" "rparen"
    ;; left sub/superscripts
    '("prescript" "Below" "Above" t)
    ;; Declaring math sizes; this command doesn't seem so relevant, but
    ;; for completion, it's included
    '("DeclareMathSizes" 4)
    ;; Gathered envionments
    '("newgather" "Name" "Pre-line" "Post-line" "After")
    '("renewgather" "Name" "Pre-line" "Post-line" "After")
    ;; Split fractions
    '("splitfrac" 2)
    '("splitdfrac" 2))

   (setq LaTeX-item-list
	 (append '(("multlined"    . LaTeX-item-equation)
		   ("lgathered" . LaTeX-item-equation)
		   ("rgathered" . LaTeX-item-equation)
		   ("spreadlines" . LaTeX-item-equation)
		   ("matrix*" .  LaTeX-item-equation)
		   ("pmatrix*" .  LaTeX-item-equation)
		   ("bmatrix*" .  LaTeX-item-equation)
		   ("Bmatrix*" .  LaTeX-item-equation)
		   ("vmatrix*" .  LaTeX-item-equation)
		   ("Vmatrix*" .  LaTeX-item-equation)
		   ("dcases"    . LaTeX-item-equation)
		   ("dcases*"    . LaTeX-item-equation))
		 LaTeX-item-list))

   (setq LaTeX-label-alist
	 (append '(("lgathered"    . LaTeX-amsmath-label)
		   ("rgathered"   . LaTeX-amsmath-label)
		   ("multlined"    . LaTeX-amsmath-label)
		   LaTeX-label-alist))))
 LaTeX-dialect)

(defun LaTeX-mathtools-env-matrix-starred (env)
  (let ((where (read-string "(optional) Vertical placement of columns: ")))
    (if (string= where "")
	(setq where "")
      (setq where (concat "[" where "]")))
    (LaTeX-insert-environment env where)))

(defun LaTeX-mathtools-env-spreadlines (env)
  (let ((spread (read-string "Spacing between lines: ")))
    (LaTeX-insert-environment env (concat TeX-grop spread TeX-grcl))
    (newline-and-indent)))

;; FIXME: there are probably more subtle ways to support more than one
;; optional argument; please change if this is the case
(defun LaTeX-mathtools-env-multlined (env)
  (let ((pos (read-string "(optional) Position: "))
	(width (read-string "(optional) Width: "))
	(extra ""))
    (if (not (string= pos ""))
	(setq pos (concat LaTeX-optop pos LaTeX-optcl))
      (setq pos ""))
    (if (not (string= width ""))
	(setq width (concat LaTeX-optop width LaTeX-optcl))
      (setq width ""))
    (setq extra (concat pos width))
    (LaTeX-insert-environment env extra)
    (newline-and-indent)))

;;; mathtools.el ends here.
