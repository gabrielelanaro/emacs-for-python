;;; amsmath.el --- Style hook for the AMS-LaTeX amsmath package.

;; Copyright (C) 2002, 2005  Free Software Foundation, Inc.
;; FIXME: What about the copyright for <= 2001?

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;; Maintainer: auctex-devel@gnu.org

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

;; This will also load the amstext, amsbsy and amsopn style files.

;;; Code:

(TeX-add-style-hook "amsmath"
 (function
  (lambda ()

    (LaTeX-add-environments
     '("align"      LaTeX-env-label)
     '("gather"     LaTeX-env-label)
     '("flalign"    LaTeX-env-label)
     '("multline"   LaTeX-env-label)
     '("alignat"    LaTeX-amsmath-env-alignat)
     '("alignat*"   LaTeX-amsmath-env-alignat)
     '("xalignat"   LaTeX-amsmath-env-alignat)
     '("xalignat*"  LaTeX-amsmath-env-alignat)
     '("xxalignat"  LaTeX-amsmath-env-alignat)
     '("aligned"    LaTeX-amsmath-env-aligned)
     '("gathered"   LaTeX-amsmath-env-aligned)
     "align*" "gather*" "flalign*" "multline*" "equation*"
     "split"
     "cases"
     "matrix" "smallmatrix" "pmatrix" "bmatrix" "Bmatrix" "vmatrix" "Vmatrix"
     "subequations"
     '("subarray" "Alignment"))

    (TeX-add-symbols
     '("eqref" TeX-arg-ref)
     '("numberwithin" TeX-arg-counter "Section level")
     '("raisetag" "Dimension")
     '("intertext" t)
     '("hdotsfor" ["Stretch"] "Number of columns to cover")
     '("xleftarrow" ["Below"] "Above")
     '("xrightarrow" ["Below"] "Above")
     '("overset" "Accent symbol" "Symbol")
     '("underset" "Accent symbol" "Symbol")
     '("dfrac" 2)
     '("tfrac" 2)
     '("binom" 2)
     '("dbinom" 2)
     '("tbinom" 2)
     '("genfrac" "Left delimiter" "Right delimiter" "Thickness"
       "Mathstyle" 2)
     '("cfrac" ["position (l or r)"] t)
     '("smash" ["where (t or b)"] t)
     '("sideset" "Left" "Right")
     '("tag" "(Tag)")
     '("tag*" "Tag")
     '("displaybreak" ["Weight (0..4)"])
     '("allowdisplaybreaks" ["Weight (1..4)"])
     '("substack" t)
     '("leftroot" "Push root index left by")
     '("uproot" "Push root index upward by")
     '("boxed" t)
     '("mspace" t)
     '("mod" t)
     '("pmod" t)
     '("pod" t)
     '("overleftrightarrow" t)
     '("underleftarrow" t)
     '("underrightarrow" t)
     '("underleftrightarrow" t)
     '("dddot" t)
     '("ddddot" t)
     "bmod" "notag"
     "dots" "dotsb" "dotsc" "dotsi" "dotsm" "dotso" "nobreakdash" 
     "lvert" "rvert" "lVert" "rVert" 
     "iint" "iiint" "iiiint" "idotsint"
     )
    
    (setq  LaTeX-item-list 
	   (append '(("split"    . LaTeX-item-equation)
		     ("multline" . LaTeX-item-equation)
		     ("multline*" . LaTeX-item-equation)
		     ("gather"   . LaTeX-item-equations)
		     ("gather*"  . LaTeX-item-equation)
		     ("gathered" . LaTeX-item-equation)
		     ("align"    . LaTeX-item-equations)
		     ("align*"   . LaTeX-item-equation)
		     ("aligned"  . LaTeX-item-equation)
		     ("alignat"  . LaTeX-item-equations)
		     ("alignat*" . LaTeX-item-equation)
		     ("xalignat"  . LaTeX-item-equations)
		     ("xalignat*" . LaTeX-item-equation)
		     ("xxalignat" . LaTeX-item-equation)
		     ("flalign"  . LaTeX-item-equations)
		     ("flalign*" . LaTeX-item-equation)
		     ("matrix" .  LaTeX-item-equation)
		     ("pmatrix" .  LaTeX-item-equation)
		     ("bmatrix" .  LaTeX-item-equation)
		     ("Bmatrix" .  LaTeX-item-equation)
		     ("vmatrix" .  LaTeX-item-equation)
		     ("Vmatrix" .  LaTeX-item-equation)
		     ("cases"    . LaTeX-item-equation))
		   LaTeX-item-list))

    ;; When `LaTeX-amsmath-label' is nil, use value of LaTeX-equation-label:
    (unless LaTeX-amsmath-label
      (setq LaTeX-amsmath-label LaTeX-equation-label))

    (setq LaTeX-label-alist
	  (append '(("align"      . LaTeX-amsmath-label)
		    ("alignat"    . LaTeX-amsmath-label)
		    ("xalignat"   . LaTeX-amsmath-label)
		    ("multline"    . LaTeX-amsmath-label)
		    ("flalign"    . LaTeX-amsmath-label)
		    ("gather"     . LaTeX-amsmath-label))
		  LaTeX-label-alist))

    ;; amsmath includes amstext, amsbsy, & amsopn.
    ;; So we run their hooks, too.
    (TeX-run-style-hooks "amstext" "amsbsy" "amsopn")

    ;; If RefTeX is loaded, make it recognize the amsmath environments.
    (when (fboundp 'reftex-add-to-label-alist)
      (reftex-add-to-label-alist '(AMSTeX))))))

(defun LaTeX-amsmath-env-alignat (env)
  (let ((ncols (read-string "Number of columns: ")))
    (LaTeX-insert-environment env (concat TeX-grop ncols TeX-grcl))
    (and (not (string= "xxalignat" env))
	 (not (string= "*" (substring env -1)))
	 (LaTeX-label env)
	 (newline-and-indent))))

(defun LaTeX-amsmath-env-aligned (env)
  (let ((where (read-string "(optional) Vertical position (t or b): ")))
    (if (string= where "")
	(setq where "")
      (setq where (concat "[" where "]")))
    (LaTeX-insert-environment env where)))

(defun LaTeX-item-equation ()
  (end-of-line 0)
  (just-one-space)
  (insert "\\\\")
  (forward-line 1)
  (indent-according-to-mode))

(defun LaTeX-item-equations ()
  (LaTeX-item-equation)
  (let ((environment (LaTeX-current-environment 1)))
    (and (LaTeX-label environment)
	 (newline-and-indent))))

(defvar LaTeX-amsmath-package-options '("intlimits" "nointlimits"
					"sumlimits" "nosumlimits"
					"namelimits" "nonamelimits"
					"leqno" "reqno" "centertags"
					"tbtags" "cmex10" "fleqn" "?")
    "Package options for the amsmath package.")

;;; amsmath.el ends here.
