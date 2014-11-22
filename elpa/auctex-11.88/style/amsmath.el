;;; amsmath.el --- Style hook for the AMS-LaTeX amsmath package.

;; Copyright (C) 2002, 2005-2007, 2012-2014  Free Software Foundation, Inc.
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
     '("aligned"    ["Vertical position (t or b)"])
     '("gathered"   ["Vertical position (t or b)"])
     '("alignedat"  LaTeX-amsmath-env-alignedat)
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
     '("shoveright" t) '("shoveleft" t)
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
     '("lvert" TeX-arg-insert-right-brace-maybe)
     '("lVert" TeX-arg-insert-right-brace-maybe)
     "rvert" "rVert"
     "iint" "iiint" "iiiint" "idotsint"
     )
    
    (setq  LaTeX-item-list 
	   (append '(("split"    . LaTeX-item-equation)
		     ("multline" . LaTeX-item-equation)
		     ("multline*" . LaTeX-item-equation)
		     ("gather"   . LaTeX-item-equation)
		     ("gather*"  . LaTeX-item-equation)
		     ("gathered" . LaTeX-item-equation)
		     ("align"    . LaTeX-item-equation)
		     ("align*"   . LaTeX-item-equation)
		     ("aligned"  . LaTeX-item-equation)
		     ("alignat"  . LaTeX-item-equation-alignat)
		     ("alignat*" . LaTeX-item-equation-alignat)
		     ("xalignat"  . LaTeX-item-equation-alignat)
		     ("xalignat*" . LaTeX-item-equation-alignat)
		     ("xxalignat" . LaTeX-item-equation-alignat)
		     ("alignedat" . LaTeX-item-equation-alignat)
		     ("flalign"  . LaTeX-item-equation)
		     ("flalign*" . LaTeX-item-equation)
		     ("matrix" .  LaTeX-item-equation)
		     ("pmatrix" .  LaTeX-item-equation)
		     ("bmatrix" .  LaTeX-item-equation)
		     ("Bmatrix" .  LaTeX-item-equation)
		     ("vmatrix" .  LaTeX-item-equation)
		     ("Vmatrix" .  LaTeX-item-equation)
		     ("subarray" . LaTeX-item-equation)
		     ("cases"    . LaTeX-item-equation))
		   LaTeX-item-list))

    ;; When `LaTeX-amsmath-label' is nil, use value of LaTeX-equation-label:
    (unless LaTeX-amsmath-label
      (setq LaTeX-amsmath-label LaTeX-equation-label))

    (setq LaTeX-label-alist
	  ;; Append amsmath environments to `LaTeX-label-alist', in order not to
	  ;; override possible custome values.
	  (append LaTeX-label-alist
		  '(("align"      . LaTeX-amsmath-label)
		    ("alignat"    . LaTeX-amsmath-label)
		    ("xalignat"   . LaTeX-amsmath-label)
		    ("multline"   . LaTeX-amsmath-label)
		    ("flalign"    . LaTeX-amsmath-label)
		    ("gather"     . LaTeX-amsmath-label))))

    (set (make-local-variable 'TeX-braces-association)
	 (append '(("\\lvert" . "\\rvert")
		   ("\\lVert" . "\\rVert"))
		 TeX-braces-association))
    (set (make-local-variable 'TeX-left-right-braces)
	 (append '(("\\lvert") ("\\rvert") ("\\lVert") ("\\rVert"))
		 TeX-left-right-braces))

    ;; amsmath includes amstext, amsbsy, & amsopn.
    ;; So we run their hooks, too.
    (TeX-run-style-hooks "amstext" "amsbsy" "amsopn")

    ;; If RefTeX is loaded, make it recognize the amsmath environments.
    (when (fboundp 'reftex-add-to-label-alist)
      (reftex-add-to-label-alist '(AMSTeX)))))
 LaTeX-dialect)

(defun LaTeX-amsmath-env-alignat (env)
  "Insert ENV with column number specifications.
Insert suitable number of ampersands also if possible."
  (let ((ncols (read-string "Number of columns: ")))
    (LaTeX-insert-environment env (concat TeX-grop ncols TeX-grcl))
    (LaTeX-item-equation-alignat t)))

(defun LaTeX-amsmath-env-alignedat (env)
  "Insert ENV with position and column number specifications.
Insert suitable number of ampersands also if possible."
  (let ((where (read-string "(Optional) Vertical position (t or b): "))
	(ncols (read-string "Number of columns: ")))
    (unless (string= where "")
      (setq where (concat LaTeX-optop where LaTeX-optcl)))
    (LaTeX-insert-environment env (concat where TeX-grop ncols TeX-grcl))
    (LaTeX-item-equation-alignat t)))

(defun LaTeX-item-equation (&optional suppress)
  "Insert contents to terminate a line in multi-line equations environment.
Put line break macro on the last line.  If the current environment
wants \\label, insert it also.

If SUPPRESS is non-nil, do not insert line break macro."
  (unless suppress
    (end-of-line 0)
    (just-one-space)
    (TeX-insert-macro "\\")
    (forward-line 1)
    (indent-according-to-mode))
  (let ((env (LaTeX-current-environment)))
    (when (and (assoc env LaTeX-label-alist)
	       (LaTeX-label env 'environment))
      (LaTeX-newline)
      (indent-according-to-mode))))

(defun LaTeX-item-equation-alignat (&optional suppress)
  "Insert contents to terminate a line in multi-line equations environment.
Put line break macro on the last line.  Next, if the current
environment wants \\label, insert it also.  And insert suitable number
of ampersands if possible.

If SUPPRESS is non-nil, do not insert line break macro."
  (LaTeX-item-equation suppress)
  (LaTeX-insert-ampersands
   (concat "\\(?:"
	   (regexp-quote LaTeX-optop) "[tb]" (regexp-quote LaTeX-optcl)
	   "\\)?")
   'LaTeX-amsmath-alignat-number-of-ampersands))

(defun LaTeX-amsmath-alignat-number-of-ampersands (start end)
  "Return the number of ampersands to insert.
The number is 2N-1 where N is the number taken from the text between
START and END."
  (let ((num (string-to-number (buffer-substring-no-properties start end))))
    (if (integerp num) (+ num num -1))))

(defvar LaTeX-amsmath-package-options '("intlimits" "nointlimits"
					"sumlimits" "nosumlimits"
					"namelimits" "nonamelimits"
					"leqno" "reqno" "centertags"
					"tbtags" "cmex10" "fleqn" "?")
    "Package options for the amsmath package.")

;;; amsmath.el ends here.
