;;; pst-node.el --- AUCTeX style for `pst-node.sty'

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Holger Sparr <holger.sparr@gmx.net>
;; Created: 21 Jun 2007
;; Based on: Jean-Philippe Georget's pst-plot.el
;; Keywords: latex, pstricks, auctex, emacs

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

;; This file adds support for `pst-node.sty'.

;;; TODO:
;;
;; -- self parsing of possible node names
;; -- adding more macro support

;;; Code:

(defalias 'LaTeX-pst-node 'LaTeX-pst-point)

(defvar LaTeX-pstnode-parameters-completion-regexp
  "\\(npos\\|nrot\\)"
  "Regexp for `string-match'ing a parameter.")

(defvar LaTeX-pstnode-parameters-boolean-regexp "show\\([a-zA-Z]+\\)"
  "Regexp for `string-match'ing a parameter.")

(defvar LaTeX-pstnode-npos-list '(".25" ".5" ".75" "1" "1.5" "2")
  "A list of values for npos in nput.")

(defvar LaTeX-pstnode-nrot-list '(":U" ":D" ":R" ":L")
  "A list of values for nrot in nput.")

(defvar LaTeX-pstnode-psmatrix-list
  '("mnode" "emnode" "name" "nodealign" "mocl" "rowsep" "colsep"
    "mnodesize")
  "A list of values for trimode in pstribox.")

;;; Parameters
(defvar LaTeX-pstnode-parameters-history nil
  "History of values for parameters in pst-node.")

(defvar LaTeX-pstnode-parameters-value-history nil
  "History of parameter values in pst-node.")

(defvar LaTeX-pstnode-parameters-name-list
  '("angle" "angleA" "angleB" "arcangle" "arcangleA" "arcangleB" "arm"
    "armA" "armB" "boxsize" "colsep" "framesize" "href" "loopsize"
    "ncurv" "ncurvA" "ncurvB" "nodesepA" "nodesepB" "npos" "nrot"
    "offset" "offsetA" "offsetB" "radius" "vref" "Xnodesep" "XnodesepA"
    "XnodesepB" "Ynodesep" "YnodesepA" "YnodesepB")
  "A list of parameters' name in pst-node.")

(defvar LaTeX-pstnode-parameters-name-history nil
  "History of parameter names in pst-node.")

;;; Derived Functions from pstricks.el defuns
(defun LaTeX-pstnode-parameter-value (param)
  "See documentation of `LaTeX-package-parameter-value'."
  (LaTeX-package-parameter-value param "pstnode"))

(defun LaTeX-pstnode-parameters-pref-and-chosen (param &optional noskip)
  "See documentation of `LaTeX-package-parameters-pref-and-chosen'."
  (LaTeX-package-parameters-pref-and-chosen param "pstnode" noskip))

(defun LaTeX-pstnode-parameters (optional &optional preparam param)
  "See documentation of `LaTeX-package-parameters-pref-and-chosen'."
  (LaTeX-package-parameters optional "pstnode" preparam param))

;;; Macros
(defun LaTeX-pstnode-macro-nput (optional &optional arg)
  "Return \\nput arguments after querying."
  (insert "[rot=" (LaTeX-pst-angle) "]{" (LaTeX-pst-angle) "}{"
          (LaTeX-pst-node) "}"))

(defun LaTeX-pstnode-macro-cnodeput (optional &optional arg)
  "Return \\cnodeput arguments after querying."
  (let ((rotation (if current-prefix-arg (LaTeX-pst-angle) nil))
        (pnt (if current-prefix-arg (LaTeX-pst-point) nil)))
    (insert (if rotation (format "{%s}" rotation) "")
            (if pnt (format "(%s)" pnt) "") "{" (LaTeX-pst-node) "}")))

(defun LaTeX-pstnode-macro-nc (optional &optional arg)
  "Return \\nc* arguments after querying."
  (let ((arrows (LaTeX-pst-arrows)))
    (insert (if arrows (format "{%s}" arrows) "") "{" (LaTeX-pst-node)
            "}{" (LaTeX-pst-node) "}")))

(defun LaTeX-pstnode-macro-pc (optional &optional arg)
  "Return \\pc* arguments after querying."
  (let ((arrows (LaTeX-pst-arrows)))
    (insert (if arrows (format "{%s}" arrows) "") "(" (LaTeX-pst-point)
            ")(" (LaTeX-pst-point) ")")))

(defun LaTeX-pstnode-macro-tnabcput (optional &optional arg)
  "Return \\t?put or \\n?put arguments after querying."
  (TeX-argument-insert (LaTeX-pstnode-parameters-pref-and-chosen
                        '("nrot" "npos")) optional))

;;; Environments
(defun LaTeX-pstnode-env-psmatrix (env)
  "Return psmatrix environment with arguments."
  (let ((opt (completing-read-multiple "Options: "
                                       LaTeX-pstnode-psmatrix-list)))
    (LaTeX-insert-environment env opt)))

(TeX-add-style-hook
 "pst-node"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("psmatrix" LaTeX-pstnode-env-psmatrix))
    (TeX-add-symbols
     '("MakeShortNab" 2) '("MakeShortTablr" 4) '("PSTnodesLoaded" 0)
     '("nput" LaTeX-pstnode-macro-nput TeX-arg-macro)
     '("cnodeput" [LaTeX-pst-parameters] LaTeX-pstnode-macro-cnodeput t)
     '("Cnode" [LaTeX-pstnode-parameters] LaTeX-pst-point-in-parens t)
     '("cnode" [LaTeX-pstnode-parameters] "Radius" t)
     '("fnode" [LaTeX-pstnode-parameters] LaTeX-pst-point-in-parens t)
     '("fnode*" [LaTeX-pstnode-parameters] LaTeX-pst-point-in-parens t)
     '("dotnode" [LaTeX-pstnode-parameters] LaTeX-pst-point-in-parens t)
     '("pnode" LaTeX-pst-point-in-parens t)
     '("Rnode" [LaTeX-pstnode-parameters ("href" "vref")]
       (TeX-arg-eval LaTeX-pst-point) t)
     '("rnode" [LaTeX-pstnode-parameters ("ref")]
       (TeX-arg-eval LaTeX-pst-point) t)
     '("circlenode" [LaTeX-pst-parameters]
       (TeX-arg-eval LaTeX-pst-point) t)
     '("dianode" [LaTeX-pst-parameters] "Node Name" t)
     '("ovalnode" [LaTeX-pst-parameters] "Node Name" t)
     '("trinode" [LaTeX-pst-parameters] "Node Name" t)
     '("dotnode" [LaTeX-pst-parameters] LaTeX-pst-point-in-parens
       "Node Name")
     '("naput" [LaTeX-pstnode-macro-tnabcput] t)
     '("nbput" [LaTeX-pstnode-macro-tnabcput] t)
     '("ncput" [LaTeX-pstnode-macro-tnabcput] t)
     '("taput" [LaTeX-pstnode-macro-tnabcput] t)
     '("tbput" [LaTeX-pstnode-macro-tnabcput] t)
     '("thput" [LaTeX-pstnode-macro-tnabcput] t)
     '("tlput" [LaTeX-pstnode-macro-tnabcput] t)
     '("trput" [LaTeX-pstnode-macro-tnabcput] t)
     '("tvput" [LaTeX-pstnode-macro-tnabcput] t)
     '("ncline" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncarc" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncdiag" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncdiagg" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncbar" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncangle" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncangles" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncloop" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("nccurve" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("nccircle" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncbox" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("ncarcbox" [LaTeX-pst-parameters] LaTeX-pstnode-macro-nc)
     '("pcline" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pccurve" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcarc" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcbar" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcdiag" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcdiagg" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcangle" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcangles" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcloop" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcbox" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("pcarcbox" [LaTeX-pst-parameters] LaTeX-pstnode-macro-pc)
     '("psspan" (TeX-arg-eval LaTeX-pst-input-int))
     '("psrowhook" t)
     '("pscolhook" t))
    (TeX-run-style-hooks
     "pstricks"))))

;;; pst-node.el ends here
