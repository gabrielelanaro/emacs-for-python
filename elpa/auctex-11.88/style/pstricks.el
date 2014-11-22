;;; pstricks.el --- AUCTeX style for the `pstricks' package.

;; Copyright (C) 2007, 2009, 2013 Free Software Foundation, Inc.

;; Author: Holger Sparr <holger.sparr@gmx.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2007-06-14
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
;;
;; AUCTeX style file for PSTricks
;;
;; Support for basic PSTricks macros and their arguments. Separate
;; history variables for point, angle, ... arguments.
;;
;; Parameter input completion together with input completion for certain
;; parameters (e.g. linestyle, linecolor and the like).
;;
;; There is a PSTricks-specific support for adding new parameters to
;; existing parameter lists or changing existing ones in optional
;; macro arguments.  You might want to make those available through
;; key bindings by using something like
;; (define-key LaTeX-mode-map (kbd "C-c p a")
;;   'LaTeX-pst-parameter-add)
;; (define-key LaTeX-mode-map (kbd "C-c p c")
;;   'LaTeX-pst-parameter-change-value)
;; in a personal style file for PSTricks.

;;; History:
;;
;; 14/06/2007 rewrite of pstricks.el based on Jean-Philippe Georget's
;;            pstricks.el version found on <URI:
;;            http://www.emacswiki.org/cgi-bin/wiki/pstricks.el>

;;; TODO:
;;
;; -- Use alist or hash-table for parameter input
;; -- Add more regularly used PSTricks macros
;; -- Prevent errors in AUCTeX modes other than LaTeX mode.
;; -- Check if the functionality for adding and changing parameters
;;    can be generalized.

;;; Code:

;;; General Functions

(defun TeX-arg-compl-list (list &optional prompt hist)
  "Input a value after PROMPT with completion from LIST and HISTORY."
  (let ((first (car list)))
    (if (and first (listp first))
        (let ((func (nth 0 first))
              (prompt (concat (or (nth 1 first) prompt) ": "))
              (compl (nth 2 first))
              (hist (or (nth 3 first) hist))
              (crm-separator (nth 4 first))
              res)
          (setq list (cdr list))
          (cond ((eq func 'completing-read-multiple)
                 (setq res (funcall func prompt list nil compl nil hist))
                 (mapconcat 'identity res crm-separator))
                ((eq func 'completing-read)
                 (setq res
                       (funcall func prompt list nil compl nil hist)))))
      (completing-read (concat prompt ": ") list nil nil nil hist))))

;; XXX: Show default value in prompt.  Perhaps extend
;; `TeX-argument-prompt' to do that.
(defun LaTeX-pst-what (what prompt default &optional arg)
  "Ask for WHAT with PROMPT with DEFAULT.
The corresponding lists LaTeX-pst-<what>-\\(list\\|history\\)
have to exist.

\(Used to define functions named LaTeX-pst-<what>.\))"
  (let ((list (intern (concat "LaTeX-pst-" what "-list")))
        (hist (intern (concat "LaTeX-pst-" what "-history"))))
    (if (not arg)
        (setq arg (TeX-arg-compl-list (symbol-value list) prompt hist)))
    (if (string= arg "")
        default
      (add-to-list list arg)
      arg)))

(defun LaTeX-pst-input-int (prompt arg)
  "Return number as string asked for with PROMPT if no number
passed with ARG."
  (unless (numberp arg)
    (setq arg (read-number (concat prompt ": ") 2)))
  (number-to-string arg))

(defun LaTeX-pst-enclose-obj (symbol op cl)
  "Enclose string returned by the `funcall' SYMBOL in OP and CL
character."
  (let ((str (funcall symbol)))
    (if str (insert (char-to-string op) str (char-to-string cl)))))

(defun LaTeX-package-parameter-value (param pname)
  "Ask for possible value of parameter PARAM given as string
available through package name PNAME and return \"param=value\"."
  (add-to-list (intern (concat "LaTeX-" pname "-parameters-name-list"))
               param)
  ;; select predefined set
  (let* ((cregexp
          (symbol-value
           (intern (concat "LaTeX-" pname
                           "-parameters-completion-regexp"))))
         (bregexp
          (symbol-value (intern (concat "LaTeX-" pname
                                        "-parameters-boolean-regexp"))))
         (parlist (cond
                   ((string-match cregexp param)
                    (intern (concat "LaTeX-" pname "-"
                                    (match-string 0 param) "-list")))
                   ((string-match bregexp param)
                    'LaTeX-pst-boolean-list)))
         val compl)
    ;; ask for value
    (setq val (TeX-arg-compl-list
               (symbol-value parlist)
               (concat "(Press TAB for completions) " param)
               (intern (concat "LaTeX-" pname
                               "-parameters-value-history"))))
    ;; FIXME: This looks broken.  `compl' is never set and unless ""
    ;; is added to parlist (at least in the Boolean case), the prompt
    ;; shown by `TeX-arg-compl-list' will be incorrect.
    (if (and (not compl) parlist) (add-to-list parlist val))
    (if (string= val "") "" (concat param "=" val))))

(defun LaTeX-package-parameters-pref-and-chosen (param pname noskip)
  "Set values for elements of PARAM from package PNAME and
further explicitly typed in parameters and return a comma
separated list as string."
  (let ((allpars "")
        (fask (intern (concat "LaTeX-" pname "-parameter-value")))
        tpara parval)
    (when param
      (while param
        (setq tpara (pop param))
        (setq parval (funcall fask tpara))
        (setq allpars
              (concat allpars
                      (if (or (string= "" allpars) (string= "" parval))
                          "" ",") parval))))
    ;; ask for parameter names as long as none is given
    (when noskip
      (while
          (not
           (string=
            ""
            (setq tpara
                  (completing-read
                   "Parameter name (RET to stop): "
                   (symbol-value (intern
                                  (concat "LaTeX-" pname
                                          "-parameters-name-list")))
                   nil nil nil (intern
                                (concat "LaTeX-" pname
                                        "-parameters-name-history"))))))
        (setq parval (funcall fask tpara))
        ;; concat param=value with other ones
        (setq allpars
              (concat allpars
                      (if (or (string= "" allpars) (string= "" parval))
                          ""
                        ",")
                      parval))))
    (add-to-list
     (intern (concat "LaTeX-" pname "-parameters-history")) allpars)
    allpars))

(defun LaTeX-package-parameters (optional pname preparam param)
  "Ask for parameters and manage several parameter lists for
package PNAME"
  (let ((fask (intern
               (concat "LaTeX-" pname "-parameters-pref-and-chosen")))
        (hlist (intern (concat "LaTeX-" pname "-parameters-history")))
        (nlist
         (symbol-value
          (intern (concat "LaTeX-" pname "-parameters-name-list")))))
    ;;
    (when (and preparam (listp preparam))
      (setq preparam (funcall fask preparam)))
    ;;
    (setq param
          (TeX-completing-read-multiple
           (concat
            "Params (use <Up,Down> for history or RET for choices): ")
           nlist nil nil nil hlist))
    ;;
    (if (not param)
        (setq param (funcall fask nil t))
      (setq param (car (symbol-value hlist))))
    (TeX-argument-insert
     (if (or (string= "" preparam) (eq preparam nil))
         param
       (concat preparam (if (string= "" param) "" (concat "," param))))
     optional)))

;;; Points
(defvar LaTeX-pst-point-list (list "0,0")
  "A list of values for point in pstricks.")

(defvar LaTeX-pst-point-history LaTeX-pst-point-list
  "History of values for point in pstricks.")

(defun LaTeX-pst-point ()
  "Ask for a point and manage point list."
  (LaTeX-pst-what "point"
                  (concat "Point (default " (car LaTeX-pst-point-history) ")")
                  (car LaTeX-pst-point-history)))

(defun LaTeX-pst-point-in-parens (optional)
  "Enclose point in parentheses."
  (LaTeX-pst-enclose-obj 'LaTeX-pst-point ?( ?)))

;;; Angles
(defvar LaTeX-pst-angle-list (list "0")
  "A list of values for angle in pstricks.")

(defvar LaTeX-pst-angle-history nil
  "History of values for angle in pstricks.")

(defun LaTeX-pst-angle ()
  "Ask for a angle and manage angle list"
  (LaTeX-pst-what "angle"
                  (concat "Angle (default " (car LaTeX-pst-angle-list) ")")
                  (car LaTeX-pst-angle-list)))

;;; Extension in one Direction
(defvar LaTeX-pst-extdir-list (list "1")
  "A list of values for extdir in pstricks.")

(defvar LaTeX-pst-extdir-history nil
  "History of values for extdir in pstricks.")

(defun LaTeX-pst-extdir (descr)
  "Ask for a extdir and manage extdir list"
  (LaTeX-pst-what "extdir"
                  (concat descr " (default " (car LaTeX-pst-extdir-list) ")")
                  (car LaTeX-pst-extdir-list)))

;;; Relative Points
(defvar LaTeX-pst-delpoint-list nil
  "A list of values for delpoint in pstricks.")

(defvar LaTeX-pst-delpoint-history nil
  "History of values for delpoint in pstricks.")

;;; Arrows
(defvar LaTeX-pst-arrows-list
  '("->" "<-" "<->" ">-<" ">-" "-<" "<<->>" "<<-" "->>" "|-|" "|-" "-|"
  "|*-|*" "[-]" "[-" "-]" "(-)" "(-" "-)" "*-*" "*-" "-*" "0-0" "0-"
  "-0" "c-c" "c-" "-c" "C-C" "C-" "-C" "cc-cc" "cc-" "-cc" "|<->|" "|<-"
  "->|" "|<*->|*" "|<*-" "->|*" "-")
  "A list of values for arrows in pstricks.")

(defvar LaTeX-pst-arrows-history nil
  "History of values for arrows in pstricks.")

;; XXX: Better ask for arrow start and end separately?
;; `LaTeX-pst-arrows-list' is not exhaustive.
(defun LaTeX-pst-arrows ()
  "Ask for a arrow type and manage arrow type list"
  (or (LaTeX-pst-what "arrows" "Arrow type" nil) ""))

;;; Dots
(defvar LaTeX-pst-dotstyle-list
  '((completing-read "Dot style" nil LaTeX-pst-dotstyle-history)
    "*" "o" "+" "|" "triangle" "triangle*" "square" "square*" "pentagon"
    "pentagon*")
  "A list of values for dotstyle in pstricks.")

(defvar LaTeX-pst-dotstyle-history nil
  "History of values for dotstyle in pstricks.")

;;; Reference Point
(defvar LaTeX-pst-refpoint-list
  '((completing-read "Reference point" t LaTeX-pst-refpoint-history)
    "l" "r" "t" "tl" "lt" "tr" "rt" "b" "bl" "br" "lb" "rb" "B" "Bl"
    "Br" "lB" "rB")
  "A list of values for refpoint in pstricks.")

(defvar LaTeX-pst-refpoint-history nil
  "History of values for refpoint in pstricks.")

(defun LaTeX-pst-refpoint ()
  "Ask for a refpoint and manage refpoint list"
  (LaTeX-pst-what "refpoint" "Reference point" nil))

;;; Color

;; FIXME: Still used?
(defvar LaTeX-pst-color-history nil
  "History of values for color in pstricks.")

;;; Others without History in Completion

(defvar LaTeX-pst-style-list
  '((completing-read "Defined Style" t))
  "A list of values for user defined styles in pstricks.")

;;; Parameters

(defvar LaTeX-pst-parameters-history nil
  "History of values for parameters in pstricks.")

(defvar LaTeX-pst-parameters-value-history nil
  "History of parameter values in pstricks.")

(defvar LaTeX-pst-basic-parameters-name-list
  '("arcsep" "arcsepA" "arcsepB" "arrowinset" "arrowlength" "arrows"
    "arrowscale" "arrowsize" "border" "bordercolor" "boxsep"
    "bracketlength" "cornersize" "curvature" "dash" "dimen" "dotangle"
    "dotscale" "dotsep" "dotsize" "dotstyle" "doublecolor" "doubleline"
    "doublesep" "doubleset" "fillcolor" "fillstyle" "framearc"
    "framesep" "gangle" "gridcolor" "griddots" "gridlabelcolor"
    "gridlabels" "gridwidth" "hatchangle" "hatchcolor" "hatchsep"
    "hatchsepinc" "hatchwidth" "hatchwidthinc" "header" "labelsep"
    "liftpen" "linearc" "linecolor" "linestyle" "linetype" "linewidth"
    "rbracketlength" "ref" "runit" "shadow" "shadowangle" "shadowcolor"
    "shadowsize" "showgrid" "showpoints" "style" "subgridcolor"
    "subgriddiv" "subgriddots" "subgridwidth" "swapaxes" "tbarsize"
    "trimode" "unit" "xunit" "yunit")
  "A list of parameter names in pstricks.")


(defvar LaTeX-pst-boolean-list '("true" "false")
  "List of binary values for key=value completion.")

;; XXX: Colors can actually be given as [-]<color>[!<num>].
(defvar LaTeX-pst-color-list
  '("black" "darkgray" "gray" "lightgray" "white"
    "red" "green" "blue" "cyan" "magenta" "yellow")
  "List of colors predefined in PSTricks.")

(defvar LaTeX-pst-fillstyle-list
  '("none" "solid" "vlines" "vlines*" "hlines" "hlines*" "crosshatch"
    "crosshatch*" "boxfill")
  "List of fill styles defined in PSTricks.")

;; From PSTricks: PostScript macros for Generic TeX, User's Guide,
;; Timothy Van Zandt, 25 July 2003, Version 97.
;; FIXME: Provide separate variables tailored to the different macros.
(defvar LaTeX-pst-basic-parameters-list
  '(;; Dimensions, coordinates and angles
    ("unit")
    ("xunit")
    ("yunit")
    ("runit")
    ;; Basic graphics parameters
    ("linewidth")
    ("linecolor" LaTeX-pst-color-list)
    ("fillstyle" LaTeX-pst-fillstyle-list)
    ("fillcolor" LaTeX-pst-color-list)
    ("arrows" LaTeX-pst-arrows-list)
    ("showpoints" LaTeX-pst-boolean-list)
    ;; Lines and polygons
    ("linearc")
    ("framearc")
    ("cornersize" ("relative" "absolute"))
    ("gangle")
    ;; Arcs, circles and ellipses
    ("arcsepA")
    ("arcsepB")
    ("arcsep")
    ;; Curves
    ("curvature")
    ;; Dots
    ("dotstyle" ("*" "o" "Bo" "x" "+" "B+" "asterisk" "Basterisk" "oplus"
		 "otimes" "|" "B|" "square" "Bsquare" "square*" "diamond"
		 "Bdiamond" "diamond*" "triangle" "Btriangle" "triangle*"
		 "pentagon" "Bpentagon" "pentagon*"))
    ("dotsize")
    ("dotscale")
    ("dotangle")
    ;; Grids
    ("gridwidth")
    ("gridcolor" LaTeX-pst-color-list)
    ("griddots")
    ("gridlabels")
    ("gridlabelcolor" LaTeX-pst-color-list)
    ("subgriddiv")
    ("subgridwidth")
    ("subgridcolor" LaTeX-pst-color-list)
    ("subgriddots")
    ;; Plots
    ("plotstyle" ("dots" "line" "polygon" "curve" "ecurve" "ccurve"))
    ("plotpoints")
    ;; Coordinate systems
    ("origin")
    ("swapaxes" LaTeX-pst-boolean-list)
    ;; Line styles
    ("linestyle" ("none" "solid" "dashed" "dotted"))
    ("dash")
    ("dotsep")
    ("border")
    ("bordercolor" LaTeX-pst-color-list)
    ("doubleline" LaTeX-pst-boolean-list)
    ("doublesep")
    ("doublecolor" LaTeX-pst-color-list)
    ("shadow" LaTeX-pst-boolean-list)
    ("shadowsize")
    ("shadowangle")
    ("shadowcolor" LaTeX-pst-color-list)
    ("dimen" ("outer" "inner" "middle"))
    ;; Fill styles
    ("hatchwidth")
    ("hatchsep")
    ("hatchcolor" LaTeX-pst-color-list)
    ("hatchangle")
    ("addfillstyle" LaTeX-pst-fillstyle-list)
    ;; Arrowheads and such
    ("arrowsize")
    ("arrowlength")
    ("arrowwinset")
    ("tbarsize")
    ("bracketlength")
    ("rbracketlength")
    ("arrowscale")
    ;; Parameters
    ("linetype")
    ;; Graphics objects
    ("liftpen")
    ;; Placing and rotating whatever
    ("labelsep")
    ;; Axes
    ("labels" ("all" "x" "y" "none"))
    ("showorigin" LaTeX-pst-boolean-list)
    ("ticks" ("all" "x" "y" "none"))
    ("tickstyle" ("full" "top" "bottom"))
    ("ticksize")
    ("axesstyle" ("axes" "frame" "none"))
    ;; Framed boxes
    ("framesep")
    ("boxsep")
    ("trimode" ("*" "U" "D" "R" "L"))
    ;; Nodes
    ("href")
    ("vref")
    ("radius")
    ;; Node connections
    ("nodesep")
    ("arcangle")
    ("angle")
    ("arm")
    ("loopsize")
    ("ncurv")
    ("boxsize")
    ("offset")
    ;; Node connections labels: I
    ("ref")
    ("nrot")
    ("npos")
    ("shortput" ("none" "nab" "tablr" "tab"))
    ;; Node connection labels: II
    ("tpos")
    ;; Attaching labels to nodes
    ("rot")
    ;; Mathematical diagrams and graphs
    ("mnode" ("R" "r" "C" "f" "p" "circle" "oval" "dia" "tri" "dot" "none"))
    ("emnode" ("R" "r" "C" "f" "p" "circle" "oval" "dia" "tri" "dot" "none"))
    ("name")
    ("nodealign" LaTeX-pst-boolean-list)
    ("mcol" ("l" "r" "c"))
    ("rowsep")
    ("colsep")
    ("mnodesize")
    ;; ...
    )
  "List of keys and values for PSTricks macro arguments.")

(defvar LaTeX-pst-parameters-name-list
  LaTeX-pst-basic-parameters-name-list
  "A list of all parameters with completion.")

(defvar LaTeX-pst-parameters-name-history nil
  "History of parameter names in pstricks.")

(defvar LaTeX-pst-parameters-completion-regexp
  "\\(arrows\\|linestyle\\|fillstyle\\|color\\|trimode\\|dotstyle\\|\\<style\\)"
  "Regexp for `string-match'ing a parameter.")

(defvar LaTeX-pst-parameters-boolean-regexp
  "\\(doubleline\\|shadow\\>\\|show[a-zA-Z]+\\)"
  "Regexp for `string-match'ing a parameter.")

(defun LaTeX-pst-parameter-value (param)
  "See documentation of `LaTeX-package-parameter-value'."
  (LaTeX-package-parameter-value param "pst"))

(defun LaTeX-pst-parameters-pref-and-chosen (param &optional noskip)
  "See documentation of `LaTeX-package-parameters-pref-and-chosen'."
  (LaTeX-package-parameters-pref-and-chosen param "pst" noskip))

;; FIXME: This is likely only a transitional function used until all
;; macros got their calls to `TeX-arg-key-val' with tailored parameter
;; lists.
(defun LaTeX-pst-parameters (optional)
  "Prompt for general parameters of a PSTricks argument."
  (TeX-arg-key-val optional LaTeX-pst-basic-parameters-list))

;;; Macros
(defun LaTeX-pst-macro-psarc (optional &optional arg)
  "Return \\psarc arguments after querying."
  (let ((arrows (LaTeX-pst-arrows))
        (pnt (if current-prefix-arg nil (LaTeX-pst-point))))
    (insert (if arrows (format "{%s}" arrows) "")
            (if pnt (format "(%s)" pnt) "")
            "{" (LaTeX-pst-extdir "Radius") "}{" (LaTeX-pst-angle) "}{"
            (LaTeX-pst-angle) "}")))

(defun LaTeX-pst-macro-pscircle (optional &optional arg)
  "Return \\pscircle arguments after querying."
  (insert "(" (LaTeX-pst-point) "){" (LaTeX-pst-extdir "Radius") "}"))

(defun LaTeX-pst-macro-rput (optional &optional arg)
  "Return \\rput arguments after querying."
  (let ((refpoint (LaTeX-pst-refpoint))
        (rotation (if current-prefix-arg (LaTeX-pst-angle) nil)))
    (insert (if refpoint (concat "[" refpoint "]") "")
            (if rotation
                (concat "{" rotation "}")
              "") "(" (LaTeX-pst-point) ")")))

(defun LaTeX-pst-macro-uput (optional &optional arg)
  "Return \\uput arguments after querying."
  (let ((dist (LaTeX-pst-extdir "Distance"))
        (refpoint (LaTeX-pst-refpoint)))
    (insert (if dist (concat "{" dist "}") "")
            (if refpoint
                (concat "[" (LaTeX-pst-refpoint) "]")
              "[]")
            "{" (LaTeX-pst-angle) "}(" (LaTeX-pst-point) ")")))

(defun LaTeX-pst-macro-multirputps (optional &optional arg)
  "Return \\multirput or \\multips arguments after querying."
  (let ((refpoint (LaTeX-pst-refpoint))
        (rotation (if current-prefix-arg (LaTeX-pst-angle) nil))
        (pnt (LaTeX-pst-point))
        (dpnt (LaTeX-pst-what "delpoint" "Increment (default 1,1)" "1,1"))
        (repi (LaTeX-pst-input-int "Repetitions" nil)))
    (insert (if refpoint (format "[%s]" refpoint) "")
            (if rotation (format "{%s}" rotation) "")
            "(" pnt ")(" dpnt "){" repi "}")))

(defun LaTeX-pst-macro-psline (optional &optional arg)
  "Return \\psline or \\ps[ce]?curve[*] arguments after querying."
  (let ((arrows (LaTeX-pst-arrows))
        (pnt1 (LaTeX-pst-point))
        (pnt2 (LaTeX-pst-point)))
    (insert (if arrows (format "{%s}" arrows) "") "(" pnt1 ")" )
    (while (and (not (string= pnt2 "")) (not (string= pnt1 pnt2)))
      (insert "(" pnt2 ")")
      (setq pnt1 pnt2)
      (setq pnt2 (LaTeX-pst-point)))))

(defun LaTeX-pst-macro-psdots (optional single)
  "Return \\psdot[s]? arguments after querying."
  (let* ((pnt1 (LaTeX-pst-point))
         (pnt2 (if single pnt1 (LaTeX-pst-point))))
    (insert "(" pnt1 ")")
    (while (and (not (string= pnt2 "")) (not (string= pnt1 pnt2)))
      (setq pnt1 pnt2)
      (insert "(" pnt1 ")")
      (setq pnt2 (LaTeX-pst-point)))))

(defun LaTeX-pst-macro-parabola (optional &optional arg)
  "Return \\parabola arguments after querying."
  (let ((arrows (LaTeX-pst-arrows)))
    (insert (if arrows (format "{%s}" arrows) "")
            "(" (LaTeX-pst-point) ")(" (LaTeX-pst-point) ")")))

(defun LaTeX-pst-macro-pnt-twolen (optional prompt1 prompt2)
  "Return point and 2 paired lengths in separate parens as arguments."
  ;; insert \psellipse[*]?, \psdiamond or \pstriangle  arguments
  (let ((pnt (if current-prefix-arg nil (LaTeX-pst-point))))
    (insert (if pnt (format "(%s)" pnt) "")
            "(" (LaTeX-pst-extdir prompt1) ","
            (LaTeX-pst-extdir prompt2) ")")))

(defun LaTeX-pst-macro-psbezier (optional &optional arg)
  "Return \\psbezier arguments after querying."
  (let ((arrows (LaTeX-pst-arrows))
        (pnt1 (LaTeX-pst-point))
        (pnt2 (LaTeX-pst-point))
        (pnt3 (LaTeX-pst-point)))
    (insert (if arrows (format "{%s}" arrows) "")
            "(" pnt1 ")(" pnt2 ")")
    (while (not (string= pnt2 pnt3))
      (insert "(" pnt3 ")")
      (setq pnt2 pnt3)
      (setq pnt3 (LaTeX-pst-point)))))

(defun LaTeX-pst-macro-pspolygon (optional &optional arg)
  "Return \\pspolygon arguments after querying."
  (let ((pnt1 (LaTeX-pst-point))
        (pnt2 (LaTeX-pst-point))
        (pnt3 (LaTeX-pst-point)))
    (insert "(" pnt1 ")(" pnt2 ")")
    (while (not (string= pnt2 pnt3))
      (insert "(" pnt3 ")")
      (setq pnt2 pnt3)
      (setq pnt3 (LaTeX-pst-point)))))

(defun LaTeX-pst-macro-psframe (optional &optional arg)
  "Return \\psframe arguments after querying."
  (let ((pnt1 (if current-prefix-arg nil (LaTeX-pst-point)))
        (pnt2 (LaTeX-pst-point)))
    (insert (if pnt1 (format "(%s)" pnt1) "") "(" pnt2 ")")))

(defun LaTeX-pst-macro-psgrid (optional &optional arg)
  "Return \\psgrid arguments after querying."
  (let* ((cpref (if current-prefix-arg (car current-prefix-arg) 0))
         (pnt1 (if (> cpref 4) (LaTeX-pst-point) nil))
         (pnt2 (if (> cpref 0) (LaTeX-pst-point) nil))
         (pnt3 (if (> cpref 0) (LaTeX-pst-point) nil)))
    (insert (if pnt1 (format "(%s)" pnt1) "")
            (if pnt2 (format "(%s)(%s)" pnt2 pnt3) ""))))

(defun LaTeX-pst-macro-newpsobject (&optional arg)
  "Return \\newpsobject arguments after querying."
  (insert "{" (read-string "New PSObject Name: ") "}"
	  ;; FIXME: It would be better to use something more confined
	  ;; than `TeX-symbol-list'.
          "{" (completing-read "Parent Object: " (TeX-symbol-list))
          "}"))

;;; Environments
(defun LaTeX-pst-env-pspicture (env)
  "Create new pspicure environment."
  (let ((opt (multi-prompt-key-value
	      (TeX-argument-prompt t "Options" nil)
	      '(("showgrid") ("shift"))))
	(p0 (LaTeX-pst-what "point" "Lower left (default 0,0)" "0,0"))
        (p1 (LaTeX-pst-what "point" "Upper right (default 1,1)" "1,1"))
        corn)
    (setq corn (concat (unless (string= "" opt) (format "[%s]" opt))
                       (if (string= "0,0" p0) "" (format "(%s)" p0))
                       "(" p1 ")"))
    (LaTeX-insert-environment env corn)))

;;; Self Parsing --  see (info "(auctex)Hacking the Parser")
(defvar LaTeX-auto-pstricks-regexp-list
  '(("\\\\newps\\(object\\){\\([a-zA-Z]+\\)}{\\([a-zA-Z]+\\)}" (1 2 3)
     LaTeX-auto-pstricks)
    ("\\\\newps\\(fontdot\\){\\([a-zA-Z]+\\)}" (1 2)
     LaTeX-auto-pstricks)
    ("\\\\newps\\(style\\){\\([a-zA-Z]+\\)}" (1 2)
     LaTeX-auto-pstricks)
    ("\\\\define\\(color\\){\\([a-zA-Z]+\\)}{\\(rgb\\|cmyk\\)}" (1 2 3)
     LaTeX-auto-pstricks)
    ("\\\\new\\(rgb\\|hsb\\|cmyk\\)\\(color\\){\\([a-zA-Z]+\\)}" (2 3 1)
     LaTeX-auto-pstricks))
  "List of regular expressions to extract arguments of \\newps* macros.")

(defvar LaTeX-auto-pstricks nil
  "Temporary for parsing \\newps* definitions.")

(defun LaTeX-pst-cleanup ()
  "Move symbols from `LaTeX-auto-pstricks' to `TeX-auto-symbol'."
  (mapcar
   (lambda (list)
     (let ((type (car list)))
       (cond ((string= type "object")
              (setq TeX-auto-symbol
                    (cons (list (nth 1 list)
                                (caddr (assoc (nth 2 list)
                                              (TeX-symbol-list))))
                          TeX-auto-symbol)))
             ((string= type "fontdot")
              (add-to-list 'LaTeX-pst-dotstyle-list (nth 1 list) t))
             ((string= type "style")
              (add-to-list 'LaTeX-pst-style-list (nth 1 list) t))
             ((string= type "color")
              (add-to-list 'LaTeX-pst-color-list (nth 1 list) t)
	      ;; FIXME: Why is an entry with "-" in front added?
              (add-to-list 'LaTeX-pst-color-list
                           (concat "-" (nth 1 list)) t)))))
   LaTeX-auto-pstricks))

(defun LaTeX-pst-prepare ()
  "Clear `LaTeX-auto-pstricks' before use."
  (setq LaTeX-auto-pstricks nil))

;; FIXME: This does not seem to work unless one does a manual reparse.
;; Check e.g. with "\definecolor" and "fillcolor=".
(add-hook 'TeX-auto-prepare-hook 'LaTeX-pst-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-pst-cleanup)

;;; Additional Functionality
(defun LaTeX-pst-parameters-add (&optional arg)
  "With ARG as prefix-argument insert new parameter\(s\) behind
nearest backward LaTeX macro in brackets. Without ARG add
parameter\(s\) to the already existing ones at the end of the
comma separated list. Point has to be within the sexp to modify."
  (interactive "P")
  (let ((newpara  (LaTeX-pst-parameters-pref-and-chosen nil t))
        (regexp "\\(") beg end check)
    (if arg
        (progn
          (re-search-backward "\\\\\\([a-zA-Z]\\)")
          (forward-word 1)
          (insert-pair nil ?[ ?]))
      (up-list 1)
      (backward-char 1)
      (save-excursion
        (setq end (point))
        (up-list -1)
        (while (re-search-forward "\\([a-zA-Z]+\\)=" end 'limit)
          (setq regexp (concat regexp
                               (match-string-no-properties 1) "\\|")))
        (setq regexp (concat (substring regexp 0 -1) ")"))
        (setq check (string-match regexp newpara))))
    (when newpara
      (insert (if arg "" ",") newpara)
      (when check
        (message
         "At least one Parameters appears twice. PLEASE CHECK!")))))
;; FIXME: Only define a key for this once it is a general-purpose
;; facility, i.e. not just for pstricks but all types of macros.
;; (define-key LaTeX-mode-map "\C-c\C-x\C-a" 'LaTeX-pst-parameters-add)

(defvar LaTeX-pst-value-regexp
  "\\([-!.a-zA-Z0-9]*\\s\\?[-!.a-zA-Z0-9]+\\)"
  "Expression matching a parameter value.")

(defun LaTeX-pst-parameter-remove-value ()
  "Remove value of current parameter and return parameter name."
  (re-search-backward
   (concat "\\(\\s(\\|,\\)[a-zA-Z]+\\([a-zA-Z]\\|=\\|="
           LaTeX-pst-value-regexp "\\)"))
  (re-search-forward "\\([a-zA-Z]+\\)=")
  (let ((para (match-string-no-properties 1)))
    (re-search-forward LaTeX-pst-value-regexp)
    (delete-region (match-beginning 1) (match-end 1))
    para))

(defun LaTeX-pst-parameter-change-value ()
  "Replace parameter value with a new one."
  (interactive)
  (let* ((para (LaTeX-pst-parameter-remove-value))
         (symb
          (when (and
                 (string-match
                  LaTeX-pst-parameters-completion-regexp para)
                 (boundp
                  (intern
                   (concat "LaTeX-pst-" (match-string 0 para) "-list"))))
            (intern (concat "LaTeX-pst-" (match-string 0 para)
                            "-list")))))
    (insert (TeX-arg-compl-list (symbol-value symb) "New Value"
                                'LaTeX-pst-parameters-value-history))))
;; FIXME: Only define a key for this once it is a general-purpose
;; facility, i.e. not just for pstricks but all types of macros.  (See
;; also `LaTeX-pst-parameters-add'.  Note that a parameter change
;; should better be made available through a `C-u' prefix of the
;; binding for the function doing the parameter addition.)
;; (define-key LaTeX-mode-map "\C-c\C-x\C-v" 'LaTeX-pst-parameter-change-value)

(TeX-add-style-hook
 "pstricks"
 (lambda ()
   (unless (member "pst-pdf" TeX-active-styles)
     (TeX-PDF-mode-off))
   (mapc 'TeX-auto-add-regexp LaTeX-auto-pstricks-regexp-list)
   (LaTeX-add-environments
    '("pspicture" LaTeX-pst-env-pspicture)
    "overlaybox" "psclip")
   (TeX-add-symbols
    '("AltClipMode" 0) '("DontKillGlue" 0) '("KillGlue" 0)
    '("NormalCoor" 0) '("SpecialCoor" 0) '("PSTricksLoaded" 0)
    '("PSTricksOff" 0) '("altcolormode" 0) '("pslinecolor" 0)
    '("pslinestyle" 0) '("pslinetype" 0) '("pslinewidth" 0)
    '("pslabelsep" 0) '("radian" 0) '("psunit" 0) '("psrunit" 0)
    '("psxunit" 0) '("psyunit" 0)
    '("arrows" (TeX-arg-eval LaTeX-pst-arrows))
    '("clipbox" ["Border"] t)
    '("closedshadow" [LaTeX-pst-parameters])
    '("openshadow" [LaTeX-pst-parameters])
    "closepath" "code" "coor" "curveto" "degrees" "dim" "endpsclip"
    "file" "fill" "grestore" "gsave" "lineto" "movepath" "moveto"
    "mrestore" "msave" "newpath" "rcoor" "rcurveto" "rlineto" "rotate"
    "scale" "stroke" "swapaxes" "translate"
    '("newcmykcolor" "Name" "Quadruple")
    '("newrgbcolor" "Name" "Triple") '("newhsbcolor" "Name" "Triple")
    '("newgray" "Name" "Value")
    '("newpsobject" LaTeX-pst-macro-newpsobject LaTeX-pst-parameters)
    '("newpsstyle" "New PSStyle Name" LaTeX-pst-parameters)
    '("newpsfontdot" "New PSDot Name" ["Factors"]
      "Fontname" "Character Number (Hex)")
    '("parabola" [LaTeX-pst-parameters] LaTeX-pst-macro-parabola)
    '("parabola*" [LaTeX-pst-parameters] LaTeX-pst-macro-parabola)
    '("psarc" [LaTeX-pst-parameters] LaTeX-pst-macro-psarc)
    '("psarc*" [LaTeX-pst-parameters] LaTeX-pst-macro-psarc)
    '("psarcn" [LaTeX-pst-parameters] LaTeX-pst-macro-psarc)
    '("pswedge" [LaTeX-pst-parameters] LaTeX-pst-macro-psarc)
    '("psbezier" [LaTeX-pst-parameters] LaTeX-pst-macro-psbezier)
    '("psbezier*" [LaTeX-pst-parameters] LaTeX-pst-macro-psbezier)
    '("pscbezier" [LaTeX-pst-parameters] LaTeX-pst-macro-pspolygon)
    '("pscircle" [LaTeX-pst-parameters] LaTeX-pst-macro-pscircle)
    '("psccurve" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("psccurve*" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("pscurve" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("pscurve*" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("pscustom" [LaTeX-pst-parameters])
    '("psdiamond" [LaTeX-pst-parameters]
      (LaTeX-pst-macro-pnt-twolen "Width" "Height"))
    '("pstriangle" [LaTeX-pst-parameters]
      (LaTeX-pst-macro-pnt-twolen "Width" "Height"))
    '("psdot" [LaTeX-pst-parameters] (LaTeX-pst-macro-psdots t))
    '("psdots" [LaTeX-pst-parameters] (LaTeX-pst-macro-psdots nil))
    '("psecurve" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("psecurve*" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("psellipse" [LaTeX-pst-parameters]
      (LaTeX-pst-macro-pnt-twolen "Radius x" "Radius y"))
    '("psellipse*" [LaTeX-pst-parameters]
      (LaTeX-pst-macro-pnt-twolen "Radius x" "Radius y"))
    '("psframe" [LaTeX-pst-parameters] LaTeX-pst-macro-psframe)
    '("psframe*" [LaTeX-pst-parameters] LaTeX-pst-macro-psframe)
    '("psframebox" [LaTeX-pst-parameters] t)
    '("pscirclebox" [LaTeX-pst-parameters] t)
    '("psdblframebox" [LaTeX-pst-parameters] t)
    '("psdiabox" [LaTeX-pst-parameters] t)
    '("psovalbox" [LaTeX-pst-parameters] t)
    '("psshadowbox" [LaTeX-pst-parameters] t)
    '("pstribox" [LaTeX-pst-parameters] t)
    '("psscalebox" "Scaling Factor(s)" t)
    '("psscaleboxto" LaTeX-pst-point-in-parens t)
    '("psgrid" [LaTeX-pst-parameters] LaTeX-pst-macro-psgrid 0)
    '("psline" [LaTeX-pst-parameters] LaTeX-pst-macro-psline)
    '("psoverlay" t)
    '("pspolygon" [LaTeX-pst-parameters] LaTeX-pst-macro-pspolygon)
    '("pspolygon*" [LaTeX-pst-parameters] LaTeX-pst-macro-pspolygon)
    '("psset" LaTeX-pst-parameters)
    '("pssetlength" TeX-arg-macro "Length")
    '("psaddtolength" TeX-arg-macro "Length")
    '("degrees" ["Full Circle"])
    '("qdisk" LaTeX-pst-point-in-parens "Radius")
    '("qline" LaTeX-pst-point-in-parens LaTeX-pst-point-in-parens)
    "pslongbox" "psrotatedown" "psrotateleft" "psrotateright"
    '("rput" LaTeX-pst-macro-rput t)
    '("rput*" LaTeX-pst-macro-rput t)
    '("cput" [LaTeX-pst-parameters]
      (TeX-arg-eval LaTeX-pst-angle) LaTeX-pst-point-in-parens t)
    '("uput" LaTeX-pst-macro-uput t)
    '("multirput" (LaTeX-pst-macro-multirputps t) t)
    '("multips" (LaTeX-pst-macro-multirputps nil) t)))
 LaTeX-dialect)

(defvar LaTeX-pstricks-package-options
  '("97" "plain" "DIA" "vtex" "distiller" "noxcolor")
  "Package options for pstricks.")

;;; pstricks.el ends here
