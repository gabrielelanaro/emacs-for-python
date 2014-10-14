;;; pst-plot.el --- AUCTeX style for `pst-plot.sty'

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

;; This file adds support for `pst-plot.sty'.

;;; TODO:
;;
;; -- improve symbol support (especially the pstScalePoints macros)
;; -- check for multido.el necessity

;;; Code:

;; Self Parsing -- see (info "(auctex)Hacking the Parser")
(defvar LaTeX-auto-pstplot-regexp-list
  '(("\\\\\\(save\\|read\\)data{?\\(\\\\[a-zA-Z]+\\)}?"
     2 LaTeX-auto-pstplot))
  "List of regular expressions to extract arguments of \\*data
  macros.")

(defvar LaTeX-auto-pstplot nil
  "Temporary for parsing \\*data definitions.")

(defun LaTeX-pstplot-cleanup ()
  "Move symbols from `LaTeX-auto-pstplot to `TeX-auto-symbol'."
  (mapcar (lambda (symbol)
            ;; (setq TeX-symbol-list (cons (list symbol 0) TeX-symbol-list))
            ;; (setq TeX-auto-symbol (cons (list symbol 0) TeX-auto-symbol)))
            (add-to-list 'LaTeX-pstplot-datasets symbol))
            LaTeX-auto-pstplot))

(defun LaTeX-pstplot-prepare ()
  "Clear `LaTeX-auto-pstplot' before use."
  (setq LaTeX-auto-pstplot nil))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-pstplot-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-pstplot-cleanup)

;;; Parameters
(defvar LaTeX-pstplot-datasets nil
  "List of parsed data sets defined with \\savedata or \\readdata.")

(defvar LaTeX-pstplot-parameters-name-list
  '("axesstyle" "labels" "plotpoints" "plotstyle" "showorigin" "ticks"
    "ticksize" "tickstyle")
  "A list of parameters' name in pst-plot.")

(defvar LaTeX-pst-ticks-list '(t "none" "all" "x" "y")
  "A list of values for ticks in pst-plot.")

(defvaralias 'LaTeX-pst-labels-list 'LaTeX-pst-ticks-list)

(defvar LaTeX-pst-plotstyle-list
  '(t "dots" "line" "polygon" "curve" "ecurve" "ccurve")
  "A list of values for tickstyles in pst-plot.")

(defvar LaTeX-pst-tickstyle-list '(t "full" "top" "bottom")
  "A list of values for tickstyles in pst-plot.")

(defvar LaTeX-pst-axesstyle-list '(t "axes" "frame" "none")
  "A list of values for axesstyles in pst-plot.")

;;; Macros
(defun LaTeX-pst-macro-psaxes (optional &optional arg)
  "Return \\psaxes arguments after querying."
(let* ((cpref (if current-prefix-arg (car current-prefix-arg) 0))
       (arrows (LaTeX-pst-arrows))
       (pnt1 (if (> cpref 4) (LaTeX-pst-point) nil))
       (pnt2 (if (> cpref 0) (LaTeX-pst-point) nil))
       (pnt3 (LaTeX-pst-point)))
  ;; insert \psaxes arguments
  (insert (if arrows (format "{%s}" arrows) "")
          (if pnt1 (format "(%s)" pnt1) "")
          (if pnt2 (format "(%s)" pnt2) "") "(" pnt3 ")")))

;;; Derived defuns
(defun LaTeX-pstplot-datasets-read ()
  (TeX-arg-compl-list "Datasets" LaTeX-pstplot-datasets))

;;; Hook
(TeX-add-style-hook
 "pst-plot"
 (function
  (lambda ()
    (mapcar 'TeX-auto-add-regexp LaTeX-auto-pstplot-regexp-list)
    (TeX-add-symbols
     '("readdata" "Macro Name" TeX-arg-file)
     '("savedata" "Macro Name" ["Values"])
     '("dataplot" ["Options"]
       (TeX-arg-eval LaTeX-pstplot-datasets-read))
     '("fileplot" ["Options"] TeX-arg-file)
     '("listplot" ["Options"] "Values")
     '("pstScalePoints" "X-Mod" "Y-Mod")
     '("psplot" [LaTeX-pst-parameter] "xmin" "xmax" t)
     '("parametricplot" [LaTeX-pst-parameter] "xmin" "xmax" t)
     '("psaxes" [LaTeX-pst-parameters] LaTeX-pst-macro-psaxes)
     "pshlabel"
     "psvlabel")
    (TeX-run-style-hooks
     "pstricks"
     "multido")
    (unless (string-match "plotstyle"
                          LaTeX-pst-parameters-completion-regexp)
      (setq LaTeX-pst-parameters-completion-regexp
            (concat
             (substring LaTeX-pst-parameters-completion-regexp 0 -2)
             "\\|plotstyle\\|ticks\\|tickstyle\\|axesstyle\\|labels\\)")))
    (make-local-variable 'LaTeX-pst-parameters-name-list)
    (setq LaTeX-pst-parameters-name-list
          (append LaTeX-pstplot-parameters-name-list
                  LaTeX-pst-parameters-name-list)))))

;;; pst-plot.el ends here
