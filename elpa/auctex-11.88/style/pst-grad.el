;;; pst-grad.el --- AUCTeX style for `pst-grad.sty'

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Holger Sparr <holger.sparr@gmx.net>
;; Created: 21 Jun 2007
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

;; This file adds support for `pst-grad.sty'.

;;; TODO:
;;
;; -- 

;;; Code:

;;; Parameters
(defvar LaTeX-pstgrad-parameters-name-list
  '("gradangle" "gradbegin" "gradend" "gradlines" "gradmidpoint"
    "gradientHSB" "GradientCircle" "GradientPos" "GradientScale")
  "A list of parameter names in pst-grad.")

;;; Aliases
(defvaralias 'LaTeX-pst-gradbegin-list 'LaTeX-pst-color-list)
(defvaralias 'LaTeX-pst-gradend-list 'LaTeX-pst-color-list)

;;; Hook
(TeX-add-style-hook
 "pst-grad"
 (function
  (lambda ()
    (TeX-run-style-hooks
     "pstricks")
    (unless (member "gradient" LaTeX-pst-fillstyle-list)
      (setq LaTeX-pst-fillstyle-list (append LaTeX-pst-fillstyle-list
                                             '("gradient")))
      (setq LaTeX-pst-parameters-completion-regexp
            (concat
             (substring LaTeX-pst-parameters-completion-regexp 0 -2)
             "\\|gradbegin\\|gradend\\)")))
    (make-local-variable 'LaTeX-pst-parameters-name-list)
    (setq LaTeX-pst-parameters-name-list
          (append LaTeX-pstgrad-parameters-name-list
                  LaTeX-pst-parameters-name-list))))
 LaTeX-dialect)

;;; pst-grad.el ends here
