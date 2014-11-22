;;; pst-slpe.el --- AUCTeX style for `pst-slpe.sty'

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

;; This file adds support for `pst-slpe.sty'.

;;; TODO:
;;
;; -- 

;;; Code:

;;; Parameters
(defvar LaTeX-pstslpe-parameters-name-list
  '("slopeangle" "slopecenter" "slopecolors" "slopebegin" "slopeend"
    "sloperadius" "slopesteps")
  "A list of parameter names in pst-slpe.")

;;; Aliases
(defvaralias 'LaTeX-pst-slopebegin-list 'LaTeX-pst-color-list)
(defvaralias 'LaTeX-pst-slopeend-list 'LaTeX-pst-color-list)

;;; Define hook
(TeX-add-style-hook
 "pst-slpe"
 (function
  (lambda ()
    (TeX-run-style-hooks
     "pstricks")
    (unless (member "slope" LaTeX-pst-fillstyle-list)
      (setq LaTeX-pst-fillstyle-list
            (append LaTeX-pst-fillstyle-list
                    '("slope" "slopes" "ccslope" "ccslopes" "radslope"
                    "radslopes")))
      (setq LaTeX-pst-parameters-completion-regexp
            (concat
             (substring LaTeX-pst-parameters-completion-regexp 0 -2)
             "\\|slopebegin\\|slopeend\\)")))
    (make-local-variable 'LaTeX-pst-parameters-name-list)
    (setq LaTeX-pst-parameters-name-list
          (append LaTeX-pstslpe-parameters-name-list
                  LaTeX-pst-parameters-name-list))))
 LaTeX-dialect)

;;; pst-slpe.el ends here
