;;; epigraph.el --- AUCTeX style for `epigraph.sty'

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2012-02-11
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

;; This file adds support for `epigraph.sty'.

;;; Code:

(TeX-add-style-hook
 "epigraph"
 (lambda ()
   (TeX-add-symbols
    '("epigraph" t t)
    '("qitem" t t)
    '("epigraphhead" [ "Distance (a number)" ] t)
    '("dropchapter" TeX-arg-size)
    "epigraphwidth"
    "textflush"
    "epigraphflush"
    "sourceflush"
    "epigraphsize"
    "epigraphrule"
    "beforeepigraphskip"
    "afterepigraphskip"
    "undodrop"
    "cleartoevenpage")

   (LaTeX-add-environments
    '("epigraphs" LaTeX-env-item))

   (add-to-list 'LaTeX-item-list '("epigraphs" . LaTeX-epigraph-qitem))

   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("epigraph" "{{")
				("qitem" "{{")
				("dropchapter" "{")
				("epigraphhead" "{")) 'function)
     (font-latex-add-keywords '("cleartoevenpage") 'warning)))
 LaTeX-dialect)

(defvar LaTeX-epigraph-package-options nil
  "Package options for the epigraph package.")

;; adapted from latex.el:`LaTeX-item-bib'
(defun LaTeX-epigraph-qitem ()
  "Insert a new qitem for use in the epigraphs environment."
  (TeX-insert-macro "qitem"))

;;; epigraph.el ends here
