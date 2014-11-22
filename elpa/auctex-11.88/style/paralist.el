;;; paralist.el -- AUCTeX style for paralist.sty

;; Copyright (C) 2003-2005, 2014 Free Software Foundation, Inc.

;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created:  2003-10-22
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

;; This file adds support for `paralist.sty'.

;;; Code:

;; Insert an itemize-ish environment and ask for an optional label
(defun LaTeX-paralist-env-item-opt-label (environment)
  "Insert ENVIRONMENT, an optional label and the first item."
  (LaTeX-insert-environment
   environment
   (let ((label (read-string "(Optional) Label: ")))
     (concat (unless (zerop (length label))
               (format "[%s]" label)))))
  (LaTeX-find-matching-begin)
  (end-of-line 1)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(TeX-add-style-hook
 "paralist"
 (lambda ()

   ;; Add compactdesc to the list of environments which have an optional
   ;; argument for each item.
   (add-to-list 'LaTeX-item-list '("compactdesc" . LaTeX-item-argument))

   ;; New symbols
   (TeX-add-symbols
    '("pointedenum")
    '("pointlessenum")
    '("paradescriptionlabel")
    '("setdefaultitem" "First level" "Second level" "Third level"
      "Fourth level")
    '("setdefaultenum" "First level" "Second level" "Third level"
      "Fourth level")
    '("setdefaultleftmargin" "First level" "Second level" "Third level"
      "Fourth level" "Fifth level" "Sixth level"))

   ;; New environments
   (LaTeX-add-environments
    '("asparaenum" LaTeX-paralist-env-item-opt-label)
    '("inparaenum" LaTeX-paralist-env-item-opt-label)
    '("compactenum" LaTeX-paralist-env-item-opt-label)
    '("asparaitem" LaTeX-paralist-env-item-opt-label)
    '("inparaitem" LaTeX-paralist-env-item-opt-label)
    '("compactitem" LaTeX-paralist-env-item-opt-label)
    '("compactdesc" LaTeX-env-item))
   ;; Environments (re)defined only when the package is loaded with particular
   ;; options.
   (unless (LaTeX-provided-package-options-member "paralist" "olditem")
     (LaTeX-add-environments
      '("itemize" LaTeX-paralist-env-item-opt-label)))
   (unless (LaTeX-provided-package-options-member "paralist" "oldenum")
     (LaTeX-add-environments
      '("enumerate" LaTeX-paralist-env-item-opt-label)))
   (when (LaTeX-provided-package-options-member "paralist" "defblank")
     (LaTeX-add-environments
      '("asparablank" LaTeX-env-item)
      '("inparablank" LaTeX-env-item)))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("setdefaultitem" "{{{{")
				("setdefaultenum" "{{{{")
				("setdefaultleftmargin" "{{{{{{"))
			      'variable)))
 LaTeX-dialect)

(defvar LaTeX-paralist-package-options '("newitem" "olditem" "newenum"
					 "oldenum" "alwaysadjust"
					 "neveradjust" "neverdecrease"
					 "increaseonly" "defblank"
					 "pointedenum" "pointlessenum"
					 "cfg" "nocfg" "flushright"
					 "flushleft")
  "Package options for the paralist package.")

;;; paralist.el ends here
