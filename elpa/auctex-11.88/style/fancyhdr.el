;;; fancyhdr.el --- AUCTeX style for `fancyhdr.sty'

;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
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

;; This file adds support for `fancyhdr.sty', version 3.2

;;; Code:

(TeX-add-style-hook
 "fancyhdr"
 (lambda ()
   (TeX-add-symbols
    '("lhead" t)
    '("lfoot" t)
    '("chead" t)
    '("cfoot" t)
    '("rhead" t)
    '("rfoot" t)
    '("nouppercase" t)
    '("MakeUppercase" t)
    '("fancyhead" [ TeX-arg-fancyhdr-position ] t)
    '("fancyfoot" [ TeX-arg-fancyhdr-position ] t)
    '("fancyheadoffset"
      [ (TeX-arg-fancyhdr-position
         "Position" ("LO" "LE" "L" "RE" "RO" "R" "0")) ] t)
    '("fancyfootoffset"
      [ (TeX-arg-fancyhdr-position
         "Position" ("LO" "LE" "L" "RE" "RO" "R" "O")) ] t)
    '("fancyhfoffset"
      [ (TeX-arg-fancyhdr-position "Position" ("E" "O" "L" "R")) ] t)
    '("fancypagestyle" TeX-arg-pagestyle t)

    "headrulewidth" "footrulewidth" "plainfootrulewidth"
    "plainheadrulewidth" "leftmark" "rightmark"
    ;; the manual does not mention any subsubsectionmark (!)
    "chaptermark" "sectionmark" "subsectionmark" "paragraphmark"
    "subparagraphmark" "footrule" "headrule")

   ;; `fancyhdr.sty' supplies these two pagestyles
   (LaTeX-add-pagestyles "fancy" "fancyplain")

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("fancyhead" "[{")
                                ("fancyfoot" "[{")
                                ("lhead" "{")
                                ("lfoot" "{")
                                ("chead" "{")
                                ("cfoot" "{")
                                ("rhead" "{")
                                ("rfoot" "{")
                                ("fancyheadoffset" "[{")
                                ("fancyfootoffset" "[{")
                                ("fancypagestyle" "{{")) 'function)
     (font-latex-add-keywords '(("headrulewidth" "")
                                ("footrulewidth" "")
                                ("plainheadrulewidth" "")
                                ("plainfootrulewidth" "")) 'variable)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults)))
 LaTeX-dialect)

;; Because there can be many positions, `TeX-completing-read-multiple' is used
;; instead of just `completing-read', and a `collection' argument is provided as
;; the list of positions differs between the macros
(defun TeX-arg-fancyhdr-position (optional &optional prompt collection)
  "Prompt for a fancyhdr position with completion.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  If non-nil, PROMPT is
used as the prompt.  If non-nil, COLLECTION is used as the
completion list for the position."
  (let* ((positions (if (not collection)
                        ;; Standard positions with no restrictions.  Lower-case
                        ;; versions, and reverse versions (e.g., OC) are left
                        ;; out for simplicity.
                        '("LO" "LE" "L" "CO" "CE" "C" "RE" "RO" "R")
                      collection))
        (arguments
         (mapconcat 'identity
                    (TeX-completing-read-multiple
                     (TeX-argument-prompt optional prompt "Position")
                     (mapcar 'list positions)) ",")))
    (TeX-argument-insert arguments optional)))

(defvar LaTeX-fancyhdr-package-options nil
  "Package options for fancyhdr.")

;;; fancyhdr.el ends here

