;;; lettrine.el --- AUCTeX style for `lettrine.sty'

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
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

;; This file adds support for `lettrine.sty'.

;;; Code:

(defvar LaTeX-lettrine-key-val-options
  '(("lines")
    ("lhang")
    ("loversize")
    ("lraise")
    ("findent")
    ("nindent")
    ("slope")
    ("ante")
    ("image" ("true")))
  "Key=value options for \\lettrine")

(TeX-add-style-hook
 "lettrine"
 (lambda ()
   (TeX-add-symbols
    '("lettrine" [ TeX-arg-key-val LaTeX-lettrine-key-val-options ]
      "Letter" "Text")
    '("LettrineImageFalse" 0)
    ;; all of the below can be configured with either \setlength or
    ;; \renewcommand
    '("LettrineFont" 0)
    '("LettrineFontHook" 0)
    '("LettrineTextFont" 0)
    '("LettrineWidth" 0)
    '("DefaultLhang" 0)
    '("DefaultLoversize" 0)
    '("DefaultLraise" 0)
    '("DefaultFindent" 0)
    '("DefaultNindent" 0)
    '("DefaultSlope" 0)
    ;; above settings can also be input a file, and pointed to with
    ;; \renewcommand
    '("DefaultOptionsFile" 0))

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lettrine" "[{{")) 'textual))))

(defvar LaTeX-lettrine-package-options nil
  "Package options for the lettrine package.")

;;; lettrine.el ends here
