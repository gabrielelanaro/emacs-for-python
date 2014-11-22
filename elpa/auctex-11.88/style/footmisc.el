;;; footmisc.el --- AUCTeX style for `footmisc.sty'

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Created: 2011-04-08
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

;; This file adds support for `footmisc.sty'.

;;; Code:

(TeX-add-style-hook
 "footmisc"
 (lambda ()
   (TeX-add-symbols
    '("DefineFNsymbols" "Name" [ "Style (text or math) " ] 1) 
    '("DefineFNsymbols*" "Name" [ "Style  (text or math)" ] 1)
    ;; These two commands define both text and math variants of the
    ;; footnote symbols
    '("DefineFNsymbolsTM" "Name" 1)
    '("DefineFNsymbolsTM*" "Name" 1)
    '("setfnsymbol" "Name")
    '("mpfootnoterule" TeX-arg-size) 
    "pagefootnoterule"
    "splitfootnoterule"
    ;; The following command references a label inside in a footnote
    '("footref" TeX-arg-ref)
    "hangfootparskip"
    "hangfootparindent"
    "footnotehint"
    '("footnotemargin" TeX-arg-size)
    "mpfootnoterule"
    "multiplefootnotemarker"
    "multfootsep")

   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("DefineFNsymbols" "{[{")
                                ("DefineFNsymbols*" "{[{")
                                ("DefineFNsymbolsTM" "{{")
                                ("DefineFNsymbolsTM*" "{{")
        			("setfnsymbol" "{")) 'function)
     (font-latex-add-keywords '(("footnoteref")) 'reference)))
 LaTeX-dialect)

(defvar LaTeX-footmisc-package-options '("perpage" "side" "ragged"
                                         "para" "symbol" "symbol*"
                                         "marginal" "flushmargin" "hang"
                                         "norule" "splitrule" "stable"
                                         "multiple")
  "Package options for the footmisc package.")

;;; footmisc.el ends here
