;;; memoir.el --- AUCTeX style for `memoir.cls'

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2012-12-28
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

;; This file adds support for `memoir.cls'.  Memoir is a very extensive
;; document class that lets you configure things very easily; `memoir'
;; loads (emulates) a lot of classes.

;;; Code:

(TeX-add-style-hook
 "memoir"
 (lambda ()
   (TeX-add-symbols
    ;; 6.4 Book and part headings
    "beforebookskip" "afterbookskip"
    "beforepartskip" "afterpartskip"

    "printbookname" "booknamefont"
    "booknamenum"
    "printbooknum" "booknumfont"
    "printpartname" "partnamefont"
    "partnamenum"
    "printpartnum" "partnumfont"

    '("printbooktitle" "Title")
    "booktitlefont"
    '("printparttitle" "Title")
    "parttitlefont"

    '("bookpagemark" "Title")
    '("partmark" "Title")

    "bookpageend" "bookblankpage" "nobookblankpage"
    "partpageend" "partblankpage" "nopartblankpage"

    '("newleadpage"    [ TeX-arg-pagestyle ] 1 "Title")
    '("newleadpage*"   [ TeX-arg-pagestyle ] 1 "Title")
    '("renewleadpage*" [ TeX-arg-pagestyle ] 1 "Title")
    '("renewleadpage*" [ TeX-arg-pagestyle ] 1 "Title")

    "leadpagetoclevel")

   ;; Emulated packages.  The `memoir' class contains a list of files
   ;; emulated at the end of the class-file
   (TeX-run-style-hooks
    "abstract" "appendix" "array" "booktabs" "ccaption"
    "changepage" "chngcntr" "chngpage" "crop" "dcolumn"
    "delarray" "enumerate" "epigraph" "ifmtarg" "ifetex"
    "ifluatex" "ifpdf" "ifxetex" "index" "makeidx" "moreverb"
    "mparhack" "needspace" "newfile" "nextpage" "pagenote"
    "parskip" "patchcmd" "setspace" "shortvrb" "showidx"
    "tabularx" "titleref" "titling" "tocbibind" "tocloft"
    "verbatim" "verse")

   (LaTeX-largest-level-set "chapter"))
 LaTeX-dialect)

;;; memoir.el ends here
