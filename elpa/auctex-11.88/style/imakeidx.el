;;; imakeidx.el --- AUCTeX style for `imakeidx.sty'.

;; Copyright (C) 2012-2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
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

;; This file adds support for `imakeidx.sty'.

;;; Code:

(defvar LaTeX-imakeidx-makeindex-options
  '(("name")
    ("title")
    ("program" ("makeindex" "xindy" "texindy" "truexindy"))
    ("options")
    ("noautomatic" ("true" "false"))
    ("intoc" ("true" "false"))
    ("columns")
    ("columnsep")
    ("columnseprule" ("true" "false")))
  "Key=value options for makeindex macro of the imakeidx package.")

(defvar LaTeX-imakeidx-indexsetup-options
  '(("level")
    ("toclevel")
    ("noclearpage" ("true" "false"))
    ("othercode"))
  "Key=value options for indexsetup macro of the imakeidx package.")
(make-variable-buffer-local 'LaTeX-imakeidx-indexsetup-options)

(TeX-add-style-hook
 "imakeidx"
 (lambda ()
   ;; `firstpagestyle' and `headers' options for `indexsetup' macro are
   ;; available only if `fancyhdr' is not loaded.  The following code works only
   ;; if `imakeidx' is loaded after `fancyhdr'.
   (unless (member "fancyhdr" TeX-active-styles)
     (setq LaTeX-imakeidx-indexsetup-options
	   (append LaTeX-imakeidx-indexsetup-options
		   `(("firstpagestyle" ,(LaTeX-pagestyle-list)))
		   '(("headers")))))

   (TeX-add-symbols
    '("makeindex" [ (TeX-arg-key-val LaTeX-imakeidx-makeindex-options) ])
    '("indexsetup" (TeX-arg-key-val LaTeX-imakeidx-indexsetup-options))
    '("splitindexoptions" "Command line option")
    '("index" [ "Index name" ] TeX-arg-index)
    '("indexprologue" [ "Spacing" ] "Text")
    '("printindex" [ "Index name" ])
    '("seealso" 2)
    '("see" 2)
    "seename"
    "alsoname"
    "indexname")

   (TeX-run-style-hooks
    "multicol"
    "xpatch"
    "pdftexcmds"
    "ifluatex"
    "ifxetex"
    "xkeyval")

   ;; Completion for the |see macro and RefTeX support taken from
   ;; `makeidx.el'
   (setq TeX-complete-list
	 (append
	  '(("|see{\\([^{}\n\r]*\\)" 1 LaTeX-index-entry-list))
	  TeX-complete-list))
   (and (fboundp 'reftex-add-index-macros)
	(reftex-add-index-macros '(default))))
 LaTeX-dialect)

(defvar LaTeX-imakeidx-package-options
  '("makeindex" "xindy" "texindy" "truexindy" "noautomatic" "nonewpage" "quiet"
    "original" "splitindex")
  "Package options for the imakeidx package.")

;; imakeidx.el ends here
