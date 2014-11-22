;;; luacode.el --- AUCTeX style for `luacode.sty' version 1.2a.

;; Copyright (C) 2013 Free Software Foundation, Inc.

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

;; This file adds support for `luacode.sty' 1.2a.

;;; Code:

(TeX-add-style-hook
 "luacode"
 (lambda ()
   (TeX-add-symbols
    '("luadirect" 1)
    '("luaexec" 1)
    '("luastring" 1)
    '("luastringN" 1)
    '("luastringO" 1)
    '("LuaCodeDebugOn" 0)
    '("LuaCodeDebugOff" 0))
   (LaTeX-add-environments
    '("luacode")
    '("luacode*"))

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("luadirect" "{")
				("luaexec" "{")
				("luastring" "{")
				("luastringN" "{")
				("luastringO" "{")
				("LuaCodeDebugOn")
				("LuaCodeDebugOff"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-luacode-package-options nil
  "Package options for the luacode package.")

;;; luacode.el ends here
