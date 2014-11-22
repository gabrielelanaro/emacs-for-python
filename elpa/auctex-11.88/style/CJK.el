;;; CJK.el --- AUCTeX style for the CJK package.

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2009-01-04
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

;; This file adds support for the CJK package, version 4.8.0
;; (22-May-2008).

;;; Code:

(defvar LaTeX-CJK-package-options
  '("lowercase" "global" "local" "active" "encapsulated")
  "Package options for the CJK package.")

(defvar LaTeX-CJK-enc-list
  '("Bg5" "Bg5+" "HK" "GB" "GBt" "GBK" "JIS" "JIS2" "SJIS" "KS" "UTF8" "CNS1"
    "CNS2" "CNS3" "CNS4" "CNS5" "CNS6" "CNS7" "CEFX" "CEFY")
  "List of encodings supported by the CJK package.")

(defun LaTeX-env-CJK (env)
  "Prompt for the arguments of ENV and insert it.
The function can be used for CJK and CJK* environments."
  (LaTeX-insert-environment
   env
   (concat
    (let ((font-enc (read-string "(Optional) Font encoding: ")))
      (unless (zerop (length font-enc)) (format "[%s]" font-enc)))
    (format "{%s}" (completing-read "Encoding: "
				    (mapcar 'list LaTeX-CJK-enc-list)))
    (format "{%s}" (read-string "Font family: ")))))

(TeX-add-style-hook
 "CJK"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("CJKencfamily" ["Font encoding"] "Encoding" "Font family")
    '("CJKchar" ["Encoding"] "First byte" "Second byte")
    '("CJKcaption" 1)
    '("CJKfamily" 1)
    '("CJKfontenc" "Encoding" "Font encoding")
    '("CJKenc" 1)
    '("Unicode" "First byte" "Second byte")
    '("CJKsymbols" 2)
    '("CJKsymbol" 1)
    "CJKbold"
    "CJKnormal"
    "CJKboldshift"
    "CJKCJKchar"
    "CJKhangulchar"
    "CJKlatinchar"
    "CJKhwkatakana"
    "CJKnohwkatakana"
    "CJKglue"
    "CJKtolerance"
    "CJKtilde"
    "nbs"
    "standardtilde"
    "CJKspace"
    "CJKnospace"
    "CJKindent"
    '("CJKaddEncHook" 2)
    "CJKkern"
    "CJKverbatim")
   ;; New environments
   (LaTeX-add-environments
    '("CJK" LaTeX-env-CJK)
    '("CJK*" LaTeX-env-CJK)))
 LaTeX-dialect)

;;; CJK.el ends here
