;; captcont.el --- AUCTeX style file for captcont.sty

;; Copyright (C) 2003, 2005 Free Software Foundation, Inc.

;; Author: Reiner Steib <Reiner.Steib@gmx.de>
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

;; AUCTeX style file for captcont.sty

;;; Code:

(TeX-add-style-hook
 "captcont"
 (lambda ()
   (TeX-add-symbols
    '("captcont"  [ "list entry" ] "Caption")
    '("captcont*" [ "list entry" ] "Caption"))
   ;; Fontification
   (when (featurep 'font-latex)
     (font-latex-add-keywords '(("captcont" "*[{")) 'textual))))

(defvar LaTeX-captcont-package-options '("figbotcap" "figtopcap" "tabbotcap"
					 "tabtopcap")
  "Package options for the captcont package.")

;;; captcont.el ends here
