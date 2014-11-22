;;; subfigure.el --- AUCTeX style file for subfigure.sty

;; Copyright (C) 2003, 2005, 2013 Free Software Foundation, Inc.

;; Author: Reiner Steib  <Reiner.Steib@gmx.de>
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

;; AUCTeX style file for `subfigure.sty'

;;; Code:

(TeX-add-style-hook
 "subfigure"
 (lambda ()
   (TeX-add-symbols
    '("subfigure"  [ "List entry" ] [ "Sub caption" ] "Figure")
    '("subtable"   [ "List entry" ] [ "Sub caption" ] "Figure")
    '("Subref" TeX-arg-ref)
    '("subref" TeX-arg-ref)
    '("subcapsize" 0)
    '("subcapfont" 0)
    '("subcaplabelfont" 0))

   (LaTeX-add-lengths "subfigtopskip" "subfigcapskip" "subfigcaptopadj"
                      "subfigbottomskip" "subfigcapmargin" "subfiglabelskip")

   ;; Install completion for labels:
   (setq TeX-complete-list
	 (append
	  '(("\\\\[Ss]ubref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")))
	  TeX-complete-list)

   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("subfigure" "[[{")
				("subtable" "[[{"))
			      'textual)
     (font-latex-add-keywords '(("Subref" "{")
				("subref" "{"))
			      'reference)))
 LaTeX-dialect)

(defvar LaTeX-subfigure-package-options '("normal" "hang" "center"
					  "centerlast" "nooneline"
					  "raggedright" "isu" "anne"
					  "scriptsize" "footnotesize"
					  "small" "normalsize" "large"
					  "Large" "rm" "sf" "tt" "md"
					  "bf" "up" "it" "sl" "sc" "RM"
					  "SF" "TT" "MD" "BF" "IT" "SL"
					  "SC" "UP" "figbotcap"
					  "figtopcap" "tabbotcap"
					  "tabtopcap" "FIGBOTCAP"
					  "FIGTOPCAP" "TABBOTCAP"
					  "TABTOPCAP" "loose" "tight")
  "Package options for the subfigure package.")

;;; subfigure.el ends here
