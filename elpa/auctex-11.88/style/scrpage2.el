;;; scrpage2.el --- AUCTeX style for scrpage2.sty.

;; Author:   Ralf Angeli <angeli@iwi.uni-sb.de>
;; Created:  2003-11-01
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

;; This file adds support for `scrpage2.sty'.

;;; Code:

(TeX-add-style-hook
 "scrpage2"
 (lambda ()

   ;; New symbols
   (TeX-add-symbols
    '("lehead" [ "scrplain-left-even" ] "scrheadings-left-even")
    '("cehead" [ "scrplain-center-even" ] "scrheadings-center-even")
    '("rehead" [ "scrplain-right-even" ] "scrheadings-right-even")
    '("lefoot" [ "scrplain-left-even" ] "scrheadings-left-even")
    '("cefoot" [ "scrplain-center-even" ] "scrheadings-center-even")
    '("refoot" [ "scrplain-right-even" ] "scrheadings-right-even")
    '("lohead" [ "scrplain-left-odd" ] "scrheadings-left-odd")
    '("cohead" [ "scrplain-center-odd" ] "scrheadings-center-odd")
    '("rohead" [ "scrplain-right-odd" ] "scrheadings-right-odd")
    '("lofoot" [ "scrplain-left-odd" ] "scrheadings-left-odd")
    '("cofoot" [ "scrplain-center-odd" ] "scrheadings-center-odd")
    '("rofoot" [ "scrplain-right-odd" ] "scrheadings-right-odd")
    '("ihead" [ "scrplain-inside" ] "scrheadings-inside")
    '("chead" [ "scrplain-center" ] "scrheadings-center")
    '("ohead" [ "scrplain-outside" ] "scrheadings-outside")
    '("ifoot" [ "scrplain-inside" ] "scrheadings-inside")
    '("cfoot" [ "scrplain-center" ] "scrheadings-center")
    '("ofoot" [ "scrplain-outside" ] "scrheadings-outside")
    '("clearscrheadfoot")
    '("clearscrheadings")
    '("clearscrplain")
    '("automark" [ "Right page" ] "Left page")
    '("headmark")
    '("manualmark")
    '("pagemark")
    '("leftmark")
    '("rightmark")
    '("setfootwidth" [ "Offset" ] "Width")
    '("setheadwidth" [ "Offset" ] "Width")
    '("setfootbotline" [ "Length" ] "Thickness")
    '("setfootsepline" [ "Length" ] "Thickness")
    '("setheadtopline" [ "Length" ] "Thickness")
    '("setheadsepline" [ "Length" ] "Thickness")
    '("deftripstyle" "Name" [ "Thickness of outer line" ]
      [ "Thickness of inner line" ] "Inner box of page head"
      "Center box of page head" "Outer box of page head"
      "Inner box of page foot" "Center box of page foot"
      "Outer box of page foot")
    '("defpagestyle" "Name" "Head definition" "Foot definition")
    '("newpagestyle" "Name" "Head definition" "Foot definition")
    '("renewpagestyle" "Name" "Head definition" "Foot definition")
    '("providepagestyle" "Name" "Head definition" "Foot definition"))

    ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lehead" "[{")
				("cehead" "[{")
				("rehead" "[{")
				("lefoot" "[{")
				("cefoot" "[{")
				("refoot" "[{")
				("lohead" "[{")
				("cohead" "[{")
				("rohead" "[{")
				("lofoot" "[{")
				("cofoot" "[{")
				("rofoot" "[{")
				("ihead" "[{")
				("chead" "[{")
				("ohead" "[{")
				("ifoot" "[{")
				("cfoot" "[{")
				("ofoot" "[{")
				("automark" "[{")
				("setfootwidth" "[{")
				("setheadwidth" "[{")
				("setfootbotline" "[{")
				("setfootsepline" "[{")
				("setheadtopline" "[{")
				("setheadsepline" "[{"))
			      'variable)
     (font-latex-add-keywords '(("deftripstyle" "{[[{{{{{{")
				("defpagestyle" "{{{")
				("newpagestyle" "{{{")
				("renewpagestyle" "{{{")
				("providepagestyle" "{{{"))
			      'function)))
 LaTeX-dialect)

(defvar LaTeX-scrpage2-package-options '("headinclude" "headexclude"
					 "footinclude" "footexclude"
					 "mpinclude" "mpexclude"
					 "headtopline" "headsepline"
					 "footsepline" "footbotline"
					 "plainheadtopline" "plainheadsepline"
					 "plainfootsepline" "plainfootbotline"
					 "ilines" "clines" "olines"
					 "automark" "manualmark"
					 "autooneside" "markuppercase"
					 "markusedcase" "nouppercase"
					 "komastyle" "standardstyle")
  "Package options for the scrpage2 package.")

;;; scrpage2.el ends here
