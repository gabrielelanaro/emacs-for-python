;;; hyperref.el --- AUCTeX style for the hyperref class.

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2008-06-21
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

;; This file adds support for the hyperref package.

;;; Code:

(defvar LaTeX-hyperref-package-options
  '("a4paper" "a5paper" "anchorcolor" "b5paper" "backref" "baseurl"
    "bookmarks" "bookmarksnumbered" "bookmarksopen"
    "bookmarksopenlevel \maxdimen" "bookmarkstype" "breaklinks"
    "CJKbookmarks" "citebordercolor" "citecolor" "colorlinks" "debug"
    "draft" "dvipdf" "dvipdfm" "dvipdfmx" "dvips" "dvipsone"
    "dviwindo" "encap" "executivepaper" "extension" "filebordercolor"
    "filecolor" "final" "frenchlinks" "hyperfigures" "hyperfootnotes"
    "hyperindex" "hypertex" "hypertexnames" "implicit" "latex2html"
    "legalpaper" "letterpaper" "linkbordercolor" "linkcolor"
    "linktocpage" "menubordercolor" "menucolor" "nativepdf"
    "naturalnames" "nesting" "pageanchor" "pagebackref"
    "pagebordercolor" "pagecolor" "pdfauthor" "pdfborder"
    "pdfcenterwindow" "pdfcreator" "pdfdirection" "pdfdisplaydoctitle"
    "pdfduplex" "pdffitwindow" "pdfhighlight" "pdfkeywords" "pdflang"
    "pdfmark" "pdfmenubar" "pdfnewwindow" "pdfnonfullscreenpagemode"
    "pdfnumcopies" "pdfpagelayout" "pdfpagemode" "pdfpagelabels"
    "pdfpagescrop" "pdfpagetransition" "pdfpicktrackbypdfsize"
    "pdfprintarea" "pdfprintclip" "pdfprintpagerange"
    "pdfprintscaling" "pdfproducer" "pdfstartpage" "pdfstartview"
    "pdfsubject" "pdftex" "pdftitle" "pdftoolbar" "pdfview"
    "pdfviewarea" "pdfviewclip" "pdfwindowui" "plainpages" "ps2pdf"
    "raiselinks" "runbordercolor" "setpagesize" "tex4ht" "textures"
    "unicode" "urlbordercolor" "urlcolor" "verbose" "vtex" "xetex")
  "Package options for the hyperref package.")

(TeX-add-style-hook
 "hyperref"
 (lambda ()
   ;; hyperref.sty loads url.sty
   (TeX-run-style-hooks "url")
   (TeX-add-symbols
    '("href" "URL" "Text")
    '("nolinkurl" t)
    '("hyperbaseurl" t)
    '("hyperimage" "Image URL" "Text")
    '("hyperdef" "Category" "Name" "Text")
    '("hyperref" "URL" "Category" "Name" "Text")
    '("hyperlink" "Name" "Text")
    '("hypertarget" "Name" "Text")
    '("phantomsection" 0)
    '("autoref" TeX-arg-ref)
    '("ref*" TeX-arg-ref)
    '("pageref*" TeX-arg-ref)
    '("pdfstringdef" "Macro name" "TeX string")
    '("texorpdfstring" "TeX string" "PDF string")
    '("hypercalcbp" t)
    '("Acrobatmenu" "Menu option" "Text")
    '("TextField" ["Parameters"] "Label")
    '("CheckBox" ["Parameters"] "Label")
    '("ChoiceMenu" ["Parameters"] "Label" "Choices")
    '("PushButton" ["Parameters"] "Label")
    '("Submit" ["Parameters"] "Label")
    '("Reset" ["Parameters"] "Label")
    '("LayoutTextField" "Label" "Field")
    '("LayoutChoiceField" "Label" "Field")
    '("LayoutCheckField" "Label" "Field")
    '("MakeRadioField" "Width" "Height")
    '("MakeCheckField" "Width" "Height")
    '("MakeTextField" "Width" "Height")
    '("MakeChoiceField" "Width" "Height")
    '("MakeButtonField" "Text"))

   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")

   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-set-syntactic-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("href" "{{")
				("nolinkurl" "{")
				("hyperbaseurl" "{")
				("hyperimage" "{{")
				("hyperdef" "{{{")
				("hyperref" "{{{{")
				("hyperlink" "{{")
				("hypertarget" "{{")
				("autoref" "{")
				("ref" "*{")
				("pageref" "*{"))
			      'reference)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords))

   ;; RefTeX
   (when (fboundp 'reftex-ref-style-activate)
     (reftex-ref-style-activate "Hyperref"))))

;;; hyperref.el ends here
