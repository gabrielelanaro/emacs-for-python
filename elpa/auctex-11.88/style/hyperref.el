;;; hyperref.el --- AUCTeX style for `hyperref.sty' v6.83m

;; Copyright (C) 2008, 2013 Free Software Foundation, Inc.

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

(defvar LaTeX-hyperref-package-options-list
  '(;; See http://www.tug.org/applications/hyperref/manual.html#x1-40003
    ;; General options
    ("draft" ("true" "false"))
    ("final" ("true" "false"))
    ("debug" ("true" "false"))
    ("verbose" ("true" "false"))
    ("implicit" ("true" "false"))
    ("setpagesize" ("true" "false"))
    ;; Options for destination names
    ("destlabel" ("true" "false"))
    ("hypertexnames" ("true" "false"))
    ("naturalnames" ("true" "false"))
    ("plainpages" ("true" "false"))
    ;; Configuration options
    ("raiselinks" ("true" "false"))
    ("breaklinks" ("true" "false"))
    ("pageanchor" ("true" "false"))
    ("nesting" ("true" "false"))
    ;; Backend drivers
    ("driverfallback")
    ("dvipdfm")
    ("dvipdfmx")
    ("dvips")
    ("dvipsone")
    ("dviwindo")
    ("hypertex")
    ("latex2html")
    ("nativepdf")
    ("pdfmark")
    ("pdftex")
    ("ps2pdf")
    ("tex4ht")
    ("textures")
    ("vtex")
    ("vtexpdfmark")
    ("xetex")
    ;; Extension options
    ("extension")
    ("hyperfigures" ("true" "false"))
    ("backref" ("section" "slide" "page" "none" "false"))
    ("pagebackref" ("true" "false"))
    ("hyperindex" ("true" "false"))
    ("hyperfootnotes" ("true" "false"))
    ("encap")
    ("linktocpage" ("true" "false"))
    ("breaklinks" ("true" "false"))
    ("colorlinks" ("true" "false"))
    ("linkcolor")
    ("anchorcolor")
    ("citecolor")
    ("filecolor")
    ("menucolor")
    ("runcolor")
    ("urlcolor")
    ("allcolors")
    ("frenchlinks" ("true" "false"))
    ("hidelinks")
    ;; PDF-specific display options
    ("bookmarks" ("true" "false"))
    ("bookmarksopen" ("true" "false"))
    ("bookmarksopenlevel")
    ("bookmarksnumbered" ("true" "false"))
    ("bookmarkstype")
    ("CJKbookmarks" ("true" "false"))
    ("pdfhighlight" ("/I" "/N" "/O" "/P"))
    ("citebordercolor")
    ("filebordercolor")
    ("linkbordercolor")
    ("menubordercolor")
    ("runbordercolor")
    ("urlbordercolor")
    ("allbordercolors")
    ("pdfborder")
    ;; PDF display and information options
    ("baseurl")
    ("pdfpagemode" ("UseOutlines" "UseThumbs" "FullScreen" "UseOC" "UseAttachments"))
    ("pdftitle")
    ("pdfauthor")
    ("pdfsubject")
    ("pdfcreator")
    ("pdfproducer")
    ("pdfkeywords")
    ("pdftrapped" ("True" "False" "Unknown"))
    ("pdfinfo")
    ("pdfview" ("XYZ" "Fit" "FitH" "FitV" "FitR" "FitB" "FitBH" "FitBV"))
    ("pdfstartpage")
    ("pdfstartview" ("XYZ" "Fit" "FitH" "FitV" "FitR" "FitB" "FitBH" "FitBV"))
    ("pdfremotestartview" ("XYZ" "Fit" "FitH" "FitV" "FitR" "FitB" "FitBH" "FitBV"))
    ("pdfpagescrop")
    ("pdfcenterwindow" ("true" "false"))
    ("pdfdirection" ("L2R" "R2L"))
    ("pdfdisplaydoctitle" ("true" "false"))
    ("pdfduplex" ("Simplex" "DuplexFlipShortEdge" "DuplexFlipLongEdge"))
    ("pdffitwindow" ("true" "false"))
    ("pdflang")
    ("pdfmenubar" ("true" "false"))
    ("pdfnewwindow" ("true" "false"))
    ("pdfnonfullscreenpagemode" ("UseNone" "UseOutlines" "UseThumbs" "FullScreen" "UseOC" "UseAttachments"))
    ("pdfnumcopies")
    ("pdfpagelayout" ("SinglePage" "OneColumn" "TwoColumnLeft" "TwoColumnRight" "TwoPageLeft" "TwoPageRight"))
    ("pdfpagelabels" ("true" "false"))
    ("pdfpagetransition" ("Blinds" "Box" "Dissolve" "Glitter" "Split" "Wipe"))
    ("pdfpicktraybypdfsize" ("true" "false"))
    ("pdfprintarea" ("MediaBox" "CropBox" "BleedBox" "TrimBox" "ArtBox"))
    ("pdfprintclip" ("MediaBox" "CropBox" "BleedBox" "TrimBox" "ArtBox"))
    ("pdfprintpagerange")
    ("pdfprintscaling" ("AppDefault" "None"))
    ("pdftoolbar" ("true" "false"))
    ("pdfviewarea" ("MediaBox" "CropBox" "BleedBox" "TrimBox" "ArtBox"))
    ("pdfviewclip" ("MediaBox" "CropBox" "BleedBox" "TrimBox" "ArtBox"))
    ("pdfwindowui" ("true" "false"))
    ("unicode" ("true" "false")))
  "Package options for the hyperref package.")

(defvar LaTeX-hyperref-href-options
  '(("pdfremotestartview" ("XYZ" "Fit" "FitH" "FitV" "FitR" "FitB" "FitBH" "FitBV"))
    ("pdfnewwindow" ("true" "false"))
    ("page")
    ("ismap" ("true" "false"))
    ("nextactionraw"))
  "Key=value options for href macro of the hyperref package.")

(TeX-add-style-hook
 "hyperref"
 (lambda ()
   ;; hyperref loads nameref and url (+ some other packages which do not have
   ;; style hooks)
   (TeX-run-style-hooks "url" "nameref")

   (TeX-add-symbols
    '("hypersetup" (TeX-arg-key-val LaTeX-hyperref-package-options-list))
    '("href" [ (TeX-arg-key-val LaTeX-hyperref-href-options) ] "URL" "Text")
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
    '("autopageref" TeX-arg-ref)
    '("autopageref*" TeX-arg-ref)
    '("pdfstringdef" "Macro name" "TeX string")
    '("pdfbookmark" [ "Level" ] "Text" "name")
    '("currentpdfbookmark" "Text" "Name")
    '("subpdfbookmark" "Text" "Name")
    '("belowpdfbookmark" "Text" "Name")
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
     (font-latex-add-keywords '(("href" "[{{")
				("nolinkurl" "{")
				("hyperbaseurl" "{")
				("hyperimage" "{{")
				("hyperdef" "{{{")
				("hyperref" "{{{{")
				("hyperlink" "{{")
				("hypertarget" "{{")
				("autoref" "{")
				("ref" "*{")
				("pageref" "*{")
				("autopageref" "*{"))
			      'reference)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords))

   ;; RefTeX
   (when (fboundp 'reftex-ref-style-activate)
     (reftex-ref-style-activate "Hyperref")))
 LaTeX-dialect)

(defun LaTeX-hyperref-package-options ()
  "Read the hyperref package options from the user."
  (TeX-read-key-val t LaTeX-hyperref-package-options-list))

;;; hyperref.el ends here
