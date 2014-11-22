;;; biblatex.el --- AUCTeX style for `biblatex.sty' version 2.8a.

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2012-11-14
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

;; This file adds support for `biblatex.sty' version 2.8a.

;;; Code:

(defvar LaTeX-biblatex-entrytype
  '(;; Regular Types
    "article" "book" "mvbook" "inbook" "bookinbook" "suppbook" "booklet"
    "collection" "mvcollection" "incollection" "suppcollection" "manual" "misc"
    "online" "patent" "periodical" "suppperiodical" "proceedings"
    "mvproceedings" "inproceedings" "reference" "mvreference" "inreference"
    "report" "set" "thesis" "unpublished" "xdata" "customa" "customb" "customc"
    "customd" "custome" "customf"
    ;; Type Aliases
    "conference" "electronic" "masterthesis" "phdthesis" "techreport" "www"
    ;; Unsupported Types
    "artwork" "audio" "bibnote" "commentary" "image" "jurisdiction"
    "legislation" "legal" "letter" "movie" "music" "performance" "review"
    "software" "standard" "video")
  "List of biblatex entry types.")

(defvar LaTeX-biblatex-executebibliographyoptions-options
  '(;; General
    ("sorting" ("nty" "nyt" "nyvt" "anyt" "anyvt" "ynt" "ydnt" "none" "debug"))
    ("sortcase" ("true" "false"))
    ("sortupper" ("true" "false"))
    ("sortlocale")
    ("sortlos" ("bib" "los"))
    ("related" ("true" "false"))
    ("sortcites" ("true" "false"))
    ("maxnames")
    ("minnames")
    ("maxbibnames")
    ("minbibnames")
    ("maxcitenames")
    ("mincitenames")
    ("maxitems")
    ("minitems")
    ("autocite" ("plain" "inline" "footnote" "superscript"))
    ("autopunct" ("true" "false"))
    ("language" (append LaTeX-biblatex-language-list
			'("autobib" "autocite" "auto")))
    ("clearlang" ("true" "false"))
    ("autolang" ("none" "hyphen" "other" "other*" "langname"))
    ("block" ("none" "space" "par" "nbpar" "ragged"))
    ("notetype" ("foot+end" "footonly" "endonly"))
    ("hyperref" ("true" "false" "auto"))
    ("backref" ("true" "false"))
    ("backrefstyle" ("none" "three" "two" "two+" "three+" "all+"))
    ("backrefsetstyle" ("setonly" "memonly" "setormem" "setandmem" "memandset"
			"setplusmem"))
    ("indexing" ("true" "false" "cite" "bib"))
    ("loadfiles" ("true" "false"))
    ("refsection" ("none" "part" "chapter" "section" "subsection"))
    ("refsegment" ("none" "part" "chapter" "section" "subsection"))
    ("citereset" ("none" "part" "chapter" "section" "subsection"))
    ("abbreviate" ("true" "false"))
    ("date" ("short" "long" "terse" "comp" "iso8601"))
    ("datelabel" ("year" "short" "long" "terse" "comp" "iso8601"))
    ("origdate" ("short" "long" "terse" "comp" "iso8601"))
    ("eventdate" ("short" "long" "terse" "comp" "iso8601"))
    ("urldate" ("short" "long" "terse" "comp" "iso8601"))
    ("alldates" ("short" "long" "terse" "comp" "iso8601"))
    ("datezeros" ("true" "false"))
    ("dateabbrev" ("true" "false"))
    ("defernumbers" ("true" "false"))
    ("punctfont" ("true" "false"))
    ("arxiv" ("abs" "ps" "pdf" "format"))
    ("texencoding" ("auto"))
    ("bibencoding" ("auto"))
    ("safeinputenc" ("true" "false"))
    ("bibwarn" ("true" "false"))
    ("mincrossrefs")
    ;; Style-specific
    ("isbn" ("true" "false"))
    ("url" ("true" "false"))
    ("doi" ("true" "false"))
    ("eprint" ("true" "false"))
    ;; Internal
    ("pagetracker" ("true" "false" "page" "spread"))
    ("citecounter" ("true" "false" "context"))
    ("citetracker" ("true" "false" "context" "strict" "constrict"))
    ("ibidtracker" ("true" "false" "context" "strict" "constrict"))
    ("opcittracker" ("true" "false" "context" "strict" "constrict"))
    ("loccittracker" ("true" "false" "context" "strict" "constrict"))
    ("idemtracker" ("true" "false" "context" "strict" "constrict"))
    ("parentracker" ("true" "false"))
    ("maxparens")
    ("firstinits" ("true" "false"))
    ("sortfirstinits" ("true" "false"))
    ("terseinits" ("true" "false"))
    ("labelalpha" ("true" "false"))
    ("maxalphanames")
    ("minalphanames")
    ("labelnumber" ("true" "false"))
    ("labeltitle" ("true" "false"))
    ("labeltitleyear" ("true" "false"))
    ("labeldate" ("true" "false"))
    ("singletitle" ("true" "false"))
    ("uniquename" ("true" "false" "init" "full" "allinit" "allfull" "mininit"
		   "minfull"))
    ("uniquelist" ("true" "false" "minyear")))
  "Key=value options for ExecuteBibliographyOptions macro of the biblatex package.")

;; See table 2 of Biblatex reference manual.
(defvar LaTeX-biblatex-language-list
  '("catalan" "croatian" "czech" "danish" "dutch" "american" "british"
    "canadian" "australian" "newzealand" "finnish" "french" "german" "austrian"
    "ngernam" "naustrian" "greek" "italian" "norwegian" "polish" "brazilian"
    "portuguese" "russian" "spanish" "swedish")
  "List of languages supported by biblatex packages.")

(defvar LaTeX-biblatex-addbibresource-options
  '(("label")
    ("location" ("local" "remote"))
    ("type" ("file"))
    ("datatype" ("bibtex" "ris" "zoterordfxml" "endnotexml")))
  "Key=value options for addbibresource macro of the biblatex package.")

(defun LaTeX-arg-addbibresource (optional &optional prompt)
  "Prompt for a BibLaTeX database file.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (let (files inputs database)
    (if LaTeX-using-Biber
	(setq files 'TeX-Biber-global-files
	      inputs 'biberinputs)
      (setq files 'BibTeX-global-files
	    inputs 'bibinputs))
    (setq files 'TeX-Biber-global-files
	  inputs 'biberinputs)
    (message "Searching for BibLaTeX files...")
    (or (symbol-value files)
	(set files (mapcar 'list (TeX-search-files-by-type
				  'biberinputs 'global t nil))))
    (setq database (completing-read
		    (TeX-argument-prompt optional prompt "BibLaTeX files")
		    (append (mapcar 'list (TeX-search-files-by-type
					   inputs 'local t nil))
			    (symbol-value files))))
    (LaTeX-add-bibliographies database)
    ;; Run style file associated to the bibliography database file in order to
    ;; immediately fill `LaTeX-bibitem-list'.  We need to strip the extension
    ;; because AUCTeX style files don't use it.
    (TeX-run-style-hooks (file-name-sans-extension database))
    (TeX-argument-insert database optional)))

;; Support for multicite commands, see § 3.7.3 of Biblatex reference manual.
(defun LaTeX-arg-biblatex-cites (optional &optional prompt)
  "Prompt for citations with completion until input is empty.
Prompt also for optional prenotes and postnotes.  If OPTIONAL is
non-nil, insert the citation key as an optional argument,
otherwise as a mandatory one.  Use PROMPT as the prompt string
for citation keys."
  ;; Prompt for global prenote and postnote.
  (and TeX-arg-cite-note-p (not current-prefix-arg)
       (let ((TeX-arg-opening-brace "(")
	     (TeX-arg-closing-brace ")")
	     (prenote (TeX-read-string
		       (TeX-argument-prompt t nil "Global prenote"))))
	 (TeX-argument-insert prenote t)
	 ;; If the prenote is empty the postnote is optional, otherwise it's
	 ;; mandatory.
	 (TeX-argument-insert
	  (TeX-read-string (TeX-argument-prompt t nil "Global postnote"))
	  (equal prenote ""))))
  (let ((items t) (noinsert nil))
    (while items
      ;; Prompt for prenote and postnote of the current keys.
      (and TeX-arg-cite-note-p (not current-prefix-arg)
	   (let ((TeX-arg-opening-brace "[")
		 (TeX-arg-closing-brace "]")
		 (prenote (TeX-read-string
			   (TeX-argument-prompt t nil "Prenote"))))
	     (TeX-argument-insert prenote t)
	     ;; If the prenote is empty the postnote is optional, otherwise it's
	     ;; mandatory.
	     (TeX-argument-insert
	      (TeX-read-string (TeX-argument-prompt t nil "Postnote"))
	      (equal prenote ""))))
      (setq items (TeX-completing-read-multiple
		   (TeX-argument-prompt optional prompt "Key")
		   (LaTeX-bibitem-list)))
      (apply 'LaTeX-add-bibitems items)
      ;; If input is empty, insert an empty group only the first time, when
      ;; `noinsert' flag is nil.
      (unless (and (not items) noinsert)
	(TeX-argument-insert (mapconcat 'identity items ",") optional))
      (setq noinsert t))))

(TeX-add-style-hook
 "biblatex"
 (lambda ()
   ;; Biblatex uses as default backend biber, run it unless biblatex `backend'
   ;; option value is one of `bibtex', `bibtex8', `bibtexu'.  Autodetection of
   ;; the backend can be overridden by setting `LaTeX-biblatex-use-Biber' as a
   ;; local variable.
   (setq LaTeX-using-Biber
	 (if (local-variable-p 'LaTeX-biblatex-use-Biber (current-buffer))
	     LaTeX-biblatex-use-Biber
	   (not (or (LaTeX-provided-package-options-member
		     "biblatex" "backend=bibtex")
		    (LaTeX-provided-package-options-member
		     "biblatex" "backend=bibtex8")
		    (LaTeX-provided-package-options-member
		     "biblatex" "backend=bibtexu")))))

   (TeX-run-style-hooks
    "etoolbox"
    "keyval"
    "kvoptions"
    "logreq"
    "ifthen"
    "url")
   (TeX-add-symbols
    ;;; Global Customization
    ;; Setting Package Options
    '("ExecuteBibliographyOptions"
      [TeX-arg-eval  mapconcat 'identity
		     (TeX-completing-read-multiple
		      "Entry type: " LaTeX-biblatex-entrytype) ","]
      (TeX-arg-key-val LaTeX-biblatex-executebibliographyoptions-options))
    ;;; Bibliography Commands
    ;; Resources
    '("addbibresource" [TeX-arg-key-val LaTeX-biblatex-addbibresource-options]
      LaTeX-arg-addbibresource)
    '("addglobalbib" [TeX-arg-key-val LaTeX-biblatex-addbibresource-options]
      LaTeX-arg-addbibresource)
    '("addsectionbib" [TeX-arg-key-val LaTeX-biblatex-addbibresource-options]
      LaTeX-arg-addbibresource)
    ;; The Bibliography
    '("printbibliography"
      [TeX-arg-key-val (("env") ("heading") ("title") ("prenote") ("postnote")
			("section") ("segment") ("sorting") ("type") ("nottype")
			("subtype") ("notsubtype") ("keyword") ("notkeyword")
			("categoy") ("notcategory") ("filter") ("check")
			("prefixnumbers") ("resetnumbers" ("true" "false"))
			("omitnumbers" ("true" "false")))])
    '("bibbysection"
      [TeX-arg-key-val (("env") ("heading") ("prenote") ("postnote"))])
    '("bibbysegment"
      [TeX-arg-key-val (("env") ("heading") ("prenote") ("postnote"))])
    '("bibbycategory"
      [TeX-arg-key-val (("env") ("prenote") ("postnote") ("section"))])
    '("printbibheading"
      [TeX-arg-key-val (("heading") ("title"))])
    ;; The List of Shorthands
    '("printshorthands"
      [TeX-arg-key-val (("env") ("heading") ("title") ("prenote") ("postnote")
			("section") ("segment") ("sorting") ("type") ("nottype")
			("subtype") ("notsubtype") ("keyword") ("notkeyword")
			("categoy") ("notcategory") ("filter") ("check"))])
    ;; Bibliography Sections
    '("newrefsection" ["Resources"])
    "endrefsection"
    ;; Bibliography Segments
    "newrefsegment"
    "endrefsegment"
    ;; Bibliography Categories
    '("DeclareBibliographyCategory" "Category")
    '("addtocategory" "Category" TeX-arg-cite)
    ;; Bibliography Headings and Environments
    '("defbibenvironment" "Name" 3)
    '("defbibheading" "Name" ["Title"] t)
    ;; Bibliography Notes
    '("defbibnote" "Name" "Text")
    ;; Bibliography Filters and Checks
    '("defbibfilter" "Name" t)
    '("defbibcheck" "Name" t)
    ;; Dynamic Entry Sets
    '("defbibentryset" "Set"
      (TeX-arg-eval mapconcat 'identity (TeX-completing-read-multiple
					 "Keys: " (LaTeX-bibitem-list)) ","))
    ;;; Citation Commands
    '("cite" (TeX-arg-conditional TeX-arg-cite-note-p
				  (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Cite" (TeX-arg-conditional TeX-arg-cite-note-p
				  (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("parencite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Parencite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("footcite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("footcitetext" (TeX-arg-conditional TeX-arg-cite-note-p
					  (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    ;; Style-specific Commands
    '("textcite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Textcite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("smartcite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Smartcite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("cite*" (TeX-arg-conditional TeX-arg-cite-note-p
				   (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("parencite*" (TeX-arg-conditional TeX-arg-cite-note-p
					(["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("supercite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    ;; Qualified Citation Lists
    '("cites" LaTeX-arg-biblatex-cites)
    '("Cites" LaTeX-arg-biblatex-cites)
    '("parencites" LaTeX-arg-biblatex-cites)
    '("Parencites" LaTeX-arg-biblatex-cites)
    '("footcites" LaTeX-arg-biblatex-cites)
    '("footcitetexts" LaTeX-arg-biblatex-cites)
    '("smartcites" LaTeX-arg-biblatex-cites)
    '("Smartcites" LaTeX-arg-biblatex-cites)
    '("textcites" LaTeX-arg-biblatex-cites)
    '("Textcites" LaTeX-arg-biblatex-cites)
    '("supercites" LaTeX-arg-biblatex-cites)
    ;; Style-independent Commands
    '("autocite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Autocite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("autocite*" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Autocite*" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("autocites" LaTeX-arg-biblatex-cites)
    '("Autocites" LaTeX-arg-biblatex-cites)
    ;; Text Commands
    '("citeauthor" (TeX-arg-conditional TeX-arg-cite-note-p
					(["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Citeauthor" (TeX-arg-conditional TeX-arg-cite-note-p
					(["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citetitle" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citetitle*" (TeX-arg-conditional TeX-arg-cite-note-p
					(["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citeyear" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citeyear*" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citedate" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citedate*" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("citeurl" (TeX-arg-conditional TeX-arg-cite-note-p
				     (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("parentext" "Text")
    '("brackettext" "Text")
    ;; Special Commands
    '("fullcite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("footfullcite" (TeX-arg-conditional TeX-arg-cite-note-p
					  (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("volcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("Volcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("Pvolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("Pvolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("fvolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("ftolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("svolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("Svolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("tvolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("Tvolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("avolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("Avolcite"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"]) ()) "Volume"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Page"]) ()) TeX-arg-cite)
    '("notecite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Notecite" (TeX-arg-conditional TeX-arg-cite-note-p
				      (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("pnotecite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("Pnotecite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    '("fnotecite" (TeX-arg-conditional TeX-arg-cite-note-p
				       (["Prenote"] ["Postnote"]) ()) TeX-arg-cite)
    ;; Low-level Commands
    '("citename"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"] ["Postnote"]) ())
      TeX-arg-cite (TeX-arg-conditional TeX-arg-cite-note-p (["Format"]) ())
      "Name list")
    '("citelist"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"] ["Postnote"]) ())
      TeX-arg-cite (TeX-arg-conditional TeX-arg-cite-note-p (["Format"]) ())
      "Literal list")
    '("citefield"
      (TeX-arg-conditional TeX-arg-cite-note-p (["Prenote"] ["Postnote"]) ())
      TeX-arg-cite (TeX-arg-conditional TeX-arg-cite-note-p (["Format"]) ())
      "Field")
    ;; Miscellaneous Commands
    "citereset"
    "citereset*"
    "mancite"
    "pno"
    "ppno"
    "nopp"
    "psq"
    "psqq"
    '("RN" "Integer")
    '("Rn" "Integer")
    ;; Localization Commands
    '("DefineBibliographyStrings"
      (TeX-arg-eval completing-read "Language: " LaTeX-biblatex-language-list) t)
    '("DefineBibliographyExtras"
      (TeX-arg-eval completing-read "Language: " LaTeX-biblatex-language-list) t)
    '("UndefineBibliographyExtras"
      (TeX-arg-eval completing-read "Language: " LaTeX-biblatex-language-list) t)
    '("DefineHyphenationExceptions"
      (TeX-arg-eval completing-read "Language: " LaTeX-biblatex-language-list) t)
    "NewBibliographyString")
   (LaTeX-add-environments
    ;;; Bibliography commands
    ;; Bibliography Sections
    '("refsection" ["Resources"])
    ;; Bibliography Segments
    "refsegment")

   ;; Declaring expert macros and environments.  Criterion: all macros and
   ;; environments to fine tune the bibliography, probably they will be used
   ;; only by expert users.
   (TeX-declare-expert-macros
    "biblatex"
    "ExecuteBibliographyOptions" "printshorthands" "newrefsection"
    "endrefsection" "newrefsegment" "endrefsegment"
    "DeclareBibliographyCategory" "addtocategory" "defbibenvironment"
    "defbibheading" "defbibnote" "defbibfilter" "defbibcheck" "defbibentryset"
    "citereset" "citereset*" "mancite" "pno" "ppno" "nopp" "psq" "psqq" "RN"
    "Rn" "DefineBibliographyStrings" "DefineBibliographyExtras"
    "UndefineBibliographyExtras" "DefineHyphenationExceptions"
    "NewBibliographyString")
   (LaTeX-declare-expert-environments
    "biblatex"
    "refsection" "refsegment"))
 LaTeX-dialect)

(defvar LaTeX-biblatex-package-options-list
  (append
   ;;; Preamble Options
   LaTeX-biblatex-executebibliographyoptions-options
   '(;;; Load-time Options
    ("backend" ("biber" "bibtex" "bibtexu" "bibtex8"))
    ("style" BibLaTeX-global-style-files)
    ("bibstyle" BibLaTeX-global-style-files)
    ("citestyle" BibLaTeX-global-style-files)
    ("natbib" ("true" "false"))
    ("mcite" ("true" "false"))
    ;;; Entry Options
    ;; Preamble/Type/Entry Options
    ("useauthor" ("true" "false"))
    ("useeditor" ("true" "false"))
    ("usetranslator" ("true" "false"))
    ("useprefix" ("true" "false"))
    ("indexing" ("true" "false" "cite" "bib"))
    ;; Type/Entry Options are not available globally.
    ;; Legacy Options (deprecated)
    ("openbib")))
  "Package options for the biblatex package.")

(defun LaTeX-biblatex-package-options nil
  "Prompt for package options for the biblatex package."
  (unless BibLaTeX-global-style-files
    (if (if (eq TeX-arg-input-file-search 'ask)
	    (not (y-or-n-p "Find BibLaTeX style yourself? "))
	  TeX-arg-input-file-search)
	;; ...then, search for BibLaTeX styles.
	(progn
	  (message "Searching for BibLaTeX styles...")
	  (setq BibLaTeX-global-style-files
		(mapcar 'identity (TeX-search-files-by-type 'bbxinputs 'global t t))))
      ;; ...else, use for completion only standard BibLaTeX styles (see §3.3 of
      ;; Biblatex reference manual).
      (setq BibLaTeX-global-style-files
	    '("numeric" "numeric-comp" "numeric-verb" "alphabetic"
	      "alphabetic-verb" "authoryear" "authoryear-comp" "authoryear-ibid"
	      "authoryear-icomp" "authortitle" "authortitle-comp"
	      "authortitle-ibid" "authortitle-icomp" "authortitle-terse"
	      "authortitle-tcomp" "authortitle-ticomp" "verbose" "verbose-ibid"
	      "verbose-note" "verbose-inote" "verbose-trad1" "verbose-trad2"
	      "verbose-trad3" "reading" "draft" "debug"))))
  (TeX-read-key-val t LaTeX-biblatex-package-options-list))

;;; biblatex.el ends here
