;;; jurabib.el --- AUCTeX style for the `jurabib' package

;; Copyright (C) 2004, 2007 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-05
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

;; This file adds support for the `jurabib' package.

;; Currently only the citation-related commands are supported.  Feel
;; free to complete the support and send the result to the AUCTeX
;; mailing list.  But be aware that the code can only be included if
;; you assign the copyright to the FSF.

;;; Code:

(TeX-add-style-hook
 "jurabib"
 (lambda ()
   ;; Taken from natbib.el and adapted.
   (let ((citecmds
	  '(("cite" . 2) ("cite*" . 2)
	    ("citetitle" . 2) ("fullcite" . 2)
	    ("citet" . 1) ("citealt" . 1)
	    ("citep" . 2) ("citealp" . 2)
	    ("citeauthor" . 2) ("citeyear" . 2)
	    ("footcite" . 2) ("footcite*" . 2)
	    ("footcitetitle" . 2) ("footfullcite" . 2)
	    ("footcitet" . 1) ("footcitealt" . 1)
	    ("footcitep" . 2) ("footcitealp" . 2)
	    ("footciteauthor" . 2) ("footciteyear" . 2))))
     ;; Add these symbols
     (apply 
      'TeX-add-symbols
      (mapcar
       (lambda (cmd)
	 (cond 
	  ((= (cdr cmd) 0)
	   ;; No optional arguments
	   (list (car cmd) 'TeX-arg-cite))
	  ((= (cdr cmd) 1)
	   ;; Just one optional argument, the post note
	   (list
	    (car cmd)
	    '(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	    'TeX-arg-cite))
	  ((= (cdr cmd) 2)
	   ;; Pre and post notes
	   (list
	    (car cmd)
	    '(TeX-arg-conditional TeX-arg-cite-note-p (natbib-note-args) nil)
	    'TeX-arg-cite))))
       citecmds))
     ;; Special cases
     (TeX-add-symbols
      ;; FIXME: Completing read for field.
      '("citefield" ; \citefield[]{}{}
	(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	"Field" TeX-arg-cite)
      '("footcitefield" ; \footcitefield[]{}{}
	(TeX-arg-conditional TeX-arg-cite-note-p (["Post-note"]) nil)
	"Field" TeX-arg-cite))

     ;; Make an entry in TeX-complete-list
     (add-to-list
      'TeX-complete-list
      (list
       (concat "\\\\\\(" 
	       (mapconcat (lambda (x) (regexp-quote (car x)))
			  (append citecmds
				  '(("citefield") ("footcitefield"))) "\\|")
	       "\\)\\(\\[[^]\n\r\\%]*\\]\\)*{\\([^{}\n\r\\%,]*,\\)*"
	       "\\([^{}\n\r\\%,]*\\)")
       4 'LaTeX-bibitem-list "}"))

     ;; Add further symbols
     (TeX-add-symbols
      '("citefullfirstfortype" 1)
      '("citenotitlefortype" 1)
      '("citeswithoutentry" 1)
      '("citetitlefortype" 1)
      '("citeworkwithtitle" 1)
      '("nextcitefull" 1)
      '("nextcitenotitle" 1)
      '("nextcitereset" 1)
      '("nextciteshort" 1)
      '("jurabibsetup" 1))

     ;; Fontification
     (when (and (featurep 'font-latex)
		(eq TeX-install-font-lock 'font-latex-setup))
       (font-latex-add-keywords '(("cite" "*[[{")
				  ("citetitle" "[[{")
				  ("fullcite" "[[{")
				  ("citet" "[{")
				  ("citealt" "[{")
				  ("citep" "[[{")
				  ("citealp" "[[{")
				  ("citeauthor" "[[{")
				  ("citeyear" "[[{")
				  ("footcite" "[[{")
				  ("footcite*" "[[{")
				  ("footcitetitle" "[[{")
				  ("footfullcite" "[[{")
				  ("footcitet" "[{")
				  ("footcitealt" "[{")
				  ("footcitep" "[[{")
				  ("footcitealp" "[[{")
				  ("footciteauthor" "[[{")
				  ("footciteyear" "[[{")
				  ("citefield" "[{{")
				  ("footcitefield" "[{{"))
				'reference)
       (font-latex-add-keywords '(("citeswithoutentry" "{")
				  ("nextcitefull" "{")
				  ("nextcitenotitle" "{")
				  ("nextcitereset" "{")
				  ("nextciteshort" "{"))
				'function)
       (font-latex-add-keywords '(("citenotitlefortype" "{")
				  ("citetitlefortype" "{")
				  ("jurabibsetup" "{"))
				'variable))

     ;; Tell RefTeX (Thanks, Carsten)
     (when (and (fboundp 'reftex-set-cite-format)
		;; Is it `reftex-cite-format' customized?
		(not (get 'reftex-cite-format 'saved-value)))
       ;; Check if RefTeX supports jurabib.
       (if (assoc 'jurabib reftex-cite-format-builtin)
	   ;; Yes, use the provided default.
	   (reftex-set-cite-format 'jurabib)
	 ;; No, set it by hand.
	 (reftex-set-cite-format
	  '((?\C-m . "\\cite{%l}")
	    (?c    . "\\cite[?][]{%l}")
	    (?t    . "\\citet{%l}")
	    (?p    . "\\citep{%l}")
	    (?e    . "\\citep[e.g.][?]{%l}")
	    (?s    . "\\citep[see][?]{%l}")
	    (?u    . "\\fullcite{%l}")
	    (?i    . "\\citetitle{%l}")
	    (?a    . "\\citeauthor{%l}")
	    (?e    . "\\citefield{?}{%l}")
	    (?y    . "\\citeyear{%l}")
	    (?f    . "\\footcite{%l}")
	    (?F    . "\\footcite[?][]{%l}")
	    (?l    . "\\footfullcite{%l}"))))))

   ;; FIXME: The following list is the edited output of
   ;; `TeX-auto-generate' which probably includes internal macros of
   ;; jurabib.  Unfortunately the macros which should be accessible to
   ;; the user are not fully documented at the time of this writing.
   ;; But instead of including only the limited part which is
   ;; documented we rather give the user a bit too much.  The list
   ;; should be reduced when there is proper documentation, though.
   (TeX-add-symbols
    '("Wrapquotes" 1)
    '("apyformat" 1)
    '("artnumberformat" 1)
    '("artvolnumformat" 2)
    '("artvolumeformat" 1)
    '("artyearformat" 1)
    '("bibAnnote" 1)
    '("bibAnnoteFile" 1)
    '("bibAnnotePath" 1)
    '("bibEIMfont" 1)
    '("bibIMfont" 1)
    '("bibYear" 1)
    '("bibedformat" 1)
    '("bibedinformat" 1)
    '("bibenf" 5)
    '("biblenf" 5)
    '("bibnf" 5)
    '("bibnumberformat" 1)
    '("bibrenf" 5)
    '("bibrlenf" 5)
    '("bibrnf" 5)
    '("biburlfont" 1)
    '("edfont" 1)
    '("formatarticlepages" ["argument"] 2)
    '("fsted" 1)
    '("fullnameoxfordcrossref" 1)
    '("incolledformat" 5)
    '("jbArchPages" 1)
    '("jbPages" 1)
    '("jbannoteformat" 1)
    '("jbapifont" 1)
    '("jbarchnameformat" 1)
    '("jbarchsig" 2)
    '("jbartPages" 1)
    '("jbartcrossrefchecked" ["argument"] 1)
    '("jbauthorindexfont" 1)
    '("jbbibargs" 5)
    '("jbbibyearformat" 1)
    '("jbcitationoyearformat" 1)
    '("jbcitationyearformat" 1)
    '("jbcrossrefchecked" ["argument"] 1)
    '("jbedafti" 1)
    '("jbedbyincollcrossreflong" 1)
    '("jbedbyincollcrossrefshort" 1)
    '("jbedbyincollcrossrefshortnoapy" 1)
    '("jbedbyincollcrossrefshortwithapy" 1)
    '("jbedition" 1)
    '("jbeditorindexfont" 1)
    '("jbendnote" 1)
    '("jbflanguage" 1)
    '("jbincollcrossref" 2)
    '("jbisbn" 1)
    '("jbissn" 1)
    '("jbnote" 2)
    '("jborganizationindexfont" 1)
    '("jbpagesformat" 1)
    '("jbprformat" 1)
    '("jbrealcitation" 2)
    '("jbshortarchformat" 1)
    '("jbshortsubarchformat" 1)
    '("jbsy" 1)
    '("jbtiafed" 1)
    '("lookatfortype" 1)
    '("nobibliography" 1)
    '("nocitebuthowcited" 1)
    '("numberandseries" 2)
    '("pageadd" 1)
    '("pernumberformat" 1)
    '("pervolnumformat" 2)
    '("pervolumeformat" 1)
    '("peryearformat" 1)
    '("revnumberformat" 1)
    '("revvolnumformat" 2)
    '("revvolumeformat" 1)
    '("revyearformat" 1)
    '("snded" 1)
    '("textitswitch" 1)
    '("translator" 3)
    '("volumeformat" 1)
    "Bibbfsasep"
    "Bibbfsesep"
    "Bibbstasep"
    "Bibbstesep"
    "Bibbtasep"
    "Bibbtesep"
    "Bibchaptername"
    "Bibetal"
    "Edbyname"
    "IbidemMidName"
    "IbidemName"
    "NAT"
    "OpCit"
    "Reprint"
    "SSS"
    "Transfrom"
    "Volumename"
    "addtoalllanguages"
    "afterfoundersep"
    "aftervolsep"
    "ajtsep"
    "alsothesisname"
    "aprname"
    "augname"
    "bibBTsep"
    "bibJTsep"
    "bibPageName"
    "bibPagesName"
    "bibaesep"
    "bibaldelim"
    "bibaltformatalign"
    "bibandname"
    "bibanfont"
    "bibansep"
    "bibapifont"
    "bibapyldelim"
    "bibapyrdelim"
    "bibarchpagename"
    "bibarchpagesname"
    "bibardelim"
    "bibartperiodhowcited"
    "bibatsep"
    "bibauthormultiple"
    "bibbdsep"
    "bibbfsasep"
    "bibbfsesep"
    "bibbstasep"
    "bibbstesep"
    "bibbtasep"
    "bibbtesep"
    "bibbtfont"
    "bibbtsep"
    "bibbudcsep"
    "bibces"
    "bibchapterlongname"
    "bibchaptername"
    "bibcite"
    "bibcolumnsep"
    "bibcommenthowcited"
    "bibcontinuedname"
    "bibcrossrefcite"
    "bibcrossrefciteagain"
    "bibeandname"
    "bibedformat"
    "bibefnfont"
    "bibeimfont"
    "bibelnfont"
    "bibenf"
    "bibfnfmt"
    "bibfnfont"
    "bibhowcited"
    "bibibidfont"
    "bibidemPfname"
    "bibidemPmname"
    "bibidemPnname"
    "bibidemSfname"
    "bibidemSmname"
    "bibidemSnname"
    "bibidempfname"
    "bibidempmname"
    "bibidempnname"
    "bibidemsfname"
    "bibidemsmname"
    "bibidemsnname"
    "bibimfont"
    "bibincollcrossrefcite"
    "bibincollcrossrefciteagain"
    "bibjtfont"
    "bibjtsep"
    "bibleftcolumn"
    "bibleftcolumnadjust"
    "bibleftcolumnstretch"
    "biblenf"
    "biblnfmt"
    "biblnfont"
    "bibnf"
    "bibnotcited"
    "bibpagename"
    "bibpagesname"
    "bibpagesnamesep"
    "bibpldelim"
    "bibprdelim"
    "bibrevtfont"
    "bibrightcolumn"
    "bibrightcolumnadjust"
    "bibrightcolumnstretch"
    "bibsall"
    "bibsdanish"
    "bibsdutch"
    "bibsenglish"
    "bibsfinnish"
    "bibsfrench"
    "bibsgerman"
    "bibsitalian"
    "bibsnfont"
    "bibsnorsk"
    "bibsportuguese"
    "bibsspanish"
    "bibtabularitemsep"
    "bibtfont"
    "bibtotalpagesname"
    "biburlprefix"
    "biburlsuffix"
    "bibvolumecomment"
    "bibvtfont"
    "bothaesep"
    "bpubaddr"
    "byname"
    "citetitleonly"
    "citeyearpar"
    "commaename"
    "commaname"
    "dateldelim"
    "daterdelim"
    "decname"
    "diffpageibidemmidname"
    "diffpageibidemname"
    "edbyname"
    "edbysep"
    "editionname"
    "editorname"
    "editorsname"
    "enoteformat"
    "etalname"
    "etalnamenodot"
    "febname"
    "fifthedname"
    "firstedname"
    "footcitetitleonly"
    "formatpages"
    "foundername"
    "fourthedname"
    "fromdutch"
    "fromenglish"
    "fromfinnish"
    "fromfrench"
    "fromgerman"
    "fromitalian"
    "fromnorsk"
    "fromportuguese"
    "fromspanish"
    "herename"
    "howcitedprefix"
    "howcitedsuffix"
    "ibidem"
    "ibidemmidname"
    "ibidemname"
    "idemPfedbyname"
    "idemPfname"
    "idemPmedbyname"
    "idemPmname"
    "idemPnedbyname"
    "idemPnname"
    "idemSfedbyname"
    "idemSfname"
    "idemSmedbyname"
    "idemSmname"
    "idemSnedbyname"
    "idemSnname"
    "idemmidname"
    "idemname"
    "idempfedbyname"
    "idempfname"
    "idempmedbyname"
    "idempmname"
    "idempnedbyname"
    "idempnname"
    "idemsfedbyname"
    "idemsfname"
    "idemsmedbyname"
    "idemsmname"
    "idemsnedbyname"
    "idemsnname"
    "incollinname"
    "inname"
    "inseriesname"
    "janname"
    "jbCheckedFirst"
    "jbFirst"
    "jbFirstAbbrv"
    "jbJunior"
    "jbLast"
    "jbNotRevedNoVonJr"
    "jbNotRevedNoVonNoJr"
    "jbNotRevedOnlyLast"
    "jbNotRevedVonJr"
    "jbNotRevedVonNoJr"
    "jbPAGES"
    "jbPageName"
    "jbPages"
    "jbPagesName"
    "jbRevedFirstNoVonJr"
    "jbRevedFirstNoVonNoJr"
    "jbRevedFirstOnlyLast"
    "jbRevedFirstVonJr"
    "jbRevedFirstVonNoJr"
    "jbRevedNotFirstNoVonJr"
    "jbRevedNotFirstNoVonNoJr"
    "jbRevedNotFirstOnlyLast"
    "jbRevedNotFirstVonJr"
    "jbRevedNotFirstVonNoJr"
    "jbVon"
    "jbactualauthorfnfont"
    "jbactualauthorfont"
    "jbaddtomakehowcited"
    "jbaensep"
    "jbafterstartpagesep"
    "jbannotatorfont"
    "jbapifont"
    "jbarchnamesep"
    "jbarchpagename"
    "jbarchpagesname"
    "jbartPages"
    "jbatsep"
    "jbauthorfnfont"
    "jbauthorfont"
    "jbauthorfontifannotator"
    "jbauthorinfo"
    "jbbeforestartpagesep"
    "jbbfsasep"
    "jbbfsesep"
    "jbbookedaftertitle"
    "jbbstasep"
    "jbbstesep"
    "jbbtasep"
    "jbbtesep"
    "jbbtfont"
    "jbbtitlefont"
    "jbcitationyearformat"
    "jbcrossrefchecked"
    "jbdisablecitationcrossref"
    "jbdoitem"
    "jbdonotindexauthors"
    "jbdonotindexeditors"
    "jbdonotindexorganizations"
    "jbdotafterbibentry"
    "jbdotafterendnote"
    "jbdy"
    "jbedbyincollcrossrefcite"
    "jbedbyincollcrossrefciteagain"
    "jbedition"
    "jbedseplikecite"
    "jbeimfont"
    "jbfirstcitepageranges"
    "jbfootnoteformat"
    "jbfootnotenumalign"
    "jbfulltitlefont"
    "jbhowcitedcomparepart"
    "jbhowcitednormalpart"
    "jbhowsepannotatorfirst"
    "jbhowsepannotatorlast"
    "jbhowsepbeforetitle"
    "jbhowsepbeforetitleae"
    "jbhowsepbeforetitleibidemname"
    "jbignorevarioref"
    "jbimfont"
    "jbindexbib"
    "jbindexonlyfirstauthors"
    "jbindexonlyfirsteditors"
    "jbindexonlyfirstorganizations"
    "jbindextype"
    "jblookforgender"
    "jbmakeinbib"
    "jbmakeinbiblist"
    "jbmakeindexactual"
    "jbnotsamearch"
    "jbonlyforbib"
    "jbonlyforcitations"
    "jbonlyforfirstcitefullbegin"
    "jbonlyforfirstcitefullend"
    "jborgauthorfont"
    "jboyearincitation"
    "jbpagename"
    "jbpagenamenodot"
    "jbpages"
    "jbpagesep"
    "jbpagesname"
    "jbpagesnamesep"
    "jbsamearch"
    "jbsamesubarch"
    "jbsamesubarchindent"
    "jbshorttitlefont"
    "jbshowbibextralabel"
    "jbssedbd"
    "jbsubarchsep"
    "jbsuperscripteditionafterauthor"
    "jbtitlefont"
    "jbts"
    "jburldef"
    "jbuseidemhrule"
    "jbyear"
    "jbyearaftertitle"
    "julname"
    "junname"
    "jurthesisname"
    "marname"
    "mastersthesisname"
    "mayname"
    "nofirstnameforcitation"
    "noibidem"
    "noidem"
    "nopage"
    "novname"
    "numbername"
    "octname"
    "ofseriesname"
    "opcit"
    "organizationname"
    "origPAGES"
    "origartPages"
    "origbibces"
    "origcrossref"
    "origpages"
    "osep"
    "phdthesisname"
    "reprint"
    "reprintname"
    "reviewbyname"
    "reviewname"
    "reviewofname"
    "samepageibidemmidname"
    "samepageibidemname"
    "secondedname"
    "sepname"
    "sndecmd"
    "snded"
    "sndeditorname"
    "sndeditorsname"
    "technicalreportname"
    "testnosig"
    "textandname"
    "texteandname"
    "theHlvla"
    "theHlvlb"
    "theHlvlc"
    "theHlvld"
    "theHlvle"
    "theHlvlf"
    "theHlvlg"
    "theHlvlh"
    "theHlvli"
    "theHlvlj"
    "theHlvlk"
    "theHlvll"
    "thedname"
    "thirdedname"
    "trans"
    "transby"
    "transfrom"
    "updatename"
    "updatesep"
    "urldatecomment"
    "volname"
    "volumename"
    "volumeofname")))

;;; jurabib.el ends here
