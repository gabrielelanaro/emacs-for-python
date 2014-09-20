;;; listings.el --- AUCTeX style for `listings.sty'

;; Copyright (C) 2004, 2005, 2009 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@iwi.uni-sb.de>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-10-17
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

;; This file adds support for `listings.sty'.
;;
;; FIXME: Please make me more sophisticated!

;;; Code:

;; The following are options taken from chapter 4 of the listings
;; manual (2006/05/08 Version 1.3c).  Experimental options described
;; in chapter 5 are not included.
(defvar LaTeX-listings-key-val-options
  '(;; Space and placement
    ("float" ("t" "b" "p" "h")) ; Support [*] as an optional prefix and that
				; tbph are not exclusive.
    ("floatplacement" ("t" "b" "p" "h"))
    ("aboveskip")
    ("belowskip")
    ("lineskip")
    ("boxpos" ("b" "c" "t"))
    ;; The printed range
    ("print" ("true" "false"))
    ("firstline")
    ("lastline")
    ("linerange")
    ("showlines" ("true" "false"))
    ("emptylines")
    ("gobble")
    ;; Languages and styles
    ("style")
    ("language")
    ("alsolanguage")
    ("defaultdialect")
    ("printpod" ("true" "false"))
    ("usekeywordsintag" ("true" "false"))
    ("tagstyle")
    ("markfirstintag")
    ("makemacrouse" ("true" "false"))
    ;; Figure out the appearance
    ("basicstyle")
    ("identifierstyle")
    ("commentstyle")
    ("stringstyle")
    ("keywordstyle")
    ("classoffset")
    ("texcsstyle")
    ("directivestyle")
    ("emph")
    ("moreemph")
    ("deleteemph")
    ("emphstyle")
    ("delim")
    ("moredelim")
    ("deletedelim")
    ;; Getting all characters right
    ("extendedchars" ("true" "false"))
    ("inputencoding") ; Could make use of `latex-inputenc-coding-alist'.
    ("upquote" ("true" "false"))
    ("tabsize")
    ("showtabs" ("true" "false"))
    ("tab")
    ("showspaces" ("true" "false"))
    ("showstringspaces" ("true" "false"))
    ("formfeed")
    ;; Line numbers
    ("numbers" ("none" "left" "right"))
    ("stepnumber")
    ("numberfirstline" ("true" "false"))
    ("numberstyle")
    ("numbersep")
    ("numberblanklines" ("true" "false"))
    ("firstnumber" ("auto" "last")) ; Can also take a number.
    ("name")
    ;; Captions
    ("title")
    ("caption") ; Insert braces?
    ("label")
    ("nolol" ("true" "false"))
    ("captionpos" ("t" "b")) ; Can be a subset of tb.
    ("abovecaptionskip")
    ("belowcaptionskip")
    ;; Margins and line shape
    ("linewidth")
    ("xleftmargin")
    ("xrightmargin")
    ("resetmargins" ("true" "false"))
    ("breaklines" ("true" "false"))
    ("breakatwhitespace" ("true" "false"))
    ("prebreak")
    ("postbreak")
    ("breakindent")
    ("breakautoindent" ("true" "false"))
    ;; Frames
    ("frame" ("none" "leftline" "topline" "bottomline" "lines" "single"
	      "shadowbox"
	      ;; Alternative to the above values.  A subset of trblTRBL can be
	      ;; given.
	      "t" "r" "b" "l" "T" "R" "B" "L"))
    ("frameround" ("t" "f")) ; The input actually has to be four times {t,f}.
    ("framesep")
    ("rulesep")
    ("framerule")
    ("framexleftmargin")
    ("framexrightmargin")
    ("framextopmargin")
    ("framebottommargin")
    ("backgroundcolor")
    ("rulecolor")
    ("fillcolor")
    ("fulesepcolor")
    ("frameshape")
    ;; Indexing
    ("index")
    ("moreindex")
    ("deleteindex")
    ("indexstyle")
    ;; Column alignment
    ("columns" ("fixed" "flexible" "fullflexible")) ; Also supports an optional
						    ; argument with {c,l,r}.
    ("flexiblecolumns" ("true" "false"))
    ("keepspaces" ("true" "false"))
    ("basewidth")
    ("fontadjust" ("true" "false"))
    ;; Escaping to LaTeX
    ("texcl" ("true" "false"))
    ("mathescape" ("true" "false"))
    ("escapechar")
    ("escapeinside")
    ("escapebegin")
    ("escapeend")
    ;; Interface to fancyvrb
    ("fancyvrb" ("true" "false"))
    ("fvcmdparams")
    ("morefvcmdparams")
    ;; Language definitions
    ("keywordsprefix")
    ("keywords")
    ("morekeywords")
    ("deletekeywords")
    ("texcs")
    ("moretexcs")
    ("deletetexcs")
    ("directives")
    ("moredirectives")
    ("deletedirectives")
    ("sensitive" ("true" "false"))
    ("alsoletter")
    ("alsodigit")
    ("alsoother")
    ("otherkeywords")
    ("tag")
    ("string")
    ("morestring")
    ("deletestring")
    ("comment")
    ("morecomment")
    ("deletecomment")
    ("keywordcomment")
    ("morekeywordcomment")
    ("deletekeywordcomment")
    ("keywordcommentsemicolon")
    ("podcomment" ("true" "false")))
  "Key=value options for listings macros and environments.")

(TeX-add-style-hook
 "listings"
 (lambda ()
   ;; New symbols
   (TeX-add-symbols
    '("lstalias" ["Alias dialect"] "Alias" ["Dialect"] "Language")
    '("lstdefinestyle" "Style name"
      (TeX-arg-key-val LaTeX-listings-key-val-options))
    '("lstinline" TeX-arg-verb)
    '("lstinputlisting" [TeX-arg-key-val LaTeX-listings-key-val-options]
      TeX-arg-file)
    "lstlistoflistings"
    '("lstnewenvironment" "Name" ["Number or arguments"] ["Default argument"]
      "Starting code" "Ending code")
    '("lstset" (TeX-arg-key-val LaTeX-listings-key-val-options)))
   ;; New environments
   (LaTeX-add-environments
    '("lstlisting" LaTeX-env-args
      [TeX-arg-key-val LaTeX-listings-key-val-options]))
   ;; Filling
   (make-local-variable 'LaTeX-indent-environment-list)
   (add-to-list 'LaTeX-indent-environment-list
		'("lstlisting" current-indentation))
   (make-local-variable 'LaTeX-verbatim-regexp)
   (setq LaTeX-verbatim-regexp (concat LaTeX-verbatim-regexp "\\|lstlisting"))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   ;; Fontification
   (when (and (fboundp 'font-latex-add-keywords)
	      (fboundp 'font-latex-set-syntactic-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("lstnewenvironment" "{[[{{")) 'function)
     (font-latex-add-keywords '(("lstinputlisting" "[{")) 'reference)
     (font-latex-add-keywords '(("lstinline" "[{") ; The second argument should
						   ; actually be verbatim.
				("lstlistoflistings" ""))
			      'textual)
     (font-latex-add-keywords '(("lstalias" "{{")
				("lstdefinestyle" "{{")
				("lstset" "{"))
			      'variable)
     ;; For syntactic fontification, e.g. verbatim constructs.
     (font-latex-set-syntactic-keywords)
     ;; Tell font-lock about the update.
     (setq font-lock-set-defaults nil)
     (font-lock-set-defaults))))

(defvar LaTeX-listings-package-options '("draft" "final" "savemem" 
					 "noaspects")
  "Package options for the listings package.")

;;; listings.el ends here
