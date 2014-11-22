;;; context-nl.el --- Support for the ConTeXt dutch interface.

;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.

;; Maintainer: Berend de Boer <berend@pobox.com>
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

;; This file is loaded by context.el when required.

;;; Code:

;; Build upon ConTeXt
(require 'context)

;;; ConText macro names

(defvar ConTeXt-environment-list-nl
  '("achtergrond" "alinea" "bloktekst" "buffer" "citaat" "combinatie"
    "commentaar" "deelomgeving" "document" "doordefinitie"
    "doornummering" "figuur" "formule" "gegeven" "interactiemenu"
    "kadertekst" "kantlijn" "kleur" "kolommen" "legenda" "lokaal"
    "lokalevoetnoten" "margeblok" "naamopmaak" "naast"
    "opelkaar" "opmaak" "opsomming" "overlay" "overzicht"
    "paginafiguur" "positioneren" "profiel"
    "regel" "regelcorrectie" "regelnummeren" "regels"
    "smaller" "symboolset" "synchronisatie"
    "tabel" "tabellen" "tabulatie" "tekstlijn" "typen"
    "uitlijnen" "uitstellen" "vanelkaar" "verbergen" "versie"
    ;; project structure
    "omgeving" "onderdeel" "produkt" "project"
    ;; flowcharts, if you have loaded this module
    "FLOWcell" "FLOWchart"
    ;; typesetting computer languages
    "EIFFEL" "JAVA" "JAVASCRIPT" "MP" "PASCAL" "PERL" "SQL" "TEX" "XML"
    ;; some metapost environments
    "MPpositiongraphic" "useMPgraphic" "MPcode" "reusableMPgraphic"
    "uniqueMPgraphic")
  "List of the ConTeXt nl interface start/stop pairs.")

(defvar ConTeXt-define-list-nl
  '("achtergrond" "startstop" "typen")
  "List of ConTeXt nl interface macro's that define things.")

(defvar ConTeXt-setup-list-nl
  '("achtergronden" "achtergrond" "alineas" "arrangeren" "blanko"
    "blok" "blokjes" "blokkopje" "blokkopjes" "boven" "boventeksten"
    "brieven" "buffer" "buttons" "citeren" "clip" "combinaties"
    "commentaar" "doordefinieren" "doornummeren" "doorspringen"
    "dunnelijnen" "externefiguren" "formules" "formulieren"
    "hoofd" "hoofdteksten" "inmarge" "inspringen" "interactiebalk"
    "interactie" "interactiemenu" "interactiescherm" "interlinie"
    "invullijnen" "invulregels" "items" "kaderteksten" "kantlijn"
    "kapitalen" "kleuren" "kleur" "kolommen" "kop" "kopnummer"
    "koppelteken" "koppen" "koptekst" "korps" "korpsomgeving"
    "labeltekst" "layout" "legenda" "lijndikte" "lijn" "lijst"
    "margeblokken" "markering" "naastplaatsen" "nummeren" "omlijnd"
    "onder" "onderstrepen" "onderteksten" "opmaak" "opsomming"
    "paginanummer" "paginanummering" "paginaovergangen" "palet"
    "papierformaat" "papier" "paragraafnummeren" "plaatsblok"
    "plaatsblokken" "plaatsblokkensplitsen" "positioneren" "profielen"
    "programmas" "publicaties" "rasters" "referentielijst" "refereren"
    "regelnummeren" "regels" "register" "roteren" "samengesteldelijst"
    "sectieblok" "sectie" "sheets" "smaller" "sorteren" "spatiering"
    "stickers" "strut" "strut" "subpaginanummer" "symboolset"
    "synchronisatiebalk" "synchronisatie" "synoniemen" "systeem"
    "taal" "tabellen" "tab" "tabulatie" "tekst" "tekstlijnen"
    "tekstpositie" "tekstteksten" "tekstvariabele" "tolerantie" "type"
    "typen" "uitlijnen" "uitvoer" "url" "velden" "veld" "versies"
    "voet" "voetnootdefinitie" "voetnoten" "voetteksten" "witruimte")
  "List of the names of ConTeXt nl interface macro's that setup things.")

;; referencing in ConTeXt
(defvar ConTeXt-referencing-list-nl
  '("in" "op" "over" "paginareferentie" "tekstreferentie" "referentie")
  "List of ConTeXt en macro's that are used for referencing."
)

(defvar ConTeXt-other-macro-list-nl
  '("regellinks" "regelmidden" "regelrechts" "toonexternefiguren")
  "List of ConTeXt nl interface macro's that are not an environment nor a setup.")

(defun ConTeXt-define-command-nl (what)
  "The ConTeXt nl interface way of creating a define command."
  (concat "definieer" what))

(defun ConTeXt-setup-command-nl (what)
  "The ConTeXt nl interface way of creating a setup command."
  (concat "stel" what "in"))

(defvar ConTeXt-project-structure-list-nl
  '("project" "omgeving" "produkt" "onderdeel")
  "List of the names of ConTeXt project structure elements for its nl interface.  List should be in logical order.")

(defvar ConTeXt-section-block-list-nl
  '("inleidingen" "hoofdteksten" "bijlagen" "uitleidingen")
  "List of the names of ConTeXt section blocks for its nl interface.  List should be in logical order.")


;; TODO:
;; ConTeXt has alternative sections like title and subject. Currently
;; the level is used to find the section name, so the alternative
;; names are never found. Have to start using the section name instead
;; of the number.
(defvar ConTeXt-section-list-nl
  '(("deel" 0)
    ("hoofdstuk" 1)
    ("paragraaf" 2)
    ("subparagraaf" 3)
    ("subsubparagraaf" 4))
  ;; ("title" 1)
  ;; ("subject" 2)
  ;; ("subsubject" 3)
  ;; ("subsubsubject" 4)
  "List of the names of ConTeXt sections for its nl interface.")

(defvar ConTeXt-text-nl "tekst"
  "The ConTeXt nl interface body text group.")

(defvar ConTeXt-item-list-nl
  '("som" "its" "mar" "ran" "sub" "sym")
  "The ConTeXt macro's that are variants of item.")

(defcustom ConTeXt-default-environment-nl "opsomming"
  "*The default environment when creating new ones with `ConTeXt-environment'."
  :group 'ConTeXt-nl-environment
  :type 'string)

(defvar ConTeXt-extra-paragraph-commands-nl
  '("crlf" "par" "plaats[A-Za-z]+")
  "List of ConTeXt macros that should have their own line.
That is, besides the section(-block) commands.")

;; Emacs en menu names and labels should go here
;; to be done


;;; Mode

(defun ConTeXt-nl-mode-initialization ()
  "ConTeXt dutch interface specific initialization."
  (mapc 'ConTeXt-add-environments (reverse ConTeXt-environment-list-nl))

  (TeX-add-symbols
   '("but" ConTeXt-arg-define-ref (TeX-arg-literal " "))
   '("som" ConTeXt-arg-define-ref (TeX-arg-literal " "))
   '("items" [ConTeXt-arg-setup] (TeX-arg-string "Comma separated list"))
   '("its" ConTeXt-arg-define-ref (TeX-arg-literal " "))
   '("nop" (TeX-arg-literal " "))
   '("ran" TeX-arg-string (TeX-arg-literal " "))
   '("sub" ConTeXt-arg-define-ref (TeX-arg-literal " "))
   '("sym" (TeX-arg-string "Symbol") (TeX-arg-literal " "))))

;;;###autoload
(defun context-nl-mode ()
  "Major mode for editing files for ConTeXt using its dutch interface.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of TeX-mode-hook, and then the value
of context-mode-hook."
  (interactive)

  ;; set the ConTeXt interface
  (setq ConTeXt-current-interface "nl")

  ;; initialization
  (ConTeXt-mode-common-initialization)
  (ConTeXt-nl-mode-initialization)

  ;; set mode line
  (setq TeX-base-mode-name "ConTeXt-nl")
  (TeX-set-mode-name))

(provide 'context-nl)

;;; context-nl.el ends here
