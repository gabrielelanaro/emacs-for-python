;;; ltx-base.el --- AUCTeX style for basic LaTeX commands.

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Frank Küster <frank@kuesterei.ch>
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

;; This file adds general support for basic LaTeX commands used for
;; writing LaTeX class files (.cls), style files (.sty) and package
;; files (.dtx).

;;; Code:

(TeX-add-style-hook
 "ltx-base"
 (function
  (lambda ()
    (TeX-add-symbols
     '("DeclareRobustCommand" TeX-arg-define-macro [ "Number of arguments" ] t)
     '("CheckCommand" TeX-arg-define-macro [ "Number of arguments" ] t)
     '("@addtoreset" TeX-arg-counter "Within counter" "counter")
     '("addvspace" "space")
     '("addpenalty" "penalty")
     '("ProvidesClass" "name" [ "release information" ])
     '("ProvidesPackage" "name" [ "release information" ])
     '("ProvidesFile" "filename" [ "release information" ])
     '("NeedsTeXFormat" "format" [ "release" ])
     '("DeclareOption" "option" t)
     ;; would be great if DeclareOption RET * RET would give
     ;; \DeclareOption*!
     "DeclareOption*"
     '("CurrentOption" 0)
     '("PassOptionsToPackage" "option list" "package")
     '("ExecuteOptions" "option list")
     "ProcessOptions"
     "ProcessOptions*"
     '("OptionNotUsed" 0)
      ;; candidate for opt/mand toggling
     '("RequirePackage" [ "option list" ] "package" [ "release" ])
     '("LoadClass" [ "option list" ] "class" [ "release" ])
     "AtEndOfPackage"
     "AtEndOfClass"
     "AtBeginDocument"
     "AtEndDocument"
     '("IfFileExists" "filename" 2)
     '("InputIfFileExists" "filename" 2)
     '("PackageWarning" "name" t)
     '("PackageWarningNoLine" "name" t)
     '("PackageInfo" "name" t)
     '("PackageError" "name" "short text" t)
     '("ClassWarning" "name" t)
     '("ClassWarningNoLine" "name" t)
     '("ClassInfo" "name" t)
     '("ClassError" "name" "short text" t)
     '("MessageBreak" 0)
     '("@ifpackageloaded" "package" 2)
     '("@ifpackagelater" "package" "date" 2)
     '("@ifpackagewith" "package" "options" 2)
     '("message" "Log Message")
     '("@ifundefined" "Macro Name" 2)
     '("@ifnextchar" (TeX-arg-literal " ") (TeX-arg-free "character") 2 )
     "expandafter")))
 LaTeX-dialect)

;; Local Variables:
;; coding: iso-8859-1
;; End:
