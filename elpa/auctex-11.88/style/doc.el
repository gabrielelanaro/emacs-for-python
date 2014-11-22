;;; doc.el --- AUCTeX style for `doc.sty'

;; Copyright (C) 2004, 2008 Free Software Foundation, Inc.

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

;; This file adds support for `doc.sty'.

;;; Code:

(defun LaTeX-env-no-comment (environment)
  "Insert ENVIRONMENT and make sure there is no commented empty line inside."
  (LaTeX-insert-environment environment)
  (unless (TeX-active-mark)
    (when (save-excursion
	    (beginning-of-line)
	    (looking-at (concat "[ \t]+$\\|[ \t]*"
				TeX-comment-start-regexp "+[ \t]*$")))
      (delete-region (line-beginning-position) (line-end-position))
      (indent-according-to-mode))))

(defun LaTeX-doc-after-insert-macrocode (env start end)
  "Make sure the macrocode environment is properly formatted after insertion."
  (when (TeX-member env '("macrocode" "macrocode*") 'string-equal)
    (save-excursion
      (goto-char end)
      (skip-chars-backward " \t")
      (when (bolp)
	(insert "%")
	(indent-according-to-mode))
      (goto-char start)
      (skip-chars-backward " \t")
      (when (bolp)
	(insert "%")
	(indent-according-to-mode)))))

(TeX-add-style-hook
 "doc"
 (lambda ()
   (add-to-list (make-local-variable 'LaTeX-indent-environment-list)
		'("macrocode" current-indentation))
   (add-to-list 'LaTeX-indent-environment-list
		'("macrocode*" current-indentation))
   (add-hook 'LaTeX-after-insert-env-hooks 'LaTeX-doc-after-insert-macrocode
	     nil t)
   (LaTeX-add-environments
    "theglossary"
    '("macrocode" LaTeX-env-no-comment)
    '("macrocode*" LaTeX-env-no-comment)
    '("macro" "Macro"))
   (TeX-add-symbols
    "EnableCrossrefs"
    "DisableCrossrefs"
    "DoNotIndex"
    "DontCheckModules"
    "CheckModules"
    "Module"
    '("DescribeMacro" "Macro")
    '("DescribeEnv" "Environment")
    "verbatim"
    "verb"
    "parg"
    "oarg"
    "marg"
    "meta"
    "cmd"
    "makelabel"
    "MacroFont"
    "MacroFont"
    "AltMacroFont"
    "AltMacroFont"
    "PrintMacroName"
    "PrintDescribeMacro"
    "PrintDescribeEnv"
    "PrintEnvName"
    "MakePrivateLetters"
    "actualchar"
    "quotechar"
    "levelchar"
    "encapchar"
    "verbatimchar"
    "SpecialIndex"
    "SpecialMainIndex"
    "SpecialMainEnvIndex"
    "SpecialUsageIndex"
    "SpecialEnvIndex"
    "SortIndex"
    "LeftBraceIndex"
    "RightBraceIndex"
    "PercentIndex"
    "OldMakeindex"
    "PercentIndex"
    "IndexPrologue"
    "IndexParms"
    "subitem"
    "subsubitem"
    "indexspace"
    "efill"
    "pfill"
    "PrintIndex"
    '("changes" "version" TeX-arg-date t)
    "generalname"
    "RecordChanges"
    "GlossaryPrologue"
    "GlossaryParms"
    "PrintChanges"
    "AlsoImplementation"
    "StopEventually"
    "OnlyDescription"
    "Finale"
    "IndexInput"
    "maketitle"
    "MakeShortVerb"
    "DeleteShortVerb"
    "MakeShortverb"
    "DeleteShortverb"
    "CheckSum"
    "CharacterTable"
    "CharTableChanges"
    "CodelineNumbered"
    "CodelineIndex"
    "PageIndex"
    "theCodelineNo"
    "theCodelineNo"
    "DocstyleParms"
    "MakePercentIgnore"
    "MakePercentComment"
    "DocInput"
    "DocInclude"
    "GetFileInfo"
    "filename"
    "fileinfo")
   (TeX-run-style-hooks "shortvrb"))
 LaTeX-dialect)

;; Local Variables:
;; coding: iso-8859-1
;; End:
