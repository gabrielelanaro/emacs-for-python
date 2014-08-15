;;; plain-tex.el --- Support for plain TeX documents.

;; Copyright (C) 2010 Free Software Foundation, Inc.

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

;; This file provides support for plain TeX in AUCTeX.

;;; Code:

(require 'tex)
(require 'tex-buf)

;;; Tool bar

(defcustom plain-TeX-enable-toolbar t
  "Enable TeX tool bar in plain TeX mode."
  :group 'TeX-tool-bar
  :type 'boolean)

(defun plain-TeX-maybe-install-toolbar ()
  "Conditionally install tool bar buttons for plain TeX mode.
Install tool bar if `plain-TeX-enable-toolbar' is non-nil."
  (when plain-TeX-enable-toolbar
    ;; Defined in `tex-bar.el':
    (TeX-install-toolbar)))


;;; Keymap and menu

(defvar plain-TeX-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-mode-map)
    map)
  "Keymap used in plain TeX mode.")

(defvar plain-TeX-menu-entries
  (TeX-menu-with-help
   `(["Macro..." TeX-insert-macro
      :help "Insert a macro and possibly arguments"]
     ["Complete" TeX-complete-symbol
      :help "Complete the current macro"]
     "-"
     ("Insert Font"
      ["Emphasize"  (TeX-font nil ?\C-e) :keys "C-c C-f C-e"]
      ["Bold"       (TeX-font nil ?\C-b) :keys "C-c C-f C-b"]
      ["Typewriter" (TeX-font nil ?\C-t) :keys "C-c C-f C-t"]
      ["Small Caps" (TeX-font nil ?\C-c) :keys "C-c C-f C-c"]
      ["Sans Serif" (TeX-font nil ?\C-f) :keys "C-c C-f C-f"]
      ["Italic"     (TeX-font nil ?\C-i) :keys "C-c C-f C-i"]
      ["Slanted"    (TeX-font nil ?\C-s) :keys "C-c C-f C-s"]
      ["Roman"      (TeX-font nil ?\C-r) :keys "C-c C-f C-r"]
      ["Calligraphic" (TeX-font nil ?\C-a) :keys "C-c C-f C-a"])
     ("Replace Font"
      ["Emphasize"  (TeX-font t ?\C-e) :keys "C-u C-c C-f C-e"]
      ["Bold"       (TeX-font t ?\C-b) :keys "C-u C-c C-f C-b"]
      ["Typewriter" (TeX-font t ?\C-t) :keys "C-u C-c C-f C-t"]
      ["Small Caps" (TeX-font t ?\C-c) :keys "C-u C-c C-f C-c"]
      ["Sans Serif" (TeX-font t ?\C-f) :keys "C-u C-c C-f C-f"]
      ["Italic"     (TeX-font t ?\C-i) :keys "C-u C-c C-f C-i"]
      ["Slanted"    (TeX-font t ?\C-s) :keys "C-u C-c C-f C-s"]
      ["Roman"      (TeX-font t ?\C-r) :keys "C-u C-c C-f C-r"]
      ["Calligraphic" (TeX-font t ?\C-a) :keys "C-u C-c C-f C-a"])
     ["Delete Font" (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
     "-"
     ["Comment or Uncomment Region" TeX-comment-or-uncomment-region
      :help "Comment or uncomment the currently selected region"]
     ["Comment or Uncomment Paragraph" TeX-comment-or-uncomment-paragraph
      :help "Comment or uncomment the paragraph containing point"]
     ,TeX-fold-menu
     "-" . ,TeX-common-menu-entries)))

(easy-menu-define plain-TeX-mode-command-menu
    plain-TeX-mode-map
    "Command menu used in TeX mode."
    (TeX-mode-specific-command-menu 'plain-tex-mode))

(easy-menu-define plain-TeX-mode-menu
    plain-TeX-mode-map
    "Menu used in plain TeX mode."
    (cons "TeX" plain-TeX-menu-entries))


;;; The mode

(defcustom plain-TeX-mode-hook nil
  "A hook run in plain TeX mode buffers."
  :type 'hook
  :group 'TeX-misc)

(TeX-abbrev-mode-setup plain-tex-mode)

;;;###autoload
(defun TeX-plain-tex-mode ()
  "Major mode in AUCTeX for editing plain TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{plain-TeX-mode-map}

Entering `plain-tex-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of plain-TeX-mode-hook."
  (interactive)
  (plain-TeX-common-initialization)
  (setq major-mode 'plain-tex-mode)
  (use-local-map plain-TeX-mode-map)
  (easy-menu-add plain-TeX-mode-menu plain-TeX-mode-map)
  (easy-menu-add plain-TeX-mode-command-menu plain-TeX-mode-map)
  (setq TeX-base-mode-name "TeX")
  (setq TeX-command-default "TeX")
  (setq TeX-sentinel-default-function 'TeX-TeX-sentinel)
  (add-hook 'tool-bar-mode-on-hook 'plain-TeX-maybe-install-toolbar nil t)
  (when (if (featurep 'xemacs)
	    (featurep 'toolbar)
	  (and (boundp 'tool-bar-mode) tool-bar-mode))
    (plain-TeX-maybe-install-toolbar))
  (TeX-run-mode-hooks 'text-mode-hook 'TeX-mode-hook 'plain-TeX-mode-hook)
  (TeX-set-mode-name))

(defun plain-TeX-common-initialization ()
  "Common initialization for plain TeX like modes."
  (VirTeX-common-initialization)
  (set-syntax-table TeX-mode-syntax-table)
  (setq local-abbrev-table latex-mode-abbrev-table)
  (setq paragraph-start
	(concat
	 "\\(^[ \t]*$"
	 "\\|" (regexp-quote TeX-esc) "par\\|"
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 "begin\\|end\\|part\\|chapter\\|"
	 "section\\|subsection\\|subsubsection\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix\\|label\\|caption\\|"
	 "\\[\\|\\]"			; display math delimitors
	 "\\)"
	 "\\|"
	 "^[ \t]*\\$\\$"		; display math delimitor
	 "\\)" ))
  (setq paragraph-separate
	(concat
	 "[ \t]*"
	 "\\("
	 (regexp-quote TeX-esc) "par\\|"
	 "%\\|"
	 "$\\|"
	 "\\$\\$\\|"
	 (regexp-quote TeX-esc)
	 "\\("
	 "begin\\|end\\|label\\|caption\\|part\\|chapter\\|"
	 "section\\|subsection\\|subsubsection\\|"
	 "paragraph\\|include\\|includeonly\\|"
	 "tableofcontents\\|appendix\\|" (regexp-quote TeX-esc)
	 "\\)"
	 "\\)"))
  (setq TeX-header-end (regexp-quote "%**end of header"))
  (setq TeX-trailer-start (regexp-quote (concat TeX-esc "bye")))
  (TeX-add-symbols
   ;; From the TeX Book, Appendix B
   ;;
   ;; XXX: This should be refined and extended by somebody who is
   ;; familiar with plain TeX.
   "dag"
   "ddag"
   "copyright"
   "TeX"
   "dots"
   "break"
   "nobreak"
   "allowbreak"
   "hbox"
   "slash"
   "enskip"
   "quad"
   "qquad"
   "enspace"
   "thinspace"
   "negthinspace"
   "smallskip"
   "medskip"
   "bigskip"
   "eject"
   "supereject"
   "goodbreak"
   "filbreak"
   "smallbreak"
   "medbreak"
   "bigbreak"
   "hrulefill"
   "dotfill"
   "rightarrowfill"
   "leftarrowfill"
   "upbracefill"
   "downbracefill"
   "halign"
   "valign"
   "omit"
   "span"
   "multispan"
   "centerline"
   "rightline"
   "leftline"
   "line"
   "par"
   "noindent"
   "frenchspacing"
   "nonfrenchspacing"
   "llap"
   "rlap"
   "raggedright"
   "ttraggedright"
   "raggedbottom"
   "normalbottom"
   "obeylines"
   "obeyspaces"
   "hsize"
   "vsize"
   "hoffset"
   "voffset"
   "tolerance"
   "looseness"
   "parindent"
   "baselineskip"
   "parskip")
  (TeX-run-style-hooks "TEX"))


;;; Miscellaneous

(defcustom plain-TeX-clean-intermediate-suffixes
  TeX-clean-default-intermediate-suffixes
  "List of regexps matching suffixes of intermediate files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defcustom plain-TeX-clean-output-suffixes TeX-clean-default-output-suffixes
  "List of regexps matching suffixes of output files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)


;;; AmSTeX

(defvar AmSTeX-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-mode-map)
    map)
  "Keymap used in `AmSTeX-mode'.")

;; Menu for AmSTeX mode
(easy-menu-define AmSTeX-mode-command-menu
    AmSTeX-mode-map
    "Command menu used in AmsTeX mode."
    (TeX-mode-specific-command-menu 'ams-tex-mode))

(easy-menu-define AmSTeX-mode-menu
  AmSTeX-mode-map
  "Menu used in AMS-TeX mode."
  (cons "AmS-TeX" plain-TeX-menu-entries))

;;;###autoload
(defun ams-tex-mode ()
  "Major mode in AUCTeX for editing AmS-TeX files.
See info under AUCTeX for documentation.

Special commands:
\\{AmSTeX-mode-map}

Entering AmS-tex-mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `AmS-TeX-mode-hook'."
  (interactive)
  (plain-TeX-common-initialization)
  (setq major-mode 'ams-tex-mode)
  (use-local-map AmSTeX-mode-map)

  ;; Menu
  (easy-menu-add AmSTeX-mode-menu AmSTeX-mode-map)
  (easy-menu-add AmSTeX-mode-command-menu AmSTeX-mode-map)

  (setq TeX-base-mode-name "AmS-TeX")
  (setq TeX-command-default "AmSTeX")
  (TeX-run-mode-hooks 'text-mode-hook 'TeX-mode-hook 'AmS-TeX-mode-hook)
  (TeX-set-mode-name))

(provide 'plain-tex)

;;; plain-tex.el ends here
