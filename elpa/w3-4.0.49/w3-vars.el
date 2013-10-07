;;; w3-vars.el,v --- All variable definitions for emacs-w3

;; Copyright (c) 1996-1999, 2001, 2013 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Variable definitions for W3.

;;; Code:

(require 'w3-cus)			; Grab everything that is customized
(require 'wid-edit)			; For `widget-keymap'

(defconst w3-version-number
  (let ((x "$State: p4.0pre.47 $"))
    (if (string-match "State:[ \t\n]+\\([^ \t\n]+\\)" x)
	(setq x (substring x (match-beginning 1) (match-end 1)))
      (setq x (substring x 1)))
    (mapconcat
     (function (lambda (x) (if (= x ?-) "." (char-to-string x)))) x ""))
  "Version number of w3-mode.")

(defconst w3-version-date (let ((x "$Date: 2001/10/01 11:42:46 $"))
			    (if (string-match "Date: \\([^ \t\n]+\\)" x)
				(substring x (match-beginning 1) (match-end 1))
			      x))
  "Date this version of w3-mode was released.")

(defconst w3-version
  (format "WWW %s %s" w3-version-number w3-version-date)
  "More descriptive version of w3-version-number.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General configuration variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-track-last-buffer nil
  "*Whether to track the last w3 buffer to automatically switch to with
 M-x w3.")

(defvar w3-gc-cons-threshold-multiplier 1
  "Amount to temporarily multiply gc-cons-threshold by when parsing HTML.
Setting this to a number greater than 1 will result in less frequent
garbage collections when parsing an HTML document, which may often speed
up handling of a large document with many elements.  The disadvantage is
that it allows Emacs's total memory usage to grow larger, which may result
in later garbage collections taking more time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store the database of HTML general entities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commentary on the basis of the current W3C entity list.  -- fx
(defvar w3-html-entities 
  '(
    ;;(excl        .  33)
    (quot        .  34)
    ;;(num         .  35)
    ;;(dollar      .  36)
    ;;(percent     .  37)
    (amp         .  38)
    (rsquo       .  39)			; should be U+8217
    ;;(apos        .  39)
    ;;(lpar        .  40)
    ;;(rpar        .  41)
    ;;(ast         .  42)
    ;;(plus        .  43)
    ;;(comma       .  44)
    ;;(period      .  46)
    ;;(colon       .  58)
    ;;(semi        .  59)
    (lt          .  60)
    ;;(equals      .  61)
    (gt          .  62)
    ;;(quest       .  63)
    ;;(commat      .  64)
    ;;(lsqb        .  91)
    ;;(rsqb        .  93)
    (uarr        .  94)			; should be U+8593
    ;;(lowbar      .  95)
    (lsquo       .  96)			; should be U+8216
    (lcub        . 123)
    ;;(verbar      . 124)
    (rcub        . 125)
    (tilde       . 126)
    (nbsp        . 160)
    (iexcl       . 161)
    (cent        . 162)
    (pound       . 163)
    (curren      . 164)
    (yen         . 165)
    (brvbar      . 166)
    (sect        . 167)
    (uml         . 168)
    (copy        . 169)
    (ordf        . 170)
    (laquo       . 171)
    (not         . 172)
    (shy         . 173)
    (reg         . 174)
    (macr        . 175)
    (deg         . 176)
    (plusmn      . 177)
    (sup2        . 178)
    (sup3        . 179)
    (acute       . 180)
    (micro       . 181)
    (para        . 182)
    (middot      . 183)
    (cedil       . 184)
    (sup1        . 185)
    (ordm        . 186)
    (raquo       . 187)
    (frac14      . 188)
    (frac12      . 189)
    (frac34      . 190)
    (iquest      . 191)
    (Agrave      . 192)
    (Aacute      . 193)
    (Acirc       . 194)
    (Atilde      . 195)
    (Auml        . 196)
    (Aring       . 197)
    (AElig       . 198)
    (Ccedil      . 199)
    (Egrave      . 200)
    (Eacute      . 201)
    (Ecirc       . 202)
    (Euml        . 203)
    (Igrave      . 204)
    (Iacute      . 205)
    (Icirc       . 206)
    (Iuml        . 207)
    (ETH         . 208)
    (Ntilde      . 209)
    (Ograve      . 210)
    (Oacute      . 211)
    (Ocirc       . 212)
    (Otilde      . 213)
    (Ouml        . 214)
    (times       . 215)
    (Oslash      . 216)
    (Ugrave      . 217)
    (Uacute      . 218)
    (Ucirc       . 219)
    (Uuml        . 220)
    (Yacute      . 221)
    (THORN       . 222)
    (szlig       . 223)
    (agrave      . 224)
    (aacute      . 225)
    (acirc       . 226)
    (atilde      . 227)
    (auml        . 228)
    (aring       . 229)
    (aelig       . 230)
    (ccedil      . 231)
    (egrave      . 232)
    (eacute      . 233)
    (ecirc       . 234)
    (euml        . 235)
    (igrave      . 236)
    (iacute      . 237)
    (icirc       . 238)
    (iuml        . 239)
    (eth         . 240)
    (ntilde      . 241)
    (ograve      . 242)
    (oacute      . 243)
    (ocirc       . 244)
    (otilde      . 245)
    (ouml        . 246)
    (divide      . 247)
    (oslash      . 248)
    (ugrave      . 249)
    (uacute      . 250)
    (ucirc       . 251)
    (uuml        . 252)
    (yacute      . 253)
    (thorn       . 254)
    (yuml        . 255)

    ;; Special handling of these
    (frac56      . "5/6")
    (frac16      . "1/6")
    (frac45      . "4/5")
    (frac35      . "3/5")
    (frac25      . "2/5")
    (frac15      . "1/5")
    (frac23      . "2/3")
    (frac13      . "1/3")
    (frac78      . "7/8")
    (frac58      . "5/8")
    (frac38      . "3/8")
    (frac18      . "1/8")
    
    ;; The following 5 entities are not mentioned in the HTML 2.0
    ;; standard, nor in any other HTML proposed standard of which I
    ;; am aware.  I am not even sure they are ISO entity names.  ***
    ;; Hence, some arrangement should be made to give a bad HTML
    ;; message when they are seen.
    (ndash       .  45)
    (mdash       .  45)
    (emsp        .  32)
    (ensp        .  32)
    (sim         . 126)
    (le          . "<=")
    (agr         . "alpha")
    (rdquo       . "''")
    (ldquo       . "``")
    (trade       . "(TM)")
    ;; To be done
    ;; (shy      . ????) ; soft hyphen
    )
  "*An assoc list of entity names and how to actually display them.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-popup-menu
  '("Emacs-W3 Commands"
    ["Back" w3-history-backward (car (w3-history-find-url-internal (url-view-url t)))]
    ["Forward" w3-history-forward (cdr (w3-history-find-url-internal (url-view-url t)))]
    "---"
    ["Reload"       (w3-reload-document) t]
    ["Show Images"  (w3-load-delayed-images) w3-delayed-images]
    "---"
    ["Add bookmark" (w3-hotlist-add-document nil) t]
    )
  "The shorter popup menu.")

(defvar w3-graphlink-menu
  '(("Open this Image (%s)"     . w3-fetch)
    ("Save this Image As..."    . w3-download-url)
    ("Copy this Image Location" . w3-save-url))
  "An assoc list of function names and labels.  These will be displayed
in a popup menu when the mouse is pressed on a hyperlink.  Format is
\( (label . function)), function is called with one argument, the URL of
the link.  Each label can have exactly one `%s' that will be replaced by
the URL of the link.")

(defvar w3-hyperlink-menu
  '(("Open this Link (%s)"        . w3-fetch)
    ("Add Bookmark for this Link" . w3-hotlist-add-document-at-point)
    ("New Window with this Link"  . w3-fetch-other-frame)
    ("Save Link As..."            . w3-download-url)
    ("Copy this Link Location to Clipboard" . w3-save-url))
  "An assoc list of function names and labels.  These will be displayed
in a popup menu when the mouse is pressed on a hyperlink.  Format is
\( (label . function)), function is called with one argument, the URL of
the link.  Each label can have exactly one `%s' that will be replaced by
the URL of the link.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables internal to W3, you should not change any of these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-graphics-list nil
  "List of graphics already read in.")

(defvar w3-delayed-images nil
  "A buffer-local variable holding positions and urls of images within
the buffer.")

(defvar w3-frameset-structure nil
  "Frameset structure, heap of '(frameset ({cols|rows} \"<dimensions>\")) and '(<frame name> <href>)")

(defvar w3-frame-name nil
  "Frame name")

(defvar w3-base-target nil
  "Base target name")

(defvar w3-target-window-distances nil
  "Target window distances")

(defvar w3-tty-char-width 8
  "*Char width to use when in a tty")

(defvar w3-tty-char-height 15
  "*Char height to use when in a tty")

(defvar w3-form-radio-elements nil "Internal variable - do not touch!")
(defvar w3-form-elements nil "Internal variable - do not touch!")

(defvar w3-user-stylesheet nil
  "The global stylesheet for this user.")

(defvar w3-current-stylesheet nil
  "The stylesheet for this document.")

(defvar w3-last-fill-pos nil
  "An internal variable for the new display engine that specifies the
last character position that was correctly filled.")

(defvar w3-active-faces nil "The list of active faces.")
(defvar w3-active-voices nil "The list of active voices.")

(defconst w3-bug-address "w3-bugs@xemacs.org"
  "Address of current maintainer, where to send bug reports.")
(defvar w3-current-isindex nil "Is the current document a searchable index?")
(defvar w3-current-buffer nil "Is the current W3 buffer")
(defvar w3-current-last-buffer nil "Last W3 buffer seen before this one.")
(defvar w3-current-links nil "An assoc list of <link> tags for this doc.")
(defvar w3-current-metainfo nil "An assoc list of <meta> tags for this doc.")
(defvar w3-current-source nil "Source of current document.")
(defvar w3-current-parse nil "Parsed version of current document.")
(defvar w3-current-badhtml nil "List of HTML warnings for this page.")
(defvar w3-find-this-link nil "Link to go to within a document.")
(defvar w3-hidden-forms nil "List of hidden form areas and their info.")
(defvar w3-hotlist nil "Default hotlist.")
(defvar w3-last-buffer nil "The last W3 buffer visited.")
(defvar w3-roman-characters "ivxLCDMVX" "Roman numerals.")
(defvar w3-setup-done nil "Have we been through setup code yet?")

(defvar w3-strict-width nil
  "*This variable will control how wide emacs thinks the current window is.
This is useful when working in batch mode, and (window-width) returns the
wrong value.  If the value is nil, it will use the value (window-width)
returns.")

(defvar w3-submit-button nil
  "A widget object specifying what button was pressed to submit a form.")

(defvar w3-explicit-conversion-tree nil
  "Tree to hold explicit coding systems for URLs and their superdirs:
   ((hostN default-coding (dirN-1 default-coding (dirN-1-1 ...) ...) ...)
    ...)")

(defvar w3-explicit-encodings-changed-since-last-save nil
  "Whether the explicit encodings tree has changed since the last save operation.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer-local variables to keep around when going into w3-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-id-positions nil "Internal use only.")
(defvar w3-imagemaps nil "Internal use only.")

;; Why aren't these just permanent-local?  -- fx
(defvar w3-persistent-variables
  '(
    ;; So we can show the URL in the list-buffers listing
    list-buffers-directory
    ;; So widgets don't get lost
    widget-field-new
    w3-form-radio-elements
    w3-form-elements
    url-current-object
    w3-current-badhtml
    w3-current-parse
    w3-current-isindex
    w3-current-last-buffer
    w3-current-links
    w3-current-metainfo
    w3-current-source
    w3-delayed-images
    w3-hidden-forms
    w3-current-stylesheet
    w3-form-labels
    w3-id-positions
    w3-imagemaps
    w3-base-target
    w3-target-window-distances
    w3-frameset-structure
    buffer-file-coding-system
    url-current-mime-headers
    )
  "A list of variables that should be preserved when entering w3-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emulation stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-netscape-emulation-minor-mode nil
  "Whether we are in the netscape emulation minor mode.")
(defvar w3-netscape-emulation-minor-mode-map (make-sparse-keymap)
  "Keymap for netscape emulation.")
(defvar w3-lynx-emulation-minor-mode nil
  "Whether we are in the lynx emulation minor mode.")
(defvar w3-lynx-emulation-minor-mode-map (make-sparse-keymap)
  "Keymap for lynx emulation.")
(defvar w3-last-search-item nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-form-labels nil "")
(dolist (var w3-persistent-variables)
  (if (boundp var)
      (make-variable-buffer-local var)))

(make-variable-buffer-local 'w3-last-fill-pos)
(make-variable-buffer-local 'w3-frame-name)
(make-variable-buffer-local 'w3-active-faces)
(make-variable-buffer-local 'w3-netscape-emulation-minor-mode)
(make-variable-buffer-local 'w3-lynx-emulation-minor-mode)
(make-variable-buffer-local 'w3-last-search-item)

(defvar w3-table-structure nil
  "Structure to hold table info")
(make-variable-buffer-local 'w3-table-structure)

(require 'w3-keymap)
(provide 'w3-vars)
