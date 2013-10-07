;;; w3-keymap.el --- Keybindings for Emacs/W3

;; Copyright (c) 1996 - 1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: fx $
;; Created: $Date: 2001/05/29 15:46:28 $
;; Keywords: comm, help, hypermedia

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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Keymap definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map widget-keymap)

    (define-key map "h" (make-sparse-keymap))
    (define-key map "H" (make-sparse-keymap))
    (define-key map "a" (make-sparse-keymap))

    (define-key map "ha"       'w3-hotlist-apropos)
    (define-key map "hd"       'w3-hotlist-delete)
    (define-key map "hi"       'w3-hotlist-add-document)
    (define-key map "hv"       'w3-hotlist-view)
    (define-key map "hr"       'w3-hotlist-rename-entry)
    (define-key map "hu"       'w3-use-hotlist)
    (define-key map "hA"       'w3-hotlist-append)
    (define-key map "hI"       'w3-hotlist-add-document-at-point)
    (define-key map "hR"       'w3-hotlist-refresh)

    (define-key map "x" (make-sparse-keymap))
    (define-key map "xa" 'w3-hotindex-add-key)
    (define-key map "xd" 'w3-hotindex-rm-key)
    (define-key map "xq" 'w3-hotindex-query)

    (define-key map "HF"       'w3-history-forward)
    (define-key map "HB"       'w3-history-backward)
    (define-key map "Hv"       'w3-show-history-list)

    (define-key map " "	   'w3-scroll-up)
    (define-key map "<"        'beginning-of-buffer)
    (define-key map ">"        'end-of-buffer)
    (define-key map "?"        'w3-help)
    (define-key map "B"        'w3-history-backward)
    (define-key map "D"        'w3-download-url-at-point)
    (define-key map "F"        'w3-history-forward)
    (define-key map "G"        'w3-show-graphics)
    (define-key map "I"        'w3-popup-info)
    (define-key map "K"        'w3-save-this-url)
    ;; FIXME!
    ;;(define-key map "P"        'w3-print-url-under-point)
    (define-key map "Q"        'w3-leave-buffer)
    (define-key map "R"        'w3-refresh-buffer)
    (define-key map "S"        'w3-source-document-at-point)
    (define-key map "U"        'w3-use-links)
    (define-key map "V"        'w3-view-this-url)
    (define-key map "\C-?"     'scroll-down)
    (define-key map [backspace] 'scroll-down)
    (define-key map "\C-c\C-b" 'w3-show-history-list)
    (define-key map "\C-c\C-v" 'w3-version)
    (define-key map "\C-o"     'w3-fetch)
    (define-key map "\M-M"     'w3-mail-document-under-point)
    (define-key map "\M-m"	   'w3-mail-current-document)
    (define-key map "\M-s"	   'w3-save-as)
    (define-key map "\M-\r"    'w3-follow-inlined-image)
    (define-key map "b"	   'w3-widget-backward)
    (define-key map "c"        'w3-mail-document-author)
    (define-key map "d"        'w3-download-this-url)
    (define-key map "f"	   'w3-widget-forward)
    (define-key map "g"        'w3-reload-document)
    (define-key map "i"        'w3-document-information)
    (define-key map "k"        'w3-save-url)
    (define-key map "l"        'w3-goto-last-buffer)
    (define-key map "m"        'w3-complete-link)
    (define-key map "n"        'w3-widget-forward)
    (define-key map "o"	   'w3-open-local)
    (define-key map "p"        'w3-print-this-url)
    (define-key map "q"	   'w3-quit)
    (define-key map "r"        'w3-reload-document)
    (define-key map "s"        'w3-source-document)
    (define-key map "u"        'w3-leave-buffer)
    (define-key map "v"	   'url-view-url)
    (define-key map "w"        'w3-submit-bug)

    ;; These are duplicated here instead of just inherited from widget-keymap
    ;; due to some issues with Emacspeak.  FIXME.
    (define-key map [tab] 'w3-widget-forward)
    (define-key map [(shift tab)] 'w3-widget-backward)
    (define-key map [(meta tab)] 'w3-widget-backward)
    (define-key map [backtab] 'w3-widget-backward)

    ;; Emulate some netscape stuff by default
    (define-key map [(control alt t)] 'url-list-processes)
    (define-key map [(control meta t)] 'url-list-processes)

    ;; Have fun with document ordering
    (define-key map [(meta space)] 'w3-next-document)
    (define-key map [(meta delete)] 'w3-prev-document)
    map)
  "Keymap to use in w3-mode.")

(provide 'w3-keymap)
