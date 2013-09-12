;;; w3-emulate.el --- All variable definitions for emacs-w3

;; Copyright (c) 1996-1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: fx $
;; Created: $Date: 2001/05/29 15:52:51 $
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

;;; Commentary:

;; Provide emulations of various other web browsers

;;; Code:

(require 'w3-vars)
(require 'url-vars)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First, we emulate Netscape 2.x
;; ------------------------------
;; This entails mainly a few new keybindings.
;; Alt-S    == Save As
;; Alt-M    == New Mail Message
;; Alt-N    == New Window
;; Alt-L    == Open Location
;; Alt-O    == Open File
;; Alt-P    == Print
;; Alt-Q    == Quit
;; Alt-F    == Search
;; Alt-G    == Search Again
;; Alt-R    == Reload
;; Alt-I    == Load Images
;; Alt-A    == Add Bookmark
;; Alt-B    == Show Bookmark Window
;; Alt-H    == Show History Window
;; Alt-Left == Back
;; Alt-Right== Forward
;; Right    == Scroll left
;; Left     == Scroll right
;; Up       == Smooth scroll up
;; Down     == Smooth scroll down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key w3-netscape-emulation-minor-mode-map "\M-s" 'w3-save-as)
(define-key w3-netscape-emulation-minor-mode-map "\M-m" 'w3-mailto)
(define-key w3-netscape-emulation-minor-mode-map "\M-n" 'make-frame)
(define-key w3-netscape-emulation-minor-mode-map "\M-l" 'w3-fetch)
(define-key w3-netscape-emulation-minor-mode-map "\M-o" 'w3-open-local)
(define-key w3-netscape-emulation-minor-mode-map "\M-p" 'w3-print-this-url)
(define-key w3-netscape-emulation-minor-mode-map "\M-q" 'w3-quit)
(define-key w3-netscape-emulation-minor-mode-map "\M-f" 'w3-search-forward)
(define-key w3-netscape-emulation-minor-mode-map "\M-g" 'w3-search-again)
(define-key w3-netscape-emulation-minor-mode-map "\M-r" 'w3-reload-document)
(define-key w3-netscape-emulation-minor-mode-map "\M-i" 'w3-load-delayed-images)
(define-key w3-netscape-emulation-minor-mode-map "\M-a" 'w3-hotlist-add-document)
(define-key w3-netscape-emulation-minor-mode-map "\M-b" 'w3-hotlist-view)
(define-key w3-netscape-emulation-minor-mode-map "\M-h" 'w3-show-history-list)

(define-key w3-netscape-emulation-minor-mode-map [up]
  (function (lambda () (interactive) (scroll-down 1))))
(define-key w3-netscape-emulation-minor-mode-map [down]
  (function (lambda () (interactive) (scroll-up 1))))
(define-key w3-netscape-emulation-minor-mode-map [right] 'scroll-left)
(define-key w3-netscape-emulation-minor-mode-map [left] 'scroll-right)
(define-key w3-netscape-emulation-minor-mode-map [(meta left)]
  'w3-history-backward)
(define-key w3-netscape-emulation-minor-mode-map [(meta right)]
  'w3-history-forward)

(defun turn-on-netscape-emulation ()
  (interactive)
  (w3-lynx-emulation-minor-mode 0)
  (w3-netscape-emulation-minor-mode 1))

(defun w3-netscape-emulation-minor-mode (&optional arg)
  "Minor mode for emulating netscape key navigation."
  (interactive "P")
  (cond
   ((null arg)
    (setq w3-netscape-emulation-minor-mode
	  (not w3-netscape-emulation-minor-mode))
    (if w3-netscape-emulation-minor-mode
	(setq w3-lynx-emulation-minor-mode nil)))
   ((= 0 arg)
    (setq w3-netscape-emulation-minor-mode nil))
   (t
    (setq w3-lynx-emulation-minor-mode nil
	  w3-netscape-emulation-minor-mode t)))
  )

(defsubst w3-skip-word ()
  (skip-chars-forward "^ \t\n\r")
  (skip-chars-forward " \t"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Now, lets try Lynx
;; ------------------
;; A few keybindings and modifications to some default functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun turn-on-lynx-emulation ()
  (interactive)
  (w3-netscape-emulation-minor-mode 0)
  (w3-lynx-emulation-minor-mode 1))

(defun w3-lynx-emulation-minor-mode (&optional arg)
  "Minor mode for emulating lynx key navigation."
  (interactive "P")
  (cond
   ((null arg)
    (setq w3-lynx-emulation-minor-mode
	  (not w3-lynx-emulation-minor-mode))
    (if w3-lynx-emulation-minor-mode
	(setq w3-netscape-emulation-minor-mode nil)))
   ((= 0 arg)
    (setq w3-lynx-emulation-minor-mode nil))
   (t
    (setq w3-lynx-emulation-minor-mode t
	  w3-netscape-emulation-minor-mode nil))))

;; The list of keybindings for lynx minor mode was compiled from:
;; http://www.crl.com/~subir/lynx/lynx_help/keystroke_commands/keystroke_help.htm

;; Movement
(define-key w3-lynx-emulation-minor-mode-map [up]   'widget-backward)
(define-key w3-lynx-emulation-minor-mode-map [down] 'widget-forward)
(define-key w3-lynx-emulation-minor-mode-map [right] 'widget-button-press)
(define-key w3-lynx-emulation-minor-mode-map [left] 'w3-history-backward)

;; Scrolling
(define-key w3-lynx-emulation-minor-mode-map "+"    'w3-scroll-up)
(define-key w3-lynx-emulation-minor-mode-map "-"    'scroll-down)
(define-key w3-lynx-emulation-minor-mode-map "b"    'scroll-down)
(define-key w3-lynx-emulation-minor-mode-map "\C-a" 'w3-start-of-document)
(define-key w3-lynx-emulation-minor-mode-map "\C-e" 'w3-end-of-document)
(define-key w3-lynx-emulation-minor-mode-map "\C-f" 'scroll-up)
(define-key w3-lynx-emulation-minor-mode-map "\C-b" 'scroll-down)
(define-key w3-lynx-emulation-minor-mode-map "\C-n" (lambda () (interactive) (scroll-up 2)))
(define-key w3-lynx-emulation-minor-mode-map "\C-p" (lambda () (interactive) (scroll-down 2)))
(define-key w3-lynx-emulation-minor-mode-map ")"    'ignore) ; forward half
(define-key w3-lynx-emulation-minor-mode-map "("    'ignore) ; back half
(define-key w3-lynx-emulation-minor-mode-map "#"    'w3-toggle-toolbar)

;; Dired bindings don't have any meaning for us

;; Other
(define-key w3-lynx-emulation-minor-mode-map "?"   'w3-help)
(define-key w3-lynx-emulation-minor-mode-map "a"   'w3-hotlist-add-document)
(define-key w3-lynx-emulation-minor-mode-map "c"   'w3-mail-document-author)
(define-key w3-lynx-emulation-minor-mode-map "d"   'w3-download-url) 
(define-key w3-lynx-emulation-minor-mode-map "e"   'ignore) ; edit current
(define-key w3-lynx-emulation-minor-mode-map "f"   'dired)
(define-key w3-lynx-emulation-minor-mode-map "g"   'w3-fetch)
(define-key w3-lynx-emulation-minor-mode-map "h"   'w3-help)
(define-key w3-lynx-emulation-minor-mode-map "i"   'ignore)
(define-key w3-lynx-emulation-minor-mode-map "j"   'w3-use-hotlist)
(define-key w3-lynx-emulation-minor-mode-map "k"   'describe-mode)
(define-key w3-lynx-emulation-minor-mode-map "l"   'w3-complete-link)
(define-key w3-lynx-emulation-minor-mode-map "m"   'w3)
(define-key w3-lynx-emulation-minor-mode-map "n"   'w3-search-again)
(define-key w3-lynx-emulation-minor-mode-map "o"   'w3-preferences-edit)
(define-key w3-lynx-emulation-minor-mode-map "p"   'w3-print-this-url)
(define-key w3-lynx-emulation-minor-mode-map "q"   'w3-quit)
(define-key w3-lynx-emulation-minor-mode-map "r"   'w3-hotlist-delete)
(define-key w3-lynx-emulation-minor-mode-map "t"   'ignore) ; tag
(define-key w3-lynx-emulation-minor-mode-map "u"   'w3-history-backward)
(define-key w3-lynx-emulation-minor-mode-map "/"   'w3-search-forward)
(define-key w3-lynx-emulation-minor-mode-map "v"   'w3-hotlist-view)
(define-key w3-lynx-emulation-minor-mode-map "V"   'w3-hotlist-view)
(define-key w3-lynx-emulation-minor-mode-map "x"   'widget-button-press)
(define-key w3-lynx-emulation-minor-mode-map "z"   'keyboard-quit)
(define-key w3-lynx-emulation-minor-mode-map "="   'w3-document-information)
(define-key w3-lynx-emulation-minor-mode-map "\\"  'w3-source-document)
(define-key w3-lynx-emulation-minor-mode-map "!"   'shell)
(define-key w3-lynx-emulation-minor-mode-map "'"   'ignore) ; toggle comment
(define-key w3-lynx-emulation-minor-mode-map "`"   'ignore) ; toggle comment
(define-key w3-lynx-emulation-minor-mode-map "*"   'ignore) ; toggle image_links
(define-key w3-lynx-emulation-minor-mode-map "@"   'ignore) ; toggle raw 8-bit
(define-key w3-lynx-emulation-minor-mode-map "["   'ignore) ; pseudo-inlines
(define-key w3-lynx-emulation-minor-mode-map "]"   'ignore) ; send head
(define-key w3-lynx-emulation-minor-mode-map "\""  'ignore) ; toggle quoting
(define-key w3-lynx-emulation-minor-mode-map "\C-r" 'w3-reload-document)
(define-key w3-lynx-emulation-minor-mode-map "\C-w" 'w3-refresh-buffer)
(define-key w3-lynx-emulation-minor-mode-map "\C-u" 'ignore) ; erase input
(define-key w3-lynx-emulation-minor-mode-map "\C-g" 'keyboard-quit)
(define-key w3-lynx-emulation-minor-mode-map "\C-t" 'ignore) ; toggle trace
(define-key w3-lynx-emulation-minor-mode-map "\C-k" 'ignore) ; cookie jar

;; Things to masquerade as other browsers in the user-agent field
;; of an HTTP request.
(defun w3-masquerade-stub (arg app version)
  (if (null arg)
      (setq arg (if (equal url-package-name "Emacs-W3") 1 0)))
  (if (= 0 arg)
      (setq url-package-name "Emacs-W3"
	    url-package-version w3-version-number)
    (setq url-package-name app
	  url-package-version version)))

(defun w3-lynx-masquerade-mode (&optional arg)
  (interactive "P")
  (w3-masquerade-stub arg "Lynx" "2.6"))

(defun turn-on-lynx-masquerade-mode ()
  (interactive)
  (w3-lynx-masquerade-mode 1))

(defun turn-off-lynx-masquerade-mode ()
  (interactive)
  (w3-lynx-masquerade-mode 0))

(defun w3-netscape-masquerade-mode (&optional arg)
  (interactive "P")
  (w3-masquerade-stub arg "Mozilla" "4.0"))

(defun turn-on-netscape-masquerade-mode ()
  (interactive)
  (w3-netscape-masquerade-mode 1))

(defun turn-off-netscape-masquerade-mode ()
  (interactive)
  (w3-netscape-masquerade-mode 0))

(defun w3-ie-masquerade-mode (&optional arg)
  (interactive "P")
  (w3-masquerade-stub arg "Internet_Explorer" "3.02"))

(defun turn-on-ie-masquerade-mode ()
  (interactive)
  (w3-ie-masquerade-mode 1))

(defun turn-off-ie-masquerade-mode ()
  (interactive)
  (w3-ie-masquerade-mode 0))

;;
(provide 'w3-emulate)

;;; Local Variables:
;;; truncate-lines: t
;;; End:
