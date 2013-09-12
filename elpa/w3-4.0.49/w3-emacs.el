;;; w3-emacs.el --- Emacs-specific functions for emacs-w3

;; Copyright (c) 1997, 1998, 2001, 2013 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Emacs-specific initialization code for W3.

;;; Code:

(eval-when-compile (require 'cl))
(require 'w3-props)
(autoload 'tooltip-show "tooltip")

(defun w3-setup-version-specifics ()
  (when (featurep 'tool-bar)
    (require 'w3-toolbar)
    (w3-toolbar-make-buttons)
    (w3-add-toolbar-to-buffer)
    (add-hook 'tooltip-hook 'w3-tooltip-get-tips)))

(defvar w3-emacs-window-width nil)
(make-variable-buffer-local 'w3-emacs-window-width)
;; Unused.  Presumably meant for `window-size-change-functions'.
(defun w3-window-size-change-function (frame)
  (let ((first (frame-first-window frame))
	(cur nil))
    (while (not (eq cur first))
      (setq cur (if cur (next-window cur nil frame) first))
      (with-current-buffer (window-buffer cur)
	(if (and (derived-mode-p 'w3-mode)
		 (not (eq (window-width cur) w3-emacs-window-width)))
	    (w3-refresh-buffer))))))

(defvar w3-face-index)
(defvar w3-display-background-properties)

(defun w3-mode-version-specifics ()
  (setq w3-emacs-window-width (window-width))
  (if w3-display-background-properties
      (let ((face (w3-make-face (intern
				 (format "w3-style-face-%05d" w3-face-index))
				"An Emacs-W3 face... don't edit by hand." t))
	    (fore (car w3-display-background-properties))
	    (inhibit-read-only t)
	    (back (cdr w3-display-background-properties)))
	(setq w3-face-index (1+ w3-face-index))
	(if fore (font-set-face-foreground face fore))
	(if back (font-set-face-background face back))
	(w3-fillin-text-property (point-min) (point-max) 'face 'face face)))
  (when (featurep 'tool-bar)
    (w3-add-toolbar-to-buffer)))

(defun w3-tooltip-get-tips (event)
  (let (widget pos help start)
    (setq start (event-start event)
	  pos (posn-point start)
	  widget (and pos (widget-at pos))
	  help (and widget (widget-get widget :help-echo)))
    (if (functionp help)
	(setq help (funcall help widget (posn-window start)
			    (window-buffer (posn-window start))
			    (posn-point start))))
    (if (stringp help)
	(tooltip-show help))))

(provide 'w3-emacs)
;;; w3-emacs.el ends here
