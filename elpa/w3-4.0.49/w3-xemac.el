;;; w3-xemac.el --- XEmacs specific functions for emacs-w3

;; Copyright (c) 1996 - 1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: legoscia $
;; Created: $Date: 2006/10/17 20:24:48 $
;; Keywords: faces, help, mouse, hypermedia

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

(require 'w3-imap)
(require 'images)
(require 'w3-widget)
(require 'w3-menu)
(require 'w3-forms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancements For XEmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-mouse-handler (e)
  "Function to message the url under the mouse cursor"
  (interactive "e")
  (if (equal (current-buffer) (event-buffer e))
      (let* ((pt (event-point e))
	     (good (eq (event-window e) (selected-window)))
	     (mouse-events))
	(if (not (and good pt (number-or-marker-p pt)))
	    nil
	  (if (and inhibit-help-echo w3-track-mouse)
	      (widget-echo-help pt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to build menus of urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-setup-version-specifics ()
  "Set up routine for XEmacs 19.12 or later"
  ;; Create the toolbar buttons
  (and (featurep 'toolbar)
       (w3-toolbar-make-buttons))

  ;; Register the default set of image conversion utilities
  (image-register-netpbm-utilities)

  ;; Add our menus, but make sure that we do it to the global menubar
  ;; not the current one, which could be anything, but usually GNUS or
  ;; VM if not the default.
  ;;
  ;; InfoDock handles all of this for us, so we don't need to worry about
  ;; it there.
  (if (and (featurep 'menubar) (not (featurep 'infodock)))
      (let ((current-menubar (default-value 'current-menubar)))
	(if current-menubar
	    (add-submenu '("Help") (cons "WWW" (cdr w3-menu-help-menu))))))

  ;; FIXME FIXME: Do sexy things to the default modeline for Emacs-W3
  
  ;; The following is a workaround for XEmacs 19.14 and XEmacs 20.0
  ;; The text property implementation is badly broken - you could not have
  ;; a text property with a `nil' value.  Bad bad bad.
  (if (or (and (= emacs-major-version 20)
	       (= emacs-minor-version 0))
	  (and (= emacs-major-version 19)
	       (= emacs-minor-version 14)))
      (defun text-prop-extent-paste-function (ext from to)
	(let ((prop (extent-property ext 'text-prop nil))
	      (val nil))
	  (if (null prop)
	      (error "Internal error: no text-prop"))
	  (setq val (extent-property ext prop nil))
	  (put-text-property from to prop val nil)
	  nil))
    )
  )

(defun w3-mode-motion-hook (e)
  (let* ((glyph  (event-glyph e))
	 (x      (and glyph (event-glyph-x-pixel e)))
	 (y      (and glyph (event-glyph-y-pixel e)))
	 (widget (and glyph (glyph-property glyph 'widget)))
	 (usemap (and widget (w3-image-widget-usemap widget)))
	 (ismap  (and widget (widget-get widget 'ismap)))
	 (echo   (and widget (widget-get widget 'href))))
    (cond
     (usemap
      (setq echo (w3-point-in-map (vector x y) usemap t)))
     (ismap
      (setq echo (format "%s?%d,%d" echo x y)))
     (t
      nil))
    (and echo (message "%s" echo))))

(defun w3-mode-version-specifics ()
  "XEmacs specific stuff for w3-mode"
  (if (featurep 'mouse)
      (progn
	(if (not w3-track-mouse)
	    (setq inhibit-help-echo nil))
	(setq mode-motion-hook 'w3-mouse-handler)))
  (case (device-type)
    ((tty stream)			; TTY or batch
     nil)
    (otherwise
     (w3-add-toolbar-to-buffer)))
  (setq mode-popup-menu w3-popup-menu))

;; Some old versions of XEmacs have a dumped version of the widget and
;; custom libraries that are too old for us to use, so we have to reload
;; the new ones.
(if (or (string-match "19\\.1[3456]" emacs-version)
	(string-match "20\\.[0123]" emacs-version))
    (mapc 'load '("wid-edit" "widget" "custom" "cus-edit" "cus-face"
		  "cus-start" "cus-load")))

(if (string-match "19\\.13" emacs-version)
    ;; This is all from Vladimir Alexiev <vladimir@cs.ualberta.ca>
    (progn
      (require 'advice)
      (declare (special widget-use-overlay-change))
      (defadvice add-text-properties (before face-list activate)
	"Allow a list of faces (ignore all but first one)."
	(let ((faces (plist-get props 'face)))
	  (if (listp faces)
	      (plist-put props 'face (car faces)))))

      (defadvice set-extent-property (before face-list activate)
	"Allow a list of faces (ignore all but first one)."
	(if (and (eq property 'face) (listp value))
	    (setq value (car value))))

      (defadvice insert-char (before inherit-ignore 
				     (char &optional count inherit buffer)
				     activate)
	"Accept and ignore a third arg INHERIT."
	(or (bufferp inherit) (setq inherit buffer)))


      (defun font-height (font) 1)
      (defun font-width (font)	1)	; works for tty
      (defalias 'get-char-property 'get-text-property)
      (defalias 'extent-object 'extent-buffer)
      (defvar shell-command-switch "-c") ; for mm

      ;; for x-overlay
      (defun extent-list (&optional buffer from to)
	"Return a list of all extents in BUFFER between FROM and TO 
\(see mapcar-extents\)."
	(mapcar-extents 'identity nil buffer from to))

      ;; for custom/widget
      (setq widget-use-overlay-change nil) ; slower, but works

      (defadvice add-hook (before ignore 
				  (hook function &optional append ignore)
				  activate)
	"Accept and ignore a fourth argument.")

      (defadvice remove-hook (before ignore 
				     (hook function &optional ignore) activate)
	"Accept and ignore a third argument.")

      (defun valid-image-instantiator-format-p (format)
	(valid-instantiator-p
	 (vector format :file (concat "foo." (symbol-name format)))
	 'image))

      (defadvice make-sparse-keymap (before ignore (&optional name) activate)
	"Accept and ignore an optional arg NAME.")
      ))

(require 'w3-toolbar)
(provide 'w3-xemacs)
(provide 'w3-xemac)
