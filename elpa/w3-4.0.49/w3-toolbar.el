;;; w3-toolbar.el --- Toolbar functions for emacs-w3

;; Copyright (c) 1996-1997, 2013 Free Software Foundation, Inc.

;; Author: William M. Perry <wmperry@gnu.org>
;; Created: $Date: 2001/07/19 14:15:52 $
;; Keywords: mouse, toolbar

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

;; Toolbar specific function for XEmacs and Emacs 21

;;; Code:

(require 'w3-vars)

(condition-case ()
    (progn
      (require 'xpm-button)
      (require 'xbm-button))
  (error nil))

(defvar w3-toolbar-icon-directory nil
  "Where the toolbar icons for W3 are.
In Emacs, this is searched preferentially to the normal search path.")
(defvar w3-toolbar-back-icon (if (featurep 'tool-bar)
				 "left_arrow")
  "Toolbar icon for back")
(defvar w3-toolbar-forw-icon (if (featurep 'tool-bar)
				 "right_arrow")
  "Toolbar icon for forward")
(defvar w3-toolbar-home-icon (if (featurep 'tool-bar)
				 "home")
  "Toolbar icon for home")
(defvar w3-toolbar-reld-icon (if (featurep 'tool-bar)
				 "refresh")
  "Toolbar icon for reload")
(defvar w3-toolbar-imag-icon (if (featurep 'tool-bar)
				 "images")
  "Toolbar icon for images")
(defvar w3-toolbar-open-icon (if (featurep 'tool-bar)
				 "open")
  "Toolbar icon for open url")
(defvar w3-toolbar-print-icon (if (featurep 'tool-bar)
				  "print")
  "Toolbar icon for printing")
(defvar w3-toolbar-find-icon (if (featurep 'tool-bar)
				 "search")
  "Toolbar icon for find")
(defvar w3-toolbar-stop-icon (if (featurep 'tool-bar)
				 "cancel")
  "Toolbar icon for stop")
(defvar w3-toolbar-help-icon (if (featurep 'tool-bar)
				 "help")
  "Toolbar icon for help")
(defvar w3-toolbar-hotl-icon (if (featurep 'tool-bar)
				 "jump_to")
  "Toolbar icon for hotlist")
(defvar w3-toolbar-file-icon (if (featurep 'tool-bar)
				 "new")
  "Toolbar icon for open url")
(defvar w3-toolbar-printer-icon (if (featurep 'tool-bar)
				 "print")
  "Toolbar icon for open url")

(defvar w3-link-toolbar-orientation 'bottom
  "*Where to put the document specific toolbar.  Must be one of these symbols:

default -- place at location specified by `default-toolbar-position'
top     -- place along the top of the frame
bottom  -- place along the bottom of the frame
right   -- place along the right edge of the frame
left    -- place along the left edge of the frame
none    -- no toolbar")

(defvar w3-toolbar-orientation 'default
  "*Where to put the w3 toolbar.  Must be one of these symbols:

default -- place at location specified by `default-toolbar-position'
top     -- place along the top of the frame
bottom  -- place along the bottom of the frame
right   -- place along the right edge of the frame
left    -- place along the left edge of the frame
none    -- no toolbar")

(defvar w3-toolbar-type 'both
  "*What the toolbar looks like.  Must be one of these symbols:

pictures -- Show icons (without captions if in XEmacs 19.13)
both     -- Show icons (with captions if in XEmacs 19.13)
text     -- Show only text buttons

Only has any meaning in XEmacs 19.12 when w3-toolbar-orientation is
not `none'.")

(defvar w3-toolbar
  '([w3-toolbar-back-icon w3-history-backward (car (w3-history-find-url-internal (url-view-url t))) "Back in history"]
    [w3-toolbar-forw-icon w3-history-forward (cdr (w3-history-find-url-internal (url-view-url t))) "Forward in history"]
    [w3-toolbar-home-icon w3 t "Go home"]
    [:style 2d :size 5]
    [w3-toolbar-reld-icon w3-reload-document t "Reload document"]
    [w3-toolbar-hotl-icon w3-hotlist-view t "View hotlist"]
    [w3-toolbar-imag-icon w3-load-delayed-images w3-delayed-images
			  "Load images"]
    [toolbar-file-icon w3-fetch t "Fetch a URL"]
    [toolbar-printer-icon w3-mouse-print-this-url t "Print document"]
    [w3-toolbar-find-icon w3-search-forward t "Search"]
    ;;[w3-toolbar-stop-icon keyboard-quit t "Stop transaction"]
    nil
    [w3-toolbar-help-icon w3-show-info-node t "Help"])
  "The toolbar for w3")

(defun w3-toolbar-make-captioned-buttons ()
  (mapcar
   (function
    (lambda (x)
      (let* ((ext (if (featurep 'xpm) ".xpm" ".xbm"))
	     (base w3-toolbar-icon-directory)
	     (up (expand-file-name (concat x "-up" ext) base))
	     (dn (expand-file-name (concat x "-dn" ext) base))
	     (no (expand-file-name (concat x "-no" ext) base))
	     (cap-up (expand-file-name (concat x "-cap-up" ext) base))
	     (cap-dn (expand-file-name (concat x "-cap-dn" ext) base))
	     (cap-no (expand-file-name (concat x "-cap-no" ext) base))
	     (var (intern (concat "w3-toolbar-" x "-icon"))))
	(set var
	     (toolbar-make-button-list up dn no cap-up cap-dn cap-no)))))
   
   '("back" "help" "find" "forw" "home"  "hotl" "stop" "imag" "reld")))

(defun w3-make-text-toolbar-button (text)
  (let ((bgcol (or
		(cdr-safe (assq 'background-toolbar-color (frame-parameters)))
		"#befbbefbbefb")))
    (if (featurep 'xpm)
	(mapcar 'make-glyph (xpm-button-create text 0 "black" bgcol))
      (xbm-button-create text 0))))

(defun w3-toolbar-make-text-buttons ()
  (setq w3-toolbar-back-icon (w3-make-text-toolbar-button "Back")
        w3-toolbar-forw-icon (w3-make-text-toolbar-button "Forward")
        w3-toolbar-home-icon (w3-make-text-toolbar-button "Home")
        w3-toolbar-reld-icon (w3-make-text-toolbar-button "Reload")
        w3-toolbar-hotl-icon (w3-make-text-toolbar-button "Hotlist")
        w3-toolbar-imag-icon (w3-make-text-toolbar-button "Images")
        w3-toolbar-open-icon (w3-make-text-toolbar-button "Open")
        w3-toolbar-print-icon (w3-make-text-toolbar-button "Print")
        w3-toolbar-find-icon (w3-make-text-toolbar-button "Find")
        w3-toolbar-help-icon (w3-make-text-toolbar-button "Help!")))

(defun w3-toolbar-make-picture-buttons ()
  (mapcar
   (function
    (lambda (x)
      (let* ((ext (if (featurep 'xpm) ".xpm" ".xbm"))
	     (base w3-toolbar-icon-directory)
	     (up (expand-file-name (concat x "-cap-up" ext) base))
	     (dn (expand-file-name (concat x "-cap-dn" ext) base))
	     (no (expand-file-name (concat x "-cap-no" ext) base))
	     (var (intern (concat "w3-toolbar-" x "-icon"))))
	(set var
	     (cond
	      ((and (file-exists-p up) (file-exists-p dn)
		    (file-exists-p no))
	       (toolbar-make-button-list up dn no))
	      ((file-exists-p up)
	       (toolbar-make-button-list up))
	      (t nil))))))
   '("back" "help" "find" "forw" "home" "hotl" "imag" "reld")))

(defun w3-toolbar-make-buttons ()
  (if (not w3-toolbar-icon-directory)
      (setq w3-toolbar-icon-directory
	    (if (fboundp 'locate-data-directory)
		(locate-data-directory "w3")
	      (file-name-as-directory
	       (expand-file-name "w3" data-directory)))))
  (condition-case nil
      (cond
       ((not (fboundp 'toolbar-make-button-list))
	nil)
       ((or (eq w3-toolbar-type 'text)
	    (null w3-toolbar-icon-directory)
	    (not (file-directory-p w3-toolbar-icon-directory)))
	(w3-toolbar-make-text-buttons))
       ((boundp 'toolbar-buttons-captioned-p)
	(w3-toolbar-make-captioned-buttons))
       ((featurep 'tool-bar)
	nil)
       ;; Fixme: Redundant?  XEmacs versions supported have captioned
       ;; buttons.
       (t
	(w3-toolbar-make-picture-buttons)))
    (error nil)))

(defun w3-link-is-defined (rel &optional rev)
  (or
   (cdr-safe (assoc rel (cdr-safe (assq 'rel w3-current-links))))
   (cdr-safe (assoc (or rev rel) (cdr-safe (assq 'rev w3-current-links))))))

;; Need to create w3-toolbar-glos-icon
;;                w3-toolbar-toc-icon
;;                w3-toolbar-copy-icon
(defvar w3-link-toolbar
  '([info::toolbar-prev-icon
     (w3-fetch (w3-link-is-defined "previous" "next"))
     (w3-link-is-defined "previous" "next")
     "Back"]
    [info::toolbar-next-icon
     (w3-fetch (w3-link-is-defined "next" "previous"))
     (w3-link-is-defined "next" "previous")
     "Next"]
    [info::toolbar-up-icon
     (w3-fetch (w3-link-is-defined "up" "down"))     
     (w3-link-is-defined "up" "down")
     "Up"]
    [w3-toolbar-home-icon
     (w3-fetch (w3-link-is-defined "home"))
     (w3-link-is-defined "home")
     "Home"]
    [w3-toolbar-toc-icon
     (w3-fetch (w3-link-is-defined "toc"))
     (w3-link-is-defined "toc")
     "Contents"]
    [w3-toolbar-find-icon
     (w3-fetch (w3-link-is-defined "index"))
     (w3-link-is-defined "index")
     "Index"]
    [w3-toolbar-glos-icon
     (w3-fetch (w3-link-is-defined "glossary"))
     (w3-link-is-defined "glossary")
     "Glossary"]
    [w3-toolbar-copy-icon
     (w3-fetch (w3-link-is-defined "copyright"))
     (w3-link-is-defined "copyright")
     "Copyright"]
    [w3-toolbar-hotl-icon
     (w3-fetch (w3-link-is-defined "bookmark"))
     (w3-link-is-defined "bookmark")
     "Bookmarks"]
    nil
    [w3-toolbar-help-icon
     (w3-fetch (w3-link-is-defined "help"))
     (w3-link-is-defined "help")
     "Help"]
    ))

(defun w3-toolbar-from-orientation (orientation)
  (if (featurep 'xemacs)
      (cond
       ((eq 'default orientation) default-toolbar)
       ((eq 'bottom orientation) bottom-toolbar)
       ((eq 'top orientation) top-toolbar)
       ((eq 'left orientation) left-toolbar)
       ((eq 'right orientation) right-toolbar))
    (error "Unimplemented")))

;; (defun w3-toolbar-dimension-from-orientation (orientation)
;;   (cond
;;    ((eq 'default w3-toolbar-orientation) nil)
;;    ((eq 'bottom w3-toolbar-orientation) bottom-toolbar-height)
;;    ((eq 'top w3-toolbar-orientation) top-toolbar-height)
;;    ((eq 'left w3-toolbar-orientation) left-toolbar-width)
;;    ((eq 'right w3-toolbar-orientation) right-toolbar-width)))

;; (defun w3-ensure-toolbar-visible (orientation)
;;   ;; Make sure a certain toolbar is visible if necessary
;;   ;; This can modify frame parameters, so watch out.
;;   (let ((dimension (w3-toolbar-dimension-from-orientation orientation))
;; 	(toolbar   (w3-toolbar-from-orientation orientation))
;; 	(dimensions nil)
;; 	(widths nil)
;; 	(heights nil)
;; 	(needs nil)
;; 	(has nil))
;;     (if (and dimension toolbar
;; 	     (setq toolbar (specifier-instance toolbar)))
;; 	(progn
;; 	  (setq dimensions (mapcar
;; 			    (function
;; 			     (lambda (glyph)
;; 			       (and (glyphp glyph)
;; 				    (cons (glyph-width glyph)
;; 					  (glyph-height glyph)))))
;; 			    (mapcar 'car
;; 				    (delq nil
;; 					  (mapcar
;; 					   (function (lambda (x)
;; 						       (and x
;; 							    (symbol-value
;; 							     (aref x 0)))))
;; 					   toolbar))))
;; 		widths (sort (mapcar 'car dimensions) '>=)
;; 		heights (sort (mapcar 'cdr dimensions) '>=)
;; 		needs (+ 7 (if (memq orientation '(top bottom))
;; 			      (car heights)
;; 			    (car widths)))
;; 		has (specifier-instance dimension))
;; 	  (if (<= has needs)
;; 	      (set-specifier dimension (cons (selected-frame) needs)))))))
			     
(defun w3-toolbar-active ()
  (interactive)
  (if (featurep 'xemacs)
      (let ((toolbar (w3-toolbar-from-orientation w3-toolbar-orientation)))
        (and toolbar (specifier-instance toolbar)))
    (error "Unimplemented")))

;; (defun w3-toggle-link-toolbar ()
;;   (interactive)
;;   (require 'info)			; For some toolbar buttons
;;   (let* ((w3-toolbar-orientation w3-link-toolbar-orientation)
;; 	 (toolbar (w3-toolbar-from-orientation w3-toolbar-orientation)))
;;     (if toolbar
;; 	(if (w3-toolbar-active)
;; 	    (set-specifier toolbar (cons (current-buffer) nil))
;; 	  (set-specifier toolbar w3-link-toolbar (current-buffer))))))

(defun w3-toggle-toolbar ()
  (interactive)
  (cond
   ((featurep 'xemacs)
    (if (eq major-mode 'w3-mode)
        (let ((toolbar (w3-toolbar-from-orientation w3-toolbar-orientation)))
          (cond
           ((w3-toolbar-active)
            (set-specifier toolbar (cons (current-buffer) nil)))
           (toolbar
            (set-specifier toolbar (cons (current-buffer) w3-toolbar)))
           (t
            (setq w3-toolbar-orientation 'default
                  toolbar (w3-toolbar-from-orientation w3-toolbar-orientation))
            (and toolbar
                 (set-specifier toolbar (cons (current-buffer) w3-toolbar))))))
      (setq w3-toolbar-orientation (if (not (eq w3-toolbar-orientation 'none))
                                       'none
                                     'default))))
   (t (error "Unimplemented"))))

(defun w3-show-info-node ()
  (interactive)
  (Info-goto-node "(w3.info)Top"))

(defun w3-mouse-print-this-url (&optional e)
  (interactive "e")
  (let ((descr '("Print document as"
		 ["PostScript" (w3-print-this-url nil "PostScript") t]
		 ["Formatted Text" (w3-print-this-url nil "Formatted Text") t]
		 ["HTML Source" (w3-print-this-url nil "HTML Source") t]
		 "--"
		 ["Cancel" (beep) t])))
    (if (fboundp 'popup-dialog-box)
        (popup-dialog-box descr)
      (popup-menu descr e))))

(defvar w3-toolbar-map
  (if (and (featurep 'tool-bar)
	   (display-graphic-p))		; would lose on tty
      (progn
	(if (not w3-toolbar-icon-directory)
	    (setq w3-toolbar-icon-directory
		  (file-name-as-directory
		   (expand-file-name "w3" data-directory))))
	(let ((tool-bar-map (make-sparse-keymap))
	      ;; Add to normal image search path:
	      (load-path (cons w3-toolbar-icon-directory load-path)))
	  (dolist (desc w3-toolbar)
	    (when desc
	      (let ((sym (aref desc 0)))
		;; w3-toolbar contains `toolbar-' symbols as well as
		;; `w3-toolbar-'.
		(unless (boundp sym)
		  (setq sym (intern (format "w3-%s" sym))))
		(if (and desc (not (keywordp (aref desc 0))))
		    (tool-bar-add-item (symbol-value sym) ; image
				       (aref desc 1) ; binding
				       (intern (aref desc 3)) ; key
				       :help (aref desc 3)
				       :enable (aref desc 2))))))
	  tool-bar-map))))

(defun w3-add-toolbar-to-buffer ()
  (cond
   ((featurep 'infodock)
    ;; Infodock handles toolbars differently
    nil)
   ((featurep 'xemacs)
    ;; XEmacs way of doing things
    (let ((toolbar (w3-toolbar-from-orientation w3-toolbar-orientation)))
      (if toolbar
	  (set-specifier toolbar (cons (current-buffer) w3-toolbar))))
    (set-specifier toolbar-buttons-captioned-p
		   (cons (current-buffer) (eq w3-toolbar-type 'both))))
   (t
    ;; Emacs 21.x way of doing things
    (set (make-local-variable 'tool-bar-map) w3-toolbar-map))
   (t
    nil)))

(provide 'w3-toolbar)
