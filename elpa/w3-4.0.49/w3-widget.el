;;; w3-widget.el --- An image widget

;; Copyright (c) 1996-1997, 2001, 2013 Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Created: $Date: 2002/02/01 17:42:49 $
;; Keywords: faces, images

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

;; This is a widget that will do the best it can with an image.
;;
;; It can handle all the common occurences of images on the world wide web
;; 1. A plain image - displays either a glyph of the image, or the
;;    alternative text
;; 2. A hyperlinked image - an image that is also a hypertext link to
;;    another page.  Displays either a glyph of the image, or the
;;    alternative text.  When activated with the mouse or the keyboard,
;;    the 'href' property of the widget is retrieved.
;; 3. Server side imagemaps - an image that has hotzones that lead to
;;    different areas.  Unfortunately, we cannot tell where the links go
;;    from the client - all processing is done by the server.  Displays
;;    either a glyph of the image, or the alternative text.  When activated
;;    with the mouse or the keyboard, the coordinates clicked on are
;;    sent to the remote server as HREF?x,y.  If the link is activated
;;    by the keyboard, then 0,0 are sent as the coordinates.
;; 4. Client side imagemaps - an image that has hotzones that lead to
;;    different areas.  All processing is done on the client side, so
;;    we can actually show a decent representation on a TTY.  Displays
;;    either a glyph of the image, or a drop-down-list of the destinations
;;    These are either URLs (http://foo/...) or alternative text.

;; FIXME: Current Emacs do provide pixel position of a mouse event, so
;; we should be able to make image-maps work.

;;; Code:

(require 'widget)
(require 'url-util)
(require 'w3-vars)
(autoload 'w3-fetch "w3")
(autoload 'w3-point-in-map "w3-imap")

(defvar widget-image-keymap (make-sparse-keymap)
  "Keymap used over glyphs in an image widget")

(defconst widget-mouse-button1 'mouse1)
(defconst widget-mouse-button2 'mouse2)
(defconst widget-mouse-button3 'mouse3)

(defvar widget-image-inaudible-p nil
  "*Whether to make images inaudible or not.")

(define-key widget-image-keymap (vector widget-mouse-button1)
  'widget-image-button-press)
(define-key widget-image-keymap (vector widget-mouse-button2)
  'widget-image-button-press)
  
(define-widget 'image 'default
  "A fairly complex image widget."
  :convert-widget 'widget-image-convert
  :value-to-internal (lambda (_widget value) value)
  :value-to-external (lambda (_widget value) value)
  :value-set 'widget-image-value-set
  :create 'widget-image-create
  :delete 'widget-image-delete
  :value-create 'widget-image-value-create
  :value-delete 'widget-image-value-delete
  :value-get 'widget-image-value-get
  :notify 'widget-image-notify
  )

(defun widget-image-convert (widget)
  (let ((args (widget-get widget :args)))
    (widget-put widget :args nil)
    (while args
      (widget-put widget (car args) (cadr args))
      (setq args (cddr args)))
    widget))

(defun widget-image-value-get (widget)
  (let ((children (widget-get widget :children)))
    (and (car children)
	 (widget-apply (car children) :value-get))))

(defun widget-image-create (widget)
  ;; Create an image widget at point in the current buffer
  (let ((where (widget-get widget 'where)))
    (cond
     ((null where)
      (setq where (set-marker (make-marker) (point))))
     ((markerp where)
      nil)
     ((integerp where)
      (setq where (set-marker (make-marker) where)))
     (t
      (error "IMPOSSIBLE position in widget-image-create: %s" where)))
    (widget-put widget 'where where))
  (widget-image-value-create widget))

(defun widget-image-value-set (widget value)
  ;; Recreate widget with new value.
  (save-excursion
    (widget-image-delete widget)
    (if (or (eq 'image (car-safe value)) ; Emacs 21
	    (widget-glyphp value))
	(widget-put widget 'glyph value)
      (widget-put widget :value value))
    (put-text-property (point)
		       (progn
			 (widget-apply widget :create)
			 (point))
		       'inaudible
		       widget-image-inaudible-p)))

(defsubst widget-image-usemap (widget)
  (let ((usemap (widget-get widget 'usemap)))
    (if (listp usemap)
	usemap
      (if (and usemap (> (length usemap) 0) (eq ?# (aref usemap 0)))
	  (setq usemap (substring usemap 1 nil)))
      (cdr-safe (assoc usemap w3-imagemaps)))))

(defun widget-image-callback (widget _widget-ignore &optional _event)
  (if (widget-get widget :href)
      (w3-fetch (widget-get widget :href) (widget-get widget :target))))

(defmacro widget-image-create-subwidget (&rest args)
  `(widget-create ,@args
		  :parent widget
		  :help-echo 'widget-image-summarize
		  'usemap (widget-get widget 'usemap)
		  :href href
		  :target target
		  :src (widget-get widget :src)
		  'ismap server-map))

(defun widget-image-value-create (widget)
  ;; Insert the printed representation of the value
  (let ((href (widget-get widget :href))
	(target (widget-get widget :target))
	(face (widget-get widget :button-face))
	(server-map (widget-get widget 'ismap))
	(client-map (widget-image-usemap widget))
	(where (or (widget-get widget 'where) (point)))
	(glyph (widget-get widget 'glyph))
	(alt (widget-get widget 'alt))
	(real-widget nil)
	(invalid-glyph nil))
    (if target (setq target (intern (downcase target))))

    ;; Specifier-instance will signal an error if we have an invalid
    ;; image specifier, which would be the case if we get screwed up
    ;; data back from a URL somewhere.

    (cond
     ((featurep 'xemacs)
      ;; All XEmacsen have support for glyphs
      (setq invalid-glyph (and glyph (condition-case ()
					 (if (fboundp 'specifier-instance)
					     (if (specifier-instance
						  (glyph-image glyph))
						 nil)
					   nil)
				       (error t)))))
     ((boundp 'image-types)
      ;; We are in Emacs 21+, which has image support
      (require 'image)
      (setq invalid-glyph
	    (and glyph
		 (not (image-type-available-p (plist-get (cdr glyph) :type)))))))

    (if (or (not glyph) invalid-glyph)
	;; Do a TTY or delayed image version of the image.
	(save-excursion
	  (if (= 0 (length alt)) (setq alt nil))
	  (goto-char where)
	  (cond
	   (client-map
	    (let* ((default nil)
		   (options (mapcar
			     (lambda (x)
			       (if (eq (aref x 0) 'default)
				   (setq default (aref x 2)))
			       (if (and (not default) (stringp (aref x 2)))
				   (setq default (aref x 2)))
			       (list 'choice-item
				     :tab-order -1
				     :delete 'widget-default-delete
				     :format "%[%t%]"
				     :tag (or (aref x 3) (aref x 2))
				     :value (aref x 2))) client-map)))
	      (setq real-widget
		    (apply 'widget-create 'menu-choice
			   :tag (or (widget-get widget :tag) alt "Imagemap")
			   :button-face face
			   :format "%[%t:%v%]"
			   :ignore-case t
			   :notify (widget-get widget :notify)
			   :delete 'widget-default-delete
			   :action (widget-get widget :action)
			   :value default
			   :parent widget
			   :help-echo 'widget-image-summarize
			   options))))
	   ((and server-map (stringp href))
	    (setq real-widget
		  (widget-image-create-subwidget
		   'item :format "%[%t%]"
		   :tag alt
		   :button-face face
		   :delete 'widget-default-delete
		   :value href
		   :action (widget-get widget :action)
		   :notify (widget-get widget :notify))))
	   (href
	    (setq real-widget
		  (widget-image-create-subwidget
		   'item :format "%[%t%]"
		   :tag (or alt "Image")
		   :button-face face
		   :value href
		   :delete 'widget-default-delete
		   :action (widget-get widget :action)
		   :notify 'widget-image-callback)))
	   (alt
	    (setq real-widget
		  (widget-image-create-subwidget
		   'item :format "%[%t%]"
		   :tag alt
		   :button-face face
		   :tab-order -1
		   :delete 'widget-default-delete
		   :action (widget-get widget :action)
		   :notify 'widget-image-callback))))
	  (if real-widget
	      (widget-put widget :children (list real-widget))))
      ;;; Actually use the image
      (cond
       ((featurep 'xemacs)
	(let ((extent (or (widget-get widget 'extent)
			  (make-extent where where))))
	  (set-extent-endpoints extent where where)
	  (widget-put widget 'extent extent)
	  (widget-put widget :children nil)
	  (set-extent-property extent 'keymap widget-image-keymap)
	  (set-extent-property extent 'begin-glyph glyph)
	  (set-extent-property extent 'detachable t)
	  (set-extent-property extent 'help-echo
			       (cond
				((and href (or client-map
					       server-map))
				 (format "%s [map]" href))
				(href href)
				(t nil)))
	  (set-glyph-property glyph 'widget widget)))
       ((fboundp 'insert-image)
	;; Emacs 21!
	(let ((buffer-read-only nil)
	      (after-change-functions nil)
	      (before-change-functions nil))
	  (insert-image glyph
			(propertize " "
				    'keymap widget-image-keymap
				    'help-echo (cond
						((and href (or client-map
							       server-map))
						 (format "%s [map]" href))
						(href href))))))))))

(defun widget-image-delete (widget)
  "Remove WIDGET from the buffer."
  (let ((extent (widget-get widget 'extent))
	(child  (car (widget-get widget :children))))
    (cond
     (extent				; Remove a glyph
      (if (fboundp 'delete-extent)
	  (delete-extent extent)
	(delete-overlay extent)))
     (child				; Remove a child widget
      (widget-apply child :delete))
     (t					; Doh!  Do nothing.
      nil))))     

(if (fboundp 'mouse-event-p)
    (defalias 'widget-mouse-event-p 'mouse-event-p)
  (defalias 'widget-mouse-event-p 'ignore))

(cond
 ((fboundp 'glyphp)
  (defalias 'widget-glyphp 'glyphp))
 ((boundp 'image-types)
  (defun widget-glyphp (glyph)
    (and (listp glyph) (eq 'image (car glyph)))))
 (t
  (defalias 'widget-glyphp 'ignore)))

(defun widget-image-button-press (event)
  (interactive "@e")
  (if (featurep 'xemacs)
      (let* ((glyph (and event (widget-mouse-event-p event)
			 (event-glyph event)))
	     (widget (and glyph (glyph-property glyph 'widget))))
	(widget-image-notify widget widget event))
    (save-excursion
      (mouse-set-point event)
      (let ((widget (widget-at (point))))
	(widget-image-notify widget widget event)))))    

(defun widget-image-usemap-default (usemap)
  (let ((rval (and usemap (car usemap))))
    (while usemap
      (if (equal (aref (car usemap) 0) "default")
	  (setq rval (car usemap)
		usemap nil))
      (setq usemap (cdr usemap)))
    rval))

(defun widget-image-summarize (widget)
  (if (widget-get widget :parent)
      (setq widget (widget-get widget :parent)))
  (let* ((ismap  (widget-get widget 'ismap))
	 (usemap (widget-image-usemap widget))
	 (href   (widget-get widget :href))
	 (alt    (widget-get widget 'alt))
	 (value  (widget-value widget)))
    (cond
     (usemap
      (setq usemap (widget-image-usemap-default usemap))
      ;; Perhaps we should do something here with showing the # of entries
      ;; in the imagemap as well as the default href?  Could get too long.
      (format "Client side imagemap: %s" value))
     (ismap
      (format "Server side imagemap: %s" href))
     ((stringp href)			; Normal hyperlink
      (format "Image hyperlink: %s" href))
     ((stringp alt)			; Alternate message was specified
      (format "Image: %s" alt))
     ((stringp value)
      (format "Image: %s" value))
     (t					; Huh?
      "A very confused image widget."))))

(defvar widget-image-auto-retrieve 'ask
  "*Whether to automatically retrieve the source of an image widget
if it is not an active hyperlink or imagemap.
If `nil', don't do anything.
If `t', automatically retrieve the source.
Any other value means ask the user each time.")

(defun widget-image-notify (widget widget-changed &optional event)
  ;; Happens when anything changes
  (let* ((glyph (and event (widget-mouse-event-p event) (event-glyph event)))
	 (x (and glyph (event-glyph-x-pixel event)))
	 (y (and glyph (event-glyph-y-pixel event)))
	 (ismap  (widget-get widget 'ismap))
	 (usemap (widget-image-usemap widget))
	 (href   (widget-get widget :href))
	 (img-src (or (widget-get widget :src)
		      (and widget-changed (widget-get widget-changed :src))))
	 (target (widget-get widget :target))
	 )
    (if target (setq target (intern (downcase target))))
    (cond
     ((and glyph usemap)		; Do the client-side imagemap stuff
      (setq href (w3-point-in-map (vector x y) usemap nil))
      (if (stringp href)
	  (w3-fetch href target)
	(message "No destination found for %d,%d" x y)))
     ((and glyph x y ismap)		; Do the server-side imagemap stuff
      (w3-fetch (format "%s?%d,%d" href x y) target))
     (usemap				; Dumbed-down tty client side imap
      (let ((choices (mapcar (lambda (entry)
			       (cons
				(or (aref entry 3) (aref entry 2))
				(aref entry 2)))
			     usemap))
	    (choice nil)
	    (case-fold-search t))
	(setq choice (completing-read "Imagemap: " choices nil t)
	      choice (cdr-safe (assoc choice choices)))
	(and (stringp choice) (w3-fetch choice target))))
     (ismap				; Do server-side dummy imagemap for tty
      (w3-fetch (concat href "?0,0") target))
     ((stringp href)			; Normal hyperlink
      (w3-fetch href target))
     ((stringp img-src)
      (cond
       ((null widget-image-auto-retrieve) nil)
       ((eq t widget-image-auto-retrieve)
	(w3-fetch img-src))
       ((funcall url-confirmation-func
		 (format "Retrieve image (%s)?"
			 (url-truncate-url-for-viewing img-src)))
	(w3-fetch img-src))))
     (t					; Huh?
      nil))))

(provide 'w3-widget)
