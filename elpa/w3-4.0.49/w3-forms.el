;;; w3-forms.el --- Emacs-w3 forms parsing code for new display engine

;; Copyright (c) 1996-1999, 2008, 2013 Free Software Foundation, Inc.

;; Author: $Author: wmperry $
;; Created: $Date: 2002/10/23 03:33:41 $
;; Keywords: faces, help, comm, data, languages

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

;; FORMS processing for HTML.

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'w3-mouse)
  (require 'w3-display)
  (require 'url)
  (require 'url-util)
  (require 'widget)
  (require 'wid-edit))

(require 'w3-vars)
(autoload 'custom-magic-reset "cus-edit")
(autoload 'w3-warn "w3")

(defvar w3-form-use-old-style nil
  "*Non-nil means use the old way of interacting for form fields.")

(defvar w3-form-keymap
  ;; Fixme: Why doesn't this use inheritance?  -- fx
  (let ((map (copy-keymap global-map))
	(eol-loc (where-is-internal 'end-of-line global-map t)))
    (substitute-key-definition 'widget-backward 'w3-widget-backward
			       map widget-keymap)
    (substitute-key-definition 'widget-forward 'w3-widget-forward
			       map widget-keymap)
    (define-key map [return]      'w3-form-maybe-submit-by-keypress)
    (define-key map "\r"          'w3-form-maybe-submit-by-keypress)
    (define-key map "\n"          'w3-form-maybe-submit-by-keypress)
    (define-key map "\t"          'w3-widget-forward)
    (define-key map "\C-k"        'widget-kill-line)
    (define-key map "\C-a"        'widget-beginning-of-line)
    (define-key map (vector w3-mouse-button3) 'w3-popup-menu)
    (if eol-loc
	(define-key map eol-loc   'widget-end-of-line))
    map))

;; A form entry area is a vector
;; [ type name default-value value maxlength options widget plist]
;; Where:
;;          type = symbol defining what type of form entry area it is
;;                 (ie: file, radio)
;;          name = the name of the form element
;; default-value = the value this started out with

(defsubst w3-form-element-type          (obj) (aref obj 0))
(defsubst w3-form-element-name          (obj) (aref obj 1))
(defsubst w3-form-element-default-value (obj) (aref obj 2))
(defsubst w3-form-element-value         (obj) (aref obj 3))
(defsubst w3-form-element-size          (obj) (aref obj 4))
(defsubst w3-form-element-maxlength     (obj) (aref obj 5))
(defsubst w3-form-element-options       (obj) (aref obj 6))
(defsubst w3-form-element-action        (obj) (aref obj 7))
(defsubst w3-form-element-widget        (obj) (aref obj 8))
(defsubst w3-form-element-plist         (obj) (aref obj 9))

(defsubst w3-form-element-set-type          (obj val) (aset obj 0 val))
(defsubst w3-form-element-set-name          (obj val) (aset obj 1 val))
(defsubst w3-form-element-set-default-value (obj val) (aset obj 2 val))
(defsubst w3-form-element-set-value         (obj val) (aset obj 3 val))
(defsubst w3-form-element-set-size          (obj val) (aset obj 4 val))
(defsubst w3-form-element-set-maxlength     (obj val) (aset obj 5 val))
(defsubst w3-form-element-set-options       (obj val) (aset obj 6 val))
(defsubst w3-form-element-set-action        (obj val) (aset obj 7 val))
(defsubst w3-form-element-set-widget        (obj val) (aset obj 8 val))
(defsubst w3-form-element-set-plist         (obj val) (aset obj 9 val))

(defvar w3-form-valid-key-sizes
  '(
    ("1024 (Premium)" . 1024)
    ("896 (Regular)" . 896)
    ("768 (Unleaded)" . 768)
    ("512 (Low Grade)" . 512)
    ("508 (Woos)" . 508)
    ("256 (Test Grade)" . 256)
    )
  "An assoc list of available key sizes and meaningful descriptions.")
   
(defun w3-form-determine-size (el size)
  (if (equal size 0)
      (setq size nil))
  (case (w3-form-element-type el)
    (checkbox 3)
    (radio 4)
    ((reset submit) (+ 2 (length (or (w3-form-element-value el)
				     (symbol-name
				      (w3-form-element-type el))))))
    (multiline 21)
    (hidden nil)
    (file (or size 26))
    ((float password text int)
     (if w3-form-use-old-style
	 (+ 2 (or size 20))
       (1+ (or size 19))))
    (image (+ 2 (length (or
			 (plist-get (w3-form-element-plist el) 'alt)
			 "Form-Image"))))
    (option
     (let ((options (copy-sequence (w3-form-element-options el))))
       (length (caar (sort options
			   (function
			    (lambda (x y)
			      (>= (length (car x))
				  (length (car y))))))))))
    (keygen
     (+ (length "Key Length: ")
	(apply 'max
	       (mapcar (function (lambda (pair)
				   (length (car pair))))
		       w3-form-valid-key-sizes))))
    (otherwise (or size 22))))
 
;;;###autoload
(defun w3-form-add-element (plist face)
  (let* ((action (plist-get plist 'action))
	 (el (vector (plist-get plist 'type)
		     (plist-get plist 'name)
		     (plist-get plist 'default)
		     (plist-get plist 'value)
		     (plist-get plist 'size)
		     (plist-get plist 'maxlength)
		     (plist-get plist 'options)
		     action
		     nil
		     plist))
	 (size (w3-form-determine-size el (plist-get plist 'size)))
	 (node (assoc action w3-form-elements)))
    (if (not (assq '*table-autolayout w3-display-open-element-stack))
 	(if node
 	    (setcdr node (cons el (cdr node)))
 	  (setq w3-form-elements (cons (cons action (list el))
 				       w3-form-elements))))
    (if size
	(set-text-properties (point)
			     (progn (insert-char ?T size) (point))
			     (list 'w3-form-info (cons el face)
				   'start-open t
				   'end-open t
				   'rear-nonsticky t)))))

(defvar widget-push-button-gui)

;;;###autoload
(defun w3-form-resurrect-widgets ()
  (let ((st (point-min))
	;; FIXME! For some reason this loses on long lines right now.
	(widget-push-button-gui nil)
	info nd action face)
    (while st
      (when (setq info (get-text-property st 'w3-form-info))
        (setq nd (or (next-single-property-change st 'w3-form-info)
                     (point-max))
              face (cdr info)
              info (car info)
              action (w3-form-element-action info))
        (goto-char st)
        (delete-region st nd)
        (if (not (w3-form-element-size info))
            (w3-form-element-set-size info 20))
        (w3-form-add-element-internal info face))
      (setq st (next-single-property-change st 'w3-form-info)))))

(defsubst w3-form-mark-widget (widget el)
  (let ((widgets (list widget))
	(children (widget-get widget :children)))
    (w3-form-element-set-widget el widget)
    ;; Get _all_ the children associated with this widget
    (while children
      (setq widgets (cons (car children) widgets))
      (if (widget-get (car children) :children)
	  (setq children (append children
				 (widget-get (car children) :children))))
      (setq children (cdr children)))
    (while (widget-get widget :parent)
      (setq widget (widget-get widget :parent)
	    widgets (cons widget widgets)))
    (setq children (widget-get widget :buttons))
    ;; Special case for radio buttons
    (while children
      (setq widgets (cons (car children) widgets))
      (if (widget-get (car children) :children)
	  (setq children (append children
				 (widget-get (car children) :children))))
      (setq children (cdr children)))
    (while widgets
      (setq widget (pop widgets))
      (widget-put widget :emacspeak-help 'w3-form-summarize-field)
      (widget-put widget :help-echo 'w3-form-summarize-field)
      (widget-put widget :w3-form-data el))))

(defun w3-form-add-element-internal (el face)
  (let* ((widget nil)
	 (buffer-read-only nil)
	 (inhibit-read-only t)
	 (widget-creation-function nil))
    (setq widget-creation-function (or (get (w3-form-element-type el)
					    'w3-widget-creation-function)
				       'w3-form-default-widget-creator)
	  widget (and (fboundp widget-creation-function)
		      (funcall widget-creation-function el face)))
    (if (not widget)
	nil
      (w3-form-mark-widget widget el))))

;; These properties tell the add-element function how to actually create
;; each type of widget.
(put 'checkbox  'w3-widget-creation-function 'w3-form-create-checkbox)
(put 'multiline 'w3-widget-creation-function 'w3-form-create-multiline)
(put 'radio     'w3-widget-creation-function 'w3-form-create-radio-button)
(put 'reset     'w3-widget-creation-function 'w3-form-create-submit-button)
(put 'submit    'w3-widget-creation-function 'w3-form-create-submit-button)
(put 'hidden    'w3-widget-creation-function 'ignore)
(put 'file      'w3-widget-creation-function 'w3-form-create-file-browser)
(put 'option    'w3-widget-creation-function 'w3-form-create-option-list)
(put 'keygen    'w3-widget-creation-function 'w3-form-create-keygen-list)
(put 'button    'w3-widget-creation-function 'w3-form-create-button)
(put 'image	'w3-widget-creation-function 'w3-form-create-image)
(put 'int       'w3-widget-creation-function 'w3-form-create-integer)
(put 'float     'w3-widget-creation-function 'w3-form-create-float)
(put 'custom    'w3-widget-creation-function 'w3-form-create-custom)
(put 'text      'w3-widget-creation-function 'w3-form-create-text)
(put 'password  'w3-widget-creation-function 'w3-form-create-password)

;; Custom support.
(defvar w3-custom-options nil)
(make-variable-buffer-local 'w3-custom-options)

(defun w3-form-create-custom (el _face)
  (require 'cus-edit)
  (let* ((var-name (w3-form-element-value el))
	 (type (plist-get (w3-form-element-plist el) 'custom-type))
	 (widget (widget-create (cond ((string-equal type "variable")
				       'custom-variable)
				      ((string-equal type "face")
				       'custom-face)
				      ((string-equal type "group")
				       'custom-group)
				      (t 'item)) (intern var-name))))
    (custom-magic-reset widget)
    (push widget w3-custom-options)
    widget))

(defun w3-form-create-checkbox (el face)
  (widget-create 'checkbox
		 :button-face face
		 (and (w3-form-element-default-value el) t)))

(defun w3-form-radio-button-update (widget child event)
  (widget-radio-action widget child event)
  (w3-form-mark-widget widget (widget-get widget :w3-form-data)))

(defun w3-form-create-radio-button (el face)
  (let* ((name (w3-form-element-name el))
	 (action (w3-form-element-action el))
	 (uniqid (cons name action))
	 (formobj (cdr (assoc uniqid w3-form-radio-elements)))
	 (widget nil)
	 )
    (if formobj
	(progn
	  (setq widget (w3-form-element-widget formobj))
	  (widget-radio-add-item widget
				 (list 'item
				       :button-face face
				       :format "%t"
				       :tag ""
				       :value (w3-form-element-value el)))
	  (w3-form-mark-widget widget el)
	  (if (w3-form-element-default-value el)
	      (progn
		(widget-put widget 'w3-form-default-value
			    (w3-form-element-value el))
		(widget-value-set widget (w3-form-element-value el))))
	  nil)
      (setq widget (widget-create
		    'radio-button-choice
		    :value (w3-form-element-value el)
		    :action 'w3-form-radio-button-update
		    (list 'item
			  :button-face face
			  :format "%t"
			  :tag ""
			  :value (w3-form-element-value el)))
	    w3-form-radio-elements (cons (cons uniqid el)
					 w3-form-radio-elements))
      (widget-put widget 'w3-form-default-value (w3-form-element-value el))
      widget)))

(defun w3-form-create-button (el face)
  ;; This handles dealing with the bogus Netscape 'button' input type
  ;; that lots of places have been using to slap javascript shit onto
  (let ((val (w3-form-element-value el)))
    (if (or (not val) (string= val ""))
	(setq val "Push Me"))
    (widget-create 'push-button
		   :notify #'ignore
		   :button-face face
		   :value-face face
		   val)))

(defun w3-form-create-image (el face)
  (widget-create 'push-button
		 :button-face face
		 :value-face face
		 :notify #'w3-form-submit/reset-callback
		 :value (or
			 (plist-get (w3-form-element-plist el) 'alt)
			 ;; Can it have a value other than "" anyway?
			 ;; w3-form-determine-size does not even bother
			 ;; to check the value.
			 (let ((val (w3-form-element-value el)))
			   (and val
				(stringp val)
				(not (zerop (length val)))
				val))
			 "Form-Image")))

(defun w3-form-create-submit-button (el face)
  (let ((val (w3-form-element-value el)))
    (if (or (not val) (string= val ""))
	(setq val (if (eq (w3-form-element-type el) 'submit)
		      "Submit"
		    "Reset")))
    (widget-create 'push-button
		   :notify #'w3-form-submit/reset-callback
		   :button-face face val)))

(defun w3-form-create-file-browser (el face)
  (widget-create 'file
		 :button-face face
		 :value-face face
		 :size (w3-form-element-size el)
		 :must-match t
		 :value (w3-form-element-value el)))

(defun w3-form-create-keygen-list (_el face)
  (let* ((size (apply 'max (mapcar (lambda (pair) (length (car pair)))
				   w3-form-valid-key-sizes)))
	 (options (mapcar (lambda (pair)
			    (list 'choice-item
				  :format "%[%t%]"
				  :tab-order -1
				  :button-face face
				  :value-face face
				  :menu-tag-get `(lambda (zed) ,(car pair))
				  :tag (truncate-string-to-width (car pair)
								 size nil ? )
				  :value (cdr pair)))
			  w3-form-valid-key-sizes)))
    (apply 'widget-create 'menu-choice
	   :emacspeak-help 'w3-form-summarize-field
	   :value 1024
	   :ignore-case t
	   :tag "Key Length"
	   :size size
	   :button-face face
	   :value-face face
	   options)))

(defun w3-form-create-option-list (el face)
  (let* ((size (w3-form-determine-size el nil))
	 (widget (apply 'widget-create 'menu-choice
		       :value (w3-form-element-value el)
		       :ignore-case t
		       :tag "Choose"
		       :format "%v"
		       :size size
		       :value-face face
		       :button-face face
		       (mapcar
			(function
			 (lambda (x)
			   (list 'choice-item
				 :format "%[%t%]"
				 :emacspeak-help 'w3-form-summarize-field
				 :menu-tag-get `(lambda (zed) ,(car x))
				 :tag (truncate-string-to-width (car x)
								size nil ? )
				 :button-face face
				 :value-face face
				 :value (car x))))
			(w3-form-element-options el)))))
    (widget-value-set widget (w3-form-element-value el))
    widget))

;(defun w3-form-create-multiline (el face)
;  (widget-create 'text :value-face face (w3-form-element-value el)))

(defun w3-form-create-multiline (_el face)
  (widget-create 'push-button
		 :button-face face
		 :notify #'w3-do-text-entry
		 "Multiline text area"))

(defun w3-form-create-integer (el face)
  (if w3-form-use-old-style
      (w3-form-default-widget-creator el face)
    (widget-create 'integer
		   :size (w3-form-element-size el)
		   :value-face face
		   :tag ""
		   :format "%v"
		   :keymap w3-form-keymap
		   :w3-form-data el
		   (w3-form-element-value el))))

(defun w3-form-create-float (el face)
  (if w3-form-use-old-style
      (w3-form-default-widget-creator el face)
    (widget-create 'number
		   :size (w3-form-element-size el)
		   :value-face face
		   :format "%v"
		   :tag ""
		   :keymap w3-form-keymap
		   :w3-form-data el
		   (w3-form-element-value el))))

(defun w3-form-create-text (el face)
  (if w3-form-use-old-style
      (w3-form-default-widget-creator el face)
    (widget-create 'editable-field
		   :keymap w3-form-keymap
		   :size (w3-form-element-size el)
		   :value-face face
		   :w3-form-data el
		   (w3-form-element-value el))))

(defun w3-form-create-password (el face)
  ;; *sigh*  This will fail under XEmacs, but I can yell at them about
  ;; upgrading separately for the release of 19.15 and 20.0
  (if w3-form-use-old-style
      (w3-form-default-widget-creator el face)
    (widget-create 'editable-field
		   :secret ?*
		   :keymap w3-form-keymap
		   :size (w3-form-element-size el)
		   :value-face face
		   :button-face face
		   :w3-form-data el
		   (w3-form-element-value el))))

(defun w3-form-default-widget-creator (el face)
  (widget-create 'link
		 :notify #'w3-form-default-button-callback
		 :value-to-internal 'w3-form-default-button-update
		 :size (w3-form-element-size el)
		 :value-face face
		 :button-face face
		 :w3-form-data el
		 (w3-form-element-value el)))

(defun w3-form-default-button-update (w v)
  (let ((info (widget-get w :w3-form-data)))
    (widget-put w :tag
		(if info
		    (truncate-string-to-width
		     (if (eq 'password (w3-form-element-type info))
			 (make-string (length v) ?*)
		       v)
		     (w3-form-element-size info) nil ? )))
    v))

(defun w3-form-default-button-callback (widget &rest _ignore)
  (let* ((obj (widget-get widget :w3-form-data))
	 (typ (w3-form-element-type obj))
	 (def (widget-value widget))
	 (val 
          (case typ
            (password
             (read-passwd "Password: " def))
            (otherwise
             (read-string
              (concat (capitalize (symbol-name typ)) ": ") def)))))
    (widget-value-set widget val))
  (w3-form-possibly-submit widget))

;; These properties tell the help-echo function how to summarize each
;; type of widget.
(put 'checkbox  'w3-summarize-function 'w3-form-summarize-checkbox)
(put 'multiline 'w3-summarize-function 'w3-form-summarize-multiline)
(put 'radio     'w3-summarize-function 'w3-form-summarize-radio-button)
(put 'reset     'w3-summarize-function 'w3-form-summarize-submit-button)
(put 'submit    'w3-summarize-function 'w3-form-summarize-submit-button)
(put 'button    'w3-summarize-function 'w3-form-summarize-submit-button)
(put 'file      'w3-summarize-function 'w3-form-summarize-file-browser)
(put 'option    'w3-summarize-function 'w3-form-summarize-option-list)
(put 'keygen    'w3-summarize-function 'w3-form-summarize-keygen-list)
(put 'image	'w3-summarize-function 'w3-form-summarize-image)
(put 'password  'w3-summarize-function 'w3-form-summarize-password)
(put 'hidden    'w3-summarize-function 'ignore)

(defun w3-form-summarize-field (widget &rest _ignore)
  "Sumarize a widget that should be a W3 form entry area.
This can be used as the :help-echo property of all w3 form entry widgets."
  (let ((info nil)
	(func nil)
	(msg nil)
	)
    (setq info (widget-get widget :w3-form-data))
    (if info
	nil
      (while (widget-get widget :parent)
	(setq widget (widget-get widget :parent)))
      (setq info (widget-get widget :w3-form-data)))
    (if (not info)
	(signal 'wrong-type-argument (list 'w3-form-widget widget)))
    (setq func (or (get (w3-form-element-type info) #'w3-summarize-function)
		   #'w3-form-summarize-default)
	  msg (and (fboundp func) (funcall func info widget)))
    ;; FIXME!  This should be removed once emacspeak is updated to
    ;; more closely follow the widget-y way of just returning the string
    ;; instead of having the underlying :help-echo or :emacspeak-help
    ;; implementation do it.
    (and msg (message "%s" msg))))

(defsubst w3-form-field-label (data)
  ;;; FIXXX!!! Need to reimplement using the new forms implementation!
  (cdr-safe
   (assoc (or (plist-get (w3-form-element-plist data) 'id)
	      (plist-get (w3-form-element-plist data) 'label))
	  w3-form-labels)))

(defun w3-form-summarize-default (data _widget)
  (let ((label (w3-form-field-label data))
	(name  (w3-form-element-name data))
	(value (widget-value (w3-form-element-widget data))))
    (format "Text field %s set to: %s" (or label (concat "called " name))
	    value)))

(defun w3-form-summarize-password (data _widget)
  (let ((label (w3-form-field-label data))
	(name  (w3-form-element-name data)))
    (format "Password field %s is a secret.  Shhh."
	    (or label (concat "called " name)))))

(defun w3-form-summarize-multiline (data _widget)
  (let ((name (w3-form-element-name data))
        (label (w3-form-field-label data))
        (value (w3-form-element-value data)))
    (format "Multiline text input %s set to: %s"
	    (or label (concat "called " name))
	    value)))

(defun w3-form-summarize-checkbox (data _widget)
  (let ((name (w3-form-element-name data))
	(label (w3-form-field-label data))
	(checked (widget-value (w3-form-element-widget data))))
    (format "Checkbox %s is %s" (or label name) (if checked "on" "off"))))

(defun w3-form-summarize-option-list (data _widget)
  (let ((name (w3-form-element-name data))
	(label (w3-form-field-label data)))
    (format "Option list (%s) set to: %s" (or label name)
	    (widget-value (w3-form-element-widget data)))))

(defun w3-form-summarize-image (data _widget)
  (let ((name (w3-form-element-name data))
	(label (w3-form-field-label data)))
    (concat "Image entry " (or label (concat "called " name)))))

(defun w3-form-summarize-submit-button (data _widget)
  (let*  ((type (w3-form-element-type data))
	  (label (w3-form-field-label data))
	  (button-text (widget-value (w3-form-element-widget data)))
	  (type-desc (case type
		       (submit "Submit Form")
		       (reset "Reset Form")
		       (button "A Button"))))
    (format "%s: %s" type-desc (or label button-text ""))))

(defun w3-form-summarize-radio-button (data widget)
  (let ((name (w3-form-element-name data))
	(label (w3-form-field-label data))
	(cur-value (widget-value (w3-form-element-widget data)))
	(this-value (widget-value (widget-get-sibling widget))))
    (if (equal this-value cur-value)
	(format "Radio group %s has  %s pressed"
		(or label name) this-value)
      (format "Press this  to change radio group %s from %s to %s" (or label name) cur-value
	      this-value))))

(defun w3-form-summarize-file-browser (data _widget)
  (let ((name (w3-form-element-name data))
	(label (w3-form-field-label data))
	(file (widget-value (w3-form-element-widget data))))
    (format "File entry %s pointing to: %s" (or label name) (or file
								"[nothing]"))))

(defun w3-form-summarize-keygen-list (data _widget)
  (format "Submitting this form will generate a %d bit key (not)"
	  (widget-value (w3-form-element-widget data))))

(defun w3-form-maybe-submit-by-keypress ()
  (interactive)
  (let ((widget (widget-at (point))))
    (if widget
	(w3-form-possibly-submit widget))))

(defsubst w3-all-widgets (actn)
  ;; Return a list of data entry widgets in form number ACTN
  (cdr-safe (assoc actn w3-form-elements)))

(defun w3-form-possibly-submit (widget &rest _ignore)
  (let* ((formobj (widget-get widget :w3-form-data))
	 (ident (w3-form-element-action formobj))
	 (widgets (w3-all-widgets ident))
	 (text-fields 0)
	 (text-p nil))
    ;;
    ;; Gack.  Netscape auto-submits forms of one text field
    ;; here we go through the list of widgets in this form and
    ;; determine which are not submit/reset/button inputs.
    ;; If the # == 1, then submit the form.
    ;;
    (while widgets
      (setq text-fields (+
			 text-fields
			 (case (w3-form-element-type (car widgets))
			   ((submit reset image button)
			    0)
			   (text
			    (setq text-p t)
			    1)
			   (otherwise
			    1)))
	    widgets (cdr widgets)))
    (if (and (= text-fields 1) text-p)
	(w3-submit-form ident))))

(defun w3-form-submit/reset-callback (widget &rest _ignore)
  (let* ((formobj (widget-get widget :w3-form-data))
	 (w3-submit-button formobj))
    (case (w3-form-element-type formobj)
      (submit (w3-submit-form (w3-form-element-action formobj)))
      (reset  (w3-revert-form (w3-form-element-action formobj)))
      (image  (w3-submit-form (w3-form-element-action formobj)))
      (otherwise
       (error
	"Impossible widget type %s triggered w3-form-submit/reset-callback"
	(w3-form-element-type formobj))))))

;;;###autoload
(defun w3-do-text-entry (widget &rest _ignore)
  (let* ((data (list widget (current-buffer)))
	 (formobj (widget-get widget :w3-form-data))
	 (buff (get-buffer-create (format "Form Entry: %s"
					  (w3-form-element-name formobj)))))
    (switch-to-buffer-other-window buff)
    (indented-text-mode)
    (erase-buffer)
    (if (w3-form-element-value formobj)
	(insert (w3-form-element-value formobj)))
    (setq w3-current-last-buffer data)
    (message "Press C-c C-c when finished with text entry.")
    (local-set-key "\C-c\C-c" 'w3-finish-text-entry)))

(defun w3-finish-text-entry ()
  (interactive)
  (if w3-current-last-buffer
      (let* ((widget (nth 0 w3-current-last-buffer))
	     (formobj (widget-get widget :w3-form-data))
	     (buff (nth 1 w3-current-last-buffer))
	     (valu (buffer-string))
	     (inhibit-read-only t)
	     )
	(local-set-key "\C-c\C-c" 'undefined)
	(kill-buffer (current-buffer))
	(condition-case ()
	    (delete-window)
	  (error nil))
	(if (not (and buff (bufferp buff) (buffer-name buff)))
	    (message "Could not find the form buffer for this text!")
	  (switch-to-buffer buff)
	  (w3-form-element-set-value formobj valu)))))

(defun w3-revert-form (actn)
  (save-excursion
    (let* ((formobjs (w3-all-widgets actn))
	   (inhibit-read-only t)
	   deft type widget formobj)
      (while formobjs
	(setq formobj (car formobjs)
	      widget (w3-form-element-widget formobj)
	      formobjs (cdr formobjs)
	      deft (w3-form-element-default-value formobj)
	      type (w3-form-element-type formobj))
	(case type
	  ((submit reset image hidden) nil)
	  (radio
	   (setq deft (widget-get widget 'w3-form-default-value))
	   (if (and widget deft)
	       (widget-value-set widget deft)))
	  (checkbox
	   (if deft
	       (widget-value-set widget t)
	     (widget-value-set widget nil)))
	  (multiline
	   (w3-form-element-set-value formobj (w3-form-element-default-value
					       formobj)))
	  (file
	   (widget-value-set widget deft))
	  (otherwise
	   (widget-value-set widget deft))))
      (widget-setup))))

(defun w3-form-encode-helper (formobjs)
  (let (
	(submit-button-data w3-submit-button)
	formobj result widget temp type)
    (while formobjs
      (setq formobj (car formobjs)
	    type (w3-form-element-type formobj)
	    widget (w3-form-element-widget formobj)
	    formobjs (cdr formobjs)
	    temp (case type
		   (reset nil)
		   (button nil)
		   (image
		    (if (and (eq submit-button-data formobj)
			     (w3-form-element-name formobj))
			(setq result (append
				      (list
				       (cons
					(concat (w3-form-element-name formobj)
						".x") "0")
				       (cons
					(concat (w3-form-element-name formobj)
						".y") "0"))
				      result)))
		    nil)
		   (submit
		    (if (and (eq submit-button-data formobj)
			     (w3-form-element-name formobj))
			(cons (w3-form-element-name formobj)
			      (w3-form-element-value formobj))))
		   (radio
		    (let* ((radio-name (w3-form-element-name formobj))
			   (radio-object (cdr-safe
					  (assoc
					   (cons
					    radio-name
					    (w3-form-element-action formobj))
					   w3-form-radio-elements)))
			   (chosen-widget (and radio-object
					       (widget-radio-chosen
						(w3-form-element-widget
						 radio-object)))))
		      (if (assoc radio-name result)
			  nil
			(cons radio-name (widget-value chosen-widget)))))
		   ((int float)
		    (cons (w3-form-element-name formobj)
			  (number-to-string (or (condition-case ()
						    (widget-value widget)
						  (error nil)) 0))))
		   (checkbox
		    (if (widget-value widget)
			(cons (w3-form-element-name formobj)
			      (w3-form-element-value formobj))))
		   (file
		    (let ((fname (widget-value widget)))
		      (with-current-buffer (get-buffer-create " *w3-temp*")
			(erase-buffer)
                        (condition-case ()
                            (insert-file-contents-literally fname)
                          (error (concat "Error accessing " fname)))
			(cons (w3-form-element-name formobj)
			      (cons (list (cons
					   "filename"
					   (file-name-nondirectory fname)))
				    (buffer-string))))))
		   (option
		    (cons (w3-form-element-name formobj)
			  (cdr-safe
			   (assoc (widget-value widget)
				  (w3-form-element-options formobj)))))
		   (keygen
		    (condition-case ()
			(require 'ssl)
		      (error (error "Not configured for SSL, please read the info pages")))
		    (if (not (fboundp 'ssl-req-user-cert))
                        (error "This version of SSL isn't capable of requesting certificates")
                      (let ((challenge (plist-get (w3-form-element-plist formobj) 'challenge))
                            (size (widget-value widget)))
                        (cons (w3-form-element-name formobj)
                              (ssl-req-user-cert size challenge)))))
		   ((multiline hidden)
		    (cons (w3-form-element-name formobj)
			  (w3-form-element-value formobj)))
		   (otherwise
		    (cons (w3-form-element-name formobj)
			  (widget-value widget)))))
      (if temp
	  (setq result (cons temp result))))
    result))

(defun w3-form-encode-make-mime-part (id data separator)
  (let (addons)
    (if (listp data)
	(progn
	  (setq addons (mapconcat (lambda (x)
				    (format "; %s=\"%s\"" (car x) (cdr x)))
				  (car data) " "))
	  (setq data (cdr data)))
      (setq addons ""))
    (format "%s\r\nContent-Disposition: form-data; name=\"%s\"%s\r\n\r\n%s"
	    separator id addons data)))

(defun w3-form-encode-multipart/x-www-form-data (formobjs)
  ;; Create a multipart form submission.
  ;; Returns a cons of two strings.  Car is the separator used.
  ;; cdr is the body of the MIME message."
  (let ((separator (format-time-string "---separator-%Y%j%H%M%S-for-www-form-data")))
    (cons separator
	  (concat
	   (mapconcat
	    (function
	     (lambda (formobj)
	       (w3-form-encode-make-mime-part (car formobj) (cdr formobj)
					      separator)))
	    (w3-form-encode-helper formobjs)
	    "\r\n")
	   "\r\n" separator "--\r\n"))))

(fset 'w3-form-encode-multipart/form-data
      'w3-form-encode-multipart/x-www-form-data)
(fset 'w3-form-encode- 'w3-form-encode-application/x-www-form-urlencoded)

(defun w3-form-encode (result &optional enctype)
  "Create a string suitably encoded for a URL request."
  (let ((func (intern (concat "w3-form-encode-" enctype))))
    (if (fboundp func)
	(funcall func result)
      (w3-warn 'html (format "Bad encoding type for form data: %s" enctype))
      (w3-form-encode-application/x-www-form-urlencoded result))))

(defun w3-form-encode-text/plain (result)
  (let ((query ""))
    (setq query
	  (mapconcat
	   (function
	    (lambda (widget)
	      (let ((nam (car widget))
		    (val (cdr widget)))
		(if (string-match "\n" nam)
		    (setq nam (mapconcat
			       (function
				(lambda (x)
				  (if (= x ?\n) "," (char-to-string x))))
			       nam "")))
		(concat nam " " val))))
	   (w3-form-encode-helper result) "\n"))
    query))

;; Fixme: check the charset issues on form submission
;; http://ppewww.ph.gla.ac.uk/%7Eflavell/charset/form-i18n.html

(defun w3-form-encode-xwfu (chunk)
  "Escape characters in a string for application/x-www-form-urlencoded.
Blasphemous crap because someone didn't think %20 was good enough for encoding
spaces.  Die Die Die."
  ;; This will get rid of the 'attributes' specified by the file type,
  ;; which are useless for an application/x-www-form-urlencoded form.
  (if (consp chunk)
      (setq chunk (cdr chunk)))

  (mapconcat
   (lambda (char)
     (cond
      ((= char ?  ) "+")
      ((memq char url-unreserved-chars) (char-to-string char))
      (t (upcase (format "%%%02x" char)))))
   ;; Fixme: Should this actually be accepting multibyte?  Is there a
   ;; better way in XEmacs?
   (if (featurep 'mule)
       (encode-coding-string chunk
			     (if (fboundp 'find-coding-systems-string)
				 (car (find-coding-systems-string chunk))
				 buffer-file-coding-system))
     chunk)
   ""))

(defun w3-form-encode-application/x-www-form-urlencoded (result)
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   (w3-form-encode-helper result) "&"))

(defun w3-form-encode-application/x-w3-isindex (result)
  (let* ((info (w3-form-encode-helper result))
	 (query (cdr-safe (assoc "isindex" info))))
    (if query
	(url-hexify-string query)
      "")))

(defun w3-submit-form (ident)
  ;; Submit form entry fields matching ACTN as their action identifier.
  (let* ((result (w3-all-widgets ident))
	 (enctype (or (cdr (assq 'enctype ident))
		      "application/x-www-form-urlencoded"))
	 (query (w3-form-encode result enctype))
	 (themeth (upcase (or (cdr (assq 'method ident)) "get")))
	 (theurl (cdr (assq 'action ident))))
    (if (and (string= "GET" themeth)
	     (string-match "\\([^\\?]*\\)\\?" theurl))
	(setq theurl (match-string 1 theurl)))
    (cond
     ((or (string= "POST" themeth)
	  (string= "PUT" themeth))
      (if (consp query)
	  (setq enctype (concat enctype "; boundary="
				(substring (car query) 2 nil)
				"")
		query (cdr query)))
      (let ((url-request-method themeth)
	    (url-request-data query)
	    (url-request-extra-headers
	     (cons (cons "Content-type" enctype) url-request-extra-headers)))
	(w3-fetch theurl)))
     ((string= "GET" themeth)
      (let ((theurl (concat theurl "?" query)))
	(w3-fetch theurl)))
     (t
      (w3-warn 'html (format "Unknown submit method: %s" themeth))
      (let ((theurl (concat theurl "?" query)))
	(w3-fetch theurl))))))

(provide 'w3-forms)
