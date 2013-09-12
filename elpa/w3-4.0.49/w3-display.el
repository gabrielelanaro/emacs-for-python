;;; w3-display.el --- W3 display engine

;; Copyright (c) 1996, 97, 98, 99, 2000, 2001, 2007, 2008, 2013 Free Software Foundation, Inc.

;; Author: William M. Perry <wmperry@cs.indiana.edu>
;; Keywords: faces, help, hypermedia

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

;; Code:

(eval-when-compile
  (require 'cl)
  (require 'w3-props))
(defvar w3-last-parse-tree)
(require 'css)
(require 'font)
(require 'url-parse)
(require 'mailcap)
(require 'w3-widget)
(require 'w3-imap)

;; Some mm-* "functions" are macros.  Ensure that they are loaded.
(eval-when-compile
  (require 'mm-decode))

(autoload 'sentence-ify "flame")
(autoload 'string-ify "flame")
(autoload '*flame "flame")
(autoload 'flatten "flame")
(autoload 'append-suffixes-hack "flame") ; I guess... -- fx
(autoload 'w3-java-run-applet "w3-java")
(autoload 'w3-mode "w3")
(autoload 'w3-add-delayed-graphic "w3")
(autoload 'w3-find-specific-link "w3")
(autoload 'w3-fix-spaces "w3")
(autoload 'mm-inline-text "mm-view")	; may not be done by Gnus
(autoload 'w3-form-resurrect-widgets "w3-forms")
(autoload 'w3-do-setup "w3")
(autoload 'w3-parse-buffer "w3-parse")
(autoload 'w3-mode "w3")
(autoload 'w3-handle-style "w3-style")
(autoload 'w3-form-add-element "w3-forms")
(autoload 'w3-warn "w3")

(defvar w3-cookie-cache nil)

(defmacro w3-d-s-var-def (var)
  `(make-variable-buffer-local (defvar ,var nil)))

(w3-d-s-var-def w3-display-label-marker)
(w3-d-s-var-def w3-display-open-element-stack)
(w3-d-s-var-def w3-display-alignment-stack)
(w3-d-s-var-def w3-display-list-stack)
(w3-d-s-var-def w3-display-form-id)
(w3-d-s-var-def w3-display-whitespace-stack)
(w3-d-s-var-def w3-display-liststyle-stack)
(w3-d-s-var-def w3-display-font-family-stack)
(w3-d-s-var-def w3-display-font-weight-stack)
(w3-d-s-var-def w3-display-font-variant-stack)
(w3-d-s-var-def w3-display-font-size-stack)
(w3-d-s-var-def w3-face-color)
(w3-d-s-var-def w3-face-background-color)
(w3-d-s-var-def w3-active-faces)
(w3-d-s-var-def w3-active-voices)
(w3-d-s-var-def w3-current-form-number)
(w3-d-s-var-def w3-face-font-family)
(w3-d-s-var-def w3-face-font-weight)
(w3-d-s-var-def w3-face-font-variant)
(w3-d-s-var-def w3-face-font-size)
(w3-d-s-var-def w3-face-font-family)
(w3-d-s-var-def w3-face-font-size)
(w3-d-s-var-def w3-face-font-style)
(w3-d-s-var-def w3-face-font-spec)
(w3-d-s-var-def w3-face-text-decoration)
(w3-d-s-var-def w3-face-face)
(w3-d-s-var-def w3-face-descr)
(w3-d-s-var-def w3-face-background-image)
(w3-d-s-var-def w3-display-css-properties)
(w3-d-s-var-def w3-display-background-properties)


(defmacro w3-get-attribute (attr args)
  `(cdr-safe (assq ,attr ,args)))
  
(defmacro w3-get-face-info (info &optional other)
  (let ((var (intern (format "w3-face-%s" info))))
    `(push (w3-get-style-info (quote ,info)
                              (or (and (not w3-user-colors-take-precedence)
                                       (cdr-safe (assq (quote ,other)
                                                       (nth 1 node))))
                                  (car ,var)))
           ,var)))

(defmacro w3-pop-face-info (info)
  (let ((var (intern (format "w3-face-%s" info))))
    `(pop ,var)))

(defmacro w3-get-all-face-info ()
  `(progn
     (w3-get-face-info font-family)
     ;; This is to handle the 'face' attribute on arbitrary elements
     (if (cdr-safe (assq 'face (nth 1 node)))
         (setf (car w3-face-font-family)
               (append (car w3-face-font-family)
                       (split-string (cdr-safe
                                      (assq 'face (nth 1 node)))
                                     " *, *"))))
     (w3-get-face-info font-style)
     (w3-get-face-info font-weight)
     (w3-get-face-info font-variant)
     (w3-get-face-info font-size)
     (w3-get-face-info text-decoration)
     (w3-get-face-info background-image)
     (w3-get-face-info color color)
     (w3-get-face-info background-color bgcolor)
     (setq w3-face-font-spec (make-font
                              :weight (car w3-face-font-weight)
                              :family  (if (not w3-user-fonts-take-precedence)
                                           (car w3-face-font-family))
                              :size (car w3-face-font-size)))))

(defmacro w3-pop-all-face-info ()
  `(progn
     (w3-pop-face-info font-family)
     (w3-pop-face-info font-weight)
     (w3-pop-face-info font-variant)
     (w3-pop-face-info font-size)
     (w3-pop-face-info font-style)
     (w3-pop-face-info text-decoration)
     (w3-pop-face-info background-image)
     (w3-pop-face-info color)
     (w3-pop-face-info background-color)))

(defvar w3-display-same-buffer nil)
(defvar w3-face-cache nil  "Cache for `w3-face-for-element'.")
(defvar w3-face-index 0)
(defvar w3-image-widgets-waiting nil)

(make-variable-buffer-local 'w3-last-fill-pos)

(if (featurep 'emacspeak)
    (defadvice widget-convert-text (around emacspeak pre act comp)
      "Protect value of personality if set originally"
      (let ((start (ad-get-arg 1))
	    (end (ad-get-arg 2))
	    (orig nil ))
	(setq orig (get-text-property start 'personality))
	ad-do-it 
	(and orig
	     (put-text-property start end 
				'personality orig)))))

(defconst w3-fill-prefixes-vector
  (let ((len 0)
        (prefix-vector (make-vector 80 nil)))
    (while (< len 80)
      (aset prefix-vector len (make-string len ? ))
      (setq len (1+ len)))
    prefix-vector))

(defvar w3-pause-keystroke nil)

(defvar w3--cur-viewing-pos)

(defsubst w3-pause ()
  (save-excursion
    (goto-char (or w3--cur-viewing-pos (point-min)))
    (cond
     ((featurep 'xemacs)
      (if (and (not (sit-for 0)) (input-pending-p))
	  (condition-case ()
	      (let ((buffer-read-only t))
		(dispatch-event (next-command-event)))
	    (error nil))))
     (t
      (if (and (not (sit-for 0)) (input-pending-p))
	  (condition-case ()
	      (progn
		(setq w3-pause-keystroke
		      (lookup-key w3-mode-map (vector (read-event))))
		(case w3-pause-keystroke
		  ((w3-quit w3-leave-buffer) nil)
		  (otherwise
		   (call-interactively w3-pause-keystroke))))
	    (error nil)))))
    (setq w3--cur-viewing-pos (point))))

(defmacro w3-get-pad-string (len)
  `(cond
      ((< ,len 0)
       "")
      ((< ,len 80)
       (aref w3-fill-prefixes-vector ,len))
      (t (make-string ,len ? ))))

(defsubst w3-set-fill-prefix-length (len)
  (setq fill-prefix (if (< len (- (or w3-strict-width (window-width)) 4))
			(w3-get-pad-string len)
		      (w3-warn
		       'html
		       "Runaway indentation!  Too deep for window width!")
		      fill-prefix)))

(defun w3-decimal-to-roman (n)
  ;; Convert from decimal to roman numerals
  (let ((curmod 1000)
	(str "")
	(j 7)
	i2 k curcnt)
    (while (>= curmod 1)
      (if (>= n curmod)
	  (progn
	    (setq curcnt (/ n curmod)
		  n (- n (* curcnt curmod)))
	    (if (= 4 (% curcnt 5))
		(setq i2 (+ j (if (> curcnt 5) 1 0))
		      str (format "%s%c%c" str
				  (aref w3-roman-characters (1- j))
				  (aref w3-roman-characters i2)))
	      (progn
		(if (>= curcnt 5)
		    (setq str (format "%s%c" str (aref w3-roman-characters j))
			  curcnt (- curcnt 5)))
		(setq k 0)
		(while (< k curcnt)
		  (setq str (format "%s%c" str
				    (aref w3-roman-characters (1- j)))
			k (1+ k)))))))
      (setq curmod (/ curmod 10)
	    j (- j 2)))
    str))

(defun w3-decimal-to-alpha (n)
  ;; Convert from decimal to alphabetical (a, b, c, ..., aa, ab,...)
  (cond
   ((< n 1) (char-to-string ?Z))
   ((<= n 26) (char-to-string (+ ?A (1- n))))
   (t (concat (w3-decimal-to-alpha (/ n 26))
	      (w3-decimal-to-alpha (% n 26))))))

(defsubst w3-get-style-info (info &optional default)
  (or (cdr-safe (assq info w3-display-css-properties)) default))

(defun w3-decode-area-coords (str)
  (let (retval)
    (while (string-match "\\([ \t0-9]+\\),\\([ \t0-9]+\\)" str)
      (setq retval (cons (vector (string-to-number (match-string 1 str))
				 (string-to-number (match-string 2 str))) retval)
	    str (substring str (match-end 0) nil)))
    (if (string-match "\\([0-9]+\\)" str)
	(setq retval (cons (vector (+ (aref (car retval) 0)
				      (string-to-number (match-string 1 str)))
				   (aref (car retval) 1)) retval)))
    (nreverse retval)))

(defun w3-normalize-color (color)
  (cond
   ((valid-color-name-p color)
    color)
   ((valid-color-name-p (concat "#" color))
    (concat "#" color))
   ((string-match "[ \t\r\n]" color)
    (w3-normalize-color
     (mapconcat (function (lambda (x) (if (memq x '(?\t ?\r ?\n ? )) ""
					(char-to-string x)))) color "")))
   ((valid-color-name-p (font-normalize-color color))
    (font-normalize-color color))
   (t
    (w3-warn 'html (format "Bad color specification: %s" color))
    nil)))

(defsubst w3-voice-for-element ()
  (if (featurep 'emacspeak)
      (let (family gain left right pitch pitch-range stress richness voice)
	(setq family (w3-get-style-info 'voice-family)
	      gain (w3-get-style-info 'gain)
	      left (w3-get-style-info 'left-volume)
	      right (w3-get-style-info 'right-volume)
	      pitch (w3-get-style-info 'pitch)
	      pitch-range (w3-get-style-info 'pitch-range)
	      stress (w3-get-style-info 'stress)
	      richness (w3-get-style-info 'richness))
	(if (or family gain left right pitch pitch-range stress richness)
	    (setq voice (dtk-personality-from-speech-style
			 (make-dtk-speech-style :family (or family 'paul)
						:gain (or gain 5)
						:left-volume (or left 5)
						:right-volume (or right 5)
						:average-pitch (or pitch 5)
						:pitch-range (or pitch-range 5)
						:stress (or stress 5)
						:richness (or richness 5))))
	  (setq voice nil))
	(or voice (car w3-active-voices)))))

(defun w3-display-colors-too-close-p (foreground background)
  ;; This algorithm could be _MUCH_ better. :)
  ;;
  ;; Perhaps doing something along the lines of converting them both
  ;; to grayscale values, then considering them worthless if they are
  ;; within some user-configurable delta?
  (equal foreground background))

(defun w3-get-default-color (backgroundp)
  (if (featurep 'xemacs)
      (or (if backgroundp
	      (face-background-name 'default)
	    (face-foreground-name 'default))
	  (frame-property nil (if backgroundp 'background-color 'foreground-color))
	  (if backgroundp "white" "black"))
      (or (if backgroundp
	      (face-background 'default)
	    (face-foreground 'default))
	  (frame-parameter nil (if backgroundp 'background-color 'foreground-color))
	  (if backgroundp "white" "black"))))

(defun w3-display-background-useless-p (color)
  (let ((foreground-color (font-color-rgb-components
			   (w3-get-default-color nil)))
	(background-color (font-color-rgb-components color)))
    (w3-display-colors-too-close-p foreground-color background-color)))

(defun w3-display-foreground-useless-p (color)
  (let ((background-color (font-color-rgb-components
			   (w3-get-default-color t)))
	(foreground-color (font-color-rgb-components color)))
    (w3-display-colors-too-close-p foreground-color background-color)))

(defun w3-display-infer-contrasting-color (color)
  ;; Simple little algorithm suggested by Adam Hammer
  ;; <hammer@math.purdue.edu>
  ;; 
  ;; Extremes are black and white, so make those the two
  ;; choices. Convert the color to grayscale (0.299R + 0.587G +
  ;; 0.114B) and then choose either black or white depending upon
  ;; which one is furthest away from the grayscale color calculated.
  ;;
  ;; Found via research? Tested?  Haha.
  (let* ((rgb (font-color-rgb-components color))
	 (r (nth 0 rgb))
	 (g (nth 1 rgb))
	 (b (nth 2 rgb))
	 (grayscale nil)
	 (white nil)
	 (max nil)
	 (black nil))
    (if (and (<= r 255) (<= g 255) (<= b 255))
	(setq max 255)
      (setq max 65535))
    (setq grayscale (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))
	  black grayscale
	  white (- max grayscale))
    (if (> white black)
	"white"
      "black")))

(defalias 'w3-make-face
  (cond
   ((not (fboundp 'make-face)) #'ignore)
   ((featurep 'xemacs) #'make-face)
   (t
    (lambda (name &optional _doc-string _temporary)
      "Define and return a new FACE described by DOC-STRING.
If the face already exists, it is unmodified."
      (make-face name)))))

(defsubst w3-face-for-element (node)
  (w3-get-all-face-info)
  (if (car w3-face-text-decoration)
      (set-font-style-by-keywords w3-face-font-spec
				  (car w3-face-text-decoration)))
  (if w3-face-font-variant
      (set-font-style-by-keywords w3-face-font-spec
				  (car w3-face-font-variant)))
  (if w3-face-font-style
      (set-font-style-by-keywords w3-face-font-spec
				  (car w3-face-font-style)))
  (setq w3-face-descr (list w3-face-font-spec
			    (car w3-face-background-image)
			    (car w3-face-color)
			    (car w3-face-background-color))
	w3-face-face (cdr-safe (assoc w3-face-descr w3-face-cache)))
  (if (or w3-face-face
	  (not (or (car w3-face-color)
		   (car w3-face-background-image)
		   (car w3-face-background-color)
		   w3-face-font-spec)))
      nil				; Do nothing, we got it already
    (setq w3-face-face
	  (w3-make-face (intern (format "w3-style-face-%05d" w3-face-index))
			"An Emacs-W3 face... don't edit by hand." t)
	  w3-face-index (1+ w3-face-index))
    (if (car w3-face-background-image)
	(w3-maybe-start-background-image-download
	 (car w3-face-background-image) w3-face-face))
    (if w3-face-font-spec
	(font-set-face-font w3-face-face w3-face-font-spec))
    (if (car w3-face-color)
	(font-set-face-foreground w3-face-face (car w3-face-color)))
    (if (car w3-face-background-color)
	(font-set-face-background w3-face-face (car w3-face-background-color)))
    (if (not (car w3-face-background-image))
	(setq w3-face-cache (cons
			     (cons w3-face-descr w3-face-face)
			     w3-face-cache))))
  w3-face-face)

(defun w3-normalize-spaces (string)
  ;; nuke spaces in the middle
  (while (string-match "[ \t\r\n][ \r\t\n]+" string)
    (setq string (concat (substring string 0 (1+ (match-beginning 0)))
			 (substring string (match-end 0)))))

  ;; nuke spaces at the beginning
  (if (string-match "^[ \t\r\n]+" string)
      (setq string (substring string (match-end 0))))

  ;; nuke spaces at the end
  (if (string-match "[ \t\n\r]+$" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)


(if (not (fboundp 'char-before))
    (defun char-before (&optional pos)
      (char-after (1- (or pos (point))))))

(defsubst w3-display-line-break (n)
  (if (or
       (memq (car w3-display-whitespace-stack) '(pre nowrap)) ; Been told
       (= w3-last-fill-pos (point))
       (> w3-last-fill-pos (point-max)))
      (if (not (eq (char-before (point)) ?\n))
 	  (setq n (1+ n))) ; at least put one line in
    (let ((fill-column (max (1+ (length fill-prefix)) fill-column)))
      (case (car w3-display-alignment-stack)
	(center
	 (fill-region-as-paragraph w3-last-fill-pos (point) 'center))
	((justify full)
	 (fill-region-as-paragraph w3-last-fill-pos (point) 'full))
	(right
	 (fill-region-as-paragraph w3-last-fill-pos (point) 'right))
	(otherwise			; Default is left justification
	 (fill-region-as-paragraph w3-last-fill-pos (point)))
	))
    (setq n (1- n)))
  (setq w3-last-fill-pos (point-max))
  (insert-char ?\n n t))

(defsubst w3-munge-line-breaks-p ()
  (eq (car w3-display-whitespace-stack) 'pre))

(defvar w3-display-nil-face (w3-make-face nil "Stub face... don't ask." t))

(defvar w3-scratch-start-point nil)

(defsubst w3-handle-string-content (string)
  (setq w3-scratch-start-point (point))
  (insert string)
  (if (w3-munge-line-breaks-p)
      (progn
	(goto-char w3-scratch-start-point)
	(if (not (search-forward "\n" nil t))
	    (subst-char-in-region w3-scratch-start-point (point-max) ?\r ?\n)
	  (subst-char-in-region w3-scratch-start-point (point-max) ?\r ? )))
    (goto-char w3-scratch-start-point)
    (while (re-search-forward
	    " [ \t\n\r]+\\|[\t\n\r][ \t\n\r]*"
	    nil 'move)
      (replace-match " "))
    (goto-char w3-scratch-start-point)
    (if (and (memq (char-before (point)) '(?  ?\t ?\r ?\n))
	     (looking-at "[ \t\r\n]"))
	(delete-region (point)
		       (progn
			 (skip-chars-forward " \t\r\n")
			 (point)))))
  (goto-char (point-max))
  (add-text-properties w3-scratch-start-point
		       (point) (list 'face w3-active-faces
				     'html-stack w3-display-open-element-stack
				     'start-open nil
				     'end-open nil
				     'front-sticky t
				     'rear-nonsticky nil
				     'personality (car w3-active-voices)
				     'duplicable t))
  )

(defun w3-display-get-cookie (args)
  (if (not (fboundp 'cookie))
      "Sorry, no cookies today."
    (let* ((href (or (w3-get-attribute 'href args)
                     (w3-get-attribute 'src args)))
	   (fname (cdr-safe (assoc href w3-cookie-cache))))
      (unless (and fname (file-exists-p fname))
        (setq fname (make-temp-file "" nil ".cki"))
        (with-current-buffer (generate-new-buffer " *cookie*")
          (mm-disable-multibyte)
          (url-insert-file-contents href)
          (setq buffer-file-name nil)
          (set-buffer-modified-p nil)
          (let ((coding-system-for-write 'binary))
            (write-region (point-min) (point-max) fname 5))
          (push (cons href fname) w3-cookie-cache)))
      (let ((st (or (cdr-safe (assq 'start args)) "Loading cookies..."))
            (nd (or (cdr-safe (assq 'end args)) "Loading cookies... done.")))
        (cookie fname st nd)))))

(defun w3-widget-buffer (widget)
  (if (featurep 'xemacs)
      (let ((extent (or (widget-get widget :button-extent)
                        (widget-get widget :field-extent))))
        (and extent (extent-buffer extent)))
    (let ((overlay (or (widget-get widget :button-overlay)
                       (widget-get widget :field-overlay))))
      (and overlay (overlay-buffer overlay)))))

(defun w3-widget-echo (widget &rest _ignore)
  (with-current-buffer (or (w3-widget-buffer widget) (current-buffer))
    (let* ((url (widget-get widget :href))
	   (name (widget-get widget :name))
	   (text (buffer-substring (widget-get widget :from)
				   (widget-get widget :to)))
	   (title (widget-get widget :title))
	   (check w3-echo-link))
      (if url
	  (setq url (url-truncate-url-for-viewing url)))
      (if name
	  (setq name (concat "anchor:" name)))
      (if (not (listp check))
	  (setq check (cons check '(title url text name))))
      (catch 'exit
        (dolist (sym check)
          (let ((val (ecase sym
                       (name name)
                       (url url)
                       (title title)
                       (text text))))
            (and (stringp val)
                 (> (length val) 0)
                 (throw 'exit val))))))))

(defun w3-follow-hyperlink (widget &rest _ignore)
  (let* ((target (or (widget-get widget :target) w3-base-target))
	 (visited (widget-get widget :visited-face))
	 (href (widget-get widget :href)))
    (if target (setq target (intern (downcase target))))
    (if visited
	(condition-case ()
	    (add-text-properties (widget-get widget :start)
				 (widget-get widget :end)
				 (list 'face visited))
	(error nil)))
    (case target
      ((_blank external)
       (w3-fetch-other-frame href))
      (_top
       (delete-other-windows)
       (w3-fetch href))
      (otherwise
       (w3-fetch href target)))))

(defun w3-balloon-help-callback (object &optional _event)
  (let* ((widget (widget-at (extent-start-position object)))
	 (href (widget-get widget :href)))
    (if href
	(url-truncate-url-for-viewing href)
      nil)))


;; Various macros
(eval-when-compile
  (defmacro w3-node-visible-p ()
    `(not (eq (car break-style) 'none)))

  (defmacro w3-handle-empty-tag ()
    `(progn
       (push (cons tag args) w3-display-open-element-stack)
       (push content content-stack)
       (setq content nil)))

  (defmacro w3-handle-content (node)
    `(progn
       (push (cons tag args) w3-display-open-element-stack)
       (push content content-stack)
       (setq content (nth 2 ,node))))

  (defmacro w3-display-handle-list-type ()
    `(add-text-properties
      (point)
      (progn
	(case (car break-style)
	  (list-item
	   (let ((list-style (or (car w3-display-liststyle-stack) 'disc))
		 (list-num (if (car w3-display-list-stack)
			       (incf (car w3-display-list-stack))
			     1))
		 (margin (1- (car left-margin-stack)))
		 (indent (w3-get-style-info 'text-indent 0)))
	     (if (> indent 0)
		 (setq margin (+ margin indent))
	       (setq margin (max 0 (- margin indent))))
	     (beginning-of-line)
	     (case list-style
	       ((disc circle square)
		(insert (format (format "%%%dc" margin)
				(or (cdr-safe (assq list-style w3-bullets))
				    ?o))))
	       ((decimal lower-roman upper-roman lower-alpha upper-alpha)
		(let ((x (case list-style
			   (lower-roman
			    (w3-decimal-to-roman list-num))
			   (upper-roman
			    (upcase
			     (w3-decimal-to-roman list-num)))
			   (lower-alpha
			    (w3-decimal-to-alpha list-num))
			   (upper-alpha
			    (upcase
			     (w3-decimal-to-alpha list-num)))
			   (otherwise
			    (int-to-string list-num)))))
		  (insert (format (format "%%%ds." margin) x))
		  )
		)
	       (otherwise
		(insert (w3-get-pad-string margin)))
	       )
	     )
	   )
	  (otherwise
	   (insert (w3-get-pad-string (+ (car left-margin-stack)
					 (w3-get-style-info 'text-indent 0)))))
	  )
	(point))
      (list 'start-open t
	    'end-open t
	    'rear-nonsticky nil
	    'face 'nil)))

  (defmacro w3-display-set-margins ()
    `(progn
       (push (+ (w3-get-style-info 'margin-left 0)
		(car left-margin-stack)) left-margin-stack)
       (push (-
	      (car right-margin-stack)
	      (w3-get-style-info 'margin-right 0)) right-margin-stack)
       (setq fill-column (car right-margin-stack))
       (w3-set-fill-prefix-length (car left-margin-stack))
       (w3-display-handle-list-type)))

  (defmacro w3-display-restore-margins ()
    `(progn
       (pop right-margin-stack)
       (pop left-margin-stack)))

  (defmacro w3-display-handle-break ()
    `(case (car break-style)
       (block				; Full paragraph break
	(if (eq (cadr break-style) 'list-item)
	    (setf (cadr break-style) 'line)
	  (w3-display-line-break 1))
	(w3-display-set-margins)
	(push
	 (w3-get-style-info 'white-space
			    (car w3-display-whitespace-stack))
	 w3-display-whitespace-stack)
	(push
	 (or (w3-get-attribute 'foobarblatz args)
	     (w3-get-style-info 'list-style-type
				(car w3-display-liststyle-stack)))
	 w3-display-liststyle-stack)
	(push
	 (or (w3-get-attribute 'align args)
	     (w3-get-style-info 'text-align
				(car w3-display-alignment-stack)))
	 w3-display-alignment-stack)
	(and w3-do-incremental-display (w3-pause)))
       ((line list-item)		; Single line break
	(w3-display-line-break 0)
	(w3-display-set-margins)
	(push
	 (or (w3-get-attribute 'foobarblatz args)
	     (w3-get-style-info 'list-style-type
				(car w3-display-liststyle-stack)))
	 w3-display-liststyle-stack)
	(push
	 (w3-get-style-info 'white-space
			    (car w3-display-whitespace-stack))
	 w3-display-whitespace-stack)
	(push
	 (w3-get-style-info 'text-align
			    (or (w3-get-attribute 'align args)
				(car w3-display-alignment-stack)))
	 w3-display-alignment-stack))
       (otherwise			; Assume 'inline' rendering as default
	nil))
    )
    

  (defmacro w3-display-progress-meter ()
    `(url-lazy-message "Drawing... %c" (aref "/|\\-" (random 4))))
    
  (defmacro w3-display-handle-end-break ()
    `(case (pop break-style)
       (block				; Full paragraph break
	(w3-display-line-break 1)
	(w3-display-restore-margins)
	(pop w3-display-whitespace-stack)
	(pop w3-display-liststyle-stack)
	(pop w3-display-alignment-stack)
	(and w3-do-incremental-display (w3-pause)))
       ((line list-item)		; Single line break
	(w3-display-restore-margins)
	(w3-display-line-break 0)
	(pop w3-display-whitespace-stack)
	(pop w3-display-liststyle-stack)
	(pop w3-display-alignment-stack))      
       (otherwise			; Assume 'inline' rendering as default
	nil))
     )
)  


;; <link> handling
(defun w3-parse-link (args)
  (let* ((type (if (w3-get-attribute 'rel args) 'rel 'rev))
	 (desc (w3-get-attribute type args))
	 (dc-desc (and desc (downcase desc))) ; canonical case
	 (dest (w3-get-attribute 'href args))
	 (plist (w3-alist-to-plist args))
	 (node-1 (assq type w3-current-links))
	 (node-2 (and node-1 desc (or (assoc desc
					     (cdr node-1))
				      (assoc dc-desc
					     (cdr node-1)))))
	 )
    ;; Canonicalize the case of link types we may look for
    ;; specifically (toolbar etc.) since that's done with
    ;; assoc.  See `w3-mail-document-author' and
    ;; `w3-link-toolbar', at least.
    (if (member dc-desc w3-defined-link-types)
	(setq desc dc-desc))
    (if dest				; ignore if HREF missing
	(cond
	 (node-2			; Add to old value
	  (setcdr node-2 (cons plist (cdr node-2))))
	 (node-1			; first rel/rev
	  (setcdr node-1 (cons (cons desc (list plist))
			       (cdr node-1))))
	 (t (setq w3-current-links
		  (cons (cons type (list (cons desc (list plist))))
			w3-current-links)))))
    (setq desc (and desc (intern dc-desc)))
    (case desc
      ((style stylesheet)
       (if w3-honor-stylesheets
	   (w3-handle-style plist)))
      (otherwise
       )
      )
    )
  )


;; Image handling
(defun w3-maybe-start-image-download (widget)
  (let* ((src (widget-get widget :src))
	 (cached-glyph (w3-image-cached-p src)))
    (cond
     ((and cached-glyph
	   (widget-glyphp cached-glyph)
	   (not (eq 'nothing
		    (condition-case ()
			(image-instance-type
			 (glyph-image-instance cached-glyph))
		      (error 'nothing)))))
      (setq w3-image-widgets-waiting (cons widget w3-image-widgets-waiting)))
     ((or w3-delay-image-loads		; Delaying images
	  (and (not (fboundp 'valid-specifier-domain-p)) ; Can't do images (XEmacs)
	       (if (fboundp 'display-graphic-p)
		   (not (display-graphic-p))
		 t))			; Can't do images (Emacs))
	  (eq (device-type) 'tty))	; Why bother?
      (w3-add-delayed-graphic widget))
     ((not (w3-image-loadable-p src nil)) ; Hey, we can't load it!
      (message "Skipping image %s" (w3-url-file-nondirectory src))
      (w3-add-delayed-graphic widget))
     (t					; Grab the images
      (let ((url-request-method "GET")
	    (url-request-data nil)
	    (url-request-extra-headers nil)
	    (url-mime-accept-string (substring
				     (mapconcat
				      (lambda (x)
					(if x
					    (concat (car x) ",")
					  ""))
				      w3-allowed-image-types "")
				     0 -1)))
	(if (featurep 'xemacs)
	    (setq w3-graphics-list (cons (cons src (make-glyph))
				     w3-graphics-list))
	  (add-to-list 'w3-graphics-list (cons src (list 'image))))
	(url-retrieve src 'w3-finalize-image-download-skip-redirects
		      (list src (widget-get widget 'buffer) widget)))))))

(defun w3-maybe-start-background-image-download (src face)
  (let* ((cached-glyph (w3-image-cached-p src))
	 (buf (current-buffer)))
    (cond
     ((and cached-glyph
	   (widget-glyphp cached-glyph)
	   (not (eq 'nothing
		    (image-instance-type
		     (glyph-image-instance cached-glyph)))))
      (set-face-background-pixmap face
				  (glyph-image-instance cached-glyph) buf))
     ((or (not (fboundp 'valid-specifier-domain-p)) ; Can't do images
	  (eq (device-type) 'tty))	; Why bother?
      nil)
     ((not (w3-image-loadable-p src nil)) ; Hey, we can't load it!
      (message "Skipping image %s" (w3-url-file-nondirectory src))
      nil)
     (t					; Grab the images
      (let ((url-request-method "GET")
	    (url-request-data nil)
	    (url-request-extra-headers nil)
	    (url-mime-accept-string (substring
				     (mapconcat
				      (function
				       (lambda (x)
					 (if x
					     (concat (car x) ",")
					   "")))
				      w3-allowed-image-types "")
				     0 -1)))
	(if (featurep 'xemacs)
	    (setq w3-graphics-list (cons (cons src (make-glyph))
				     w3-graphics-list))
	  (add-to-list 'w3-graphics-list (cons src (list 'image))))
	(url-retrieve src 'w3-finalize-image-download-skip-redirects (list src buf 'background face)))))))

(defun w3-finalize-image-download-skip-redirects (&rest args)
  (let (;; redirect-url
        errorp)
    ;; Handle both styles of `url-retrieve' callbacks...
    (cond
     ((listp (car args))
      ;; Emacs 22 style.  First argument is a list.
      (let ((status (car args)))
	(when (eq (car status) :error)
	  (setq errorp (cadr status))
	  (setq status (cddr status)))
	;; (when (eq (car status) :redirect)
	;;   (setq redirect-url (second (car args))))

	(setq args (cdr args))))

     ((eq (car args) :redirect)
      ;; Pre-22 redirect.
      ;; (setq redirect-url (cadr args))
      (while (eq (car args) :redirect)
	(setq args (cddr args)))))

    (if errorp
	(progn
	  ;;(message "Reading of %s failed: %s" (or redirect-url (car args)) errorp)
	  (url-mark-buffer-as-dead (current-buffer)))
      ;; Actually, for images we don't want to know the real URL, as the
      ;; original address is used when putting the images in the right
      ;; place.  Thus we ignore redirect-url.
      (apply #'w3-finalize-image-download args))))

(defun w3-finalize-image-download (url buffer &optional widget face)
  (let ((glyph nil)
	(node nil)
	(handle (mm-dissect-buffer t))
	(align (ignore-errors
		 (widget-get widget 'align))))
    (url-mark-buffer-as-dead (current-buffer))
    ;;(message "Enhancing image...")
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (mm-insert-part handle)
      (setq glyph 
            (let ((type (cdr-safe (assoc (car (mm-handle-type handle))
                                         w3-image-mappings))))
              (if (fboundp 'image-normalize)
                  (image-normalize type (buffer-string))
                (create-image (buffer-string) type 'data
                              :ascent (case align
                                        ((bottom nil) 100)
                                        (center 'center)
                                        (top 0)))))))
    ;;(message "Enhancing image... done")
    (cond
     ((w3-image-invalid-glyph-p glyph)
      (setq glyph nil)
      (message "Reading of %s failed." url))
     ((and (featurep 'xemacs)
	   (eq (aref glyph 0) 'xbm))
      (let ((temp-fname (url-generate-unique-filename "%s.xbm")))
	(with-current-buffer (generate-new-buffer " *xbm-garbage*")
	  (erase-buffer)
	  (insert (aref glyph 2))
	  (setq glyph temp-fname)
	  (let ((coding-system-for-write 'binary))
	    (write-region (point-min) (point-max) temp-fname))
	  (kill-buffer (current-buffer)))
	(setq glyph (make-glyph (list (cons 'x glyph))))
	(condition-case ()
	    (delete-file temp-fname)
	  (error nil)))))
    (setq node (assoc url w3-graphics-list))
    (cond
     ((and node glyph)
      (if (fboundp 'set-glyph-image)
	  (set-glyph-image (cdr node) (glyph-image glyph))
	(setcdr (cdr node) (cdr glyph))))
     (glyph
      (setq w3-graphics-list (cons (cons url glyph) w3-graphics-list)))
     (t nil))

    (cond
     ((or (not buffer)
	  (not (widget-glyphp glyph))
	  (not (buffer-name buffer)))
      nil)
     ((and (eq widget 'background)
	   (featurep 'xemacs))
      (set-face-background-pixmap face
				  (glyph-image-instance glyph)
				  buffer))
     ((not (eq widget 'background))
      (with-current-buffer buffer
	(if (eq major-mode 'w3-mode)
	    (widget-value-set widget glyph)
	  (setq w3-image-widgets-waiting
		(cons widget w3-image-widgets-waiting))))))))

(defcustom w3-min-img-size 15
  "Image size under which the alt string is replaced by `w3-dummy-img-alt-repl'.
15 is a bit aggressive, 5 pixels would be safer"
  :group 'w3-images
  :type 'integer
  )

(defcustom w3-dummy-img-re
  "\\b\\(boule\\|bullet\\|dot\\|pebble[0-9]*[a-z]?[0-9]*\\|pixel\\)\\b"
  "Image name regexp for which the alt string is replaced by `w3-dummy-img-alt-repl'."
  :group 'w3-images
  :type 'regexp)

(defcustom w3-dummy-img-alt-repl "*"
  "Dummy image alt string replacement."
  :group 'w3-images
  :type 'string)  

(defvar w3--height)
(defvar w3--width)
(defvar w3--args)

(defun w3-default-image-alt-func (fname)
  ;; Assumes w3--height/width bound by calling function
  (if (or (and (stringp w3--height)
	       (< (string-to-number w3--height) w3-min-img-size))
	  (and (stringp w3--width)
	       (< (string-to-number w3--width) w3-min-img-size))
	  (string-match w3-dummy-img-re fname))
      w3-dummy-img-alt-repl
    (concat "[" (file-name-sans-extension fname) "]")))

(defsubst w3-image-alt (src)
  (let* ((doc-alt (w3-get-attribute 'alt w3--args))
         (alt (or (and (stringp doc-alt) (string-match "[^ \t\n]" doc-alt) doc-alt)
                  (cond
                   ((null w3-auto-image-alt) "")
                   ((eq t w3-auto-image-alt)
                    (concat "[IMAGE(" (w3-url-file-nondirectory src) ")] "))
                   ((stringp w3-auto-image-alt)
                    (format w3-auto-image-alt (w3-url-file-nondirectory src)))
                   ((functionp w3-auto-image-alt)
                    (funcall w3-auto-image-alt (w3-url-file-nondirectory src))))))
         c)
    (while (setq c (string-match "[\C-i\C-j\C-l\C-m]" alt))
      (aset alt c ? ))
    alt))

(defvar w3--hyperimage-info)
(defvar w3--hyperlink-info)

(defvar w3-display-current-row nil)
(defvar w3-display-current-col nil)

(defsubst w3-handle-image (args)
  (let* ((w3--height (w3-get-attribute 'height args))
         (w3--width (w3-get-attribute 'width args))
         (src (or (w3-get-attribute 'src args) "Error Image"))
         (alt (w3-image-alt src))
         (ismap (and (assq 'ismap args) 'ismap))
         (usemap (w3-get-attribute 'usemap args))
         (href (and w3--hyperlink-info
                    (cadr (widget-plist-member (cadr w3--hyperlink-info) :href))))
         (target (and w3--hyperlink-info
                      (cadr (widget-plist-member (cadr w3--hyperlink-info)
                                                 :target))))
         (widget nil)
         (align (or (w3-get-attribute 'align args)
                    (w3-get-style-info 'vertical-align)))
         (face w3-active-faces))
    (if (assq '*table-autolayout w3-display-open-element-stack)
        (insert alt)
      (setq w3--hyperimage-info
            (list (point)
                  (list 'image
                        :src src	; Where to load the image from
                        'alt alt	; Textual replacement
                        'ismap ismap	; Is it a server-side map?
                        'usemap usemap	; Is it a client-side map?
                        :href href	; Hyperlink destination
                        :target target	; target frame
                        :button-face face ; img:link or img:visited entry in stylesheet
                        'row w3-display-current-row
                        'column w3-display-current-col
                        'align align
                        )))
      (setq widget (apply (function widget-create) (cadr w3--hyperimage-info)))
      (widget-put widget 'buffer (current-buffer))
      ;;(w3-maybe-start-image-download widget) ; in w3-resurrect-images
      (if (widget-get widget :from)
          (add-text-properties (widget-get widget :from)
                               (widget-get widget :to)
                               (list 'html-stack w3-display-open-element-stack)))
      (goto-char (point-max)))))

;; The table handling
(eval-and-compile
  (when (featurep 'mule)
    (if (and (featurep 'xemacs)
	     (not (find-charset 'w3-dingbats)))
	(make-charset 'w3-dingbats "Dingbats character set for Emacs/W3"
		      '(registry "" dimension 1 chars 96 final ?:))
      (if (not (charsetp 'w3-dingbats))
	  (define-charset nil 'w3-dingbats
	    (vector
	     1				; dimension
	     96				; chars
	     1				; width
	     1				; direction
	     ?:				; iso-final-char
	     0				; iso-graphic-plane (whats this?)
	     "dingbats" "emacs/w3-dingbats"
	     "Dingbats character set for Emacs/W3"))))))

(defun w3-make-char (oct)
  (when (featurep 'mule)
    (if (featurep 'xemacs)
	(make-char 'w3-dingbats (if (characterp oct) (char-int oct) oct))
      (make-char 'w3-dingbats oct))))

(defvar w3-table-ascii-border-chars
  [nil  nil  nil  ?+ nil  ?- ?+ ?- nil ?+ ?| ?| ?+ ?- ?| ?+]
  "*Vector of ascii characters to use to draw table borders.
This vector is used when terminal characters are unavailable")

(defvar w3-table-glyph-border-chars
  [nil  nil  nil  11 nil  2 7 14 nil 3 8 6 1 15 4 5]
  "Vector of characters to use to draw table borders.
This vector is used when terminal characters are used via glyphs")

(defvar w3-table-graphic-border-chars
  (vector
   nil
   nil
   nil
   (w3-make-char ?j)
   nil
   (w3-make-char ?q)
   (w3-make-char ?m)
   (w3-make-char ?v)
   nil
   (w3-make-char ?k)
   (w3-make-char ?x)
   (w3-make-char ?u)
   (w3-make-char ?l)
   (w3-make-char ?w)
   (w3-make-char ?t)
   (w3-make-char ?n))
  "Vector of characters to use to draw table borders.
This vector is used when terminal characters are used directly")

(defvar w3-table-border-chars w3-table-ascii-border-chars
  "Vector of characters to use to draw table borders.
w3-setup-terminal-chars sets this to one of 
w3-table-ascii-border-chars, 
w3-table-glyph-border-chars, or
w3-table-graphic-border-chars.")

(defsubst w3-table-lookup-char (l u r b &optional char)
  (or char (aref w3-table-border-chars (logior (if l 1 0)
					       (if u 2 0)
					       (if r 4 0)
					       (if b 8 0)))))

(defvar w3-terminal-properties nil)

(defsubst w3-insert-terminal-char (character &optional count inherit)
  (if w3-terminal-properties
      (set-text-properties (point)
			   (progn
			     (insert-char (or character ? )
					  (or count 1) inherit)
			     (point))
			   w3-terminal-properties)
    (remove-text-properties (point)
			    (progn
			      (insert-char (or character ? ) (or count 1) inherit)
			      (point))
			    '(face nil))))

(defsubst w3-horizontal-rule-char ()
  (w3-table-lookup-char t nil t nil w3-horizontal-rule-char))

(defun w3-setup-terminal-chars ()
  "Try to find the best set of characters to draw table borders with.
On a console, this can trigger some Emacs display bugs.

Initializes a number of variables:
w3-terminal-properties to either nil or a list of properties including 'face
w3-table-border-chars to one of the the three other vectors"
  (interactive)
  (setq w3-table-border-chars w3-table-ascii-border-chars
	w3-terminal-properties nil)
  (cond
   ((and w3-use-terminal-characters
	 (eq (device-type) 'x))
    (if (and (find-face 'w3-table-hack-x-face)
	     (face-differs-from-default-p 'w3-table-hack-x-face))
	nil
      (make-face 'w3-table-hack-x-face)
      (if (not (face-differs-from-default-p 'w3-table-hack-x-face))
	  (font-set-face-font 'w3-table-hack-x-face
			      (make-font :family "terminal"
					 :registry "*"
					 :encoding "*"
					 ))))
    (cond
     ((not (face-differs-from-default-p 'w3-table-hack-x-face))
      nil)
     ((and w3-use-terminal-glyphs (fboundp 'face-id))
      (let ((id (face-id 'w3-table-hack-x-face))
	    (c (length w3-table-border-chars)))
	(while (> (decf c) 0)
	  (if (aref w3-table-glyph-border-chars c)
	      (aset standard-display-table (aref w3-table-glyph-border-chars c)
		    (vector (+ (* 256 id)
			       (aref w3-table-graphic-border-chars c))))))
	(setq w3-table-border-chars w3-table-glyph-border-chars
	      w3-terminal-properties nil)))
     (t 
      (setq w3-table-border-chars w3-table-graphic-border-chars
	    w3-terminal-properties (list 'start-open t
					 'end-open t
					 'rear-nonsticky t
					 'w3-table-border t
					 'face 'w3-table-hack-x-face)))))
   ((and w3-use-terminal-characters-on-tty
	 (eq (device-type) 'tty))
    (let ((c (length w3-table-border-chars)))
      (while (> (decf c) 0)
	(and (aref w3-table-glyph-border-chars c)
	     (aref w3-table-graphic-border-chars c)
	     (standard-display-g1 (aref w3-table-glyph-border-chars c)
				  (aref w3-table-graphic-border-chars c)))))
    (setq w3-table-border-chars w3-table-glyph-border-chars
	  w3-terminal-properties (list 'w3-table-border t)))
   (t
    nil))
  w3-table-border-chars)

(defun w3-unsetup-terminal-characters nil
  (interactive)
  (w3-excise-terminal-characters (buffer-list))
  (standard-display-default 1 15)
  (setq w3-table-border-chars w3-table-ascii-border-chars))

(defun w3-excise-terminal-characters (buffs)
  "Replace hacked characters with ascii characters in buffers BUFFS.
Should be run before restoring w3-table-border-chars to ascii characters.
This will only work if we used glyphs rather than text properties"
  (interactive (list (list (current-buffer))))
  (let ((inhibit-read-only t)
	(tr (make-string 16 ? ))
	(i 0))
    (while (< i (length tr))
      (aset tr i i)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i (length w3-table-border-chars))
      (and (aref w3-table-border-chars i)
	   (< (aref w3-table-border-chars i) 16)
	   (aset tr 
		 (aref w3-table-glyph-border-chars i)
		 (aref w3-table-ascii-border-chars i)))
      (setq i (1+ i)))
    (mapcar (lambda (buf)
              (with-current-buffer buf
                (if (eq major-mode 'w3-mode)
                    (translate-region (point-min)
                                      (point-max)
                                      tr))))
	    buffs)))

(defvar w3-display-table-cut-words-p nil
  "*Whether to cut words that are oversized in table cells")
  
(defvar w3-display-table-force-borders (featurep 'emacspeak)
  "*Whether to always draw table borders
Can sometimes make the structure of a document clearer")

(defvar w3-display-current-cell-offset 0)

(defun w3-display-table-cut ()
  (save-excursion
    (goto-char (point-min))
    (let ((offset -1))
      (while (< offset 0)
  	(end-of-line)
  	(setq offset (- fill-column (current-column)))
  	(cond ((< offset 0)
  	       (condition-case nil
  		   (progn (forward-char offset)
  			  (insert ?\n))
 		 (error (setq offset 0))))
  	      ((not (eobp))
  	       (forward-line 1)
  	       (setq offset -1)))))))

(defun w3-display-fix-widgets ()
  ;; Make markers belong to the right buffer
  (save-excursion
    (let ((st (point-min))
 	  (nd nil)
	  (widget nil) parent)
      (while (setq st (next-single-property-change st 'button))
 	(setq nd (or (next-single-property-change st 'button) (point-max))
 	      widget (widget-at st)
 	      parent (and widget (widget-get widget :parent))
 	      )
	(if (not widget)
	    nil
	  (widget-put widget :from (set-marker (make-marker) st))
	  (widget-put widget :to   (set-marker (make-marker) nd))
	  (if (not parent)
	      nil
	    (widget-put parent :from (set-marker (make-marker) st))
	    (widget-put parent :to   (set-marker (make-marker) nd))))
 	(if (condition-case ()
 		(get-text-property (1+ nd) 'button)
 	      (error nil))
 	    (setq st nd)
 	  (setq st (min (point-max) (1+ nd))))))))

(defun w3-size-of-tree (tree minmax)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      ;; XXX fill-column set to 1 fails when fill-prefix is set
      ;; XXX setting fill-column at all isn't really right
      ;; for example <hr>s shouldn't be especially wide
      ;; we should set a flag that makes w3 never wrap a line
      (let ((fill-column (cond ((eq minmax 'min)
				3)
			       ((eq minmax 'max)
				400))) 
	    (fill-prefix "")
	    (w3-last-fill-pos (point-min))
            retval
	    (w3-do-incremental-display nil)
	    (hr-regexp  (concat "^"
				(regexp-quote 
				 (make-string 5 (w3-horizontal-rule-char)))
				"*$"))
	    )
	;;(push 'left  w3-display-alignment-stack)
	(push (if (eq minmax 'max) 'nowrap) w3-display-whitespace-stack)
	(while tree
	  (push (cons '*td w3--args) w3-display-open-element-stack)
	  (w3-display-node (pop tree)))
	(pop w3-display-whitespace-stack)
	(goto-char (point-min))
	(while (re-search-forward hr-regexp nil t)
	  (replace-match "" t t))
	(goto-char (point-min))
	(while (not (eobp))
	  ;; loop invariant: at beginning of uncounted line
	  (end-of-line)
	  (skip-chars-backward " ")
	  (setq retval (cons (current-column)
			     retval))
	  (beginning-of-line 2))
	(if (= (point-min) (point-max))
	    (setq retval 0)
	  (setq retval (apply 'max (cons 0 retval))))
	(delete-region (point-min) (point-max))
	retval))))

(defun w3-display-table-dimensions (node)
  ;; fill-column sets maximum width
  (let (min-vector
	max-vector
	rows cols
	;;(w3-form-elements (and (boundp 'w3-form-elements) w3-form-elements))
	(table-info (assq 'w3-table-info (cadr node)))) 
    
    (if table-info 
	(setq min-vector (nth 1 table-info)
	      max-vector (nth 2 table-info)
	      rows       (nth 3 table-info)
	      cols       (nth 4 table-info))

      (push (cons '*table-autolayout w3--args) w3-display-open-element-stack)
      (let (content
	    cur
	    (table-spans (list nil))	; don't make this '(nil) 
	    ptr
	    col
	    constraints
	    
	    colspan rowspan min max)
	(setq content (nth 2 node))
	(setq rows 0 cols 0)
	(while content
	  (setq cur (pop content))
	  (if (stringp cur)
	      nil
	    (case (car cur)
	      ((thead tfoot col colgroup)
	       (if (nth 2 cur)
		   (setq content (append (nth 2 cur) content))))
	      (tr
	       (setq col 0)
	       (setq rows (1+ rows))
	       (setq ptr table-spans)
	       (dolist (td (nth 2 cur))
                 (setq colspan (string-to-number (or (let ((attr (cdr-safe (assq 'colspan (nth 1 td)))))
                                                       (unless (zerop (length attr)) attr)) "1"))
                       rowspan (string-to-number (or (let ((attr (cdr-safe (assq 'rowspan (nth 1 td)))))
                                                       (unless (zerop (length attr)) attr))"1"))
                       min  (w3-size-of-tree  (nth 2 td) 'min)
                       max  (w3-size-of-tree  (nth 2 td) 'max)
                       )
                 (while (eq (car-safe (car-safe (cdr ptr))) col)
                   (setq col (+ col (cdr (cdr (car (cdr ptr))))))
                   (if (= 0 (decf (car (cdr (car (cdr ptr))))))
                       (pop (cdr ptr))
                     (setq ptr (cdr ptr))))
                 (push (list col colspan min max)
                       constraints)
                 (if (= rowspan 1) nil
                   (push (cons col (cons (1- rowspan) colspan)) (cdr ptr))
                   (setq ptr (cdr ptr)))
                 (setq col (+ col colspan))
                 )
	       (while (cdr ptr)
		 (if (= 0 (decf (car (cdr (car (cdr ptr))))))
		     (pop (cdr ptr))
		   (setq ptr (cdr ptr))))
	       (setq cols (max cols col))
	       )
	      (caption
	       nil)
	      (otherwise
	       (setq content (nth 2 cur)))
	      )
	    )
	  )
	(setq constraints (sort constraints
                                (lambda (a b) (< (cadr a) (cadr b))))
	      min-vector (make-vector cols 0)
	      max-vector (make-vector cols 0))
	(let (start end i mincellwidth maxcellwidth)
	  (dolist (c constraints)
            (cond ((= (cadr c) 1) 
                   (aset min-vector (car c) 
                         (max (aref min-vector (car c))
                              (nth 2 c)))
                   (aset max-vector (car c) 
                         (max (aref max-vector (car c))
                              (nth 3 c))))
                  (t 
                   (setq start (car c)
                         end (+ (car c) (cadr c))
                         mincellwidth 0
                         maxcellwidth 0
                         i start)
                   (while (< i end)
                     (setq mincellwidth (+ mincellwidth
                                           (aref min-vector i))
                           maxcellwidth (+
                                         maxcellwidth
                                         (aref max-vector i))
                           i (1+ i)))
                   (setq i start)
                   (if (= mincellwidth 0)
                       ;; if existing width is 0 divide evenly
                       (while (< i end)
                         (aset min-vector i
                               (/ (nth 2 c) (cadr c)))
                         (aset max-vector i
                               (/ (nth 3 c) (cadr c)))
                         (setq i (1+ i)))
                     ;; otherwise weight it by existing widths
                     (while (< i end)
                       (aset min-vector i
                             (max (aref min-vector i)
                                  (/ (* (nth 2 c)
                                        (aref min-vector i))
                                     mincellwidth)))
                       (aset max-vector i
                             (max (aref max-vector i)
                                  (/ (* (nth 3 c)
                                        (aref max-vector i))
                                     maxcellwidth)))
                       (setq i (1+ i))))
                   )))))
      (push (cons 'w3-table-info
		  (list min-vector max-vector rows cols))
	    (cadr node))
      (pop w3-display-open-element-stack))
    
    (let (max-width
	  min-width 
	  ret-vector
	  col
	  )
    

      (setq max-width (apply '+ (append max-vector (list cols 1))))
      (setq min-width (apply '+ (append min-vector (list cols 1))))

      ;; the comments in the cond are excerpts from rfc1942 itself
      (cond 
       ;;   1.  The minimum table width is equal to or wider than the available
       ;;       space. In this case, assign the minimum widths and allow the
       ;;       user to scroll horizontally. For conversion to braille, it will
       ;;       be necessary to replace the cells by references to notes
       ;;       containing their full content. By convention these appear
       ;;       before the table.
       ((>= min-width fill-column)
	(setq ret-vector min-vector))
     
       ;;   2.  The maximum table width fits within the available space. In
       ;;       this case, set the columns to their maximum widths.
       ((<= max-width fill-column)
	(setq ret-vector max-vector))
     
       ;;   3.  The maximum width of the table is greater than the available
       ;;       space, but the minimum table width is smaller. In this case,
       ;;       find the difference between the available space and the minimum
       ;;       table width, lets call it W. Lets also call D the difference
       ;;       between maximum and minimum width of the table.
     
       ;;       For each column, let d be the difference between maximum and
       ;;       minimum width of that column. Now set the column's width to the
       ;;       minimum width plus d times W over D. This makes columns with
       ;;       large differences between minimum and maximum widths wider than
       ;;       columns with smaller differences.
       (t
	(setq ret-vector (make-vector cols 0))
	(let ((W (- fill-column min-width))
	      (D (- max-width min-width))
	      d extra)
	  (setq col 0)
	  (while (< col (length ret-vector))
	    (setq d (- (aref max-vector col)
		       (aref min-vector col)))
	    (aset ret-vector col 
		  (+ (aref min-vector col)
		     (/ (* d W) D)))
	    (setq col (1+ col)))
	  (setq extra (- fill-column
			 (apply '+ (append ret-vector
					   (list (length ret-vector) 1))))
		col 0)
	  (while (and (< col (length ret-vector)) (> extra 0))
	    (if (= 1 (- (aref max-vector col) (aref ret-vector col) ))
		(aset ret-vector col (1+ (aref ret-vector col))))
	    (setq extra (1- extra)
		  col (1+ col)))
	  )))
      (list rows cols ret-vector))))

(defun w3-display-table (node)
  (let* ((dimensions (w3-display-table-dimensions node))
	 (num-cols (max (cadr dimensions) 1))
	 (num-rows (max (car dimensions) 1))
	 (column-dimensions (caddr dimensions))
	 (row-dimensions (make-vector num-rows 0))
	 (row-index 0)
	 (table-width (apply '+ (append column-dimensions (list num-cols 1)))))
    (cond
     ((or (<= (cadr dimensions) 0) (<= (car dimensions) 0))
      ;; We have an invalid table
      nil)
     ((assq '*table-autolayout w3-display-open-element-stack)
      ;; don't bother displaying the table if all we really need is the size
      (progn (insert-char ?T table-width) (insert "\n")))
     (t
      (let* ((tag  (nth 0 node))
	     (args (nth 1 node))
	     (border-node (cdr-safe (assq 'border args)))
	     (border (or w3-display-table-force-borders
			 (and border-node (or
					   (/= 0 (string-to-number border-node))
					   (string= "border" border-node)))))
	     (border-char (unless border ? ))
	     (valign nil)
	     ;; (align nil)
	     (content (nth 2 node))
	     (avgwidth (/ (- fill-column num-cols num-cols) num-cols))
	     (formatted-cols (make-vector num-cols nil))
	     (table-rowspans (make-vector num-cols 0))
	     (table-colspans (make-vector num-cols 1))
	     (whole-table-rowspans (make-vector num-rows nil))
	     (whole-table-colspans (make-vector num-rows nil))
	     (prev-colspans  (make-vector num-cols 0))
	     (prev-rowspans  (make-vector num-cols 0))
	     (table-colwidth (make-vector num-cols 0))
	     (fill-prefix "")
	     (height nil)
	     (cols nil)
	     (row 0)
	     (this-rectangle nil)
	     (inhibit-read-only t)
	     (i 0)
	     (origin (- (point) w3-display-current-cell-offset))
	     (inside-table-p (and w3-display-current-row w3-display-current-col))
	     (incoming-w3-table-structure w3-table-structure)
	     )
	;; for emacspeak
	(when (featurep 'emacspeak)
	  (if inside-table-p
	      (let ((surrounding-table-descr (car incoming-w3-table-structure)))
		(setq w3-table-structure
		      (list (list w3-display-current-row w3-display-current-col origin origin nil)))
		(setcar (cddr surrounding-table-descr)
			(append w3-table-structure
				(caddr surrounding-table-descr))))
	    (setq w3-table-structure (list (list origin origin nil))) ; to be completed (see below)
	    (setq incoming-w3-table-structure (append w3-table-structure
						      incoming-w3-table-structure))))

	(push (cons tag args) w3-display-open-element-stack)

	(if (memq 'nowrap w3-display-whitespace-stack)
	    (setq fill-prefix "")
	  (case (car w3-display-alignment-stack)
	    (center
	     (w3-set-fill-prefix-length
	      (max 0 (/ (- fill-column table-width) 2))))
	    (right
	     (w3-set-fill-prefix-length
	      (max 0 (- fill-column table-width))))
	    (t
	     (setq fill-prefix ""))))
	(while content
	  (case (caar content)
	    ((thead tfoot col colgroup)
	     (if (nth 2 (car content))
		 (setq content (append (nth 2 (car content)) (cdr content)))
	       (setq content (cdr content))))
	    (tr
	     (setq w3-display-css-properties (css-get
					      (nth 0 (car content))
					      (nth 1 (car content))
					      w3-current-stylesheet
					      w3-display-open-element-stack))
	     (setq cols (nth 2 (car content))
		   valign (or (cdr-safe (assq 'valign (nth 1 (car content))))
			      (w3-get-style-info 'vertical-align))
		   ;; align  (or (cdr-safe (assq 'align  (nth 1 (car content))))
		   ;;            (w3-get-style-info 'text-align))
		   content (cdr content)
		   row (1+ row))
	     (if (and valign (stringp valign))
		 (setq valign (intern (downcase valign))))
	     ;; this is iffy
	     ;;(if align (push (intern (downcase align)) w3-display-alignment-stack))
	     (save-excursion
	       (save-restriction
		 (let* ((cell-origin (point))
			(w3-display-current-cell-offset (1+ cell-origin)))
		   (narrow-to-region cell-origin cell-origin)
		   (setq fill-column avgwidth
			 w3-last-fill-pos (point-min)
			 i 0)
		   ;; skip over columns that have leftover content
		   (while (and (< i num-cols)
			       (/= 0 (aref table-rowspans i)))
		     (setq i (+ i (max 1 (aref table-colspans i)))))
		   ;; Need to push the properties for the table onto the stack
		   (setq w3-display-css-properties (css-get
						    tag
						    args
						    w3-current-stylesheet
						    w3-display-open-element-stack))
		   (push (w3-face-for-element (list tag args nil)) w3-active-faces)
		   (push (w3-voice-for-element) w3-active-voices)
		   (push (cons tag args) w3-display-open-element-stack)
		   (while cols
		     ;; And need to push these bogus placeholders on there
		     ;; so that w3-display-node doesn't pop off the real face
		     ;; or voice we just put in above.
		     (push nil w3-active-faces)
		     (push nil w3-active-voices)
		     (let* ((node (car cols))
			    (attributes (nth 1 node))
			    (colspan-attr (cdr-safe (assq 'colspan attributes)))
			    (colspan (string-to-number
				      (or (unless (zerop (length colspan-attr))
					    colspan-attr)
					  "1")))
			    (rowspan-attr (cdr-safe (assq 'rowspan attributes)))
			    (rowspan (string-to-number
				      (or (unless (zerop (length rowspan-attr))
					    rowspan-attr)
					  "1")))
			    fill-column
			    (fill-prefix "")
			    (w3-do-incremental-display nil)
			    (indent-tabs-mode nil)
			    c e
			    )

		       (aset table-colspans i colspan)
		       (aset table-rowspans i rowspan)

		       (setq fill-column 0)
		       (setq c i
			     e (+ i colspan))
		       (while (< c e)
			 (setq fill-column (+ fill-column 
					      (aref column-dimensions c)
					      1)
			       c (1+ c)))
		       (setq fill-column (1- fill-column))
		       (aset table-colwidth i fill-column)

		       (setq w3-last-fill-pos (point-min))
		       (push (cons (nth 0 node) (nth 1 node))
			     w3-display-open-element-stack)
		       (let ((w3-display-current-row row)
			     (w3-display-current-col (1+ i)))
			 (w3-display-node node))
		       (setq fill-column (aref table-colwidth i))
		       (if w3-display-table-cut-words-p
			   (w3-display-table-cut)
			 (w3-display-line-break 1))
		       (setq cols (cdr cols))
		       (goto-char (point-min))
		       (skip-chars-forward "\t\n\r")
		       (beginning-of-line)
		       (delete-region (point-min) (point))
		       (goto-char (point-max))
		       (skip-chars-backward " \t\n\r")
		       (delete-region (point) (point-max))
		       (if (>= fill-column (current-column))
			   (let ((opos (point)))
			     (insert-char ?  (- fill-column (current-column)) t)
			     (remove-text-properties opos
						     (point)
						     '(w3-hyperlink-info nil))))
		       (goto-char (point-min))
		       ;; This gets our text properties out to the
		       ;; end of lines for table rows/cells with backgrounds
		       (while (not (eobp))
			 (re-search-forward "$" nil t)
			 (if (>= fill-column (current-column))
			     (let ((opos (point)))
			       (insert-char ?  (- fill-column (current-column)) t)
			       (remove-text-properties opos
						       (point)
						       '(w3-hyperlink-info nil))))
			 (or (eobp) (forward-char 1)))
		       (aset formatted-cols i (extract-rectangle (point-min) (point-max)))
		       (delete-region (point-min) (point-max))
		       (let ((j (1- colspan)))
			 (while (> j 0)
			   (aset table-colspans (+ i j) 0)
			   (setq j (1- j))))		
		       (setq i (+ i colspan))
		       ;; skip over columns that have leftover content
		       (while (and (< i num-cols)
				   (/= 0 (aref table-rowspans i)))
			 (setq i (+ i (max 1 (aref table-colspans i)))))
		       ))
		   (pop w3-display-open-element-stack)
		   (pop w3-active-faces)
		   (pop w3-active-voices)
		   (w3-pop-all-face-info)
		   ;; finish off the columns
		   (while (< i num-cols)
		     (aset table-colwidth i (aref column-dimensions i))
		     (aset table-colspans i 1)
		     (setq i (1+ i))
		     (while (and (< i num-cols)
				 (/= 0 (aref table-rowspans i)))
		       (setq i (+ i (max 1 (aref table-colspans i))))))

		   ;; on the last row empty any pending rowspans per the rfc
		   (if content nil
		     (fillarray table-rowspans 1)) 

		   ;; Find the tallest rectangle that isn't a rowspanning cell
		   (setq height 0 
			 i 0)
		   (while (< i num-cols)
		     (if (= 1 (aref table-rowspans i))
			 (setq height (max height (length (aref formatted-cols i)))))
		     (setq i (+ i (max 1 (aref table-colspans i)))))

		   ;; Make all rectangles the same height
		   (setq i 0)
		   (while (< i num-cols)
		     (setq this-rectangle (aref formatted-cols i))
		     (if (> height (length this-rectangle))
			 (let ((colspan-fill-line
				(make-string (abs (aref table-colwidth i)) ? )))
			   (case valign
			     ((center middle)
			      (aset formatted-cols i
				    (append (make-list (/ (- height (length this-rectangle)) 2) 
						       colspan-fill-line)
					    this-rectangle)))
			     (bottom
			      (aset formatted-cols i 
				    (append (make-list (- height (length this-rectangle))
						       colspan-fill-line)
					    this-rectangle))))))
		     (setq i (+ i (max 1 (aref table-colspans i))))))))
	     

	     ;; fix broken colspans (this should only matter on illegal tables)
	     (setq i 0)
	     (while (< i num-cols)
	       (if (= (aref table-colspans i) 0)
		   (aset table-colspans i 1))
	       (setq i (+ i (aref table-colspans i))))

	     ;; Insert a separator 
	     (insert fill-prefix)
	     (setq i 0)
	     (let (rflag bflag tflag lflag)
	       (while (< i num-cols)
		 (setq rflag (= (aref prev-rowspans i) 0)
		       bflag (/= (aref table-colspans i) 0)
		       tflag (/= (aref prev-colspans  i) 0))
		 ;; insert the vertical dividers if necessary
		 (w3-insert-terminal-char
		  (w3-table-lookup-char lflag tflag rflag bflag border-char))
		 (setq lflag t)
		 (cond
		  ((= (aref prev-rowspans i) 0)
		   ;; First row, insert the top horizontal divider
                   ;;BL Everything in this function commented out with ;;BL is
                   ;;BL done so to borderless tables work better.  This was an
                   ;;BL attempt to not show the 'spaces' border around the
                   ;;BL table, to save screen real estate, but it messes up
                   ;;BL indentation on cell columns.
                   ;;BL(if border
		       (w3-insert-terminal-char
			(w3-table-lookup-char t nil t nil border-char) 
			(aref column-dimensions i));;BL)
		   (setq i (1+ i)))
		  ((car (aref formatted-cols i))
		   ;; Slap in the rows
		   (insert (pop (aref formatted-cols i)))
		   (setq lflag nil)
		   (setq i (+ i (max (aref table-colspans i)
				     (aref prev-colspans  i) 1))))
		  (t
		   (if border
		       (insert-char ?  (aref table-colwidth i) t))
		   (setq lflag nil)
		   (setq i (+ i (max (aref table-colspans i)
				     (aref prev-colspans  i) 1))))))
               ;;BL (if (not border)
               ;;BL     nil
		 (w3-insert-terminal-char
		  (w3-table-lookup-char lflag (/= row 1) nil t border-char))
		 (insert "\n"));;BL)
	     
	     ;; recalculate height (in case we've shortened a rowspanning cell)
	     (setq height 0 
		   i 0)
	     (while (< i num-cols)
	       (if (= 1 (aref table-rowspans i))
		   (setq height (max height (length (aref formatted-cols i)))))
	       (setq i (+ i (max 1 (aref table-colspans i)))))

	     (aset whole-table-rowspans row-index (copy-sequence table-rowspans))
	     (aset whole-table-colspans row-index (copy-sequence table-colspans))
	     
	     ;; update row-dimensions
	     (aset row-dimensions row-index (1+ height))
	     (setq row-index (1+ row-index))

	     ;; Insert a row back in original buffer
	     (while (> height 0)
	       (insert fill-prefix)
	       (w3-insert-terminal-char (w3-table-lookup-char nil t nil t border-char))
	       (setq i 0)
	       (while (< i num-cols)
		 (if (car (aref formatted-cols i))
		     (insert (pop (aref formatted-cols i))) 
		   (insert-char ?  (aref table-colwidth i) t)) 
		 (w3-insert-terminal-char (w3-table-lookup-char nil t nil t border-char))
		 (setq i (+ i (max (aref table-colspans i) 1))))
	       (insert "\n")
	       ;;(and w3-do-incremental-display (w3-pause))
	       (setq height (1- height)))
	     
	     (setq i 0)
	     (while (< i num-cols)
	       (if (> (aref table-rowspans i) 0)
		   (decf (aref table-rowspans i)))
	       (incf i))
	     
	     (setq prev-rowspans (copy-sequence table-rowspans))
	     (setq prev-colspans (copy-sequence table-colspans))
	 
	     (and w3-do-incremental-display (w3-pause))
	     )
	    (caption
	     (let ((left (length fill-prefix))
		   (fill-prefix "")
		   (fill-column table-width)
		   (start (point)))
	       (w3-display-node (pop content))
	       (indent-rigidly start (point) left)))
	    (otherwise			
	     (delete-horizontal-space)
	     (setq content (nth 2 (car content))))
	    ))
	(if (= (length column-dimensions) 0) nil
	  (insert fill-prefix)
	  (setq i 0)
	  (let (tflag lflag)
	    (while (< i num-cols)
	      (setq tflag (/= (aref prev-colspans  i) 0))
	      (w3-insert-terminal-char (w3-table-lookup-char lflag tflag t nil border-char))
	      (setq lflag t)
	      (w3-insert-terminal-char
	       (w3-table-lookup-char t nil t nil border-char)
	       (aref column-dimensions i))
	      (setq i (1+ i)))
	    (w3-insert-terminal-char
	     (w3-table-lookup-char t t nil nil border-char))
	    (insert "\n")))

	;; for emacspeak
	(when (featurep 'emacspeak)
	  ;; completion of table info
	  (let ((dimensions (list num-rows num-cols
				  row-dimensions column-dimensions
				  whole-table-rowspans whole-table-colspans)))
	    (cond (inside-table-p
		   (setcar (cddr (cdar w3-table-structure)) (- (point) w3-display-current-cell-offset 1))
		   (setcdr (cddr (cddar w3-table-structure)) dimensions))
		  (t
		   (setcar (cdar w3-table-structure) (1- (point)))
		   (setcdr (cddar w3-table-structure) dimensions))))
	  (setq w3-table-structure incoming-w3-table-structure))
	)
      (pop w3-display-open-element-stack)))))



(defun w3-display-create-unique-id ()
  (let* ((date (current-time-string))
	 (dateinfo (and date (timezone-parse-date date)))
	 (timeinfo (and date (timezone-parse-time (aref dateinfo 3)))))
    (if (and dateinfo timeinfo)
	(concat (aref dateinfo 0)	; Year
		(aref dateinfo 1)	; Month
		(aref dateinfo 2)	; Day
		(aref timeinfo 0)	; Hour
		(aref timeinfo 1)	; Minute 
		(aref timeinfo 2)	; Second
		)
      "HoplesSLYCoNfUSED")))

(defun w3-display-chop-into-table (node cols)
  ;; Chop the content of 'node' up into 'cols' columns suitable for inclusion
  ;; as the content of a table
  (let ((content (nth 2 node))
	(items nil)
	(rows nil))
    (setq cols (max cols 1))
    (while content
      (push (list 'td nil (list (pop content))) items)
      (if (= (length items) cols)
	  (setq rows (cons (nreverse items) rows)
		items nil)))
    (if items				; Store any leftovers
	(setq rows (cons (nreverse items) rows)
	      items nil))
    (while rows
      (push (list 'tr nil (pop rows)) items))
    items))

(defun w3-fix-color (color)
  (if (and color
	   (string-match "^[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]$" color))
      (concat "#" color)
    color))

(defun w3-display-normalize-form-info (args)
  (let* ((plist (w3-alist-to-plist args))
	 (type (intern (downcase
			(or (plist-get plist 'type) "text"))))
	 (name (plist-get plist 'name))
	 (value (or (plist-get plist 'value) ""))
	 (size (if (plist-get plist 'size)
		   (string-to-number (plist-get plist 'size))))
	 (maxlength (if (plist-get plist 'maxlength)
			(string-to-number
			 (plist-get plist 'maxlength))))
	 (default value)
	 (checked (assq 'checked args)))
    (if (memq type '(checkbox radio)) (setq default checked))
    (if (and (eq type 'checkbox) (string= value ""))
	(setq value "on"))
    (if (and (not (memq type '(submit reset button)))
	     (not name))
	(setq name (symbol-name type)))
    (while (and name (string-match "[\r\n]+" name))
      (setq name (concat (substring name 0 (match-beginning 0))
			 (substring name (match-end 0) nil))))
    (setq plist (plist-put plist 'type type)
	  plist (plist-put plist 'name name)
	  plist (plist-put plist 'value value)
	  plist (plist-put plist 'size size)
	  plist (plist-put plist 'default default)
	  plist (plist-put plist 'internal-form-number w3-current-form-number)
	  plist (plist-put plist 'action w3-display-form-id)
	  plist (plist-put plist 'maxlength maxlength))
    plist))

(defvar w3-resurrect-images-offset nil
  "A-list of image-alt offsets for widgets cut in tables, used in `w3-resurrect-images'.
Format: (((image-alt row column) . offset) ...)")

(defun w3-resurrect-images ()
  (let ((st (point-min))
	(inhibit-read-only t)
	info nd widget)
    (while st
      (if (setq info (get-text-property st 'w3-hyperimage-info))
	  (progn
	    (setq nd (or (next-single-property-change st 'w3-hyperimage-info)
			 (point-max)))
	    (let* ((raw-alt (widget-get info 'alt))
		   ;; remove trailing blanks:
		   (drawn-alt (if (and (stringp raw-alt) (string-match "\\(.*[^ \t\n]+\\)[ \t\n]+$" raw-alt))
				  (substring raw-alt 0 (match-end 1))
				raw-alt))
		   (row (widget-get info 'row))
		   (col (widget-get info 'column))
		   (max (point-max))
		   (offset-elt (assoc (list drawn-alt row col) w3-resurrect-images-offset))
		   (offset (if offset-elt (cdr offset-elt) 0))
		   to-cut new-offset)
	      (delete-region st nd)
	      (goto-char st)
	      (setq widget (apply (function widget-create) info))
	      (if (not (zerop offset))
		  ;; already started on this widget - remove beginning of drawn-alt
		  (delete-region st (+ st offset)))
	      (setq to-cut (- (point-max) max)
		    new-offset (1+ (- nd st)))
	      (cond ((> to-cut 0)
		     ;; cut end of drawn-alt if too long after resurrection
		     (delete-region nd (+ nd to-cut))
		     (if offset-elt
			 (setcdr offset-elt (+ offset new-offset))
		       (setq w3-resurrect-images-offset
			     (cons (cons (list drawn-alt row col) new-offset)
				   w3-resurrect-images-offset))))
		    ((< to-cut 0)
		     (insert (make-string (- to-cut) ? )))))
	    (widget-put widget 'buffer (current-buffer))
	    (w3-maybe-start-image-download widget)
	    (if (widget-get widget :from)
		(add-text-properties (widget-get widget :from)
				     (widget-get widget :to)
				     (list 'html-stack w3-display-open-element-stack)))))
      (setq st (next-single-property-change st 'w3-hyperimage-info)))
    (setq w3-resurrect-images-offset nil)))

(require 'w3-mouse)
(defvar w3-display-hackmap nil "Keymap used for hyperlink widgets")
  
(defun w3-resurrect-hyperlinks ()
  (if (and (not w3-display-hackmap) (featurep 'xemacs))
      (progn
	(setq w3-display-hackmap (make-sparse-keymap))
	(set-keymap-parent w3-display-hackmap widget-button-keymap)
	(define-key w3-display-hackmap (vector w3-mouse-button3) 'w3-popup-menu)))
  (let ((st (point-min))
	(nd (point-min))
	(inhibit-read-only t)
	info)
    (while st
      (if (setq info (get-text-property st 'w3-hyperlink-info))
	  (progn
	    (while (memq (char-after st) '(?\t ?\r ?\n ?\ ))
	      (setq st (1+ st)))
	    (setq nd (or (next-single-property-change st 'w3-hyperlink-info)
			 (point-max)))
	    (apply 'widget-convert-text 'link st nd st nd (nconc
							   (list :start st
								 :button-keymap w3-display-hackmap
								 :end nd)
							   info))))
      (setq st (next-single-property-change st 'w3-hyperlink-info)))))

(defun w3-display-convert-arglist (args)
  (let ((rval nil)
	(newsym nil)
	(cur nil))
    (while (setq cur (pop args))
      (setq newsym (intern (concat ":" (symbol-name (car cur))))
	    rval (plist-put rval newsym (cdr cur))))
    rval))

(defvar w3-auto-run-java nil
  "*Non-nil means cause Java applets to run automatically in another process.")

(defun w3-display-handle-java (node)
  (let ((options (nth 1 node))
	(params (mapcar (lambda (subnode)
			  (if (eq (car-safe subnode) 'param)
			      (cons (cdr-safe (assq 'name (nth 1 subnode)))
				    (cdr-safe (assq 'value (nth 1 subnode))))))
			(nth 2 node))))
    (setq options (delq nil options)
	  params (delq nil params))
    (if (not (assq 'codebase options))
	(push (cons 'codebase (url-view-url t)) options))
    (w3-java-run-applet options params)))

(defvar filladapt-mode)
(defvar voice-lock-mode)
(defvar widget-push-button-gui)

(defun w3-display-node (node &optional nofaces)
  (let (
	(content-stack (list (list node)))
	(right-margin-stack (list fill-column))
	(left-margin-stack (list 0))
	(inhibit-read-only t)
	(widget-push-button-gui nil)
	node
	insert-before
	insert-after
	tag
	args
	content
	w3--hyperlink-info
	w3--hyperimage-info
	break-style
	id
	last-element
	)
    (while content-stack
      (setq content (pop content-stack))
      (pop w3-active-faces)
      (pop w3-active-voices)
      (w3-display-progress-meter)
      (setq last-element (pop w3-display-open-element-stack))
      (case (car last-element)
	;; Any weird, post-display-of-content stuff for specific tags
	;; goes here.   Couldn't think of any better way to do this when we
	;; are iterative.  *sigh*
	(a
	 (if (not w3--hyperlink-info)
	     nil
	   (add-text-properties (car w3--hyperlink-info) (point)
				(list
				 'duplicable t
				 'balloon-help #'w3-balloon-help-callback
				 'start-open t
				 'end-open t
				 'rear-nonsticky t
				 'w3-hyperlink-info (cadr w3--hyperlink-info))))
	 (setq w3--hyperlink-info nil))
	(img
	 (if w3--hyperimage-info
	     (add-text-properties (car w3--hyperimage-info) (point)
				  (list
				   'duplicable t
				   'start-open t
				   'end-open t
				   'rear-nonsticky t
				   'w3-hyperimage-info (cadr w3--hyperimage-info))))
	 (setq w3--hyperimage-info nil))
	((ol ul dl dir menu)
	 (pop w3-display-list-stack))
	(label
	 (if (and (markerp w3-display-label-marker)
		  (marker-position w3-display-label-marker)
		  (marker-buffer w3-display-label-marker))
	     (push (cons (or (cdr-safe (assq 'for (cdr last-element)))
			     (cdr-safe (assq 'id (cdr last-element)))
			     "unknown")
			 (buffer-substring w3-display-label-marker (point)))
		   w3-form-labels)))
	(otherwise
	 nil))
      (if (car insert-after)
	  (w3-handle-string-content (car insert-after)))
      (pop insert-after)
      (w3-display-handle-end-break)
      (w3-pop-all-face-info)
      ;; Handle the element's content
      (while content
	(w3-display-progress-meter)
	(if (stringp (car content))
	    (w3-handle-string-content (pop content))
	  (setq node (pop content)
		tag (nth 0 node)
		args (nth 1 node)
		id (or (w3-get-attribute 'name args)
		       (w3-get-attribute 'id args))
		)
	  ;; This little bit of magic takes care of inline styles.
	  ;; Evil Evil Evil, but it appears to work.
	  (if (w3-get-attribute 'style args)
	      (let ((unique-id (or (w3-get-attribute 'id args)
				   (w3-display-create-unique-id)))
		    (sheet "")
		    (class (assq 'class args)))
		(setq sheet (format "%s.%s { %s }\n" tag unique-id
				    (w3-get-attribute 'style args)))
		(if class
		    (setcdr class (cons unique-id (cdr class)))
		  (setf (nth 1 node) (cons (cons 'class (list unique-id))
					   (nth 1 node))))
		(setf (nth 1 node) (cons (cons 'id unique-id) (nth 1 node)))
		(w3-handle-style (list 'data sheet
				       'notation "text/css"))))
	  (setq w3-display-css-properties (css-get
					   (nth 0 node)
					   (nth 1 node)
					   w3-current-stylesheet
					   w3-display-open-element-stack))
	  (push (w3-get-style-info 'display) break-style)
	  (push (w3-get-style-info 'insert-after) insert-after)
	  (setq insert-before (w3-get-style-info 'insert-before))
	  (w3-display-handle-break)
	  (if (w3-node-visible-p)
	      nil
	    (setq insert-before nil
		  tag '*invisible)
	    (setcar insert-after nil))
	  (if insert-before
	      (w3-handle-string-content insert-before))
	  (if nofaces
	      nil
	    (push (w3-face-for-element node) w3-active-faces)
	    (push (w3-voice-for-element) w3-active-voices))
	  (setq insert-before nil)
	  (if id
	      (setq w3-id-positions (cons
				     (cons (intern id)
					   (set-marker (make-marker)
						       (point-max)))
				     w3-id-positions)))
	  (case tag
	    (a				; Hyperlinks
	     (let* (
		    (st nil)
		    (old-props w3-display-css-properties)
		    (active-face nil)
		    (visited-face nil)
		    (munged (copy-sequence args)))
	       (if (assq 'class munged)
		   (push ":active" (cdr (assq 'class munged)))
		 (setq munged (cons (cons 'class '(":active")) munged)))
	       (setq w3-display-css-properties (css-get
						tag
						munged
						w3-current-stylesheet
						w3-display-open-element-stack))
	       (setq active-face (w3-face-for-element (list tag munged nil)))
	       (w3-pop-all-face-info)
	       (setq munged (copy-sequence args))
	       (if (assq 'class munged)
		   (push ":visited" (cdr (assq 'class munged)))
		 (setq munged (cons (cons 'class '(":visited")) munged)))
	       (setq w3-display-css-properties (css-get
						tag
						munged
						w3-current-stylesheet
						w3-display-open-element-stack))
	       (setq visited-face (w3-face-for-element (list tag munged nil)))
	       (w3-pop-all-face-info)
	       (setq w3-display-css-properties old-props)
	       (if (w3-get-attribute 'href args)
		   (setq st (point)
			 w3--hyperlink-info
                         (list
                          st
                          (append
                           (list :args nil
                                 :value "" :tag ""
                                 :action 'w3-follow-hyperlink
                                 :button-face '(nil)
                                 :active-face active-face
                                 :visited-face visited-face
                                 :from (set-marker
                                        (make-marker) st)
                                 :help-echo 'w3-widget-echo
                                 :emacspeak-help 'w3-widget-echo
                                 )
                           (w3-display-convert-arglist args)))))
	       (w3-handle-content node)
	       )
	     )
	    ((ol ul dl menu)
	     (push (if (or (w3-get-attribute 'start args)
			   (w3-get-attribute 'seqnum args))
		       (1- (string-to-number (or (w3-get-attribute 'start args)
					      (w3-get-attribute 'seqnum args))))
		     0) w3-display-list-stack)
	     (w3-handle-content node))
	    (dir
	     (push 0 w3-display-list-stack)
	     (setq node
		   (list tag args
			 (list
			  (list 'table nil
				(w3-display-chop-into-table node 3)))))
	     (w3-handle-content node))
	    (multicol
	     (setq node (list tag args
			      (list
			       (list 'table nil
				     (w3-display-chop-into-table node 2)))))
	     (w3-handle-content node))
	    (img			; inlined image
             (w3-handle-image args)
	     (w3-handle-empty-tag))
	    (frameset
	     (if w3-display-frames
		 (let ((frames (nth 2 node))
		       (frameset-cardinal 0)
		       (cols (cdr-safe (assq 'cols args)))
		       (rows (cdr-safe (assq 'rows args))))
		   (while (and frames (memq (car (car frames)) '(frame frameset)))
		      (setq frameset-cardinal (1+ frameset-cardinal)
			    frames (cdr frames)))
		   (push (list 'frameset
			       frameset-cardinal
			       (if (w3-frameset-dimensions-p cols)
				   (assq 'cols args)
				 (if (w3-frameset-dimensions-p rows)
				     (assq 'rows args))))
			 w3-frameset-structure)
		   (w3-handle-content node))
	       (w3-handle-content node)))
	    (frame
	     (if w3-display-frames
		 (let* ((href (or (w3-get-attribute 'src args)
				  (w3-get-attribute 'href args)))
			(name (or (w3-get-attribute 'name args)
				  (w3-get-attribute 'title args)
				  (w3-get-attribute 'alt args)
				  "Unknown frame name")))
		   (push (list 'frame name href) w3-frameset-structure)
		   (w3-handle-content
		    (list tag args
			  (list
			   (list 'p nil
				 (list
				  (list 'a
					(cons (cons 'href href)
					      args)
					(list "Fetch frame: " name))))))))
	       (w3-handle-empty-tag)))
	    (noframes
	     (if w3-display-frames
		 (w3-handle-empty-tag)
	       (w3-handle-content node)))
	    (applet			; Wow, Java
	     (if w3-auto-run-java
		 (w3-display-handle-java node)
	       (w3-handle-content node)))
	    (script			; Scripts
	     (w3-handle-empty-tag))
	    ((embed object)		; Embedded images/content
	     (w3-handle-content node)
	     )
	    (hr				; Cause line break & insert rule
	     (let* ((perc (or (w3-get-attribute 'width args)
			      (w3-get-style-info 'width)
			      "100%"))
		    (width nil))
	       (if (stringp perc)
		   (setq perc (/ (min (string-to-number perc) 100) 100.0)
			 width (truncate (* fill-column perc)))
		 (setq width perc))
	       (w3-insert-terminal-char (w3-horizontal-rule-char) width)
	       (w3-handle-empty-tag)))
	    (map			; Client side imagemaps
	     (let ((name (or (w3-get-attribute 'name args)
			     (w3-get-attribute 'id args)
			     "unnamed"))
		   (areas
		    (mapcar
		     (function
		      (lambda (node)
			(let* ((args (nth 1 node))
			       (type (downcase (or
						(w3-get-attribute 'shape args)
						"rect")))
			       (coords (w3-decode-area-coords
					(or (cdr-safe
					     (assq 'coords args)) "")))
			       (alt (w3-get-attribute 'alt args))
			       (href (if (assq 'nohref args)
					 t
				       (or (w3-get-attribute 'src args)
					   (w3-get-attribute 'href args))))
			       )
			  (vector type coords href alt))
			)
		      )
		     (nth 2 node))))
	       (setq w3-imagemaps (cons (cons name areas) w3-imagemaps)))
	     (w3-handle-empty-tag)
	     )
	    (note
	     ;; Ewwwwhhh.  Looks gross, but it works.  This converts a
	     ;; <note> into a two-cell table, so that things look all
	     ;; pretty.
	     (setq node
		   (list 'note nil
			 (list
			  (list 'table nil
				(list
				 (list 'tr nil
				       (list
					(list 'td (list 'align 'right)
					      (list
					       (concat
						(or (w3-get-attribute 'role args)
						    "CAUTION") ":")))
					(list 'td nil
					      (nth 2 node)))))))))
	     (w3-handle-content node)
	     )
	    (table
             (let ((w3--args args))
               (w3-display-table node))
	     (setq w3-last-fill-pos (point))
	     (w3-handle-empty-tag)
	     )
	    (isindex
	     (let ((prompt (or (w3-get-attribute 'prompt args)
			       "Search on (+ separates keywords): "))
		   action node)
	       (setq action (or (w3-get-attribute 'src args)
				(w3-get-attribute 'href args)
				(url-view-url t)))
	       (if (and prompt (string-match "[^: \t-]+$" prompt))
		   (setq prompt (concat prompt ": ")))
	       (setq node
		     (list 'isindex nil
			   (list
			    (list 'hr nil nil)
			    (list 'form
				  (list (cons 'action action)
					(cons 'enctype
					      "application/x-w3-isindex")
					(cons 'method "get"))
				  (list
				   prompt
				   (list 'input
					 (list (cons 'type "text")
					       (cons 'name "isindex"))))))))
	       (w3-handle-content node)
	       (setq w3-current-isindex (cons action prompt)))
	     )
	    ((html body)
	     (let ((fore (car (remq nil w3-face-color)))
		   (back (car (remq nil w3-face-background-color)))
		   (pixm (car (remq nil w3-face-background-image)))
		   (alink (w3-get-attribute 'alink args))
		   (vlink (w3-get-attribute 'vlink args))
		   (link  (w3-get-attribute 'link args))
		   (sheet "")
		   )
	       (if link
		   (setq sheet (format "%sa:link { color: %s }\n" sheet
				       (w3-fix-color link))))
	       (if vlink
		   (setq sheet (format "%sa:visited { color: %s }\n" sheet
				       (w3-fix-color vlink))))
	       (if alink
		   (setq sheet (format "%sa:active { color: %s }\n" sheet
				       (w3-fix-color alink))))
	       (if w3-user-colors-take-precedence
		   nil
		 (if (/= (length sheet) 0)
		     (w3-handle-style (list 'data sheet
					    'notation "text/css")))
		 (if (and (w3-get-attribute 'background args)
			  (not pixm))
		     (progn
		       (setq pixm (w3-get-attribute 'background args))
		       (setf (car w3-face-background-image) pixm)))
		 (if (and (w3-get-attribute 'text args) (not fore))
		     (progn
		       (setq fore (w3-fix-color (w3-get-attribute 'text args)))
		       (setf (car w3-face-color) fore)))

		 ;; Here we do some sanity checking of the colors
		 ;; selected by the author.

		 ;; If they specify the foreground and not the
		 ;; background, _AND_ we determine the contrast is not
		 ;; enough, then don't honor the foreground at all.
		 (if (and fore (not back) (not pixm) (w3-display-foreground-useless-p fore))
		     (setq fore nil))

		 ;; If we wanted to be really weird, we could infer
		 ;; the background for them instead of just ignoring
		 ;; the foreground.  But I think this might be too
		 ;; shocking for the average user. :)
		 ;; 
		 ;; (setq back (w3-display-infer-contrasting-color fore)))

		 ;; If they specify the background and not the
		 ;; foreground, _AND_ we determine the contrast is not
		 ;; enough, then infer a new foreground color.
		 (if (and back (not pixm) (not fore) (w3-display-background-useless-p back))
		     (setq fore (w3-display-infer-contrasting-color back)))

		 (setf (car w3-face-color) fore)
		 (setf (car w3-face-background-color) back)

		 (if (not (featurep 'xemacs))
		     (setq w3-display-background-properties (cons fore back))
		   (if pixm
		       (w3-maybe-start-background-image-download pixm 'default))
		   (if fore
		       (font-set-face-foreground 'default fore (current-buffer)))
		   (if back
		       (font-set-face-background 'default back (current-buffer)))))
	       (w3-handle-content node)))
	    (*document
	     (let ((info (mapcar (lambda (x)
                                   (cons x (and (boundp x) (symbol-value x))))
				 w3-persistent-variables)))
	       (if (not w3-display-same-buffer)
		   (set-buffer (generate-new-buffer "Untitled")))
	       (setq w3-current-form-number 0
		     w3-display-open-element-stack nil
		     w3-last-fill-pos (point-min))
	       (setcar right-margin-stack
		       (min (- (or w3-strict-width (window-width))
			       w3-right-margin)
			    (or w3-maximum-line-length
				(window-width))))
	       (condition-case nil
		   (switch-to-buffer (current-buffer))
		 (error (message  "W3 buffer %s is being drawn." (buffer-name (current-buffer)))))

	       (buffer-disable-undo (current-buffer))
	       (dolist (x info)
                 (if (boundp (car x))
                     (set (car x) (cdr x))))
	       ;; ACK!  We don't like filladapt mode!
	       (set (make-local-variable 'filladapt-mode) nil)
	       (set (make-local-variable 'adaptive-fill-mode) nil)
	       (set (make-local-variable 'voice-lock-mode) t)
	       (set (make-local-variable 'w3--cur-viewing-pos) (point-min))
	       (setq w3-current-stylesheet (css-copy-stylesheet
					    w3-user-stylesheet)
		     w3-last-fill-pos (point)
		     fill-prefix "")
	       )
	     (w3-handle-content node)
	     )
	    (*invisible
	     (w3-handle-empty-tag))
	    (meta
	     (let ((name (w3-get-attribute 'name args))
		   (value (or (w3-get-attribute 'content args) "")))
	       ;; http-equiv is dealt with by `w3-fetch-callback'.
	       (if name
		   (setq w3-current-metainfo (cons
					      (cons name value)
					      w3-current-metainfo)))))
	    (link
	     ;; This doesn't handle blank-separated values per the RFC.
	     (w3-parse-link args)
	     (w3-handle-empty-tag))
	    (title
	     (let ((potential-title "")
		   (content (nth 2 node)))
	       (while content
		 (setq potential-title (concat potential-title (car content))
		       content (cdr content)))
	       (setq potential-title (w3-normalize-spaces potential-title))
	       (if (or w3-display-same-buffer
		       (string-match "^[ \t]*$" potential-title))
		   nil
		 (rename-buffer (generate-new-buffer-name
				 (w3-fix-spaces potential-title)))))
	     (w3-handle-empty-tag))
	    (base
	     (setq w3-base-target (cdr-safe (assq 'target args)))
	     (w3-handle-content node))
	    (form
	     (setq w3-current-form-number (1+ w3-current-form-number))
	     (let* ((action (w3-get-attribute 'action args)))
	       (if (not action)
		   (setq args (cons (cons 'action (url-view-url t)) args)))
	       (setq w3-display-form-id (cons
					 (cons 'form-number
					       w3-current-form-number)
					 args))
	       (w3-handle-content node)))
	    (keygen
	     (w3-form-add-element 
	      (w3-display-normalize-form-info 
	       (cons '(type . "keygen")
		     args))
	      w3-active-faces)
	     (w3-handle-empty-tag))
	    (input
	     (w3-form-add-element
	      (w3-display-normalize-form-info args)
	      w3-active-faces)
	     (w3-handle-empty-tag)
	     )
	    (select
	     (let* ((plist (w3-display-normalize-form-info args))
		    (tmp nil)
		    (multiple (assq 'multiple args))
		    (value nil)
		    (name (plist-get plist 'name))
		    (options (mapcar
			      (function
			       (lambda (n)
				 (setq tmp (w3-normalize-spaces
					    (apply 'concat (nth 2 n)))
				       tmp (vector tmp
						   (or
						    (cdr-safe
						     (assq 'value (nth 1 n)))
						    tmp)
						   (assq 'selected (nth 1 n))))
				 (if (assq 'selected (nth 1 n))
				     (setq value (aref tmp 0)))
				 tmp))
			      (nth 2 node))))
	       (if (not value)
		   (setq value (and options (aref (car options) 0))))
	       (setq plist (plist-put plist 'value value))
	       (if multiple
		   (progn
		     (setq options
			   (mapcar
			    (function
			     (lambda (opt)
			       (list 'div nil
				     (list
				      (list 'input
					    (list (cons 'name name)
						  (cons 'type "checkbox")
						  (cons (if (aref opt 2)
							    'checked
							  '__bogus__) "yes")
						  (cons 'value (aref opt 1))))
				      " " (aref opt 0) (list 'br nil nil)))))
			    options))
		     (setq node (list 'p nil options))
		     (w3-handle-content node))
		 (setq options (mapcar (function
					(lambda (x)
					  (cons (aref x 0) (aref x 1))))
				       options))
		 (setq plist (plist-put plist 'type 'option)
		       plist (plist-put plist 'options options))
		 (w3-form-add-element plist w3-active-faces)
		 ;; This should really not be necessary, but some versions
		 ;; of the widget library leave point _BEFORE_ the menu
		 ;; widget instead of after.
		 (goto-char (point-max))
		 (w3-handle-empty-tag))))
	    (textarea
	     (let* ((plist (w3-display-normalize-form-info args))
		    (value (apply 'concat (nth 2 node))))
	       (setq plist (plist-put plist 'type 'multiline)
		     plist (plist-put plist 'value value))
	       (w3-form-add-element plist w3-active-faces))
	     (w3-handle-empty-tag)
	     )
	    (style
	     (w3-handle-style (w3-alist-to-plist
			       (cons (cons 'data (apply 'concat (nth 2 node)))
				     (nth 1 node))))
	     (w3-handle-empty-tag))
	    (label
	     (if (not (markerp w3-display-label-marker))
		 (setq w3-display-label-marker (make-marker)))
	     (set-marker w3-display-label-marker (point))
	     (w3-handle-content node))
	    ;; Emacs-W3 stuff that cannot be expressed in a stylesheet
	    (pinhead
	     ;; This check is so that we don't screw up table auto-layout
	     ;; by changing our text midway through the parse/layout/display
	     ;; steps.
	     (if (nth 2 node)
		 nil
	       (setcar (cddr node)
		       (list
			(if (fboundp 'yow)
			    (yow)
			  "AIEEEEE!  I am having an UNDULATING EXPERIENCE!"))))
	     (w3-handle-content node))
	    (flame
	     (if (nth 2 node)
		 nil
	       (setcar
		(cddr node)
		(list
		 (condition-case ()
		     (concat
		      (sentence-ify
		       (string-ify
			(append-suffixes-hack (flatten (*flame))))))
		   (error
		    "You know, everything is really a graphics editor.")))))
	     (w3-handle-content node))
	    (cookie
	     (if (nth 2 node)
		 nil
	       (setcar
		(cddr node)
		(list
		 (w3-display-get-cookie args))))
	     (w3-handle-content node))
	    ;; Generic formatting - all things that can be fully specified
	    ;; by a CSS stylesheet.
	    (otherwise
	     (w3-handle-content node))
	    )				; case tag
	  )				; stringp content
	)				; while content
      )					; while content-stack
    )
  )

(defun w3-draw-tree (tree)
  ;; The main entry point - wow complicated
  (setq w3-current-stylesheet w3-user-stylesheet)
  (while tree
    (w3-display-node (car tree))
    (setq tree (cdr tree)))
  (w3-display-fix-widgets)
  (let ((inhibit-read-only t))
    (w3-resurrect-images)
    (w3-resurrect-hyperlinks)
    (w3-form-resurrect-widgets)))

(defun time-display (&optional tree)
  ;; Return the # of seconds it took to draw 'tree'
  (let ((st (nth 1 (current-time)))
	(nd nil))
    (w3-draw-tree (or tree w3-last-parse-tree))
    (setq nd (nth 1 (current-time)))
    (- nd st)))


(defun w3-fixup-eol-faces ()
  ;; Remove 'face property at end of lines - underlining screws up stuff
  ;; also remove 'mouse-face property at the beginning and end of lines 
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(remove-text-properties (match-beginning 0) (match-end 0)
				'(face nil mouse-face nil) nil)))))

(defsubst w3-finish-drawing ()
  (let (url glyph widget)
    (while w3-image-widgets-waiting
      (setq widget (car w3-image-widgets-waiting)
	    w3-image-widgets-waiting (cdr w3-image-widgets-waiting)
	    url (widget-get widget :src)
	    glyph (cdr-safe (assoc url w3-graphics-list)))
      (condition-case nil
	  (widget-value-set widget glyph)
	(error nil))))
  (if (and url-current-object (url-target url-current-object))
      (progn
	(push-mark (point) t)
	(w3-find-specific-link (url-target url-current-object)))
    (goto-char (point-min)))
  (and (not (featurep 'xemacs))
       (not (eq (device-type) 'tty))
       (w3-fixup-eol-faces))
  (message "Drawing... done"))

;;;###autoload
(defun w3-region (st nd)
  "Parse and display the region of this buffer between ST and ND."
  (interactive "r")
  (if (not w3-setup-done) (w3-do-setup))
  (let* ((source (buffer-substring st nd))
	 (w3-display-same-buffer t)
	 (parse nil))
    (save-window-excursion
      (with-current-buffer (get-buffer-create " *w3-region*")
	(erase-buffer)
	(insert source)
	(setq parse (w3-parse-buffer (current-buffer))))
      (narrow-to-region st nd)
      (delete-region (point-min) (point-max))
      (w3-draw-tree parse)
      (w3-finish-drawing)
      (widen))))

(defun w3-refresh-buffer ()
  (interactive)
  (let ((parse w3-current-parse)
	(inhibit-read-only t)
	(w3-display-same-buffer t)
	(origin (point)))
    (if (not parse)
	(error "Could not find the parse tree for this buffer.  EEEEK!"))
    (erase-buffer)
    (w3-draw-tree parse)
    (w3-finish-drawing)
    (w3-mode)
    (set-buffer-modified-p nil)
    (goto-char (min origin (point-max)))))

(defun w3-prepare-tree (parse)
  (w3-draw-tree parse)
  (set-buffer-modified-p nil)
  (setq w3-current-parse parse
	w3-current-source nil)
  (w3-finish-drawing)
  (w3-mode)
  (w3-maybe-fetch-frames))

(defun w3-prepare-buffer ()
  ;; The text/html viewer - does all the drawing and displaying of the buffer
  ;; that is necessary to go from raw HTML to a good presentation.
  (let* ((source (buffer-string))
	 (source-buf (current-buffer))
	 (parse (w3-parse-buffer source-buf)))
    (set-buffer-modified-p nil)
    (unwind-protect
	(w3-draw-tree parse)
      (kill-buffer source-buf)
      (set-buffer-modified-p nil)
      (setq w3-current-source source
	    w3-current-parse parse)
      (w3-finish-drawing)
      (w3-mode))
    (set-buffer-modified-p nil))
  (w3-maybe-fetch-frames))

(defun w3-maybe-fetch-frames ()
  (if w3-frameset-structure
      (cond ((or (eq w3-display-frames t)
		 (and (eq w3-display-frames 'ask)
		      (y-or-n-p "Fetch frames? ")))
	     (w3-frames)
	     t))))

(defun w3-buffer-visiting (url)
  "Return the name of a buffer (if any) that is visiting URL."
  (setq url (url-normalize-url url))
  (let ((bufs (buffer-list))
	(found nil))
    (while (and bufs (not found))
      (with-current-buffer (car bufs)
	(setq found (if (and
			 (not (string-match "^ " (buffer-name (car bufs))))
			 (eq major-mode 'w3-mode)
			 url-current-object
			 (equal (url-normalize-url (url-view-url t)) url))
			(car bufs) nil)
	      bufs (cdr bufs))))
    found))

(defun w3-frames (&optional new-frame)
  "Set up and fetch W3 frames. With optional prefix, do so in a new frame."
  (interactive "P")
  (if (not w3-display-frames)
      (let ((w3-display-frames t))
	(w3-refresh-buffer)))
  ;; FIXME!!!
  (let* ((old-asynch (default-value 'url-be-asynchronous))
	 (structure (reverse w3-frameset-structure)))
    (if new-frame
	(select-frame (make-frame)))
    (unwind-protect
	(progn
	  (setq-default url-be-asynchronous nil)
	  ;; set up frames
	  (while structure
	    (if (eq (car (car structure)) 'frameset)
		(setq structure (w3-display-frameset structure))
	      (pop structure)))
	  ;; compute target window distances
	  (let ((origin-buffer (current-buffer))
		(stop nil))
	    (while (not stop)
	      (or w3-target-window-distances
		  (setq w3-target-window-distances
			(w3-compute-target-window-distances)))
	      (other-window 1)
	      (if (eq (current-buffer) origin-buffer)
		  (setq stop t)))))
      (setq-default url-be-asynchronous old-asynch))))

(defun w3-frameset-dimensions-p (str)
  (and str (not (string-equal str "*")) (not (string-match "100%" str))))

(defun w3-display-frameset (frameset-structure)
  (let* ((structure frameset-structure)
	 (frameset-cardinal (nth 1 (car structure)))
	 (current-dims (cdr (cdr (car structure))))
	 (cols (cdr-safe (assq 'cols current-dims)))
	 (rows (cdr-safe (assq 'rows current-dims)))
	 (char-width (if (> (frame-char-width) 1)
			  (frame-char-width)
			w3-tty-char-width))
	 (char-height (if (> (frame-char-height) 1)
			  (frame-char-height)
			w3-tty-char-height))
	 (inhibit-frame nil))
    (pop structure)
    ;; columns ?
    (if (w3-frameset-dimensions-p cols)
	(setq cols (w3-decode-frameset-dimensions
		    cols (window-width) window-min-width char-width))
      ;; rows ?
      (if (w3-frameset-dimensions-p rows)
	  (setq rows (w3-decode-frameset-dimensions
		      rows (window-height) window-min-height char-height))
	;; default: columns of equal width
	(let ((fwidth (/ (window-width) frameset-cardinal))
	      (cardinal frameset-cardinal))
	  (while (> cardinal 0)
	    (push fwidth cols)
	    (setq cardinal (1- cardinal))))))
    (while (> frameset-cardinal 0)
      (cond ((cdr cols)
	     (if (or (< (car cols) window-min-width)
		     (< (- (window-width) (car cols)) window-min-width))
		 (setq inhibit-frame (format "Width %d" (window-width)))
	       (split-window-horizontally (car cols)))
	     (pop cols))
	    ((cdr rows)
	     (if (or (< (car rows) window-min-height)
		     (< (- (window-height) (car rows)) window-min-height))
		 (setq inhibit-frame (format "Height %d" (window-height)))
	       (split-window-vertically (car rows)))
	     (pop rows)))
      (cond ((eq (car (car structure)) 'frame)
	     (let ((href (nth 2 (car structure)))
		   (name (nth 1 (car structure)))
		   (w3-notify 'semibully)
		   (next-frame-window (next-window)))
	       (pop structure)
	       (cond (inhibit-frame
		      (w3-warn 'html (format "%s insufficient to split windows for HTML frame \"%s\""
					     inhibit-frame name))
		      (setq inhibit-frame nil
			    next-frame-window (selected-window)))
		     (t
		      (w3-fetch href)))
	       (let ((framebuf (w3-buffer-visiting href)))
		 (cond (framebuf
			(with-current-buffer framebuf
                          (setq w3-frame-name name
                                w3-target-window-distances nil))
			(select-window next-frame-window))))))
	    ((eq (car (car structure)) 'frameset)
	     (cond (inhibit-frame
		    (w3-warn 'html (format "%s insufficient to split windows for HTML frameset"
					   inhibit-frame))
		    (let ((sub-frameset-cardinal (cadr (car structure))))
		      (pop structure)
		      (while (> sub-frameset-cardinal 0)
			(pop structure)
			(setq sub-frameset-cardinal (1- sub-frameset-cardinal))))
		    (setq inhibit-frame nil))
		   (t
		    (setq structure (w3-display-frameset structure))))))
      (setq frameset-cardinal (1- frameset-cardinal)))
    structure))

(defun w3-compute-target-window-distances ()
  "Compute an alist of target names and window distances"
  (let ((origin-buffer (current-buffer))
	(distance 0)
	(stop nil)
	(window-distances nil))
    (while (not stop)
      (if w3-frame-name
	  (push (cons (intern (downcase w3-frame-name)) distance)
		window-distances))
      (other-window 1)
      (setq distance (1+ distance))
      (if (eq (current-buffer) origin-buffer)
	  (setq stop t)))
    window-distances))

(defun w3-decode-frameset-dimensions (dims available-dimension min-dim pixel-dim)
  "Returns numbers of lines or columns in Emacs, computed from specified frameset dimensions"
  (let ((dimensions nil))
    (if dims
	(let ((nb-stars 0)
	      (norm-stars 0)
	      (remaining-available-dimension available-dimension))
	  (while (string-match "\\([0-9]*\\*\\|[0-9]+%?\\)" dims)
	    (let ((match (substring dims (match-beginning 1) (match-end 1))))
	      (setq dims (substring dims (match-end 1)))
	      (cond ((string-match "\\([0-9]+\\)\\*" match)
		     ;; divide rest with relative weights
		     (let ((weight (car (read-from-string
					 (substring match (match-beginning 1) (match-end 1))))))
		     (push (cons '* weight) dimensions)
		     (setq nb-stars (1+ nb-stars)
			   norm-stars (+ norm-stars weight))))
		    ((string-match "\\*" match)
		     ;; divide equally
		     (push '* dimensions)
		     (setq nb-stars (1+ nb-stars)
			   norm-stars (1+ norm-stars)))
		    (t
		     (cond ((string-match "\\([0-9]+\\)%" match)
			    ;; percentage of available height
			    (push (/ (* (car (read-from-string (substring match 0 -1)))
					available-dimension)
				     100)
				  dimensions))
			   (t
			    ;; absolute number: pixel height
			    (let* ((dim-in-pixels (car (read-from-string match)))
				   (dim (max (/ dim-in-pixels pixel-dim)
					     min-dim)))
			      (cond ((<= dim remaining-available-dimension)
				     (push dim dimensions)
				     (setq remaining-available-dimension
					   (- remaining-available-dimension (car dimensions))))
				    (t
				     (w3-warn 'html (format "Frame dimension too large: %d" dim-in-pixels))
				     ;; too large: replace with *
				     (push '* dimensions)
				     (setq nb-stars (1+ nb-stars)
					   norm-stars (1+ norm-stars)))))))))))
	  (if (zerop nb-stars)
	      ;; push => reverse order
	      (reverse dimensions)
	    ;; substitute numbers for *
	    (let ((star-replacement (/ remaining-available-dimension norm-stars))
		  (star-dimensions dimensions))
	      (setq dimensions nil)
	      (while star-dimensions
		(push (cond ((eq '* (car star-dimensions))
			     star-replacement)
			    ((listp (car star-dimensions))
			     (* (cdar star-dimensions) star-replacement))
			    (t
			     (car star-dimensions)))
		      dimensions)
		(pop star-dimensions))
	      ;; push + push => in order
	      dimensions))))))

(provide 'w3-display)
