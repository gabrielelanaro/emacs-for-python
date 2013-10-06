;;; css.el --- Cascading Style Sheet parser

;; Copyright (c) 1996-2001, 2007, 2013 Free Software Foundation, Inc.

;; Author: $Author: legoscia $
;; Created: $Date: 2007/11/15 12:28:29 $
;; Keywords: 

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

(eval-and-compile
  (require 'cl)
  (require 'font))
(autoload 'url-expand-file-name "url-expand")
(autoload 'url-insert-file-contents "url-handlers")
(autoload 'url-view-url "url-util")

(if (eval-when-compile (not (fboundp 'frame-char-height)))
    (defun frame-char-height (&optional frame)
      "Height in pixels of a line in the font in frame FRAME.
If FRAME is omitted, the selected frame is used.
For a terminal frame, the value is always 1."
      (font-height (face-font 'default frame))))

(if (eval-when-compile (not (fboundp 'frame-char-width)))
    (defun frame-char-width (&optional frame)
      "Width in pixels of characters in the font in frame FRAME.
If FRAME is omitted, the selected frame is used.
For a terminal screen, the value is always 1."
      (font-width (face-font 'default frame))))

;; CBI = Cant Be Implemented - due to limitations in emacs/xemacs
;; NYI = Not Yet Implemented - due to limitations of space/time
;; NYPI = Not Yet Partially Implemented - possible partial support, eventually

(defconst css-properties
  '(;; Property name    Inheritable?   Type of data
    ;; Base CSS level 1 properties: http://www.w3.org/pub/WWW/TR/REC-CSS1
    ;; Font properties, Section 5.2
    [font-family      t              string-list]
    [font-style       t              symbol]
    [font-variant     t              symbol]
    [font-weight      t              weight]
    [font-size        t              height]
    [font             nil            font]

    ;; Color and background properties, Section 5.3
    [color            t              color]
    [background       nil            color-shorthand]
    [background-color nil            color]
    [background-image nil            url]    ; NYI
    [background-repeat nil           symbol] ; CBI
    [background-attachment nil       symbol] ; CBI
    [background-position nil         symbol] ; CBI

    ;; Text properties, Section 5.4
    [word-spacing     t              length] ; CBI
    [letter-spacing   t              length] ; CBI
    [text-decoration  t              symbol-list]
    [vertical-align   nil            symbol]
    [text-transform   t              symbol]
    [text-align       t              symbol]
    [text-indent      t              length] ; NYI
    [line-height      t              length] ; CBI

    ;; Box properties, Section 5.5
    [margin           nil            boundary-shorthand]
    [margin-left      nil            length]
    [margin-right     nil            length]
    [margin-top       nil            length]
    [margin-bottom    nil            length]
    [padding          nil            boundary-shorthand]
    [padding-left     nil            length]
    [padding-right    nil            length]
    [padding-top      nil            length]
    [padding-bottom   nil            length]
    [border           nil            border-shorthand]
    [border-left      nil            border]
    [border-right     nil            border]
    [border-top       nil            border]
    [border-bottom    nil            border]
    [border-top-width nil            nil]
    [border-right-width nil          nil]
    [border-bottom-width nil         nil]
    [border-left-width nil           nil]
    [border-width     nil            boundary-shorthand]
    [border-color     nil            color]
    [border-style     nil            symbol]
    [width            nil            length] ; NYPI
    [height           nil            length] ; NYPI
    [float            nil            symbol]
    [clear            nil            symbol]

    ;; Classification properties, Section 5.6
    [display          nil            symbol]
    [list-style-type  t              symbol]
    [list-style-image t              url]
    [list-style-position t           symbol]
    [list-style       nil            list-style]
    [white-space      t              symbol]

    ;; These are for specifying speech properties (ACSS-style)
    ;; http://www.w3.org/pub/WWW/Style/CSS/Speech/NOTE-ACSS

    ;; General audio properties, Section 3
    [volume           t              string] ; Needs its own type?
    [pause-before     nil            time]
    [pause-after      nil            time]
    [pause            nil            pause]
    [cue-before       nil            string]
    [cue-after        nil            string]
    [cue-during       nil            string]
    [cue              nil            string] ; Needs its own type?

    ;; Spatial properties, Section 4
    [azimuth          t              angle]
    [elevation        t              elevation]

    ;; Speech properties, Section 5
    [speed            t              string]
    [voice-family     t              string-list]
    [pitch            t              string]
    [pitch-range      t              percentage]
    [stress           t              percentage]
    [richness         t              percentage]
    [speak-punctuation t             symbol]
    [speak-date       t              symbol]
    [speak-numeral    t              symbol]
    [speak-time       t              symbol]

    ;; Proposed printing extensions
    ;; http://www.w3.org/pub/WWW/Style/Group/WD-PRINT-961220
    ;; These apply only to pages (@page directive)
    [size             nil            symbol]
    [orientation      nil            symbol]
    [margin-inside    nil            length]
    ;; These apply to the document
    [page-break-before nil           symbol]
    [page-break-after  nil           symbol]
    
    ;; These are for specifying speech properties (Raman-style)
    [voice-family     t              string]
    [gain             t              symbol]
    [left-volume      t              integer]
    [right-volume     t              integer]
    [pitch            t              integer]
    [pitch-range      t              integer]
    [stress           t              integer]
    [richness         t              integer]
    )
  "A description of the various CSS properties and how to interpret them.")

(put 'font 'css-shorthand t)
(put 'background 'css-shorthand t)
(put 'margin 'css-shorthand t)
(put 'padding 'css-shorthand t)
(put 'border 'css-shorthand t)
(put 'list-style 'css-shorthand t)

(dolist (entry css-properties)
  (put (aref entry 0) 'css-inherit (aref entry 1))
  (put (aref entry 0) 'css-type    (aref entry 2)))

(defconst css-weights
  '(nil					;never used
    :extra-light
    :light
    :demi-light
    :medium
    :normal
    :demi-bold
    :bold
    :extra-bold
    )
  "List of CSS font weights.")

(defvar css-syntax-table
  (let ((st (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?` "\"" st)
    (modify-syntax-entry ?{ "(" st)
    (modify-syntax-entry ?} ")" st)
    st)
  "The syntax table for parsing stylesheets")

(defvar css-scratch-val)
(defvar css-scratch-id)
(defvar css-scratch-class)
(defvar css-scratch-possibles)
(defvar css-scratch-current)
(defvar css-scratch-classes)
(defvar css-scratch-class-match)
(defvar css-scratch-current-rule)
(defvar css-scratch-current-value)

(defsubst css-replace-regexp (regexp to-string)
  (goto-char (point-min))
  (while (re-search-forward regexp nil t)
    (replace-match to-string t nil)))

(defun css-contextual-match (rule stack)
  (let ((ancestor)
	(p-args)
	(p-class)
	(p-tag)
	(matched t))
    (while rule
      (setq p-tag (caar rule)
	    ancestor (assq p-tag stack))
      (case p-tag
	(*document			; Class matching only
	 (setq matched nil)
	 (while (setq ancestor (car stack))
	   (setq stack (cdr stack)
		 p-args (cdr ancestor)
		 p-class (or (cdr-safe (assq 'class p-args)) t))
	   (if (equal p-class (cdar rule))
	       (setq matched t
		     rule nil
		     stack nil))))
	(otherwise
	 (if (not ancestor)
	     (setq rule nil
		   matched nil)
	   (setq p-args (cdr ancestor)
		 p-class (or (cdr-safe (assq 'class p-args)) t))
	   (if (not (equal p-class (cdar rule)))
	       (setq matched nil
		     rule nil)))))
      (setq rule (cdr rule)))
    matched))

(defsubst css-get-internal (tag args sheet element-stack)
  (setq css-scratch-id (or (cdr-safe (assq 'id args))
			   (cdr-safe (assq 'name args)))
	css-scratch-class (or (cdr-safe (assq 'class args)) t)
	css-scratch-possibles (gethash tag sheet))
  (while css-scratch-possibles
    (setq css-scratch-current (car css-scratch-possibles)
	  css-scratch-current-rule (car css-scratch-current)
	  css-scratch-current-value (cdr css-scratch-current)
	  css-scratch-classes (if (listp (car css-scratch-current-rule))
				  (cdar css-scratch-current-rule)
				(cdr css-scratch-current-rule))
	  css-scratch-class-match t
	  css-scratch-possibles (cdr css-scratch-possibles))
    (if (eq t css-scratch-classes)
	(setq css-scratch-classes nil))
    (if (eq t css-scratch-class)
	(setq css-scratch-class nil))
    (while css-scratch-classes
      (if (not (member (pop css-scratch-classes) css-scratch-class))
	  (setq css-scratch-class-match nil
		css-scratch-classes nil)))
    (cond
     ((and (listp (car css-scratch-current-rule)) css-scratch-class-match)
      ;; Contextual!
      (setq css-scratch-current-rule (cdr css-scratch-current-rule))
      (if (css-contextual-match css-scratch-current-rule element-stack)
	  (setq css-scratch-val
		(append css-scratch-val css-scratch-current-value)))
      )
     (css-scratch-class-match
      (setq css-scratch-val (append css-scratch-val css-scratch-current-value))
      )
     (t
      nil))
    )
  )

(defsubst css-get (tag args &optional sheet element-stack)
  (setq css-scratch-val nil
	css-scratch-class (or (cdr-safe (assq 'class args)) t))

  ;; check for things without the class
  (if (listp css-scratch-class)
      (css-get-internal tag nil sheet element-stack))

  ;; check for global class values
  (css-get-internal '*document args sheet element-stack)

  ;; Now check for things with the class - they will be stuck on the front
  ;; of the list, which will mean we do the right thing
  (css-get-internal tag args sheet element-stack)

  ;; Defaults are up to the calling application to provide
  css-scratch-val)

(defun css-ancestor-get (info ancestors sheet)
  ;; Inheritable property, check ancestors
  (let (cur)
    (while ancestors
      (setq cur (car ancestors)
 	    css-scratch-val (css-get info (car cur) (cdr cur) sheet)
 	    ancestors (if css-scratch-val nil (cdr ancestors)))))
  css-scratch-val)  

(defun css-split-selector (tag)
  ;; Return a list 
  (cond
   ((string-match " " tag)		; contextual
    (let ((tags (split-string tag "[ \t]+"))
	  (result nil))
      (while tags
	(setq result (cons (css-split-selector (car tags)) result)
	      tags (cdr tags)))
      result))
   ((string-match "[:\\.]" tag)
    (let ((tag (if (= (match-beginning 0) 0)
		   '*document
		 (intern (downcase (substring tag 0 (match-beginning 0))))))
	  (rest (substring tag (match-beginning 0) nil))
	  (classes nil))
      (while (string-match "^[:\\.][^:\\.]+" rest)
	(if (= ?. (aref rest 0))
	    (setq classes (cons (substring rest 1 (match-end 0)) classes))
	  (setq classes (cons (substring rest 0 (match-end 0)) classes)))
	(setq rest (substring rest (match-end 0) nil)))
      (setq classes (sort classes 'string-lessp))
      (cons tag classes)))
   ((string-match "^#" tag)		; id selector
    (cons '*document (list tag)))
   (t
    (cons (intern (downcase tag)) t)
    )
   )
  )

(defun css-applies-to (st nd)
  (let ((results nil)
	(save-pos nil))
    (narrow-to-region st nd)
    (goto-char st)
    (skip-chars-forward " \t\r\n")
    (while (not (eobp))
      (setq save-pos (point))
      (skip-chars-forward "^,")
      (skip-chars-backward " \r\t\n")
      (setq results (cons (css-split-selector
			   (buffer-substring save-pos (point))) results))
      (skip-chars-forward ", \t\r\n"))
    (widen)
    results))

(defun css-split-font-shorthand (font)
  ;; [<font-weight> || <font-style>]? <font-size> [ / <line-height> ]? <font-family>
  (let (weight size height family retval)
    (if (not (string-match " *\\([0-9.]+[^ /]+\\)" font))
	(progn
	  (message "Malformed font shorthand: %s" font)
	  nil)
      (setq weight (if (/= 0 (match-beginning 0))
		       (substring font 0 (match-beginning 0)))
	    size (match-string 1 font)
	    font (substring font (match-end 0) nil))
      (if (string-match " */ *\\([^ ]+\\) *" font)
	  ;; they specified a line-height as well
	  (setq height (match-string 1 font)
		family (substring font (match-end 0) nil))
	(if (string-match "^[ \t]+" font)
	    (setq family (substring font (match-end 0) nil))
	  (setq family font)))
      (if weight
	  (push (cons 'font-weight (css-expand-value 'weight weight)) retval))
      (if size
	  (push (cons 'font-size (css-expand-length size)) retval))
      (if height
	  (push (cons 'line-height (css-expand-length height t)) retval))
      (if family
	  (push (cons 'font-family (css-expand-value 'string-list family)) retval))
      retval)))

(defun css-expand-length (spec &optional height)
  (cond
   ((not (stringp spec)) spec)
   ((string-equal spec "auto") nil)
   ((and (string-match "\\([+-]?\\([0-9]+\\|[0-9]*\\.[0-9]+\\)\\)%" spec)
	 (fboundp 'frame-char-height))
    ;; A percentage
    ;; XXX: should be relative to encosing element
    (setq spec (/ (string-to-number (match-string 1 spec)) 100.0))
    (if height
	(round (* (frame-char-height) spec))
      (max 0 (round (* (frame-char-width) spec)))))
   ((string-match "\\([+-]?\\([0-9]+\\|[0-9]*\\.[0-9]+\\)\\)%" spec)
    ;; No frame-char-width/height
    (setq spec (/ (string-to-number (match-string 1 spec)) 100.0))
    (if height
	(max 0 (round (* (/ (frame-pixel-height) (frame-height)) spec)))
      (max 0 (round (* (/ (frame-pixel-width) (frame-width)) spec)))))
   ((string-match "\\([+-]?\\([0-9]+\\|[0-9]*\\.[0-9]+\\)\\)e[mx]" spec)
    ;; Character based
    ;; XXX: should be relative to font size of enclosing element
    (round (font-spatial-to-canonical
            (concat (number-to-string
                     (* (string-to-number (match-string 1 spec))
                        (if height (frame-char-height) (frame-char-width))))
                    "px"))))
   (t
    (truncate (font-spatial-to-canonical spec)))
   )
  )

(defsubst css-unhex-char (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

(defsubst css-pow (x n)
  (apply '* (make-list n x)))

(defun css-unhex (x)
  (let ((ord (length x))
	(rval 0))
    (while (> ord 0)
      (setq rval (+ rval
		    (* (css-pow 16 (- (length x) ord))
		       (css-unhex-char (aref x (1- ord)))))
	    ord (1- ord)))
    rval))

(defmacro css-symbol-list-as-regexp (&rest keys)
  `(eval-when-compile
     (concat "^\\("
	     (mapconcat 'symbol-name
			(quote ,keys)
			"\\|") "\\)$")))

(defun css-expand-color (color)
  (condition-case _
      (cond
       ((string-match "^\\(transparent\\|none\\)$" color)
	(setq color nil))
       ((string-match "^#" color)
	(let (r g b)
	  (cond
	   ((string-match "^#...$" color)
	    ;; 3-char rgb spec, expand out to six chars by replicating
	    ;; digits, not adding zeros.
	    (setq r (css-unhex (make-string 2 (aref color 1)))
		  g (css-unhex (make-string 2 (aref color 2)))
		  b (css-unhex (make-string 2 (aref color 3)))))
	   ((string-match "^#\\(..\\)\\(..\\)\\(..\\)$" color)
	    (setq r (css-unhex (match-string 1 color))
		  g (css-unhex (match-string 2 color))
		  b (css-unhex (match-string 3 color))))
	   (t
	    (setq color (substring color 1))
	    (let* ((n (/ (length color) 3))
		   (max (float (css-pow 16 n))))
	      (setq r (css-unhex (substring color 0 n))
		    g (css-unhex (substring color n (* n 2)))
		    b (css-unhex (substring color (* n 2) (* n 3)))
		    r (round (* (/ r max) 255))
		    g (round (* (/ g max) 255))
		    b (round (* (/ b max) 255))))))
	  (setq color (vector 'rgb r g b))))
       ((string-match "^rgb *( *\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\) *) *$" color)
	;; rgb(r,g,b) 0 - 255, cutting off at 255
	(setq color (vector
		     'rgb
		     (min (string-to-number (match-string 1 color)) 255)
		     (min (string-to-number (match-string 2 color)) 255)
		     (min (string-to-number (match-string 3 color)) 255))))
       ((string-match "^rgb *( *\\([0-9]+\\) *%[, ]+\\([0-9]+\\) *%[, ]+\\([0-9]+\\) *% *) *$" color)
	;; rgb(r%,g%,b%) 0 - 100%, cutting off at 100%
	(let ((r (min (string-to-number (match-string 1 color)) 100.0))
	      (g (min (string-to-number (match-string 2 color)) 100.0))
	      (b (min (string-to-number (match-string 3 color)) 100.0)))
	  (setq r (round (* r 2.55))
		g (round (* g 2.55))
		b (round (* b 2.55))
		color (vector 'rgb r g b))))
       (t
	;; Hmmm... pass it through unmangled and hope the underlying
	;; windowing system can handle it.
	)
       )
    (error
     (display-warning 'css (format "Couldn't interpret color value %s" color))
     (setq color nil)))
  color
  )

(defvar css--url)
(defvar css--purl)

(defun css-expand-value (type value)
  (if value
      (case type
	(length				; CSS, Section 6.1
	 (setq value (css-expand-length value)))
	(height
	 (setq value (css-expand-length value t)))
	(percentage			; CSS, Section 6.2
	 (setq value (/ (string-to-number value)
			(if (fboundp 'float) (float 100) 1))))
	(color				; CSS, Section 6.3
	 (setq value (css-expand-color value)))
	(url				; CSS, Section 6.4
	 ;; Potentially remove url(...) from around the URL
	 (if (string-match "url *(\\([^ )]+\\) *)" value)
	     (setq value (match-string 1 value)))
	 ;; Nuke quotes
	 (if (string-match "\"\\([^\"]+\\)\"" value)
	     (setq value (match-string 1 value)))
	 ;; Nuke whitespace
	 (if (string-match " *\\([^ ]+\\) *" value)
	     (setq value (match-string 1 value)))
	 (setq value (url-expand-file-name value (or css--url css--purl))))
	(angle				; ACSS, Section 2.2.1
	 )
	(time				; ACSS, Section 2.2.2
	 (let ((val (string-to-number value))
	       (units 'ms))
	   (if (string-match "^[0-9]+ *\\([a-zA-Z.]+\\)" value)
	       (setq units (intern (downcase (match-string 1 value)))))
	   (setq value (case units
			 ((s second seconds)
			  val)
			 ((min minute minutes)
			  (* val 60))
			 ((hr hour hours)
			  (* val 60 60))
			 ((day days)
			  (* val 24 60 60))
			 (otherwise
			  (/ val (float 1000)))))))
	(elevation			; ACSS, Section 4.2
	 (if (string-match
	      (css-symbol-list-as-regexp below level above higher lower) value)
	     (setq value (intern (downcase (match-string value 1)))
		   value (case value
			   (below -90)
			   (above 90)
			   (level 0)
			   (higher 45)
			   (lower -45)
			   ))
	   (setq value (css-expand-value 'angle value))))
	(color-shorthand		; CSS, Section 5.3.7
	 ;; color|image|repeat|attach|position
	 (let ((keys (split-string value " +"))
	       cur color image repeat attach position)
	   (while (setq cur (pop keys))
	     (cond
	      ((string-match "url" cur)	; Only image can have a URL
	       (setq image (css-expand-value 'url cur)))
	      ((string-match "%" cur)	; Only position can have a perc.
	       (setq position (css-expand-value 'percentage cur)))
	      ((string-match "repeat" cur) ; Only repeat
	       (setq repeat (intern (downcase cur))))
	      ((string-match "scroll\\|fixed" cur)
	       (setq attach (intern (downcase (substring cur
							 (match-beginning 0)
							 (match-end 0))))))
	      ((string-match (css-symbol-list-as-regexp
			      top center bottom left right) cur)
	       )
	      (t
	       (setq color (css-expand-value 'color cur)))))
	   (setq value (list (cons 'background-color color)
			     (cons 'background-image image)
			     (cons 'background-repeat repeat)
			     (cons 'background-attachment attach)
			     (cons 'background-position position)))))
	(font				; CSS, Section 5.2.7
	 ;; [style | variant | weight]? size[/line-height]? family
	 (setq value (css-split-font-shorthand value)))
	(border				; width | style | color
	 ;; FIXME
	 )
	(border-shorthand		; width | style | color
	 ;; FIXME
	 )
	(list-style			; CSS, Section 5.6.6
	 ;; keyword | position | url
	 (setq value (split-string value "[ ,]+"))
	 (if (= (length value) 1)
	     (setq value (list (cons 'list-style-type
				     (intern (downcase (car value))))))
	   (setq value (list (cons 'list-style-type
				   (css-expand-value 'symbol (nth 0 value)))
			     (cons 'list-style-position
				   (css-expand-value 'symbol (nth 1 value)))
			     (cons 'list-style-image
				   (css-expand-value 'url (nth 2 value)))))))
	(boundary-shorthand		; CSS, Section 5.5.x
	 ;; length|percentage|auto {1,4}
	 (setq value (split-string value "[ ,]+"))
	 (let* ((top (intern (format "%s-top" type)))
		(bottom (intern (format "%s-bottom" type)))
		(left (intern (format "%s-left" type)))
		(right (intern (format "%s-right" type))))
	   (setq top (cons top (css-expand-value (get top 'css-type)
						 (nth 0 value)))
		 right (cons right (css-expand-value (get right 'css-type)
						     (nth 1 value)))
		 bottom (cons bottom (css-expand-value (get bottom 'css-type)
						       (nth 2 value)))
		 left (cons left (css-expand-value (get left 'css-type)
						   (nth 3 value)))
		 value (list top right bottom left))))
	(weight				; CSS, Section 5.2.5
	 ;; normal|bold|bolder|lighter|[1-9]00
	 (cond
	  ((string-match "^[0-9]+" value)
	   (setq value (/ (string-to-number value) 100)
		 value (or (nth value css-weights) :bold)))
	  ((string-match (css-symbol-list-as-regexp normal bold bolder lighter)
			 value)
	   (setq value (intern (downcase (concat ":" value)))))
	  (t (setq value (intern ":normal")))))
	;; The rest of these deal with how we handle things internally
	((symbol integer)		; Read it in
	 (setq value (read (downcase value))))
	(symbol-list			; A space/comma delimited symlist
	 (setq value (downcase value)
	       value (split-string value "[ ,]+")
	       value (mapcar 'intern value)))
	(string-list			; A space/comma delimited list
	 (setq value (split-string value " *, *")))
	(otherwise			; Leave it as is
	 t)
	)
    )
  value
  )

(defun css-parse-args (st &optional nd)
  ;; Return an assoc list of attribute/value pairs from a CSS style entry
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	(case-fold-search t)
	)
    (save-excursion
      (if (stringp st)
	  (progn
	    (set-buffer (get-buffer-create " *css-style-temp*"))
	    (set-syntax-table css-syntax-table)
	    (erase-buffer)
	    (insert st)
	    (setq st (point-min)
		  nd (point-max)))
	(set-syntax-table css-syntax-table))
      (save-restriction
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward ";, \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t:,;")
	  (downcase-region name-pos (point))
	  (setq name (intern (buffer-substring name-pos (point))))
	  (skip-chars-forward " \t\n")
	  (if (not (eq (char-after (point)) ?:)) ; There is no value
	      (setq value nil)
	    (skip-chars-forward " \t\n:")
	    (setq val-pos (point)
		  value
		  (cond
		   ((or (= (or (char-after val-pos) 0) ?\")
			(= (or (char-after val-pos) 0) ?'))
		    (buffer-substring (1+ val-pos)
				      (condition-case ()
					  (prog2
					      (forward-sexp 1)
					      (1- (point))
					    (skip-chars-forward "\""))
					(error
					 (skip-chars-forward "^ \t\n")
					 (point)))))
		   (t
		    (buffer-substring val-pos
				      (progn
					(skip-chars-forward "^;")
					(skip-chars-backward " \t")
					(point)))))))
	  (setq value (css-expand-value (get name 'css-type) value))
	  (if (get name 'css-shorthand)
	      (setq results (append value results))
	    (setq results (cons (cons name value) results)))
	  (skip-chars-forward ";, \n\t"))
	results))))

(defun css-handle-media-directive (data active)
  (let (type)
    (if (string-match "\\([^ \t\r\n{]+\\)" data)
	(setq type (intern (downcase (substring data (match-beginning 1)
						(match-end 1))))
	      data (substring data (match-end 1)))
      (setq type 'unknown))
    (if (string-match "^[ \t\r\n]*{" data)
	(setq data (substring data (match-end 0))))
    (if (memq type active)
	(save-excursion
	  (insert data)))))

(defvar url-mime-accept-string)
(defvar url-current-object)

(defun css-handle-import (data)
  (let ((purl url-current-object)       ;FIXME: Should this be css--purl?
        (url (css-expand-value 'url data)))
    (and url
	 (let* ((url-mime-accept-string "text/css ; level=2")
                (sheet
                 (with-current-buffer (generate-new-buffer " *styleimport*")
                   ;; ftp/file URLs can signal an error.
                   (ignore-errors
                     (url-insert-file-contents url))
                   (css-clean-buffer)
                   (prog1 (buffer-string)
                     (set-buffer-modified-p nil)
                     (kill-buffer (current-buffer))))))
	   (insert sheet)))))

(defun css-clean-buffer ()
  ;; Nuke comments, etc.
  (goto-char (point-min))
  (let ((save-pos nil))
    (while (search-forward "/*" nil t)
      (setq save-pos (- (point) 2))
      (delete-region save-pos
		     (if (search-forward "*/" nil t)
			 (point)
		       (end-of-line)
		       (point)))))
  (goto-char (point-min))
  (delete-matching-lines "^[ \t\r]*$")	; Nuke blank lines
  (css-replace-regexp "^[ \t\r]+" "")	; Nuke whitespace at beg. of line
  (css-replace-regexp "[ \t\r]+$" "")	; Nuke whitespace at end of line
  (goto-char (point-min)))

(defalias 'css-color-light-p
  (if (featurep 'xemacs)
      (lambda (color-or-face)
        (let (color)
          (cond
           ((or (facep color-or-face)
                (and (symbolp color-or-face)
                     (find-face color-or-face)))
            (setq color (specifier-instance (face-background color-or-face))))
           ((color-instance-p color-or-face)
            (setq color color-or-face))
           ((color-specifier-p color-or-face)
            (setq color (specifier-instance color-or-face)))
           ((stringp color-or-face)
            (setq color (make-color-instance color-or-face)))
           (t (signal 'wrong-type-argument 'color-or-face-p)))
          (if color
              (not (< (apply '+ (color-instance-rgb-components color))
                      (/ (apply '+ (color-instance-rgb-components
                                    (make-color-instance "white"))) 3)))
            t)))

    (lambda (color-or-face)
      (let ((colors
             (cond
              ((null window-system)
               nil)
              ((facep color-or-face)
               (color-values (or (face-background color-or-face)
                                 (frame-parameter nil 'background-color))))
              ((stringp color-or-face)
               (color-values color-or-face))
              ((font-rgb-color-p color-or-face)
               (list (font-rgb-color-red color-or-face)
                     (font-rgb-color-green color-or-face)
                     (font-rgb-color-blue color-or-face)))
              (t
               (signal 'wrong-type-argument 'color-or-face-p)))))
        (not (< (apply '+ colors)
                (/ (apply '+ (color-values "white")) 3)))))
    ))

(defun css-active-device-types (&optional device)
  (let ((types (list 'all
		     (if (featurep 'xemacs) 'xemacs 'emacs)
		     (if (or (featurep 'xemacs)
			     (if (fboundp 'display-multi-font-p)
				 (display-multi-font-p)))
			 'multifont 'unifont)
		     (if (css-color-light-p 'default) 'light 'dark)))
	(type (device-type device)))
    ;; For reasons I don't really want to get into, emacspeak and TTY
    ;; are mutually exclusive for most of our purposes (insert-before,
    ;; xetc)
    (if (featurep 'emacspeak)
	(setq types (cons 'speech types))
      (if (eq type 'tty)
	  (setq types (cons 'tty types))))
    (cond
     ((eq 'color (device-class))
      (if (not (device-bitplanes))
	  (setq types (cons 'color types))
	(setq types
	      (append
	       (list (intern (format "%dbit-color"
				     (device-bitplanes)))
		     (intern (format "%dbit"
				     (device-bitplanes)))
		     'color) types))
	(if (= 24 (device-bitplanes))
	    (setq types (cons 'truecolor types)))))
     ((eq 'grayscale (device-class))
      (setq types (append (list (intern (format "%dbit-grayscale"
						(device-bitplanes)))
				'grayscale)
			  types)))
     ((eq 'mono (device-class))
      (setq types (append (list 'mono 'monochrome) types)))
     (t
      (setq types (cons 'unknown types))))
    ;; FIXME: Remove me when the real 3.0 comes out
    (if (and (memq 'tty types) (memq 'color types))
	(setq types (cons 'ansi-tty types)))
    types))

(defmacro css-rule-specificity-internal (rule)
  `(progn
     (setq tmp (cdr ,rule))
     (if (listp tmp)
	 (while tmp
	   (if (= ?# (aref (car tmp) 0))
	       (incf a)
	     (incf b))
	   (setq tmp (cdr tmp))))))

(defsubst css-specificity (rule)
  ;; To find specificity, according to the september 1996 CSS draft
  ;; a = # of ID attributes in the selector
  ;; b = # of class attributes in the selector
  ;; c = # of tag names in the selector
  (let ((a 0) (b 0) (c 0) tmp)
    (if (not (listp (car rule)))
	(css-rule-specificity-internal rule)
      (setq c (length rule))
      (while rule
	(css-rule-specificity-internal (pop rule))))
    (+ (* 100 a) (* 10 b) c)
    )
  )

(defun css-copy-stylesheet (sheet)
  (let ((new (make-hash-table :size (hash-table-count sheet))))
    (maphash
     (function
      (lambda (k v)
	(puthash k (copy-tree v) new))) sheet)
    new))

(defsubst css-store-rule (attrs applies-to sheet)
  (let (rules cur tag node)
    (while applies-to
      (setq cur (pop applies-to)
	    tag (car cur))
      (if (listp tag)
	  (setq tag (car tag)))
      (setq rules (gethash tag sheet))
      (cond
       ((null rules)
	;; First rule for this tag.  Create new ruleset
	(puthash tag (list (cons cur attrs)) sheet))
       ((setq node (assoc cur rules))
	;; Similar rule already exists, splice in our information
	(setcdr node (append attrs (cdr node))))
       (t
	;; First rule for this particular combination of tag/ancestors/class.
	;; Slap it onto the existing set of rules and push back into sheet.
	(setq rules (cons (cons cur attrs) rules))
	(puthash tag rules sheet))
       )
      )
    )
  )

(defun css-parse (url &optional string inherit)
  (let ((css--url url)
	(url-mime-accept-string
	 "text/css ; level=2")
	(save-pos nil)
	(applies-to nil)		; List of tags to apply style to
	(attrs nil)			; List of name/value pairs
	(device-type nil)
	(css--purl (url-view-url t))
	(pobj url-current-object)
	(active-device-types (css-active-device-types (selected-device)))
	(sheet inherit))
    (if (not sheet)
	(setq sheet (make-hash-table :size 13 :test 'eq)))
    (with-current-buffer (generate-new-buffer " *style*")
      (setq url-current-object pobj)
      (set-syntax-table css-syntax-table)
      (erase-buffer)
      (ignore-errors
	(if url (url-insert-file-contents url)))
      (goto-char (point-max))
      (if string (insert string))
      (css-clean-buffer)
      (goto-char (point-min))
      (while (not (eobp))
	(setq save-pos (point))
	(cond
	 ;; *sigh* SGML comments are being used to 'hide' data inlined
	 ;; with the <style> tag from older browsers.
	 ((or (looking-at "<!--+")	; begin
	      (looking-at "--+>"))	; end
	  (goto-char (match-end 0)))
	 ;; C++ style comments
	 ((looking-at "[ \t]*//")
	  (end-of-line))
	 ;; Pre-Processor directives
	 ((looking-at "[ \t\r]*@\\([^ \t\r\n]\\)")
	  (let (data directive)
	    (skip-chars-forward " @\t\r") ; Past any leading whitespace
	    (setq save-pos (point))
	    (skip-chars-forward "^ \t\r\n") ; Past the @ directive
	    (downcase-region save-pos (point))
	    (setq directive (intern (buffer-substring save-pos (point))))
	    (skip-chars-forward " \t\r")
	    (setq save-pos (point))
	    (cond
	     ((looking-at "[^{]*\\({\\)")
	      (goto-char (match-beginning 1))
	      (condition-case ()
		  (forward-sexp 1)
		(error (goto-char (point-max))))
	      (setq data (buffer-substring save-pos (1- (point)))))
	     ((looking-at "[\"']+")
	      (setq save-pos (1+ save-pos))
	      (condition-case ()
		  (forward-sexp 1)
		(error (goto-char (point-max))))
	      (setq data (buffer-substring save-pos (1- (point)))))
	     (t
	      (skip-chars-forward "^;")))
	    (if (not data)
		(setq data (buffer-substring save-pos (point))))
	    (setq save-pos (point))
	    (case directive
	     (import (css-handle-import data))
	     (media (css-handle-media-directive data active-device-types))
	     (t (message "Unknown directive in stylesheet: @%s" directive)))))
	 ;; Giving us some output device information, old way
	 ((looking-at "[ \t\r]*:\\([^: \n]+\\):")
	  (downcase-region (match-beginning 1) (match-end 1))
	  (setq device-type (intern (buffer-substring (match-beginning 1)
						      (match-end 1))))
	  (goto-char (match-end 0))
	  (if (not (memq device-type active-device-types))
	      ;; Not applicable to us... skip the info
	      (progn
		(if (re-search-forward ":[^:{ ]*:" nil t)
		    (goto-char (match-beginning 0))
		  (goto-char (point-max))))))
	 ;; Default is to treat it like a stylesheet declaration
	 (t
	  (skip-chars-forward "^{")
	  ;;(downcase-region save-pos (point))
	  (setq applies-to (css-applies-to save-pos (point)))
	  (skip-chars-forward "^{")
	  (setq save-pos (point))
	  (condition-case ()
	      (forward-sexp 1)
	    (error (goto-char (point-max))))
	  (skip-chars-backward "\r}")
	  (subst-char-in-region save-pos (point) ?\n ? )
	  (subst-char-in-region save-pos (point) ?\r ? )
	  ;; This is for not choking on garbage at the end of the buffer.
	  ;; I get bit by this every once in a while when going through my
	  ;; socks gateway.
	  (if (eobp)
	      nil
	    (setq attrs (css-parse-args (1+ save-pos) (point)))
	    (skip-chars-forward "}\r\n")
	    (css-store-rule attrs applies-to sheet))
	  )
	 )
	(skip-chars-forward " \t\r\n"))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    sheet)
  )

;; Tools for pretty-printing an existing stylesheet.
(defun css-rule-name (rule)
  (cond
   ((listp (car rule))			; Contextual
    (mapconcat 'css-rule-name 
	       (reverse rule) " "))
   ((listp (cdr rule))			; More than one class
    (let ((classes (cdr rule))
	  (rval (symbol-name (car rule))))
      (while classes
	(setq rval (concat rval
			   (if (= (aref (car classes) 0) ?:)
			       (pop classes)
			     (concat "." (pop classes))))))
      rval))
   (t
    (symbol-name (car rule)))))

(defun css-display (sheet)
  (with-output-to-temp-buffer "CSS Stylesheet"
    (set-buffer standard-output)
    (indented-text-mode)
    (insert "# Stylesheet auto-regenerated by css.el\n#\n"
	    "# This is a mixture of the default stylesheet and any\n"
	    "# styles specified by the document.  The rules are in no\n"
	    "# particular order.\n\n")
    (let (tmp cur)
      (maphash
       (lambda (_k v)
         (while v
           (setq cur (pop v))
           (insert (css-rule-name (car cur)))
           (insert " { ")
           (insert "\n")
           ;; Display the rules
           (setq tmp (cdr cur))
           (let (prop val)
             (while tmp
               (setq prop (caar tmp)
                     val (cdar tmp)
                     tmp (cdr tmp))
               (case (get prop 'css-type)
                 (symbol-list
                  (setq val (mapconcat 'symbol-name val ",")))
                 (weight
                  (setq val (substring (symbol-name val) 1 nil)))
                 (otherwise
                  nil)
                 )
               (insert (format "  %s: %s;\n" prop val))))
           (insert "}\n\n")             ;
           ))
       sheet))))

(provide 'css)
