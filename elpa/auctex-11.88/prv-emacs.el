;;; prv-emacs.el --- GNU Emacs specific code for preview.el

;; Copyright (C) 2001, 02, 03, 04, 05  Free Software Foundation, Inc.

;; Author: David Kastrup
;; Keywords: convenience, tex, wp

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
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'tex-site)
(require 'tex)
(require 'latex)

(defvar preview-compatibility-macros nil
  "List of macros only present when compiling/loading.")

(defcustom preview-transparent-color '(highlight :background)
  "Color to appear transparent in previews.
Set this to something unusual when using `preview-transparent-border',
to the default background in most other cases."
  :type '(radio (const :tag "None" nil)
		 (const :tag "Autodetect" t)
		 (color :tag "By name" :value "white")
		 (list :tag "Take from face"
		       :value (default :background)
		       (face)
		       (choice :tag "What to take"
			(const :tag "Background" :value :background)
			(const :tag "Foreground" :value :foreground))))
  :group 'preview-appearance)

;;; Note that the following default introduces a border only when
;;; Emacs blinks politely when point is on an image (the tested
;;; unrelated function was introduced at about the time image blinking
;;; became tolerable).
(defcustom preview-transparent-border (unless (fboundp 'posn-object-x-y) 1.5)
  "Width of transparent border for previews in pt.
Setting this to a numeric value will add a border of
`preview-transparent-color' around images, and will turn
the heuristic-mask setting of images to default to 't since
then the borders are correctly detected even in case of
palette operations.  If the transparent color is something
not present otherwise in the image, the cursor display
will affect just this border.  A width of 0 is interpreted
by PostScript as meaning a single pixel, other widths are
interpreted as PostScript points (1/72 of 1in)"
  :group 'preview-appearance
  :type '(choice (const :value nil :tag "No border")
		 (number :value 1.5 :tag "Border width in pt")))

(defun preview-get-heuristic-mask ()
  "Get heuristic-mask to use for previews.
Consults `preview-transparent-color'."
  (cond ((stringp preview-transparent-color)
	 (color-values preview-transparent-color))
	((or (not (consp preview-transparent-color))
	     (integerp (car preview-transparent-color)))
	 preview-transparent-color)
	(t (color-values (preview-inherited-face-attribute
			  (nth 0 preview-transparent-color)
			  (nth 1 preview-transparent-color)
			  'default)))))

(defsubst preview-create-icon-1 (file type ascent border)
  `(image
    :file ,file
    :type ,type
    :ascent ,ascent
    ,@(and border
	   '(:mask (heuristic t)))))

(defun preview-create-icon (file type ascent border)
  "Create an icon from FILE, image TYPE, ASCENT and BORDER."
  (list
   (preview-create-icon-1 file type ascent border)
   file type ascent border))

(put 'preview-filter-specs :type
     #'(lambda (keyword value &rest args)
	 (if (image-type-available-p value)
	     `(image :type ,value
		     ,@(preview-filter-specs-1 args))
	   (throw 'preview-filter-specs nil))))

;; No defcustom here: does not seem to make sense.

(defvar preview-tb-icon-specs
  '((:type xpm :file "prvtex24.xpm")
    (:type xbm :file "prvtex24.xbm")))

(defvar preview-tb-icon nil)

(defun preview-add-urgentization (fun ov &rest rest)
  "Cause FUN (function call form) to be called when redisplayed.
FUN must be a form with OV as first argument,
REST as the remainder, returning T."
  (let ((dispro (overlay-get ov 'display)))
    (unless (eq (car dispro) 'when)
      (overlay-put ov 'display `(when (,fun ,ov ,@rest)  . ,dispro)))))

(defun preview-remove-urgentization (ov)
  "Undo urgentization of OV by `preview-add-urgentization'.
Returns the old arguments to `preview-add-urgentization'
if there was any urgentization."
  (let ((dispro (overlay-get ov 'display)))
    (when (eq (car-safe dispro) 'when)
      (prog1
	  (car (cdr dispro))
	(overlay-put ov 'display (cdr (cdr dispro)))))))

(defsubst preview-icon-copy (icon)
  "Prepare a later call of `preview-replace-active-icon'."

  ;; This is just a GNU Emacs specific efficiency hack because it
  ;; is easy to do.  When porting, don't do anything complicated
  ;; here, rather deliver just the unchanged icon and make
  ;; `preview-replace-active-icon' do the necessary work of replacing
  ;; the icon where it actually has been stored, probably
  ;; in the car of the strings property of the overlay.  This string
  ;; might probably serve as a begin-glyph as well, in which case
  ;; modifying the string in the strings property would change that
  ;; glyph automatically.

  (cons 'image (cdr icon)))

(defsubst preview-replace-active-icon (ov replacement)
  "Replace the active Icon in OV by REPLACEMENT, another icon."
  (let ((img (overlay-get ov 'preview-image)))
    (setcdr (car img) (cdar replacement))
    (setcdr img (cdr replacement))))

(defvar preview-button-1 [mouse-2])
(defvar preview-button-2 [mouse-3])

(defmacro preview-make-clickable (&optional map glyph helpstring click1 click2)
  "Generate a clickable string or keymap.
If MAP is non-nil, it specifies a keymap to add to, otherwise
a new one is created.  If GLYPH is given, the result is made
to display it wrapped in a string.  In that case,
HELPSTRING is a format string with one or two %s specifiers
for preview's clicks, displayed as a help-echo.  CLICK1 and CLICK2
are functions to call on preview's clicks."
  `(let ((resmap ,(or map '(make-sparse-keymap))))
     ,@(if click1
           `((define-key resmap preview-button-1 ,click1)))
     ,@(if click2
           `((define-key resmap preview-button-2 ,click2)))
     ,(if glyph
	  `(propertize
	    "x"
	    'display ,glyph
	    'mouse-face 'highlight
	    'help-echo
	    ,(if (stringp helpstring)
		 (format helpstring preview-button-1 preview-button-2)
	       `(format ,helpstring preview-button-1 preview-button-2))
	    'keymap resmap)
	'resmap)))

(defvar preview-overlay nil)

(put 'preview-overlay
     'modification-hooks
     '(preview-handle-modification))

(put 'preview-overlay
     'insert-in-front-hooks
     '(preview-handle-insert-in-front))

(put 'preview-overlay
     'insert-behind-hooks
     '(preview-handle-insert-behind))

;; We have to fake our way around atomicity.

;; Here is the beef: for best intuitiveness, we want to have
;; insertions be carried out as expected before iconized text
;; passages, but we want to insert *into* the overlay when not
;; iconized.  A preview that has become empty can not get content
;; again: we remove it.  A disabled preview needs no insert-in-front
;; handler.

(defvar preview-change-list nil
  "List of tentatively changed overlays.")

(defcustom preview-dump-threshold
  "^ *\\\\begin *{document}[ %]*$"
  "*Regexp denoting end of preamble.
This is the location up to which preamble changes are considered
to require redumping of a format."
  :group 'preview-latex
  :type 'string)

(defun preview-preamble-changed-function
  (ov after-change beg end &optional length)
  "Hook function for change hooks on preamble.
See info node `(elisp) Overlay Properties' for
definition of OV, AFTER-CHANGE, BEG, END and LENGTH."
  (let ((format-cons (overlay-get ov 'format-cons)))
    (preview-unwatch-preamble format-cons)
    (preview-format-kill format-cons)
    (setcdr format-cons t)))

(defun preview-watch-preamble (file command format-cons)
  "Set up a watch on master file FILE.
FILE can be an associated buffer instead of a filename.
COMMAND is the command that generated the format.
FORMAT-CONS contains the format info for the main
format dump handler."
  (let ((buffer (if (bufferp file)
		    file
		  (find-buffer-visiting file))) ov)
    (setcdr
     format-cons
     (cons command
	   (when buffer
	     (with-current-buffer buffer
	       (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (unless (re-search-forward preview-dump-threshold nil t)
		     (error "Can't find preamble of `%s'" file))
		   (setq ov (make-overlay (point-min) (point)))
		   (overlay-put ov 'format-cons format-cons)
		   (overlay-put ov 'insert-in-front-hooks
				'(preview-preamble-changed-function))
		   (overlay-put ov 'modification-hooks
				'(preview-preamble-changed-function))
		   ov))))))))

(defun preview-unwatch-preamble (format-cons)
  "Stop watching a format on FORMAT-CONS.
The watch has been set up by `preview-watch-preamble'."
  (when (consp (cdr format-cons))
    (when (cddr format-cons)
      (delete-overlay (cddr format-cons)))
    (setcdr (cdr format-cons) nil)))

(defun preview-register-change (ov)
  "Register not yet changed OV for verification.
This stores the old contents of the overlay in the
`preview-prechange' property and puts the overlay into
`preview-change-list' where `preview-check-changes' will
find it at some later point of time."
  (unless (overlay-get ov 'preview-prechange)
    (if (eq (overlay-get ov 'preview-state) 'disabled)
	(overlay-put ov 'preview-prechange t)
      (overlay-put ov 'preview-prechange
		   (save-restriction
		     (widen)
		     (buffer-substring-no-properties
		      (overlay-start ov) (overlay-end ov)))))
    (push ov preview-change-list)))

(defun preview-check-changes ()
  "Check whether the contents under the overlay have changed.
Disable it if that is the case.  Ignores text properties."
  (dolist (ov preview-change-list)
    (condition-case nil
	(with-current-buffer (overlay-buffer ov)
	  (let ((text (save-restriction
			(widen)
			(buffer-substring-no-properties
			 (overlay-start ov) (overlay-end ov)))))
	    (if (zerop (length text))
		(preview-delete ov)
	      (unless
		  (or (eq (overlay-get ov 'preview-state) 'disabled)
		      (preview-relaxed-string=
		       text (overlay-get ov 'preview-prechange)))
		(overlay-put ov 'insert-in-front-hooks nil)
		(overlay-put ov 'insert-behind-hooks nil)
		(preview-disable ov)))))
      (error nil))
    (overlay-put ov 'preview-prechange nil))
  (setq preview-change-list nil))

(defun preview-handle-insert-in-front
  (ov after-change beg end &optional length)
  "Hook function for `insert-in-front-hooks' property.
See info node `(elisp) Overlay Properties' for
definition of OV, AFTER-CHANGE, BEG, END and LENGTH."
  (if after-change
      (unless undo-in-progress
	(if (eq (overlay-get ov 'preview-state) 'active)
	    (move-overlay ov end (overlay-end ov))))
    (preview-register-change ov)))

(defun preview-handle-insert-behind
  (ov after-change beg end &optional length)
  "Hook function for `insert-behind-hooks' property.
This is needed in case `insert-before-markers' is used at the
end of the overlay.  See info node `(elisp) Overlay Properties'
for definition of OV, AFTER-CHANGE, BEG, END and LENGTH."
  (if after-change
      (unless undo-in-progress
	(if (eq (overlay-get ov 'preview-state) 'active)
	    (move-overlay ov (overlay-start ov) beg)))
    (preview-register-change ov)))

(defun preview-handle-modification
  (ov after-change beg end &optional length)
  "Hook function for `modification-hooks' property.
See info node `(elisp) Overlay Properties' for
definition of OV, AFTER-CHANGE, BEG, END and LENGTH."
  (unless after-change
    (preview-register-change ov)))

(defun preview-toggle (ov &optional arg event)
  "Toggle visibility of preview overlay OV.
ARG can be one of the following: t displays the overlay,
nil displays the underlying text, and 'toggle toggles.
If EVENT is given, it indicates the window where the event
occured, either by being a mouse event or by directly being
the window in question.  This may be used for cursor restoration
purposes."
  (let ((old-urgent (preview-remove-urgentization ov))
	(preview-state
	 (if (if (eq arg 'toggle)
		 (null (eq (overlay-get ov 'preview-state) 'active))
	       arg)
	     'active
	   'inactive))
	(strings (overlay-get ov 'strings)))
    (unless (eq (overlay-get ov 'preview-state) 'disabled)
      (overlay-put ov 'preview-state preview-state)
      (if (eq preview-state 'active)
	  (progn
	    (overlay-put ov 'category 'preview-overlay)
	    (if (eq (overlay-start ov) (overlay-end ov))
		(overlay-put ov 'before-string (car strings))
	      (dolist (prop '(display keymap mouse-face help-echo))
		(overlay-put ov prop
			     (get-text-property 0 prop (car strings))))
	      (overlay-put ov 'before-string nil))
	    (overlay-put ov 'face nil))
	(dolist (prop '(display keymap mouse-face help-echo))
	  (overlay-put ov prop nil))
	(overlay-put ov 'face 'preview-face)
	(unless (cdr strings)
	  (setcdr strings (preview-inactive-string ov)))
	(overlay-put ov 'before-string (cdr strings)))
      (if old-urgent
	  (apply 'preview-add-urgentization old-urgent))))
  (if event
      (preview-restore-position
       ov
       (if (windowp event)
	   event
	 (posn-window (event-start event))))))

(defsubst preview-buffer-recode-system (base)
  "This is supposed to translate unrepresentable base encodings
into something that can be used safely for byte streams in the
run buffer.  A noop for Emacs."
  base)

(defun preview-mode-setup ()
  "Setup proper buffer hooks and behavior for previews."
  (set (make-local-variable 'desktop-save-buffer)
       #'desktop-buffer-preview-misc-data)
  (add-hook 'pre-command-hook #'preview-mark-point nil t)
  (add-hook 'post-command-hook #'preview-move-point nil t)
  (easy-menu-add preview-menu LaTeX-mode-map)
  (unless preview-tb-icon
    (setq preview-tb-icon (preview-filter-specs preview-tb-icon-specs)))
  (when preview-tb-icon
    (define-key LaTeX-mode-map [tool-bar preview]
      `(menu-item "Preview at point" preview-at-point
		  :image ,preview-tb-icon
		  :help "Preview on/off at point")))
  (when buffer-file-name
    (let* ((filename (expand-file-name buffer-file-name))
	   format-cons)
      (when (string-match (concat "\\." TeX-default-extension "\\'")
			  filename)
	(setq filename (substring filename 0 (match-beginning 0))))
      (setq format-cons (assoc filename preview-dumped-alist))
      (when (consp (cdr format-cons))
	(preview-unwatch-preamble format-cons)
	(preview-watch-preamble (current-buffer)
				(cadr format-cons)
				format-cons)))))

(defvar preview-marker (make-marker)
  "Marker for fake intangibility.")

(defvar preview-temporary-opened nil)

(defvar preview-last-location nil
  "Restored cursor position marker for reopened previews.")
(make-variable-buffer-local 'preview-last-location)

(defun preview-mark-point ()
  "Mark position for fake intangibility."
  (when (eq (get-char-property (point) 'preview-state) 'active)
    (unless preview-last-location
      (setq preview-last-location (make-marker)))
    (set-marker preview-last-location (point))
    (set-marker preview-marker (point))
    (preview-move-point))
  (set-marker preview-marker (point)))

(defun preview-restore-position (ov window)
  "Tweak position after opening/closing preview.
The treated overlay OV has been triggered in WINDOW.  This function
records the original buffer position for reopening, or restores it
after reopening.  Note that by using the mouse, you can open/close
overlays not in the active window."
  (when (eq (overlay-buffer ov) (window-buffer window))
    (with-current-buffer (overlay-buffer ov)
      (if (eq (overlay-get ov 'preview-state) 'active)
	  (setq preview-last-location
		(set-marker (or preview-last-location (make-marker))
			    (window-point window)))
	(when (and
	       (markerp preview-last-location)
	       (eq (overlay-buffer ov) (marker-buffer preview-last-location))
	       (< (overlay-start ov) preview-last-location)
	       (> (overlay-end ov) preview-last-location))
	  (set-window-point window preview-last-location))))))
      
(defun preview-move-point ()
  "Move point out of fake-intangible areas."
  (preview-check-changes)
  (let* (newlist (pt (point)) (lst (overlays-at pt)) distance)
    (setq preview-temporary-opened
	  (dolist (ov preview-temporary-opened newlist)
	    (and (overlay-buffer ov)
		 (eq (overlay-get ov 'preview-state) 'inactive)
		 (if (and (eq (overlay-buffer ov) (current-buffer))
			  (or (<= pt (overlay-start ov))
			      (>= pt (overlay-end ov))))
		     (preview-toggle ov t)
		   (push ov newlist)))))
    (when lst
      (if (or disable-point-adjustment
	      global-disable-point-adjustment
	      (preview-auto-reveal-p
	       preview-auto-reveal
	       (setq distance
		     (and (eq (marker-buffer preview-marker)
			      (current-buffer))
			  (- pt (marker-position preview-marker))))))
	  (preview-open-overlays lst)
	(while lst
	  (setq lst
		(if (and
		     (eq (overlay-get (car lst) 'preview-state) 'active)
		     (> pt (overlay-start (car lst))))
		    (overlays-at
		     (setq pt (if (and distance (< distance 0))
				  (overlay-start (car lst))
				(overlay-end (car lst)))))
		  (cdr lst))))
	(goto-char pt)))))

(defun preview-open-overlays (list &optional pos)
  "Open all previews in LIST, optionally restricted to enclosing POS."
  (dolist (ovr list)
    (when (and (eq (overlay-get ovr 'preview-state) 'active)
	       (or (null pos)
		   (and
		    (> pos (overlay-start ovr))
		    (< pos (overlay-end ovr)))))
      (preview-toggle ovr)
      (push ovr preview-temporary-opened))))

(defadvice replace-highlight (before preview)
  "Make `query-replace' open preview text about to be replaced."
  (preview-open-overlays
   (overlays-in (ad-get-arg 0) (ad-get-arg 1))))

(defcustom preview-query-replace-reveal t
  "*Make `query-replace' autoreveal previews."
  :group 'preview-appearance
  :type 'boolean
  :require 'preview
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (if value
	     (ad-enable-advice 'replace-highlight 'before 'preview)
	   (ad-disable-advice 'replace-highlight 'before 'preview))
	 (ad-activate 'replace-highlight))
  :initialize #'custom-initialize-reset)

;; Check whether the four-argument form of `face-attribute' exists.
;; If not, we will get a `wrong-number-of-arguments' error thrown.
;; Use `defun' instead of `defsubst' here so that the decision may be
;; reverted at load time if you are compiling with one Emacs and using
;; another.
(if (condition-case nil
	(progn
	  (face-attribute 'default :height nil nil)
	  t)
      (wrong-number-of-arguments nil))

    (defun preview-inherited-face-attribute (face attribute &optional inherit)
      "Fetch face attribute while adhering to inheritance.
This searches FACE for an ATTRIBUTE, using INHERIT
for resolving unspecified or relative specs.  See the fourth
argument of function `face-attribute' for details."
      (face-attribute face attribute nil inherit))

  (defun preview-inherited-face-attribute (face attribute &optional inherit)
    "Fetch face attribute while adhering to inheritance.
This searches FACE for an ATTRIBUTE.  If it is 'unspecified,
first inheritance is consulted (if INHERIT is non-NIL), then
INHERIT is searched if it is a face or a list of faces.
Relative specs are evaluated recursively until they get absolute or
are not resolvable.  Relative specs are float values."
    (let ((value (face-attribute face attribute)))
      (when inherit
	(setq inherit
	      (append
	       (let ((ancestors (face-attribute face :inherit)))
		 (cond ((facep ancestors) (list ancestors))
		       ((consp ancestors) ancestors)))
	       (cond ((facep inherit) (list inherit))
		     ((consp inherit) inherit)))))
      (cond ((null inherit) value)
	    ((floatp value)
	     (let ((avalue
		    (preview-inherited-face-attribute
		     (car inherit) attribute (or (cdr inherit) t))))
	       (cond ((integerp avalue)
		      (round (* avalue value)))
		     ((floatp avalue)
		      (* value avalue))
		     (t value))))
	    ((eq value 'unspecified)
	     (preview-inherited-face-attribute
	      (car inherit) attribute (or (cdr inherit) t)))
	    (t value)))))

(defun preview-get-colors ()
  "Return colors from the current display.
Fetches the current screen colors and makes a vector
of colors as numbers in the range 0..65535.
Pure borderless black-on-white will return triple NIL.
The fourth value is the transparent border thickness."
  (let
      ((bg (color-values (preview-inherited-face-attribute
			  'preview-reference-face :background 'default)))
       (fg (color-values (preview-inherited-face-attribute
			  'preview-reference-face :foreground 'default)))
       (mask (preview-get-heuristic-mask)))
    (if (equal '(65535 65535 65535) bg)
	(setq bg nil))
    (if (equal '(0 0 0) fg)
	(setq fg nil))
    (unless (and (numberp preview-transparent-border)
		 (consp mask) (integerp (car mask)))
      (setq mask nil))
    (vector bg fg mask preview-transparent-border)))

(defmacro preview-mark-active ()
  "Return t if the mark is active."
  'mark-active)

(defun preview-import-image (image)
  "Convert the printable IMAGE rendition back to an image."
  (cond ((stringp image)
	 (propertize image 'face 'preview-face))
	((eq (car image) 'image)
	 image)
	(t
	 (preview-create-icon-1 (nth 0 image)
				(nth 1 image)
				(nth 2 image)
				(if (< (length image) 4)
				    (preview-get-heuristic-mask)
				  (nth 3 image))))))

(defsubst preview-supports-image-type (imagetype)
  "Check if IMAGETYPE is supported."
  (image-type-available-p imagetype))

(provide 'prv-emacs)
;;; prv-emacs.el ends here
