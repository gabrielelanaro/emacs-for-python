;;; prv-xemacs.el --- XEmacs support for preview-latex

;; Copyright (C) 2001-2006 Free Software Foundation, Inc.

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

(require 'overlay)
(require 'tex-site)
(require 'tex)
(require 'latex)

;; Compatibility macros and functions.

(eval-when-compile
  (defvar preview-compatibility-macros nil
    "List of macros only present when compiling/loading uncompiled.")

  (defmacro preview-defmacro (name &rest rest)
    (when (featurep 'xemacs)
      (push
       (if (fboundp name)
           (cons name (symbol-function name))
         name)
       preview-compatibility-macros)
      `(eval-when-compile (defmacro ,name ,@rest))))
  (push 'preview-defmacro preview-compatibility-macros))

(preview-defmacro assoc-default (key alist test)
  `(cdr (assoc* ,key ,alist
                :test #'(lambda(a b) (funcall ,test b a)))))

(preview-defmacro display-mm-height () '(device-mm-height))
(preview-defmacro display-mm-width () '(device-mm-width))
(preview-defmacro display-pixel-height () '(device-pixel-height))
(preview-defmacro display-pixel-width () '(device-pixel-width))
(preview-defmacro line-beginning-position () '(point-at-bol))
(preview-defmacro line-end-position () '(point-at-eol))

;; This is not quite the case, but unless we're playing with duplicable extents,
;; the two are equivalent in XEmacs.
(preview-defmacro match-string-no-properties (&rest args)
  `(match-string ,@args))

(preview-defmacro face-attribute (face attr)
  (cond
    ((eq attr :height)
     `(round (/ (* ,(/ 720.0 25.4)
		   (face-height ,face)
		   (device-mm-height))
		(device-pixel-height))))
    ((eq attr :foreground)
     `(face-foreground-instance ,face))
    ((eq attr :background)
     `(face-background-instance ,face))
    (t
     (error 'unimplemented (format "Don't know how to fake %s" attr)))))

(preview-defmacro make-temp-file (prefix dir-flag)
  (if (not dir-flag)
      (error 'unimplemented "Can only fake make-temp-file for directories"))
  `(let (file)
     (while (condition-case ()
                (progn
                  (setq file
                        (make-temp-name ,prefix))
                  (make-directory file)
                  nil)
              (file-already-exists t))
       nil)
     file))

(preview-defmacro set-buffer-multibyte (multibyte)
  "Set the representation type of the current buffer.  If MULTIBYTE
is non-`nil', the buffer becomes multibyte.  If MULTIBYTE is
`nil', the buffer becomes unibyte.

Because XEmacs does not implement multibyte versus unibyte buffers
per se (they just have encodings which may be unibyte or multibyte),
this is only implemented for the `nil' case."
  (if (not multibyte)
      `(if (fboundp 'set-buffer-file-coding-system)
           (set-buffer-file-coding-system 'binary))
    (error 'unimplemented "`set-buffer-multibyte is only implemented for the binary case.")))

(preview-defmacro next-single-char-property-change (pos prop)
  "Return the position of next property change for a specific property.
This is like `next-single-property-change', except that if no
change is found before the end of the buffer, it returns
\(point-max) rather than `nil'."
  `(or (next-single-property-change ,pos ,prop)
       (point-max)))

(preview-defmacro previous-single-char-property-change (pos prop)
  "Return the position of previous property change for a specific property.
This is like `next-single-property-change', except that if no
change is found before the end of the buffer, it returns
\(point-min) rather than `nil'."
  `(or (previous-single-property-change ,pos ,prop)
       (point-min)))

(preview-defmacro with-temp-message (message &rest body)
  "Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area.

The message is displayed with label `progress'; see `display-message'."
  (let ((current-message (make-symbol "current-message"))
        (temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
           (,current-message))
       (unwind-protect
           (progn
             (when ,temp-message
               (setq ,current-message (current-message))
               (display-message 'progress ,temp-message))
             ,@body)
         (and ,temp-message
              (if ,current-message
                  (display-message 'progress ,current-message)
                (message nil)))))))

(defun preview-mark-active ()
  "Return t if the mark is active."
  (and (mark)
       t))

(defvar preview-transparent-border)

;; Images.

(defsubst preview-supports-image-type (imagetype)
  "Return whether IMAGETYPE is supported by XEmacs."
  (memq imagetype (image-instantiator-format-list)))

;; TODO: Generalize this so we can create the fixed icons using it.

;; Argh, dired breaks :file :(
;; This is a temporary kludge to get around that until a fixed dired
;; or a fixed XEmacs is released.

(defun preview-create-icon-1 (file type ascent)
  "Create an icon from FILE, image TYPE and ASCENT."
  (let ((glyph
	 (make-glyph
	  (vector type
		  :file file
		  :data (with-temp-buffer
			  (insert-file-contents-literally file)
			  (buffer-string))))))
    (set-glyph-baseline glyph ascent)
    glyph))

(defun preview-create-icon (file type ascent border)
  "Create an icon from FILE, image TYPE, ASCENT and BORDER."
  (list
   (preview-create-icon-1 file type ascent)
   file type ascent border))

(defvar preview-ascent-spec)

(put 'preview-filter-specs :type
     #'(lambda (keyword value &rest args)
	 (if (preview-supports-image-type value)
	     (let* (preview-ascent-spec
		    (glyph (make-glyph `[,value
					 ,@(preview-filter-specs-1 args)])))
	       (when preview-ascent-spec
		 (set-glyph-baseline glyph preview-ascent-spec))
	       glyph)
	   (throw 'preview-filter-specs nil))))

(put 'preview-filter-specs :ascent
     #'(lambda (keyword value &rest args)
	 (setq preview-ascent-spec value)
	 (preview-filter-specs-1 args)))

;; No defcustom here: does not seem to make sense.

(defvar preview-tb-icon-specs
  '((:type xpm :file "prvtex-cap-up.xpm" :ascent 75)
    (:type xbm :file "prvtex24.xbm" :ascent 75)))

(defvar preview-tb-icon nil)

;; Image frobbing.

(defun preview-add-urgentization (fun ov &rest rest)
  "Cause FUN (function call form) to be called when redisplayed.
FUN must be a form with OV as first argument,
REST as the remainder, returning T.  An alternative is to give
what `preview-remove-urgentization' returns, this will reinstate
the previous state."
  (set-extent-initial-redisplay-function
   ov
   (if (null rest)
       fun
     `(lambda (ov) (,fun ,ov ,@rest)))))

(defun preview-remove-urgentization (ov)
  "Undo urgentization of OV by `preview-add-urgentization'.
Returns the old arguments to `preview-add-urgentization'
if there was any urgentization."
  (prog1 (list (extent-property ov 'initial-redisplay-function) ov)
    (set-extent-initial-redisplay-function ov nil)))

(defsubst preview-icon-copy (icon)
  "Prepare for a later call of `preview-replace-active-icon'."
  icon)

(defsubst preview-replace-active-icon (ov replacement)
  "Replace the active Icon in OV by REPLACEMENT, another icon."
  (set-extent-property ov 'preview-image replacement)
  (add-text-properties 0 1 (list 'end-glyph (car replacement))
		       (car (extent-property ov 'strings)))
  (if (eq (extent-property ov 'preview-state) 'active)
      (set-extent-end-glyph ov (car replacement))))

(defvar preview-button-1 'button2)
(defvar preview-button-2 'button3)

;; The `x' and invisible junk is because XEmacs doesn't bother to insert
;; the extents of a zero-length string. Bah.
;; When this is fixed, we'll autodetect this case and use zero-length
;; strings where possible.
(defmacro preview-make-clickable (&optional map glyph helpstring click1 click2)
  "Generate a clickable string or keymap.
If MAP is non-nil, it specifies a keymap to add to, otherwise
a new one is created.  If GLYPH is given, the result is made
to display it wrapped in a string.  In that case,
HELPSTRING is a format string with one or two %s specifiers
for preview's clicks, displayed as a help-echo.  CLICK1 and CLICK2
are functions to call on preview's clicks."
  `(let (,@(and glyph '((res (copy-sequence "x"))))
           (resmap ,(or map '(make-sparse-keymap))))
     ,@(if click1
           `((define-key resmap preview-button-1 ,click1)))
     ,@(if click2
           `((define-key resmap preview-button-2 ,click2)))
     ,@(if glyph
	   `((add-text-properties
              0 1
              (list 'end-glyph ,glyph
		    'mouse-face 'highlight
              'preview-balloon-help
	      ,(if (stringp helpstring)
		   (format helpstring preview-button-1 preview-button-2)
		 `(format ,helpstring preview-button-1 preview-button-2))
              'preview-keymap resmap)
              res)
             res)
	 '(resmap))))

(defun preview-click-reroute (ov event)
  "If OV received a click EVENT on a glyph, reroute to special map."
  (let ((oldmap (extent-keymap ov)))
    (unwind-protect
	(progn
	  (set-extent-keymap ov
			     (and (event-over-glyph-p event)
				  (extent-property ov 'preview-keymap)))
	  (dispatch-event event))
      (set-extent-keymap ov oldmap))))

(defun preview-reroute-map (ov)
  "Get rerouting keymap for OV for catching glyph clicks only."
  (let ((map (make-sparse-keymap))
	(fun `(lambda (event)
		(interactive "e")
		(preview-click-reroute ,ov event))))
    (define-key map preview-button-1 fun)
    (define-key map preview-button-2 fun)
    map))

(defun preview-balloon-reroute (ov)
  "Give balloon help only if over glyph of OV."
  (and (eq ov (event-glyph-extent (mouse-position-as-motion-event)))
       (extent-property ov 'preview-balloon-help)))

;; Most of the changes to this are junking the use of overlays;
;; a bit of it is different, and there's a little extra paranoia.

;; We also have to move the image from the begin to the end-glyph
;; whenever the extent is invisible because of a bug in XEmacs-21.4's
;; redisplay engine.
(defun preview-toggle (ov &optional arg event)
  "Toggle visibility of preview overlay OV.
ARG can be one of the following: t displays the overlay,
nil displays the underlying text, and 'toggle toggles.
If EVENT is given, it indicates the window where the event
occured, either by being a mouse event or by directly being
the window in question.  This may be used for cursor restoration
purposes."
  (if (not (bufferp (extent-object ov)))
      (error 'wrong-type-argument ov))
  (let ((old-urgent (preview-remove-urgentization ov))
        (preview-state
         (if (if (eq arg 'toggle)
                 (not (eq (extent-property ov 'preview-state) 'active))
               arg)
             'active
           'inactive))
        (strings (extent-property ov 'strings)))
    (unless (eq (extent-property ov 'preview-state) 'disabled)
      (set-extent-property ov 'preview-state preview-state)
      (if (eq preview-state 'active)
          (progn
	    (unless (extent-keymap ov)
	      (set-extent-keymap ov (preview-reroute-map ov))
	      (set-extent-property ov 'balloon-help #'preview-balloon-reroute))
	    (set-extent-begin-glyph ov nil)
	    (set-extent-end-glyph-layout ov 'text)
	    (set-extent-end-glyph ov (get-text-property
				      0 'end-glyph (car strings)))
            (set-extent-properties ov '(invisible t
					isearch-open-invisible ignore
					isearch-invisible t
                                        face nil))
	    (dolist (prop '(preview-keymap
			    mouse-face preview-balloon-help))
              (set-extent-property ov prop
                                   (get-text-property 0 prop (car strings)))))
	(unless (cdr strings)
	  (setcdr strings (preview-inactive-string ov)))
	(set-extent-end-glyph ov nil)
	(set-extent-begin-glyph-layout ov 'text)
	(set-extent-begin-glyph ov (get-text-property
				    0 'end-glyph (cdr strings)))
        (set-extent-properties ov `(face preview-face
				    mouse-face nil
				    invisible nil
				    isearch-invisible nil
				    preview-keymap
				    ,(get-text-property
				     0 'preview-keymap (cdr strings))
				    preview-balloon-help
				    ,(get-text-property
				     0 'preview-balloon-help (cdr strings)))))
      (if old-urgent
          (apply 'preview-add-urgentization old-urgent))))
  (if event
      (preview-restore-position
       ov
       (if (windowp event)
	   event
	 (event-window event)))))

; Does FALLBACKS need to be implemented? Likely not.
(defmacro preview-inherited-face-attribute (face attribute &optional
                                              fallbacks)
  "Fetch face attribute while adhering to inheritance.
This searches FACE and all its ancestors for an ATTRIBUTE.
FALLBACKS is unused."
  `(face-attribute ,face ,attribute))

(defun preview-get-colors ()
  "Return colors from the current display.
Fetches the current screen colors and makes a vector
of colors as numbers in the range 0..65535.
Pure borderless black-on-white will return quadruple NIL."
  (let
      ((bg (color-instance-rgb-components (preview-inherited-face-attribute
             'preview-reference-face :background 'default)))
       (fg (color-instance-rgb-components (preview-inherited-face-attribute
                                           'preview-reference-face :foreground 'default))))
    (if (equal '(65535 65535 65535) bg)
        (setq bg nil))
    (if (equal '(0 0 0) fg)
        (setq fg nil))
    (vector bg fg nil nil)))

(defcustom preview-use-balloon-help nil
  "*Is balloon help enabled in preview-latex?"
  :group 'preview-appearance
  :type 'boolean)

(defcustom preview-buffer-recoding-alist
  (if (and (= emacs-major-version 21)
	   (< emacs-minor-version 5))
      '((utf-8-unix . raw-text-unix)
	(utf-8-dos . raw-text-dos)
	(utf-8-mac . raw-text-mac)
	(utf-8 . raw-text)))
  "Translate buffer encodings into process encodings.
TeX is sometimes bad dealing with 8bit encodings and rather bad
dealing with multibyte encodings.  So the process encoding output
might need to get temporarily reprocessed into the original byte
stream before the buffer characters can be identified.  XEmacs
21.4 is rather bad at preserving incomplete multibyte characters
in that process.  This variable makes it possible to use a
reconstructable coding system in the run buffer instead.  Specify
an alist of base coding system names here, which you can get
using

  \(coding-system-name (coding-system-base buffer-file-coding-system))

in properly detected buffers."
  :group 'preview-latex
  :type '(repeat (cons symbol symbol)))

(defun preview-buffer-recode-system (base)
  "This is supposed to translate unrepresentable base encodings
 into something that can be used safely for byte streams in the
 run buffer.  XEmacs mule-ucs is so broken that this may be
 needed."
  (or (cdr (assq (coding-system-name base)
		 preview-buffer-recoding-alist))
      base))

(defun preview-mode-setup ()
  "Setup proper buffer hooks and behavior for previews."
  (set (make-local-variable 'desktop-save-buffer)
       #'desktop-buffer-preview-misc-data)
  (mapc #'make-local-hook
        '(pre-command-hook post-command-hook
	  before-change-functions after-change-functions))
  (add-hook 'pre-command-hook #'preview-mark-point nil t)
  (add-hook 'post-command-hook #'preview-move-point nil t)
  (and preview-use-balloon-help
       (not (and (boundp 'balloon-help-mode)
		 balloon-help-mode))
       (balloon-help-minor-mode 1))
  (add-hook 'before-change-functions #'preview-handle-before-change nil t)
  (add-hook 'after-change-functions #'preview-handle-after-change nil t)
  (easy-menu-add preview-menu)
  (unless preview-tb-icon
    (setq preview-tb-icon (preview-filter-specs
				       preview-tb-icon-specs))
    (when preview-tb-icon
      (setq preview-tb-icon
	    (vector
	     (list preview-tb-icon)
	     #'preview-at-point
	     t
	     "Preview on/off at point"))))
;;; [Courtesy Stephen J. Turnbull, with some modifications
;;;  Message-ID: <87el9fglsj.fsf@tleepslib.sk.tsukuba.ac.jp>
;;;  I could not have figured this out for the world]
;;; Hm, there really ought to be a way to get the spec that would be
;;; instantiated in a given domain
  (when preview-tb-icon
    (let ((tb (cdadar (or (specifier-spec-list default-toolbar (current-buffer))
			  (specifier-spec-list default-toolbar 'global)))))
      (unless (member preview-tb-icon tb)
	(set-specifier default-toolbar
		       (append tb (list preview-tb-icon))
		       (current-buffer)))))
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
  (when (eq (extent-object ov) (window-buffer window))
    (with-current-buffer (extent-object ov)
      (if (eq (extent-property ov 'preview-state) 'active)
	  (setq preview-last-location
		(set-marker (or preview-last-location (make-marker))
			    (window-point window)))
	(when (and
	       (markerp preview-last-location)
	       (eq (extent-object ov) (marker-buffer preview-last-location))
	       (< (extent-start-position ov) preview-last-location)
	       (> (extent-end-position ov) preview-last-location))
	  (set-window-point window preview-last-location))))))

(defun preview-move-point ()
  "Move point out of fake-intangible areas."
  (preview-check-changes)
  (let (newlist (pt (point)) distance)
    (setq preview-temporary-opened
	  (dolist (ov preview-temporary-opened newlist)
	    (and (extent-object ov)
		 (not (extent-detached-p ov))
		 (eq (extent-property ov 'preview-state) 'inactive)
		 (if (and (eq (extent-object ov) (current-buffer))
			  (or (<= pt (extent-start-position ov))
			      (>= pt (extent-end-position ov))))
		     (preview-toggle ov t)
		   (push ov newlist)))))
    (if	(preview-auto-reveal-p
	 preview-auto-reveal
	 (setq distance
	       (and (eq (marker-buffer preview-marker)
			(current-buffer))
		    (- pt (marker-position preview-marker)))))
	(map-extents #'preview-open-overlay nil
		     pt pt nil nil 'preview-state 'active)
      (let (newpt)
	(while (setq newpt
		     (map-extents #'preview-skip-overlay nil
				  pt pt (and distance (< distance 0)) nil
				  'preview-state 'active))
	  (setq pt newpt))
	(goto-char pt)))))

(defun preview-skip-overlay (ovr backward)
  "Skip point over OVR, BACKWARD is set if backwards.
Returns new position or NIL."
  (if backward
      (and (> (extent-start-position ovr) (point-min))
	   (1- (extent-start-position ovr)))
    (and (<= (extent-end-position ovr) (point-max))
	 (> (extent-end-position ovr) (extent-start-position ovr))
	 (extent-end-position ovr))))

(defun preview-open-overlay (ovr ignored)
  "Open the active preview OVR, IGNORED gets ignored.
NIL is returned: this is for `map-extents'."
  (preview-toggle ovr)
  (push ovr preview-temporary-opened)
  nil)

(defadvice isearch-highlight (before preview protect disable)
  "Make isearch open preview text that's a search hit.
Also make `query-replace' open preview text about to be replaced."
  (map-extents #'preview-open-overlay nil
	       (ad-get-arg 0) (ad-get-arg 1)
	       nil nil 'preview-state 'active))

(defcustom preview-query-replace-reveal t
  "*Make `isearch' and `query-replace' autoreveal previews."
  :group 'preview-appearance
  :type 'boolean
  :require 'preview
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (if value
	     (ad-enable-advice 'isearch-highlight 'before 'preview)
	   (ad-disable-advice 'isearch-highlight 'before 'preview))
	 (ad-activate 'isearch-highlight))
  :initialize #'custom-initialize-reset)

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

(defvar preview-preamble-format-cons nil
  "Where our preamble is supposed to end.")
(make-variable-buffer-local 'preview-preamble-format-cons)

(defun preview-preamble-check-change (beg end)
  "Hook function for change hooks on preamble.
Reacts to changes between BEG and END."
  (when (and (consp (cdr preview-preamble-format-cons))
	     (cddr preview-preamble-format-cons)
	     (< beg (cddr preview-preamble-format-cons)))
    (preview-unwatch-preamble preview-preamble-format-cons)
    (preview-format-kill preview-preamble-format-cons)
    (setcdr preview-preamble-format-cons t)))

(defun preview-watch-preamble (file command format-cons)
  "Set up a watch on master file FILE.
FILE can be an associated buffer instead of a filename.
COMMAND is the command that generated the format.
FORMAT-CONS contains the format info for the main
format dump handler."
  (let ((buffer (if (bufferp file)
		    file
		  (find-buffer-visiting file))) ov)
    (setq preview-preamble-format-cons nil)
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
		   (setq preview-preamble-format-cons format-cons)
		   (point)))))))))

(defun preview-unwatch-preamble (format-cons)
  "Stop watching a format on FORMAT-CONS.
The watch has been set up by `preview-watch-preamble'."
  (when (consp (cdr format-cons))
    (setcdr (cdr format-cons) nil)))

(defun preview-register-change (ov map-arg)
  "Register not yet changed OV for verification.
This stores the old contents of the overlay in the
`preview-prechange' property and puts the overlay into
`preview-change-list' where `preview-check-changes' will
find it at some later point of time.  MAP-ARG is ignored;
it is usually generated by `map-extents'."
  (unless (extent-property ov 'preview-prechange)
    (if (eq (extent-property ov 'preview-state) 'disabled)
	(set-extent-property ov 'preview-prechange t)
      (set-extent-property ov
			   'preview-prechange
			   (save-restriction
			     (widen)
			     (buffer-substring-no-properties
			      (extent-start-position ov)
			      (extent-end-position ov)))))
    (push ov preview-change-list))
  nil)

(defun preview-check-changes ()
  "Check whether the contents under the overlay have changed.
Disable it if that is the case.  Ignores text properties."
  (dolist (ov preview-change-list)
    (condition-case nil
	(with-current-buffer (extent-object ov)
	  (let ((text (save-restriction
			(widen)
			(buffer-substring-no-properties
			 (extent-start-position ov)
			 (extent-end-position ov)))))
	    (if (or (zerop (length text))
		    (extent-detached-p ov))
		(preview-delete ov)
	      (unless
		  (or (eq (extent-property ov 'preview-state) 'disabled)
		      (preview-relaxed-string=
		       text (extent-property ov 'preview-prechange)))
		(preview-disable ov)))))
      (error nil))
    (set-extent-property ov 'preview-prechange nil))
  (setq preview-change-list nil))

(defun preview-handle-before-change (beg end)
  "Hook function for `before-change-functions'.
Receives BEG and END, the affected region."
  (map-extents #'preview-register-change nil beg end
	       nil nil 'preview-state)
  (preview-preamble-check-change beg end))

(defun preview-handle-after-change (beg end length)
  "Hook function for `after-change-functions'.
Receives BEG and END, the affected region, and LENGTH
of an insertion."
  (when (and preview-change-list
	     (zerop length)
	     (not (eq this-command 'undo)))
    (map-extents (lambda (ov maparg)
		   (set-extent-endpoints
		    ov maparg (extent-end-position ov))) nil
		    beg beg end 'start-in-region 'preview-state 'active)
    (map-extents (lambda (ov maparg)
		   (set-extent-endpoints
		    ov (extent-start-position ov) maparg)) nil
		    end end beg 'end-in-region 'preview-state 'active)))

(defun preview-import-image (image)
  "Convert the printable IMAGE rendition back to an image."
  (cond ((stringp image)
	 (setq image (copy-sequence image))
	 (add-text-properties 0 (length image)
			      '(face preview-face)
			      image)
	 image)
	((eq (car image) 'image)
	 (let ((plist (cdr image)))
	   (preview-create-icon-1
	    (plist-get plist :file)
	    (plist-get plist :type)
	    (plist-get plist :ascent))))
	(t
	 (preview-create-icon-1 (nth 0 image)
				(nth 1 image)
				(nth 2 image)))))

(provide 'prv-xemacs)

;;; Local variables:
;;; eval: (put 'preview-defmacro 'lisp-indent-function 'defun)
;;; end:

;;; prv-xemacs.el ends here
