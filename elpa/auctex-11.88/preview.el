;;; preview.el --- embed preview LaTeX images in source buffer

;; Copyright (C) 2001-2006, 2010-2014  Free Software Foundation, Inc.

;; Author: David Kastrup
;; Keywords: tex, wp, convenience

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

;; This style is for the "seamless" embedding of generated images
;; into LaTeX source code.  Please see the README and INSTALL files
;; for further instruction.
;;
;; Please use the usual configure script for installation: more than
;; just Elisp files are involved: a LaTeX style, icon files, startup
;; code and so on.
;;
;; Quite a few things with regard to preview-latex's operation can be
;; configured by using
;; M-x customize-group RET preview RET
;;
;; Please report bugs with M-x preview-report-bug RET.

;;; Code:

(require 'tex-site)
(require 'tex)
(require 'tex-buf)
(require 'latex)

(eval-when-compile
  (condition-case nil
      (require 'desktop)
    (file-error (message "Missing desktop package:
preview-latex buffers will not survive across sessions.")))
  (condition-case nil
      (require 'reporter)
    (file-error (message "Missing reporter library, probably from the mail-lib package:
preview-latex's bug reporting commands will probably not work.")))
  (require 'info)
  (defvar error))

;; we need the compatibility macros which do _not_ get byte-compiled.
(eval-when-compile
  (if (featurep 'xemacs)
      (load-library "prv-xemacs.el")))

;; if the above load-library kicked in, this will not cause anything
;; to get loaded.
(require (if (featurep 'xemacs)
	     'prv-xemacs 'prv-emacs))

(defgroup preview nil "Embed Preview images into LaTeX buffers."
  :group 'AUCTeX
  :prefix "preview-"
  :link '(custom-manual "(preview-latex)Top")
  :link '(info-link "(preview-latex)The Emacs interface")
  :link '(url-link :tag "Homepage" "http://www.gnu.org/software/auctex/"))

(defgroup preview-gs nil "Preview's Ghostscript renderer."
  :group 'preview
  :prefix "preview-")

(defgroup preview-appearance nil "Preview image appearance."
  :group 'preview
  :prefix "preview-")

(defconst preview-specs-type
  '(repeat
    (list :tag "Image spec"
	  ;; Use an extra :value keyword to avoid a bug in
	  ;; `widget-convert' of XEmacs 21.4 and Emacs 21.
	  ;; Analogously for the following `const' statements.
	  (const :format "" :value :type)
	  (choice :tag "Image type"
		  (const xpm)
		  (const xbm)
		  (symbol :tag "Other"))
	  (set :inline t :tag "Minimum font size"
	       (list :inline t :tag ""
		     (const :format "" :value :min)
		     (integer :tag "pixels")))
	  (const :format "" :value :file) (string :tag "Filename")
	  (set :inline t :tag "Ascent ratio"
	       (list :inline t :tag ""
		     (const :format "" :value :ascent)
		     (integer :tag "percent of image"
			      :value 50))))))

(defun preview-specs-setter (symbol value)
  "Set SYMBOL to VALUE and clear `preview-min-alist' property.
This is used in icon specs, so that customizing will
clear cached icons."
  (put symbol 'preview-min-alist nil)
  (set-default symbol value))

(defcustom preview-nonready-icon-specs
  '((:type xpm :min 26 :file "prvwrk24.xpm" :ascent 90)
    (:type xpm :min 22 :file "prvwrk20.xpm" :ascent 90)
    (:type xpm :min 17 :file "prvwrk16.xpm" :ascent 90)
    (:type xpm :min 15 :file "prvwrk14.xpm" :ascent 90)
    (:type xpm         :file "prvwrk12.xpm" :ascent 90)
    (:type xbm         :file "prvwrk24.xbm" :ascent 90))
  "The icon used for previews to be generated.
The spec must begin with `:type'.  File names are relative to
`load-path' and `data-directory', a spec `:min' requires a
minimal pixel height for `preview-reference-face' before the spec
will be considered.  Since evaluating the `:file' spec takes
considerable time under XEmacs, it should come after the `:min'
spec to avoid unnecessary evaluation time."
  :group 'preview-appearance
  :type preview-specs-type
  :set #'preview-specs-setter)

(defvar preview-nonready-icon)

(defcustom preview-error-icon-specs
  '((:type xpm :min 22 :file "prverr24.xpm" :ascent 90)
    (:type xpm :min 18 :file "prverr20.xpm" :ascent 90)
    (:type xpm         :file "prverr16.xpm" :ascent 90)
    (:type xbm         :file "prverr24.xbm" :ascent 90))
  "The icon used for PostScript errors.
The spec must begin with `:type'.  File names are relative to
`load-path' and `data-directory', a spec `:min' requires a
minimal pixel height for `preview-reference-face' before the spec
will be considered.  Since evaluating the `:file' spec takes
considerable time under XEmacs, it should come after the `:min'
spec to avoid unnecessary evaluation time."
  :group 'preview-appearance
  :type preview-specs-type
  :set #'preview-specs-setter
)

(defvar preview-error-icon)

(defcustom preview-icon-specs
  '((:type xpm :min 24 :file "prvtex24.xpm" :ascent 75)
    (:type xpm :min 20 :file "prvtex20.xpm" :ascent 75)
    (:type xpm :min 16 :file "prvtex16.xpm" :ascent 75)
    (:type xpm         :file "prvtex12.xpm" :ascent 75)
    (:type xbm :min 24 :file "prvtex24.xbm" :ascent 75)
    (:type xbm :min 16 :file "prvtex16.xbm" :ascent 75)
    (:type xbm         :file "prvtex12.xbm" :ascent 75))
  "The icon used for an open preview.
The spec must begin with `:type'.  File names are relative to
`load-path' and `data-directory', a spec `:min' requires a
minimal pixel height for `preview-reference-face' before the spec
will be considered.  Since evaluating the `:file' spec takes
considerable time under XEmacs, it should come after the `:min'
spec to avoid unnecessary evaluation time."
  :group 'preview-appearance
  :type preview-specs-type
  :set #'preview-specs-setter)

(defvar preview-icon)

(defgroup preview-latex nil "LaTeX options for preview."
  :group 'preview
  :prefix "preview-")

(defcustom preview-image-creators
  '((dvipng
     (open preview-gs-open preview-dvipng-process-setup)
     (place preview-gs-place)
     (close preview-dvipng-close))
    (png (open preview-gs-open)
	 (place preview-gs-place)
	 (close preview-gs-close))
    (jpeg (open preview-gs-open)
	  (place preview-gs-place)
	  (close preview-gs-close))
    (pnm (open preview-gs-open)
	  (place preview-gs-place)
	  (close preview-gs-close))
    (tiff (open preview-gs-open)
	  (place preview-gs-place)
	  (close preview-gs-close)))
  "Define functions for generating images.
These functions get called in the process of generating inline
images of the specified type.  The open function is called
at the start of a rendering pass, the place function for
placing every image, the close function at the end of
the pass.  Look at the documentation of the various
functions used here for the default settings, and at
the function `preview-call-hook' through which those are
called.  Additional argument lists specified in here
are passed to the functions before any additional
arguments given to `preview-call-hook'.

Not all of these image types may be supported by your copy
of Ghostscript, or by your copy of Emacs."
  :group 'preview-gs
  :type '(alist :key-type (symbol :tag "Preview's image type")
		:value-type
		(alist :tag "Handler" :key-type (symbol :tag "Operation:")
		       :value-type (list :tag "Handler"
					 (function :tag "Handler function")
					 (repeat :tag "Additional \
function args" :inline t sexp))
		       :options (open place close))))

(defcustom preview-gs-image-type-alist
  '((png png "-sDEVICE=png16m")
    (dvipng png "-sDEVICE=png16m")
    (jpeg jpeg "-sDEVICE=jpeg")
    (pnm pbm "-sDEVICE=pnmraw")
    (tiff tiff "-sDEVICE=tiff12nc"))
  "*Alist of image types and corresponding Ghostscript options.
The `dvipng' and `postscript' (don't use) entries really specify
a fallback device when images can't be processed by the requested
method, like when PDFTeX was used."
  :group 'preview-gs
  :type '(repeat (list :tag nil (symbol :tag "preview image-type")
		       (symbol :tag "Emacs image-type")
		       (repeat :inline t :tag "Ghostscript options" string))))

(defcustom preview-image-type 'png
  "*Image type to be used in images."
  :group 'preview-gs
  :type (append '(choice)
		(mapcar (lambda (symbol) (list 'const (car symbol)))
			preview-image-creators)
		'((symbol :tag "Other"))))

(defun preview-call-hook (symbol &rest rest)
  "Call a function from `preview-image-creators'.
This looks up SYMBOL in the `preview-image-creators' entry
for the image type `preview-image-type' and calls the
hook function given there with the arguments specified there
followed by REST.  If such a function is specified in there,
that is."
  (let ((hook (cdr (assq symbol
		    (cdr (assq preview-image-type
			       preview-image-creators))))))
    (when hook
      (apply (car hook) (append (cdr hook) rest)))))


(defvar TeX-active-tempdir nil
  "List of directory name, top directory name and reference count.")
(make-variable-buffer-local 'TeX-active-tempdir)

(defcustom preview-bb-filesize 1024
  "Size of file area scanned for bounding box information."
  :group 'preview-gs :type 'integer)

(defcustom preview-preserve-indentation t
  "*Whether to keep additional whitespace at the left of a line."
  :group 'preview-appearance :type 'boolean)

(defun preview-extract-bb (filename)
  "Extract EPS bounding box vector from FILENAME."
  (with-temp-buffer
    (insert-file-contents-literally filename nil 0 preview-bb-filesize
				    t)
    (goto-char (point-min))
    (when (search-forward-regexp "%%BoundingBox:\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)" nil t)
      (vector
       (if preview-preserve-indentation
	   (min 72 (string-to-number (match-string 1)))
	 (string-to-number (match-string 1)))
       (string-to-number (match-string 2))
       (string-to-number (match-string 3))
       (string-to-number (match-string 4))
       ))))

(defcustom preview-prefer-TeX-bb nil
  "*Prefer TeX bounding box to EPS one if available.
If `preview-fast-conversion' is set, this option is not
 consulted since the TeX bounding box has to be used anyway."
  :group 'preview-gs
  :type 'boolean)

(defcustom preview-TeX-bb-border 0.5
  "*Additional space in pt around Bounding Box from TeX."
  :group 'preview-gs
  :type 'number)

(defvar preview-coding-system nil
  "Coding system used for LaTeX process.")
(make-variable-buffer-local 'preview-coding-system)
(defvar preview-parsed-font-size nil
  "Font size as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-font-size)
(defvar preview-parsed-magnification nil
  "Magnification as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-magnification)
(defvar preview-parsed-pdfoutput nil
  "PDFoutput as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-pdfoutput)
(defvar preview-parsed-counters nil
  "Counters as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-counters)
(defvar preview-parsed-tightpage nil
  "Tightpage as parsed from the log of LaTeX run.")
(make-variable-buffer-local 'preview-parsed-tightpage)

(defun preview-get-magnification ()
  "Get magnification from `preview-parsed-magnification'."
  (if preview-parsed-magnification
      (/ preview-parsed-magnification 1000.0) 1.0))

(defun preview-TeX-bb (list)
  "Calculate bounding box from (ht dp wd).
LIST consists of TeX dimensions in sp (1/65536 TeX point)."
  (and
   (consp list)
   (let* ((dims (vconcat (mapcar
			  #'(lambda (x)
			      (/ x 65781.76)) list)))
	  (box
	   (vector
	    (+ 72 (min 0 (aref dims 2)))
	    (+ 720 (min (aref dims 0) (- (aref dims 1)) 0))
	    (+ 72 (max 0 (aref dims 2)))
	    (+ 720 (max (aref dims 0) (- (aref dims 1)) 0))))
	  (border (if preview-parsed-tightpage
		      (vconcat (mapcar
				#'(lambda(x)
				    (/ x 65781.76)) preview-parsed-tightpage))
		    (vector (- preview-TeX-bb-border)
			    (- preview-TeX-bb-border)
			    preview-TeX-bb-border
			    preview-TeX-bb-border))))
     (dotimes (i 4 box)
       (aset box i (+ (aref box i) (aref border i)))))))

(defcustom preview-gs-command
  (or ;; The GS wrapper coming with TeX Live
      (executable-find "rungs")
      ;; The MikTeX builtin GS
      (let ((gs (executable-find "mgs")))
	;; Check if mgs is functional for external non-MikTeX apps.
	;; See http://blog.miktex.org/post/2005/04/07/Starting-mgsexe-at-the-DOS-Prompt.aspx
	(when (and gs (= 0 (shell-command (concat gs " -q -dNODISPLAY -c quit"))))
	  gs))
      ;; Windows ghostscript
      (executable-find "GSWIN32C.EXE")
      ;; standard GhostScript
      (executable-find "gs"))
  "*How to call gs for conversion from EPS.  See also `preview-gs-options'."
  :group 'preview-gs
  :type 'string)

(defcustom preview-gs-options '("-q" "-dDELAYSAFER" "-dNOPAUSE"
				"-DNOPLATFONTS" "-dPrinted"
				"-dTextAlphaBits=4"
				"-dGraphicsAlphaBits=4")
  "*Options with which to call gs for conversion from EPS.
See also `preview-gs-command'."
  :group 'preview-gs
  :type '(repeat string))

(defvar preview-gs-queue nil
  "List of overlays to convert using gs.
Buffer-local to the appropriate TeX process buffer.")
(make-variable-buffer-local 'preview-gs-queue)

(defvar preview-gs-outstanding nil
  "Overlays currently processed.")
(make-variable-buffer-local 'preview-gs-outstanding)

(defcustom preview-gs-outstanding-limit 2
  "*Number of requests allowed to be outstanding.
This is the number of not-yet-completed requests we
might at any time have piped into Ghostscript.  If
this number is larger, the probability of Ghostscript
working continuously is higher when Emacs is rather
busy.  If this number is smaller, redisplay will
follow changes in the displayed buffer area faster."
  :group 'preview-gs
  :type '(restricted-sexp
	  :match-alternatives
	  ((lambda (value) (and
			    (integerp value)
			    (> value 0)
			    (< value 10))))
	  :tag "small number"))

(defvar preview-gs-answer nil
  "Accumulated answer of Ghostscript process.")
(make-variable-buffer-local 'preview-gs-answer)

(defvar preview-gs-image-type nil
  "Image type for gs produced images.")
(make-variable-buffer-local 'preview-gs-image-type)

(defvar preview-gs-sequence nil
  "Pair of sequence numbers for gs produced images.")
(make-variable-buffer-local 'preview-gs-sequence)

(defvar preview-scale nil
  "Screen scale of images.
Magnify by this factor to make images blend with other
screen content.  Buffer-local to rendering buffer.")
(make-variable-buffer-local 'preview-scale)

(defvar preview-colors nil
  "Color setup list.
An array with elements 0, 1 and 2 for background,
foreground and border colors, respectively.  Each element
is a list of 3 real numbers between 0 and 1, or NIL
of nothing special should be done for the color")
(make-variable-buffer-local 'preview-colors)

(defvar preview-gs-init-string nil
  "Ghostscript setup string.")
(make-variable-buffer-local 'preview-gs-init-string)

(defvar preview-ps-file nil
  "PostScript file name for fast conversion.")
(make-variable-buffer-local 'preview-ps-file)

(defvar preview-gs-dsc nil
  "Parsed DSC information.")
(make-variable-buffer-local 'preview-gs-dsc)

(defvar preview-resolution nil
  "Screen resolution where rendering started.
Cons-cell of x and y resolution, given in
dots per inch.  Buffer-local to rendering buffer.")
(make-variable-buffer-local 'preview-resolution)

(defun preview-gs-resolution (scale xres yres)
  "Generate resolution argument for gs.
Calculated from real-life factor SCALE and XRES and
YRES, the screen resolution in dpi."
  (format "-r%gx%g"
	  (/ (* scale xres) (preview-get-magnification))
	  (/ (* scale yres) (preview-get-magnification))))

(defun preview-gs-behead-outstanding (err)
  "Remove leading element of outstanding queue after error.
Return element if non-nil.  ERR is the error string to
show as response of Ghostscript."
  (let ((ov (pop preview-gs-outstanding)))
    (when ov
      (preview-gs-flag-error ov err)
      (overlay-put ov 'queued nil))
    ov))

(defvar preview-gs-command-line nil)
(make-variable-buffer-local 'preview-gs-command-line)
(defvar preview-gs-file nil)
(make-variable-buffer-local 'preview-gs-file)

(defcustom preview-fast-conversion t
  "*Set this for single-file PostScript conversion.
This will have no effect when `preview-image-type' is
set to `postscript'."
  :group 'preview-latex
  :type 'boolean)

(defun preview-string-expand (arg &optional separator)
  "Expand ARG as a string.
It can already be a string.  Or it can be a list, then it is
recursively evaluated using SEPARATOR as separator.  If a list
element is in itself a CONS cell, the CAR of the list (after symbol
dereferencing) can evaluate to either a string, in which case it is
used as a separator for the rest of the list,
or a boolean (t or nil) in which case the rest of the list is
either evaluated and concatenated or ignored, respectively.
ARG can be a symbol, and so can be the CDR
of a cell used for string concatenation."
  (cond
   ((stringp arg) arg)
   ((consp arg)
    (mapconcat
     #'identity
     (delq nil
	   (mapcar
	    (lambda(x)
	      (if (consp x)
		  (let ((sep (car x)))
		    (while (and (symbolp sep)
				(not (memq sep '(t nil))))
		      (setq sep (symbol-value sep)))
		    (if (stringp sep)
			(preview-string-expand (cdr x) sep)
		      (and sep
			   (preview-string-expand (cdr x)))))
		(preview-string-expand x)))
	    arg))
     (or separator "")))
   ((and (symbolp arg) (not (memq arg '(t nil))))
    (preview-string-expand (symbol-value arg) separator))
   (t (error "Bad string expansion"))))

(defconst preview-expandable-string
  ((lambda (f) (funcall f (funcall f 'sexp)))
   (lambda (x)
     `(choice
       string
       (repeat :tag "Concatenate"
	(choice
	 string
	 (cons :tag "Separated list"
	       (choice (string :tag "Separator")
		       (symbol :tag "Indirect separator or flag"))
	       ,x)
	 (symbol :tag "Indirect variable (no separator)")))
       (symbol :tag "Indirect variable (with separator)"))))
  "Type to be used for `preview-string-expand'.
Just a hack until we get to learn how to do this properly.
Recursive definitions are not popular with Emacs,
so we define this type just two levels deep.  This
kind of expandible string can either be just a string, or a
cons cell with a separator string in the CAR, and either
an explicit list of elements in the CDR, or a symbol to
be consulted recursively.")

(defcustom preview-dvipng-command
  "dvipng -picky -noghostscript %d -o \"%m/prev%%03d.png\""
  "*Command used for converting to separate PNG images.

You might specify options for converting to other image types,
but then you'll need to adapt `preview-dvipng-image-type'."
  :group 'preview-latex
  :type 'string)

(defcustom preview-dvipng-image-type
  'png
  "*Image type that dvipng produces.

You'll need to change `preview-dvipng-command' too,
if you customize this."
  :group 'preview-latex
  :type '(choice (const png)
		 (const gif)
		 (symbol :tag "Other" :value png)))

(defcustom preview-dvips-command
  "dvips -Pwww -i -E %d -o %m/preview.000"
  "*Command used for converting to separate EPS images."
  :group 'preview-latex
  :type 'string)

(defcustom preview-fast-dvips-command
  "dvips -Pwww %d -o %m/preview.ps"
  "*Command used for converting to a single PS file."
  :group 'preview-latex
  :type 'string)

(defcustom preview-pdf2dsc-command
  "pdf2dsc %s.pdf %m/preview.dsc"
  "*Command used for generating dsc from a PDF file."
  :group 'preview-latex
  :type 'string)

(defun preview-gs-queue-empty ()
  "Kill off everything remaining in `preview-gs-queue'."
  (mapc #'preview-delete preview-gs-outstanding)
  (dolist (ov preview-gs-queue)
    (if (overlay-get ov 'queued)
	(preview-delete ov)))
  (setq preview-gs-outstanding nil)
  (setq preview-gs-queue nil))

(defvar preview-error-condition nil
  "Last error raised and to be reported.")

(defun preview-log-error (err context &optional process)
  "Log an error message to run buffer.
ERR is the caught error syndrome, CONTEXT is where it
occured, PROCESS is the process for which the run-buffer
is to be used."
  (when (or (null process) (buffer-name (process-buffer process)))
    (with-current-buffer (or (and process
				  (process-buffer process))
			     (current-buffer))
      (save-excursion
	(goto-char (or (and process
			    (process-buffer process)
			    (marker-buffer (process-mark process))
			    (process-mark process))
		       (point-max)))
	(insert-before-markers
	 (format "%s: %s\n"
		 context (error-message-string err)))
	(display-buffer (current-buffer)))))
  (setq preview-error-condition err))

(defun preview-reraise-error (&optional process)
  "Raise an error that has been logged.
Makes sure that PROCESS is removed from the \"Compilation\"
tag in the mode line."
  (when preview-error-condition
    (unwind-protect
	(signal (car preview-error-condition) (cdr preview-error-condition))
      (setq preview-error-condition nil
	    compilation-in-progress (delq process compilation-in-progress)))))

(defun preview-gs-sentinel (process string)
  "Sentinel function for rendering process.
Gets the default PROCESS and STRING arguments
and tries to restart Ghostscript if necessary."
  (condition-case err
      (let ((status (process-status process)))
	(when (memq status '(exit signal))
	  (setq compilation-in-progress (delq process compilation-in-progress)))
	(when (buffer-name (process-buffer process))
	  (with-current-buffer (process-buffer process)
	    (goto-char (point-max))
	    (insert-before-markers "\n" mode-name " " string)
	    (forward-char -1)
	    (insert " at "
		    (substring (current-time-string) 0 -5))
	    (forward-char 1)
	    (TeX-command-mode-line process)
	    (when (memq status '(exit signal))
	      ;; process died.
	      ;;  Throw away culprit, go on.
	      (let* ((err (concat preview-gs-answer "\n"
				  (process-name process) " " string))
		     (ov (preview-gs-behead-outstanding err)))
		(when (and (null ov) preview-gs-queue)
		  (save-excursion
		    (goto-char (if (marker-buffer (process-mark process))
				   (process-mark process)
				 (point-max)))
		    (insert-before-markers err)))
		(delete-process process)
		(if (or (null ov)
			(eq status 'signal))
		    ;; if process was killed explicitly by signal, or if nothing
		    ;; was processed, we give up on the matter altogether.
		    (progn
		      (when preview-ps-file
			(condition-case nil
			    (preview-delete-file preview-ps-file)
			  (file-error nil)))
		      (preview-gs-queue-empty))

		  ;; restart only if we made progress since last call
		  (let (filenames)
		    (dolist (ov preview-gs-outstanding)
		      (setq filenames (overlay-get ov 'filenames))
		      (condition-case nil
			  (preview-delete-file (nth 1 filenames))
			(file-error nil))
		      (setcdr filenames nil)))
		  (setq preview-gs-queue (nconc preview-gs-outstanding
						preview-gs-queue))
		  (setq preview-gs-outstanding nil)
		  (preview-gs-restart)))))))
    (error (preview-log-error err "Ghostscript" process)))
  (preview-reraise-error process))

(defun preview-gs-filter (process string)
  "Filter function for processing Ghostscript output.
Gets the usual PROCESS and STRING parameters, see
`set-process-filter' for a description."
  (with-current-buffer (process-buffer process)
    (setq preview-gs-answer (concat preview-gs-answer string))
    (while (string-match "GS\\(<[0-9]+\\)?>" preview-gs-answer)
      (let* ((pos (match-end 0))
	     (answer (substring preview-gs-answer 0 pos)))
	(setq preview-gs-answer (substring preview-gs-answer pos))
	(condition-case err
	    (preview-gs-transact process answer)
	  (error (preview-log-error err "Ghostscript filter" process))))))
  (preview-reraise-error))

(defun preview-gs-restart ()
  "Start a new Ghostscript conversion process."
  (when preview-gs-queue
    (if preview-gs-sequence
	(setcar preview-gs-sequence (1+ (car preview-gs-sequence)))
      (setq preview-gs-sequence (list 1)))
    (setcdr preview-gs-sequence 1)
    (let* ((process-connection-type nil)
	   (outfile (format "-dOutputFile=%s"
			    (preview-ps-quote-filename
			     (format "%s/pr%d-%%d.%s"
				     (car TeX-active-tempdir)
				     (car preview-gs-sequence)
				     preview-gs-image-type))))
	   (process
	    (apply #'start-process
		   "Preview-Ghostscript"
		   (current-buffer)
		   preview-gs-command
		   outfile
		   preview-gs-command-line)))
      (goto-char (point-max))
      (insert-before-markers "Running `Preview-Ghostscript' with ``"
			     (mapconcat #'shell-quote-argument
					(append
					 (list preview-gs-command
					       outfile)
					 preview-gs-command-line)
					" ") "''\n")
      (setq preview-gs-answer "")
      (process-kill-without-query process)
      (set-process-sentinel process #'preview-gs-sentinel)
      (set-process-filter process #'preview-gs-filter)
      (process-send-string process preview-gs-init-string)
      (setq mode-name "Preview-Ghostscript")
      (push process compilation-in-progress)
      (TeX-command-mode-line process)
      (set-buffer-modified-p (buffer-modified-p))
      process)))

(defun preview-gs-open (&optional setup)
  "Start a Ghostscript conversion pass.
SETUP may contain a parser setup function."
  (let ((image-info (assq preview-image-type preview-gs-image-type-alist)))
    (setq preview-gs-image-type (nth 1 image-info))
    (setq preview-gs-sequence nil)
    (setq preview-gs-command-line (append
				   preview-gs-options
				   (nthcdr 2 image-info))
	  preview-gs-init-string
	  (format "{DELAYSAFER{.setsafe}if}stopped pop\
/.preview-BP currentpagedevice/BeginPage get dup \
null eq{pop{pop}bind}if def\
<</BeginPage{currentpagedevice/PageSize get dup 0 get 1 ne exch 1 get 1 ne or\
{.preview-BP %s}{pop}ifelse}bind/PageSize[1 1]>>setpagedevice\
/preview-do{[count 3 roll save]3 1 roll dup length 0 eq\
{pop}{setpagedevice}{ifelse .runandhide}\
stopped{handleerror quit}if \
aload pop restore}bind def "
		  (preview-gs-color-string preview-colors)))
    (preview-gs-queue-empty)
    (preview-parse-messages (or setup #'preview-gs-dvips-process-setup))))

(defun preview-gs-color-value (value)
  "Return string to be used as color value for an RGB component.
Conversion from Emacs color numbers (0 to 65535) in VALUE
to Ghostscript floats."
  (format "%g" (/ value 65535.0)))

(defun preview-pdf-color-string (colors)
  "Return a string that patches PDF foreground color to work properly."
  ;; Actually, this is rather brutal.  It will only be invoked in
  ;; cases, however, where previously it was not expected that
  ;; anything readable turned up, anyway.
  (let ((fg (aref colors 1)))
    (if fg
	(concat
	 "/GS_PDF_ProcSet GS_PDF_ProcSet dup maxlength dict copy dup begin\
/graphicsbeginpage{//graphicsbeginpage exec "
	 (mapconcat #'preview-gs-color-value fg " ")
	 " 3 copy rg RG}bind store end readonly store "))))

(defun preview-gs-color-string (colors)
  "Return a string setting up colors"
  (let ((bg (aref colors 0))
	(fg (aref colors 1))
	(mask (aref colors 2))
	(border (aref colors 3)))
    (concat
     (and (or (and mask border) (and bg (not fg)))
	  "gsave ")
     (and bg
	 (concat
	  (mapconcat #'preview-gs-color-value bg " ")
	  " setrgbcolor clippath fill "))
     (and mask border
	 (format "%s setrgbcolor false setstrokeadjust %g \
setlinewidth clippath strokepath \
matrix setmatrix true \
{2 index{newpath}if round exch round exch moveto pop false}\
{round exch round exch lineto}{curveto}{closepath}\
pathforall pop fill "
		 (mapconcat #'preview-gs-color-value mask " ")
		 (* 2 border)))
	  ;; I hate antialiasing.  Warp border to integral coordinates.
     (and (or (and mask border) (and bg (not fg)))
	  "grestore ")
     (and fg
	  (concat
	   (mapconcat #'preview-gs-color-value fg " ")
	   " setrgbcolor")))))

(defun preview-dvipng-color-string (colors res)
  "Return color setup tokens for dvipng.
Makes a string of options suitable for passing to dvipng.
Pure borderless black-on-white will return an empty string."
  (let
      ((bg (aref colors 0))
       (fg (aref colors 1))
       (mask (aref colors 2))
       (border (aref colors 3)))
    (concat
     (and bg
	  (format "--bg 'rgb %s' "
		  (mapconcat #'preview-gs-color-value bg " ")))
     (and fg
	  (format "--fg 'rgb %s' "
		  (mapconcat #'preview-gs-color-value fg " ")))
     (and mask border
	  (format "--bd 'rgb %s' "
		  (mapconcat #'preview-gs-color-value mask " ")))
     (and border
	  (format "--bd %d" (max 1 (round (/ (* res border) 72.0))))))))

(defun preview-gs-dvips-process-setup ()
  "Set up Dvips process for conversions via gs."
  (unless (preview-supports-image-type preview-gs-image-type)
    (error "preview-image-type setting '%s unsupported by this Emacs"
	   preview-gs-image-type))
  (setq preview-gs-command-line (append
				 preview-gs-command-line
				 (list (preview-gs-resolution
					(preview-hook-enquiry preview-scale)
					(car preview-resolution)
					(cdr preview-resolution)))))
  (if preview-parsed-pdfoutput
      (preview-pdf2dsc-process-setup)
    (let ((process (preview-start-dvips preview-fast-conversion)))
      (setq TeX-sentinel-function #'preview-gs-dvips-sentinel)
      (list process (current-buffer) TeX-active-tempdir preview-ps-file
	    preview-gs-image-type))))

(defun preview-dvipng-process-setup ()
  "Set up dvipng process for conversion."
  (setq preview-gs-command-line (append
				 preview-gs-command-line
				 (list (preview-gs-resolution
					(preview-hook-enquiry preview-scale)
					(car preview-resolution)
					(cdr preview-resolution)))))
  (if preview-parsed-pdfoutput
      (if (preview-supports-image-type preview-gs-image-type)
	  (preview-pdf2dsc-process-setup)
	(error "preview-image-type setting '%s unsupported by this Emacs"
	       preview-gs-image-type))
    (unless (preview-supports-image-type preview-dvipng-image-type)
      (error "preview-dvipng-image-type setting '%s unsupported by this Emacs"
	     preview-dvipng-image-type))
    (let ((process (preview-start-dvipng)))
      (setq TeX-sentinel-function #'preview-dvipng-sentinel)
      (list process (current-buffer) TeX-active-tempdir t
	  preview-dvipng-image-type))))


(defun preview-pdf2dsc-process-setup ()
  (let ((process (preview-start-pdf2dsc)))
    (setq TeX-sentinel-function #'preview-pdf2dsc-sentinel)
    (list process (current-buffer) TeX-active-tempdir preview-ps-file
	  preview-gs-image-type)))

(defun preview-dvips-abort ()
  "Abort a Dvips run."
  (preview-gs-queue-empty)
  (condition-case nil
      (delete-file
       (let ((gsfile preview-gs-file))
	 (with-current-buffer TeX-command-buffer
	   (funcall (car gsfile) "dvi"))))
    (file-error nil))
  (when preview-ps-file
      (condition-case nil
	  (preview-delete-file preview-ps-file)
	(file-error nil)))
  (setq TeX-sentinel-function nil))

(defalias 'preview-dvipng-abort 'preview-dvips-abort)
;  "Abort a DviPNG run.")

(defun preview-gs-dvips-sentinel (process command &optional gsstart)
  "Sentinel function for indirect rendering DviPS process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply.  Starts gs if GSSTART is set."
  (condition-case err
      (let ((status (process-status process))
	    (gsfile preview-gs-file))
	(cond ((eq status 'exit)
	       (delete-process process)
	       (setq TeX-sentinel-function nil)
	       (condition-case nil
		   (delete-file
		    (with-current-buffer TeX-command-buffer
		      (funcall (car gsfile) "dvi")))
		 (file-error nil))
	       (if preview-ps-file
		   (preview-prepare-fast-conversion))
	       (when gsstart
		 (if preview-gs-queue
		     (preview-gs-restart)
		   (when preview-ps-file
		     (condition-case nil
			 (preview-delete-file preview-ps-file)
		       (file-error nil))))))
	      ((eq status 'signal)
	       (delete-process process)
	       (preview-dvips-abort))))
    (error (preview-log-error err "DviPS sentinel" process)))
  (preview-reraise-error process))

(defun preview-pdf2dsc-sentinel (process command &optional gsstart)
  "Sentinel function for indirect rendering PDF process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply.  Starts gs if GSSTART is set."
  (condition-case err
      (let ((status (process-status process)))
	(cond ((eq status 'exit)
	       (delete-process process)
	       (setq TeX-sentinel-function nil)
	       (setq preview-gs-init-string
		     (concat preview-gs-init-string
			     (preview-pdf-color-string preview-colors)))
	       (preview-prepare-fast-conversion)
	       (when gsstart
		 (if preview-gs-queue
		     (preview-gs-restart)
		   (when preview-ps-file
		     (condition-case nil
			 (preview-delete-file preview-ps-file)
		       (file-error nil))))))
	      ((eq status 'signal)
	       (delete-process process)
	       (preview-dvips-abort))))
    (error (preview-log-error err "PDF2DSC sentinel" process)))
  (preview-reraise-error process))

(defun preview-gs-close (process closedata)
  "Clean up after PROCESS and set up queue accumulated in CLOSEDATA."
  (setq preview-gs-queue (nconc preview-gs-queue closedata))
  (if process
      (if preview-gs-queue
	  (if TeX-process-asynchronous
	      (if (and (eq (process-status process) 'exit)
		       (null TeX-sentinel-function))
		  ;; Process has already finished and run sentinel
		  (progn
		    (when preview-ps-file
		      (condition-case nil
			  (preview-delete-file preview-ps-file)
			(file-error nil)))
		    (preview-gs-restart))
		(setq TeX-sentinel-function
		      `(lambda (process command)
			 (,(if preview-parsed-pdfoutput
			       'preview-pdf2dsc-sentinel
			     'preview-gs-dvips-sentinel)
			  process
			  command
			  t))))
	    (TeX-synchronous-sentinel "Preview-DviPS" (cdr preview-gs-file)
				      process))
    ;; pathological case: no previews although we sure thought so.
	(delete-process process)
	(unless (eq (process-status process) 'signal)
	  (preview-dvips-abort)))))

(defun preview-dvipng-sentinel (process command &optional placeall)
  "Sentinel function for indirect rendering DviPNG process.
The usual PROCESS and COMMAND arguments for
`TeX-sentinel-function' apply.  Places all snippets if PLACEALL is set."
  (condition-case err
      (let ((status (process-status process)))
	(cond ((eq status 'exit)
	       (delete-process process)
	       (setq TeX-sentinel-function nil)
	       (when placeall
		 (preview-dvipng-place-all)))
	      ((eq status 'signal)
	       (delete-process process)
	       (preview-dvipng-abort))))
    (error (preview-log-error err "DviPNG sentinel" process)))
  (preview-reraise-error process))

(defun preview-dvipng-close (process closedata)
  "Clean up after PROCESS and set up queue accumulated in CLOSEDATA."
  (if preview-parsed-pdfoutput
      (preview-gs-close process closedata)
    (setq preview-gs-queue (nconc preview-gs-queue closedata))
    (if process
	(if preview-gs-queue
	    (if TeX-process-asynchronous
		(if (and (eq (process-status process) 'exit)
			 (null TeX-sentinel-function))
		    ;; Process has already finished and run sentinel
		    (preview-dvipng-place-all)
		  (setq TeX-sentinel-function (lambda (process command)
						(preview-dvipng-sentinel
						 process
						 command
						 t))))
	      (TeX-synchronous-sentinel "Preview-DviPNG" (cdr preview-gs-file)
					process))
	  ;; pathological case: no previews although we sure thought so.
	  (delete-process process)
	  (unless (eq (process-status process) 'signal)
	    (preview-dvipng-abort))))))

(defun preview-dsc-parse (file)
  "Parse DSC comments of FILE.
Returns a vector with offset/length pairs corresponding to
the pages.  Page 0 corresponds to the initialization section."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (let ((last-pt (point-min))
	  trailer
	  pagelist
	  lastbegin
	  pt
	  case-fold-search
	  (level 0))
      (while (search-forward-regexp "\
%%\\(?:\\(BeginDocument:\\)\\|\
\\(EndDocument[\n\r]\\)\\|\
\\(Page:\\)\\|\
\\(Trailer[\n\r]\\)\\)" nil t)
	(setq pt (match-beginning 0))
	(cond ((null (memq (char-before pt) '(?\C-j ?\C-m nil))))
	      (trailer (error "Premature %%%%Trailer in `%s' at offsets %d/%d"
			      file trailer pt))
	      ((match-beginning 1)
	       (if (zerop level)
		   (setq lastbegin pt))
	       (setq level (1+ level)))
	      ((match-beginning 2)
	       (if (zerop level)
		   (error "Unmatched %%%%EndDocument in `%s' at offset %d"
			  file pt)
		 (setq level (1- level))))
	      ((> level 0))
	      ((match-beginning 3)
	       (push (list last-pt (- pt last-pt)) pagelist)
	       (setq last-pt pt))
	      ((match-beginning 4)
	       (setq trailer pt))))
      (unless (zerop level)
	(error "Unmatched %%%%BeginDocument in `%s' at offset %d"
	       file lastbegin))
      (push (list last-pt
		  (- (or trailer (point-max)) last-pt)) pagelist)
      (vconcat (nreverse pagelist)))))

(defun preview-gs-dsc-cvx (page dsc)
  "Generate PostScript code accessing PAGE in the DSC object.
The returned PostScript code will need the file on
top of the stack, and will replace it with an executable
object corresponding to the wanted page."
  (let ((curpage (aref dsc page)))
    (format "dup %d setfileposition %d()/SubFileDecode filter cvx"
	    (1- (car curpage)) (nth 1 curpage))))

(defun preview-ps-quote-filename (str &optional nonrel)
  "Make a PostScript string from filename STR.
The file name is first made relative unless
NONREL is not NIL."
  (unless nonrel (setq str (file-relative-name str)))
  (let ((index 0))
    (while (setq index (string-match "[\\()]" str index))
      (setq str (replace-match "\\\\\\&" t nil str)
	    index (+ 2 index)))
    (concat "(" str ")")))

(defun preview-prepare-fast-conversion ()
  "This fixes up all parameters for fast conversion."
  (let* ((file (if (consp (car preview-ps-file))
		   (if (consp (caar preview-ps-file))
		       (car (last (caar preview-ps-file)))
		     (caar preview-ps-file))
		 (car preview-ps-file)))
	 (all-files (if (and (consp (car preview-ps-file))
			     (consp (caar preview-ps-file)))
			(caar preview-ps-file)
		      (list file))))
    (setq preview-gs-dsc (preview-dsc-parse file))
    (setq preview-gs-init-string
	  (concat (format "{<</PermitFileReading[%s]>> setuserparams \
.locksafe} stopped pop "
			  (mapconcat 'preview-ps-quote-filename all-files ""))
		  preview-gs-init-string
		  (format "[%s(r)file]aload exch %s .runandhide aload pop "
			  (preview-ps-quote-filename file)
			  (preview-gs-dsc-cvx 0 preview-gs-dsc))))))

(defun preview-gs-urgentize (ov buff)
  "Make a displayed overlay render with higher priority.
This function is used in fake conditional display properties
for reordering the conversion order to prioritize on-screen
images.  OV is the overlay in question, and BUFF is the
Ghostscript process buffer where the buffer-local queue
is located."
  ;; It does not matter that ov gets queued twice in that process: the
  ;; first version to get rendered will clear the 'queued property.
  ;; It cannot get queued more than twice since we remove the
  ;; conditional display property responsible for requeuing here.
  ;; We don't requeue if the overlay has been killed (its buffer made
  ;; nil).  Not necessary, but while we are checking...
  ;; We must return t.
  (preview-remove-urgentization ov)
  (when (and (overlay-get ov 'queued)
	     (overlay-buffer ov))
    (with-current-buffer buff
      (push ov preview-gs-queue)))
  t)


(defun preview-gs-place (ov snippet box run-buffer tempdir ps-file imagetype)
  "Generate an image placeholder rendered over by Ghostscript.
This enters OV into all proper queues in order to make it render
this image for real later, and returns the overlay after setting
a placeholder image.  SNIPPET gives the number of the
snippet in question for the file to be generated.
BOX is a bounding box if we already know one via TeX.
RUN-BUFFER is the buffer of the TeX process,
TEMPDIR is the correct copy of `TeX-active-tempdir',
PS-FILE is a copy of `preview-ps-file', IMAGETYPE is the image type
for the file extension."
  (overlay-put ov 'filenames
	       (unless (eq ps-file t)
		 (list
		  (preview-make-filename
		   (or ps-file
		       (format "preview.%03d" snippet))
		   tempdir))))
  (overlay-put ov 'queued
	       (vector box nil snippet))
  (overlay-put ov 'preview-image
	       (list (preview-icon-copy preview-nonready-icon)))
  (preview-add-urgentization #'preview-gs-urgentize ov run-buffer)
  (list ov))

(defun preview-mouse-open-error (string)
  "Display STRING in a new view buffer on click."
  (let ((buff (get-buffer-create
	       "*Preview-Ghostscript-Error*")))
    (with-current-buffer buff
      (kill-all-local-variables)
      (set (make-local-variable 'view-exit-action) #'kill-buffer)
      (setq buffer-undo-list t)
      (erase-buffer)
      (insert string)
      (goto-char (point-min)))
    (view-buffer-other-window buff)))

(defun preview-mouse-open-eps (file &optional position)
  "Display eps FILE in a view buffer on click.
Place point at POSITION, else beginning of file."
  (let ((default-major-mode
	  (or
	   (assoc-default "x.ps" auto-mode-alist #'string-match)
	   default-major-mode))
	(buff (get-file-buffer file)))
    (save-excursion
      (if buff
	  (pop-to-buffer buff)
	(view-file-other-window file))
      (goto-char (or position (point-min)))
      (if (eq major-mode 'ps-mode)          ; Bundled with GNU Emacs
	  (message "%s" (substitute-command-keys "\
Try \\[ps-run-start] \\[ps-run-buffer] and \
\\<ps-run-mode-map>\\[ps-run-mouse-goto-error] on error offset." )))
      (if (eq major-mode 'postscript-mode) ; Bundled with XEmacs, limited
	  (message "%s" (substitute-command-keys "\
Try \\[ps-shell] and \\[ps-execute-buffer]."))))))

(defun preview-gs-flag-error (ov err)
  "Make an eps error flag in overlay OV for ERR string."
  (let* ((filenames (overlay-get ov 'filenames))
	 (file (car (nth 0 filenames)))
	 (outfile (format "-dOutputFile=%s"
			  (preview-ps-quote-filename
			   (car (nth 1 filenames)))))
	 (ps-open
	  `(lambda() (interactive "@")
	     (preview-mouse-open-error
	      ,(concat
		(mapconcat #'shell-quote-argument
			    (append (list
				     preview-gs-command
				     outfile)
				    preview-gs-command-line)
			    " ")
		 "\nGS>"
		 preview-gs-init-string
		 (aref (overlay-get ov 'queued) 1)
		 err))))
	 (str
	  (preview-make-clickable
	   nil
	   preview-error-icon
	   "%s views error message
%s more options"
	   ps-open
	   `(lambda() (interactive)
	      (popup-menu
	       '("PostScript error"
		 ["View error" ,ps-open]
		 ["View source"
		  (lambda () (interactive "@")
		    ,(if preview-ps-file
			 `(preview-mouse-open-eps
			   ,(if (consp (car file))
				(nth 1 (car file))
			      (car file))
			   ,(nth 0 (aref preview-gs-dsc
					 (aref (overlay-get ov 'queued) 2))))
		       `(preview-mouse-open-eps ,file)))]))))))
    (overlay-put ov 'strings (cons str str))
    (preview-toggle ov)))

(defun preview-gs-transact (process answer)
  "Work off Ghostscript transaction.
This routine is the action routine called via the process filter.
The Ghostscript process buffer of PROCESS will already be selected, and
and the standard output of Ghostscript up to the next prompt will be
given as ANSWER."
  (let ((ov (pop preview-gs-outstanding))
	(have-error (not
		     (string-match "\\`GS\\(<[0-9]+\\)?>\\'" answer ))))
    (when (and ov (overlay-buffer ov))
      (let ((queued (overlay-get ov 'queued)))
	(when queued
	  (let* ((bbox (aref queued 0))
		 (filenames (overlay-get ov 'filenames))
		 (oldfile (nth 0 filenames))
		 (newfile (nth 1 filenames)))
	    (if have-error
		(preview-gs-flag-error ov answer)
	      (condition-case nil
		  (preview-delete-file oldfile)
		(file-error nil))
	      (overlay-put ov 'filenames (cdr filenames))
	      (preview-replace-active-icon
	       ov
	       (preview-create-icon (car newfile)
				    preview-gs-image-type
				    (preview-ascent-from-bb
				     bbox)
				    (aref preview-colors 2))))
	    (overlay-put ov 'queued nil)))))
    (while (and (< (length preview-gs-outstanding)
		   preview-gs-outstanding-limit)
		(setq ov (pop preview-gs-queue)))
      (let ((queued (overlay-get ov 'queued)))
	(when (and queued
		   (not (memq ov preview-gs-outstanding))
		   (overlay-buffer ov))
	  (let* ((filenames (overlay-get ov 'filenames))
		 (oldfile (car (nth 0
				    (nconc filenames
					   (list
					    (preview-make-filename
					     (format "pr%d-%d.%s"
						     (car preview-gs-sequence)
						     (cdr preview-gs-sequence)
						     preview-gs-image-type)
					     TeX-active-tempdir))))))
		 (bbox (aset queued 0
			     (or (and preview-prefer-TeX-bb
				      (aref queued 0))
				 (and (stringp oldfile)
				      (preview-extract-bb
				       oldfile))
				 (aref queued 0)
				 (error "No bounding box"))))
		 (snippet (aref queued 2))
		 (gs-line
		  (format
		   "%s<<%s>>preview-do\n"
		   (if preview-ps-file
		       (concat "dup "
			       (preview-gs-dsc-cvx
				snippet
				preview-gs-dsc))
		     (format "%s(r)file cvx"
			     (preview-ps-quote-filename
			      (if (listp oldfile)
				  (car (last oldfile))
				oldfile))))
		   (if preview-parsed-tightpage
		       ""
		     (format "/PageSize[%g %g]/PageOffset[%g \
%g[1 1 dtransform exch]{0 ge{neg}if exch}forall]"
			     (- (aref bbox 2) (aref bbox 0))
			     (- (aref bbox 3) (aref bbox 1))
			     (aref bbox 0) (aref bbox 1))))))
	    (setcdr preview-gs-sequence (1+ (cdr preview-gs-sequence)))
	    (setq preview-gs-outstanding
		  (nconc preview-gs-outstanding
			 (list ov)))
	    (aset queued 1 gs-line)
	    ;; ignore errors because of dying processes: they will get
	    ;; caught by the sentinel, anyway.
	    (condition-case nil
		(process-send-string
		 process
		 gs-line)
	      (error nil))))))
    (unless preview-gs-outstanding
      (condition-case nil
	  (process-send-eof process)
	(error nil)))))

(defun preview-hook-enquiry (hook)
  "Gets a value from a configured hook.
HOOK is a list or single item, for which the first resolving to
non-nil counts.  Entries can be a callable function, or
a symbol that is consulted, or a value.  Lists are evaluated
recursively."
  (cond ((functionp hook)
	 (funcall hook))
	((consp hook)
	 (let (res)
	   (while (and (not res) hook)
	     (setq res (preview-hook-enquiry (car hook))
		   hook (cdr hook)))
	   res))
	((and (symbolp hook) (boundp hook))
	 (symbol-value hook))
	(t hook)))

(defcustom preview-scale-function #'preview-scale-from-face
  "*Scale factor for included previews.
This can be either a function to calculate the scale, or
a fixed number."
  :group 'preview-appearance
  :type '(choice (function-item preview-scale-from-face)
		 (const 1.0)
		 (number :value 1.0)
		 (function :value preview-scale-from-face)))

(defcustom preview-default-document-pt 10
  "*Assumed document point size for `preview-scale-from-face'.
If the point size (such as 11pt) of the document cannot be
determined from the document options itself, assume this size.
This is for matching screen font size and previews."
  :group 'preview-appearance
  :type
          '(choice (const :tag "10pt" 10)
                  (const :tag "11pt" 11)
                  (const :tag "12pt" 12)
                  (number :tag "Other" :value 11.0))
)

(defcustom preview-document-pt-list '(preview-parsed-font-size
  preview-auctex-font-size
  preview-default-document-pt)
  "*How `preview-document-pt' figures out the document size."
  :group 'preview-appearance
  :type
  '(repeat (choice
	    ;; This is a bug: type function seems to match variables, too.
	    (restricted-sexp :match-alternatives (functionp)
			     :tag "Function" :value preview-auctex-font-size)
	    (variable :value preview-parsed-font-size)
	    (number :value 11))))

(defun preview-auctex-font-size ()
  "Calculate the default font size of document.
If packages, classes or styles were called with an option
like 10pt, size is taken from the first such option if you
had let your document be parsed by AucTeX."
  (catch 'return (dolist (option (TeX-style-list))
		   (if (string-match "\\`\\([0-9]+\\)pt\\'" option)
		       (throw 'return
			      (string-to-number
			       (match-string 1 option)))))))

(defsubst preview-document-pt ()
  "Calculate the default font size of document."
  (preview-hook-enquiry preview-document-pt-list))

(defun preview-scale-from-face ()
  "Calculate preview scale from `preview-reference-face'.
This calculates the scale of EPS images from a document assumed
to have a default font size given by function `preview-document-pt'
so that they match the reference face in height."
  `(lambda nil
     (/ ,(/ (preview-inherited-face-attribute 'preview-reference-face :height
					      'default) 10.0)
	(preview-document-pt))))

(defvar preview-min-spec)

(defun preview-make-image (symbol)
  "Make an image from a preview spec list.
The first spec that is workable (given the current setting of
`preview-min-spec') from the given symbol is used here.  The
icon is cached in the property list of the symbol."
  (let ((alist (get 'preview-min-alist symbol)))
    (cdr (or
	  (assq preview-min-spec alist)
	  (car (put symbol 'preview-min-alist
		    (cons
		     (cons preview-min-spec
			   (preview-filter-specs
			    (symbol-value symbol)))
		     alist)))))))

(defun preview-filter-specs (spec-list)
  "Find the first of the fitting specs and make an image."
  (let (image)
    (while (and spec-list
		(not (setq image
			   (catch 'preview-filter-specs
			     (preview-filter-specs-1 (car spec-list))))))
      (setq spec-list (cdr spec-list)))
    image))

(defun preview-filter-specs-1 (specs)
  (and specs
       (if (get 'preview-filter-specs (car specs))
	   (apply (get 'preview-filter-specs (car specs)) specs)
	 `(,(nth 0 specs) ,(nth 1 specs)
	   ,@(preview-filter-specs-1 (nthcdr 2 specs))))))

(put 'preview-filter-specs :min
     #'(lambda (keyword value &rest args)
	 (if (> value preview-min-spec)
	     (throw 'preview-filter-specs nil)
	   (preview-filter-specs-1 args))))

(defvar preview-datadir (file-name-directory load-file-name)
  "The directory relative to which package data may be found.
This should be hardwired into the startup file containing the
autoloads for preview-latex.")

(put 'preview-filter-specs :file
     #'(lambda (keyword value &rest args)
	 `(:file ,(expand-file-name value (expand-file-name "images"
							    preview-datadir))
		 ,@(preview-filter-specs-1 args))))

(defvar preview-lispdir TeX-lisp-directory
  "The directory where the preview lisp files are located.")

(defun preview-ascent-from-bb (bb)
  "This calculates the image ascent from its bounding box.
The bounding box BB needs to be a 4-component vector of
numbers (can be float if available)."
  ;; baseline is at 1in from the top of letter paper (11in), so it is
  ;; at 10in from the bottom precisely, which is 720 in PostScript
  ;; coordinates.  If our bounding box has its bottom not above this
  ;; line, and its top above, we can calculate a useful ascent value.
  ;; If not, something is amiss.  We just use 100 in that case.

  (let ((bottom (aref bb 1))
	(top (aref bb 3)))
    (if (and (<= bottom 720)
	     (> top 720))
	(round (* 100.0 (/ (- top 720.0) (- top bottom))))
      100)))

(defface preview-face '((((background dark))
			 (:background "dark slate gray"))
			(t
			 (:background "beige")))
  "Face to use for the preview source."
  :group 'preview-appearance)

(defface preview-reference-face '((t nil))
  "Face consulted for colors and scale of active previews.
Fallback to :inherit and 'default implemented."
  :group 'preview-appearance)

(defcustom preview-auto-reveal
  '(eval (preview-arrived-via (key-binding [left]) (key-binding [right])
			      'backward-char 'forward-char))
  "*Cause previews to open automatically when entered.
Possibilities are:
T autoopens,
NIL doesn't,
a symbol will have its value consulted if it exists,
defaulting to NIL if it doesn't.
An integer will specify a maximum cursor movement distance.
Larger movements won't open the preview.
A CONS-cell means to call a function for determining the value.
The CAR of the cell is the function to call which receives
the CDR of the CONS-cell in the rest of the arguments, while
point and current buffer point to the position in question.
All of the options show reasonable defaults."
  :group 'preview-appearance
  :type '(choice (const :tag "Off" nil)
		 (const :tag "On" t)
		 (symbol :tag "Indirect variable" :value reveal-mode)
		 (integer :tag "Maximum distance" :value 1)
		 (cons :tag "Function call"
		       :value (eval (preview-arrived-via
				     (key-binding [left])
				     (key-binding [right])))
		       function (list :tag "Argument list"
				      (repeat :inline t sexp)))))

(defun preview-auto-reveal-p (mode distance)
  "Decide whether to auto-reveal.
Returns non-NIL if region should be auto-opened.
See `preview-auto-reveal' for definitions of MODE, which gets
set to `preview-auto-reveal'.  DISTANCE specifies the movement
distance with which point has been reached in case it has been
a movement starting in the current buffer."
  (cond ((symbolp mode)
	 (and (boundp mode)
              (symbol-value mode)))
	((integerp mode)
	 (and distance (/= 0 distance) (<= (abs distance) mode)))
	((consp mode)
	 (apply (car mode) (cdr mode)))
	(t mode)))

(defun preview-arrived-via (&rest list)
  "Indicate auto-opening.
Returns non-NIL if called by one of the commands in LIST."
  (memq this-command list))

(defcustom preview-equality-transforms '(identity
					 preview-canonical-spaces)
"Transformation functions for region changes.
These functions are tried in turn on the strings from the
regions of a preview to decide whether a preview is to be considered
changed.  If any transform leads to equal results, the preview is
considered unchanged."
  :group 'preview-appearance
  :type '(repeat function))

(defun preview-relaxed-string= (&rest args)
"Check for functional equality of arguments.
The arguments ARGS are checked for equality by using
`preview-equality-transforms' on them until it is exhausted
or one transform returns equality."
  (let ((lst preview-equality-transforms))
    (while (and lst (not (apply #'string= (mapcar (car lst) args))))
      (setq lst (cdr lst)))
    lst))

(defun preview-canonical-spaces (arg)
  "Convert ARG into canonical form.
Removes comments and collapses white space, except for multiple newlines."
  (let (pos)
    (while (setq pos (string-match "\\s<.*[\n\r][ \t]*" arg pos))
      (setq arg (replace-match "" t t arg 0)))
    (while (setq pos (string-match "[ \t]*\\(\\([ \t]\\)\\|[\n\r][ \t]*\\)"
				   arg pos))
      (setq arg (replace-match (if (match-beginning 2) " " "\n") t t arg 0)
	    pos (1+ pos)))
    (while (setq pos (string-match "\n+" arg pos))
      (if (string= "\n" (match-string 0 arg))
	  (setq arg (replace-match " " t t arg 0)
		pos (1+ pos))
	(setq pos (match-end 0)))))
  arg)

(defun preview-regenerate (ovr)
  "Pass the modified region in OVR again through LaTeX."
  (let ((begin (overlay-start ovr))
	(end (overlay-end ovr)))
    (with-current-buffer (overlay-buffer ovr)
      (preview-delete ovr)
      (preview-region begin end))))

(defcustom preview-inner-environments '("Bmatrix" "Vmatrix" "aligned"
					"array" "bmatrix" "cases"
					"gathered" "matrix" "pmatrix"
					"smallmatrix" "split"
					"subarray" "vmatrix")
  "Environments not to be previewed on their own."
  :group 'preview-latex
  :type '(repeat string))


(defun preview-next-border (backwards)
  "Search for the next interesting border for `preview-at-point'.
Searches backwards if BACKWARDS is non-nil."
  (let (history preview-state (pt (point)))
    (catch 'exit
      (while
	  (null
	   (memq
	    (setq preview-state
		  (if backwards
		      (if (> (setq pt
				   (previous-single-char-property-change
				    pt 'preview-state)) (point-min))
			  (get-char-property (1- pt) 'preview-state)
			(throw 'exit (or history (point-min))))
		    (if (< (setq pt
				 (next-single-char-property-change
				  pt 'preview-state)) (point-max))
			(get-char-property pt 'preview-state)
		      (throw 'exit (or history (point-max))))))
	    '(active inactive)))
	(setq history (and (not preview-state) pt)))
      (or history pt))))

(defun preview-at-point ()
  "Do the appropriate preview thing at point.
If point is positioned on or inside of an unmodified preview area,
its visibility is toggled.

If not, the surroundings are run through preview.  The
surroundings don't extend into unmodified previews or past
contiguous previews invalidated by modifications.

Overriding any other action, if a region is
active (`transient-mark-mode' or `zmacs-regions'), it is run
through `preview-region'."
  (interactive)
  (if (TeX-active-mark)
      (preview-region (region-beginning) (region-end))
    (catch 'exit
      (dolist (ovr (overlays-in (max (point-min) (1- (point)))
				(min (point-max) (1+ (point)))))
	(let ((preview-state (overlay-get ovr 'preview-state)))
	  (when preview-state
	    (unless (eq preview-state 'disabled)
	      (preview-toggle ovr 'toggle (selected-window))
	      (throw 'exit t)))))
      (preview-region (preview-next-border t)
		      (preview-next-border nil)))))

(defun preview-disabled-string (ov)
  "Generate a before-string for disabled preview overlay OV."
  (concat (preview-make-clickable
	   (overlay-get ov 'preview-map)
	   preview-icon
	   "\
%s regenerates preview
%s more options"
	   `(lambda() (interactive) (preview-regenerate ,ov)))
;; icon on separate line only for stuff starting on its own line
	  (with-current-buffer (overlay-buffer ov)
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (overlay-start ov))
		(if (bolp) "\n" ""))))))

(defun preview-disable (ovr)
  "Change overlay behaviour of OVR after source edits."
  (overlay-put ovr 'queued nil)
  (preview-remove-urgentization ovr)
  (overlay-put ovr 'preview-image nil)
  (overlay-put ovr 'timestamp nil)
  (setcdr (overlay-get ovr 'strings) (preview-disabled-string ovr))
  (preview-toggle ovr)
  (overlay-put ovr 'preview-state 'disabled)
  (dolist (filename (overlay-get ovr 'filenames))
    (condition-case nil
	(preview-delete-file filename)
      (file-error nil))
    (overlay-put ovr 'filenames nil)))

(defun preview-delete (ovr &rest ignored)
  "Delete preview overlay OVR, taking any associated file along.
IGNORED arguments are ignored, making this function usable as
a hook in some cases"
  (let ((filenames (overlay-get ovr 'filenames)))
    (overlay-put ovr 'filenames nil)
    (delete-overlay ovr)
    (dolist (filename filenames)
      (condition-case nil
	  (preview-delete-file filename)
	(file-error nil)))))

(defun preview-clearout (&optional start end timestamp)
  "Clear out all previews in the current region.
When called interactively, the current region is used.
Non-interactively, the region between START and END is
affected.  Those two values default to the borders of
the entire buffer.  If TIMESTAMP is non-nil, previews
with a `timestamp' property of it are kept."
  (interactive "r")
  (dolist (ov (overlays-in (or start (point-min))
			   (or end (point-max))))
    (and (overlay-get ov 'preview-state)
	 (not (and timestamp
		   (equal timestamp (overlay-get ov 'timestamp))))
	 (preview-delete ov))))

(defun preview-clearout-buffer (&optional buffer)
  "Clearout BUFFER from previews, current buffer if nil."
  (interactive)
  (if buffer
      (with-current-buffer buffer (preview-clearout))
    (preview-clearout)))

(defun preview-clearout-section ()
  "Clearout previews from LaTeX section."
  (interactive)
  (save-excursion
    (LaTeX-mark-section)
    (preview-clearout (region-beginning) (region-end))))

(defun preview-clearout-at-point ()
  "Clearout any preview at point."
  (interactive)
  (preview-clearout (max (point-min) (1- (point)))
		    (min (point-max) (1+ (point)))))

(defun preview-walk-document (func)
  "Cycle through all buffers belonging to current document.
Each buffer having the same master file as the current file
has FUNC called with its current buffer being set to it."
  (let* ((buffers (buffer-list))
	 (master (expand-file-name (TeX-master-file t)))
	 (default-buffers (list (current-buffer)
				(find-buffer-visiting master))))
    (while buffers
      (with-current-buffer (pop buffers)
	(when
	    (or (memq (current-buffer) default-buffers)
		(and (memq major-mode '(plain-tex-mode latex-mode))
		     (or (stringp TeX-master)
			 (eq TeX-master t))
		     (string= (expand-file-name (TeX-master-file t))
			      master)))
	  (funcall func))))))

(defun preview-clearout-document ()
  "Clear out all previews in current document.
The document consists of all buffers that have the same master file
as the current buffer.  This makes the current document lose
all previews."
  (interactive)
  (preview-walk-document #'preview-clearout-buffer))

(defun preview-kill-buffer-cleanup (&optional buf)
  "This is a cleanup function just for use in hooks.
Cleans BUF or current buffer.  The difference to
`preview-clearout-buffer' is that previews
associated with the last buffer modification time are
kept."
  (with-current-buffer (or buf (current-buffer))
    (save-restriction
      (widen)
      (preview-clearout (point-min) (point-max) (visited-file-modtime)))))

(add-hook 'kill-buffer-hook #'preview-kill-buffer-cleanup)
(add-hook 'before-revert-hook #'preview-kill-buffer-cleanup)

(defvar preview-last-counter)

(defun preview-extract-counters (ctr)
  (setq preview-last-counter
	(prog1 (copy-sequence ctr)
	  (dolist (elt preview-last-counter)
	    (setq ctr (delete elt ctr)))))
  (apply #'concat ctr))

(defun desktop-buffer-preview-misc-data (&rest ignored)
  "Hook function that extracts previews for persistent sessions."
  (unless (buffer-modified-p)
    (setq preview-last-counter nil)
    (save-restriction
      (widen)
      (let (save-info (timestamp (visited-file-modtime)))
	(dolist (ov (sort (overlays-in (point-min) (point-max))
			  (lambda (x y) (< (overlay-start x)
					   (overlay-start y)))))
	  (when (and (memq (overlay-get ov 'preview-state) '(active inactive))
		     (null (overlay-get ov 'queued))
		     (cdr (overlay-get ov 'preview-image)))
	    (push (preview-dissect ov timestamp) save-info)))
	(and save-info
	     (cons 'preview (cons timestamp (nreverse save-info))))))))

(eval-after-load "desktop"
  '(add-hook
    'desktop-buffer-misc-functions
    #'desktop-buffer-preview-misc-data))

(defvar preview-temp-dirs nil
"List of top level temporary directories in use from preview.
Any directory not in this list will be cleared out by preview
on first use.")

(defun preview-dissect (ov timestamp)
  "Extract all persistent data from OV and TIMESTAMP it."
  (let ((filenames (butlast (nth 0 (overlay-get ov 'filenames)))))
    (overlay-put ov 'timestamp timestamp)
    (list (overlay-start ov)
	  (overlay-end ov)
	  (cdr (overlay-get ov 'preview-image))
	  filenames
	  (let ((ctr (overlay-get ov 'preview-counters)))
	    (and ctr
		 (cons (preview-extract-counters (car ctr))
		       (preview-extract-counters (cdr ctr))))))))

(defun preview-buffer-restore-internal (buffer-misc)
  "Restore previews from BUFFER-MISC if proper.
Remove them if they have expired."
  (let ((timestamp (visited-file-modtime)) tempdirlist files)
    (setq preview-parsed-counters nil)
    (when (eq 'preview (pop buffer-misc))
      (preview-get-geometry)
      (if (equal (pop buffer-misc) timestamp)
	  (dolist (ovdata buffer-misc)
	    (setq tempdirlist
		  (apply #'preview-reinstate-preview tempdirlist
			 timestamp ovdata)))
	(dolist (ovdata buffer-misc)
	  (setq files (nth 3 ovdata))
	  (condition-case nil
	      (delete-file (nth 0 files))
	    (file-error nil))
	  (unless (member (nth 1 files) tempdirlist)
	    (push (nth 1 files) tempdirlist)))
	(dolist (dir tempdirlist)
	  (condition-case nil
	      (delete-directory dir)
	    (file-error nil)))))))


(defun preview-buffer-restore (buffer-misc)
  "At end of desktop load, reinstate previews.
This delay is so that minor modes changing buffer positions
\(like `x-symbol-mode' does) will not wreak havoc.
BUFFER-MISC is the appropriate data to be used."
  (add-hook 'desktop-delay-hook `(lambda ()
				   (with-current-buffer ,(current-buffer)
				     (preview-buffer-restore-internal
				      ',buffer-misc)))))

(defun desktop-buffer-preview (desktop-buffer-file-name
			       desktop-buffer-name
			       desktop-buffer-misc)
  "Hook function for restoring persistent previews into a buffer."
  (when (and desktop-buffer-file-name
	     (file-readable-p desktop-buffer-file-name))
    (let ((buf (find-file-noselect desktop-buffer-file-name)))
      (if (eq (car desktop-buffer-misc) 'preview)
	  (with-current-buffer buf
	    (preview-buffer-restore desktop-buffer-misc)
	    buf)
	buf))))

(eval-after-load "desktop"
  '(if (boundp 'desktop-buffer-mode-handlers)
       (add-to-list 'desktop-buffer-mode-handlers
		    '(latex-mode . desktop-buffer-preview))
     (add-hook 'desktop-buffer-handlers '(lambda ()
					   (desktop-buffer-preview
					    desktop-buffer-file-name
					    desktop-buffer-name
					    desktop-buffer-misc)))))

(defcustom preview-auto-cache-preamble 'ask
  "*Whether to generate a preamble cache format automatically.
Possible values are nil, t, and `ask'."
  :group 'preview-latex
  :type '(choice (const :tag "Cache" t)
		 (const :tag "Don't cache" nil)
		 (const :tag "Ask" ask)))

(defvar preview-dumped-alist nil
  "Alist of dumped masters.
The elements are (NAME . ASSOC).  NAME is the master file name
\(without extension), ASSOC is what to do with regard to this
format.  Possible values: NIL means no format is available
and none should be generated.  T means no format is available,
it should be generated on demand.  If the value is a cons cell,
the CAR of the cons cell is the command with which the format
has been generated, and the CDR is some Emacs-flavor specific
value used for maintaining a watch on possible changes of the
preamble.")

(defun preview-cleanout-tempfiles ()
  "Clean out all directories and files with non-persistent data.
This is called as a hook when exiting Emacs."
  (mapc #'preview-kill-buffer-cleanup (buffer-list))
  (mapc #'preview-format-kill preview-dumped-alist))

(defun preview-inactive-string (ov)
  "Generate before-string for an inactive preview overlay OV.
This is for overlays where the source text has been clicked
visible.  For efficiency reasons it is expected that the buffer
is already selected and unnarrowed."
  (concat
   (preview-make-clickable (overlay-get ov 'preview-map)
			   preview-icon
			   "\
%s redisplays preview
%s more options")
;; icon on separate line only for stuff starting on its own line
   (with-current-buffer (overlay-buffer ov)
     (save-excursion
       (save-restriction
	 (widen)
	 (goto-char (overlay-start ov))
	 (if (bolp) "\n" ""))))))

(defun preview-dvipng-place-all ()
  "Place all images dvipng has created, if any.
Deletes the dvi file when finished."
  (let (filename queued oldfiles snippet)
    (dolist (ov (prog1 preview-gs-queue (setq preview-gs-queue nil)))
      (when (and (setq queued (overlay-get ov 'queued))
		 (setq snippet (aref (overlay-get ov 'queued) 2))
		 (setq filename (preview-make-filename
				 (format "prev%03d.%s"
					 snippet preview-dvipng-image-type)
				 TeX-active-tempdir)))
	(if (file-exists-p (car filename))
	    (progn
	      (overlay-put ov 'filenames (list filename))
	      (preview-replace-active-icon
	       ov
	       (preview-create-icon (car filename)
				    preview-dvipng-image-type
				    (preview-ascent-from-bb
				     (aref queued 0))
				    (aref preview-colors 2)))
	      (overlay-put ov 'queued nil))
	  (push filename oldfiles)
	  (overlay-put ov 'filenames nil)
	  (push ov preview-gs-queue))))
    (if (setq preview-gs-queue (nreverse preview-gs-queue))
	(progn
	  (preview-start-dvips preview-fast-conversion)
	  (setq TeX-sentinel-function (lambda (process command)
					(preview-gs-dvips-sentinel
					 process
					 command
					 t)))
	  (dolist (ov preview-gs-queue)
	    (setq snippet (aref (overlay-get ov 'queued) 2))
	    (overlay-put ov 'filenames
			 (list
			  (preview-make-filename
			   (or preview-ps-file
			       (format "preview.%03d" snippet))
			   TeX-active-tempdir))))
	  (while (setq filename (pop oldfiles))
	    (condition-case nil
		(preview-delete-file filename)
	      (file-error nil))))
      (condition-case nil
	  (let ((gsfile preview-gs-file))
	    (delete-file
	     (with-current-buffer TeX-command-buffer
	       (funcall (car gsfile) "dvi"))))
	(file-error nil)))))

(defun preview-active-string (ov)
  "Generate before-string for active image overlay OV."
  (preview-make-clickable
   (overlay-get ov 'preview-map)
   (car (overlay-get ov 'preview-image))
   "%s opens text
%s more options"))

(defun preview-make-filename (file tempdir)
  "Generate a preview filename from FILE and TEMPDIR.
Filenames consist of a CONS-cell with absolute file name as CAR
and TEMPDIR as CDR.  TEMPDIR is a copy of `TeX-active-tempdir'
with the directory name, the reference count and its top directory
name elements.  If FILE is already in that form, the file name itself
gets converted into a CONS-cell with a name and a reference count."
  (if (consp file)
      (progn
	(if (consp (car file))
	    (setcdr (car file) (1+ (cdr (car file))))
	  (setcar file (cons (car file) 1)))
	file)
    (setcar (nthcdr 2 tempdir) (1+ (nth 2 tempdir)))
    (cons (expand-file-name file (nth 0 tempdir))
	  tempdir)))

(defun preview-attach-filename (attached file)
  "Attaches the absolute file name ATTACHED to FILE."
  (if (listp (caar file))
      (setcar (car file) (cons attached (caar file)))
    (setcar (car file) (list attached (caar file))))
  file)

(defun preview-delete-file (file)
  "Delete a preview FILE.
See `preview-make-filename' for a description of the data
structure.  If the containing directory becomes empty,
it gets deleted as well."
  (let ((filename
	 (if (consp (car file))
	     (and (zerop
		   (setcdr (car file) (1- (cdr (car file)))))
		  (car (car file)))
	   (car file))))
    (if filename
	(unwind-protect
	    (if (listp filename)
		(dolist (elt filename) (delete-file elt))
	      (delete-file filename))
	  (let ((tempdir (cdr file)))
	    (when tempdir
	      (if (> (nth 2 tempdir) 1)
		  (setcar (nthcdr 2 tempdir) (1- (nth 2 tempdir)))
		(setcdr file nil)
		(delete-directory (nth 0 tempdir)))))))))

(defvar preview-buffer-has-counters nil)
(make-variable-buffer-local 'preview-buffer-has-counters)

(defun preview-place-preview (snippet start end
				      box counters tempdir place-opts)
  "Generate and place an overlay preview image.
This generates the filename for the preview
snippet SNIPPET in the current buffer, and uses it for the
region between START and END.  BOX is an optional preparsed
TeX bounding BOX passed on to the `place' hook.
COUNTERS is the info about saved counter structures.
TEMPDIR is a copy of `TeX-active-tempdir'.
PLACE-OPTS are additional arguments passed into
`preview-parse-messages'.  Returns
a list with additional info from the placement hook.
Those lists get concatenated together and get passed
to the close hook."
  (preview-clearout start end tempdir)
  (let ((ov (make-overlay start end nil nil nil)))
    (when (fboundp 'TeX-overlay-prioritize)
      (overlay-put ov 'priority (TeX-overlay-prioritize start end)))
    (overlay-put ov 'preview-map
		 (preview-make-clickable
		  nil nil nil
		  `(lambda(event) (interactive "e")
		     (preview-toggle ,ov 'toggle event))
		  `(lambda(event) (interactive "e")
		     (preview-context-menu ,ov event))))
    (overlay-put ov 'timestamp tempdir)
    (when (cdr counters)
      (overlay-put ov 'preview-counters counters)
      (setq preview-buffer-has-counters t))
    (prog1 (apply #'preview-call-hook 'place ov snippet box
		  place-opts)
      (overlay-put ov 'strings
		   (list (preview-active-string ov)))
      (preview-toggle ov t))))

;; The following is a brutal hack.  It relies on `begin' being let to
;; the start of the interesting area when TeX-region-create is being
;; called.

(defun preview-counter-find (begin)
  "Fetch the next preceding or next preview-counters property.
Factored out because of compatibility macros XEmacs would
not use in advice."
  ;; The following two lines are bug workaround for Emacs < 22.1.
  (if (markerp begin)
      (setq begin (marker-position begin)))
  (or (car (get-char-property begin 'preview-counters))
      (cdr (get-char-property (max (point-min)
				   (1- begin))
			      'preview-counters))
      (cdr (get-char-property
	    (max (point-min)
		 (1- (previous-single-char-property-change
		      begin
		      'preview-counters)))
	    'preview-counters))
      (car (get-char-property
	    (next-single-char-property-change begin 'preview-counters)
	    'preview-counters))))

(defadvice TeX-region-create (around preview-counters)
  "Write out counter information to region."
  (let ((TeX-region-extra
	 (concat
	  (and (boundp 'begin)
	       preview-buffer-has-counters
	       (mapconcat
		#'identity
		(cons
		 ""
		 (preview-counter-find (symbol-value 'begin)))
		"\\setcounter"))
	  TeX-region-extra)))
    ad-do-it))

(defun preview-reinstate-preview (tempdirlist timestamp start end
  image filename &optional counters)
  "Reinstate a single preview.
This gets passed TEMPDIRLIST, a list consisting of the kind
of entries used in `TeX-active-tempdir', and TIMESTAMP, the
time stamp under which the file got read in.  It returns an augmented
list.  START and END give the buffer location where the preview
is to be situated, IMAGE the image to place there, and FILENAME
the file to use: a triple consisting of filename, its temp directory
and the corresponding topdir.  COUNTERS is saved counter information,
if any."
  (when
      (or (null filename) (file-readable-p (car filename)))
    (when filename
      (unless (equal (nth 1 filename) (car TeX-active-tempdir))
	(setq TeX-active-tempdir
	      (or (assoc (nth 1 filename) tempdirlist)
		  (car (push (append (cdr filename) (list 0))
			     tempdirlist))))
	(setcar (cdr TeX-active-tempdir)
		(car (or (member (nth 1 TeX-active-tempdir)
				 preview-temp-dirs)
			 (progn
			   (add-hook 'kill-emacs-hook
				     #'preview-cleanout-tempfiles t)
			   (push (nth 1 TeX-active-tempdir)
				 preview-temp-dirs))))))
      (setcar (nthcdr 2 TeX-active-tempdir)
	      (1+ (nth 2 TeX-active-tempdir)))
      (setcdr filename TeX-active-tempdir)
      (setq filename (list filename)))
    (let ((ov (make-overlay start end nil nil nil)))
      (when (fboundp 'TeX-overlay-prioritize)
	(overlay-put ov 'priority (TeX-overlay-prioritize start end)))
      (overlay-put ov 'preview-map
		   (preview-make-clickable
		    nil nil nil
		    `(lambda(event) (interactive "e")
		       (preview-toggle ,ov 'toggle event))
		    `(lambda(event) (interactive "e")
		       (preview-context-menu ,ov event))))
      (when counters
	(overlay-put
	 ov 'preview-counters
	 (cons
	    (mapcar #'cdr
		    (if (string= (car counters) "")
			preview-parsed-counters
		      (setq preview-parsed-counters
			    (preview-parse-counters (car counters)))))
	    (mapcar #'cdr
		    (if (string= (cdr counters) "")
			preview-parsed-counters
		      (setq preview-parsed-counters
			    (preview-parse-counters (cdr counters)))))))
	(setq preview-buffer-has-counters t))
      (overlay-put ov 'filenames filename)
      (overlay-put ov 'preview-image (cons (preview-import-image image)
					   image))
      (overlay-put ov 'strings
		   (list (preview-active-string ov)))
      (overlay-put ov 'timestamp timestamp)
      (preview-toggle ov t)))
  tempdirlist)

(defun preview-back-command (&optional nocomplex)
  "Move backward a TeX token.
If NOCOMPLEX is set, only basic tokens and no argument sequences
will be skipped over backwards."
  (let ((oldpos (point)) oldpoint)
    (condition-case nil
	(or (search-backward-regexp "\\(\\$\\$?\
\\|\\\\[^a-zA-Z@]\
\\|\\\\[a-zA-Z@]+\
\\|\\\\begin[ \t]*{[^}]+}\
\\)\\=" (line-beginning-position) t)
	    nocomplex
	    (if (eq ?\) (char-syntax (char-before)))
		(while
		    (progn
		      (setq oldpoint (point))
		      (backward-sexp)
		      (and (not (eq oldpoint (point)))
			   (eq ?\( (char-syntax (char-after))))))
	      (backward-char)))
      (error (goto-char oldpos)))))

(defcustom preview-required-option-list '("active" "tightpage" "auctex"
					  (preview-preserve-counters
					   "counters"))
  "Specifies required options passed to the preview package.
These are passed regardless of whether there is an explicit
\\usepackage of that package present."
  :group 'preview-latex
  :type preview-expandable-string)

(defcustom preview-preserve-counters nil
  "Try preserving counters for partial runs if set."
  :group 'preview-latex
  :type 'boolean)

(defcustom preview-default-option-list '("displaymath" "floats"
					 "graphics" "textmath" "sections"
					 "footnotes")
  "*Specifies default options to pass to preview package.
These options are only used when the LaTeX document in question does
not itself load the preview package, namely when you use preview
on a document not configured for preview.  \"auctex\", \"active\",
\"dvips\" and \"delayed\" need not be specified here."
  :group 'preview-latex
  :type '(list (set :inline t :tag "Options known to work"
		    :format "%t:\n%v%h" :doc
		    "The above options are all the useful ones
at the time of the release of this package.
You should not need \"Other options\" unless you
upgraded to a fancier version of just the LaTeX style.
Please also note that `psfixbb' fails to have an effect if
`preview-fast-conversion' or `preview-prefer-TeX-bb'
are selected."
		    (const "displaymath")
		    (const "floats")
		    (const "graphics")
		    (const "textmath")
		    (const "sections")
		    (const "footnotes")
		    (const "showlabels")
		    (const "psfixbb"))
	       (set :tag "Expert options" :inline t
		    :format "%t:\n%v%h" :doc
		    "Expert options should not be enabled permanently."
		    (const "noconfig")
		    (const "showbox")
		    (const "tracingall"))
	       (repeat :inline t :tag "Other options" (string))))

(defcustom preview-default-preamble
  '("\\RequirePackage[" ("," . preview-default-option-list)
				      "]{preview}[2004/11/05]")
  "*Specifies default preamble code to add to a LaTeX document.
If the document does not itself load the preview package, that is,
when you use preview on a document not configured for preview, this
list of LaTeX commands is inserted just before \\begin{document}."
  :group 'preview-latex
  :type preview-expandable-string)

(defcustom preview-LaTeX-command '("%`%l \"\\nonstopmode\\nofiles\
\\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
preview-default-preamble "\\fi}\"%' %t")
  "*Command used for starting a preview.
See description of `TeX-command-list' for details."
  :group 'preview-latex
  :type preview-expandable-string)

(defun preview-goto-info-page ()
  "Read documentation for preview-latex in the info system."
  (interactive)
  (info "(preview-latex)"))

(eval-after-load 'info '(add-to-list 'Info-file-list-for-emacs
				     '("preview" . "preview-latex")))

(defvar preview-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-p" #'preview-at-point)
    (define-key map "\C-r" #'preview-region)
    (define-key map "\C-b" #'preview-buffer)
    (define-key map "\C-d" #'preview-document)
    (define-key map "\C-f" #'preview-cache-preamble)
    (define-key map "\C-c\C-f" #'preview-cache-preamble-off)
    (define-key map "\C-i" #'preview-goto-info-page)
    ;;  (define-key map "\C-q" #'preview-paragraph)
    (define-key map "\C-e" #'preview-environment)
    (define-key map "\C-s" #'preview-section)
    (define-key map "\C-w" #'preview-copy-region-as-mml)
    (define-key map "\C-c\C-p" #'preview-clearout-at-point)
    (define-key map "\C-c\C-r" #'preview-clearout)
    (define-key map "\C-c\C-s" #'preview-clearout-section)
    (define-key map "\C-c\C-b" #'preview-clearout-buffer)
    (define-key map "\C-c\C-d" #'preview-clearout-document)
    map))

(defun preview-copy-text (ov)
  "Copy the text of OV into the kill buffer."
  (save-excursion
    (set-buffer (overlay-buffer ov))
    (copy-region-as-kill (overlay-start ov) (overlay-end ov))))

(defun preview-copy-mml (ov)
  "Copy an MML representation of OV into the kill buffer.
This can be used to send inline images in mail and news when
using MML mode."
  (when (catch 'badcolor
	  (let ((str (car (preview-format-mml ov))))
	    (if str
		(if (eq last-command 'kill-region)
		    (kill-append str nil)
		  (kill-new str))
	      (error "No image file available")))
	  nil)
    (let (preview-transparent-border)
      (preview-regenerate ov))))

(defun preview-copy-region-as-mml (start end)
  (interactive "r")
  (when (catch 'badcolor
	  (let (str lst dont-ask)
	    (dolist (ov (overlays-in start end))
	      (when (setq str (preview-format-mml ov dont-ask))
		(setq dont-ask (cdr str))
		(and
		 (>= (overlay-start ov) start)
		 (<= (overlay-end ov) end)
		 (push (list (- (overlay-start ov) start)
			     (- (overlay-end ov) start)
			     (car str)) lst))))
	    (setq str (buffer-substring start end))
	    (dolist (elt (nreverse (sort lst #'car-less-than-car)))
	      (setq str (concat (substring str 0 (nth 0 elt))
				(nth 2 elt)
				(substring str (nth 1 elt)))))
	    (if (eq last-command 'kill-region)
		(kill-append str nil)
	      (kill-new str)))
	  nil)
    (let (preview-transparent-border)
      (preview-region start end))))

(autoload 'mailcap-extension-to-mime "mailcap")

(defun preview-format-mml (ov &optional dont-ask)
  "Return an MML representation of OV as string.
This can be used to send inline images in mail and news when
using MML mode.  If there is nothing current available,
NIL is returned.  If the image has a colored border and the
user wants it removed when asked (unless DONT-ASK is set),
'badcolor is thrown a t.  The MML is returned in the car of the
result, DONT-ASK in the cdr."
  (and (memq (overlay-get ov 'preview-state) '(active inactive))
       (not (overlay-get ov 'queued))
       (let* ((text (with-current-buffer (overlay-buffer ov)
		     (buffer-substring (overlay-start ov)
				       (overlay-end ov))))
	      (image (cdr (overlay-get ov 'preview-image)))
	      file type)
	 (cond ((consp image)
		(and (not dont-ask)
		     (nth 3 image)
		     (if (y-or-n-p "Replace colored borders? ")
			 (throw 'badcolor t)
		       (setq dont-ask t)))
		(setq file (car (car (last (overlay-get ov 'filenames))))
		      type (mailcap-extension-to-mime
			    (file-name-extension file)))
		(cons
		 (format "<#part %s
description=\"%s\"
filename=%s>
<#/part>"
			 (if type
			     (format "type=\"%s\" disposition=inline" type)
			   "disposition=attachment")
			 (if (string-match "[\n\"]" text)
			     "preview-latex image"
			   text)
			 (if (string-match "[ \n<>]" file)
			     (concat "\"" file "\"")
			   file))
		 dont-ask))
	       ((stringp image)
		(cons image dont-ask))))))

(defun preview-active-contents (ov)
  "Check whether we have a valid image associated with OV."
  (and (memq (overlay-get ov 'preview-state) '(active inactive)) t))

(defun preview-context-menu (ov ev)
  "Pop up a menu for OV at position EV."
  (popup-menu
   `("Preview"
     ["Toggle" (preview-toggle ,ov 'toggle ',ev)
      (preview-active-contents ,ov)]
     ["Regenerate" (preview-regenerate ,ov)]
     ["Remove" (preview-delete ,ov)]
     ["Copy text" (preview-copy-text ,ov)]
     ["Copy MIME" (preview-copy-mml ,ov)
      (preview-active-contents ,ov)])
   ev))

(defvar preview-TeX-style-dir)

(defun preview-TeX-style-cooked ()
  "Return `preview-TeX-style-dir' in cooked form.
This will be fine for prepending to a `TEXINPUT' style
environment variable, including an initial `.' at the front."
  (if (or (zerop (length preview-TeX-style-dir))
	  (member (substring preview-TeX-style-dir -1) '(";" ":")))
      preview-TeX-style-dir
    (let ((sep
	   (cond
	    ((stringp TeX-kpathsea-path-delimiter)
	     TeX-kpathsea-path-delimiter)
	    ((string-match
	      "\\`.[:]"
	      (if (file-name-absolute-p preview-TeX-style-dir)
		  preview-TeX-style-dir
		(expand-file-name preview-TeX-style-dir)))
	     ";")
	    (t ":"))))
      (concat "." sep preview-TeX-style-dir sep))))

(defun preview-set-texinputs (&optional remove)
  "Add `preview-TeX-style-dir' into `TEXINPUTS' variables.
With prefix argument REMOVE, remove it again."
  (interactive "P")
  (let ((case-fold-search nil)
	(preview-TeX-style-dir (preview-TeX-style-cooked))
	pattern)
    (if remove
	(progn
	  (setq pattern (concat "\\`\\(TEXINPUTS[^=]*\\)=\\(.*\\)"
				(regexp-quote preview-TeX-style-dir)))
	  (dolist (env (copy-sequence process-environment))
	    (if (string-match pattern env)
		(setenv (match-string 1 env)
			(and (or (< (match-beginning 2) (match-end 2))
				 (< (match-end 0) (length env)))
			     (concat (match-string 2 env)
				     (substring env (match-end 0))))))))
      (setq pattern (regexp-quote preview-TeX-style-dir))
      (dolist (env (cons "TEXINPUTS=" (copy-sequence process-environment)))
	(if (string-match "\\`\\(TEXINPUTS[^=]*\\)=" env)
	    (unless (string-match pattern env)
	      (setenv (match-string 1 env)
		      (concat preview-TeX-style-dir
			      (substring env (match-end 0))))))))))

(defcustom preview-TeX-style-dir nil
  "This variable contains the location of uninstalled TeX styles.
If this is nil, the preview styles are considered to be part of
the installed TeX system.

Otherwise, it can either just specify an absolute directory, or
it can be a complete TEXINPUTS specification.  If it is the
latter, it has to be followed by the character with which
kpathsea separates path components, either `:' on Unix-like
systems, or `;' on Windows-like systems.  And it should be
preceded with .: or .; accordingly in order to have . first in
the search path.

The `TEXINPUT' environment type variables will get this prepended
at load time calling \\[preview-set-texinputs] to reflect this.
You can permanently install the style files using
\\[preview-install-styles].

Don't set this variable other than with customize so that its
changes get properly reflected in the environment."
  :group 'preview-latex
  :set (lambda (var value)
	 (and (boundp var)
	      (symbol-value var)
	      (preview-set-texinputs t))
	 (set var value)
	 (and (symbol-value var)
	      (preview-set-texinputs)))
  :type '(choice (const :tag "Installed" nil)
		 (string :tag "Style directory or TEXINPUTS path")))

;;;###autoload
(defun preview-install-styles (dir &optional force-overwrite
				   force-save)
  "Installs the TeX style files into a permanent location.
This must be in the TeX search path.  If FORCE-OVERWRITE is greater
than 1, files will get overwritten without query, if it is less
than 1 or nil, the operation will fail.  The default of 1 for interactive
use will query.

Similarly FORCE-SAVE can be used for saving
`preview-TeX-style-dir' to record the fact that the uninstalled
files are no longer needed in the search path."
  (interactive "DPermanent location for preview TeX styles
pp")
  (unless preview-TeX-style-dir
    (error "Styles are already installed"))
  (dolist (file (or
		 (condition-case nil
		     (directory-files
		      (progn
			(string-match
			 "\\`\\(\\.[:;]\\)?\\(.*?\\)\\([:;]\\)?\\'"
			 preview-TeX-style-dir)
			(match-string 2 preview-TeX-style-dir))
		      t "\\.\\(sty\\|def\\|cfg\\)\\'")
		   (error nil))
		 (error "Can't find files to install")))
    (copy-file file dir (cond ((eq force-overwrite 1) 1)
			      ((numberp force-overwrite)
			       (> force-overwrite 1))
			      (t force-overwrite))))
  (if (cond ((eq force-save 1)
	     (y-or-n-p "Stop using non-installed styles permanently "))
	    ((numberp force-save)
	     (> force-save 1))
	    (t force-save))
      (customize-save-variable 'preview-TeX-style-dir nil)
    (customize-set-variable 'preview-TeX-style-dir nil)))

;;;###autoload
(defun LaTeX-preview-setup ()
  "Hook function for embedding the preview package into AUCTeX.
This is called by `LaTeX-mode-hook' and changes AUCTeX variables
to add the preview functionality."
  (remove-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'preview-mode-setup)
  (define-key LaTeX-mode-map "\C-c\C-p" preview-map)
  (easy-menu-define preview-menu LaTeX-mode-map
    "This is the menu for preview-latex."
    '("Preview"
      "Generate previews"
      ["(or toggle) at point" preview-at-point]
      ["for environment" preview-environment]
      ["for section" preview-section]
      ["for region" preview-region (preview-mark-active)]
      ["for buffer" preview-buffer]
      ["for document" preview-document]
      "---"
      "Remove previews"
      ["at point" preview-clearout-at-point]
      ["from section" preview-clearout-section]
      ["from region" preview-clearout (preview-mark-active)]
      ["from buffer" preview-clearout-buffer]
      ["from document" preview-clearout-document]
      "---"
      "Turn preamble cache"
      ["on" preview-cache-preamble]
      ["off" preview-cache-preamble-off]
      "---"
      ("Customize"
       ["Browse options"
	(customize-group 'preview)]
       ["Extend this menu"
	(easy-menu-add-item
	 nil '("Preview")
	 (customize-menu-create 'preview))])
      ["Read documentation" preview-goto-info-page]
      ["Report Bug" preview-report-bug]))
  (if (eq major-mode 'latex-mode)
      (preview-mode-setup))
  (if (boundp 'desktop-buffer-misc)
      (preview-buffer-restore desktop-buffer-misc)))

(defun preview-clean-subdir (dir)
  "Cleans out a temporary DIR with preview image files."
  (condition-case err
      (progn
	(mapc #'delete-file
	      (directory-files dir t "\\`pr" t))
	(delete-directory dir))
    (error (message "Deletion of `%s' failed: %s" dir
		    (error-message-string err)))))

(defun preview-clean-topdir (topdir)
  "Cleans out TOPDIR from temporary directories.
This does not erase the directory itself since its permissions
might be needed for colloborative work on common files."
  (mapc #'preview-clean-subdir
	(condition-case nil
	    (directory-files topdir t "\\`tmp" t)
	  (file-error nil))))

(defun preview-create-subdirectory ()
  "Create a temporary subdir for the current TeX process.
If necessary, generates a fitting top
directory or cleans out an existing one (if not yet
visited in this session), then returns the name of
the created subdirectory relative to the master directory,
in shell-quoted form.  `TeX-active-tempdir' is
set to the corresponding TEMPDIR descriptor as described
in `preview-make-filename'.  The directory is registered
in `preview-temp-dirs' in order not to be cleaned out
later while in use."
  (let ((topdir (expand-file-name (TeX-active-master "prv"))))
    (if (file-directory-p topdir)
	(unless (member topdir preview-temp-dirs)
	  ;;  Cleans out the top preview directory by
	  ;;  removing subdirs possibly left from a previous session.
	  (preview-clean-topdir topdir)
	  (push topdir preview-temp-dirs))
      (make-directory topdir)
      (add-to-list 'preview-temp-dirs topdir))
    (add-hook 'kill-emacs-hook #'preview-cleanout-tempfiles t)
    (setq TeX-active-tempdir
	  (list (make-temp-file (expand-file-name
			   "tmp" (file-name-as-directory topdir)) t)
		topdir
		0))
    (shell-quote-argument
     (concat (file-name-as-directory (file-name-nondirectory topdir))
	     (file-name-nondirectory (nth 0 TeX-active-tempdir))))))

;; Hook into TeX immediately if it's loaded, use LaTeX-mode-hook if not.
(if (featurep 'latex)
    (LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup))

;;;###autoload (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)

(defun preview-parse-counters (string)
  "Extract counter information from STRING."
  (let ((list preview-parsed-counters) (pos 0))
    (while (eq pos (string-match " *\\({\\([^{}]+\\)}{[-0-9]+}\\)" string pos))
      (setcdr (or (assoc (match-string 2 string) list)
		  (car (push (list (match-string 2 string)) list)))
	      (match-string 1 string))
      (setq pos (match-end 1)))
    list))

(defun preview-parse-tightpage (string)
  "Build tightpage vector from STRING,"
  (read (concat "[" string "]")))

(defvar preview-parse-variables
  '(("Fontsize" preview-parsed-font-size
     "\\` *\\([0-9.]+\\)pt\\'" 1 string-to-number)
    ("Magnification" preview-parsed-magnification
     "\\` *\\([0-9]+\\)\\'" 1 string-to-number)
    ("PDFoutput" preview-parsed-pdfoutput
     "" 0 stringp)
    ("Counters" preview-parsed-counters
     ".*" 0 preview-parse-counters)
    ("Tightpage" preview-parsed-tightpage
     "\\` *\\(-?[0-9]+ *\\)\\{4\\}\\'" 0 preview-parse-tightpage)))

(defun preview-error-quote (string run-coding-system)
  "Turn STRING with potential ^^ sequences into a regexp.
To preserve sanity, additional ^ prefixes are matched literally,
so the character represented by ^^^ preceding extended characters
will not get matched, usually."
  (let (output case-fold-search)
    (when (featurep 'mule)
      (setq string (encode-coding-string string run-coding-system)))
    (while (string-match "\\^\\{2,\\}\\(\\([@-_?]\\)\\|[8-9a-f][0-9a-f]\\)"
			 string)
      (setq output
	    (concat output
		    (regexp-quote (substring string
					     0
					     (- (match-beginning 1) 2)))
		    (if (match-beginning 2)
			(concat
			 "\\(?:" (regexp-quote
				  (substring string
					     (- (match-beginning 1) 2)
					     (match-end 0)))
			 "\\|"
			 (char-to-string
			  (logxor (aref string (match-beginning 2)) 64))
			 "\\)")
		      (char-to-string
		       (string-to-number (match-string 1 string) 16))))
	    string (substring string (match-end 0))))
    (setq output (concat output (regexp-quote string)))
    (if (featurep 'mule)
	(decode-coding-string output
			      (or (and (boundp 'TeX-japanese-process-output-coding-system)
				       TeX-japanese-process-output-coding-system)
				  buffer-file-coding-system))
      output)))

(defun preview-parse-messages (open-closure)
  "Turn all preview snippets into overlays.
This parses the pseudo error messages from the preview
document style for LaTeX.  OPEN-CLOSURE is called once
it is certain that we have a valid output file, and it has
to return in its CAR the PROCESS parameter for the CLOSE
call, and in its CDR the final stuff for the placement hook."
  (with-temp-message "locating previews..."
    (let (TeX-error-file TeX-error-offset snippet box counters
	  file line
	  (lsnippet 0) lstart (lfile "") lline lbuffer lpoint
	  lcounters
	  string after-string error context-start
	  context offset
	  parsestate (case-fold-search nil)
	  (run-buffer (current-buffer))
	  (run-coding-system preview-coding-system)
	  (run-directory default-directory)
	  tempdir
	  close-data
	  open-data
	  fast-hook
	  slow-hook)
      ;; clear parsing variables
      (dolist (var preview-parse-variables)
	(set (nth 1 var) nil))
      (goto-char (point-min))
      (unwind-protect
	  (progn
	    (while
		(re-search-forward "\
^\\(!\\|\\(.*?\\):[0-9]+:\\) \\|\
\(\\(/*\
\\(?:\\.+[^()\r\n{} /]*\\|[^()\r\n{} ./]+\
\\(?: [^()\r\n{} ./]+\\)*\\(?:\\.[-0-9a-zA-Z_.]*\\)?\\)\
\\(?:/+\\(?:\\.+[^()\r\n{} /]*\\|[^()\r\n{} ./]+\
\\(?: [^()\r\n{} ./]+\\)*\\(?:\\.[-0-9a-zA-Z_.]*\\)?\\)?\\)*\\)\
)*\\(?: \\|\r?$\\)\\|\
\\()+\\)\\|\
 !\\(?:offset(\\([---0-9]+\\))\\|\
name(\\([^)]+\\))\\)\\|\
^Preview: \\([a-zA-Z]+\\) \\([^\n\r]*\\)\r?$" nil t)
;;; Ok, here is a line by line breakdown:
;;; match-alternative 1:
;;; error indicator for TeX error, either style.
;;; match-alternative 2:
;;; The same, but file-line-error-style, matching on file name.
;;; match-alternative 3:
;;; Too ugly to describe in detail.  In short, we try to catch file
;;; names built from path components that don't contain spaces or
;;; other special characters once the file extension has started.
;;;
;;; Position for searching immediately after the file name so as to
;;; not miss closing parens or something.
;;; (match-string 3) is the file name.
;;; match-alternative 4:
;;; )+\( \|$\)
;;; a closing paren followed by the end of line or a space: a just
;;; closed file.
;;; match-alternative 5 (wrapped into one shy group with
;;; match-alternative 6, so that the match on first char is slightly
;;; faster):
;;; !offset(\([---0-9]+\))
;;; an AUCTeX offset message. (match-string 5) is the offset itself
;;; !name(\([^)]+\))
;;; an AUCTeX file name message.  (match-string 6) is the file name
;;; TODO: Actually, the latter two should probably again match only
;;; after a space or newline, since that it what \message produces.
;;;disabled in prauctex.def:
;;;\(?:Ov\|Und\)erfull \\.*[0-9]*--[0-9]*
;;;\(?:.\{79\}
;;;\)*.*$\)\|
;;; This would have caught overfull box messages that consist of
;;; several lines of context all with 79 characters in length except
;;; of the last one.  prauctex.def kills all such messages.
	      (setq file (match-string-no-properties 2))
	      (cond
	       ((match-beginning 1)
		(if (looking-at "\
\\(?:Preview\\|Package Preview Error\\): Snippet \\([---0-9]+\\) \\(started\\|ended\\(\
\\.? *(\\([---0-9]+\\)\\+\\([---0-9]+\\)x\\([---0-9]+\\))\\)?\\)\\.")
		    (progn
		      (when file
			(unless TeX-error-file
			  (push nil TeX-error-file)
			  (push nil TeX-error-offset))
			(unless (car TeX-error-offset)
			  (rplaca TeX-error-file file)))
		      (setq snippet (string-to-number (match-string 1))
			    box (unless
				    (string= (match-string 2) "started")
				  (if (match-string 4)
				      (mapcar #'(lambda (x)
						  (* (preview-get-magnification)
						     (string-to-number x)))
					      (list
					       (match-string 4)
					       (match-string 5)
					       (match-string 6)))
				    t))
			    counters (mapcar #'cdr preview-parsed-counters)
			    error (progn
				    (setq lpoint (point))
				    (end-of-line)
				    (buffer-substring lpoint (point)))

			    ;; And the context for the help window.
			    context-start (point)

			    ;; And the line number to position the cursor.
;;; variant 1: profiling seems to indicate the regexp-heavy solution
;;; to be favorable.  Removing incomplete characters from the error
;;; context is an absolute nuisance.
			    line (and (re-search-forward "\
^l\\.\\([0-9]+\\) \\(\\.\\.\\.\\(?:\\^*\\(?:[89a-f][0-9a-f]\\|[]@-\\_?]\\)\\|\
\[0-9a-f]?\\)\\)?\\([^\n\r]*?\\)\r?
\\([^\n\r]*?\\)\\(\\(?:\\^+[89a-f]?\\)?\\.\\.\\.\\)?\r?$" nil t)
				      (string-to-number (match-string 1)))
			    ;; And a string of the context to search for.
			    string (and line (match-string 3))
			    after-string (and line (buffer-substring
						    (+ (match-beginning 4)
						       (- (match-end 3)
							  (match-beginning 0)))
						    (match-end 4)))

			    ;; And we have now found to the end of the context.
			    context (buffer-substring context-start (point))
			    ;; We may use these in another buffer.
			    offset (or (car TeX-error-offset) 0)
			    file (car TeX-error-file))
		      (when (and (stringp file)
				 (or (string= file "<none>")
				     (TeX-match-extension file)))
			;; if we are the first time round, check for fast hooks:
			(when (null parsestate)
			  (setq open-data
				(save-excursion (funcall open-closure))
				tempdir TeX-active-tempdir)
			  (dolist
			      (lst (if (listp TeX-translate-location-hook)
				       TeX-translate-location-hook
				     (list TeX-translate-location-hook)))
			    (let ((fast
				   (and (symbolp lst)
					(get lst 'TeX-translate-via-list))))
			      (if fast
				  (setq fast-hook
					(nconc fast-hook (list fast)))
				(setq slow-hook
				      (nconc slow-hook (list lst)))))))
			(condition-case err
			    (save-excursion (run-hooks 'slow-hook))
			  (error (preview-log-error err "Translation hook")))
			(push (vector file (+ line offset)
				      string after-string
				      snippet box counters) parsestate)))
		  ;; else normal error message
		  (forward-line)
		  (re-search-forward "^l\\.[0-9]" nil t)
		  (forward-line 2)))
	       ((match-beginning 3)
		;; New file -- Push on stack
		(push (match-string-no-properties 3) TeX-error-file)
		(push nil TeX-error-offset)
		(goto-char (match-end 3)))
	       ((match-beginning 4)
		;; End of file -- Pop from stack
		(when (> (length TeX-error-file) 1)
		  (pop TeX-error-file)
		  (pop TeX-error-offset))
		(goto-char (1+ (match-beginning 0))))
	       ((match-beginning 5)
		;; Hook to change line numbers
		(setq TeX-error-offset
		      (list (string-to-number (match-string 5)))))
	       ((match-beginning 6)
		;; Hook to change file name
		(setq TeX-error-file (list (match-string-no-properties 6))))
	       ((match-beginning 7)
		(let ((var
		       (assoc (match-string-no-properties 7)
			      preview-parse-variables))
		      (offset (- (match-beginning 0) (match-beginning 8)))
		      (str (match-string-no-properties 8)))
		  ;; paste together continuation lines:
		  (while (= (- (length str) offset) 79)
		    (search-forward-regexp "^\\([^\n\r]*\\)\r?$")
		    (setq offset (- (length str))
			  str (concat str (match-string-no-properties 1))))
		  (when (and var
			     (string-match (nth 2 var) str))
		    (set (nth 1 var)
			 (funcall (nth 4 var)
				  (match-string-no-properties
				   (nth 3 var)
				   str))))))))
	    (when (null parsestate)
	      (error "LaTeX found no preview images")))
	(unwind-protect
	    (save-excursion
	      (setq parsestate (nreverse parsestate))
	      (condition-case err
		  (dolist (fun fast-hook)
		    (setq parsestate
			  (save-excursion (funcall fun parsestate))))
		(error (preview-log-error err "Fast translation hook")))
	      (setq snippet 0)
	      (dolist (state parsestate)
		(setq lsnippet snippet
		      file (aref state 0)
		      line (aref state 1)
		      string (aref state 2)
		      after-string (aref state 3)
		      snippet (aref state 4)
		      box (aref state 5)
		      counters (aref state 6))
		(unless (string= lfile file)
		  (set-buffer (if (string= file "<none>")
				  (with-current-buffer run-buffer
				    TeX-command-buffer)
				(find-file-noselect
				 (expand-file-name file run-directory))))
		  (setq lfile file))
		(save-excursion
		  (save-restriction
		    (widen)
		    ;; a fast hook might have positioned us already:
		    (if (number-or-marker-p string)
			(progn
			  (goto-char string)
			  (setq lpoint
				(if (number-or-marker-p after-string)
				    after-string
				  (line-beginning-position))))
		      (if (and (eq (current-buffer) lbuffer)
			       (<= lline line))
			  ;; while Emacs does the perfectly correct
			  ;; thing even when when the line differences
			  ;; get zero or negative, I don't trust this
			  ;; to be universally the case across other
			  ;; implementations.  Besides, if the line
			  ;; number gets smaller again, we are probably
			  ;; rereading the file, and restarting from
			  ;; the beginning will probably be faster.
			  (progn
			    (goto-char lpoint)
			    (if (/= lline line)
				(if (eq selective-display t)
				    (re-search-forward "[\n\C-m]" nil
						       'end
						       (- line lline))
				  (forward-line (- line lline)))))
			(goto-line line))
		      (setq lpoint (point))
		      (cond
		       ((search-forward (concat string after-string)
					(line-end-position) t)
			(backward-char (length after-string)))
		       ;;ok, transform ^^ sequences
		       ((search-forward-regexp
			 (concat "\\("
				 (setq string
				       (preview-error-quote
					string
					run-coding-system))
				 "\\)"
				 (setq after-string
				       (preview-error-quote
					after-string
					run-coding-system)))
			 (line-end-position) t)
			(goto-char (match-end 1)))
		       ((search-forward-regexp
			 (concat "\\("
				 (if (string-match
				      "^[^\0-\177]\\{1,6\\}" string)
				     (setq string
					   (substring string (match-end 0)))
				   string)
				 "\\)"
				 (if (string-match
				      "[^\0-\177]\\{1,6\\}$" after-string)
				     (setq after-string
					   (substring after-string
						      0 (match-beginning 0)))))
			 (line-end-position) t)
			(goto-char (match-end 1)))
		       (t (search-forward-regexp
			   string
			   (line-end-position) t))))
		    (setq lline line
			  lbuffer (current-buffer))
		    (if box
			(progn
			  (if (and lstart (= snippet lsnippet))
			      (setq close-data
				    (nconc
				     (preview-place-preview
				      snippet
				      (save-excursion
					(preview-back-command
					 (= (prog1 (point)
					      (goto-char lstart))
					    lstart))
					(point))
				      (point)
				      (preview-TeX-bb box)
				      (cons lcounters counters)
				      tempdir
				      (cdr open-data))
				     close-data))
			    (with-current-buffer run-buffer
			      (preview-log-error
			       (list 'error
				     (format
				      "End of Preview snippet %d unexpected"
				      snippet)) "Parser")))
			  (setq lstart nil))
		      ;; else-part of if box
		      (setq lstart (point) lcounters counters)
		      ;; >= because snippets in between might have
		      ;; been ignored because of TeX-default-extension
		      (unless (>= snippet (1+ lsnippet))
			(with-current-buffer run-buffer
			  (preview-log-error
			   (list 'error
				 (format
				  "Preview snippet %d out of sequence"
				  snippet)) "Parser"))))))))
	  (preview-call-hook 'close (car open-data) close-data))))))

(defun preview-get-geometry ()
  "Transfer display geometry parameters from current display.
Returns list of scale, resolution and colors.  Calculation
is done in current buffer."
  (condition-case err
      (let* ((geometry
	      (list (preview-hook-enquiry preview-scale-function)
		    (cons (/ (* 25.4 (display-pixel-width))
			     (display-mm-width))
			  (/ (* 25.4 (display-pixel-height))
			     (display-mm-height)))
		    (preview-get-colors)))
	     (preview-min-spec
	      (* (cdr (nth 1 geometry))
		 (/
		  (preview-inherited-face-attribute
		   'preview-reference-face :height 'default)
		  720.0))))
	(setq preview-icon (preview-make-image 'preview-icon-specs)
	      preview-error-icon (preview-make-image
				  'preview-error-icon-specs)
	      preview-nonready-icon (preview-make-image
				     'preview-nonready-icon-specs))
	geometry)
    (error (error "Display geometry unavailable: %s"
		  (error-message-string err)))))

(defun preview-set-geometry (geometry)
  "Set geometry variables from GEOMETRY.
Buffer-local `preview-scale', `preview-resolution',
and `preview-colors' are set as given."
  (setq preview-scale (nth 0 geometry)
	preview-resolution (nth 1 geometry)
	preview-colors (nth 2 geometry)))

(defun preview-start-dvipng ()
  "Start a DviPNG process.."
  (let* ((file preview-gs-file)
	 tempdir
	 (res (/ (* (car preview-resolution)
		    (preview-hook-enquiry preview-scale))
		 (preview-get-magnification)))
	 (resolution  (format " -D%d " res))
	 (colors (preview-dvipng-color-string preview-colors res))
	 (command (with-current-buffer TeX-command-buffer
		    (prog1
			(concat (TeX-command-expand preview-dvipng-command
						    (car file))
				" " colors resolution)
		      (setq tempdir TeX-active-tempdir))))
	 (name "Preview-DviPNG"))
    (setq TeX-active-tempdir tempdir)
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (setq TeX-sentinel-function
	  (lambda (process name) (message "%s: done." name)))
    (if TeX-process-asynchronous
	(let ((process (start-process name (current-buffer) TeX-shell
				      TeX-shell-command-option
				      command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process 'TeX-command-filter)
	  (set-process-sentinel process 'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (push process compilation-in-progress)
	  (sit-for 0)
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil (current-buffer) nil
		    TeX-shell-command-option
		    command))))

(defun preview-start-dvips (&optional fast)
  "Start a DviPS process.
If FAST is set, do a fast conversion."
  (let* ((file preview-gs-file)
	 tempdir
	 (command (with-current-buffer TeX-command-buffer
		    (prog1
			(TeX-command-expand (if fast
						preview-fast-dvips-command
					      preview-dvips-command)
					    (car file))
		      (setq tempdir TeX-active-tempdir))))
	 (name "Preview-DviPS"))
    (setq TeX-active-tempdir tempdir)
    (setq preview-ps-file (and fast
			       (preview-make-filename
				(preview-make-filename
				 "preview.ps" tempdir) tempdir)))
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (setq TeX-sentinel-function
	  (lambda (process name) (message "%s: done." name)))
    (if TeX-process-asynchronous
	(let ((process (start-process name (current-buffer) TeX-shell
				      TeX-shell-command-option
				      command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process 'TeX-command-filter)
	  (set-process-sentinel process 'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (push process compilation-in-progress)
	  (sit-for 0)
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil (current-buffer) nil
		    TeX-shell-command-option
		    command))))

(defun preview-start-pdf2dsc ()
  "Start a PDF2DSC process."
  (let* ((file preview-gs-file)
	 tempdir
	 pdfsource
	 (command (with-current-buffer TeX-command-buffer
		    (prog1
			(TeX-command-expand preview-pdf2dsc-command
					    (car file))
		      (setq tempdir TeX-active-tempdir
			    pdfsource (funcall `,(car file) "pdf")))))
	 (name "Preview-PDF2DSC"))
    (setq TeX-active-tempdir tempdir)
    (setq preview-ps-file (preview-attach-filename
			   pdfsource
			   (preview-make-filename
			    (preview-make-filename
			     "preview.dsc" tempdir) tempdir)))
    (goto-char (point-max))
    (insert-before-markers "Running `" name "' with ``" command "''\n")
    (setq mode-name name)
    (setq TeX-sentinel-function
	  (lambda (process name) (message "%s: done." name)))
    (if TeX-process-asynchronous
	(let ((process (start-process name (current-buffer) TeX-shell
				      TeX-shell-command-option
				      command)))
	  (if TeX-after-start-process-function
	      (funcall TeX-after-start-process-function process))
	  (TeX-command-mode-line process)
	  (set-process-filter process 'TeX-command-filter)
	  (set-process-sentinel process 'TeX-command-sentinel)
	  (set-marker (process-mark process) (point-max))
	  (push process compilation-in-progress)
	  (sit-for 0)
	  process)
      (setq mode-line-process ": run")
      (set-buffer-modified-p (buffer-modified-p))
      (sit-for 0)				; redisplay
      (call-process TeX-shell nil (current-buffer) nil
		    TeX-shell-command-option
		    command))))

(defun preview-TeX-inline-sentinel (process name)
  "Sentinel function for preview.
See `TeX-sentinel-function' and `set-process-sentinel'
for definition of PROCESS and NAME."
  (if process (TeX-format-mode-line process))
  (let ((status (process-status process)))
    (if (memq status '(signal exit))
	(delete-process process))
    (when (eq status 'exit)
      (save-excursion
	(goto-char (point-max))
	(forward-line -1)
	(if (search-forward "abnormally with code 1" nil t)
	    (replace-match "as expected with code 1" t t)
	  (if (search-forward "finished" nil t)
	      (insert " with nothing to show"))))
      (condition-case err
	  (preview-call-hook 'open)
	(error (preview-log-error err "LaTeX" process)))
      (preview-reraise-error process))))

(defcustom preview-format-extensions '(".fmt" ".efmt")
  "Possible extensions for format files.
Those are just needed for cleanup."
  :group 'preview-latex
  :type '(repeat string))

(defun preview-format-kill (format-cons)
  "Kill a cached format.
FORMAT-CONS is intended to be an element of `preview-dumped-alist'.
Tries through `preview-format-extensions'."
  (dolist (ext preview-format-extensions)
    (condition-case nil
	(delete-file (preview-dump-file-name (concat (car format-cons) ext)))
      (file-error nil))))

(defun preview-dump-file-name (file)
  "Make a file name suitable for dumping from FILE."
  (if file
      (concat (file-name-directory file)
	      "prv_"
	      (progn
		(setq file (file-name-nondirectory file))
		(while (string-match " " file)
		  (setq file (replace-match "_" t t file)))
		file))
    "prv_texput"))

(defun preview-do-replacements (string replacements)
  "Perform replacements in string.
STRING is the input string, REPLACEMENTS is a list of replacements.
A replacement is a cons-cell, where the car is the match string,
and the cdr is a list of strings or symbols.  Symbols get dereferenced,
and strings get evaluated as replacement strings."
  (let (rep case-fold-search)
    (while replacements
      (setq rep (pop replacements))
      (cond ((symbolp rep)
	     (setq string (preview-do-replacements
			   string (symbol-value rep))))
	    ((string-match (car rep) string)
	     (setq string
		   (mapconcat (lambda(x)
				(if (symbolp x)
				    (symbol-value x)
				  (replace-match x t nil string)))
			      (cdr rep) ""))))))
  string)

(defconst preview-LaTeX-disable-pdfoutput
  '(("\\`\\(pdf[^ ]*\\)\
\\(\\( [-&]\\([^ \"]\\|\"[^\"]*\"\\)*\\|\
 \"[-&][^\"]*\"\\)*\\)\\(.*\\)\\'"
   . ("\\1\\2 \"\\\\pdfoutput=0 \" \\5")))
  "This replacement places `\"\\pdfoutput=0 \"' after the options
of any command starting with `pdf'.")

(defcustom preview-LaTeX-command-replacements
  nil
  "Replacement for `preview-LaTeX-command'.
This is passed through `preview-do-replacements'."
  :group 'preview-latex
  :type '(repeat
	  (choice
	   (symbol :tag "Named replacement" :value preview-LaTeX-disable-pdfoutput)
	   (cons (string :tag "Matched string")
		 (repeat :tag "Concatenated elements for replacement"
			 (choice (symbol :tag "Variable with literal string")
				 (string :tag "non-literal regexp replacement")))))))

(defvar preview-format-name)

(defcustom preview-dump-replacements
  '(preview-LaTeX-command-replacements
    ("\\`\\([^ ]+\\)\
\\(\\( +-\\([^ \\\\\"]\\|\\\\\\.\\|\"[^\"]*\"\\)*\\)*\\)\\(.*\\)\\'"
     . ("\\1 -ini -interaction=nonstopmode \"&\\1\" " preview-format-name ".ini \\5")))
  "Generate a dump command from the usual preview command."
  :group 'preview-latex
  :type '(repeat
	  (choice (symbol :tag "Named replacement")
		  (cons string (repeat (choice symbol string))))))

(defcustom preview-undump-replacements
  '(("\\`\\([^ ]+\\)\
 .*? \"\\\\input\" \\(.*\\)\\'"
     . ("\\1 -interaction=nonstopmode \"&" preview-format-name "\" \\2")))
  "Use a dumped format for reading preamble."
  :group 'preview-latex
  :type '(repeat
	  (choice (symbol :tag "Named replacement")
		  (cons string (repeat (choice symbol string))))))


(defun preview-cache-preamble (&optional format-cons)
  "Dump a pregenerated format file.
For the rest of the session, this file is used when running
on the same master file.

Returns the process for dumping, nil if there is still a valid
format available.

If FORMAT-CONS is non-nil, a previous format may get reused."
  (interactive)
  (let* ((dump-file
	  (expand-file-name (preview-dump-file-name (TeX-master-file "ini"))))
	 (master (TeX-master-file))
	 (format-name (expand-file-name master))
	 (preview-format-name (shell-quote-argument
			       (preview-dump-file-name (file-name-nondirectory
							master))))
	 (master-file (expand-file-name (TeX-master-file t)))
	 (command (preview-do-replacements
		   (TeX-command-expand
		    (preview-string-expand preview-LaTeX-command)
		    'TeX-master-file)
		   preview-dump-replacements))
	 (preview-auto-cache-preamble nil))
    (unless (and (consp (cdr format-cons))
		 (string= command (cadr format-cons)))
      (unless format-cons
	(setq format-cons (assoc format-name preview-dumped-alist)))
      (if format-cons
	  (preview-cache-preamble-off format-cons)
	(setq format-cons (list format-name))
	(push format-cons preview-dumped-alist))
      ;; mylatex.ltx expects a file name to follow.  Bad. `.tex'
      ;; in the tools bundle is an empty file.
      (write-region "\\ifx\\pdfoutput\\undefined\\else\
\\let\\PREVIEWdump\\dump\\def\\dump{%
\\edef\\next{{\\catcode`\\ 9 \\pdfoutput=\\the\\pdfoutput\\relax\
\\the\\everyjob}}\\everyjob\\next\\catcode`\\ 10 \\let\\dump\\PREVIEWdump\\dump}\\fi\\input mylatex.ltx \\relax\n" nil dump-file)
      (TeX-save-document master)
      (prog1
	  (preview-generate-preview
	   nil (file-name-nondirectory master)
	   command)
	(add-hook 'kill-emacs-hook #'preview-cleanout-tempfiles t)
	(setq TeX-sentinel-function
	      `(lambda (process string)
		 (condition-case err
		     (progn
		       (if (and (eq (process-status process) 'exit)
				(zerop (process-exit-status process)))
			   (preview-watch-preamble
			    ',master-file
			    ',command
			    ',format-cons)
			 (preview-format-kill ',format-cons))
		       (delete-file ',dump-file))
		   (error (preview-log-error err "Dumping" process)))
		 (preview-reraise-error process)))))))

(defun preview-cache-preamble-off (&optional old-format)
  "Clear the pregenerated format file.
The use of the format file is discontinued.
OLD-FORMAT may already contain a format-cons as
stored in `preview-dumped-alist'."
  (interactive)
  (unless old-format
    (setq old-format
	  (let ((master-file (expand-file-name (TeX-master-file))))
	    (or (assoc master-file preview-dumped-alist)
		(car (push (list master-file) preview-dumped-alist))))))
  (preview-unwatch-preamble old-format)
  (preview-format-kill old-format)
  (setcdr old-format nil))

(defun preview-region (begin end)
  "Run preview on region between BEGIN and END."
  (interactive "r")
  (TeX-region-create (TeX-region-file TeX-default-extension)
		     (buffer-substring begin end)
		     (if buffer-file-name
			 (file-name-nondirectory buffer-file-name)
		       "<none>")
		     (save-restriction
		       (widen)
		       (let ((inhibit-point-motion-hooks t)
			     (inhibit-field-text-motion t))
			 (+ (count-lines (point-min) begin)
			    (save-excursion
			      (goto-char begin)
			      (if (bolp) 0 -1))))))
  (preview-generate-preview t (TeX-region-file nil t)
			    (preview-do-replacements
			     (TeX-command-expand
			      (preview-string-expand preview-LaTeX-command)
			      'TeX-region-file)
			     preview-LaTeX-command-replacements)))

(defun preview-buffer ()
  "Run preview on current buffer."
  (interactive)
  (preview-region (point-min) (point-max)))

;; We have a big problem: When we are dumping preambles, diagnostics
;; issued in later runs will not make it to the output when the
;; predumped format skips the preamble.  So we have to place those
;; after \begin{document}.  This we can only do if regions never
;; include the preamble.  We could do this in our own functions, but
;; that would not extend to the operation of C-c C-r g RET.  So we
;; make this preamble skipping business part of TeX-region-create.
;; This will fail if the region is to contain just part of the
;; preamble -- a bad idea anyhow.

(defadvice TeX-region-create (before preview-preamble preactivate activate)
  "Skip preamble for the sake of predumped formats."
  (when (string-match TeX-header-end (ad-get-arg 1))
    (ad-set-arg 1
 		(prog1 (substring (ad-get-arg 1) (match-end 0))
 		  (ad-set-arg 3
			      (with-temp-buffer
				(insert (substring (ad-get-arg 1)
						   0 (match-end 0)))
				(+ (ad-get-arg 3)
				   (count-lines (point-min) (point-max))
				   (if (bolp) 0 -1))))))))

(defun preview-document ()
  "Run preview on master document."
  (interactive)
  (TeX-save-document (TeX-master-file))
  (preview-generate-preview
   nil (TeX-master-file nil t)
   (preview-do-replacements
    (TeX-command-expand
     (preview-string-expand preview-LaTeX-command)
     'TeX-master-file)
    preview-LaTeX-command-replacements)))

(defun preview-environment (count)
  "Run preview on LaTeX environment.
This avoids running environments through preview that are
indicated in `preview-inner-environments'.  If you use a prefix
argument COUNT, the corresponding level of outward nested
environments is selected."
  (interactive "p")
  (save-excursion
    (let (currenv)
      (dotimes (i (1- count))
	(setq currenv (LaTeX-current-environment))
	(if (string= currenv "document")
	    (error "No enclosing outer environment found"))
	(LaTeX-find-matching-begin))
      (while (member (setq currenv (LaTeX-current-environment))
		     preview-inner-environments)
	(LaTeX-find-matching-begin))
      (if (string= currenv "document")
	  (error "No enclosing outer environment found"))
      (preview-region
       (save-excursion (LaTeX-find-matching-begin) (point))
       (save-excursion (LaTeX-find-matching-end) (point))))))

(defun preview-section ()
  "Run preview on LaTeX section." (interactive)
  (save-excursion
    (LaTeX-mark-section)
    (preview-region (region-beginning) (region-end))))


(defun preview-generate-preview (region-p file command)
  "Generate a preview.
REGION-P is the region flag, FILE the file (without default
extension and directory), COMMAND is the command to use.

It returns the started process."
  (setq TeX-current-process-region-p region-p)
  (let* ((geometry (preview-get-geometry))
	 (commandbuff (current-buffer))
	 (pr-file (cons
		   (if TeX-current-process-region-p
		       'TeX-region-file
		     'TeX-master-file)
		   file))
	 (master (TeX-master-file))
	 (master-file (expand-file-name master))
	 (dumped-cons (assoc master-file
			     preview-dumped-alist))
	 process)
    (unless dumped-cons
      (push (setq dumped-cons (cons master-file
				    (if (eq preview-auto-cache-preamble 'ask)
					(y-or-n-p "Cache preamble? ")
				      preview-auto-cache-preamble)))
	    preview-dumped-alist))
    (when (cdr dumped-cons)
      (let* (TeX-current-process-region-p)
	(setq process (preview-cache-preamble dumped-cons))
	(if process
	    (setq TeX-sentinel-function
		  `(lambda (process string)
		     (funcall ,TeX-sentinel-function process string)
		     (TeX-inline-preview-internal
		      ,command ,file
		      ',pr-file ,commandbuff
		      ',dumped-cons
		      ',master
		      ',geometry
		      (buffer-string)))))))
    (or process
	(TeX-inline-preview-internal command file
				     pr-file commandbuff
				     dumped-cons master
				     geometry))))

(defun TeX-inline-preview-internal (command file pr-file
				    commandbuff dumped-cons master
				    geometry
				    &optional str)
  "Internal stuff for previewing.
COMMAND and FILE should be explained in `TeX-command-list'.
PR-FILE is the target file name in the form for `preview-gs-file'.
COMMANDBUFF, DUMPED-CONS, MASTER, and GEOMETRY are
internal parameters, STR may be a log to insert into the current log."
  (set-buffer commandbuff)
  (let*
      ((preview-format-name (shell-quote-argument
			     (preview-dump-file-name
			      (file-name-nondirectory master))))
       (process
	(TeX-run-command
	 "Preview-LaTeX"
	 (if (consp (cdr dumped-cons))
	     (preview-do-replacements
	      command preview-undump-replacements)
	   command) file)))
    (condition-case err
	(progn
	  (when str
	    (save-excursion
	      (goto-char (point-min))
	      (insert str)
	      (when (= (process-mark process) (point-min))
		(set-marker (process-mark process) (point)))))
	  (preview-set-geometry geometry)
	  (setq preview-gs-file pr-file)
	  (setq TeX-sentinel-function 'preview-TeX-inline-sentinel)
	  (when (featurep 'mule)
	    (setq preview-coding-system
		  (or (and (boundp 'TeX-japanese-process-output-coding-system)
			   TeX-japanese-process-output-coding-system)
		      (with-current-buffer commandbuff
			buffer-file-coding-system)))
	    (when preview-coding-system
	      (setq preview-coding-system
		    (preview-buffer-recode-system
		     (coding-system-base preview-coding-system))))
	    (set-process-coding-system
	     process preview-coding-system))
	  (TeX-parse-reset)
	  (setq TeX-parse-function 'TeX-parse-TeX)
	  (if TeX-process-asynchronous
	      process
	    (TeX-synchronous-sentinel "Preview-LaTeX" file process)))
      (error (preview-log-error err "Preview" process)
	     (delete-process process)
	     (preview-reraise-error process)))))

(defconst preview-version AUCTeX-version
  "Preview version.
If not a regular release, the date of the last change.")

(defconst preview-release-date AUCTeX-date
  "Preview release date using the ISO 8601 format, yyyy-mm-dd.")

(defun preview-dump-state (buffer)
  (condition-case nil
      (progn
	(unless (local-variable-p 'TeX-command-buffer (current-buffer))
	  (setq buffer (with-current-buffer buffer (TeX-active-buffer))))
	(when (bufferp buffer)
	  (insert "\nRun buffer contents:\n\n")
	  (if (< (buffer-size buffer) 5000)
	      (insert-buffer-substring buffer)
	    (insert-buffer-substring buffer 1 2500)
	    (insert "...\n\n[...]\n\n\t...")
	    (insert-buffer-substring buffer
				     (- (buffer-size buffer) 2500)
				     (buffer-size buffer)))
	  (insert "\n")))
    (error nil)))

;;;###autoload
(defun preview-report-bug () "Report a bug in the preview-latex package."
  (interactive)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "bug-auctex@gnu.org"
     preview-version
     '(AUCTeX-version
       LaTeX-command-style
       image-types
       preview-image-type
       preview-image-creators
       preview-dvipng-image-type
       preview-dvipng-command
       preview-pdf2dsc-command
       preview-gs-command
       preview-gs-options
       preview-gs-image-type-alist
       preview-fast-conversion
       preview-prefer-TeX-bb
       preview-dvips-command
       preview-fast-dvips-command
       preview-scale-function
       preview-LaTeX-command
       preview-required-option-list
       preview-preserve-counters
       preview-default-option-list
       preview-default-preamble
       preview-LaTeX-command-replacements
       preview-dump-replacements
       preview-undump-replacements
       preview-auto-cache-preamble
       preview-TeX-style-dir)
     `(lambda () (preview-dump-state ,(current-buffer)))
     (lambda ()
       (insert (format "\nOutput from running `%s -h':\n"
		       preview-gs-command))
       (call-process preview-gs-command nil t nil "-h")
       (insert "\n"))
     "Remember to cover the basics.  Including a minimal LaTeX example
file exhibiting the problem might help."
     )))

(eval-when-compile
  (when (boundp 'preview-compatibility-macros)
    (dolist (elt preview-compatibility-macros)
      (if (consp elt)
	  (fset (car elt) (cdr elt))
	(fmakunbound elt)))))

(makunbound 'preview-compatibility-macros)

(provide 'preview)
;;; preview.el ends here
