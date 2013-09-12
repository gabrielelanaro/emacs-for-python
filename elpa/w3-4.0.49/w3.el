;;; w3.el --- Fully customizable, largely undocumented web browser for Emacs

;; Copyright (c) 1996-2001, 2007-2008, 2013 Free Software Foundation, Inc.

;; Author: William Perry and many more
;; Version: 4.0.49
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

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

;; This is a major mode for browsing documents written in Hypertext Markup
;; Language (HTML).  These documents are typicallly part of the World Wide
;; Web (WWW), a project to create a global information net in hypertext
;; format.

;;; Code:

(require 'w3-compat)
(eval-when-compile (require 'cl))
(require 'css)
(require 'url-vars)
(require 'url-parse)
(require 'w3-vars)
(eval-and-compile
  (require 'w3-display))

;; Some mm-* "functions" are macros.  Ensure that they are loaded.
(eval-when-compile
  (require 'mm-decode))

(autoload 'w3-parse-hotlist "w3-hot")
(autoload 'w3-menu-install-menus "w3-menu")


(defun w3-notify-when-ready (buff)
  "Notify the user when BUFF is ready.
See the variable `w3-notify' for the different notification behaviors."
  (if (stringp buff) (setq buff (get-buffer buff)))
  (cond
   ((null buff) nil)
   ((eq w3-notify 'newframe)
    ;; Since we run asynchronously, perhaps while Emacs is waiting for input,
    ;; we must not leave a different buffer current.
    ;; We can't rely on the editor command loop to reselect
    ;; the selected window's buffer.
    (with-current-buffer buff
      (make-frame)))
   ((eq w3-notify 'bully)
    (pop-to-buffer buff)
    (delete-other-windows))
   ((eq w3-notify 'semibully)
    (condition-case nil
	(switch-to-buffer buff)
      (error (message "W3 buffer %s is ready." (buffer-name buff)))))
   ((eq w3-notify 'aggressive)
    (pop-to-buffer buff))
   ((eq w3-notify 'friendly)
    (display-buffer buff 'not-this-window))
   ((eq w3-notify 'polite)
    (beep)
    (message "W3 buffer %s is ready." (buffer-name buff)))
   ((eq w3-notify 'quiet)
    (message "W3 buffer %s is ready." (buffer-name buff)))
   (t (message ""))))

;;;###autoload
(defun w3-open-local (fname)
  "Find a local file, and interpret it as a hypertext document.
Prompt for an existing file or directory, and retrieve it as a
hypertext document."
  (interactive "FLocal file: ")
  (setq fname (expand-file-name fname))
  (w3-do-setup)
  (w3-fetch (concat "file:" fname)))

;;;###autoload
(defun w3-find-file (fname)
  "Find a local file, and interpret it as a hypertext document.
Prompt for an existing file or directory, and retrieve it as a
hypertext document."
  (interactive "FLocal file: ")
  (w3-open-local fname))
 
;;;###autoload
(defun w3-fetch-other-frame (&optional url)
  "Attempt to follow the hypertext reference under point in a new frame."
  (interactive (list (w3-read-url-with-default)))
  (cond
   ((and (fboundp 'make-frame)
	 (fboundp 'select-frame)
	 (not (eq (device-type) 'tty)))
    (let ((frm (make-frame)))
      (select-frame frm)
      (delete-other-windows)
      (w3-fetch url)))
   (t (w3-fetch url))))

(defun w3-fetch-other-window (&optional url)
  "Attempt to follow the hypertext reference under point in a new window."
  (interactive (list (w3-read-url-with-default)))
  (split-window)
  (w3-fetch url))

(defun w3-read-url-with-default ()
  (url-do-setup)
  (let* ((completion-ignore-case t)
	 (default
	   (cond
	    ((eq major-mode 'w3-mode)
	     (or (and current-prefix-arg (w3-view-this-url t))
		 (url-view-url t)))
	    ((url-get-url-at-point)
	     (url-get-url-at-point))
	    (t "http://www.")))
	 (url nil))
    (setq url
	  (completing-read "URL: "  'url-completion-function
			   nil nil default))
    (if (string= url "")
	(setq url (if (eq major-mode 'w3-mode)
		      (if (and current-prefix-arg (w3-view-this-url t))
			  (w3-view-this-url t)
			(url-view-url t))
		    (url-get-url-at-point))))
    url))

(defvar w3-explicit-coding-system nil
  "Coding system to decode documents.

The global value is usually nil.  It will be bound locally if a user
invokes some commands which read a coding system from the user.")

(defun w3-decode-charset (handle)
  "Decode charset-encoded text in the document.
HANDLE is the MIME handle of the original part.
Return the coding system used for the decoding."
  (let* ((encoding (mm-handle-encoding handle))
	 (charset (or w3-explicit-coding-system
		      (mail-content-type-get (mm-handle-type handle)
					     'charset)
		      "iso-8859-1"))
	 (type (mm-handle-media-type handle))
	 (coding-system (mm-charset-to-coding-system charset)))
    (if (or (not coding-system)
	    (eq coding-system 'ascii))
	;; Does this work for XEmacs?  Should we actually guess (which
	;; is what `undecided' involves)?  In Emacs 20 we'll get
	;; byte-combination anyhow when switching to multibyte below,
	;; but we can't leave the buffer as unibyte, or we can't deal
	;; properly with non-ASCII entities inserted by the parsing
	;; stage.
	(setq coding-system 'undecided))
    (save-excursion
      (if encoding
	  (mm-decode-content-transfer-encoding encoding type))
      (when (and (featurep 'mule)
		 (if (boundp 'default-enable-multibyte-characters)
		     default-enable-multibyte-characters
		   t)
		 coding-system)
	(mm-decode-coding-region (point-min) (point-max) coding-system)
	;; Potentially useful is the buffer's going to be saved, and
	;; for the mode-line indication.
	(set-buffer-file-coding-system coding-system)))
    coding-system))

(defvar http-header)			; dynamically bound below

(defun w3-http-equiv-headers (tree)
  "Grovel parse TREE for <meta http-equiv...> elements.
Concatenate the equivalent MIME header onto the dynamically-bound
variable `http-header'."
  (if (consp tree)
      (dolist (node tree)
	(if (consp node)
	    (if (eq 'meta (car-safe node))
		(let ((attrs (cadr node)))
		  (if (assq 'http-equiv attrs)
		      (if (assq 'content attrs)
			  (setq http-header
				(concat 
				 http-header
				 (format "%s: %s\n" 
					 (cdr (assq 'http-equiv attrs))
					 (cdr (assq 'content attrs))))))))
	      (w3-http-equiv-headers (nth 2 node)))))))

(defun w3-nasty-disgusting-http-equiv-handling (buffer url)
  "Propagate information from <meta http-equiv...> elements to MIME headers.
Operate on BUFFER."
  (let (content-type end-of-headers)
    (with-current-buffer buffer
      (goto-char (point-min))
      (mail-narrow-to-head)
      (setq content-type (mail-fetch-field "content-type"))
      (goto-char (point-max))	 ; Make sure we are beyond the headers
      (setq end-of-headers (point))
      (widen)
      (let ((case-fold-search t))
	(if (and content-type (string-match "^text/html" content-type)
		 ;; Try not to parse past the head element.
		 (re-search-forward "</[ \n]*head\\|<[ \n]*body" nil t))
	    (let ((end-of-head (match-beginning 0)))
	      ;; Find any <meta http-equiv> stuff in the head so we
	      ;; can promote it into the MIME headers before
	      ;; mm-dissect-buffer looks at them.
	      (let (http-header)
		(save-restriction
		  (narrow-to-region end-of-headers end-of-head)
		  (goto-char (point-min))
		  ;; Quick check before parsing.
		  (if (search-forward "http-equiv=" nil t)
		      (w3-http-equiv-headers
		       ;; We need to take a copy of the region we're
		       ;; going to parse (which we hope is small) to
		       ;; avoid assumptions about what
		       ;; `w3-parse-buffer' does in the way of
		       ;; widening and munging character references
		       ;; &c.
		       (with-temp-buffer
			 (setq url-current-object (url-generic-parse-url url))
			 (insert-buffer-substring buffer
						  end-of-headers end-of-head)
			 (w3-parse-buffer)))))
		(when http-header
		  (goto-char (point-min))
		  (unless (save-excursion
			    (search-forward ":" (line-end-position) t))
		    (forward-line))
		  (insert http-header)))))))))

(defun w3-setup-reload-timer (url must-be-viewing &optional time)
  "Set up a timer to load URL at optional TIME.
If TIME is unspecified, default to 5 seconds.  Only loads document if
MUST-BE-VIEWING is the current URL when the timer expires."
  (if (or (not time)
	  (<= time 0))
      (setq time 5))
  (let ((func
	 `(lambda ()
	    (if (equal (url-view-url t) ,must-be-viewing)
		(let ((w3-reuse-buffers 'no))
		  (if (equal ,url (url-view-url t))
		      (kill-buffer (current-buffer)))
		  (w3-fetch ,url))))))
    (cond
     ((featurep 'itimer)
      (start-itimer "reloader" func time))
     ((fboundp 'run-at-time)
      (run-at-time time nil func))
     (t
      (w3-warn 'url "Cannot set up timer for automatic reload, sorry!")))))

(defun w3-handle-refresh-header (reload)
  (if (and reload
	   url-honor-refresh-requests
	   (or (eq url-honor-refresh-requests t)
	       (funcall url-confirmation-func "Honor refresh request? ")))
      (let ((uri (url-view-url t)))
	(if (string-match ";" reload)
	    (progn
	      (setq uri (substring reload (match-end 0) nil)
		    reload (substring reload 0 (match-beginning 0)))
	      (if (string-match
		   "ur[li][ \t]*=[ \t]*\"*\\([^ \t\"]+\\)\"*"
		   uri)
		  (setq uri (match-string 1 uri)))
	      (setq uri (url-expand-file-name uri (url-view-url t)))))
	(w3-setup-reload-timer uri (url-view-url t)
			       (string-to-number (or reload "5"))))))

(defun w3-fetch-redirect-callback (&rest args)
  (let (redirect-url)
    ;; Handle both styles of `url-retrieve' callbacks...
    (cond
     ((listp (car args))
      ;; Emacs 22 style.  First argument is a list.
      (let ((status (car args)))
	(when (eq (car status) :error)
	  (setq status (cddr args)))
	(when (eq (car status) :redirect)
	  (setq redirect-url (second (car args))))

	(setq args (cdr args))))

     ((eq (car args) :redirect)
      ;; Pre-22 redirect.
      (setq redirect-url (cadr args))
      (while (eq (car args) :redirect)
	(setq args (cddr args)))))

    ;; w3-fetch-callback can handle errors, too.
    (w3-fetch-callback (or redirect-url (car args)))))

(defun w3-fetch-callback (url)
  (w3-nasty-disgusting-http-equiv-handling (current-buffer) url)
  ;; Process any cookie and refresh headers.
  (let (headers)
    (ignore-errors
      (save-restriction
	(mail-narrow-to-head)
	(goto-char (point-min))
	(unless (save-excursion
		  (search-forward ":" (line-end-position) t))
	  (forward-line))
	(setq headers (mail-header-extract))
	(let (refreshed)
	  (dolist (header headers)
	    ;; Act on multiple cookies if necessary, but only on a
	    ;; single refresh request in case there's more than one.
	    (case (car header)
	      (refresh (unless refreshed
			 (w3-handle-refresh-header (cdr header))
			 (setq refreshed t))))))))
    (let ((handle (mm-dissect-buffer t))
	  (w3-explicit-coding-system
	   (or w3-explicit-coding-system
	       (w3-recall-explicit-coding-system url))))
      (message "Downloading of `%s' complete." url)
      (url-mark-buffer-as-dead (current-buffer))
      (unless headers
	(setq headers (list (cons 'content-type
				  (mm-handle-media-type handle)))))
      ;; Fixme: can handle be null?
      (cond
       ((or (equal (mm-handle-media-type handle) "text/html")
	    ;; Ultimately this should be handled by an XML parser, but
	    ;; this will mostly work for now:
	    (equal (mm-handle-media-type handle) "application/xhtml+xml"))
	;; Special case text/html if it comes through w3-fetch
	(set-buffer (generate-new-buffer " *w3-html*"))
	(mm-insert-part handle)
	(w3-decode-charset handle)
	(setq url-current-object (url-generic-parse-url url))
	(w3-prepare-buffer)
	(setq url-current-mime-headers headers)
	(w3-notify-when-ready (current-buffer))
	(mm-destroy-parts handle))
	;;        ((equal (mm-handle-media-type handle) "text/xml")
	;; 	;; Special case text/xml if it comes through w3-fetch
	;; 	(set-buffer (generate-new-buffer " *w3-xml*"))
	;; 	(mm-disable-multibyte)
	;; 	(mm-insert-part handle)
	;; 	(w3-decode-charset handle)
	;;      !!! Need some function to view XML nicely... maybe the
	;;      !!! customize tree control?
	;; 	(setq url-current-object (url-generic-parse-url url)
	;; 	      url-current-mime-headers headers)
	;; 	(mm-destroy-parts handle)
	;; 	(w3-notify-when-ready (current-buffer)))
       ((equal (car-safe (mm-handle-type handle))
	       "application/x-elisp-parsed-html")
	;; Also need to special-case pre-parsed representations of HTML.
	;; Fixme: will this need decoding?
	(w3-prepare-tree (read (set-marker (make-marker) 1
					   (mm-handle-buffer handle)))))
       ((mm-inlinable-p handle)
	;; We can view it inline!
	(set-buffer (generate-new-buffer url))
	(require 'mm-view)	       ; make sure methods are defined
	(mm-display-part handle)
	(set-buffer-modified-p nil)
	(w3-mode)
	(if (equal "image" (mm-handle-media-supertype handle))
	    (setq cursor-type nil))
	(setq url-current-mime-headers headers)
	(w3-notify-when-ready (current-buffer)))
       (t
	;; Must be an external viewer
	(mm-display-part handle)
	;;(mm-destroy-parts handle)
	)))))

;;;###autoload
(defun w3-fetch (&optional url target)
  "Retrieve a document over the World Wide Web.
Defaults to URL of the current document, if any.
With prefix argument, use the URL of the hyperlink under point instead."
  (interactive (list (w3-read-url-with-default)))
  (w3-do-setup)
  (if (and (boundp 'command-line-args-left)
	   command-line-args-left
	   (string-match url-nonrelative-link (car command-line-args-left)))
      (setq url (car command-line-args-left)
	    command-line-args-left (cdr command-line-args-left)))
  (if (or (null url) (equal url "")) (error "No document specified!"))

  ;; In the common case, this is probably cheaper than searching.
  (while (= (string-to-char url) ? )
    (setq url (substring url 1)))

  (or target (setq target w3-base-target))
  (if (stringp target)
      (setq target (intern (downcase target))))
  (and target
       (let ((window-distance (cdr-safe (assq target
					      w3-target-window-distances))))
	 (if (numberp window-distance)
	     (other-window window-distance)
	   (case target
	     ((_blank external)
	      (w3-fetch-other-frame url))
	     (_top
	      (delete-other-windows))
	     (otherwise
	      (message "target %S not found." target))))))

  (cond
   ((= (string-to-char url) ?#)
    (w3-relative-link url))
   ((and (interactive-p) current-prefix-arg)
    (w3-download-url url))
   (t
    (let ((w3-current-buffer (current-buffer))
	  (buf (w3-buffer-visiting url)))
      (if (or (not buf)
	      (cond
	       ((not (equal (downcase (or url-request-method "GET")) "get"))
		t)
	       ((memq w3-reuse-buffers '(no never reload)) t)
	       ((memq w3-reuse-buffers '(yes reuse always)) nil)
	       (t
		(when (and w3-reuse-buffers (not (eq w3-reuse-buffers 'ask)))
		  (ding)
		  (message
		   "Warning: Invalid value for variable w3-reuse-buffers: %s"
		   (prin1-to-string w3-reuse-buffers))
		  (sit-for 2))
		(not (funcall url-confirmation-func
			      (format "Reuse URL in buffer %s? "
				      (buffer-name buf)))))))
	  (url-retrieve url 'w3-fetch-redirect-callback (list url))
	(w3-notify-when-ready buf))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History for forward/back buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-history-stack nil
  "History stack viewing history.
This is an assoc list, with the oldest items first.
Each element is a cons cell of (url . timeobj), where URL
is the normalized URL (default ports removed, etc), and TIMEOBJ is
a standard Emacs time.  See the `current-time' function documentation
for information on this format.")

(defun w3-history-find-url-internal (url)
  "Search in the history list for URL.
Returns a cons cell, where the car is the 'back' node, and
the cdr is the 'next' node."
  (let* ((node (assoc url w3-history-stack))
	 (next (cadr (memq node w3-history-stack)))
	 (last nil)
	 (temp nil)
	 (todo w3-history-stack))
    ;; Last node is a little harder to find without using back links
    (while (and (not last) todo)
      (if (string= (caar todo) url)
	  (setq last (or temp 'none))
	(setq temp (pop todo))))
    (cons (if (not (symbolp last)) last)
	  next)))

(defun w3-history-forward ()
  "Go forward in the history from this page."
  (interactive)
  (let ((next (cadr (w3-history-find-url-internal (url-view-url t))))
	(w3-reuse-buffers 'yes))
    (if next
	(w3-fetch next))))

(defun w3-history-backward ()
  "Go backward in the history from this page."
  (interactive)
  (let ((last (caar (w3-history-find-url-internal (url-view-url t))))
	(w3-reuse-buffers 'yes))
    (if last
	(w3-fetch last))))

(defun w3-history-push (referer url)
  "REFERER is the url we followed this link from.  URL is the link we got to."
  (if (not referer)
      (setq w3-history-stack (list (cons url (current-time))))
    (let ((node (memq (assoc referer w3-history-stack) w3-history-stack)))
      (if node
	  (setcdr node (list (cons url (current-time))))
	(setq w3-history-stack (append w3-history-stack
				       (list
					(cons url (current-time)))))))))

(defalias 'w3-add-urls-to-history 'w3-history-push)
(defalias 'w3-backward-in-history 'w3-history-backward)
(defalias 'w3-forward-in-history 'w3-history-forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-describe-entities ()
  "Show an DTD fragment listing all the entities currently defined."
  (interactive)
  (switch-to-buffer (get-buffer-create "W3 Entities"))
  (let ((buffer-file-name (concat (make-temp-name "entities") ".dtd")))
    (set-auto-mode))
  (erase-buffer)
  (let (entity)
    (mapatoms
     (lambda (x)
       (setq entity (get x 'html-entity-expansion))
       (if entity
	   (insert (format "<!entity %s %s \"%s\">\n" x (car entity)
			   (cdr entity)))))))
  (goto-char (point-min)))

(defun w3-document-information (&optional buff)
  "Display information on the document in buffer BUFF."
  (interactive)
  (if (interactive-p)
      (let ((w3-notify 'friendly))
	(if (get-buffer "Document Information")
	    (kill-buffer (get-buffer "Document Information")))
	(w3-fetch "about:document"))
    (setq buff (or buff (current-buffer)))
    (with-current-buffer buff
      (let* ((url (url-view-url t))
	     (title (buffer-name))
	     (case-fold-search t)
	     (attributes (url-file-attributes url))
	     (lastmod (or (cdr-safe (assq 'last-modified
					  url-current-mime-headers))
			  (nth 5 attributes)))
	     (hdrs url-current-mime-headers)
	     (size (cdr (assq 'content-length url-current-mime-headers)))
	     (info w3-current-metainfo)
	     (links w3-current-links))
	(set-buffer (get-buffer-create url-working-buffer))
	;; (setq url-current-can-be-cached nil)
	(erase-buffer)
	(if (consp lastmod)
	    (if (equal '(0 . 0) lastmod)
		(setq lastmod nil)
	      (setq lastmod (current-time-string lastmod))))
	;; (setq url-current-mime-type "text/html")
	(insert "\
Content-Type: text/html\n
<html>
 <head>
  <title>Document Information</title>
 </head>
 <body
  <table border>
   <tr><th colspan=2>Document Information</th></tr>
   <tr><td>Title:</td><td>" title "</td></tr>
   <tr><td>Location:</td><td>" url "</td></tr>")
	(if size (insert "\
   <tr><td>Size:</td><td>" (url-pretty-length (if (stringp size)
						  (string-to-number size)
						size)) "</td></tr>"))
	(insert "\
   <tr><td>Last Modified:</td><td>" (or lastmod "None Given") "</td></tr>\n")
	(when hdrs
	  (setq hdrs (delete (assq 'last-modified hdrs) hdrs))
	  (setq hdrs (delete (assq 'content-length hdrs) hdrs))
	  (setq hdrs (mapcar (lambda (pair)
			       (cons (symbol-name (car pair))
				     (cdr pair)))
			     hdrs)))
	(let* ((maxlength (car (sort (mapcar (lambda (x)
					       (length (car x)))
					     hdrs)
				     '>)))
	       (fmtstring (format "\
  <tr><td align=right>%%%ds:</td><td>%%s</td></tr>" maxlength)))
	  (insert
	   "\
  <tr><th colspan=2>MetaInformation</th></tr>\n"
	   (mapconcat
	    (lambda (x)
	      (if (/= (length (car x)) 0)
		  (format fmtstring
			  (url-insert-entities-in-string
			   (capitalize (car x)))
			  (url-insert-entities-in-string
			   (if (numberp (cdr x))
			       (int-to-string (cdr x))
			     (cdr x))))))
	    (sort hdrs
		  (lambda (x y) (string-lessp (car x) (car y))))
	    "\n")))
	(when links
	  ;; collapse `rel' and `rev' components
	  (setq links (apply 'append (mapcar 'cdr links)))
	  ;; extract
	  (setq links (mapcar
		       (lambda (elt)
			 (cons (or (plist-get (cadr elt) 'title)
				   (car elt))
			       (plist-get (cadr elt) 'href)))
		       links))
	  (let* ((maxlength (car (sort (mapcar (lambda (x)
						 (length (car x)))
					       links)
				       '>)))
		 (fmtstring
		  (format
		   "   <tr><td>%%%ds:</td><td><a href='%%s'>%%s</a></td></tr>"
		   maxlength)))
	    (insert
	     "   <tr><th colspan=2>Document Links</th></tr>\n")
	    (while links
	      (if (and (caar links) (cdar links))
		  (insert (format fmtstring
				  (url-insert-entities-in-string
				   (capitalize (caar links)))
				  (url-insert-entities-in-string
				   (cdar links))
				  (url-insert-entities-in-string
				   (cdar links))) "\n"))
	      (setq links (cdr links)))))
	
	(if info
	    (let* ((maxlength (car (sort (mapcar (lambda (x)
						   (length (car x)))
						 info)
					 '>)))
		   (fmtstring (format
			       "   <tr><td>%%%ds:</td><td>%%s</td></tr>"
			       maxlength)))
	      (insert
	       "   <tr><th colspan=2>Miscellaneous Variables</th></tr>\n")
	      (while info
		(if (and (caar info) (cdar info))
		    (insert (format fmtstring
				    (url-insert-entities-in-string
				     (capitalize (caar info)))
				    (url-insert-entities-in-string
				     (cdar info))) "\n"))
		(setq info (cdr info)))))
	(insert "  </table></body></html>\n")
	(current-buffer)))))

(defun w3-insert-formatted-url (p)
  "Insert a formatted url into a buffer.
With prefix arg, insert the url under point."
  (interactive "P")
  (let (buff str)
    (cond
     (p
      (setq p (widget-at (point)))
      (or p (error "No url under point"))
      (setq str (format "<a href=\"%s\">%s</a>" (widget-get p :href)
			(read-string "Link text: "
				     (buffer-substring
                                      (widget-get p :from)
                                      (widget-get p :to))))))
     (t
      (setq str (format "<a href=\"%s\">%s</a>" (url-view-url t)
			(read-string "Link text: " (buffer-name))))))
    (setq buff (read-buffer "Insert into buffer: " nil t))
    (if buff
	(with-current-buffer buff
	  (insert str))
      (message "Cancelled."))))

(defun w3-first-n-items (l n)
  "Return the first N items from list L."
  (let ((x 0)
	y)
    (if (> n (length l))
	(setq y l)
      (while (< x n)
	(setq y (nconc y (list (nth x l)))
	      x (1+ x))))
    y))

(defun w3-widget-button-press ()
  (interactive)
  (if (widget-at (point))
      (widget-button-press (point))))

(defun w3-widget-button-click (e)
  (interactive "@e")
  (if (featurep 'xemacs)
      (cond
       ((and (event-point e)
	     (widget-at (event-point e)))
	(widget-button-click e))
       ((and (fboundp 'event-glyph)
	     (event-glyph e)
	     (glyph-property (event-glyph e) 'widget))
	(widget-button-click e)))
    (save-excursion
      (mouse-set-point e)
      (if (widget-at (point))
	  (widget-button-click e)))))
   
;;;###autoload
(defun w3-maybe-follow-link-mouse (e)
  "Maybe follow a hypertext link under point.
If there is no link under point, this will try using
`url-get-url-at-point'"
  (interactive "e")
  (save-excursion
    (mouse-set-point e)
    (w3-maybe-follow-link)))

;;;###autoload
(defun w3-maybe-follow-link ()
  "Maybe follow a hypertext link under point.
If there is no link under point, this will try using
`url-get-url-at-point'"
  (interactive)
  (require 'w3)
  (w3-do-setup)
  (let* ((widget (widget-at (point)))
         (url1 (and widget (widget-get widget :href)))
         (url2 (url-get-url-at-point)))
    (cond
      (url1 (widget-button-press (point)))
      ((and url2 (string-match url-nonrelative-link url2)) (w3-fetch url2))
      (t (message "No URL could be found!")))))

;;;###autoload
(defun w3-follow-url-at-point-other-frame (&optional pt)
  "Follow the URL under PT, defaults to link under (point)."
  (interactive "d")
  (let ((url (url-get-url-at-point pt)))
    (and url (w3-fetch-other-frame url))))

;;;###autoload
(defun w3-follow-url-at-point (&optional pt)
  "Follow the URL under PT, defaults to link under (point)."
  (interactive "d")
  (let ((url (url-get-url-at-point pt)))
    (and url (w3-fetch url))))

(defun w3-fix-spaces (string)
  "Remove spaces/tabs at beginning of STRING and convert newlines to spaces."
  ;(url-convert-newlines-to-spaces
   (url-strip-leading-spaces
    (url-eat-trailing-space string)));)

(defun w3-source-document-at-point ()
  "View source to the document pointed at by link under point."
  (interactive)
  (w3-source-document t))

(defun w3-source-document (under)
  "View this document's source."
  (interactive "P")
  (let* ((url (if under (w3-view-this-url) (url-view-url t))))
    (set-buffer (generate-new-buffer (concat "Source of: " url)))
    (url-insert-file-contents url)
    (put-text-property (point-min) (point-max) 'w3-base url)
    (goto-char (point-min))
    (setq buffer-file-truename url
	  buffer-file-name url)
    ;; Null filename bugs `set-auto-mode' in Mule ...
    (condition-case ()
	(set-auto-mode)
      (error nil))
    (setq buffer-file-truename nil
	  buffer-file-name nil)
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (w3-notify-when-ready (current-buffer)))
  (run-hooks 'w3-source-file-hook))

(defun w3-mail-document-under-point ()
  "Mail the document pointed to by the hyperlink under point."
  (interactive)
  (w3-mail-current-document t))

(defun w3-mail-current-document (under &optional format)
  "Mail the current-document to someone."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (format (or format
		     (completing-read
		      "Format: "
		      '(("HTML Source")
			("Formatted Text")
			("PostScript")
			)
		  nil t)))
	 (case-fold-search t)
	 (url (cond
	       ((stringp under) under)
	       (under (w3-view-this-url t))
	       (t (url-view-url t))))
	 (content-charset (or (and (boundp 'buffer-file-coding-system)
				   (symbol-value buffer-file-coding-system)
				   (symbol-name buffer-file-coding-system))
			      "iso-8859-1"))
	 (content-type (concat "text/plain; charset=" content-charset))
	 (str
	  (save-excursion
	    (cond
	     ((and (equal "HTML Source" format) under)
	      (setq content-type (concat "text/html; charset="
					 content-charset))
	      (let ((url-source t))
		;; Fixme: this needs a callback -- which?
		(url-retrieve url)))
	     ((equal "HTML Source" format)
	      (setq content-type (concat "text/html; charset="
					 content-charset))
	      (if w3-current-source
		  (let ((x w3-current-source))
		    (set-buffer (get-buffer-create url-working-buffer))
		    (erase-buffer)
		    (insert x))
		;; Fixme: this needs a callback -- which?
		(url-retrieve url)))
	     ((and under (equal "PostScript" format))
	      (setq content-type "application/postscript")
	      (w3-fetch url)
	      (require 'ps-print)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((equal "PostScript" format)
	      (require 'ps-print)
	      (let ((ps-spool-buffer-name " *w3-temp*"))
		(if (get-buffer ps-spool-buffer-name)
		    (kill-buffer ps-spool-buffer-name))
		(setq content-type "application/postscript")
		(ps-spool-buffer-with-faces)
		(set-buffer ps-spool-buffer-name)))
	     ((and under (equal "Formatted Text" format))
	      (setq content-type (concat "text/plain; charset="
					 content-charset))
	      (w3-fetch url))
	     ((equal "Formatted Text" format)
	      (setq content-type (concat "text/plain; charset="
					 content-charset))))
	    (buffer-string))))
    (funcall url-mail-command)
    (mail-subject)
    (if (and (boundp 'mime/editor-mode-flag) mime/editor-mode-flag)
        (insert format " from <URL: " url ">")
      (insert format " from <URL: " url ">\n"
              "Mime-Version: 1.0\n"
              "Content-transfer-encoding: 8bit\n"
              "Content-type: " content-type))
    (re-search-forward mail-header-separator nil)
    (forward-char 1)
    (if (and (boundp 'mime/editor-mode-flag)
             (boundp 'mime-tag-format)
             mime/editor-mode-flag)
        (insert (format mime-tag-format content-type) "\n"))
    (save-excursion
      (insert str))
    (cond ((equal "HTML Source" format)
           (if (or (search-forward "<head>" nil t)
		   (search-forward "<html>" nil t))
	       (insert "\n"))
           (insert (format "<base href=\"%s\">" url))))
    ;; Fixme: not defined.
    (mail-to)))

(defun w3-internal-use-history (hist-item)
  ;; Go to the link in the history
  (let ((url (nth 0 hist-item))
	(buf (nth 1 hist-item))
	(pnt (nth 2 hist-item)))
    (cond
     ((null buf)			; Find a buffer with same url
      (let ((x (buffer-list))
	    (found nil))
	(while (and x (not found))
	  (with-current-buffer (car x)
	    (setq found (string= (url-view-url t) url))
	    (if (not found) (setq x (cdr x)))))
	(cond
	 (found
	  (switch-to-buffer (car x))
	  (if (number-or-marker-p pnt) (goto-char pnt)))
	 (t
	  (w3-fetch url)))))
     ((buffer-name buf)			; Reuse the old buffer if possible
      (switch-to-buffer buf)
      (if (number-or-marker-p pnt) (goto-char pnt))
      (if (and url (= ?# (string-to-char url)))	; Destination link
	  (progn
	    (goto-char (point-min))
	    (w3-find-specific-link (substring url 1 nil)))))
     ;; Fixme: url-maybe-relative not defined.
     (url (url-maybe-relative url))		; Get the link
     (t (message "Couldn't understand whats in the history.")))))

(defun w3-relative-link (url)
  (if (equal "#" (substring url 0 1))
      (progn
	(push-mark (point) t)
	(goto-char (point-min))
	(w3-find-specific-link (substring url 1 nil)))
    (w3-fetch (url-expand-file-name url))))

(defun w3-maybe-eval ()
  "Maybe evaluate a buffer of Emacs Lisp code."
  (if (funcall url-confirmation-func "This is emacs-lisp code, evaluate it?")
      (eval-buffer (current-buffer))
    (emacs-lisp-mode)))

(defun w3-use-links ()
  "Select one of the <LINK> tags from this document and fetch it."
  (interactive)
  (and (not w3-current-links)
       (error "No links defined for this document"))
  (w3-fetch "about:document"))

(defun w3-find-this-file ()
  "Do a `find-file' on the currently viewed html document.
Do this if it is a file: or ftp: reference"
  (interactive)
  (or url-current-object
      (error "Not a URL-based buffer"))
  (let ((type (url-type url-current-object)))
    (cond
     ((equal type "file")
      (find-file (url-filename url-current-object)))
     ((equal type "ftp")
      (find-file
       (format "/%s%s:%s"
	       (if (url-user url-current-object)
		   (concat (url-user url-current-object) "@"))
	       (url-host url-current-object)
	       (url-filename url-current-object))))
     (t (message "Sorry, I can't get that file so you can alter it.")))))

(defun w3-insert-this-url (pref-arg)
  "Insert the current url in another buffer.
With prefix ARG, insert URL under point"
  (interactive "P")
  (let ((thebuf (get-buffer (read-buffer "Insert into buffer: ")))
	(url (if pref-arg (w3-view-this-url t) (url-view-url t))))
    (if (and url (not (equal "Not on a link!" url)))
	(with-current-buffer thebuf
	  (insert url))
      (message "Not on a link!"))))

(defun w3-in-assoc (elt list)
  "Check to see if ELT matches any of the regexps in the car elements of LIST."
  (let (rslt)
    (while (and list (not rslt))
      (and (car (car list))
	   (stringp (car (car list)))
	   (not (string= (car (car list)) ""))
	   (string-match (car (car list)) elt)
	   (setq rslt (car list)))
      (setq list (cdr list)))
    rslt))

(defun w3-goto-last-buffer ()
  "Go to last WWW buffer visited."
  (interactive)
  (if w3-current-last-buffer
      (if w3-frame-name
	  (progn
	    (delete-other-windows)
	    (set-buffer w3-current-last-buffer)
	    (w3-goto-last-buffer))
	(w3-notify-when-ready w3-current-last-buffer))
    (message "No previous buffer found.")))

(fset 'w3-replace-regexp 'url-replace-regexp)

;;;###autoload
(defun w3-preview-this-buffer ()
  "See what this buffer will look like when its formatted as HTML.
HTML is the HyperText Markup Language used by the World Wide Web to
specify formatting for text.  More information on HTML can be found at
ftp.w3.org:/pub/www/doc."
  (interactive)
  (w3-fetch (concat "www://preview/" (buffer-name))))

(defun w3-source ()
  "Show the source of a file."
  (let ((tmp (buffer-name (generate-new-buffer "Document Source"))))
    (set-buffer url-working-buffer)
    (kill-buffer tmp)
    (rename-buffer tmp)
    ;; Make the URL show in list-buffers output
    (make-local-variable 'list-buffers-directory)
    (setq list-buffers-directory (url-view-url t))
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    (w3-notify-when-ready (get-buffer tmp))))

(defvar w3-mime-list-for-code-conversion
  '("text/plain" "text/html")
  "List of MIME types that require Mules' code conversion.")

(defvar w3-compression-encodings
  '("x-gzip" "gzip" "x-compress" "compress")
  "List of MIME encodings that denote compression.")

;; This looks bogus  -- fx
(defvar w3-no-conversion-encodings
  w3-compression-encodings
  "List of MIME encodings that require Mule not to convert
even though the MIME type is nil or listed in `w3-mime-list-for-code-conversion'.")

(defun w3-show-history-list ()
  "Format the url-history-list prettily and show it to the user."
  (interactive)
  (w3-fetch "www://auto/history"))

(defvar ps-spool-buffer-name)

(defun w3-save-as (&optional type)
  "Save a document to the local disk."
  (interactive)
  (let* ((completion-ignore-case t)
         (format (or type (completing-read
                           "Format: "
                           '(("HTML Source")
                             ("Formatted Text")
                             ("PostScript")
                             ("Binary"))
                           nil t)))
         (fname (expand-file-name
                 (read-file-name "File name: " default-directory)))
         (source w3-current-source)
         (text (buffer-string))
         (url (url-view-url t)))
    (with-current-buffer (generate-new-buffer " *w3-save-as*")
      (cond
       ((equal "Binary" format)
	(insert source))
       ((equal "HTML Source" format)
	(insert source)
	(goto-char (point-min))
	(if (re-search-forward "<head>" nil t)
	    (insert "\n"))
	(insert (format "<BASE HREF=\"%s\">\n" url)))
       ((or (equal "Formatted Text" format)
	    (equal "" format))
	(insert text))
       ((equal "PostScript" format)
	(require 'ps-print)
	(let ((ps-spool-buffer-name (buffer-name)))
	  (ps-spool-buffer-with-faces))))
      (let ((coding-system-for-write 'binary))
	(write-region (point-min) (point-max) fname))
      (kill-buffer (current-buffer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for logging of bad HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-reconstruct-tag (tagname desc)
  (concat "<" tagname " "
	  (mapconcat
	   (function (lambda (x)
		       (if (cdr x)
			   (concat (car x) "=\"" (cdr x) "\"")
			 (car x)))) desc " ") ">"))

(defun w3-debug-if-found (regexp type desc)
  (and w3-debug-html
       (save-excursion
	 (if (re-search-forward regexp nil t)
	     (w3-log-bad-html type desc)))))

(defun w3-log-bad-html (type desc)
  "Log bad HTML to the buffer specified by w3-debug-buffer."
  (if w3-debug-html
      (with-current-buffer (get-buffer-create w3-debug-buffer)
	(goto-char (point-max))
	(insert (make-string (1- (window-width)) w3-horizontal-rule-char) "\n")
	(cond
	 ((stringp type) (insert type "\n" desc "\n"))
	 ((eq type 'bad-quote)
	  (insert "Unterminated quoting character in SGML attribute value.\n"
		  desc "\n"))
	 ((eq type 'no-quote)
	  (insert "Unquoted SGML attribute value.\n" desc "\n"))
	 ((eq type 'no-textarea-end)
	  (insert "Unterminated <textarea> tag.\n"
		  (w3-reconstruct-tag "textarea" desc) "\n"))
	 ((eq type 'bad-link-tag)
	  (insert "Must specify either REL or REV with a <link> tag.\n"
		  (w3-reconstruct-tag "link" desc) "\n"))
	 ((eq type 'no-a-end)
	  (insert "Unterminated <a> tag.\n"
		  (w3-reconstruct-tag "a" desc) "\n"))
	 ((eq type 'no-form-end)
	  (insert "Unterminated <form> tag.\n"
		  (w3-reconstruct-tag "form" desc) "\n"))
	 ((eq type 'bad-base-tag)
	  (insert "Malformed <base> tag.\n"
		  (w3-reconstruct-tag "base" desc) "\n"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to handle formatting an html buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-add-delayed-graphic (widget)
  "Add a delayed image for the current buffer."
  (setq w3-delayed-images (cons widget w3-delayed-images)))


(defun w3-load-flavors ()
  "Load the correct emacsen specific stuff."
  (if (featurep 'xemacs)
      (require 'w3-xemac)
    (require 'w3-emacs))
  (if (featurep 'emacspeak)
      (condition-case ()
	  (progn
	    (require 'dtk-css-speech)
	    (require 'w3-speak))))
  (condition-case ()
      (require 'w3-site-init)
    (error nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic bug submission.                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-submit-bug ()
  "Submit a bug on Emacs-w3."
  (interactive)
  (require 'reporter)
  (and (yes-or-no-p "Do you really want to submit a bug on Emacs-w3? ")
       (let ((url (url-view-url t))
	     (vars '(window-system
		     window-system-version
		     system-type
		     ange-ftp-version
		     url-gateway-method
		     efs-version
		     ange-ftp-version
		     url-version
		     url)))
	 (if (and url (string= url "file:nil")) (setq url nil))
	 (dolist (x vars)
           (if (not (and (boundp x) (symbol-value x)))
               (setq vars (delq x vars))))
	 (reporter-submit-bug-report w3-bug-address
				     (concat "WWW v" w3-version-number " of "
					     w3-version-date)
				     vars
				     nil nil
				     "Description of Problem:"))))

(defalias 'w3-bug 'w3-submit-bug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for searching						    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-nuke-spaces-in-search (x)
  "Remove spaces from search strings . . ."
  (let ((new ""))
    (while (not (equal x ""))
      (setq new (concat new (if (= (string-to-char x) 32) "+"
			      (substring x 0 1)))
	    x (substring x 1 nil)))
    new))

(defun w3-search ()
  "Perform a search, if this is a searchable index."
  (interactive)
  (let* (querystring			; The string to send to the server
	 (data
	  (cond
	   ((null w3-current-isindex)
	    (let ((rels (cdr-safe (assq 'rel w3-current-links)))
		  val cur)
	      (while rels
		(setq cur (car rels)
		      rels (cdr rels))
		(if (and (or (string-match "^isindex$" (car cur))
			     (string-match "^index$" (car cur)))
			 (plist-get (cadr cur) 'href))
		    (setq val (plist-get (cadr cur) 'href)
			  rels nil))
		)
	      (if val
		  (cons val "Search on (+ separates keywords): "))))
	   ((eq w3-current-isindex t)
	    (cons (url-view-url t) "Search on (+ separates keywords): "))
	   ((consp w3-current-isindex)
	    w3-current-isindex)
	   (t nil)))
	 index)
    (if (null data) (error "Not a searchable index!"))
    (setq index (car data))
    (setq querystring (w3-nuke-spaces-in-search (read-string (cdr data))))
    (if (string-match "\\(.*\\)\\?.*" index)
	(setq index (match-string 1 index)))
    (w3-fetch
     (concat index (if (= ?? (string-to-char (substring index -1 nil)))
		       "" "?") querystring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto documentation, etc                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-help ()
  "Print documentation on w3 mode."
  (interactive)
  (w3-fetch "about:"))

;;;###autoload
(defun w3-version (&optional here)
  "Show the version number of W3 in the minibuffer.
If optional argument HERE is non-nil, insert info at point."
  (interactive "P")
  (let ((version-string
         (format "WWW %s, URL %s"
                 w3-version-number
                 url-version)))
    (if here
        (insert version-string)
      (if (interactive-p)
          (message "%s" version-string)
        version-string))))

;;;###autoload
(defun w3 ()
  "Retrieve the default World Wide Web home page.
The World Wide Web is a global hypertext system started by CERN in
Switzerland in 1991.

The home page is specified by the variable `w3-default-homepage'.  The
document should be specified by its fully specified Uniform Resource
Locator.  The document will be parsed as HTML (if appropriate) and
displayed in a new buffer."
  (interactive)
  (w3-do-setup)
  (if (and w3-track-last-buffer
	   (bufferp w3-last-buffer)
	   (buffer-name w3-last-buffer))
      (progn
	(switch-to-buffer w3-last-buffer)
	(message "Reusing buffer.  To reload, type %s."
		 (substitute-command-keys "\\[w3-reload-document]")))
    (cond
     ((null w3-default-homepage) (call-interactively 'w3-fetch))
     ((not (stringp w3-default-homepage))
      (error "Invalid setting for w3-default-homepage: %S"
	     w3-default-homepage))
     ((not (string-match ".*:.*" w3-default-homepage))
      (w3-fetch (concat "file:" w3-default-homepage)))
     (t
      (w3-fetch w3-default-homepage)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff for good local file handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-ff (file)
  "Find a file in any window already displaying it.
Otherwise just as `display-buffer', and using this function."
  (if (not (eq 'tty (device-type)))
      (let ((f (window-frame (display-buffer (find-file-noselect file)))))
	(set-mouse-position f 1 0)
	(raise-frame f)
	(unfocus-frame))
    (display-buffer (find-file-noselect file))))

(defun w3-default-local-file()
  "Use find-file to open the local file"
  (w3-ff (url-filename url-current-object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definition							    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-search-forward (string)
  (interactive "sSearch: ")
  (setq w3-last-search-item string)
  (if (and (not (search-forward string nil t))
	   (funcall url-confirmation-func
		    "End of document reached; continue from beginning? "))
      (progn
	(goto-char (point-min))
	(w3-search-forward string))))

(defun w3-search-again ()
  (interactive)
  (if (and w3-last-search-item
	   (stringp w3-last-search-item))
      (if (and (not (search-forward w3-last-search-item nil t))
	       (funcall url-confirmation-func
			"End of document reached; continue from beginning? "))
	  (progn
	    (goto-char (point-min))
	    (w3-search-again)))))

(defun w3-find-specific-link (link)
  (let ((pos (assq (intern link) w3-id-positions)))
    (if pos
	(progn
	  (goto-char (cdr pos))
	  (if (and (eolp) (not (eobp)))
	      (forward-char 1)))
      (message "Link #%s not found." link))))

(defun w3-force-reload-document ()
  "Reload the current document.
Take it from the network, even if cached and in local mode."
  (let ((url-standalone-mode nil))
    (w3-reload-document)))

(defun w3-reload-document (&optional explicit-coding-system)
  "Reload the current document.
With prefix argument, it reads a coding system to decode the document."
  (interactive
   (list (if (and (featurep 'mule) current-prefix-arg)
	     (read-coding-system "Coding system: "))))
  (let ((tmp (url-view-url t))
	(pnt (point))
	(window-start (progn
			(move-to-window-line 0)
			(point)))
	(url-request-extra-headers '(("Pragma" . "no-cache")))
	(w3-explicit-coding-system explicit-coding-system))
    (kill-buffer (current-buffer))
    (w3-fetch tmp)
    (if explicit-coding-system
	(w3-record-explicit-coding-system tmp explicit-coding-system))
    (goto-char pnt)
    (set-window-start (selected-window) (min window-start (point-max)))))

(defun w3-recall-explicit-coding-system (url)
  "Find user-specified explicit coding system for this URL.
Look for it in `w3-explicit-conversion-tree'"
  (let* ((urlobj (if (stringp url)
		     (url-generic-parse-url url)
		   url))
	 (hostname (or (url-host urlobj) "localhost"))
	 (fname-list (split-string (url-filename urlobj) "\\/")))
    ;; now recurse
    (w3-find-explicit-coding-system (cons hostname fname-list)
				    w3-explicit-conversion-tree)))

(defun w3-find-explicit-coding-system (fname-list tree)
  "Recall a user-specified explicit coding system."
  (let ((branch (assoc (car fname-list) tree)))
    (and branch
	 (or (and (cdr fname-list) (cddr branch)
		  (w3-find-explicit-coding-system (cdr fname-list)
						  (cddr branch)))
	     (cadr branch)))))

(defun w3-record-explicit-coding-system (url coding-system)
  "Record user-specified explicit coding system for URLs
as high as possible in w3-explicit-conversion-tree"
  (let* ((urlobj (if (stringp url)
		     (url-generic-parse-url url)
		   url))
	 (hostname (or (url-host urlobj) "localhost"))
	 (fname-list (split-string (url-filename urlobj) "\\/"))
	 (tree (or (assoc hostname w3-explicit-conversion-tree)
		   (let ((branch (list hostname)))
		     (setq w3-explicit-conversion-tree
			   (cons branch w3-explicit-conversion-tree))
		     branch))))
    ;; now recurse
    (w3-add-explicit-coding-system fname-list coding-system tree)
    (setq w3-explicit-encodings-changed-since-last-save t)))

(defun w3-add-explicit-coding-system (fname-list coding-system tree)
  "Memorize a user-specified explicit coding system."
  (if (and (cadr tree) (not (equal (cadr tree) coding-system)))
      (setcar (cdr tree) nil))
  (let ((branch (assoc (car fname-list) (cddr tree))))
    (cond (branch
	   ;; update existing branch
	   (cond ((cdr fname-list)
		  (or (equal (cadr branch) coding-system)
		      (null (cadr branch))
		      (setcar (cdr branch) nil))
		  (w3-add-explicit-coding-system (cdr fname-list)
						 coding-system branch))
		 (t
		  (setcar (cdr branch) coding-system))))
	  (t
	   ;; create a new branch
	   (setcdr tree
		   (if fname-list
		       (let ((subbranch (list (car fname-list))))
			 (w3-add-explicit-coding-system
			  (cdr fname-list) coding-system subbranch)
			 (cons (if (or (null (cddr tree))
				       (equal coding-system (cadr tree)))
				   coding-system)
			       (cons subbranch (cddr tree))))
		     (list coding-system)))))))

(defun w3-write-explicit-encodings (&optional fname)
  "Write the explicit encodings file into `w3-explicit-encodings-file'."
  (interactive)
  (or fname
      (and w3-explicit-encodings-file
	   (setq fname (expand-file-name w3-explicit-encodings-file))))
  (cond
   ((not w3-explicit-encodings-changed-since-last-save) nil)
   ((not (file-writable-p fname))
    (message "Explicit encodings file %s (see variable `w3-explicit-encodings-file') is unwritable." fname))
   (t
    (let ((make-backup-files nil)
	  (version-control nil)
	  (require-final-newline t))
      (with-current-buffer (get-buffer-create " *w3-tmp*")
	(erase-buffer)
	(insert "(setq w3-explicit-conversion-tree\n      '"
		(prin1-to-string w3-explicit-conversion-tree)
		")\n\n")
	(write-file fname)
	(kill-buffer (current-buffer))))))
  (setq w3-explicit-encodings-changed-since-last-save nil))

(defun w3-leave-buffer ()
  "Bury this buffer, but don't kill it."
  (interactive)
  (let ((x w3-current-last-buffer))
    (if w3-frame-name
	(w3-leave-or-quit-frameset x nil)
      (progn
	(bury-buffer nil)
	(if (and (bufferp x) (buffer-name x))
	    (w3-notify-when-ready x))))))

(defun w3-quit (&optional mega)
  "Quit WWW mode."
  (interactive "P")
  (if mega
      (mapcar
       (lambda (x)
	 (with-current-buffer (get-buffer x)
	   (if (eq major-mode 'w3-mode)
	       (w3-quit nil))))
       (buffer-list))
    (let ((x w3-current-last-buffer))
      (if w3-frame-name
	  (w3-leave-or-quit-frameset x t)
	(progn
	  (kill-buffer (current-buffer))
	  (if (and (bufferp x) (buffer-name x))
	      (w3-notify-when-ready x)))))))

(defun w3-leave-or-quit-frameset (x quit-p &optional top-down-p)
  (set-buffer x)
  (delete-other-windows)
  (let ((structure (reverse w3-frameset-structure)))
    (while structure
      (let ((elt (car structure)))
	(if (eq (car elt) 'frame)
	    (let* ((url (nth 2 elt))
		   (buf (w3-buffer-visiting url)))
	      (if buf
		  (progn
		    (set-buffer buf)
		    (if w3-frameset-structure
			(w3-leave-or-quit-frameset buf quit-p t)
		      (if quit-p
			  (kill-buffer buf)
			(bury-buffer buf))))))))
      (pop structure)))
  (if top-down-p
      (if quit-p
	  (kill-buffer x)
	(bury-buffer x))
    (progn
      (set-buffer x)
      (if quit-p
	  (w3-quit nil)
	(w3-leave-buffer)))))

(defun w3-view-this-url (&optional no-show)
  "View the URL of the link under point."
  (interactive)
  (let* ((widget (widget-at (point)))
	 (parent (and widget (widget-get widget :parent)))
	 (href (or (and widget (widget-get widget :href))
		   (and parent (widget-get parent :href)))))
    (cond
     ((and no-show href)
      href)
     (href
      (message "%s" (url-truncate-url-for-viewing href)))
     (no-show
      nil)
     (widget
      (widget-echo-help (point)))
     (t
      nil))))

(defun w3-load-delayed-images ()
    "Load inlined images that were delayed, if any."
  (interactive)
  (let ((w3-delay-image-loads nil)
	(todo w3-delayed-images))
    (setq w3-delayed-images nil)
    (while todo
      (w3-maybe-start-image-download (car todo))
      (setq todo (cdr todo)))))

(defun w3-save-this-url ()
  "Save url under point in the kill ring."
  (interactive)
  (w3-save-url t))

(defun w3-save-url (under-pt)
  "Save current url in the kill ring."
  (interactive "P")
  (let ((x (cond
	    ((stringp under-pt) under-pt)
	    (under-pt (w3-view-this-url t))
	    (t (url-view-url t)))))
    (if x
	(progn
	  (kill-new x)
	  (message "Stored URL in kill-ring."))
      (error "No URL to store"))))

(fset 'w3-end-of-document 'end-of-buffer)
(fset 'w3-start-of-document 'beginning-of-buffer)

(defun w3-scroll-up (&optional lines)
  "Scroll forward in View mode, or exit if end of text is visible.
No arg means whole window full.  Arg is number of lines to scroll."
  (interactive "P")
  (if (and (pos-visible-in-window-p (point-max))
	   ;; Allow scrolling backward at the end of the buffer.
	   (or (null lines)
	       (> lines 0)))
      nil
    (let ((view-lines (1- (window-height))))
      (setq lines
	    (if lines (prefix-numeric-value lines)
	      view-lines))
      (if (>= lines view-lines)
	  (scroll-up nil)
	(if (>= (- lines) view-lines)
	    (scroll-down nil)
	  (scroll-up lines)))
      (cond ((pos-visible-in-window-p (point-max))
	     (goto-char (point-max))
	     (recenter -1)))
      (move-to-window-line -1)
      (beginning-of-line))))


(defun w3-mail-document-author ()
  "Send mail to the author of this document, if possible."
  (interactive)
  (let ((x w3-current-links)
	(y nil)
	(found nil))
    (setq found (cdr-safe (assoc "reply-to" url-current-mime-headers)))
    (if (and found (not (string-match url-nonrelative-link found)))
	(setq found (list (list 'href (concat "mailto:" found)))))
    (while (and x (not found))
      (setq y (car x)
	    x (cdr x)
	    found (cdr-safe (assoc "made" y))))
    (if found
	(let ((possible nil)
	      (href nil))
	  (setq x (car found))		; Fallback if no mail(to|server) found
	  (while found
	    (setq href (plist-get (pop found) 'href))
	    (if (and href (string-match "^mail[^:]+:" href))
		(setq possible (cons href possible))))
	  (case (length possible)
	    (0				; No mailto links found
	     (w3-fetch href))		; fall back onto first 'made' link
	    (1				; Only one found, get it
	     (w3-fetch (car possible)))
	    (otherwise
	     (w3-fetch (completing-read "Choose an address: "
					(mapcar 'list possible)
					nil t (car possible))))))
      (message "Could not automatically determine authors address, sorry."))))

(defun w3-kill-emacs-func ()
  "Routine called when exiting Emacs.  Do miscellaneous clean up."
  (url-history-save-history)
  (message "Cleaning up w3 temporary files...")
  ;; FIXME!  This needs to be in the URL library now I guess?
  '(let ((x (nconc
	    (and (file-exists-p w3-temporary-directory)
		 (directory-files w3-temporary-directory t "url-tmp.*"))
	    (and (file-exists-p url-temporary-directory)
		 (directory-files url-temporary-directory t
				  (concat "url"
					  (int-to-string
					   (user-real-uid)) ".*")))
	    (and (file-exists-p url-temporary-directory)
		 (directory-files url-temporary-directory t "url-tmp.*")))))
    (while x
      (condition-case ()
	  (delete-file (car x))
	(error nil))
      (setq x (cdr x))))
  (message "Cleaning up w3 temporary files... done."))

(defalias 'w3-warn
  (cond
   ((fboundp 'display-warning)
    #'display-warning)
   ((fboundp 'warn)
    (lambda (class message &optional level)
      (if (and (eq class 'html)
	       (not w3-debug-html))
	  nil
	(warn "(%s/%s) %s" class (or level 'warning) message))))
   (t
    (lambda (class message &optional level)
      (if (and (eq class 'html)
	       (not w3-debug-html))
	  nil
	(with-current-buffer (get-buffer-create "*W3-WARNINGS*")
	  (goto-char (point-max))
	  (save-excursion
	    (insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
	  (display-buffer (current-buffer))))))))

(defun w3-map-links (function &optional maparg)
  "Map FUNCTION over the hypertext links in current buffer,
FUNCTION is called with the arguments WIDGET and MAPARG."
  (let ((parent)
	(highly-unlikely-name-for-a-variable-holding-a-function function))
    (widget-map-buttons
     (lambda (widget arg)
       (setq parent (and widget (widget-get widget :parent)))
       ;; Check to see if its got a URL tacked on it somewhere
       (cond
	((and widget (widget-get widget :href))
	 (funcall highly-unlikely-name-for-a-variable-holding-a-function
		  widget maparg))
	((and parent (widget-get parent :href))
	 (funcall highly-unlikely-name-for-a-variable-holding-a-function
		  widget maparg))
	(t nil))
       nil))))

(defun w3-refresh-stylesheets ()
  "Reload all stylesheets."
  (interactive)
  (setq w3-user-stylesheet nil
	w3-face-cache nil)
  (w3-find-default-stylesheets)
  )

(defvar w3-loaded-stylesheets nil
  "A list of all the stylesheets Emacs-W3 loaded at startup.")

(defvar w3--package-directory (file-name-directory load-file-name))

(defun w3-find-default-stylesheets ()
  (setq w3-loaded-stylesheets nil)
  (let* ((lightp (css-color-light-p 'default))
	 (longname (if lightp "stylesheet-light" "stylesheet-dark"))
	 (shortname (if lightp "light.css" "dark.css"))
	 (w3-lisp (file-name-directory (locate-library "w3")))
	 (w3-root (expand-file-name "../.." w3-lisp))
	 (no-user-init (= 0 (length user-init-file)))
	 (w3-configuration-directory (if no-user-init
					 "/this/is/a/highly/unlikely/directory/name"
				       w3-configuration-directory))
	 (directories (list
                       (expand-file-name "etc" w3--package-directory)
		       (if (fboundp 'locate-data-directory)
			   (locate-data-directory "w3"))
		       data-directory
		       (concat data-directory "w3/")
		       (expand-file-name "../../w3" data-directory)
		       w3-lisp
		       w3-root
		       (expand-file-name "w3" w3-root)
		       (expand-file-name "etc" w3-root)
		       (expand-file-name "etc/w3" w3-root)
 		       (expand-file-name "../" w3-lisp)
 		       (expand-file-name "../w3" w3-lisp)
 		       (expand-file-name "../etc" w3-lisp)
		       w3-configuration-directory))
	 (total-found 0)
	 (possible (append
		    (apply
		     'append
		     (mapcar
		      (function
		       (lambda (dir)
			 (list
			  (expand-file-name shortname dir)
			  (expand-file-name longname dir)
			  (expand-file-name "stylesheet" dir)
			  (expand-file-name "default.css" dir))))
		      directories))
		    (and (not no-user-init)
			 (list w3-default-stylesheet))))
	 (remember possible)
	 (found nil)
	 (cur nil))
    (while possible
      (setq cur (car possible)
	    possible (cdr possible)
	    found (and cur (file-exists-p cur) (file-readable-p cur)
		       (not (file-directory-p cur)) cur))
      (if found
	  (setq total-found (1+ total-found)
		w3-loaded-stylesheets (cons cur w3-loaded-stylesheets)
		w3-user-stylesheet (css-parse (concat "file:" cur) nil
					      w3-user-stylesheet))))
    (if (= 0 total-found)
	(progn
	  (w3-warn
	   'style
	   (concat
	    "No stylesheets found!  Check configuration! DANGER DANGER!\n"
	    "Emacs-W3 checked for its stylesheet in the following places\n"
	    "and did not find one.  This means that some formatting will\n"
	    "be wrong, and most colors and fonts will not be set up correctly.\n"
	    "------\n"
	    (mapconcat 'identity remember "\n")
	    "------"))
	  (error "No stylesheets found!  Check configuration! DANGER DANGER!")))))

(defvar w3-widget-global-map nil)

;;;###autoload
(defun w3-do-setup ()
  "Do setup.
This is to avoid conflict with user settings when W3 is dumped with
Emacs."
  (unless w3-setup-done
    (url-do-setup)
    (w3-load-flavors)
    (w3-setup-version-specifics)
    (setq w3-default-configuration-file (expand-file-name
					 (or w3-default-configuration-file
					     "profile")
					 w3-configuration-directory))
    (if (and init-file-user
	     w3-default-configuration-file
	     (file-exists-p w3-default-configuration-file))
	(condition-case e
	    (load w3-default-configuration-file nil t)
	  (error
	   (let ((buf-name " *Configuration Error*"))
	     (if (get-buffer buf-name)
		 (kill-buffer (get-buffer buf-name)))
	     (display-error e (get-buffer-create buf-name))
	     (save-excursion
	       (switch-to-buffer-other-window buf-name)
	       (shrink-window-if-larger-than-buffer))
	     (w3-warn 'configuration
		      (format (eval-when-compile
				(concat
				 "Configuration file `%s' contains an error.\n"
				 "Please consult the `%s' buffer for details."))
			      w3-default-configuration-file buf-name))))))
	       
    ;; Load the explicit encodings file if it exists
    (if (and w3-explicit-encodings-file
	     (file-exists-p w3-explicit-encodings-file))
	(condition-case nil
	    (load w3-explicit-encodings-file nil t)
	  (error nil)))

    (if (and (eq w3-user-colors-take-precedence 'guess)
	     (not (eq (device-type) 'tty))
	     (not (eq (device-class) 'mono)))
	(progn
	  (setq w3-user-colors-take-precedence t)
	  (w3-warn
	   'html
	   "Disabled document color specification because of mono display.")))

    (w3-refresh-stylesheets)
    (setq w3-setup-done t)

    (add-minor-mode 'w3-netscape-emulation-minor-mode " NS"
		    w3-netscape-emulation-minor-mode-map)
    (add-minor-mode 'w3-lynx-emulation-minor-mode " Lynx"
		    w3-lynx-emulation-minor-mode-map)
  
    (setq url-package-version w3-version-number
	  url-package-name "Emacs-W3")

    (w3-setup-terminal-chars)

    (cond
     ((memq system-type '(ms-dos ms-windows))
      (setq w3-hotlist-file (or w3-hotlist-file
				(expand-file-name "~/mosaic.hot"))
	    ))
     ((memq system-type '(axp-vms vax-vms))
      (setq w3-hotlist-file (or w3-hotlist-file
				(expand-file-name "~/mosaic.hotlist-default"))
	    ))
     (t
      (setq w3-hotlist-file (or w3-hotlist-file
				(expand-file-name "~/.mosaic-hotlist-default"))
	    )))
  
    ;; Set up a hook that will save the history list when exiting
    ;; emacs
    (add-hook 'kill-emacs-hook 'w3-kill-emacs-func)

    ;; Load in the hotlist if they haven't set it already
    (or w3-hotlist (w3-parse-hotlist))

    ;; Set the default home page, honoring their defaults, then the
    ;; standard WWW_HOME, then default to the documentation @ gnu.org
    (or w3-default-homepage
	(setq w3-default-homepage
	      (or (getenv "WWW_HOME")
		  "http://elpa.gnu.org/packages/w3.html")))

    (run-hooks 'w3-load-hook)))

(defun w3-mark-link-as-followed (ext dat)
  "Mark a link as followed."
  (message "Reimplement w3-mark-link-as-followed"))

(defun w3-only-links ()
  (let (result)
    (w3-map-links (lambda (x _y) (push x result)))
    result))

(defun w3-download-redirect-callback (&rest args)
  (let (redirect-url errorp)
    ;; Handle both styles of `url-retrieve' callbacks...
    (cond
     ((listp (car args))
      ;; Emacs 22 style.  First argument is a list.
      (let ((status (car args)))
	(when (eq (car status) :error)
	  (setq errorp t)
	  (setq status (cddr args)))
	(when (eq (car status) :redirect)
	  (setq redirect-url (second (car args))))

	(setq args (cdr args))))

     ((eq (car args) :redirect)
      ;; Pre-22 redirect.
      (setq redirect-url (cadr args))
      (while (eq (car args) :redirect)
	(setq args (cddr args)))))

    (if errorp
	(message "Download of %s failed." (url-view-url t))
      (w3-download-callback (car args)))))

(defun w3-download-callback (fname)
  (let ((coding-system-for-write 'binary))
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (write-region (point) (point-max) fname))
  (url-mark-buffer-as-dead (current-buffer))
  (message "Download of %s complete." (url-view-url t))
  (sit-for 3))

(defun w3-download-url-at-point ()
  "Download the URL under point."
  (interactive)
  (w3-download-url-wrapper t))

(defun w3-download-this-url ()
  "Download the current URL."
  (interactive)
  (w3-download-url-wrapper nil))
  
(defun w3-download-url-wrapper (under-pt)
  "Download current URL."
  (let ((x (if under-pt (w3-view-this-url t) (url-view-url t))))
    (if x
	(w3-download-url x)
      (error "No link found"))))
	     
(defun w3-download-url (url &optional file-name)
  (interactive (list (w3-read-url-with-default)))
  (let* ((url-mime-accept-string "*/*")
	 (urlobj (url-generic-parse-url url))
	 (stub-fname (w3-url-file-nondirectory (or (url-filename urlobj) "")))
	 (dir (or mailcap-download-directory "~/"))
	 (fname (or file-name
		    (expand-file-name
		     (read-file-name "Filename to save as: "
				     dir
				     stub-fname
				     nil
				     stub-fname) dir))))
    (url-retrieve url 'w3-download-redirect-callback (list fname))))

;;;###autoload
(defun w3-follow-link-other-frame (&optional p)
  "Attempt to follow the hypertext reference under point in a new frame.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (cond
   ((and (fboundp 'make-frame)
	 (fboundp 'select-frame))
    (let ((frm (make-frame)))
      (select-frame frm)
      (w3-follow-link p)))
   (t (w3-follow-link p))))

;;;###autoload
(defun w3-follow-link (&optional p)
  "Attempt to follow the hypertext reference under point.
With prefix-arg P, ignore viewers and dump the link straight
to disk."
  (interactive "P")
  (let* ((widget (widget-at (point)))
	 (href (and widget (widget-get widget :href))))
    (cond
     ((null href) nil)
     (p
      (w3-download-url href))
     (t
      (w3-fetch href)))))

;;;###autoload
(defun w3-next-document ()
  (interactive)
  (let ((link (or (let ((rel (assq 'rel w3-current-links)))
                    (and rel (assoc "next" rel)))
                  (let ((rev (assq 'rev w3-current-links)))
                    (and rev (or (assoc "previous" rev)
                                 (assoc "prev" rev))))))
        href)
    (and link (setq link (cdr link)))
    (while (and link (null href))
      (setq href (plist-get (car link) 'href))
      (setq link (cdr link)))
    (if href
        (w3-fetch href)
      (error "No NEXT document"))))

;;;###autoload
(defun w3-prev-document ()
  (interactive)
  (let ((link (or (let ((rel (assq 'rel w3-current-links)))
                    (and rel (or (assoc "previous" rel)
                                 (assoc "prev" rel))))
                  (let ((rev (assq 'rev w3-current-links)))
                    (and rev (assoc "next" rev)))))
        href)
    (and link (setq link (cdr link)))
    (while (and link (null href))
      (setq href (plist-get (car link) 'href))
      (setq link (cdr link)))
    (if href
        (w3-fetch href)
      (error "No PREVIOUS document"))))

;; Why are these defined?
(defun w3-widget-forward (arg)
  "Move point to the next field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (widget-forward arg))

(defun w3-widget-backward (arg)
  "Move point to the previous field or button.
With optional ARG, move across that many fields."
  (interactive "p")
  (w3-widget-forward (- arg)))

(defun w3-complete-link ()
  "Choose a link from the current buffer and follow it."
  (interactive)
  (let (links-alist
	link-at-point
	choice
	(completion-ignore-case t))
    (setq link-at-point (widget-at (point))
	  link-at-point (and
			 link-at-point
			 (widget-get link-at-point :href)
			 (widget-get link-at-point :from)
			 (widget-get link-at-point :to)
			 (w3-fix-spaces
			  (buffer-substring-no-properties
			   (widget-get link-at-point :from)
			   (widget-get link-at-point :to)))))
    (w3-map-links (lambda (widget _arg)
		    (if (and (widget-get widget :from)
			     (widget-get widget :to))
			(setq links-alist (cons
					   (cons
					    (w3-fix-spaces
					     (buffer-substring-no-properties
					      (widget-get widget :from)
					      (widget-get widget :to)))
					    (widget-get widget :href))
					   links-alist)))))
    (if (not links-alist) (error "No links in current document"))
    (setq links-alist (sort links-alist (lambda (x y)
					  (string< (car x) (car y)))))
    ;; Destructively remove duplicate entries from links-alist.
    (let ((remaining-links links-alist))
      (while remaining-links
	(if (equal (car remaining-links) (car (cdr remaining-links)))
	    (setcdr remaining-links (cdr (cdr remaining-links)))
	  (setq remaining-links (cdr remaining-links)))))
    (setq choice (completing-read
		  (if link-at-point
		      (concat "Link (default "
			      (if (< (length link-at-point) 20)
				  link-at-point
				(concat
				 (substring link-at-point 0 17) "..."))
			      "): ")
		    "Link: ") links-alist nil t))
    (if (and (string= choice "") link-at-point)
	(setq choice link-at-point))
    (let ((match (try-completion choice links-alist)))
      (cond
       ((eq t match)			; We have an exact match
	(setq choice (cdr (assoc choice links-alist))))
       ((stringp match)
	(setq choice (cdr (assoc match links-alist))))
       (t (setq choice nil)))
      (if choice
	  (w3-fetch choice)))))

(defun w3-display-errors ()
  "Display any HTML errors for the current page."
  (interactive)
  (let ((w3-notify 'friendly)
	(inhibit-read-only t)
	(buffer nil)
	(todo w3-current-badhtml)
	(url (url-view-url t)))
    (if (not todo)
	(error "No HTML errors on this page!  Amazing, isn't it?"))
    (with-current-buffer
        (get-buffer-create (concat "HTML Errors for: " (or url "???")))
      (setq buffer (current-buffer))
      (erase-buffer)
      (while todo
	(goto-char (point-min))
	(insert "\n" (car todo))
	(setq todo (cdr todo)))
      (when url
	(goto-char (point-min))
	(insert (format "HTML Errors for: <URL:%s>\n" url)))
      (set (make-local-variable 'font-lock-keywords)
	   w3-html-errors-font-lock-keywords)
      (set (make-local-variable 'font-lock-keywords-only) nil)
      (set (make-local-variable 'font-lock-keywords-case-fold-search) nil)
      (set (make-local-variable 'font-lock-syntax-table) nil)
      (set (make-local-variable 'font-lock-beginning-of-syntax-function)
	   'beginning-of-line)
      (run-hooks 'w3-display-errors-hook))
    (w3-notify-when-ready buffer)))

(defun w3-mode ()
  "Mode for viewing HTML documents.
If called interactively, will display the current buffer as HTML.

Current keymap is:
\\{w3-mode-map}"
  (interactive)
  (w3-do-setup)
  (if (interactive-p)
      (w3-preview-this-buffer)
    ;; This code used to keep a few buffer-local variables around so
    ;; that we could be a nice major mode and kill all the local
    ;; variables like we are supposed to, but still save some of them
    ;; that were set up in the URL and parsing libraries.
    ;;
    ;: Two problems with this approach:
    ;;
    ;; 1) It kills buffer-local faces in XEmacs, which we use extensively.
    ;; 2) With Emacspeak it causes all the personality properties on
    ;;    the text to mysteriously disappear.
    ;;
    ;; So screw it... I'm leaving the code in here commented out so
    ;; that I don't forget and try to 'fix' this later in life.
    ;;
    ;; - Bill Perry Nov 21, 2001
    ;; (let ((tmp (mapcar (lambda (x) (cons x (and (boundp x) (symbol-value x))))
    ;;                    w3-persistent-variables)))
    ;;    (kill-all-local-variables))
    ;;    (mapcar (lambda (x) (if (boundp (car x))
    ;;                            (set-variable (car x) (cdr x)))) tmp))
    (use-local-map w3-mode-map)
    (setq mode-name "WWW")
    (setq major-mode 'w3-mode)
    (w3-mode-version-specifics)
    (w3-menu-install-menus)
    (setq truncate-lines t
	  mode-line-format w3-modeline-format)
    (run-hooks 'w3-mode-hook)
    (widget-setup)))

(put 'w3-mode 'mode-class 'special)

(require 'url)
(require 'w3-parse)
(require 'w3-display)
;; (require 'w3-auto)
(require 'w3-emulate)
(require 'w3-menu)
(require 'w3-mouse)
(provide 'w3)

;;; w3.el ends here
