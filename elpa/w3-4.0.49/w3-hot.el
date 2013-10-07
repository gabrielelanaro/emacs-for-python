;;; w3-hot.el --- Main functions for emacs-w3 on all platforms/versions

;; Copyright (c) 1996 - 1999, 2013 Free Software Foundation, Inc.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure for hotlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (
;;  ("name of item1" . "http://foo.bar.com/")    ;; A single item in hotlist
;;  ("name of item2" . (                         ;; A sublist
;;                      ("name of item3" . "http://www.ack.com/")
;;                     ))
;; )  ; end of hotlist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'w3-vars)
(require 'w3-parse)
(require 'url-parse)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hotlist Handling Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-html-bookmarks nil)

;;;###autoload
(defun w3-read-html-bookmarks (fname)
  "Import an HTML file into the Emacs-w3 format."
  (interactive "fBookmark file: ")
  (if (not (file-readable-p fname))
      (error "Can not read %s..." fname))
  (with-current-buffer (get-buffer-create " *bookmark-work*")
    (erase-buffer)
    (insert-file-contents fname)
    (let* ((w3-debug-html nil)
	   (bkmarks nil)
	   (parse (w3-parse-buffer (current-buffer))))
      (setq parse w3-last-parse-tree
	    bkmarks (nreverse (w3-grok-html-bookmarks parse))
	    w3-hotlist bkmarks))))

(defvar w3--cur-stack)

(defsubst w3-hot-push-new-menu ()
  (setq w3--cur-stack (cons (list "") w3--cur-stack)))

;; This stores it in menu format
'(defsubst w3-hot-push-new-item (title href)
   (setcar w3--cur-stack (cons (vector title (list 'w3-fetch href) t)
                           (car w3--cur-stack))))

;; This stores it in alist format
(defsubst w3-hot-push-new-item (title href)
  (setcar w3--cur-stack (cons (cons title href) (car w3--cur-stack))))

(defvar w3--cur-title)

(defsubst w3-hot-finish-submenu ()
  (let ((x (nreverse (car w3--cur-stack)))
        (y (pop w3--cur-title)))
    (while (string= y "")
      (setq y (pop w3--cur-title)))
    (and x (setcar x y))
    (setq w3--cur-stack (cdr w3--cur-stack))
    (if w3--cur-stack
        (setcar w3--cur-stack (cons x (car w3--cur-stack)))
      (setq w3--cur-stack (list x)))))


(defun w3-grok-html-bookmarks-internal (tree)
  (let (node tag content args)
    (while tree
      (setq node (car tree)
	    tree (cdr tree)
	    tag (and (listp node) (nth 0 node))
	    args (and (listp node) (nth 1 node))
	    content (and (listp node) (nth 2 node)))
      (cond
       ((eq tag 'hr)
	(setq w3--cur-title '("------")))
       ((eq tag 'title)
	(setq w3--cur-title (list (w3-normalize-spaces (car content))))
	(w3-grok-html-bookmarks-internal content))
       ((memq tag '(dl ol ul))
	(w3-hot-push-new-menu)
	(w3-grok-html-bookmarks-internal content)
	(w3-hot-finish-submenu))
       ((and (memq tag '(dt li p))
	     (stringp (car content)))
	(setq w3--cur-title (cons (w3-normalize-spaces (car content))
			      w3--cur-title)))
       ((and (eq tag 'a)
	     (stringp (car-safe content))
	     (cdr-safe (assq 'href args)))
	(w3-hot-push-new-item (w3-normalize-spaces (car-safe content))
			      (cdr-safe (assq 'href args))))
       (content
	(w3-grok-html-bookmarks-internal content))))))

(defun w3-grok-html-bookmarks (chunk)
  (let (
	w3--cur-title
	w3--cur-stack
	)
    (w3-grok-html-bookmarks-internal chunk)
    (reverse (car w3--cur-stack))))

(defvar w3--alist)

(defun w3-hot-convert-to-alist-internal (l &optional prefix)
  (dolist (node l)
    (cond
     ((stringp node)
      ;; Top-level node... ignore.
      )
     ((stringp (cdr node))
      ;; A real hyperlink, push it onto the alist.
      (push (cons (if prefix (concat prefix " / " (car node)) (car node))
                  (cdr node))
            w3--alist))
     (t
      ;; A submenu, add to prefix and recurse.
      (w3-hot-convert-to-alist-internal
       (cdr node) (if prefix (concat prefix " / " (car node)) (car node)))))))

(defun w3-hot-convert-to-alist (l)
  (let ((w3--alist nil))
    (w3-hot-convert-to-alist-internal l)
    w3--alist))
       
(defun w3-delete-from-alist (x alist)
  ;; Remove X from ALIST, return new alist
  (if (eq (assoc x alist) (car alist)) (cdr alist)
    (delq (assoc x alist) alist)))

(defun w3-hotlist-parse-old-mosaic-format ()
  (let (cur-link cur-alias)
    (while (re-search-forward "^\n" nil t) (replace-match ""))
    (goto-char (point-min))
    (forward-line 2)
    (while (not (eobp))
      (re-search-forward "^[^ ]*" nil t)
      (setq cur-link (buffer-substring (match-beginning 0) (match-end 0)))
      (setq cur-alias (buffer-substring (progn
					  (forward-line 1)
					  (beginning-of-line)
					  (point))
					(progn
					  (end-of-line)
					  (point))))
      (if (not (equal cur-alias ""))
	  (setq w3-hotlist (cons (list cur-alias cur-link) w3-hotlist))))))

;;;###autoload
(defun w3-parse-hotlist (&optional fname)
  "Read in the hotlist specified by FNAME"
  (if (not fname) (setq fname w3-hotlist-file))
  (setq w3-hotlist nil)
  (if (not (file-exists-p fname))
      (message "%s does not exist!" fname)
    (let* ((buffer (get-buffer-create " *HOTW3*"))
	   (case-fold-search t))
      (with-current-buffer buffer
        (erase-buffer)
        (insert-file-contents fname)
        (goto-char (point-min))
        (cond
         ((looking-at "ncsa-xmosaic-hotlist-format-1") ;; Old-style NCSA Mosaic
          (w3-hotlist-parse-old-mosaic-format))
         ((or (looking-at "<!DOCTYPE")	; Some HTML style, including netscape
              (re-search-forward "<a[ \n]+href" nil t))
          (w3-read-html-bookmarks fname))
         (t
          (message "Cannot determine format of hotlist file: %s" fname)))
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

;;;###autoload
(defun w3-use-hotlist ()
  "Possibly go to a link in your W3/Mosaic hotlist.
This is part of the emacs World Wide Web browser.  It will prompt for
one of the items in your 'hotlist'.  A hotlist is a list of often
visited or interesting items you have found on the World Wide Web."
  (interactive)
  (if (not w3-setup-done) (w3-do-setup))
  (if (not w3-hotlist) (message "No hotlist in memory!")
    (let* ((completion-ignore-case t)
	   (hot-alist (w3-hot-convert-to-alist w3-hotlist))
	   (url (cdr (assoc
		      (completing-read "Goto Document: " hot-alist nil t)
		      hot-alist))))
      (if (string= "" url) (error "No document specified!"))
      (w3-fetch url))))

;;;###autoload
(defun w3-hotlist-add-document-at-point (pref-arg)
  "Add the document pointed to by the hyperlink under point to the hotlist."
  (interactive "P")
  (let ((url (w3-view-this-url t))
	(widget (widget-at (point)))
	(title nil))
    (or url (error "No link under point."))
    (if (and (widget-get widget :from)
	     (widget-get widget :to))
	(setq title (buffer-substring (widget-get widget :from)
				      (widget-get widget :to))))
    (w3-hotlist-add-document pref-arg (or title url) url)))

;;;###autoload
(defun w3-hotlist-add-document (_pref-arg &optional _the-title _the-url)
  "Add this documents url to the hotlist"
  (interactive "P")
  (error "Adding to hotlist not implemented yet."))

;;;###autoload
(defun w3-hotlist-delete ()
  "Deletes a document from your hotlist file"
  (interactive)
  (error "Deleting from hotlist not implemented yet."))

;;;###autoload
(defun w3-hotlist-refresh ()
  "Reload the default hotlist file into memory"
  (interactive)
  (w3-do-setup)
  (w3-parse-hotlist))

;;;###autoload
(defun w3-hotlist-apropos (regexp)
  "Show hotlist entries matching REGEXP."
  (interactive "sW3 Hotlist Apropos (regexp): ")
  (or w3-setup-done (w3-do-setup))
  (w3-fetch (concat "hotlist:search?regexp=" (url-hexify-string regexp))))

;;;###autoload
(defun w3-hotlist-view ()
  "Show the hotlist."
  (interactive)
  (w3-fetch "hotlist:view"))

(provide 'w3-hot)
