;;; url-hotlist.el --- URL interface to bookmarks

;; Copyright (c) 1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: wmperry $
;; Created: $Date: 1999/12/05 08:35:52 $
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

;;; Code:

(require 'url-util)
(require 'url-parse)
(require 'w3-hot)

(defun url-hotlist-html-generator (node)
  (cond
   ((stringp node)
    ;; Top-level node...
    (insert " <h1 align=\"center\"> " node " </h1>\n"))
   ((stringp (cdr node))
    ;; A real hyperlink, insert it into the buffer
    (insert (format "   <dd> <a href=\"%s\">%s</a>\n" (cdr node) (car node))))
   (t
    ;; A submenu
    (insert " <dl>\n   <dt><b>" (car node) "</b>\n")
    (mapc #'url-hotlist-html-generator (cdr node))
    (insert " </dl>\n"))))

(defun url-hotlist (url)
  "URL-based interface onto Emacs/W3 hotlists."
  (let ((action (url-filename url))
	(func nil)
	(query-args nil))
    (if (string-match (eval-when-compile (regexp-quote "?")) action)
	(setq action (substring action 0 (match-beginning 0))
	      query-args (url-parse-query-string (substring (url-filename url) (match-end 0)) t)))
    (setq func (intern (downcase (format "url-hotlist-%s" action))))
    (with-current-buffer (generate-new-buffer " *w3-hotlist-url*")
      (insert "Content-type: text/html\n\n")
      (if (fboundp func)
	  (funcall func query-args)
	(insert "<html>\n"
		" <head>\n"
		"  <title>Unknown hotlist action</title>\n"
		" </head>\n"
		" <body>\n"
		"  <p>\n"
		"   Unknown hotlist URL action <b>" action "</b>\n"
		"  </p>\n"
		" </body>\n"
		"</html>\n"))
      (current-buffer))))

(defun url-hotlist-view (_query-args)
  (insert "<html>\n"
	  " <head>\n"
	  "  <title>Hotlist View</title>\n"
	  " </head>\n"
	  " <body>\n")
  (mapc #'url-hotlist-html-generator w3-hotlist)
  (insert " </body>\n"
	  "</html>\n"))  

(defun url-hotlist-search (query-args)
  (let ((regexp (cdr-safe (assoc "regexp" query-args)))
	(hot-alist (w3-hot-convert-to-alist w3-hotlist)))
    (insert "<html>\n"
	    " <head>\n"
	    "  <title>Hotlist search results</title>\n"
	    " </head>\n"
	    " <body>\n")
    (if (not regexp)
	(insert "  <h3>Malformed search URL</h3>\n")
      (insert "  <p>Search results for:<br> <tt>"
              (mapconcat #'identity regexp "<br>\n")
	      "</tt></h3>\n"
	      "  <ul>\n")
      (dolist (node hot-alist)
        (dolist (r regexp)
          (if (string-match r (car node))
              (insert (format "   <li> <a href=\"%s\">%s</a>\n"
                              (cdr node) (car node))))))
      (insert "  </ul>\n"))
    (insert " </body>\n"
	    "</html>\n")))

(provide 'url-hotlist)
