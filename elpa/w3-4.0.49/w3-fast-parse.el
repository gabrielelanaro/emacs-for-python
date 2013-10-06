;;; w3-fast-parse.el --- Parse HTML and/or XML for Emacs/W3

;; Author: William M. Perry <wmperry@gnu.org>

;; Copyright © 2001, 2013 Free Software Foundation
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'xml)
(require 'w3-vars)

(defvar w3-last-parse-tree)             ;From w3-parse.el.

(defvar w3-fast-parse-tidy-program nil)

(defun w3-fast-parse-find-program (program var)
  (if (not (symbol-value var))
      (let (p)
	(mapc (lambda (d)
		(if (file-executable-p (setq p (expand-file-name program d)))
		    (set var p)))
	      exec-path)))
  (symbol-value var))

(defun w3-fast-parse-find-tidy-program ()
  (w3-fast-parse-find-program "tidy" 'w3-fast-parse-tidy-program))

(defvar base-object)

(defun w3-fast-parse-cleanup (tree)
  (let* (node tag attrs content btdt)
    (while (setq node (car tree))
      (setq tree (cdr tree))
      (if (stringp node)
	  nil 				; Do nothing
	(setq tag (xml-node-name node)
	      attrs (xml-node-attributes node)
	      content (xml-node-children node))
	(cond
	 ((eq tag 'base)
	  (setq base-object (cdr-safe (or (assq 'src attrs) (assq 'href attrs)))))
	 ((setq btdt
		(or (assq 'src attrs)
		    (assq 'background attrs)
		    (assq 'codebase attrs)
		    (assq 'href attrs)
		    (assq 'action attrs)))
	  (setcdr btdt (url-expand-file-name (cdr btdt) base-object))
	  (setq btdt (if (url-have-visited-url (cdr btdt))
			 ":visited"
		       ":link"))
	  (if (assq 'class attrs)
	      (setcdr (assq 'class attrs)
		      (cons btdt (cdr (assq 'class attrs))))
	    (setf (nth 1 node)
		  (cons (cons 'class (list btdt)) attrs))))
	 (t
	  nil))
	(w3-fast-parse-cleanup content)))))

(defun w3-fast-parse-buffer (&optional buff)
  "Parse contents of BUFF as HTML.
BUFF defaults to the current buffer.
Destructively alters contents of BUFF.
Returns a data structure containing the parsed information."
  (if (not w3-setup-done) (w3-do-setup))
  (let ((tree nil)
	(base-object nil)
	(tidy-program (w3-fast-parse-find-tidy-program)))
    (save-excursion
      (if buff
	  (set-buffer buff)
	(setq buff (current-buffer)))
      (buffer-disable-undo buff)
      (widen)
      (call-process-region (point-min) (point-max)
			   tidy-program
			   t (list buff nil) nil
			   "-quiet" "-asxml" "-clean")
      (setq tree (xml-parse-region (point-min) (point-max) buff))
      (setq w3-last-parse-tree
	    (list
	     (list '*document
		   nil
		   tree)))
      (w3-fast-parse-cleanup w3-last-parse-tree)
      w3-last-parse-tree)))

(provide 'w3-fast-parse)
