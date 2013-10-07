;;; w3-hotindex.el --- Keywords for the hotlist

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Laurent Martelli <martelli@iie.cnam.fr>
;; Created: 1997/12/31

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO:
;;   patch w3-hot.el so that it removes hotindex entries.
;;   update w3-hotindex-key-list when removing entries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structure for hotindexes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (
;;  ("name of item1" "key1" "key2")
;; )  ; end of hotlist
;; Every "name of item" must be in the hotlist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(require 'w3-vars)
(require 'w3-hot)

;; --- non Hotindex specific definitions ---
;; It should probably go in an other file
(defun member-nocase (elt list)
  "Returns non-nil if ELT is a string that belongs to LIST case insensitively.
Actually, non-nil means the string as it is in LIST.
It won't work great if there are some non-string objects in LIST. I mean, 
it will raise an error."
  (setq elt (downcase elt))
  (while (and list (not (string= elt (downcase (car list)))))
    (setq list (cdr list)))
  (car list))
;; --- end of non Hotindex specific definitions ---


(defvar w3-hotindex ()
  "*The hotindex list. It must have the following format:
 ((NAME KEY1 KEY2 ...) ...)
 NAME is a string identifying the item. It must be in w3-hotlist.
 KEYn are strings which are keywords associated with the item.")

(defvar w3-hotindex-file (concat w3-configuration-directory "hotindex")
  "*The file where to save the HotIndex.")

(defvar w3-hotindex-key-list ()
  "A list of keys used in w3-hotindex")

(defun w3-hotindex-build-completion-key-list (key-list)
  "Build a list of keys suitable for use with completing-read."
  (mapcar (lambda (x) (list x))
	  key-list))

(defun w3-hotindex-build-key-list (hotindex)
  "(w3-hotindex-build-key-list HOTINDEX)
Returns a list of the keys appearing in HOTINDEX."
  (let (key-list key)
    (while hotindex
      ;; set key is to the list of keys of the current entry
      (setq key (cdr (car hotindex)))
      (while key
	(unless (member-nocase (car key) key-list)
	  (setq key-list (cons (car key) key-list)))
	(setq key (cdr key)))
      (setq hotindex (cdr hotindex)))
    key-list))

;;;###autoload
(defun w3-hotindex-add-key (name keyword)
  "*Add a keyword to an item in w3-hotindex. Completion is done
on the list of all keywords."
  (interactive (let ((completion-ignore-case t))
		 (list (completing-read "Entry: " w3-hotlist nil t)
		       (completing-read "Keyword: "
					(w3-hotindex-build-completion-key-list 
					 w3-hotindex-key-list)
					nil nil))))
  (let ((item (assoc name w3-hotindex)) key)
    (or item
	(progn	;; if the item does not exist, we create it
	  (setq item (list name))
	  (setq w3-hotindex (cons item w3-hotindex))))
    ;; If that key is already there, do not add it again
    ;; (The comparison is case-insensitive)
    (setq key (cdr item))
    (message "existant keys = %S" key)
    (while (and key (not (string= (downcase (car key)) (downcase keyword))))
      (setq key (cdr key)))
    ;; leading and trailing spaces should be eliminated before this.
    ;; Is there a function doing this ????
    (if key 
	(message "%s is already associated with this entry." keyword)
	(setcdr item (cons keyword (cdr item)))))
  (w3-hotindex-save nil)
  ;; rebuilds the list of keys
  ;; It would better to do this incrementally
  (setq w3-hotindex-key-list (w3-hotindex-build-key-list w3-hotindex)))

;;;###autoload
(defun w3-hotindex-rm-key (entry keyword)
  "*Remove a keyword from an item of w3-hotindex."
  (interactive (let ((entry nil)
		     (key-list nil)
		     (completion-ignore-case t))
		 (setq entry (completing-read "Entry: " w3-hotindex nil t))
		 (setq key-list (mapcar 
				 (lambda (x) (list x))
				 (cdr (assoc entry w3-hotindex))))
		 (list entry 
		       (completing-read "Keyword to remove: " key-list nil t))))
  (let ((item (delete keyword (assoc entry w3-hotindex))))
    ;; If there are no more keywords for this entry, remove it
    (if (null (cdr item))
	(setq w3-hotindex (delq item w3-hotindex))))
  (w3-hotindex-save nil)
  ;; rebuilds the list of keys
  ;; It would better to do this incrementally
  (setq w3-hotindex-key-list (w3-hotindex-build-key-list w3-hotindex))
  )

;;;###autoload
(defun w3-hotindex-rename-entry (old new)
  "Renames an entry in the HotIndex. Intended to be called from 
w3-hotlist-rename-entry. OLD should equal the entry to be renamed.
Case is therefore important."
  (let ((entry (assoc old w3-hotindex)))
    (if entry
	(progn (setcar entry new)
	       (w3-hotindex-save nil)))))

;;;###autoload
(defun w3-hotindex-delete-entry (title)
  "Deletes an entry in the HotIndex. Intended to be called from 
w3-hotlist-delete. OLD should equal the entry to be deleted.
Case is therefore important."
  (let ((entry (assoc title w3-hotindex)))
    (if entry
	(progn (setq w3-hotindex (delq entry w3-hotindex))
	       (setq w3-hotindex-key-list (w3-hotindex-build-key-list 
					   w3-hotindex-key-list))
	       (w3-hotindex-save nil)))))

;;;###autoload
(defun w3-hotindex-query (key)
  "Query the HotIndex for KEY."
  (interactive (list (let ((completion-ignore-case t))
		 (completing-read "Key: " (w3-hotindex-build-completion-key-list 
					   w3-hotindex-key-list)
				  nil t))))
  (let ((index w3-hotindex) result)
    (message "SEARCHING IN %S" index)
    (message "LOOKING FOR %S" key)
    (while index
      (if (member-nocase key (cdr (car index)))
	  (progn
	    (push (assoc-string (caar index) w3-hotlist t) result)
	    (message "MATCH in %S" (car index))
	    (message "ADDING %S" (assoc-string (caar index) w3-hotlist t)))
	(message "no match in %S" (car index)))
      (setq index (cdr index)))
    (let ((w3-hotlist result)
	  (w3-reuse-buffers 'no))
      (w3-hotlist-view))))

(defvar print-readably)

(defun w3-hotindex-save (filename)
  "*Save the index structure in filename. If filename is nil, 
save into w3-configuration-directory/hotindex."
  (interactive "i")
  (let ((output-buffer 
	 (find-file-noselect (or filename w3-hotindex-file)))
	output-marker)
    (with-current-buffer output-buffer
      ;; Delete anything that is in the file
      (delete-region (point-min) (point-max))
      (setq output-marker (point-marker))
      (let ((print-readably t)
	    (print-escape-newlines t)
	    (standard-output output-marker))
	(princ ";; W3 HotIndex\n")
	(princ ";; ===========\n")
	(princ "(setq-default w3-hotindex '")
	(prin1 w3-hotindex)
	(princ ")\n;; ==================\n")
	(princ ";; End of W3 HotIndex\n")))
    (set-marker output-marker nil)
    (with-current-buffer output-buffer
      (save-buffer))
    ))

(defun w3-hotindex-check ()
  "Checks that the entries of w3-hotindex are in w3-hotlist.
Raises an error if some entries are unresolved."
  (let ((index w3-hotindex) unresolved)
    (while index
      (unless (assoc-string (caar index) w3-hotlist t)
	(setq unresolved (cons (caar index) unresolved)))
      (setq index (cdr index)))
    (if unresolved
	(error "Unresolved entries found in w3-hotindex : %S" unresolved)))
  )

(defun w3-hotindex-load ()
  "Load the file containing the hotindex, and updates w3-hotindex-key-list."
  (interactive)
  (load w3-hotindex-file t)
  (w3-hotindex-check)
  (setq w3-hotindex-key-list (w3-hotindex-build-key-list w3-hotindex))
  )

(w3-hotlist-refresh)
(w3-hotindex-load)

(provide 'w3-hotindex)
