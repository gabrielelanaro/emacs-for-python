;;; w3-props.el --- Additional text property stuff

;; Copyright (c) 1996, 1997, 2013 Free Software Foundation, Inc.

;; Author: $Author: fx $
;; Created: $Date: 2001/09/09 15:33:14 $
;; Keywords: faces

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

;; Additional text property functions.

;; The following three text property functions are not generally available (and
;; it's not certain that they should be) so they are inlined for speed.
;; The case for `fillin-text-property' is simple; it may or not be generally
;; useful.  (Since it is used here, it is useful in at least one place.;-)
;; However, the case for `append-text-property' and `prepend-text-property' is
;; more complicated.  Should they remove duplicate property values or not?  If
;; so, should the first or last duplicate item remain?  Or the one that was
;; added?  In our implementation, the first duplicate remains.

;;; Code:

(defsubst w3-fillin-text-property (start end setprop markprop value
					 &optional object)
  "Fill in one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to put where none are
already in place.  Therefore existing property values are not overwritten.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((start (text-property-any start end markprop nil object)) next)
    (while start
      (setq next (next-single-property-change start markprop object end))
      (put-text-property start next setprop value object)
      (put-text-property start next markprop value object)
      (setq start (text-property-any next end markprop nil object)))))

(defsubst w3-props-unique (list)
  "Uniquify LIST, deleting elements using `delq'.
Return the list with subsequent duplicate items removed by side effects."
  (let ((list list))
    (while list
      (setq list (setcdr list (delq (car list) (cdr list))))))
  list)

;; A generalisation of `facemenu-add-face' for any property, but without the
;; removal of inactive faces via `facemenu-discard-redundant-faces' and special
;; treatment of `default'.  Uses `unique' to remove duplicate property values.
(defsubst prepend-text-property (start end prop value &optional object)
  "Prepend to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to prepend to the value
already in place.  The resulting property values are always lists, and unique.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property
       start next prop
       (w3-props-unique (append val (if (listp prev) prev (list prev))))
       object)
      (setq start next))))

(defsubst append-text-property (start end prop value &optional object)
  "Append to one property of the text from START to END.
Arguments PROP and VALUE specify the property and value to append to the value
already in place.  The resulting property values are always lists, and unique.
Optional argument OBJECT is the string or buffer containing the text."
  (let ((val (if (listp value) value (list value))) next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
	    prev (get-text-property start prop object))
      (put-text-property
       start next prop
       (w3-props-unique (append (if (listp prev) prev (list prev)) val))
       object)
      (setq start next))))

(provide 'w3-props)
