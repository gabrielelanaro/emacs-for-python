;;; w3-imap.el --- Imagemap functions

;; Copyright (c) 1996-1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: legoscia $
;; Created: $Date: 2006/10/12 21:32:16 $
;; Keywords: hypermedia

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

(require 'w3-vars)
(require 'url)
(require 'url-handlers)

(eval-and-compile
  (require 'widget))

(eval-when-compile
  (defmacro x-coord (pt) (list 'aref pt 0))
  (defmacro y-coord (pt) (list 'aref pt 1)))

;; (defun w3-point-in-rect (point coord1 coord2 &rest _ignore)
;;   "Return t iff POINT is within a rectangle defined by COORD1 and COORD2.
;; All arguments are vectors of [X Y] coordinates."
;;   ;; D'uhhh, this is hard.
;;   (and (>= (x-coord point) (x-coord coord1))
;;        (<= (x-coord point) (x-coord coord2))
;;        (>= (y-coord point) (y-coord coord1))
;;        (<= (y-coord point) (y-coord coord2))))

;; (defun w3-point-in-circle (point coord1 coord2 &rest _ignore)
;;   "Return t iff POINT is within a circle defined by COORD1 and COORD2.
;; All arguments are vectors of [X Y] coordinates."
;;   ;; D'uhhh, this is (barely) slightly harder.
;;   (let (radius1 radius2)
;;     (setq radius1 (+
;; 		   (*
;; 		    (- (y-coord coord1) (y-coord coord2))
;; 		    (- (y-coord coord1) (y-coord coord2)))
;; 		   (*
;; 		    (- (x-coord coord1) (x-coord coord2))
;; 		    (- (x-coord coord1) (x-coord coord2)))
;; 		   )
;; 	  radius2 (+
;; 		   (*
;; 		    (- (y-coord coord1) (y-coord point))
;; 		    (- (y-coord coord1) (y-coord point)))
;; 		   (*
;; 		    (- (x-coord coord1) (x-coord point))
;; 		    (- (x-coord coord1) (x-coord point)))
;; 		   )
;; 	  )
;;     (<= radius2 radius1)))

;; A polygon is a vector
;; poly[0] = # of sides
;; poly[1] = # of sides used
;; poly[2] = vector of X coords
;; poly[3] = vector of Y coords

(defsubst w3-image-poly-nsegs (p)
  (aref p 0))

(defsubst w3-image-poly-used-segs (p)
  (aref p 1))

(defsubst w3-image-poly-x-coords (p)
  (aref p 2))

(defsubst w3-image-poly-y-coords (p)
  (aref p 3))

(defsubst w3-image-poly-x-coord (p n)
  (aref (w3-image-poly-x-coords p) n))

(defsubst w3-image-poly-y-coord (p n)
  (aref (w3-image-poly-y-coords p) n))

(defun w3-image-poly-alloc (n)
  (if (< n 3)
      (error "w3-image-poly-alloc: invalid number of sides (%d)" n))

  (vector n 0 (make-vector n nil) (make-vector n nil)))

(defun w3-image-poly-assign (p x y)
  (if (>= (w3-image-poly-used-segs p) (w3-image-poly-nsegs p))
      (error "w3-image-poly-assign: out of space in the w3-image-polygon"))
  (aset (w3-image-poly-x-coords p) (w3-image-poly-used-segs p) x)
  (aset (w3-image-poly-y-coords p) (w3-image-poly-used-segs p) y)
  (aset p 1 (1+ (w3-image-poly-used-segs p))))

(defun w3-image-ccw (p0 p1 p2)
  (let (dx1 dx2 dy1 dy2 retval)
    (setq dx1 (- (x-coord p1) (x-coord p0))
	  dy1 (- (y-coord p1) (y-coord p0))
	  dx2 (- (x-coord p2) (x-coord p0))
	  dy2 (- (y-coord p2) (y-coord p0)))
    (cond
     ((> (* dx1 dy2) (* dy1 dx2))
      (setq retval 1))
     ((< (* dx1 dy2) (* dy1 dx2))
      (setq retval -1))
     ((or (< (* dx1 dx2) 0)
	  (< (* dy1 dy2) 0))
      (setq retval -1))
     ((< (+ (* dx1 dx1) (* dy1 dy1))
	 (+ (* dx2 dx2) (* dy2 dy2)))
      (setq retval 1))
     (t
      (setq retval 0)))
    retval))

(defun w3-image-line-intersect (l1 l2)
  (and (<= (* (w3-image-ccw (car l1) (cdr l1) (car l2))
	      (w3-image-ccw (car l1) (cdr l1) (cdr l2))) 0)
       (<= (* (w3-image-ccw (car l2) (cdr l2) (car l1))
	      (w3-image-ccw (car l2) (cdr l2) (cdr l1))) 0)))

(defun w3-point-in-poly (point &rest pgon)
  "Return t iff POINT is within a polygon defined by the list of points PGON.
All arguments are either vectors of [X Y] coordinates or lists of such
vectors."
  ;; Right now, this fails on some points that are right on a line segment
  ;; but it works for everything else (I think)
  (if (< (length pgon) 3)
      ;; Malformed polygon!!!
      nil
    (let ((p (w3-image-poly-alloc (length pgon)))
	  (hitcount 0)
	  (i 0)
	  (ip1 0)
	  (l1 nil)
	  (l2 (cons (vector (x-coord point) (1+ (y-coord point)))
		    (vector (x-coord point) (y-coord point))))
	  )
      (while pgon
	(w3-image-poly-assign p (x-coord (car pgon)) (y-coord (car pgon)))
	(setq pgon (cdr pgon)))
      (while (< i (w3-image-poly-nsegs p))
	;; Check for wraparound
	(setq ip1 (1+ i))
	(if (= ip1 (w3-image-poly-nsegs p))
	    (setq ip1 0))

	(setq l1 (cons (vector (w3-image-poly-x-coord p i)
			       (w3-image-poly-y-coord p i))
		       (vector (w3-image-poly-x-coord p ip1)
			       (w3-image-poly-y-coord p ip1))))

	(if (w3-image-line-intersect l1 l2)
	    (setq hitcount (1+ hitcount)))
	(setq i (1+ i)))
      (= 1 (% hitcount 2)))))

;; (defun w3-point-in-default (_point &rest _ignore)
;;   t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun w3-point-in-map (point map &optional alt-text)
  (let (func args done cur default slot)
    (setq slot (if alt-text 3 2))
    (while (and map (not done))
      (setq cur (car map)
	    func (intern-soft (format "w3-point-in-%s" (aref cur 0)))
	    args (aref cur 1)
	    done (and func (fboundp func) (apply func point args))
	    map (cdr map))
      (if (equal (aref cur 0) "default")
	  (setq default (aref cur slot)
		done nil)))
    (cond
     ((and done (aref cur 2)) ; Found a link
      (if alt-text
	  (or (aref cur 3) (aref cur 2))
	(aref cur slot)))
     (default
       default)
     (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular image stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-allowed-image-types
  (mapcar (function (lambda (x) (list (car x)))) w3-image-mappings))
(defvar w3-image-size-restriction nil)

(defmacro w3-image-cached-p (href)
  "Return non-nil iff HREF is in the image cache."
  `(cdr-safe (assoc ,href w3-graphics-list)))

(defun w3-image-loadable-p (href force)
  (or force
      (let ((attribs (condition-case nil (url-file-attributes href)
		       (error nil))))
        (and attribs
             ;; this is clearly an error: `file-attributes' returns
             ;; the permissions string as the 8th element, not a mime type!
             ;; (assoc (nth 8 attribs) w3-allowed-image-types)
             (or (null w3-image-size-restriction)
                 (and (<= (nth 7 attribs) 0)
                      (or (not (numberp w3-image-size-restriction))
                          (<= (nth 7 attribs) w3-image-size-restriction))))))))

(defmacro w3-image-invalid-glyph-p (glyph)
  `(if (vectorp glyph)
       (progn
	 (or (null (aref ,glyph 0))
	     (null (aref ,glyph 2))
	     (equal (aref ,glyph 2) "")))
     (not (eq 'image (car-safe glyph)))))

;; data structure in storage is a vector
;; if (href == t) then no action should be taken
;; [ type coordinates href (hopefully)descriptive-text]


(provide 'w3-imap)
