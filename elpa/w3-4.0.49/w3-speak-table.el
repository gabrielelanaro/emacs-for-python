;;; w3-speak-table.el --- Speak W3 tables

;; Copyright (c) 2013  Free Software Foundation, Inc.

;; Author: Thierry Emery <Thierry.Emery@nmu.alcatel.fr>
;;         T.V. Raman <raman@Adobe.COM>

;; This file is not part of GNU Emacs, but the same permissions apply.
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

(require 'w3-keymap)
(require 'w3-vars)

(eval-when-compile
  (require 'cl)
  (load "cl-extra"))

;;{{{  inline functions

(defsubst w3-table-inside-table-display-p ()
  "Indicates by looking at `w3-table-structure' whether (point) is inside a W3 table"
  (loop for table-info in w3-table-structure
    if (and (>= (point) (car table-info))
	    (<= (point) (cadr table-info)))
    return t))

(defmacro w3-within-cell (cell-info table-info &rest forms)
  "Enables to recursively enter the current cell using `extract-rectangle'
using CELL-INFO and TABLE-INFO and process FORMS inside it (for instance to process subtables)"
  `(let* ((cell-row (w3-cell-info-row ,cell-info))
	    (cell-col (w3-cell-info-column ,cell-info))
	    (cell-beg (w3-cell-info-start ,cell-info))
	    (cell-end (w3-cell-info-end ,cell-info))
	    (cell-contents (extract-rectangle cell-beg cell-end))
	    (cell-x (count-lines (save-excursion (goto-char cell-beg) (beginning-of-line) (point))
				 (save-excursion (beginning-of-line) (point))))
	    (current-col (current-column))
	    (cell-y (save-excursion (forward-line (- cell-x))
				    (move-to-column current-col)
				    (- (point) cell-beg)))
	    cell-table-structure)
       ;; really inside cell ?
       (when (and ,cell-info
		  (>= (point) cell-beg)
		  (<= (point) cell-end))
	 ;; find current subtables structure
	 (loop for subtable in (w3-table-info-subtables ,table-info)
	   if (and (= cell-row (car subtable))
		   (= cell-col (cadr subtable)))
	   do (setq cell-table-structure (cons (cddr subtable) cell-table-structure)))
	 (with-temp-buffer
	   (mapc (lambda (s) (insert s ?\n)) cell-contents)
	   ;; remove end of line padding
	   (when cell-table-structure
	     (goto-char (point-min))
	     (end-of-line)
	     (while (not (eobp))
	       (delete-horizontal-space)
	       (forward-line 1)
	       (end-of-line)))
	   ;; reposition
	   (goto-char (point-min))
	   (forward-line cell-x)
	   (move-to-column cell-y)
	   (setq w3-table-structure (nreverse cell-table-structure))
	   ,@forms))))

(put 'w3-within-cell 'lisp-indent-function 2)
(put 'w3-within-cell 'edebug-form-spec '(sexp sexp &rest form))

(defmacro w3-table-compute-relative-movement (&rest forms)
  "Record a movement done by &rest FORMS (e.g. inside a temporary buffer)
and return it as (horizontal-offset . vertical-offset)"
  `(let ((origin-line-beg (save-excursion (beginning-of-line) (point)))
	   (origin-char-col (current-column)))
       ,@forms
       (cons (- (current-column) origin-char-col)
	     (let* ((new-line-beg (save-excursion (beginning-of-line) (point)))
		    (line-diff (count-lines new-line-beg origin-line-beg)))
	       (if (< new-line-beg origin-line-beg)
		   (- line-diff)
		 line-diff)))))
(put 'w3-table-compute-relative-movement 'lisp-indent-function 0)
(put 'w3-table-compute-relative-movement 'edebug-form-spec '(&rest form))

(defsubst w3-table-redo-relative-movement (movement)
  "Redo a movement indicated as (horizontal-offset . vertical-offset)"
  (when movement
    (let ((to-col (+ (current-column) (car movement)))
	  (vertical (cdr movement)))
      (forward-line vertical)
      (move-to-column to-col))))

(defmacro w3-table-move-within-cell (at-depth cell-info move-function)
  "Move within a cell (in a temporary buffer) and reflect the same movement
in the containing table in the original buffer"
  (declare (indent 2) (debug t))
  `(if (null ,cell-info)
       (error "Not inside a W3 cell")
     (let (table-movement)
       (w3-within-cell ,cell-info table-info
                       (setq table-movement
                             (w3-table-compute-relative-movement
                              (funcall ,move-function (1- ,at-depth)))))
       (w3-table-redo-relative-movement table-movement))))

(defmacro w3-table-move-within-subtable (at-depth cell-info move-function)
  "Move within a subtable (in a temporary buffer) and reflect the same movement
in the containing table in the original buffer"
  (declare (indent 2) (debug t))
  `(if (null ,cell-info)
	 (error "Not inside a W3 cell")
       (let ((subtable-info (w3-cell-info-current-subtable ,cell-info))
	     table-movement)
	 (if (null subtable-info)
	     (error "Not inside a W3 table")
	   (w3-within-cell ,cell-info table-info
			   (setq table-movement
				 (w3-table-compute-relative-movement
				  (funcall ,move-function (1- ,at-depth) subtable-info))))
	   (w3-table-redo-relative-movement table-movement)))))

;;}}}
;;{{{  find a table

(defun w3-find-table (&optional at-depth)
  "Search forward for a table, go to its start and return (start . end)"
  (interactive "p")
  (let* ((table-info (w3-table-info 1 t))
	 (cell-info (and table-info (w3-table-info-current-cell table-info))))
    (if (and (numberp at-depth) (> at-depth 1))
	(if (null cell-info)
	    (error "Not inside a W3 table cell")
	  (w3-table-move-within-cell at-depth cell-info
				     'w3-find-table))
      (if (or (not w3-table-structure)
	      (> (point) (car (car w3-table-structure))))
	  (error "No other W3 table")
	(let (table-found)
	  (loop for table-info in w3-table-structure
	    until (> (point) (car table-info))
	    do (setq table-found table-info)
	    finally return (progn
			     (goto-char (car table-found))
			     (cons (car table-found) (cdr table-found)))))))))

;;}}}
;;{{{ table info

;;; Return table info if inside a table cell.
(defstruct w3-table-info
  start					; starting point in buffer
  end					; end point in buffer
  subtables				; w3-table-structure extract
  current-cell				; `w3-cell-info' struct
  rows					; number of rows
  columns				; number of columns
  row-heights
  column-widths
  rowspans
  colspans)

(defun w3-table-info (&optional to-depth noerror)
  "Give table info as a `w3-table-info' struct, limited to TO-DEPTH if it is a number.
TO-DEPTH = 0 means without current-cell info.
TO-DEPTH = n > 0 means without nth subtable info in the nth current-cell info.
If no table is found and NOERROR is nil, an error is signaled."
  (let ((origin (point))
	start end subtables dimensions)
    (loop for info in w3-table-structure
      if (and (>= origin (car info))
	      (<= origin (cadr info)))
      do (setq start (car info)
	       end (cadr info)
	       subtables (caddr info)
	       dimensions (cdddr info)))
    (if (not dimensions)
	(unless noerror
	  (error "Not inside a W3 table"))
      (let ((table-info (make-w3-table-info)))
	(setf (w3-table-info-start table-info)
	      start)
	(setf (w3-table-info-end table-info)
	      end)
	(setf (w3-table-info-subtables table-info)
	      subtables)
	(setf (w3-table-info-rows table-info)
	      (nth 0 dimensions))
	(setf (w3-table-info-columns table-info)
	      (nth 1 dimensions))
	(setf (w3-table-info-row-heights table-info)
	      (nth 2 dimensions))
	(setf (w3-table-info-column-widths table-info)
	      (nth 3 dimensions))
	(setf (w3-table-info-rowspans table-info)
	      (nth 4 dimensions))
	(setf (w3-table-info-colspans table-info)
	      (nth 5 dimensions))
	;; current cell info
	(when (or (null to-depth)
		  (and (numberp to-depth) (> to-depth 0)))
	  (setf (w3-table-info-current-cell table-info)
		(w3-cell-info table-info to-depth)))
	table-info))))

;;}}}
;;{{{  Location

(defun w3-table-current-row-number (&optional table-info)
  "Return spanless row number"
  (let* ((table-info (or table-info (w3-table-info 0)))
	 (start (w3-table-info-start table-info))
	 (line-count (1+ (count-lines (save-excursion (goto-char start) (beginning-of-line) (point))
				      (save-excursion (beginning-of-line) (point)))))
	 (num-rows (w3-table-info-rows table-info))
	 (row-heights (w3-table-info-row-heights table-info))
	 (row 0))
    (while (and (< row num-rows)
		(< (aref row-heights row) line-count))
      (setq line-count (- line-count (aref row-heights row))
	    row (1+ row)))
    (when (< row num-rows)
      (1+ row))))

(defun w3-table-current-column-number (&optional table-info)
  "Return spanless column number"
  (let* ((table-info (or table-info (w3-table-info 0)))
	 (char-col (current-column))
	 (num-cols (w3-table-info-columns table-info))
	 (col-widths (w3-table-info-column-widths table-info))
	 (col 0))
    (while (and (< col num-cols)
		(< (aref col-widths col) char-col))
      (setq char-col (- char-col (aref col-widths col) 1)
	    col (1+ col)))
    (when (< col num-cols)
      (1+ col))))

(defun w3-table-row-column-spans (&optional table-info)
  "Return spanning cell ((row . rowspan) . (column . colspan))"
  (let* ((table-info (or table-info (w3-table-info 0)))
	 (start (w3-table-info-start table-info))
	 (line-count (1+ (count-lines (save-excursion (goto-char start) (beginning-of-line) (point))
				      (save-excursion (beginning-of-line) (point)))))
	 (num-rows (w3-table-info-rows table-info))
	 (row-heights (w3-table-info-row-heights table-info))
	 (table-rowspans (w3-table-info-rowspans table-info))
	 (row 0)
	 (rowspan 0)
	 (char-col (current-column))
	 (num-cols (w3-table-info-columns table-info))
	 (col-widths (w3-table-info-column-widths table-info))
	 (table-colspans (w3-table-info-colspans table-info))
	 col colspan)
    ;; look for spanning cell origin row
    (while (and (< row num-rows)
		(> line-count 0))
      (setq row (+ row rowspan))
      (when (< row num-rows)
	(let ((row-rowspans (aref table-rowspans row))
	      (row-colspans (aref table-colspans row))
	      (row-char-col char-col))
	  (setq col 0
		colspan 0)
	  ;; look for spanning cell origin column for this candidate spanning cell origin row
	  (while (and (< col num-cols)
		      (> row-char-col -1))
	    (setq col (+ col colspan))
	    (when (< col num-cols)
	      (setq colspan (aref row-colspans col))
	      (loop for i from 0 to (1- colspan)
		do (setq row-char-col (- row-char-col (aref col-widths (+ col i)) 1)))))
	  ;; take into account row-span from this candidate spanning cell origin column
	  (setq rowspan (if (< col num-cols)
			    (aref row-rowspans col)
			  1))
	  (loop for i from 0 to (1- rowspan)
	    do (setq line-count (- line-count (aref row-heights (+ row i))))))))
    
    (cons
     (and (< row num-rows) (cons (1+ row) rowspan))
     (and (< col num-cols) (cons (1+ col) colspan)))))

;;}}}
;;{{{  cell info

;;; Return cell info if inside a table cell.
(defstruct w3-cell-info
  row					; row number of this cell in its table
  column				; column number of this cell in its table
  rowspan
  colspan
  start					; starting point of cell contents in buffer (for `extract-rectangle')
  end					; end point of cell contents in buffer (for `extract-rectangle')
  current-subtable			; `w3-table-info' struct
  )

(defun w3-cell-info (&optional table-info to-depth)
  "If inside a table, tell which cell it is in, its rowspan, colspan,
start and end points, and its current subtable as a `w3-cell-info' struct.
NB: row and col start from 1
    beg and end delimit the interior of the cell, so they can be passed on
    to `extract-rectangle' in order to get cell contents."
  (let* ((origin (point))
	 (table-info (or table-info (w3-table-info 0)))
	 (table-row-col-spans (w3-table-row-column-spans table-info))
	 (table-row (caar table-row-col-spans))
	 (rowspan (cdar table-row-col-spans))
	 (table-col (cadr table-row-col-spans))
	 (colspan (cddr table-row-col-spans)))
    (when (and table-row table-col)
      (let* ((cell-info (make-w3-cell-info))
	     (start (w3-table-info-start table-info))
	     (table-row-heights (w3-table-info-row-heights table-info))
	     (table-col-widths (w3-table-info-column-widths table-info))
	     (table-row-index (1- table-row))
	     (table-col-index (1- table-col))
	     cell-beg
	     (cell-beg-offset 0)
	     cell-end
	     (cell-end-offset 0))
	;; determine beg
	(goto-char start)
	(loop for i from 0 to (1- table-row-index)
	      do (next-line (aref table-row-heights i)))
	(loop for i from 0 to (1- table-col-index)
	      do (setq cell-beg-offset (+ cell-beg-offset (aref table-col-widths i) 1)))
	(move-to-column (+ (current-column) cell-beg-offset))
	(save-excursion
	  (next-line 1)
	  (move-to-column (1+ (current-column)))
	  (setq cell-beg (point)))
	;; determine end
	(loop for i from 0 to (1- rowspan)
	      do (next-line (aref table-row-heights (+ table-row-index i))))
	(loop for i from 0 to (1- colspan)
	      do (setq cell-end-offset
		       (+ cell-end-offset (aref table-col-widths (+ table-col-index i)) 1)))
	(move-to-column (+ (current-column) cell-end-offset))
	(next-line -1)
	(setq cell-end (point))
	;; result
	(goto-char origin)
	(setf (w3-cell-info-row cell-info) table-row)
	(setf (w3-cell-info-column cell-info) table-col)
	(setf (w3-cell-info-rowspan cell-info) rowspan)
	(setf (w3-cell-info-colspan cell-info) colspan)
	(setf (w3-cell-info-start cell-info) cell-beg)
	(setf (w3-cell-info-end cell-info) cell-end)
	;; if inside cell, current subtable info (recursive call to w3-table-info)
	(when (and (or (null to-depth)
		       (and (numberp to-depth) (> to-depth 1)))
		   (<= cell-beg (point)) (<= (point) cell-end))
	  (w3-within-cell cell-info table-info
			  (when w3-table-structure
			    (setf (w3-cell-info-current-subtable cell-info)
				  (w3-table-info (and (numberp to-depth) (1- to-depth)) t)))))
	cell-info))))

;;}}}
;;{{{  extracting table elements

(defun w3-table-this-cell-contents (&optional at-depth table-info)
  "Return formatted contents of this cell as a list if strings.
Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (null cell-info)
	(error "Not inside a W3 table cell")
      (if (and (numberp at-depth) (> at-depth 1))
	  (let ((subtable-info (w3-cell-info-current-subtable cell-info)))
	    (if (null subtable-info)
		(error "Not inside a W3 table")
	      (w3-within-cell cell-info table-info
			      (w3-table-this-cell-contents (1- at-depth) subtable-info))))
	(mapconcat 'identity
		   (extract-rectangle (w3-cell-info-start cell-info)
				      (w3-cell-info-end cell-info))
		   "\n")))))

(defun w3-table-speak-this-cell-info (&optional at-depth)
  "Speak coordinates of current table cell.
Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let*
      ((table-info (w3-table-info at-depth))
       (cell-info (w3-table-info-current-cell table-info)))
    (message "Row %s Column %s of a %s by %s table %s"
             (w3-cell-info-row cell-info)
             (w3-cell-info-column cell-info)
             (w3-table-info-rows table-info)
             (w3-table-info-columns table-info)
             (if at-depth
                 (format " at nesting level %s" at-depth)
               ""))))

(defun w3-table-focus-on-this-cell (&optional at-depth)
  "Focus on current cell --optional argument at-depth
specifies nesting level. Focusing on a cell results in its
contents being displayed in a separate buffer in W3 mode.
This is useful to navigate pages that use a single table
cell for a newspaper style column"
  (interactive "p")
  (setq at-depth (or at-depth 1))
  (let ((contents (w3-table-this-cell-contents at-depth))
        (buffer (get-buffer-create
                 (format "Cell-%s" (buffer-name))))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)
      (w3-mode)
      (insert contents)
      (goto-char (point-min))
      (w3-resurrect-hyperlinks)
      (w3-resurrect-images))
    (switch-to-buffer buffer)))


(defun w3-table-speak-this-cell (&optional at-depth)
  "Speak contents of current table cell.
Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let ((contents (w3-table-this-cell-contents at-depth)))
  (dtk-speak contents)))

;;}}}
;;{{{  Table navigation

(defun w3-table-move-to-table-start (&optional at-depth table-info)
  "If inside a table, move to its top left corner.
Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-table-start)
      (goto-char (w3-table-info-start table-info))
      (when (and (interactive-p)
		 (featurep 'emacspeak))
	(emacspeak-auditory-icon 'large-movement)
	(emacspeak-speak-line)))))

(defun w3-table-move-to-table-end (&optional at-depth table-info)
  "If inside a table, move to its bottom right corner.
Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-table-end)
      (goto-char (1- (w3-table-info-end table-info)))
      (when (and (interactive-p)
		 (featurep 'emacspeak))
	(emacspeak-auditory-icon 'large-movement)
	(emacspeak-speak-line)))))

(defun w3-table-move-to-beginning-of-previous-table-row (&optional at-depth table-info)
  "Move to previous table row. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-beginning-of-previous-table-row)
      (when (= 1 (w3-cell-info-row cell-info))
	(error "First row"))
      (goto-char (w3-cell-info-start cell-info))
      (forward-line -2)
      (beginning-of-line)
      (forward-char 1)
      (when (featurep 'emacspeak)
	(dtk-speak (w3-table-this-cell-contents at-depth))
	(emacspeak-auditory-icon 'select-object)))))

(defun w3-table-move-to-beginning-of-next-table-row (&optional at-depth table-info)
  "Move to next table row. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-beginning-of-next-table-row)
      (when (= (w3-table-info-rows table-info) (w3-cell-info-row cell-info))
	(error "Last row"))
      (goto-char (w3-cell-info-end cell-info))
      (forward-line 2)
      (beginning-of-line)
      (forward-char 1)
      (when (featurep 'emacspeak)
	(dtk-speak (w3-table-this-cell-contents at-depth))
	(emacspeak-auditory-icon 'select-object)))))

(defun w3-table-move-to-previous-table-row (&optional at-depth table-info)
  "Move to previous table row. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-previous-table-row)
      (let ((char-col (current-column)))
	(when (= 1 (w3-cell-info-row cell-info))
	  (error "First row"))
	(goto-char (w3-cell-info-start cell-info))
	(forward-line -2)
	(move-to-column char-col)
	(setq cell-info (w3-cell-info table-info))
	(goto-char (w3-cell-info-start cell-info))
	(when (featurep 'emacspeak)
	  (dtk-speak (w3-table-this-cell-contents at-depth))
	  (emacspeak-auditory-icon 'select-object))))))

(defun w3-table-move-to-next-table-row (&optional at-depth table-info)
  "Move to next table row. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-next-table-row)
      (let ((char-col (current-column)))
	(when (= (w3-table-info-rows table-info) (w3-cell-info-row cell-info))
	  (error "Last row"))
	(goto-char (w3-cell-info-end cell-info))
	(forward-line 1)
	(move-to-column char-col)
	(setq cell-info (w3-cell-info table-info))
	(goto-char (w3-cell-info-start cell-info))
	(when (featurep 'emacspeak)
	  (dtk-speak (w3-table-this-cell-contents at-depth))
	  (emacspeak-auditory-icon 'select-object))))))

(defun w3-table-move-to-previous-table-column (&optional at-depth table-info)
  "Moves to the start of the previous table column.
Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-previous-table-column)
      (let* ((column (w3-table-current-column-number table-info))
	     (column-index (if (numberp column)
			       (1- column)
			     (w3-table-info-columns table-info)))
	     (position 0)
	     (widths (w3-table-info-column-widths table-info))
	     (c  1))
	(when (= 0 column-index)
	  (error "First column"))
	(loop for w across widths
	  while  (< c column-index)
	  do
	  (incf c)
	  (incf position (1+ w)))
	(beginning-of-line)
	(move-to-column (1+ position))
	(when (featurep 'emacspeak)
	  (dtk-speak (w3-table-this-cell-contents at-depth))
	  (emacspeak-auditory-icon 'select-object))))))

(defun w3-table-move-to-next-table-column (&optional at-depth table-info)
  "Move to next column. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-next-table-column)
      (let* ((current (current-column))
	     (column (w3-table-current-column-number table-info))
	     (column-index (if (numberp column)
			       (1- column)
			     (w3-table-info-columns table-info)))
	     (check 0)
	     (widths (w3-table-info-column-widths table-info)))
	(when (= (1- (w3-table-info-columns table-info)) column-index)
	  (error "Last column"))
	(loop for w across widths
	  until (> check current)
	  do
	  (setq check (+ check w 1 )))
	(beginning-of-line)
	(move-to-column (1+ check))
	(when (featurep 'emacspeak)
	  (dtk-speak (w3-table-this-cell-contents at-depth))
	  (emacspeak-auditory-icon 'select-object))))))

(defun w3-table-move-to-top-of-table-column (&optional at-depth table-info)
  "Move to top of current column. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(w3-table-move-within-subtable at-depth cell-info
				       'w3-table-move-to-top-of-table-column)
      (let ((table-start nil)
	    (column (w3-table-current-column-number table-info))
	    (motion 0)
	    (widths (w3-table-info-column-widths table-info)))
	(set-mark (point))
	(save-excursion
	  (w3-table-move-to-table-start table-info)
	  (setq table-start (point)))
	(goto-char  table-start)
	(forward-line 1)
	(loop for c from 1 to (1- column)
	  do
	  (incf motion  (1+ (aref widths (1- c)))))
	(move-to-column (1+ motion))
	(when (featurep 'emacspeak)
	  (emacspeak-auditory-icon 'large-movement)
	  (dtk-speak (w3-table-this-cell-contents at-depth)))))))

;;}}}
;;{{{  Column browsing

;;;###autoload
(defun w3-table-speak-current-table-column (&optional at-depth table-info)
  "Speak current table column. Prefix arg can be used to specify the desired table nesting."
  (interactive "p")
  (let* ((table-info (or table-info (w3-table-info at-depth)))
	 (cell-info (w3-table-info-current-cell table-info)))
    (if (and (numberp at-depth) (> at-depth 1))
	(if (null cell-info)
	    (error "Not inside a W3 cell")
	  (let ((subtable-info (w3-cell-info-current-subtable cell-info)))
	    (if (null subtable-info)
		(error "Not inside a W3 table")
	      (w3-within-cell cell-info table-info
                'w3-table-speak-current-table-column))))
      (let ((table-start nil)
	    (table-end nil)
	    (column (w3-table-current-column-number table-info))
	    (top-left nil)
	    (bottom-right nil)
	    (widths (w3-table-info-column-widths table-info)))
	(save-excursion
	  (save-restriction
	    (w3-table-move-to-table-start table-info)
	    (setq table-start (point))
	    (w3-table-move-to-table-end table-info)
	    (setq table-end (point))
	    (narrow-to-region table-start table-end)
	    (goto-char table-start)
	    (loop for c from 1 to (1- column)
                  do
                  (forward-char (1+ (aref widths (1- c)))))
	    (setq top-left (point))
	    (goto-char table-end)
	    (beginning-of-line)
	    (loop for c from 1 to column
                  do
                  (forward-char (1+ (aref widths (1- c)))))
	    (setq bottom-right (point))))
	(emacspeak-speak-rectangle top-left bottom-right)))))

;;}}}
;;{{{  bind them to useful keys

(defvar emacspeak-prefix)

;;;###autoload
(defun w3-table-setup-keys ()
  "Setup emacspeak table browsing keys in w3 mode"
  (let ((key (make-vector 1 (aref emacspeak-prefix 0))))
    (define-key w3-mode-map ","
      'w3-table-focus-on-this-cell)
    (define-key w3-mode-map "." 'w3-table-speak-this-cell)
    (define-key w3-mode-map
      (concat emacspeak-prefix "=")
      'w3-table-move-to-top-of-table-column)
    (define-key w3-mode-map "=" 'w3-table-speak-this-cell-info)
    (define-key w3-mode-map
      (concat emacspeak-prefix ".")
      'w3-table-speak-this-cell)
    (define-key w3-mode-map
      (concat emacspeak-prefix "<")
      'w3-table-move-to-table-start)
    (define-key w3-mode-map
      (concat emacspeak-prefix ">")
      'w3-table-move-to-table-end)
    (define-key w3-mode-map
      (concatenate 'vector key "-")
      'w3-table-move-to-beginning-of-previous-table-row)
    (define-key w3-mode-map
      (concatenate 'vector key "+")
      'w3-table-move-to-beginning-of-next-table-row)
    (define-key w3-mode-map
      (concatenate 'vector key '[up])
      'w3-table-move-to-previous-table-row)
    (define-key w3-mode-map
      "|"
      'w3-table-speak-current-table-column)
    (define-key w3-mode-map
      (concatenate 'vector key '[down])
      'w3-table-move-to-next-table-row)
    (define-key w3-mode-map
      (concatenate 'vector key '[left])
      'w3-table-move-to-previous-table-column)
    (define-key w3-mode-map
      (concatenate 'vector key '[right])
      'w3-table-move-to-next-table-column)
    ))

(add-hook 'w3-mode-hook 'w3-table-setup-keys)

;;}}}
(provide 'w3-structure)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
