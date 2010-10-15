(make-variable-buffer-local 'loc-changes-alist)
(defvar loc-changes-alist '()
  "A buffer-local association-list (alist) of line numbers and
their corresponding markers in the buffer. The 'key' is the line number; the value
the marker"
  )


(defun loc-changes-goto-line (line-number &optional column-number)
  "Position `point' at LINE-NUMBER of the current buffer. If
COLUMN-NUMBER is given, position `point' at that column just
before that column number within the line. Note that the beginning of
the line starts at column 0, so the column number display will be one less
than COLUMN-NUMBER. For example COLUMN-NUMBER 1 will set before the first
column on the line and show 0. 

The Emacs `goto-line' docstring says it is the wrong thing to use
that function in a Lisp program. So here is something that I
proclaim is okay to use in a Lisp program."
  (unless (wholenump line-number)
    (error "Expecting line-number parameter `%s' to be a whole number"
	   line-number))
  (unless (> line-number 0)
    (error "Expecting line-number parameter `%d' to be greater than 0"
	   line-number))
  (let ((last-line (line-number-at-pos (point-max))))
    (unless (<= line-number last-line)
      (error 
       "Line number %d should not exceed %d, the number of lines in the buffer"
       line-number last-line))
    (goto-char (point-min))
    (forward-line (1- line-number))
    (if column-number
	(let ((last-column 
	       (save-excursion
		 (move-end-of-line 1)
		 (current-column))))
	  (cond ((not (wholenump column-number))
		 (message 
		  "Column ignored. Expecting column-number parameter `%s' to be a whole number"
			  column-number))
		((<= column-number 0)
		 (message 
		  "Column ignored. Expecting column-number parameter `%d' to be a greater than 1"
			  column-number))
		((>= column-number last-column)
		 (message 
		  "Column ignored. Expecting column-number parameter `%d' to be a less than %d"
		   column-number last-column))
		(t (forward-char (1- column-number)))))
      )
    )
  )

(defun loc-changes-add-elt (pos)
  "Add an element `loc-changes-alist'. The car will be POS and a
marker for it will be created at the point."
  (setq loc-changes-alist
	(cons (cons pos (point-marker)) loc-changes-alist)))

(defun loc-changes-add-and-goto (line-number &optional opt-buffer)
  "Add a marker at LINE-NUMBER and record LINE-NUMBER and its
marker association in `loc-changes-alist'."
  (let ((buffer (or opt-buffer (current-buffer))))
    (with-current-buffer buffer
      (loc-changes-goto-line line-number)
      (loc-changes-add-elt line-number)
      ))
  )

(defun loc-changes-clear-buffer (&optional opt-buffer)
  "Remove all location-tracking associations in BUFFER."
  (interactive "bbuffer: ")
  (let ((buffer (or opt-buffer (current-buffer)))
	)
    (with-current-buffer buffer
      (setq loc-changes-alist '())
      ))
  )

(defun loc-changes-reset-position (&optional opt-buffer no-insert)
  "Update `loc-changes-alist' the line number of point is what is 
so its line line number at point Take existing marks and use the current (updated) positions for each of those.
This may be useful for example in debugging if you save the
buffer and then cause the debugger to reread/reevaluate the file
so that its positions are will be reflected."
  (let* ((line-number (line-number-at-pos (point)))
	 (elt (assq line-number loc-changes-alist)))
    (if elt
	(setcdr elt (point))
      (unless no-insert
	(loc-changes-add-elt line-number)
	)
      ))
)

(defun loc-changes-goto (position &optional opt-buffer no-update)
  "Go to the position inside BUFFER taking into account the
previous location marks. Normally if the position hasn't been
seen before, we will add a new mark for this position. However if
NO-UPDATE is set, no mark is added."
  (unless (wholenump position)
    (error "Expecting line-number parameter `%s' to be a whole number"
	   position))
  (let ((elt (assq position loc-changes-alist)))
    (if elt
	(let ((marker (cdr elt)))
	  (unless (markerp marker)
	    (error "Internal error: loc-changes-alist is not a marker"))
	  (goto-char (marker-position marker)))
      (if no-update
	  (loc-changes-goto-line position)
	(loc-changes-add-and-goto position))
      )
    )
  )

(provide 'loc-changes)
