;;; multi-prompt.el --- Completing read of multiple strings

;; Copyright (C) 1996, 1997, 2000, 2009 Free Software Foundation

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: auctex-devel@gnu.org
;; Created: 1996-08-31
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package is written for use in emacs lisp programs, where the
;; user is prompted for a string of the form:
;;
;;   FOO,BAR,BAZ
;;
;; where FOO, BAR, and BAZ are elements of some table.  The function
;; `multi-prompt' is a replacement `completing-read' that will allow
;; the user to enter a string like the above, yet get completion on
;; all FOO, BAR, and BAZ.

;;; Code:

(defvar multi-prompt-found nil
  "List of entries currently added during a `multi-prompt'.")

;;;###autoload
(defun multi-prompt (separator
		     unique prompt table
		     &optional mp-predicate require-match initial history)
  "Completing prompt for a list of strings.  
The first argument SEPARATOR should be the string (of length 1) to
separate the elements in the list.  The second argument UNIQUE should
be non-nil, if each element must be unique.  The remaining elements
are the arguments to `completing-read'.  See that."
  (let ((old-map (if require-match
		     minibuffer-local-must-match-map
		   minibuffer-local-completion-map))
	(new-map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-parent)
	;; `set-keymap-parent' was introduced in Emacs 19.32.
	(set-keymap-parent new-map old-map)
      (setq new-map (copy-keymap old-map)))
    (define-key new-map separator (if require-match
				      'multi-prompt-next-must-match
				    'multi-prompt-next))
    (define-key new-map "\C-?" 'multi-prompt-delete)
    (let* ((minibuffer-local-completion-map new-map)
	   (minibuffer-local-must-match-map new-map)
	   (multi-prompt-found nil)
	   (done nil)
	   (filter (cond (unique
			  (lambda (x)
			    (and (not (member (car x) multi-prompt-found))
				 (or (null mp-predicate)
				     (funcall mp-predicate x)))))
			 (mp-predicate)))
	   (answer (catch 'multi-prompt-exit
		     (while t
		       (let ((extra (catch 'multi-prompt-next
				      (throw 'multi-prompt-exit
					     (completing-read prompt 
							      table
							      filter
							      require-match
							      initial
							      history)))))
			 (cond ((eq extra 'back)
				(when multi-prompt-found
				  (setq prompt (substring 
						prompt 0 
						(- 0 (length separator)
						   (length
						    (car multi-prompt-found))))
					initial (car multi-prompt-found))
				  (setq multi-prompt-found 
					(cdr multi-prompt-found))))
			       (t
				(setq prompt (concat prompt extra separator)
				      initial nil)
				(setq multi-prompt-found
				      (cons extra multi-prompt-found)))))))))
      (if (string= answer "")
	  multi-prompt-found
	(nreverse (cons answer multi-prompt-found))))))

(defun multi-prompt-delete ()
  (interactive)
  (if (bobp)
      (throw 'multi-prompt-next 'back)
    (call-interactively 'backward-delete-char)))

(defun multi-prompt-next ()
  (interactive)
  (throw 'multi-prompt-next
         (cond
          ((fboundp 'minibuffer-contents-no-properties)
           ;; buffer-substring no longer works in emacs-21, it returns 
           ;; the whole prompt line. Use this instead.
           (minibuffer-contents-no-properties))
          (t
           (buffer-substring-no-properties (point-min) (point-max))))))
         
(defun multi-prompt-next-must-match ()
  (interactive)
  (when  (call-interactively 'minibuffer-complete)
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (when (or (not require-match)
		(assoc content table))
	(throw 'multi-prompt-next content)))))


;;; Support for key=value completion

;; The following code was ripped out of crm.el
;; (completing-read-multiple) and extended to support comma-separated
;; key=value lists.  The code is separate from the code above.

;; WARNING: This obviously relies on internals of crm.el and
;; minibuffer.el and will therefore have to be adapted if these
;; change.

;; TODO: How to support stuff like "caption={[one]two}" or
;; "morekeywords={one,three,five}"?

(defvar multi-prompt-key-value-sep "="
  "Single-character string separating key=value pairs.")
(defvar multi-prompt-completion-table nil
  "Completion table used by `multi-prompt-key-value'.")

(defun multi-prompt-key-value-collection-fn (string predicate flag)
  "Function used by `multi-prompt-key-value' to compute completion values.
The value of STRING is the string to be completed.

The value of PREDICATE is a function to filter possible matches, or
nil if none.

The value of FLAG is used to specify the type of completion operation.
A value of nil specifies `try-completion'.  A value of t specifies
`all-completions'.  A value of lambda specifes a test for an exact match.

For more information on STRING, PREDICATE, and FLAG, see the Elisp
Reference sections on 'Programmed Completion' and 'Basic Completion
Functions'."
  (let ((beg 0) (last 0) matched)
    (while (string-match multi-prompt-key-value-sep string beg)
      (setq matched t
	    last beg
	    beg (match-end 0)))
    (completion-table-with-context
     (substring string 0 beg)
     (if (not matched)
	 multi-prompt-completion-table
       (cadr (assoc (substring string last (1- beg))
		    multi-prompt-completion-table)))
     (substring string beg)
     predicate
     flag)))

(defun multi-prompt-expand-completion-table (table)
  "Return an expanded version of completion table TABLE.
This is achieved by eval'ing all variables in the value parts of
the alist elements."
  (mapcar (lambda (x)
	    (if (and (cadr x) (symbolp (cadr x)) (not (functionp (cadr x))))
		(cons (car x) (list (eval (cadr x))))
	      x))
	  table))

;; Silence the byte compiler.
(defvar crm-local-must-match-map)
(defvar crm-local-completion-map)

;;;###autoload
(defun multi-prompt-key-value
  (prompt table &optional predicate require-match initial-input
	  hist def inherit-input-method)
  "Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer."
  (let* ((minibuffer-completion-table #'multi-prompt-key-value-collection-fn)
	 (minibuffer-completion-predicate predicate)
	 (minibuffer-completion-confirm
	  (unless (eq require-match t) require-match))
	 (multi-prompt-completion-table
	  ;; Expand the table here because completion would otherwise
	  ;; interpret symbols in the table as functions.  However, it
	  ;; would be nicer if this could be done during the actual
	  ;; completion in order to avoid walking through the whole
	  ;; table.
	  (multi-prompt-expand-completion-table table))
	 (map (if require-match
		  crm-local-must-match-map
		crm-local-completion-map))
	 (input (read-from-minibuffer
		 prompt initial-input map
		 nil hist def inherit-input-method)))
    (and def (string-equal input "") (setq input def))
    input))

(provide 'multi-prompt)

;;; multi-prompt.el ends here
