;;; context.el --- Support for ConTeXt documents.

;; Copyright (C) 2003-2006, 2008, 2010, 2012
;;   Free Software Foundation, Inc.

;; Maintainer: Berend de Boer <berend@pobox.com>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This is in progress ConTeXt support for AUCTeX. Please report
;; anomalies or things you believe should be added.

;; AUCTeX is closely interwoven with LaTeX.  We have to split up
;; things without breaking 'em.

;; some parts are stolen from latex.el and adapted to ConTeXt.

;; TODO
;; 1. indentation still bad.
;; 2. paragraph refilling doesn't work 100%, and is very slow.
;; 4. Remove dependency on LaTeX by moving LaTeX commands to TeX.
;; 5. Most ConTeXt macro's don't currently have lisp code to query for
;;    arguments. As ConTeXt arguments are quite complex, the LaTeX way
;;    of querying for arguments just doesn't cut it.
;; 6. Check auto-parsing: does it detect % interface=nl for example?
;; 7. Complete adding ConTeXt macro's. Perhaps parse cont-en.xml and
;;    generate the interfaces?
;; 8. Add to menu: make TeX hash (mktexlsr), context format and metapost format.

;; TODO Documentation
;; 1. multifile done differently with ConTeXt

;;; Code:

(require 'tex-buf)
(require 'tex)
(require 'latex) ; for functions like `TeX-look-at' and `LaTeX-split-long-menu'
(require 'plain-tex) ; for `plain-TeX-common-initialization'

(defgroup ConTeXt-macro nil
  "Special support for ConTeXt macros in AUCTeX."
  :prefix "TeX-"
  :group 'ConTeXt
  :group 'TeX-macro)


;;; variables

;; globals used in certain macro's.
(defvar done-mark nil
  "Position of point afterwards, default nil (meaning end).")

(defvar reference nil
  "Set by `ConTeXt-section-ref', used by `ConTeXt-section-section'.")

(defvar title nil
  "Set by `ConTeXt-section-title', used by `ConTeXt-section-section'.")


;; others

(defvar ConTeXt-known-interfaces '("cz" "de" "en" "it" "nl" "ro" "uk"))

(defcustom ConTeXt-default-interface "en"
  "Default interface to be used when running ConTeXt."
  :group 'ConTeXt
  :type 'string)

(defvar ConTeXt-current-interface "en"
  "Interface to be used for inserting macros and ConTeXt run.")
(make-variable-buffer-local 'ConTeXt-current-interface)

(defvar ConTeXt-menu-changed nil)
;; Need to update ConTeXt menu.
(make-variable-buffer-local 'ConTeXt-menu-changed)

(defvar ConTeXt-largest-level nil
  "Largest sectioning level within current document.")
(make-variable-buffer-local 'ConTeXt-largest-level)

(defun ConTeXt-largest-level ()
  (TeX-update-style)
  ConTeXt-largest-level)


;;; Syntax

(defvar ConTeXt-optop "["
  "The ConTeXt optional argument opening character.")

(defvar ConTeXt-optcl "]"
  "The ConTeXt optional argument closing character.")


;; Define a ConTeXt macro

(defvar ConTeXt-define-list ()
  "Calls ConTeXt-XX-define-list where XX is the current interface.")

(defun ConTeXt-define-command (what)
  "The ConTeXt macro to define WHAT."
  (funcall
   (intern (concat "ConTeXt-define-command-" ConTeXt-current-interface)) what))

(defun ConTeXt-insert-define (define)
  "Insert the ConTeXt define macro DEFINE."
  (insert TeX-esc (ConTeXt-define-command define))
  (newline)
  (indent-according-to-mode)
  (ConTeXt-arg-setup nil))


;; Setup a ConTeXt macro

(defvar ConTeXt-setup-list ()
  "Calls ConTeXt-XX-setup-list where XX is the current interface.")

(defun ConTeXt-setup-command (what)
  "The ConTeXt macro to setup WHAT."
  (funcall
   (intern (concat "ConTeXt-setup-command-" ConTeXt-current-interface)) what))

(defun ConTeXt-insert-setup (setup)
  "Insert the ConTeXt setup macro SETUP."
  (insert TeX-esc (ConTeXt-setup-command setup))
  (newline)
  (indent-according-to-mode)
  (ConTeXt-arg-setup nil))


;; Referencing ConTeXt macro's

(defvar ConTeXt-referencing-list ()
  "Calls ConTeXt-XX-other-macro-list where XX is the current interface.")

(defun ConTeXt-referencing-command (what)
  "The ConTeXt macro to call WHAT is itself, no interface specific calls."
  what)

(defun ConTeXt-insert-referencing (what)
  "Insert the ConTeXt referencing SETUP."
  (insert TeX-esc (ConTeXt-referencing-command what))
  (newline)
  (indent-according-to-mode)
  (ConTeXt-arg-setup nil))


;; Other ConTeXt macro's

(defvar ConTeXt-other-macro-list ()
  "Calls ConTeXt-XX-other-macro-list where XX is the current interface.")

(defun ConTeXt-other-macro-command (what)
  "The ConTeXt macro to call WHAT is itself, no interface specific calls."
  what)

(defun ConTeXt-insert-other-macro (other-macro)
  "Insert the ConTeXt other macro's macro SETUP."
  (insert TeX-esc (ConTeXt-other-macro-command other-macro))
  (newline)
  (indent-according-to-mode)
  (ConTeXt-arg-setup nil))


;;; Project structure

(defvar ConTeXt-project-structure-list ()
  "Calls ConTeXt-XX-project-structure where XX is the current interface.")

(defun ConTeXt-project-structure (N)
  "Insert a ConTeXt project structure where N is in index into `ConTeXt-project-structure-list'."
  (funcall (intern(concat
		   "ConTeXt-project-"
		   (nth N ConTeXt-project-structure-list)
		   "-insert"))))

(defun ConTeXt-project-project-insert ()
  "Insert a basic template for a new ConTeXt project."
  (save-excursion
    (insert "% The following names are examples only\n")
    (insert TeX-esc (ConTeXt-environment-start-name) (nth 0 ConTeXt-project-structure-list) " myproject")
    (newline 2)
    (insert TeX-esc (nth 1 ConTeXt-project-structure-list) " myenvironment")
    (newline 2)
    (insert TeX-esc (nth 2 ConTeXt-project-structure-list) " myproduct1")
    (newline 2)
    (insert TeX-esc (nth 2 ConTeXt-project-structure-list) " myproduct2")
    (newline 2)
    (insert TeX-esc (ConTeXt-environment-stop-name) (nth 0 ConTeXt-project-structure-list))))

(defun ConTeXt-project-environment-insert ()
  "Insert a basic template for the environment of a ConTeXt project."
  (save-excursion
    (insert "% The name 'myenvironment' is an example only.\n"
	    "% It must match the name in your project file.\n")
    (insert TeX-esc (ConTeXt-environment-start-name)
	    (nth 1 ConTeXt-project-structure-list) " myenvironment\n\n")
    (insert "% Put environment charateristics that must be defined at the "
	    "highest level here\n\n")
    (insert TeX-esc (ConTeXt-environment-stop-name)
	    (nth 1 ConTeXt-project-structure-list))))

(defun ConTeXt-project-product-insert ()
  "Insert a basic template for a product of a ConTeXt project."
  (save-excursion
    (insert "% The following names are examples only\n")
    (insert TeX-esc (ConTeXt-environment-start-name)
	    (nth 2 ConTeXt-project-structure-list) " myproduct1")
    (newline 2)
    (insert TeX-esc (nth 0 ConTeXt-project-structure-list) " myproject")
    (newline 2)
    (insert "% Components are optional. "
	    "You can also just start your document here.\n")
    (insert TeX-esc (nth 3 ConTeXt-project-structure-list) " mycomponent1")
    (newline 2)
    (insert TeX-esc (nth 3 ConTeXt-project-structure-list) " mycomponent2")
    (newline 2)
    (insert TeX-esc (ConTeXt-environment-stop-name)
	    (nth 2 ConTeXt-project-structure-list))))

(defun ConTeXt-project-component-insert ()
  "Insert a basic template for a component of a ConTeXt project."
  (save-excursion
    (insert "% The following names are examples only\n")
    (insert TeX-esc (ConTeXt-environment-start-name)
	    (nth 3 ConTeXt-project-structure-list) " mycomponent1")
    (newline 2)
    (insert TeX-esc (nth 0 ConTeXt-project-structure-list) " myproject\n")
    (insert TeX-esc (nth 2 ConTeXt-project-structure-list) " myproduct1")
    (newline 2)
    (insert "% ... text here ...")
    (newline 2)
    (insert TeX-esc (ConTeXt-environment-stop-name)
	    (nth 3 ConTeXt-project-structure-list))))


;;; Section blocks

(defvar ConTeXt-section-block-list ()
  "Calls ConTeXt-XX-section-list where XX is the current interface.")

(defun ConTeXt-section-block (section-block)
  "Insert the ConTeXt section block SECTION-BLOCK."
  (ConTeXt-environment-menu section-block))


;;; Sections

(defun ConTeXt-section (arg)
  "Insert a template for a ConTeXt section.
Determinate the type of section to be inserted, by the argument ARG.

If ARG is nil or missing, use the current level.
If ARG is a list (selected by \\[universal-argument]), go downward one level.
If ARG is negative, go up that many levels.
If ARG is positive or zero, use absolute level:

	0 : part
	1 : chapter
	2 : section
	3 : subsection
	4 : subsubsection
	5 : subsubsubsection

Or:

	0 : title
	1 : subject
	2 : subsubject
	3 : subsubsubject

The following variables can be set to customize:

`ConTeXt-section-hook'    Hooks to run when inserting a section.
`ConTeXt-section-ref'   Prefix to all section references."

  (interactive "*P")
  (let* ((val (prefix-numeric-value arg))
	 (level (cond ((null arg)
		       (ConTeXt-current-section))
		      ((listp arg)
		       (ConTeXt-down-section))
		      ((< val 0)
		       (ConTeXt-up-section (- val)))
		      (t val)))
	 (name (ConTeXt-section-name level))
	 (toc nil)
	 (title "")
	 (done-mark (make-marker)))
    (newline)
    (run-hooks 'ConTeXt-section-hook)
    (newline)
    (if (marker-position done-mark)
	(goto-char (marker-position done-mark)))
    (set-marker done-mark nil)))

;; LaTeX has a max function here, which makes no sense.
;; I think you want to insert a section that is max ConTeXt-largest-level
(defun ConTeXt-current-section ()
  "Return the level of the section that contain point.
See also `ConTeXt-section' for description of levels."
  (save-excursion
    (min (ConTeXt-largest-level)
	 (if (re-search-backward outline-regexp nil t)
	     (+ 1 (- (ConTeXt-outline-level) (ConTeXt-outline-offset)))
	   (ConTeXt-largest-level)))))

(defun ConTeXt-down-section ()
  "Return the value of a section one level under the current.
Tries to find what kind of section that have been used earlier in the
text, if this fail, it will just return one less than the current
section."
  (save-excursion
    (let ((current (ConTeXt-current-section))
	  (next nil)
	  (regexp outline-regexp))
      (if (not (re-search-backward regexp nil t))
	  (1+ current)
	(while (not next)
	  (cond
	   ((eq (ConTeXt-current-section) current)
	    (if (re-search-forward regexp nil t)
		(if (<= (setq next (ConTeXt-current-section)) current) ;Wow!
		    (setq next (1+ current)))
	      (setq next (1+ current))))
	   ((not (re-search-backward regexp nil t))
	    (setq next (1+ current)))))
	next))))

(defun ConTeXt-up-section (arg)
  "Return the value of the section ARG levels above this one."
  (save-excursion
    (if (zerop arg)
	(ConTeXt-current-section)
      (let ((current (ConTeXt-current-section)))
	(while (and (>= (ConTeXt-current-section) current)
		    (re-search-backward outline-regexp
					nil t)))
	(ConTeXt-up-section (1- arg))))))

(defvar ConTeXt-section-list ()
  "ConTeXt-XX-section-list where XX is the current interface.")

(defun ConTeXt-section-name (level)
  "Return the name of the section corresponding to LEVEL."
  (let ((entry (TeX-member level ConTeXt-section-list
			   (function (lambda (a b) (equal a (nth 1 b)))))))
    (if entry
	(nth 0 entry)
      nil)))

(defun ConTeXt-section-level (name)
  "Return the level of the section NAME."
  (let ((entry (TeX-member name ConTeXt-section-list
			   (function (lambda (a b) (equal a (nth 0 b)))))))

    (if entry
	(nth 1 entry)
      nil)))


;;; Section Hooks.

(defcustom ConTeXt-section-hook
  '(ConTeXt-section-heading
    ConTeXt-section-title
    ConTeXt-section-ref
    ConTeXt-section-section)
  "List of hooks to run when a new section is inserted.

The following variables are set before the hooks are run

level - numeric section level, see the documentation of `ConTeXt-section'.
name - name of the sectioning command, derived from `level'.
title - The title of the section, default to an empty string.
`done-mark' - Position of point afterwards, default nil (meaning end).

The following standard hook exist -

ConTeXt-section-heading: Query the user about the name of the
sectioning command.  Modifies `level' and `name'.

ConTeXt-section-title: Query the user about the title of the
section.  Modifies `title'.

ConTeXt-section-section: Insert ConTeXt section command according to
`name', `title', and `reference'.  If `title' is an empty string,
`done-mark' will be placed at the point they should be inserted.

ConTeXt-section-ref: Insert a reference for this section command.

To get a full featured `ConTeXt-section' command, insert

 (setq ConTeXt-section-hook
			 '(ConTeXt-section-heading
				 ConTeXt-section-title
				 ConTeXt-section-section
				 ConTeXt-section-ref))

in your .emacs file."
  :group 'ConTeXt-macro
  :type 'hook
  :options
  '(ConTeXt-section-heading
    ConTeXt-section-title
    ConTeXt-section-ref
    ConTeXt-section-section))

(defun ConTeXt-section-heading ()
  "Hook to prompt for ConTeXt section name.
Insert this hook into `ConTeXt-section-hook' to allow the user to change
the name of the sectioning command inserted with `\\[ConTeXt-section]'."
  (let ((string (completing-read
		 (concat "Select level: (default " name ") ")
		 ConTeXt-section-list
		 nil nil nil)))
    ;; Update name
    (if (not (zerop (length string)))
	(setq name string))))

(defun ConTeXt-section-title ()
  "Hook to prompt for ConTeXt section title.
Insert this hook into `ConTeXt-section-hook' to allow the user to change
the title of the section inserted with `\\[ConTeXt-section]."
  (setq title (read-string "What title: ")))

(defun ConTeXt-section-section ()
  "Hook to insert ConTeXt section command into the file.
Insert this hook into `ConTeXt-section-hook' after those hooks which sets
the `name', `title', and `reference' variables, but before those hooks which
assumes the section already is inserted."
  (insert TeX-esc name)
  (cond ((null reference))
	((zerop (length reference))
	 (insert ConTeXt-optop)
	 (set-marker done-mark (point))
	 (insert ConTeXt-optcl))
	(t
	 (insert ConTeXt-optop reference ConTeXt-optcl)))
  (insert TeX-grop)
  (if (zerop (length title))
      (set-marker done-mark (point)))
  (insert title TeX-grcl)
  (newline)
  ;; If RefTeX is available, tell it that we've just made a new section
  (and (fboundp 'reftex-notice-new-section)
       (funcall (symbol-function 'reftex-notice-new-section))))

(defun ConTeXt-section-ref ()
  "Hook to insert a reference after the sectioning command.
Insert this hook into `ConTeXt-section-hook' to prompt for a label to be
inserted after the sectioning command."

  (setq reference (completing-read
		   (TeX-argument-prompt t nil
					"Comma separated list of references")
		   (LaTeX-label-list) nil nil))
  ;; No reference or empty string entered?
  (if (string-equal "" reference)
      (setq reference nil)))


;; Various
(defun TeX-ConTeXt-sentinel (process name)
  "Cleanup TeX output buffer after running ConTeXt."
  (cond ((TeX-TeX-sentinel-check process name))
	((save-excursion
	   ;; in a full ConTeXt run there will multiple texutil
	   ;; outputs. Just looking for "another run needed" would
	   ;; find the first occurence
	   (goto-char (point-max))
	   (re-search-backward "TeXUtil " nil t)
	   (re-search-forward "another run needed" nil t))
	 (message (concat "You should run ConTeXt again "
			  "to get references right, "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))
	((re-search-forward "removed files :" nil t)
	 (message "sucessfully cleaned up"))
	((re-search-forward "^ ?TeX\\(Exec\\|Util\\)" nil t) ;; strange regexp --pg
	 (message (concat name ": successfully formatted "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-Show))
	(t
	 (message (concat name ": problems after "
			  (TeX-current-pages)))
	 (setq TeX-command-next TeX-command-default))))


;;; Environments

(defgroup ConTeXt-environment nil
  "Environments in AUCTeX."
  :group 'ConTeXt-macro)

;; TODO: interface awareness
(defcustom ConTeXt-default-environment "itemize"
  "*The default environment when creating new ones with `ConTeXt-environment'."
  :group 'ConTeXt-environment
  :type 'string)
(make-variable-buffer-local 'ConTeXt-default-environment)

(TeX-auto-add-type "environment" "ConTeXt")

(fset 'ConTeXt-add-environments-auto
      (symbol-function 'ConTeXt-add-environments))
(defun ConTeXt-add-environments (&rest environments)
  "Add ENVIRONMENTS to the list of known environments."
  (apply 'ConTeXt-add-environments-auto environments)
  (setq ConTeXt-menu-changed t))

;; (defvar ConTeXt-environment-list ()
;;	"ConTeXt-environment-list-XX where XX is the current interface.")

(defvar ConTeXt-environment-history nil)

(defun ConTeXt-environment-start-name ()
  "Return the \\start translated to the language in current interface."
  ;; it is "inizia", others are "start"
  (cond ((equal ConTeXt-current-interface "it")
	 "inizia")
	((member ConTeXt-current-interface ConTeXt-known-interfaces)
	 "start")
	(t
	 ;; this should not happen
	 (error "Unknown interface: %s" ConTeXt-current-interface))))

(defun ConTeXt-environment-stop-name ()
  "Return the \\stop translated to the language in current interface."
  ;; it is "termina", others are "stop"
  (cond ((equal ConTeXt-current-interface "it")
	 "termina")
	((member ConTeXt-current-interface ConTeXt-known-interfaces)
	 "stop")
	(t
	 ;; this should not happen
	 (error "Unknown interface: %s" ConTeXt-current-interface))))


(defun ConTeXt-environment (arg)
  "Make ConTeXt environment (\\start...-\\stop... pair).
With optional ARG, modify current environment."
  (interactive "*P")
  (let ((environment (
		      completing-read (concat "Environment type: (default "
					      (if (TeX-near-bobp)
						  "text"
						ConTeXt-default-environment)
					      ") ")
		      ConTeXt-environment-list
		      nil nil nil
		      'ConTeXt-environment-history)
		     ))
    ;; Get default
    (cond ((and (zerop (length environment))
		(TeX-near-bobp))
	   (setq environment "text"))
	  ((zerop (length environment))
	   (setq environment ConTeXt-default-environment))
	  (t
	   (setq ConTeXt-default-environment environment)))

    (let ((entry (assoc environment ConTeXt-environment-list)))
      (when (null entry)
	(ConTeXt-add-environments (list environment)))
      (if arg
	  (ConTeXt-modify-environment environment)
	(ConTeXt-environment-menu environment)))))

(defun ConTeXt-modify-environment (environment)
  "Modify current environment."
  (save-excursion
    (ConTeXt-find-matching-stop)
    (re-search-backward (concat (regexp-quote TeX-esc)
				(ConTeXt-environment-stop-name)
				" *\\([a-zA-Z]*\\)")
			(save-excursion (beginning-of-line 1) (point)))
    (replace-match
     (concat TeX-esc (ConTeXt-environment-stop-name) environment) t t)
    (beginning-of-line 1)
    (ConTeXt-find-matching-start)
    (re-search-forward (concat (regexp-quote TeX-esc)
			       (ConTeXt-environment-start-name)
			       " *\\([a-zA-Z]*\\)")
		       (save-excursion (end-of-line 1) (point)))
    (replace-match
     (concat TeX-esc (ConTeXt-environment-start-name) environment) t t)))


(defun ConTeXt-environment-menu (environment)
  "Insert ENVIRONMENT around point or region."
  (let ((entry (assoc environment ConTeXt-environment-list)))
    (cond ((not (and entry (nth 1 entry)))
	   (ConTeXt-insert-environment environment))
	  ((numberp (nth 1 entry))
	   (let ((count (nth 1 entry))
		 (args ""))
	     (while (> count 0)
	       (setq args (concat args TeX-grop TeX-grcl))
	       (setq count (- count 1)))
	     (ConTeXt-insert-environment environment args)))
	  ((stringp (nth 1 entry))
	   (let ((prompts (cdr entry))
		 (args ""))
	     (while prompts
	       (setq args (concat args
				  TeX-grop
				  (read-from-minibuffer
				   (concat (car prompts) ": "))
				  TeX-grcl))
	       (setq prompts (cdr prompts)))
	     (ConTeXt-insert-environment environment args)))
	  (t
	   (apply (nth 1 entry) environment (nthcdr 2 entry))))))

(defun ConTeXt-close-environment ()
  "Insert \\stop... to match the current environment."
  (interactive "*")
  (beginning-of-line)
  (let ((empty-line (looking-at "[ \t]*$")))
    (end-of-line)
    (if (not empty-line)
	(newline)))
  (insert TeX-esc (ConTeXt-environment-stop-name)
	  (ConTeXt-current-environment))
  ;; indent broken, so don't do it.
  ;;(indent-according-to-mode)
  (end-of-line)
  (newline))

(defun ConTeXt-insert-environment (environment &optional extra)
  "Insert ENVIRONMENT, with optional argument EXTRA."
  (if (and (TeX-active-mark)
	   (not (eq (mark) (point))))
      (save-excursion
	(if (< (mark) (point))
	    (exchange-point-and-mark))
	(insert TeX-esc (ConTeXt-environment-start-name) environment)
	(newline)
	(forward-line -1)
	(indent-according-to-mode)
	(if extra (insert extra))
	(goto-char (mark))
	(or (TeX-looking-at-backward "^[ \t]*")
	    (newline))
	(insert TeX-esc (ConTeXt-environment-stop-name) environment)
	(newline)
	(forward-line -1)
	(indent-according-to-mode)
	;;(goto-char (point))
	)
    (or (TeX-looking-at-backward "^[ \t]*")
	(newline))
    (insert TeX-esc (ConTeXt-environment-start-name) environment)
    (indent-according-to-mode)
    (if extra (insert extra))
    (end-of-line)
    (newline-and-indent)
    (newline)
    (insert TeX-esc (ConTeXt-environment-stop-name) environment)
    (or (looking-at "[ \t]*$")
	(save-excursion (newline-and-indent)))
    (indent-according-to-mode)
    (end-of-line 0)))


;; with the following we can call a function on an environment. Say
;; you have metapost stuff within your TeX file, go to the environment
;; and run ConTeXt-work-on-environment (suggested Key: C-c !). AUCTeX
;; sees that you are inside e.g. \startMPpage....\stopMPpage and
;; looks in ConTeXt-environment-helper for a function to be called.

;; % so pressing C-c ! inside the following ...
;;\startuseMPgraphic{Logo}{Scale}
;; % Top rectangle
;; filldraw (0,0)--(2cm,0)--(2cm,1cm)--(0,1cm)--cycle withcolor blue ;
;; % Bottom black rectangle
;; drawfill (0,0)--(2cm,0)--(2cm,-1cm)--(0,-1cm)--cycle withcolor black;
;; % White Text
;; draw btex \bf AB etex withcolor white ;
;; % resize to size
;; currentpicture := currentpicture scaled \MPvar{Scale} ;
;; \stopuseMPgraphic

;; % ...should give you a "new buffer" (currently narrowed to region
;; % and switched to metapost-mode and recursive-edit)

;; % Top rectangle
;; filldraw (0,0)--(2cm,0)--(2cm,1cm)--(0,1cm)--cycle withcolor blue ;
;; % Bottom black rectangle
;; drawfill (0,0)--(2cm,0)--(2cm,-1cm)--(0,-1cm)--cycle withcolor black;
;; % White Text
;; draw btex \bf AB etex withcolor white ;
;; % resize to size
;; currentpicture := currentpicture scaled \MPvar{Scale} ;


(defvar ConTeXt-environment-helper
  '(("useMPgraphic" . ConTeXt-mp-region)
    ("MPpage" . ConTeXt-mp-region))
  "Alist that holds functions to call for working on regions.
An entry looks like: (\"environment\" . function)")

(defun ConTeXt-mp-region ()
  "Edit region in `metapost-mode'."
  (ConTeXt-mark-environment t)
  (narrow-to-region (mark) (point))
  (metapost-mode)
  (message "Type `M-x exit-recursive-edit' to get back")
  (recursive-edit)
  (context-mode)
  (widen))

;; find smarter name. Suggestions welcome
(defun ConTeXt-work-on-environment ()
  "Takes current environment and does something on it (todo: documentation)."
  (interactive)
  (let ((fun (cdr (assoc (ConTeXt-current-environment)
			 ConTeXt-environment-helper))))
    (when (functionp fun)
      (funcall fun))))

(defun ConTeXt-current-environment ()
  "Return the name of the current environment."
  ;; don't make this interactive.
  (let ((beg))
    (save-excursion
      (ConTeXt-last-unended-start)
      (setq beg (+ (point) (length (ConTeXt-environment-start-name)) 1))
      (goto-char (match-end 0))
      (skip-chars-forward "a-zA-Z")
      (buffer-substring beg (point)))))

(defun ConTeXt-last-unended-start ()
  "Leave point at the beginning of the last `\\start...' that is unstopped looking from the current cursor."
  (while (and (re-search-backward "\\\\start[a-zA-Z]*\\|\\\\stop[a-zA-Z]*")
	      (looking-at "\\\\stop[a-zA-Z]*"))
    (ConTeXt-last-unended-start)))

(defun ConTeXt-mark-environment (&optional inner)
  "Set mark to end of current environment (\\start...-\\stop...) and
point to the matching begin.
If optional INNER is not nil, include \\start... and \\stop, otherwise only
the contents."
  (interactive)
  (let ((cur (point)))
    (ConTeXt-find-matching-stop inner)
    (set-mark (point))
    (goto-char cur)
    (ConTeXt-find-matching-start inner)
    (TeX-activate-region)))

(defun ConTeXt-find-matching-stop (&optional inner)
  "Find end of current \\start...\\stop-Pair.
If INNER is non-nil, go to the point just past before
\\stop... macro.  Otherwise goto the point just past \\stop..."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc)
			"\\("
			(ConTeXt-environment-start-name)
			"\\|"
			(ConTeXt-environment-stop-name)
			"\\)"
			))
	(level 1)
	(pos))
    ;;jump over the \start... when at the beginning of it.
    (when (looking-at (concat (regexp-quote TeX-esc)
			      (ConTeXt-environment-start-name)))
      (re-search-forward regexp nil t))
    (while (and (> level 0)
		(re-search-forward regexp nil t)
		(goto-char (1- (match-beginning 1)))
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-start-name)))
		       (re-search-forward regexp nil t)
		       (setq level (1+ level)))
		      ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-stop-name)))
		       (re-search-forward regexp nil t)
		       (setq level (1- level))))))
    ;; now we have to look if we want to start behind the \start... macro
    (if inner
	(beginning-of-line)
      (skip-chars-forward "a-zA-Z"))))

(defun ConTeXt-find-matching-start (&optional inner)
  "Find beginning of current \\start...\\stop-Pair.
If INNER is non-nil, go to the point just past the \\start... macro."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc)
			"\\("
			(ConTeXt-environment-start-name)
			"\\|"
			(ConTeXt-environment-stop-name)
			"\\)"
			))
	(level 1)
	(pos))
    (while (and (> level 0)
		(re-search-backward regexp nil t)
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-stop-name)))
		       (setq level (1+ level)))
		      ((looking-at (concat (regexp-quote TeX-esc)
					   (ConTeXt-environment-start-name)))
		       (setq level (1- level))))))
    ;; now we have to look if we want to start behind the \start... macro
    (when inner
      ;; \startfoo can have 0 or more {} and [] pairs. I assume that
      ;; skipping all those parens will be smart enough. It fails when
      ;; the first part in the \start-\stop-environment is { or [, like
      ;; in \startquotation   {\em important} \stopquotation. There is
      ;; yet another pitfall: \startsetups SomeSetup foo bar
      ;; \stopsetups will use SomeSetup as the argument and the
      ;; environment
      (skip-chars-forward "\\\\a-zA-Z")
      (save-excursion
	(while (progn
		 (skip-chars-forward "\t\n ")
		 (forward-comment 1)
		 (skip-chars-forward "\t\n ")
		 (looking-at "\\s\("))
	  (forward-list 1)
	  (setq pos (point))))
      (when pos
	(goto-char pos))
      (unless (bolp)
	(forward-line)))))

;;; items

(defun ConTeXt-insert-item ()
  "Insert a new item."
  (interactive "*")
  (or (TeX-looking-at-backward "^[ \t]*")
      (newline))
  (TeX-insert-macro "item")
  (indent-according-to-mode))


;;; Macro Argument Hooks

(defun ConTeXt-optional-argument-insert (arg &optional prefix)
  "Insert ARG surrounded by square brackets."
  (insert ConTeXt-optop)
  (insert arg)
  (insert ConTeXt-optcl))

(defun ConTeXt-required-argument-insert (arg &optional prefix)
  "Insert ARG surrounded by curly braces."
  (insert TeX-grop)
  (insert arg)
  (insert TeX-grcl))

(defun ConTeXt-argument-insert (arg optional &optional prefix)
  "Insert ARG surrounded by curly braces.

If OPTIONAL, only insert it if not empty, and then use square brackets."
  (if optional
      (if
	  (not (string-equal arg ""))
	  (ConTeXt-optional-argument-insert arg prefix))
    (ConTeXt-required-argument-insert arg prefix)))

(defun ConTeXt-arg-ref (optional &optional prompt definition)
  "Prompt for a reference completing with known references."
  (let ((ref (completing-read (TeX-argument-prompt optional prompt "ref")
			      (LaTeX-label-list))))
    (if (and definition (not (string-equal "" ref)))
	(LaTeX-add-labels ref))
    (ConTeXt-argument-insert ref optional)))

(defun ConTeXt-arg-define-ref (&optional prompt)
  "Prompt for an optional reference completing with known references."
  (ConTeXt-arg-ref t prompt t))

(defun ConTeXt-arg-setup (optional &optional prompt)
  "Prompt for setup arguments."
  (let ((setup (read-from-minibuffer
		(TeX-argument-prompt optional prompt "Setup"))))
    (ConTeXt-argument-insert setup t)))


;; paragraph (re)-formatting

(defvar ConTeXt-item-list ()
  "List of macro's considered items.")

(defun ConTeXt-paragraph-commands-regexp ()
  "Return a regexp matching macros that should have their own line."
  (concat
   (regexp-quote TeX-esc) "\\("
   "[][]\\|"  ; display math delimitors (is this applicable to ConTeXt??)
   (ConTeXt-environment-start-name) "\\|"
   (ConTeXt-environment-stop-name) "\\|"
   (mapconcat 'car ConTeXt-section-list "\\b\\|") "\\b\\|"
   (mapconcat 'identity ConTeXt-extra-paragraph-commands "\\b\\|")
   "\\b\\|"
   (mapconcat 'identity ConTeXt-item-list "\\b\\|") "\\b\\)"))


;; Outline support

(defun ConTeXt-environment-full-start-name (environment)
  "Return the ConTeXt macro name that starts ENVIRONMENT.
It is interface aware"
  (concat (ConTeXt-environment-start-name) environment))

(defun ConTeXt-outline-regexp (&optional anywhere)
  "Return regexp for ConTeXt section blocks and sections.

If optional argument ANYWHERE is not nil, do not require that the
header is at the start of a line."
  (concat
   (if anywhere "" "^")
   "[ \t]*"
   (regexp-quote TeX-esc)
   "\\("
   (mapconcat 'ConTeXt-environment-full-start-name ConTeXt-section-block-list "\\|") "\\|"
   (mapconcat 'car ConTeXt-section-list "\\|")
   "\\)\\b"
   (if TeX-outline-extra
       "\\|"
     "")
   (mapconcat 'car TeX-outline-extra "\\|")
   "\\|" (ConTeXt-header-end) "\\b"
   "\\|" (ConTeXt-trailer-start) "\\b"))

(defvar ConTeXt-text "Name of ConTeXt macro that begins the text body.")

(defun ConTeXt-header-end ()
  "Default end of header marker for ConTeXt documents."
  (concat
   (regexp-quote TeX-esc)
   (ConTeXt-environment-start-name)
   ConTeXt-text))

(defun ConTeXt-trailer-start ()
  "Default start of trailer marker for ConTeXt documents."
  (concat
   (regexp-quote TeX-esc)
   (ConTeXt-environment-stop-name)
   ConTeXt-text))

(defun ConTeXt-outline-offset ()
  "Offset to add to `ConTeXt-section-list' levels to get outline level."
  (- 4 (ConTeXt-largest-level)))

(defun ConTeXt-start-environment-regexp (list)
  "Regular expression that matches a start of all environments mentioned in LIST."
  (concat
   "start\\("
   (mapconcat 'identity list "\\|")
   "\\)\\b"))

;; The top headings are \starttext, \startfrontmatter, \startbodymatter etc.
;; \part, \chapter etc. are children of that.
(defun ConTeXt-outline-level ()
  "Find the level of current outline heading in an ConTeXt document."
  (cond ((looking-at (concat (ConTeXt-header-end) "\\b")) 1)
	((looking-at (concat (ConTeXt-trailer-start) "\\b")) 1)
	((TeX-look-at TeX-outline-extra)
	 (max 1 (+ (TeX-look-at TeX-outline-extra)
		   (ConTeXt-outline-offset))))
	(t
	 (save-excursion
	   (skip-chars-forward " \t")
	   (forward-char 1)
	   (cond ((looking-at (ConTeXt-start-environment-regexp
			       ConTeXt-section-block-list)) 1)
		 ((TeX-look-at ConTeXt-section-list)
		  (max 1 (+ (TeX-look-at ConTeXt-section-list)
			    (ConTeXt-outline-offset))))
		 (t
		  (error "Unrecognized header")))))))


;;; Fonts

(defcustom ConTeXt-font-list '((?\C-b "{\\bf " "}")
			   (?\C-c "{\\sc " "}")
			   (?\C-e "{\\em " "}")
			   (?\C-i "{\\it " "}")
			   (?\C-r "{\\rm " "}")
			   (?\C-s "{\\sl " "}")
			   (?\C-t "{\\tt " "}")
			   (?\C-d "" "" t))
  "List of fonts used by `TeX-font'.

Each entry is a list.
The first element is the key to activate the font.
The second element is the string to insert before point, and the third
element is the string to insert after point.
If the fourth and fifth element are strings, they specify the prefix and
suffix to be used in math mode.
An optional fourth (or sixth) element means always replace if t."
  :group 'TeX-macro
  :type '(repeat
	   (group
	    :value (?\C-a "" "")
	    (character :tag "Key")
	    (string :tag "Prefix")
	    (string :tag "Suffix")
	    (option (group
		     :inline t
		     (string :tag "Math Prefix")
		     (string :tag "Math Suffix")))
	    (option (sexp :format "Replace\n" :value t)))))


;; Imenu support

(defun ConTeXt-outline-name ()
  "Guess a name for the current header line."
  (save-excursion
    (if (re-search-forward "{\\([^\}]*\\)}" (point-at-eol) t)
	(match-string 1)
      (buffer-substring-no-properties (point) (point-at-eol)))))

;; This imenu also includes commented out chapters. Perhaps a feature
;; for LaTeX, not sure we want or need that for ConTeXt.

(defun ConTeXt-imenu-create-index-function ()
  "Imenu support function for ConTeXt."
  (TeX-update-style)
  (let (entries level (regexp (ConTeXt-outline-regexp)))
    (goto-char (point-max))
    (while (re-search-backward regexp nil t)
      (let* ((name (ConTeXt-outline-name))
	     (level (make-string (1- (ConTeXt-outline-level)) ?\ ))
	     (label (concat level level name))
	     (mark (make-marker)))
	(set-marker mark (point))
	(set-text-properties 0 (length label) nil label)
	(setq entries (cons (cons label mark) entries))))
    entries))


;; Indentation, copied from Berend's context mode.
;; TODO: doesn't work great.

(defvar ConTeXt-indent-allhanging t)
(defvar ConTeXt-indent-arg 2)
(defvar ConTeXt-indent-basic 2)
(defvar ConTeXt-indent-item ConTeXt-indent-basic)
(defvar ConTeXt-indent-item-re "\\\\\\(item\\|sym\\)\\>")

(defvar ConTeXt-indent-syntax-table (make-syntax-table TeX-mode-syntax-table)
  "Syntax table used while computing indentation.")

(progn
  (modify-syntax-entry ?$ "." ConTeXt-indent-syntax-table)
  (modify-syntax-entry ?\( "." ConTeXt-indent-syntax-table)
  (modify-syntax-entry ?\) "." ConTeXt-indent-syntax-table))

(defun ConTeXt-indent-line (&optional arg)
  (with-syntax-table ConTeXt-indent-syntax-table
    ;; TODO: Rather than ignore $, we should try to be more clever about it.
    (let ((indent
	   (save-excursion
	     (beginning-of-line)
	     (ConTeXt-find-indent))))
      (if (< indent 0) (setq indent 0))
      (if (<= (current-column) (current-indentation))
	  (indent-line-to indent)
	(save-excursion (indent-line-to indent))))))

(defun ConTeXt-find-indent (&optional virtual)
  "Find the proper indentation of text after point.
VIRTUAL if non-nil indicates that we're only trying to find the
indentation in order to determine the indentation of something
else.  There might be text before point."
  (save-excursion
    (skip-chars-forward " \t")
    (or
     ;; Trust the current indentation, if such info is applicable.
     (and virtual (>= (current-indentation) (current-column))
	  (current-indentation))
     ;; Put leading close-paren where the matching open brace would be.
     (condition-case nil
	 (and (eq (char-syntax (char-after)) ?\))
	      (save-excursion
		(skip-syntax-forward " )")
		(backward-sexp 1)
		(ConTeXt-find-indent 'virtual)))
       (error nil))
     ;; Default (maybe an argument)
     (let ((pos (point))
	   (char (char-after))
	   (indent 0)
	   up-list-pos)
       ;; Look for macros to be outdented
       (cond ((looking-at (concat (regexp-quote TeX-esc)
				  (ConTeXt-environment-stop-name)))
	      (setq indent (- indent ConTeXt-indent-basic)))
	     ((looking-at ConTeXt-indent-item-re)
	      (setq indent (- indent ConTeXt-indent-item))))
       ;; Find the previous point which determines our current indentation.
       (condition-case err
	   (progn
	     (backward-sexp 1)
	     (while (> (current-column) (current-indentation))
	       (backward-sexp 1)))
	 (scan-error
	  (setq up-list-pos (nth 2 err))))
       (cond
	((= (point-min) pos) 0)	 ; We're really just indenting the first line.
	((integerp up-list-pos)
	 ;; Have to indent relative to the open-paren.
	 (goto-char up-list-pos)
	 (if (and (not ConTeXt-indent-allhanging)
		  (> pos (progn (down-list 1)
				(forward-comment (point-max))
				(point))))
	     ;; Align with the first element after the open-paren.
	     (current-column)
	   ;; We're the first element after a hanging brace.
	   (goto-char up-list-pos)
	   (+ indent ConTeXt-indent-basic (ConTeXt-find-indent 'virtual))))
	;; We're now at the "beginning" of a line.
	((not (and (not virtual) (eq (char-after) ?\\)))
	 ;; Nothing particular here: just keep the same indentation.
	 (+ indent (current-column)))
	;; We're now looking at an item.
	((looking-at ConTeXt-indent-item-re)
	 ;; Indenting relative to an item, have to re-add the outdenting.
	 (+ indent (current-column) ConTeXt-indent-item))
	;; We're looking at an environment starter.
	((and (looking-at (concat (regexp-quote TeX-esc)
				  (ConTeXt-environment-start-name)))
	      (not (looking-at (concat (regexp-quote TeX-esc)
				       (ConTeXt-environment-start-name)
				       ConTeXt-text)))) ; other environments?
	 (+ indent (current-column) ConTeXt-indent-basic))
	(t
	 (let ((col (current-column)))
	   (if (not (and char (eq (char-syntax char) ?\()))
	       ;; If the first char was not an open-paren, there's
	       ;; a risk that this is really not an argument to the
	       ;; macro at all.
	       (+ indent col)
	     (forward-sexp 1)
	     (if (< (line-end-position)
		    (save-excursion (forward-comment (point-max))
				    (point)))
		 ;; we're indenting the first argument.
		 (min (current-column) (+ ConTeXt-indent-arg col))
	       (skip-syntax-forward " ")
	       (current-column))))))))))


;; XML inside ConTeXt support

(defun ConTeXt-last-unended-start-xml ()
  "Leave point at the beginning of the last `tag' that is unstopped."
  (while (and (re-search-backward "<[_A-Za-z][-:._A-Za-z0-9]*\\([ \t\r\n]\\|[_A-Za-z][-:._A-Za-z0-9]*\=\"[^\"]*\"\\)*>\\|</[_A-Za-z][-:_A-Za-z0-9]*>")
	      (looking-at "</[_A-Za-z][-:._A-Za-z0-9]*>"))
    (ConTeXt-last-unended-start-xml)))

(defun ConTeXt-close-xml-tag ()
  "Create an </...> to match the last unclosed <...>.  Not fool-proof."
  (interactive "*")
  (let ((new-line-needed (bolp)) text indentation)
    (save-excursion
      (condition-case nil
	  (ConTeXt-last-unended-start-xml)
	(error (error "Couldn't find unended XML tag")))
      (setq indentation (current-column))
      (re-search-forward "<\\([_A-Za-z][-:._A-Za-z0-9]*\\)")
      (setq text (buffer-substring (match-beginning 1) (match-end 1))))
    (indent-to indentation)
    (insert "</" text ">")
    (if new-line-needed (insert ?\n))))


;; Key bindings

(defvar ConTeXt-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-mode-map)

    (define-key map "\e\C-a"  'ConTeXt-find-matching-start)
    (define-key map "\e\C-e"  'ConTeXt-find-matching-stop)
    ;; likely to change in the future
    (define-key map "\C-c!"    'ConTeXt-work-on-environment)
    (define-key map "\C-c\C-e" 'ConTeXt-environment)
    (define-key map "\C-c\n"   'ConTeXt-insert-item)
    (or (key-binding "\e\r")
	(define-key map "\e\r"    'ConTeXt-insert-item)) ;*** Alias
    (define-key map "\C-c]" 'ConTeXt-close-environment)
    (define-key map "\C-c\C-s" 'ConTeXt-section)
    ;; XML in ConTeXt support
    (define-key map "\C-c/" 'ConTeXt-close-xml-tag)
    map)
  "Keymap used in `ConTeXt-mode'.")


;;; Menu building

;; functions to create menu entries

;; ConTeXt \start... \stop... pairs
;; (Choose a different name than the one in LaTeX mode.  Otherwise the
;; contents of the "Insert Environment" and "Change Environment" menus
;; will not be updated correctly upon loading and switching between
;; LaTeX and ConTeXt files.  AFAICS this is due to a bug in
;; easymenu.el not returning the correct keymap when
;; `easy-menu-change' (and therefore `easy-menu-get-map') is called.
;; It just sees an entry with a matching name and returns this first
;; match.)
(defvar ConTeXt-environment-menu-name "Insert Environment   (C-c C-e)")

(defun ConTeXt-environment-menu-entry (entry)
  "Create an entry for the environment menu."
  (vector (car entry) (list 'ConTeXt-environment-menu (car entry)) t))

(defvar ConTeXt-environment-modify-menu-name "Change Environment   (C-u C-c C-e)")

(defun ConTeXt-environment-modify-menu-entry (entry)
  "Create an entry for the change environment menu."
  (vector (car entry) (list 'ConTeXt-modify-environment (car entry)) t))

;; ConTeXt define macros
(defvar ConTeXt-define-menu-name "Define")

(defun ConTeXt-define-menu-entry (entry)
  "Create an entry for the define menu."
  (vector entry (list 'ConTeXt-define-menu entry)))

(defun ConTeXt-define-menu (define)
  "Insert DEFINE from menu."
  (ConTeXt-insert-define define))

;; ConTeXt setup macros
(defvar ConTeXt-setup-menu-name "Setup")

(defun ConTeXt-setup-menu-entry (entry)
  "Create an entry for the setup menu."
  (vector entry (list 'ConTeXt-setup-menu entry)))

(defun ConTeXt-setup-menu (setup)
  "Insert SETUP from menu."
  (ConTeXt-insert-setup setup))

;; ConTeXt referencing macros
(defvar ConTeXt-referencing-menu-name "Referencing")

(defun ConTeXt-referencing-menu-entry (entry)
  "Create an entry for the referencing menu."
  (vector entry (list 'ConTeXt-referencing-menu entry)))

(defun ConTeXt-referencing-menu (referencing)
  "Insert REFERENCING from menu."
  (ConTeXt-insert-referencing referencing))

;; ConTeXt other macros
(defvar ConTeXt-other-macro-menu-name "Other macro")

(defun ConTeXt-other-macro-menu-entry (entry)
  "Create an entry for the other macro menu."
  (vector entry (list 'ConTeXt-other-macro-menu entry)))

(defun ConTeXt-other-macro-menu (other-macro)
  "Insert OTHER MACRO from menu."
  (ConTeXt-insert-other-macro other-macro))


;; meta-structure project structure menu entries

(defvar ConTeXt-project-structure-menu-name "Project Structure")

(defun ConTeXt-project-structure-menu (project-structure)
  "Insert project structure from menu."
  (ConTeXt-project-structure
   (let ((l ConTeXt-project-structure-list))
     (- (length l) (length (member project-structure l))))))

(defun ConTeXt-project-structure-menu-entry (entry)
  "Create an ENTRY for the project structure menu."
  (vector entry (list 'ConTeXt-project-structure-menu entry)))


;; meta-structure section blocks menu entries

(defvar ConTeXt-section-block-menu-name "Section Block")

(defun ConTeXt-section-block-menu (section-block)
  "Insert section block from menu."
  (ConTeXt-section-block section-block))

(defun ConTeXt-section-block-menu-entry (entry)
  "Create an ENTRY for the section block menu."
  (vector entry (list 'ConTeXt-section-block-menu entry)))


;; section menu entries

(defvar ConTeXt-section-menu-name "Section  (C-c C-s)")

(defun ConTeXt-section-enable-symbol (level)
  "Symbol used to enable section LEVEL in the menu bar."
  (intern (concat "ConTeXt-section-" (int-to-string level) "-enable")))

(defun ConTeXt-section-enable (entry)
  "Enable or disable section ENTRY from `ConTeXt-section-list'."
  (let ((level (nth 1 entry)))
    (set (ConTeXt-section-enable-symbol level)
	 (>= level ConTeXt-largest-level))))

(defun ConTeXt-section-menu (level)
  "Insert section from menu."
  (let ((ConTeXt-section-hook (delq 'ConTeXt-section-heading
				    (copy-sequence ConTeXt-section-hook))))
    (ConTeXt-section level)))

(defun ConTeXt-section-menu-entry (entry)
  "Create an ENTRY for the section menu."
  (let ((enable (ConTeXt-section-enable-symbol (nth 1 entry))))
    (set enable t)
    (vector (car entry) (list 'ConTeXt-section-menu (nth 1 entry)) enable)))


;; etexshow support

(defun ConTeXt-etexshow ()
  "Call etexshow, if available, to show the definition of a ConText macro."
  (interactive)
  (if (fboundp 'etexshow)
      (let ()
	(require 'etexshow)
	(funcall (symbol-function 'etexshow-cmd)))
    (error "etexshow is not installed.  Get it from http://levana.de/emacs/")))

;; menu itself

(easy-menu-define ConTeXt-mode-command-menu
  ConTeXt-mode-map
  "Command menu used in ConTeXt mode."
  (TeX-mode-specific-command-menu 'context-mode))

;; it seems the menu is evaluated at compile/load-time
;; we don't have ConTeXt-current-interface at that time
;; so make sure to do updates based on that variable in
;; ConTeXt-menu-update
(easy-menu-define ConTeXt-mode-menu
  ConTeXt-mode-map
  "Menu used in ConTeXt mode."
  (TeX-menu-with-help
   `("ConTeXt"
     (,ConTeXt-project-structure-menu-name)
     (,ConTeXt-section-block-menu-name)
     (,ConTeXt-section-menu-name)
     ["Add Table of Contents to Emacs Menu" (imenu-add-to-menubar "TOC") t]
     "-"
     ["Macro ..." TeX-insert-macro
      :help "Insert a macro and possibly arguments"]
     ["Complete" TeX-complete-symbol
      :help "Complete the current macro or environment name"]
     ["Show ConTeXt Macro Definition" ConTeXt-etexshow]
     "-"
     (,ConTeXt-environment-menu-name)
     (,ConTeXt-environment-modify-menu-name)
     ["Item" ConTeXt-insert-item
      :help "Insert a new \\item into current environment"]
     (,ConTeXt-define-menu-name)
     (,ConTeXt-setup-menu-name)
     (,ConTeXt-other-macro-menu-name)
     "-"
     ("Insert Font"
      ["Emphasize"  (TeX-font nil ?\C-e) :keys "C-c C-f C-e"]
      ["Bold"       (TeX-font nil ?\C-b) :keys "C-c C-f C-b"]
      ["Typewriter" (TeX-font nil ?\C-t) :keys "C-c C-f C-t"]
      ["Small Caps" (TeX-font nil ?\C-c) :keys "C-c C-f C-c"]
      ["Sans Serif" (TeX-font nil ?\C-f) :keys "C-c C-f C-f"]
      ["Italic"     (TeX-font nil ?\C-i) :keys "C-c C-f C-i"]
      ["Slanted"    (TeX-font nil ?\C-s) :keys "C-c C-f C-s"]
      ["Roman"      (TeX-font nil ?\C-r) :keys "C-c C-f C-r"]
      ["Calligraphic" (TeX-font nil ?\C-a) :keys "C-c C-f C-a"])
     ("Replace Font"
      ["Emphasize"  (TeX-font t ?\C-e) :keys "C-u C-c C-f C-e"]
      ["Bold"       (TeX-font t ?\C-b) :keys "C-u C-c C-f C-b"]
      ["Typewriter" (TeX-font t ?\C-t) :keys "C-u C-c C-f C-t"]
      ["Small Caps" (TeX-font t ?\C-c) :keys "C-u C-c C-f C-c"]
      ["Sans Serif" (TeX-font t ?\C-f) :keys "C-u C-c C-f C-f"]
      ["Italic"     (TeX-font t ?\C-i) :keys "C-u C-c C-f C-i"]
      ["Slanted"    (TeX-font t ?\C-s) :keys "C-u C-c C-f C-s"]
      ["Roman"      (TeX-font t ?\C-r) :keys "C-u C-c C-f C-r"]
      ["Calligraphic" (TeX-font t ?\C-a) :keys "C-u C-c C-f C-a"])
     ["Delete Font" (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
     "-"
     ["Comment or Uncomment Region"
      TeX-comment-or-uncomment-region
      :help "Make the selected region outcommented or active again"]
     ["Comment or Uncomment Paragraph"
      TeX-comment-or-uncomment-paragraph
      :help "Make the current paragraph outcommented or active again"]
     ,TeX-fold-menu
     "-" . ,TeX-common-menu-entries)))

(defun ConTeXt-menu-update (&optional menu)
  "Update entries on AUCTeX menu."
  (or (not (memq major-mode '(context-mode)))
      (null ConTeXt-menu-changed)
      (not (fboundp 'easy-menu-change))
      (progn
	(TeX-update-style)
	(setq ConTeXt-menu-changed nil)
	(message "Updating section menu...")
	(mapc 'ConTeXt-section-enable ConTeXt-section-list)
	(message "Updating environment menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-environment-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-environment-menu-entry
				   (ConTeXt-environment-list))))
	(message "Updating modify environment menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-environment-modify-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-environment-modify-menu-entry
				   (ConTeXt-environment-list))))
	(message "Updating define menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-define-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-define-menu-entry
				   ConTeXt-define-list)))
	(message "Updating setup menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-setup-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-setup-menu-entry
				   ConTeXt-setup-list)))
	(message "Updating referencing menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-referencing-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-referencing-menu-entry
				   ConTeXt-referencing-list)))
	(message "Updating other macro's menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-other-macro-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-other-macro-menu-entry
				   ConTeXt-other-macro-list)))
	(message "Updating project structure menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-project-structure-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-project-structure-menu-entry
				   ConTeXt-project-structure-list)))
	(message "Updating section block menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-section-block-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-section-block-menu-entry
				   ConTeXt-section-block-list)))
	(message "Updating section menu...")
	(easy-menu-change '("ConTeXt") ConTeXt-section-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'ConTeXt-section-menu-entry
				   ConTeXt-section-list)))
	(message "Updating...done")
	(and menu (easy-menu-return-item ConTeXt-mode-menu menu))
	)))

;;; Option expander

(defvar ConTeXt-texexec-option-nonstop "--nonstop "
  "Command line option for texexec to use nonstopmode.")

(defun ConTeXt-expand-options ()
  "Expand options for texexec command."
  (concat
   (let ((engine (eval (nth 4 (assq TeX-engine (TeX-engine-alist))))))
     (when engine
       (format "--engine=%s " engine)))
   (unless (eq ConTeXt-current-interface "en")
     (format "--interface=%s " ConTeXt-current-interface))
   (when TeX-source-correlate-mode
     (format "--passon=\"%s\" "
	     (if (eq TeX-source-correlate-method-active 'synctex)
		 TeX-synctex-tex-flags
	       TeX-source-specials-tex-flags)))
   (unless TeX-interactive-mode
     ConTeXt-texexec-option-nonstop)))

;;; Mode

;; ConTeXt variables that are interface aware
;; They are mapped to interface specific variables

(defvar ConTeXt-language-variable-list
  '(ConTeXt-define-list
    ConTeXt-setup-list
    ConTeXt-referencing-list
    ConTeXt-other-macro-list
    ConTeXt-project-structure-list
    ConTeXt-section-block-list
    ConTeXt-section-list
    ConTeXt-text
    ConTeXt-item-list
    ConTeXt-extra-paragraph-commands))

(defcustom ConTeXt-clean-intermediate-suffixes
  ;; See *suffixes in texutil.pl.
  '("\\.tui" "\\.tup" "\\.ted" "\\.tes" "\\.top" "\\.log" "\\.tmp" "\\.run"
    "\\.bck" "\\.rlg" "\\.mpt" "\\.mpx" "\\.mpd" "\\.mpo" "\\.tuo" "\\.tub"
    "\\.top" "-mpgraph\\.mp" "-mpgraph\\.mpd" "-mpgraph\\.mpo" "-mpgraph\\.mpy"
    "-mprun\\.mp" "-mprun\\.mpd" "-mprun\\.mpo" "-mprun\\.mpy")
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(defcustom ConTeXt-clean-output-suffixes
  '("\\.dvi" "\\.pdf" "\\.ps")
  "List of regexps matching suffixes of files to be deleted.
The regexps will be anchored at the end of the file name to be matched,
i.e. you do _not_ have to cater for this yourself by adding \\\\' or $."
  :type '(repeat regexp)
  :group 'TeX-command)

(TeX-abbrev-mode-setup context-mode)

(defun ConTeXt-mode-common-initialization ()
  "Initialization code that is common for all ConTeXt interfaces."
  ;; `plain-TeX-common-initialization' kills all local variables, but
  ;; we need to keep ConTeXt-current-interface, so save and restore
  ;; it.
  (let (save-ConTeXt-current-interface)
    (setq save-ConTeXt-current-interface ConTeXt-current-interface)
    (plain-TeX-common-initialization)
    (setq ConTeXt-current-interface save-ConTeXt-current-interface))
  (setq major-mode 'context-mode)

  (setq local-abbrev-table context-mode-abbrev-table)

  ;; Make language specific variables buffer local
  (dolist (symbol ConTeXt-language-variable-list)
    (make-variable-buffer-local symbol))

  (require (intern (concat "context-" ConTeXt-current-interface)))
  (dolist (symbol ConTeXt-language-variable-list)
    (set symbol (symbol-value (intern (concat (symbol-name symbol) "-"
					      ConTeXt-current-interface)))))

  ;; Create certain regular expressions based on language
  (setq ConTeXt-indent-item-re (concat "\\\\\\(" (mapconcat 'identity ConTeXt-item-list "\\|") "\\)\\>"))

  ;; What's the deepest level at we can collapse a document?
  ;; set only if user has not set it. Need to be set before menu is created.
  ;; level 2 is "section"
  (or ConTeXt-largest-level
      (setq ConTeXt-largest-level 2))

  ;; keybindings
  (use-local-map ConTeXt-mode-map)

  ;; Indenting
  (set (make-local-variable 'indent-line-function) 'ConTeXt-indent-line)
  (set (make-local-variable 'fill-indent-according-to-mode) t)

  ;; Paragraph formatting
  (set (make-local-variable 'LaTeX-syntactic-comments) nil)
  (set (make-local-variable 'LaTeX-paragraph-commands-regexp)
       (ConTeXt-paragraph-commands-regexp))
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'LaTeX-fill-paragraph)
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (setq paragraph-start
	(concat
	 "[ \t]*\\("
	 (ConTeXt-paragraph-commands-regexp) "\\|"
	 "\\$\\$\\|" ; Plain TeX display math
	 "$\\)"))
  (setq paragraph-separate
	(concat
	 "[ \t]*\\("
	 "\\$\\$" ; Plain TeX display math
	 "\\|$\\)"))

  ;; Keybindings and menu
  (use-local-map ConTeXt-mode-map)
  (easy-menu-add ConTeXt-mode-menu ConTeXt-mode-map)
  (easy-menu-add ConTeXt-mode-command-menu ConTeXt-mode-map)
  (setq ConTeXt-menu-changed t)

  (if (= emacs-major-version 20)
      (make-local-hook 'activate-menubar-hook))
  (add-hook 'activate-menubar-hook 'ConTeXt-menu-update nil t)

  ;; Outline support
  (require 'outline)
  (set (make-local-variable 'outline-level) 'ConTeXt-outline-level)
  (set (make-local-variable 'outline-regexp) (ConTeXt-outline-regexp t))
  ;;(make-local-variable 'outline-heading-end-regexp)
  (setq TeX-header-end (ConTeXt-header-end)
	TeX-trailer-start (ConTeXt-trailer-start))

  ;; font switch support
  (set (make-local-variable 'TeX-font-list) ConTeXt-font-list)

  ;; imenu support
  (set (make-local-variable 'imenu-create-index-function)
       'ConTeXt-imenu-create-index-function)

  ;; run hooks
  (setq TeX-command-default "ConTeXt")
  (setq TeX-sentinel-default-function 'TeX-ConTeXt-sentinel)
  (TeX-run-mode-hooks 'text-mode-hook 'TeX-mode-hook 'ConTeXt-mode-hook))

(defun context-guess-current-interface ()
  "Guess what ConTeXt interface the current buffer is using."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq ConTeXt-current-interface
	  (cond ((re-search-forward "%.*?interface=en" (+ 512 (point)) t)
		 "en")
		((re-search-forward "%.*?interface=nl" (+ 512 (point)) t)
		 "nl")
		((re-search-forward "\\\\starttext" (+ 1024 (point)) t)
		 "en")
		((re-search-forward "\\\\starttekst" (+ 1024 (point)) t)
		 "nl")
		(t
		 ConTeXt-default-interface)))))

;;;###autoload
(defalias 'ConTeXt-mode 'context-mode)

;;;###autoload
(defun context-mode ()
  "Major mode in AUCTeX for editing ConTeXt files.

Special commands:
\\{ConTeXt-mode-map}

Entering `context-mode' calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of context-mode-hook."
  (interactive)
  (context-guess-current-interface)
  (require (intern (concat "context-" ConTeXt-current-interface)))
  (funcall (intern (concat "context-" ConTeXt-current-interface "-mode"))))

(provide 'context)

;;; context.el ends here
