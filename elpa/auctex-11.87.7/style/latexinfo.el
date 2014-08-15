;;; latexinfo.el - Support for LaTeXinfo files.

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Marc Gemis <makke@wins.uia.ac.be>
;; Version: $Id: latexinfo.el,v 1.7 2008-02-03 14:53:30 angeli Exp $

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

;;; Code:

;;; LaTeXinfo mode

(defvar TeX-latexinfo-node-regexp
  '("\\\\node[ \t]+\\([^,\n\r%]+\\)" 1 TeX-auto-label)
  "Matches LaTeXinfo \\node commands, only current node will be found.
We ignore next, previous and up fields.")

(defvar LaTeXinfo-mode nil
  "Non-nil means LaTeXinfo minor mode is active.")
  (make-variable-buffer-local 'LaTeXinfo-mode)

(defvar LaTeXinfo-mode-map nil
  "Keymap containing LaTeXinfo commands.")

(if LaTeXinfo-mode-map
    ()
  (setq LaTeXinfo-mode-map (make-sparse-keymap))
  (define-key LaTeXinfo-mode-map "\C-c\C-u\C-b" 'latexinfo-format-buffer)
  (define-key LaTeXinfo-mode-map "\C-c\C-u\C-r" 'latexinfo-format-region)
  (define-key LaTeXinfo-mode-map "\C-c\C-u\C-s" 'latexinfo-show-structure)
  (define-key LaTeXinfo-mode-map "\C-c\C-ud" 'makke:latexinfo-delete-structure)
  (define-key LaTeXinfo-mode-map "\C-c\C-ug" 'latexinfo-goto-node)
  (define-key LaTeXinfo-mode-map "\C-c\C-ui" 'makke:latexinfo-structure))

(or (assq 'LaTeXinfo-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'LaTeXinfo-mode LaTeXinfo-mode-map)
		minor-mode-map-alist)))

(defun TeX-arg-latexinfo-index (optional &optional prompt)
  "Prompt for a LaTeXinfo index type with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Index")
		    '(("cp") ("vr") ("fn") ("tp") ("pg") ("ky"))
		    nil t)
   optional))

(defun LaTeX-item-latexinfo-menu ()
  "Insert a new menu item"
  (insert "* ::")
  (backward-char 2))

(defun latexinfo-goto-node () ; temporarily here, later in latexinfo-upd.el ??
  "Place pointer on the node given by the user, read node with completion
This fails when the user types in the label of something else"
  (interactive)
  (let ((node-name (completing-read "Goto Node: " (LaTeX-label-list))))
    (goto-char (point-min))
    (if (re-search-forward
	 (concat
	  TeX-esc "node[ \\t]+" node-name ","
	  "\\|"
	  TeX-esc "label{" LaTeX-section-label node-name
	  "\\|"
	  TeX-esc "label{" node-name
	  )
	 (point-max) t)
	(beginning-of-line 1)
    (error "No such node"))))

;;; Hook

(TeX-add-style-hook "latexinfo"
 (function
  (lambda ()
    (require 'latexinfo)
    (require 'latexinfo-structure)

    (require 'min-map)
    (setq LaTeXinfo-mode t)
    
    (TeX-auto-add-regexp TeX-latexinfo-node-regexp)

    (TeX-add-symbols
     '("node"
       (TeX-arg-literal " ")
       (TeX-arg-free TeX-arg-define-label "Node name")
       (TeX-arg-literal ", ")
       (TeX-arg-free TeX-arg-label "Next node")
       (TeX-arg-literal ", ")
       (TeX-arg-free TeX-arg-label "Previous node")
       (TeX-arg-literal ", ")
       (TeX-arg-free TeX-arg-label "Up node"))
     '("setfilename" TeX-arg-file)

     '("var" t)
     '("dfn" t)
     '("emph" t)
     '("kbd" t)
     '("code" t)
     '("samp" t)
     '("key" t)
     '("ctrl" t)
     '("file" t)

     '("comment"
       (TeX-arg-literal " ")
       (TeX-arg-free "Comment"))
     '("c"
       (TeX-arg-literal " ")
       (TeX-arg-free "Comment"))

     '("cindex" t)
     '("cpsubindex" 2)
     '("cpindexbold" t)

     '("newindex" TeX-arg-latexinfo-index)

     '("br" nil)
     '("w" "Text")
     '("dots" nil)
     '("refill" nil)
     '("bullet" nil)
     '("copyright" nil)
     '("sp" nil)

     '("xref" TeX-arg-label)
     '("pxref" TeX-arg-label)
     '("inforef"
       (TeX-arg-literal "{")
       (TeX-arg-free "Name of node")
       (TeX-arg-literal ", ")
       (TeX-arg-free "Name for note")
       (TeX-arg-literal ", ")
       (TeX-arg-free TeX-arg-file "Info file")
       (TeX-arg-literal "}")))

    (LaTeX-add-environments "menu" "tex" "ignore" "ifinfo" "iftex"
			    "example" "same" "display" "format")

    ; Menu's have a special kind of items
    (make-local-variable 'LaTeX-item-list)
    (setq LaTeX-item-list (cons '("menu" . LaTeX-item-latexinfo-menu)
				LaTeX-item-list))

    (make-local-variable 'TeX-font-list)
    (setq TeX-font-list
	  (list (list ?\C-b (concat TeX-esc "b{") "}")
		(list ?\C-c (concat TeX-esc "sc{") "}")
		(list ?\C-e (concat TeX-esc "emph{") "}")
		(list ?\C-i (concat TeX-esc "i{") "}")
		(list ?\C-r (concat TeX-esc "r{") "}")
		(list ?\C-s (concat TeX-esc "samp{") "}")
		(list ?\C-t (concat TeX-esc "t{") "}")
		(list ?s    (concat TeX-esc "strong{") "}")
		(list ?\C-f (concat TeX-esc "file{") "}")
		(list ?\C-d (concat TeX-esc "dfn{") "}")
		(list ?\C-v (concat TeX-esc "var{") "}")
		(list ?k    (concat TeX-esc "key{") "}")
		(list ?\C-k (concat TeX-esc "kbd{") "}")
		(list ?c    (concat TeX-esc "code{") "}")
		(list ?C    (concat TeX-esc "cite{") "}")))

    ;; need the following stuff to let xref and pxref work
    (make-local-variable 'LaTeX-section-label)
    (setq LaTeX-section-label ""))))

;;; latexinfo.el ends here
