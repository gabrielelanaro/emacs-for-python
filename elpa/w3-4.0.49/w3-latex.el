;;; w3-latex.el --- Emacs-W3 printing via LaTeX

;; Copyright (c) 1996-1997, 2013  Free Software Foundation, Inc.

;; Author: wmperry
;; Created: 1996/06/30 18:08:34
;; Keywords: hypermedia, printing, typesetting

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

;; Elisp code to convert a W3 parse tree into a LaTeX buffer.
;;
;; Heavily hacked upon by William Perry <wmperry@cs.indiana.edu> to add more
;; bells and whistles.
;;
;; KNOWN BUGS:
;; 1) This does not use stylesheets to get the formatting information
;; 2) This means that the new drawing routines need to be abstracted
;;    further so that the same main engine can be used for either
;;    text-output (standard stuff in w3-draw), LaTeX output (this file),
;;    Postscript (to-be-implemented), etc., etc.
;; 3) This still doesn't handle tables.
;;

;;; Code:

(require 'w3-cus)
(require 'w3-print)
(require 'w3-vars)
(require 'url-vars)

;; Internal variables - do not touch!
(defvar w3-latex-current-url nil "What URL we are formatting")
(defvar w3-latex-verbatim nil "Whether we are in a {verbatim} block or not")
(defvar w3-latex-links-list nil "List of links for endnote usage")

(defvar w3-latex-entities
  '((nbsp . "~")
    (iexcl . "!`")
;   (cent . "")
    (pound . "\\pounds ")
;   (curren . "")
;   (yen . "")
    (brvbar . "|")
    (sect . "\\S")
    (uml . "\\\"{ }")
    (copy . "\\copyright ")
;   (ordf . "")
    (laquo . "$\\ll$")
    (not . "\\neg")
    (shy . "-")
    (reg . "(R)")
    (macr . "\\={ }")
    (deg . "$\\deg$")
    (plusmn . "$\\pm$")
    (sup2 . "$^{2}$")
    (sup3 . "$^{3}$")
    (acute . "\\'{ }")
    (micro . "$\\mu$")
    (para . "\\P ")
    (middot . "$\\cdot$")
    (cedil . "\\c{ }")
    (sup1 . "$^{1}$")
;   (ordm . "")
    (raquo . "$\\gg$")
    (frac14 . "$\frac{1}{4}$")
    (frac12 . "$\frac{1}{2}$")
    (frac34 . "$\frac{3}{4}$")
    (iquest . "?`")
    (Agrave . "\\`{A}")
    (Aacute . "\\'{A}")
    (Acirc . "\\^{A}")
    (Atilde . "\\~{A}")
    (Auml . "\\\"{A}")
    (Aring . "\\AA ")
    (AElig . "\\AE ")
    (Ccedil . "\\c{C}")
    (Egrave . "\\`{E}")
    (Eacute . "\\'{E}")
    (Ecirc . "\\^{E}")
    (Euml . "\\\"{E}")
    (Igrave . "\\`{I}")
    (Iacute . "\\'{I}")
    (Icirc . "\\^{I}")
    (Iuml . "\\\"{I}")
;   (ETH . "")
    (Ntilde . "\\~{N}")
    (Ograve . "\\`{O}")
    (Oacute . "\\'{O}")
    (Ocirc . "\\^{O}")
    (Otilde . "\\~{O}")
    (Ouml . "\\\"{O}")
    (times . "$\\times$")
    (Oslash . "\\O")
    (Ugrave . "\\`{U}")
    (Uacute . "\\'{U}")
    (Ucirc . "\\^{U}")
    (Uuml . "\\\"{U}")
    (Yacute . "\\'{Y}")
;   (THORN . "")
    (szlig . "\\ss ")
    (agrave . "\\`{a}")
    (aacute . "\\'{a}")
    (acirc . "\\^{a}")
    (atilde . "\\~{a}")
    (auml . "\\\"{a}")
    (aring . "\\aa ")
    (aelig . "\\ae ")
    (ccedil . "\\c{c}")
    (egrave . "\\`{e}")
    (eacute . "\\'{e}")
    (ecirc . "\\^{e}")
    (euml . "\\\"{e}")
    (igrave . "\\`{i}")
    (iacute . "\\'{i}")
    (icirc . "\\^{i}")
    (iuml . "\\\"{i}")
;   (eth . "")
    (ntilde . "\\~{n}")
    (ograve . "\\`{o}")
    (oacute . "\\'{o}")
    (ocirc . "\\^{o}")
    (otilde . "\\~{o}")
    (ouml . "\\\"{o}")
    (divide . "$\\div$")
    (oslash . "\\o")
    (ugrave . "\\`{u}")
    (uacute . "\\'{u}")
    (ucirc . "\\^{u}")
    (uuml . "\\\"{u}")
    (yacute . "\\'{y}")
;   (thorn . "")
    (yuml . "\\\"{y}"))
  "Defines mappings between `w3-html-entities' and LaTeX characters.")

(defvar w3-latex-use-maketitle t)
(defvar w3-latex-print-links t
  "Can be nil, t, or `footnote'.")
(defvar w3-latex-use-latex2e t)
(defvar w3-latex-docstyle "{article}")
(defvar w3-latex-packages nil)

(defun w3-latex-replace-entities (str)
  (let ((start 0))
    (while (string-match "[\200-\377]" str start)
      ; get the character code, and then search for a match in
      ; w3-html-entities.  If one is found, use it to perform a lookup
      ; in w3-latex-entities, and use the resulting match to replace
      ; the character.
      (let* ((match (rassq (aref str (match-beginning 0))
			   w3-html-entities))
	     (replace (and match
			   (assq (car match) w3-latex-entities))))
	(if replace
	    (setq str (replace-match (cdr replace) t t str)))
	(setq start (match-end 0))))
    str))

(defun w3-latex-insert-string (str)
  ;;; convert string to a LaTeX-compatible one.
  (let ((todo (list (cons "\\\\"          "-BaCkSlAsH-")
		    (cons "[%&#_{}$]"     "\\\\\\&")
		    (cons "\\^"           "{\\\\textasciicircum}")
		    (cons "~"             "{\\\\textasciitilde}")
		    (cons "[*]"           "{\\&}")
		    (cons "[><|]"         "$\\&$")
		    (cons "-BaCkSlAsH-"   "$\\\\backslash$"))))
    (if w3-latex-verbatim
	(setq todo (append todo '(("\n" . "\\\\newline\\\\nullspace\n")
				  (" " . "\\\\ ")))))
    (with-current-buffer (get-buffer-create " *w3-latex-munging*")
      (erase-buffer)
      (insert str)
      (while todo
	(goto-char (point-min))
	(while (re-search-forward (caar todo) nil t)
	  (replace-match (cdar todo)))
	(setq todo (cdr todo)))
      (setq str (w3-latex-replace-entities (buffer-string))))
    (insert str)))

(defun w3-latex-ignore (_tree)
  ;;; ignores any contents of this tree.
  nil)

(defun w3-latex-contents (tree)
  ;;; passes contents of subtree through to the latex-subtree
  (let ((contents (car (cdr (cdr tree)))))
    (while contents
      (w3-latex-subtree (car contents))
      (setq contents (cdr contents)))))

(defun w3-latex-html (tree)
  (insert "% This document automatically generated by Emacs-W3 v"
	  w3-version-number "\n")
  (if w3-latex-current-url
      (insert "% from <URL:" w3-latex-current-url ">\n"))
  (insert "%\n"
	  "\\batchmode\n\\begin{document}\n")
  (insert "\\setlength{\\parindent}{0pt}\n"
	  "\\setlength{\\parskip}{1.5ex}\n")
  (insert "\\newcommand{\\nullspace}{\\rule{0pt}{0pt}}")
  (w3-latex-contents tree)
  (if w3-latex-links-list (w3-latex-endnotes))
  (insert "\\end{document}\n"))

(defun w3-latex-title (tree)
  (insert (if w3-latex-use-maketitle
              "\\title{"
            "\\section*{\\centering "))
  (w3-latex-contents tree)
  (insert "}\n")
  (if w3-latex-use-maketitle
      (insert "\\author{}\\date{}\n\\maketitle")))

(defun w3-latex-heading (tree)
  ;; look through the additional markup to see if an align=right or
  ;; align=center is in here...
  (let ((align (assq 'align (car (cdr tree))))
	(sym (car tree)))
    (insert "\n\n")
    (cond ((and align (string-equal (cdr align) "center"))
	   (insert "\\begin{center}\n"))
	  ((and align (string-equal (cdr align) "right"))
	   (insert "\\begin{flushright}\n")))
    (cond ((eq sym 'h1) (insert "\\section*{"))
	  ((eq sym 'h2) (insert "\\subsection*{"))
	  ((eq sym 'h3) (insert "\\subsubsection*{"))
	  ((eq sym 'h4) (insert "\\subsubsection*{"))
	  ((eq sym 'h5) (insert "\\paragraph*{"))
	  ((eq sym 'h6) (insert "\\subparagraph*{")))
    (w3-latex-contents tree)
    (insert "}\n")
    (cond ((and align (string-equal (cdr align) "center"))
	   (insert "\\end{center}\n"))
	  ((and align (string-equal (cdr align) "right"))
	   (insert "\\end{flushright}\n")))))

(defun w3-latex-bold (tree)
  (insert "{\\bf ")
  (w3-latex-contents tree)
  (insert "}"))
(defun w3-latex-italic (tree)
  (insert "{\\em ")
  (w3-latex-contents tree)
  (insert "}"))
(defun w3-latex-typewriter (tree)
  (insert "{\\tt ")
  (w3-latex-contents tree)
  (insert "}"))

(defun w3-latex-list (tree)
  (let* ((sym (car tree))
	 (list-type (cond ((eq sym 'ol) "enumerate")
			  ((eq sym 'dl) "description")
			  (t "itemize"))))
    (insert (concat "\n\\begin{" list-type "}\n"))
    (w3-latex-contents tree)
    (insert (concat "\n\\end{" list-type "}\n"))))

(defun w3-latex-list-item (tree)
  (let ((sym (car tree)))
    (cond ((eq sym 'dt)
	   (insert "\n\\item["))
	  ((eq sym 'dd)
	   ;; don't do anything for dd -- the item is handled by dt.
	   nil)
	  (t (insert "\n\\item")))
    (w3-latex-contents tree)
    (if (eq sym 'dt)
	(insert "]"))))

(defun w3-latex-center (tree)
  (insert "\\begin{center}")
  (w3-latex-contents tree)
  (insert "\\end{center}"))

(defun w3-latex-rule (_tree)
  ; use \par to make paragraph division clear.
  (insert "\n\\par\\noindent\\rule{\\textwidth}{.01in}\n"))

(defun w3-latex-para (tree)
  ;; look through the additional markup to see if an align=right or
  ;; align=center is in here...
  (let ((align (assq 'align (car (cdr tree)))))
    (cond ((and align
		(string-equal (cdr align) "center"))
	   (w3-latex-center tree))
	  ((and align
		(string-equal (cdr align) "right"))
	   (insert "\\begin{flushright}")
	   (w3-latex-contents tree)
	   (insert "\\end{flushright}"))
	  (t (insert "\\par ")
	     (w3-latex-contents tree)))))

(defun w3-latex-quote (tree)
  (insert "\\begin{quote}\n")
  (w3-latex-contents tree)
  (insert "\\end{quote}\n"))

(defun w3-latex-break (_tree)
  ;; no content allowed
  (insert "\\newline "))

(defun w3-latex-endnotes ()
  (let ((i 1))
    (insert "\\begin{thebibliography}{99}\n")
    (while w3-latex-links-list
      (insert (concat "\\bibitem{ref" (number-to-string i) "}"))
      (w3-latex-insert-string (car w3-latex-links-list))
      (insert "\n")
      (setq w3-latex-links-list (cdr w3-latex-links-list))
      (setq i (1+ i)))
    (insert "\\end{thebibliography}\n")))

(defun w3-latex-href (tree)
  (let ((href (cdr-safe (assq 'href (cadr tree))))
	(name (cdr-safe (assq 'name (cadr tree)))))
    (cond
     ((not w3-latex-print-links)	; No special treatment
      (w3-latex-contents tree))
     (name
      (w3-latex-contents tree)
      (insert (concat "\\label{" name "}")))
     (href				; Special treatment requested
;      (insert "\\underline{")		; and we have a URL - underline
      (w3-latex-contents tree)		; it.
;      (insert "}")
      (cond 
       ((char-equal ?# (aref href 0))
	(insert (concat " (see page~\\pageref{"
			(substring href 1)
			"})")))
       ((eq w3-latex-print-links 'footnote)
	(insert "\\footnote{")		; Request to prepare footnote 
	(w3-latex-insert-string href)
	(insert "}"))
       (t				; Otherwise, prepare endnotes
	(let ((mem (member href w3-latex-links-list))
	      (i (1+ (length w3-latex-links-list))))
	  (if mem
	      (setq i (- i (length mem)))
	    (setq w3-latex-links-list
		  (append w3-latex-links-list (cons href nil))))
	  (insert (concat "~\\cite{ref" (number-to-string i) "}"))))))
     (t					; Special treatment requested, but
      (w3-latex-contents tree)))))	; no URL - do nothing.

(defun w3-latex-preformatted (tree)
  (let ((w3-latex-verbatim t))
    (insert "\\par\\noindent\\begin{tt}")
    (w3-latex-contents tree)
    (insert "\\end{tt}\\par")
    ))

(defun w3-latex-xmp (tree)
  (insert "\\begin{verbatim}")
  (w3-latex-contents tree)
  (insert "\\end{verbatim}"))

(let ((todo '((title . w3-latex-title)
	      (html . w3-latex-html)
	      (pre . w3-latex-preformatted)
	      (xmp . w3-latex-xmp)
	      (h1 . w3-latex-heading)
	      (h2 . w3-latex-heading)
	      (h3 . w3-latex-heading)
	      (h4 . w3-latex-heading)
	      (h5 . w3-latex-heading)
	      (h6 . w3-latex-heading)
	      (a  . w3-latex-href)
	      (strong . w3-latex-bold)
	      (b . w3-latex-bold)
	      (dfn . w3-latex-bold)
	      (em . w3-latex-italic)
	      (i . w3-latex-italic)
	      (address . w3-latex-italic)
	      (code . w3-latex-typewriter)
	      (samp . w3-latex-typewriter) 
	      (tt . w3-latex-typewriter)
	      (kbd . w3-latex-typewriter) 
	      (var . w3-latex-typewriter)
	      (ol . w3-latex-list)
	      (dl . w3-latex-list)
	      (ul . w3-latex-list) 
	      (menu . w3-latex-list)
	      (dir . w3-latex-list)
	      (li . w3-latex-list-item)
	      (dt . w3-latex-list-item) 
	      (dd . w3-latex-list-item)
	      (center . w3-latex-center)
	      (hr . w3-latex-rule)
	      (p . w3-latex-para)
	      (br . w3-latex-break)
	      (blockquote . w3-latex-quote))))
  (while todo
    (put (caar todo) 'w3-latex-formatter (cdar todo))
    (setq todo (cdr todo))))

(defun w3-latex-subtree (tree)
  (cond
   ((stringp tree)
    (w3-latex-insert-string tree))
   ((stringp (car-safe tree))
    (while tree
      (w3-latex-insert-string (car tree))
      (setq tree (cdr tree))))
   ((symbolp (car tree))
    (let ((proc (get (car tree) 'w3-latex-formatter)))
      (if (and proc (fboundp proc))
	  (funcall proc tree)
	;; anything else gets passed through unchanged
	(w3-latex-contents tree))))
   (t
    (w3-latex-contents tree))))

;; Fixme: url-working-buffer is never defined.
;;;###autoload
(defun w3-parse-tree-to-latex (tree &optional url)
  ; assumes that url-working-buffer exists.
  (set-buffer (get-buffer-create url-working-buffer))
  (setq w3-latex-current-url url)
  (erase-buffer)
  (goto-char (point-min))
  (insert (if w3-latex-use-latex2e
              "\\documentclass"
            "\\documentstyle")
          w3-latex-docstyle "\n")
  (if (and w3-latex-use-latex2e
	   w3-latex-packages)
      (insert (apply 'concat
		     (mapcar (lambda (x) (concat "\\usepackage{" x "}\n"))
			     w3-latex-packages))))
  (while tree
    (w3-latex-subtree (car tree))
    (setq tree (cdr tree))))

;;;###autoload
(defun w3-show-dvi ()
  "Uses xdvi to show DVI file created from `w3-parse-tree-to-latex'."
  (interactive)
  (w3-parse-tree-to-latex w3-current-parse)
  (save-window-excursion
    (set-buffer url-working-buffer)
    (let ((coding-system-for-write 'binary)
          (file (make-temp-file "w3" nil ".tex")))
      (write-region
       (point-min) (point-max) file nil 5)
      ;; FIXME: Race-condition.  We should run this in temporary-file-directory!
      ;; (let ((default-directory (file-name-directory file)))
      ;;   (call-process "latex" nil t nil (file-relative-name file))
      ;;   (call-process "latex" nil t nil (file-relative-name file))
      (delete-file file)
      (error "Not implemented")
      ;;   (call-process "xdvi" nil nil nil
      ;;                 (replace-regexp-in-string "\\.tex\\'" ".dvi"
      ;;                                           (file-relative-name file)))
      ;;   (shell-command
      ;;    (format 
      ;;     "(cd %s ; latex w3-tmp.latex ; latex w3-tmp.latex ; xdvi w3-tmp.dvi ; rm -f w3-tmp*) &"
      ;;     temporary-file-directory)))
      )))

(provide 'w3-latex)
