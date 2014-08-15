;;; prosper.el --- Prosper style file for AUCTeX

;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Authors:  Phillip Lord<p.lord@russet.org.uk>
;;           Nevin Kapur <nevin@jhu.edu>
;; Keywords: tex, wp, prosper
;; Version: 0.6
;; URL: http://www.mts.jhu.edu/~kapur/emacs/prosper.el

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; This is a propser (http://prosper.sourceforge.net/) style file for
;; AUCTeX.

;;; Installation: 
;; 
;; For this file to work you need to have a working installation of 
;; AucTeX. After that installtion is simple. Put this file into one of 
;; the directories specified in `TeX-style-path', with the name 
;; "style" rather than "auto" as it might get over written in the 
;; latter.  
;; 
;; Then stick the current for into your .emacs 
;; (eval-after-load "latex" 
;;   '(add-to-list 'LaTeX-style-list '("prosper"))) 
;;  
;;
;; And that should be it. You check whether it's worked or not by 
;; opening a prosper document, and trying `LaTeX-environment'. "slide" 
;; should be available by tab completion and it should ask you about 
;; overlays.  
;;
;; The environment "prosper" should be inserted immediately after the
;; document environment.  It will prompt you for options available
;; under prosper and create a skeleton document.

;;; Bugs:
;;
;; Currently the documentclass expansion doesn't work, unless you
;; enter a documentclass line to let auctex know which style files to
;; load. Then delete this and do it again. Not good. I know no way
;; around this. 

;;; Code:

;; Constants:


;;;; This is partly working now, and it a little neater than it
;;;; was. The main problem is that the redefinition of "documentclass"
;;;; does not happen until its all too late, so that stuff never
;;;; happens correctly. This is easy enough to fix by fiddling with
;;;; auctex. I shall have to download the latest version, and see if
;;;; its already been fixed.



(defconst LaTeX-prosper-version
  "$Id: prosper.el,v 1.5 2008-05-25 06:50:33 angeli Exp $"
  "prosper.el version.")

(defconst LaTeX-prosper-transition-styles '("Split"
					  "Blinds"
					  "Box"
					  "Wipe"
					  "Dissolve"
					  "Glitter"
					  "Replace")
  "List of transition styles provided by prosper.")

(defconst LaTeX-prosper-slide-styles
  '("alienglow" "autumn" "azure"
    "contemporain" "darkblue" "default" "frames"
    "lignesbleues" "nuancegris" "troispoints"
    "alcatel" "gyom" "pascal" "rico"    
    ))

(defun LaTeX-prosper-insert-title (optional)
  (newline)
  (mapc (lambda(f)
	  (TeX-insert-macro f)
	  (newline))
	'("title" "subtitle" "author" "email" "institution" "slideCaption"
	  "Logo" "DefaultTransition"))
  (LaTeX-insert-environment "document")
  (TeX-insert-macro "maketitle"))


;; Utility functions
(defun LaTeX-prosper-arg-pdftransition (environment)
  (let ((default
          (if (boundp 'LaTeX-prosper-transition-history)
              (car LaTeX-prosper-transition-history)
            "Replace")))
    (TeX-argument-insert
     (completing-read 
      (TeX-argument-prompt nil 
                           (format "Transition (Default %s) " default)
                           t)
      (mapcar 'list LaTeX-prosper-transition-styles)
      nil
      t
      nil
      'LaTeX-prosper-transition-history
      default)
     nil)))

(defun LaTeX-prosper-slide-style-prompt()
  (completing-read
   "Slide Style?"
   (mapcar 'list LaTeX-prosper-slide-styles)
   nil nil nil nil "default" ))
   
   
(defun LaTeX-prosper-insert-options(environment)
  (insert "[" )
  (insert (LaTeX-prosper-slide-style-prompt) " ")
  (mapc (lambda(f)
	  (if (y-or-n-p (car f))
	      (insert (car (cdr f)) " ")))
	'(("Draft?" "draft")
	  ("Color Slides?" "slideColor")
	  ("Disable running total on each slide?" "nototal")
	  ("Is the final version going to be PDF?" "pdf")
	  ("Are you going to use Adobe Distiller" "distiller")))
  (delete-char -1)
  (insert "]"))

(defun LaTeX-prosper-insert-slide (environment) 
  (if (y-or-n-p "Surround with overlay ?") 
      (progn (TeX-insert-macro "overlays") 
             (if (search-backward "{" 0 t) 
                 (progn 
                   (goto-char (+ 1 (point))) 
                   (insert "%\n"))))) 
  (let ((title (read-string "Title: "))) 
    (LaTeX-insert-environment "slide" (concat TeX-grop title TeX-grcl)))) 



;; AUCTeX configuration
(TeX-add-style-hook "prosper"
		    (function
		     (lambda ()
		       (LaTeX-add-environments
			'("slide" LaTeX-prosper-insert-slide)
			'("itemstep" LaTeX-env-item)
			'("Itemize" LaTeX-env-item))
                       (TeX-add-symbols
                        '("documentclass" 
                          LaTeX-prosper-insert-options
                          LaTeX-prosper-insert-title)
                        '("title" "Title of the presentation")
			'("subtitle" "Subtitle of the presentation")
			'("author" "Author name")
			'("email" "Author email")
			'("institution" "Author institution")
			'("slideCaption" "Caption for slide")
			'("Logo" "Logo")
			'("displayVersion" TeX-arg-free)
			'("DefaultTransition"
			  LaTeX-prosper-arg-pdftransition)
			'("NoFrenchBabelItemize" TeX-arg-free)
			'("part" LaTeX-prosper-arg-part)
			'("overlays" "Number of overlays" t)
			'("FontTitle" "Color slides" "Black & White Slides")
			'("FontText" "Color slides" "Black & White Slides")
			'("fontTitle" "Text")
			'("fontText" "Text")
			'("ColorFoot" "Color")
			'("PDFtransition" LaTeX-prosper-arg-pdftransition)
			'("myitem" "Level" "Definition")
			'("fromSlide" "Number" t)
			'("fromSlide*" "Number" t)
			'("onlySlide" "Number" t)
			'("onlySlide*" "Number" t)
			'("OnlySlide" "Number")
			'("UntilSlide" "Number")
			'("untilSlide*" "Number")
			'("PDForPS" TeX-arg-conditional)
			'("onlyInPS" t)
			'("onlyInPDF" t)
			'("FromSlide" "Number")))))


;;; prosper.el ends here
