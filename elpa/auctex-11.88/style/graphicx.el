;;; graphicx.el --- AUCTeX style file for graphicx.sty

;; Copyright (C) 2000, 2004, 2005 by Free Software Foundation, Inc.

;; Author: Ryuichi Arafune <arafune@debian.org>
;; Created: 1999/3/20
;; Keywords: tex

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;  This package supports the includegraphcics macro in graphicx style.

;; Acknowledgements
;;  Dr. Thomas Baumann <thomas.baumann@ch.tum.de>
;;  David Kastrup <David.Kastrup@t-online.de>
;;  Masayuki Akata <ataka@milk.freemail.ne.jp>

;;; Code:

(TeX-add-style-hook
 "graphicx"
 (lambda ()
   (TeX-add-symbols
    '("reflectbox" "Argument")
    '("resizebox" "Width" "Height" "Argument")
    '("resizebox*" "Width" "Total height" "Argument")
    '("rotatebox" [ "Options" ] "Angle" "Argument")
    '("scalebox" "Horizontal scale" [ "Vertical scale" ] "Argument")
    '("includegraphics" LaTeX-arg-includegraphics))
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("reflectbox" "{")
				("resizebox" "*{{{")
				("rotatebox" "[{{")
				("scalebox" "{[{"))
			      'textual)
     (font-latex-add-keywords '(("includegraphics" "*[[{")) 'reference)))
 LaTeX-dialect)

(defun LaTeX-includegraphics-extensions (&optional list)
  "Return appropriate extensions for input files to \\includegraphics."
  ;; FIXME: This function may check for latex/pdflatex later.
  (concat "\\."
	  (mapconcat 'identity
		     (or list LaTeX-includegraphics-extensions)
		     "$\\|\\.")
	  "$"))

(defun LaTeX-includegraphics-read-file-TeX ()
  "Read image file for \\includegraphics.
Offers all graphic files found in the TeX search path.  See
`LaTeX-includegraphics-read-file' for more."
  ;; Drop latex/pdflatex differences for now.  Might be (re-)included later.
  (completing-read
   "Image file: "
   (TeX-delete-dups-by-car
    (mapcar 'list
	    (TeX-search-files nil LaTeX-includegraphics-extensions t t)))
   nil nil nil))

(defun LaTeX-includegraphics-read-file-relative ()
  "Read image file for \\includegraphics.

Lists all graphic files in the master directory and its
subdirectories and inserts the relative file name.  This option
doesn't works with Emacs 21.3 or XEmacs.  See
`LaTeX-includegraphics-read-file' for more."
  (file-relative-name
   (read-file-name
    "Image file: " nil nil nil nil
    ;; FIXME: Emacs 21.3 and XEmacs 21.4.15 dont have PREDICATE as the sixth
    ;; argument (Emacs 21.3: five args; XEmacs 21.4.15: sixth is HISTORY).
    (lambda (fname)
      (or (file-directory-p fname)
 	  (string-match (LaTeX-includegraphics-extensions) fname))))
   (TeX-master-directory)))

(defun LaTeX-arg-includegraphics (prefix)
  "Ask for mandantory and optional arguments for the \\includegraphics command.

The extent of the optional arguments is determined by the prefix argument and
`LaTeX-includegraphics-options-alist'."
  (let* ((maybe-left-brace "[")
	 (maybe-comma "")
	 show-hint
	 (image-file (funcall LaTeX-includegraphics-read-file))
	 (incl-opts
	  (cond
	   ((numberp
	     (if (listp current-prefix-arg)
		 (setq current-prefix-arg (car current-prefix-arg))
	       current-prefix-arg))
	    (cdr
	     (assq current-prefix-arg LaTeX-includegraphics-options-alist)))
	   ;; If no prefix is given, use `0' and tell the user about the
	   ;; prefix.
	   ((eq current-prefix-arg nil)
	    (setq show-hint t)
	    (cdr (assq 0 LaTeX-includegraphics-options-alist)))
	   (t
	    (cdr (assq 0 LaTeX-includegraphics-options-alist)))))
	 ;; Order the optional aruments like in the tables in epslatex.ps,
	 ;; page 14.  But collect y-or-n options at the end, so that the use
	 ;; can skip some options by typing `RET RET ... RET n n n ... n'
	 ;;
	 ;; Options from Table 1 (epslatex.ps, page 14):
	 (totalheight
	  (TeX-arg-maybe
	   'totalheight incl-opts
	   '(read-string
	     (concat "Total Height (" TeX-default-unit-for-image "): "))))
	 (height
	  (TeX-arg-maybe
	   'height incl-opts
	   ;; Either totalheight or height make sense:
	   '(when (zerop (length totalheight))
	      (read-string
	       (concat "Figure height (" TeX-default-unit-for-image "): ")))))
	 (width
	  (TeX-arg-maybe
	   'width incl-opts
	   '(read-string
	     (concat "Figure width (" TeX-default-unit-for-image "): "))))
	 (scale
	  (TeX-arg-maybe
	   'angle incl-opts
	   ;; If size is already specified, don't ask for scale:
	   '(when (zerop (+ (length totalheight)
			    (length height)
			    (length width)))
	      (read-string "Scale: "))))
	 (angle
	  (TeX-arg-maybe
	   'angle incl-opts
	   '(read-string "Rotation angle: ")))
	 (origin
	  (TeX-arg-maybe
	   'origin incl-opts
	   '(read-string
	     (concat
	      "Origin (any combination of `lcr' (horizontal) "
	      "and `tcbB' (vertical)): "))))
	 (page ;; Not in any table; Only for PDF.
	  (TeX-arg-maybe
	   'page incl-opts
	   '(read-string "Page: ")))
	 (bb
	  (TeX-arg-maybe
	   'bb incl-opts
	   '(y-or-n-p "Set Bounding Box? ")))
	 ;; Table 2:
	 (viewport
	  (TeX-arg-maybe
	   'viewport incl-opts
	   '(y-or-n-p "Set viewport? ")))
	 (trim
	  (TeX-arg-maybe
	   'trim incl-opts
	   '(and (not viewport)
		 (y-or-n-p "Set trim? "))))
	 ;; Table 3:
	 (clip
	  (TeX-arg-maybe
	   'clip incl-opts
	   ;; If viewport, we also use clip.
	   '(or viewport
		(y-or-n-p "Clipping figure? "))))
	 (keepaspectratio
	  (TeX-arg-maybe
	   'keepaspectratio incl-opts
	   ;; If we have width and [total]height...
	   '(or (and (not (zerop (length width)))
		     (or (not (zerop (length totalheight)))
			 (not (zerop (length height)))))
		(y-or-n-p "Keep Aspectratio? "))))
	 ;; Used for bb, trim, viewport, ...:
	 llx lly urx ury)
    ;; Now insert stuff...
    (when (not (zerop (length totalheight)))
      (insert
       maybe-left-brace maybe-comma "totalheight="
       (car (TeX-string-divide-number-unit totalheight))
       (if (zerop
	    (length
	     (car (cdr (TeX-string-divide-number-unit totalheight)))))
	   TeX-default-unit-for-image
	 (car (cdr (TeX-string-divide-number-unit totalheight)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length height)))
      (insert maybe-left-brace maybe-comma
	      "height=" (car (TeX-string-divide-number-unit height))
	      (if (zerop
		   (length
		    (car (cdr (TeX-string-divide-number-unit height)))))
		  TeX-default-unit-for-image
		(car (cdr (TeX-string-divide-number-unit height)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length width)))
      (insert maybe-left-brace maybe-comma
	      "width=" (car (TeX-string-divide-number-unit width))
	      (if (zerop
		   (length
		    (car (cdr (TeX-string-divide-number-unit width)))))
		  TeX-default-unit-for-image
		(car (cdr (TeX-string-divide-number-unit width)))))
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length scale)))
      (insert maybe-left-brace maybe-comma "scale=" scale)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length angle)))
      (insert maybe-left-brace maybe-comma "angle=" angle)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when (not (zerop (length origin)))
      (insert maybe-left-brace maybe-comma "origin=" origin)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when bb
      (setq llx (read-string "Bounding Box lower left x: "))
      (setq lly (read-string "Bounding Box lower left y: "))
      (setq urx (read-string "Bounding Box upper right x: "))
      (setq ury (read-string "Bounding Box upper right y: "))
      (insert maybe-left-brace maybe-comma
	      "bb=" llx " " lly " " urx " " ury)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (when viewport
      (setq llx (read-string "Viewport lower left x: "))
      (setq lly (read-string "Viewport lower left y: "))
      (setq urx (read-string "Viewport upper right x: "))
      (setq ury (read-string "Viewport upper right y: "))
      (insert maybe-left-brace maybe-comma
	      "viewport=" llx " " lly " " urx " " ury)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when trim
      (setq llx (read-string "Trim lower left x: "))
      (setq lly (read-string "Trim lower left y: "))
      (setq urx (read-string "Trim Upper right x: "))
      (setq ury (read-string "Trim Upper right y: "))
      (insert maybe-left-brace maybe-comma
	      "trim=" llx " " lly " " urx " " ury)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (when clip
      (insert maybe-left-brace maybe-comma "clip")
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    (when keepaspectratio
      (insert maybe-left-brace maybe-comma "keepaspectratio")
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (when (not (zerop (length page)))
      (insert maybe-left-brace maybe-comma "page=" page)
      (setq maybe-comma ",")
      (setq maybe-left-brace ""))
    ;;
    (if (zerop (length maybe-left-brace))
	(insert "]"))
    (TeX-insert-braces 0)
    (insert
     (if LaTeX-includegraphics-strip-extension-flag
	 ;; We don't have `replace-regexp-in-string' in all (X)Emacs versions:
	 (with-temp-buffer
	   (insert image-file)
	   (goto-char (point-max))
	   (when (search-backward-regexp (LaTeX-includegraphics-extensions)
					 nil t 1)
	     (replace-match ""))
	   (buffer-string))
       image-file))
    (when show-hint
      (message
       (concat
	"Adding `C-u C-u' before the command asks for more optional arguments."
	"\nSee `LaTeX-includegraphics-options-alist' for details."))
      (sit-for 3))
    t))

;;; graphicx.el ends here
