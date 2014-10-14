;;; psfig.el - Support for the psfig style option.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Contributed by Marc Gemis <makke@wins.uia.ac.be>
;; Please direct comments to him.

;;; Code:

(TeX-add-style-hook "psfig"
 (function
  (lambda ()
	;; probable some of the following symbols may be removed
    (TeX-add-symbols "protect" "figurepath"  "fbox"
		     "other" "letter" "other" "then" "Sine" "Cosine"
		     "psdraft" "psfull" "psscalefirst" "psrotatefirst"
		     "psnodraftbox" "psdraftbox" "pssilent" "psnoisy"
		     "minmaxtest"
     '("psfig" TeX-arg-psfig)
     '("psfigurepath" t)
		     )
    (LaTeX-add-environments
     '("psfigure" LaTeX-env-psfigure)
     )
    )))

(defun TeX-arg-psfig (optional)
   "Ask for file, width and length. Insert psfig macro"
   (let ((psfile (read-file-name "PS-file: " "" "" nil))
	 (figwidth (read-string "Figure width: "))
	 (figheight (read-string "Figure height: "))
	 )

     (insert TeX-grop "figure=" psfile)
     (if (not (zerop (length figwidth)))
	 (insert ",width=" figwidth))
     (if (not (zerop (length figheight)))
	 (insert ",height=" figheight))
     (insert TeX-grcl)
     )
   )


(defun LaTeX-env-psfigure (environment)
  "Create  with \\label and \\caption and \\psfig commands."
  (let ((float (read-string "Float to: " LaTeX-float))
	(caption (read-string "Caption: "))
	(label (read-string "Label: " LaTeX-figure-label))
        ; gf: ask if this should be centered
	(psfile (read-file-name "PS-file: " "" "" nil))
	(figwidth (read-string "Figure width: "))
	(figheight (read-string "Figure height: "))
	)

    (setq LaTeX-float (if (zerop (length float))
			  LaTeX-float
			float))

    (LaTeX-insert-environment "figure"
			      (concat LaTeX-optop LaTeX-float LaTeX-optcl))

    (insert TeX-esc "centerline" TeX-grop TeX-esc "psfig" TeX-grop
	    "figure=" psfile)
    (if (not (zerop (length figwidth)))
	(insert ",width=" figwidth))
    (if (not (zerop (length figheight)))
	(insert ",height=" figheight))
    (insert TeX-grcl TeX-grcl)
    (if (zerop (length caption))
	()
      (newline-and-indent)
      (insert TeX-esc "caption" TeX-grop caption TeX-grcl))
    (if (or (zerop (length label))
	    (equal LaTeX-figure-label label))
	()
      (newline-and-indent)
      (insert TeX-esc "label" TeX-grop label TeX-grcl))

    (forward-line 2)))

;;; psfig.el ends here
