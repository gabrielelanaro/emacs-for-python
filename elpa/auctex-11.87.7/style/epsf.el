;;; epsf.el - Support for the epsf style option.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Contributed by Marc Gemis <makke@wins.uia.ac.be>

;;; Code: 

(TeX-add-style-hook
 "epsf"
 (lambda ()
   (TeX-add-symbols
    '("epsfsize" TeX-arg-epsfsize)
    '("epsffile" TeX-arg-file)
    '("epsfbox" TeX-arg-file)
    "epsflly" "epsfury" "testit" "epsfgetlitbb"
    "epsfnormal" "epsfgetbb" "other" "epsfsetgraph"
    "PsFragSpecialArgs" "epsfaux" "testit" "epsfgrab"
    "epsfllx" "epsflly" "epsfury" "epsfverbosetrue")))

(defun TeX-arg-epsfsize (optional &optional prompt definition)
  "Create a line that print epsf figures at a certain percentage"
  (interactive)
  (let ((scale (read-string "Scale in percent (default 75): ")))
    (setq scale (if (zerop (length scale)) "75" scale))
    (save-excursion
      ; append #1#{scale#1}
      (insert "#1#2" TeX-grop "0." scale "#1" TeX-grcl)
      ; insert \def before \epsfsize
      (beginning-of-line 1)
      (newline)
      (insert TeX-esc "def")
      (forward-line -1)
      (insert "% From now on print figures at " scale "% of original size"))
    (end-of-line)))

;;; epsf.el ends here
