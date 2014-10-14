;;; amsbook.el --- Style hook for the AMS-LaTeX book document class.

;;; Code:

(TeX-add-style-hook "amsbook"
 (function
  (lambda ()
    (TeX-run-style-hooks "amsmath" "amsthm"))))

;;; amsbook.el ends here.
