;;; jreport.el - Special code for jreport style.

;;; Code:

(TeX-add-style-hook
 "jreport"
 (lambda ()
   (LaTeX-largest-level-set "chapter"))
 LaTeX-dialect)


;;; jreport.el ends here
