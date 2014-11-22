;;; jbook.el - Special code for jbook style.

;;; Code:

(TeX-add-style-hook
 "jbook"
 (lambda ()
   (LaTeX-largest-level-set "chapter"))
 LaTeX-dialect)

;;; jbook.el ends here
