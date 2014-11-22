;;; jarticle.el - Special code for jarticle style.

;;; Code:

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section"))
 LaTeX-dialect)

;;; jarticle.el ends here
