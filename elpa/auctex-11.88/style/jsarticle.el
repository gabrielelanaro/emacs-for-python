;;; jsarticle.el - Special code for jsarticle style.

;;; Code:

(TeX-add-style-hook
 "jsarticle"
 (lambda ()
   (LaTeX-largest-level-set "section"))
 LaTeX-dialect)

;;; jsarticle.el ends here
