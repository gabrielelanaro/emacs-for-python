;;; jsbook.el - Special code for jsbook style.

;;; Code:

(TeX-add-style-hook
 "jsbook"
 (lambda () 
   (LaTeX-largest-level-set "chapter"))
 LaTeX-dialect)

;;; jsbook.el ends here
