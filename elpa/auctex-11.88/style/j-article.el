;;; j-article.el - Special code for j-article style.

;;; Code:

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section"))
 LaTeX-dialect)

;;; j-article.el ends here
