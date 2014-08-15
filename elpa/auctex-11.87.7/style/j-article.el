;;; j-article.el - Special code for j-article style.

;; $Id: j-article.el,v 1.4 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "j-article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; j-article.el ends here
