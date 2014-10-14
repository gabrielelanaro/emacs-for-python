;;; article.el - Special code for article style.

;; $Id: article.el,v 1.4 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "article"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; article.el ends here
