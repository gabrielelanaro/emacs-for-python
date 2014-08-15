;;; jsarticle.el - Special code for jsarticle style.

;; $Id: jsarticle.el,v 1.2 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "jsarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jsarticle.el ends here
