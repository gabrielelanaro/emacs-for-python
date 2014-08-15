;;; jarticle.el - Special code for jarticle style.

;; $Id: jarticle.el,v 1.4 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "jarticle"
 (lambda ()
   (LaTeX-largest-level-set "section")))

;;; jarticle.el ends here
