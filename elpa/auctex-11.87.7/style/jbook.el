;;; jbook.el - Special code for jbook style.

;; $Id: jbook.el,v 1.3 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "jbook"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; jbook.el ends here
