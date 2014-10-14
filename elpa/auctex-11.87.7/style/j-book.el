;;; j-book.el - Special code for j-book style.

;; $Id: j-book.el,v 1.3 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "j-book"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-book.el ends here
