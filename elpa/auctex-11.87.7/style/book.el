;;; book.el - Special code for book style.

;; $Id: book.el,v 1.5 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; book.el ends here
