;;; jsbook.el - Special code for jsbook style.

;; $Id: jsbook.el,v 1.2 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "jsbook"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; jsbook.el ends here
