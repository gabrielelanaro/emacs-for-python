;;; jreport.el - Special code for jreport style.

;; $Id: jreport.el,v 1.3 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "jreport"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))


;;; jreport.el ends here
