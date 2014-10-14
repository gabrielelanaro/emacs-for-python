;;; report.el - Special code for report style.

;; $Id: report.el,v 1.3 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "report"
 (lambda () 
   (LaTeX-largest-level-set "chapter")))

;;; report.el ends here
