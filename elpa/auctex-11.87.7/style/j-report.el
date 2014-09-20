;;; j-report.el - Special code for j-report style.

;; $Id: j-report.el,v 1.3 2005-03-17 10:02:06 angeli Exp $

;;; Code:

(TeX-add-style-hook
 "j-report"
 (lambda ()
   (LaTeX-largest-level-set "chapter")))

;;; j-report.el ends here
