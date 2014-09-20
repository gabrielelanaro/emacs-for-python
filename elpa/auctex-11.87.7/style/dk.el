;;; dk.el - Setup AUC TeX for editing Danish text.

;; $Id: dk.el,v 1.2 1993-12-15 21:42:40 amanda Exp $

;;; Code:

(TeX-add-style-hook "dk"
 (function (lambda ()
   (run-hooks 'TeX-language-dk-hook))))

;;; dk.el ends here
