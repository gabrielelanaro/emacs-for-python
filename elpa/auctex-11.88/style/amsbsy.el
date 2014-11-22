;;; amsbsy.el --- Style hook for the AMS-LaTeX amsbsy package.
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(TeX-add-style-hook "amsbsy"
 (function
  (lambda ()
    (TeX-add-symbols
     '("boldsymbol" "Symbol")
     '("pmb"        "Symbol")
     )))
 LaTeX-dialect)

(defvar LaTeX-amsbsy-package-options nil
  "Package options for the amsbsy package.")

;;; amsbsy.el ends here.
