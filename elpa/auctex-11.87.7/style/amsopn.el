;;; amsopn.el --- Style hook for the AMS-LaTeX amsopn package.
;;;
;;; AUTHOR: Carsten Dominik <dominik@strw.leidenuniv.nl>

;;; Code:

(TeX-add-style-hook "amsopn"
 (function
  (lambda ()
    (TeX-add-symbols
     '("DeclareMathOperator"  "Operator (with \\)" "Text")
     '("DeclareMathOperator*" "Operator (with \\)" "Text")
     '("operatorname" t)
     '("operatorname*" t)))))

(defvar LaTeX-amsopn-package-options '("namelimits" "nonamelimits")
  "Package options for the amsopn package.")

;;; amsopn.el ends here.
