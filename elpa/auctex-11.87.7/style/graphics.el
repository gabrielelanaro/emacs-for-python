;;; graphics.el --- Handle graphical commands in LaTeX 2e.

;;; Code:

(TeX-add-style-hook "graphics"
 (function
  (lambda ()
    (TeX-run-style-hooks "graphicx"))))

;;; graphics.el ends here.
