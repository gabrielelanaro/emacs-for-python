;;; english.el --- Setup AUCTeX for editing English text.

;;; Code:

(TeX-add-style-hook
 "english"
 (lambda ()
   (run-hooks 'TeX-language-en-hook))
 LaTeX-dialect)

;;; english.el ends here
