;;; danish.el --- Setup AUCTeX for editing Danish text.

;;; Code:

(TeX-add-style-hook
 "danish"
 (lambda ()
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language `("danish" "\"`" "\"'" ,TeX-quote-after-quote)))
   (setq LaTeX-babel-hyphen-language "danish")
   ;; Fontification of quotation marks.
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\">" "\"<" german)))
   (run-hooks 'TeX-language-dk-hook))
 LaTeX-dialect)

;;; danish.el ends here
