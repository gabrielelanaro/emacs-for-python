;;; slovak.el --- Setup AUCTeX for editing Slovak text.

(TeX-add-style-hook
 "slovak"
 (lambda ()
   (unless (eq (car TeX-quote-language) 'override)
     (setq TeX-quote-language `("slovak" "\\uv{" "}" ,TeX-quote-after-quote)))
   (when (fboundp 'font-latex-add-quotes)
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\"<" "\">" french)))
   (run-hooks 'TeX-language-sk-hook)))
