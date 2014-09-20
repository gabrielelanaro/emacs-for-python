;;; ngerman.el --- Setup AUCTeX for editing German text.

;;; Commentary:
;;
;; Cater for some specialities of `(n)german.sty', e.g. special quote
;; and hyphen strings or that `"' makes the following letter an
;; umlaut.

;;; Code:

(defvar LaTeX-german-mode-syntax-table
  (copy-syntax-table LaTeX-mode-syntax-table)
  "Syntax table used in LaTeX mode when using `german.sty'.")

(modify-syntax-entry ?\"  "w"  LaTeX-german-mode-syntax-table)

(TeX-add-style-hook
 "ngerman"
 (lambda ()
   (set-syntax-table LaTeX-german-mode-syntax-table)
   ;; XXX: Handle former customizations of the now defunct
   ;; German-specific variables.  References to the respective
   ;; variables are to be deleted in future versions. (now = 2005-04-01)
   (unless (eq (car TeX-quote-language) 'override)
     (let ((open-quote (if (and (boundp 'LaTeX-german-open-quote)
				LaTeX-german-open-quote)
			   LaTeX-german-open-quote
			 "\"`"))
	   (close-quote (if (and (boundp 'LaTeX-german-close-quote)
				 LaTeX-german-close-quote)
			    LaTeX-german-close-quote
			  "\"'"))
	   (q-after-q (if (and (boundp 'LaTeX-german-quote-after-quote)
			       LaTeX-german-quote-after-quote)
			  LaTeX-german-quote-after-quote
			t)))
       (setq TeX-quote-language
	     `("ngerman" ,open-quote ,close-quote ,q-after-q))))
   (setq LaTeX-babel-hyphen-language "ngerman")
   ;; Fontification
   (when (and (eq TeX-install-font-lock 'font-latex-setup)
	      (featurep 'font-latex))
     (font-latex-add-quotes '("\"`" "\"'"))
     (font-latex-add-quotes '("\">" "\"<" german))
     ;; Prevent "| from leading to color bleed.
     (font-latex-add-to-syntax-alist (list (cons ?\" "\\"))))
   (run-hooks 'TeX-language-de-hook)))

;;; ngerman.el ends here
