(deftheme eugeneai-theme
  "Created 2012-10-07.")

(custom-theme-set-variables
 'eugeneai-theme
 '(current-language-environment "Russian")
 '(safe-local-variable-values (quote ((TeX-master . "dis") (py-master-file . "/path/to/interactivetest.py") (whitespace-line-column . 80) (lexical-binding . t))))
 '(tool-bar-mode nil)
 '(show-paren-mode t))

(custom-theme-set-faces
 'eugeneai-theme
 '(default ((t (:inherit nil :stipple nil :background "grey28" :foreground "wheat1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(flyspell-duplicate ((t (:foreground "pale green"))))
 '(flyspell-incorrect ((t (:foreground "orange red"))))
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "orange"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "orange3"))))
 '(tex-verbatim ((t (:family "Ubuntu Mono"))))
 '(tooltip ((((class color)) (:inherit variable-pitch :background "lightyellow" :foreground "black" :height 0.5)))))

(provide-theme 'eugeneai-theme)
