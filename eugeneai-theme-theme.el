(deftheme eugeneai-theme
  "Wheat color based theme, stolen in internet 2013-10-13.")

(custom-theme-set-variables
 'eugeneai-theme
 '(safe-local-variable-values (quote ((TeX-master . "dis") (py-master-file . "/path/to/interactivetest.py") (whitespace-line-column . 80) (lexical-binding . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-theme-set-faces
 'eugeneai-theme
 '(flyspell-duplicate ((t (:foreground "pale green"))))
 '(flyspell-incorrect ((t (:foreground "orange red"))))
 '(linum ((t (:inherit (shadow default) :background "black" :foreground "orange"))))
 '(region ((((class color) (min-colors 88) (background dark)) (:background "orange3"))))
 '(tex-verbatim ((t (:family "Ubuntu Mono"))))
 '(tooltip ((((class color)) (:inherit variable-pitch :background "lightyellow" :foreground "black" :height 0.5))))
 '(default ((t (:inherit nil :stipple nil :background "gray20" :foreground "wheat1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :foundry "unknown" :family "Fira Sans Mono")))))

(provide-theme 'eugeneai-theme)
