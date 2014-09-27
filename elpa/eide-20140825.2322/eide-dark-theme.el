(deftheme eide-dark
  "Emacs-IDE dark color theme")

(custom-theme-set-faces
 'eide-dark
 '(default ((t (:background "black" :foreground "gray90"))))
 '(region ((t (:background "gray50"))))
 '(font-lock-builtin-face ((t (:background "brown" :foreground "yellow"))))
 '(font-lock-comment-face ((t (:foreground "deep sky blue"))))
 '(font-lock-constant-face ((t (:background "maroon4" :foreground "misty rose"))))
 '(font-lock-function-name-face ((t (:foreground "orange" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "salmon" :weight bold))))
 '(font-lock-string-face ((t (:background "gray15" :foreground "gray90"))))
 '(font-lock-type-face ((t (:foreground "medium sea green"))))
 '(font-lock-variable-name-face ((t (:foreground "dark orange"))))
 '(fringe ((t (:background "black"))))
 '(mode-line ((t (:background "gray")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-dark)
