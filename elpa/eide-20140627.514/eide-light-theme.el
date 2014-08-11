(deftheme eide-light
  "Emacs-IDE light color theme")

(custom-theme-set-faces
 'eide-light
 '(default ((t (:background "old lace" :foreground "black"))))
 '(region ((t (:background "bisque"))))
 '(font-lock-builtin-face ((t (:background "yellow" :foreground "red"))))
 '(font-lock-comment-face ((t (:foreground "light slate blue"))))
 '(font-lock-constant-face ((t (:background "misty rose" :foreground "deep pink"))))
 '(font-lock-function-name-face ((t (:foreground "red" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "brown" :weight bold))))
 '(font-lock-string-face ((t (:background "white" :foreground "black"))))
 '(font-lock-type-face ((t (:foreground "sea green"))))
 '(font-lock-variable-name-face ((t (:foreground "orange red"))))
 '(fringe ((t (:background "old lace"))))
 '(mode-line ((t (:background "wheat")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-light)
