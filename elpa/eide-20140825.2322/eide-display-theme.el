(deftheme eide-display
  "Emacs-IDE override of Emacs default settings for display")

(custom-theme-set-variables
 'eide-display
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode (quote right))
 '(show-trailing-whitespace t)
 '(show-paren-mode t)
 '(line-number-mode t)
 '(column-number-mode t)
 '(which-function-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(gdb-many-windows t))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-display)
