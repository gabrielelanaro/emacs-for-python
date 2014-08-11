(deftheme eide-browsing
  "Emacs-IDE override of Emacs default settings for browsing")

(custom-theme-set-variables
 'eide-browsing
 '(tags-case-fold-search nil))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'eide-browsing)
