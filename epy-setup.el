;; epy-setup.el - setup and load all the paths where the extensions
;; are contained

;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

;;
;; Adjust load path to add the following paths
;; yasnippet/
;; plugins/
;; auto-complete

(add-to-list 'load-path
             epy-install-dir)
(add-to-list 'load-path
	     (concat epy-install-dir "yasnippet"))
(add-to-list 'load-path
	     (concat epy-install-dir "plugins"))
(add-to-list 'load-path
	     (concat epy-install-dir "auto-complete"))
(add-to-list 'load-path
	     (concat epy-install-dir "flymake"))

(provide 'epy-setup)
