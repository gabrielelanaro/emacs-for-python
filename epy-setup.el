;; epy-setup.el - setup and load all the paths where the extensions
;; are contained

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
