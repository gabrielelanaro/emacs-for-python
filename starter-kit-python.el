;; Starterkitted emacs-for-python

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
	     (concat epy-install-dir "yasnippet"))
(add-to-list 'load-path
	     (concat epy-install-dir "plugins"))
(add-to-list 'load-path
	     (concat epy-install-dir "auto-complete"))
(add-to-list 'load-path
	     (concat epy-install-dir "flymake"))

;;============
;; Extensions 
;;============

;; Yasnippet
(autoload 'yas/initialize "yasnippet" "Start Yasnippet" t)
(eval-after-load 'yasnippet
  (yas/initialize)
  (yas/load-directory (concat epy-install-dir "yasnippet/snippets"))
  (setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))
  )

;; Auto-completion
(autoload ac-config-default "auto-complete-config")
(eval-after-load 'auto-complete-config
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories 
               (concat epy-install-dir "auto-complete/ac-dict"))
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  )

;; Parentheses Pairing
(setq skeleton-pair t)

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; Virtualenv workon command
(require 'virtualenv)

;; Flymake for python configuration
(require 'python-flymake)

