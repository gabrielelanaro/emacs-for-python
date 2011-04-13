;; ibuffer by default

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Ido mode with fuzzy matching
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'smart-operator)

;; Open Next Line
(require 'open-next-line)

;; Auto Completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
	     (concat epy-install-dir "auto-complete/ac-dict"))
(ac-config-default)

;; Yasnippet - force the loading of the custom version of yasnippet
(require 'yasnippet (concat epy-install-dir "extensions/yasnippet/yasnippet"))

(yas/initialize)
(yas/load-directory (concat epy-install-dir "extensions/yasnippet/snippets"))
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
(setq yas/wrap-around-region 'cua)

(provide 'epy-editing)
