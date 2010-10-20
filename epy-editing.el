;; ibuffer by default

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Ido mode with fuzzy matching
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

(require 'smart-operator)
;; Parentheses Pairing
(setq skeleton-pair t)

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)


;; Open Next Line
(require 'open-next-line)

;; Auto Completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
	     (concat epy-install-dir "auto-complete/ac-dict"))
(ac-config-default)

;; Yasnippet
(require 'yasnippet)

(yas/initialize)
(yas/load-directory (concat epy-install-dir "extensions/yasnippet/snippets"))

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

(provide 'epy-editing)
