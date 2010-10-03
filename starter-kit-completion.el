;;; starter-kit-completion.el --- A few common completion tricks
;;

;; -----------------------------------------------------------------------------
;; AUTO INSERTION OF MATCHING SYMBOLS
;; -----------------------------------------------------------------------------
(setq skeleton-pair t)
(define-key global-map "(" 'skeleton-pair-insert-maybe)
(define-key global-map "[" 'skeleton-pair-insert-maybe)
(define-key global-map "{" 'skeleton-pair-insert-maybe)

(defun ac-eshell-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(require 'auto-complete-config nil t)
;; (require 'ac-dabbrev)

;;(setq ac-auto-start .5)
;;(setq ac-quick-help-delay 0.5)

;; Do What I Mean mode
(setq ac-dwim t)
(ac-config-default)

;; set also the completion for eshell
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

(defun turn-on-auto-complete-mode ()
  (auto-complete-mode))

(defun ac-python-mode-setup ()
  (message "In ac-python-mode-setup")
  (load (concat epy-install-dir "completion/ac-ropemacs-config.el"))
  
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-hook 'rope-open-project-hook 'ac-nropemacs-setup))

(defun setup-python-completion ()
  (setq-default ac-sources
                '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'python-mode-hook 'turn-on-auto-complete-mode)
  (add-hook 'python-mode-hook 'ac-python-mode-setup)
  )

(provide 'starter-kit-completion)
;;; starter-kit-completion.el ends here
