;;; epy-completion.el --- A few common completion tricks

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

;; ropemacs Integration with auto-completion
(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
      (concat ac-prefix completion))
    (rope-completions)))

(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))
(defun ac-python-mode-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'rope-open-project-hook 'ac-nropemacs-setup)

(provide 'epy-completion)
;;; epy-completion.el ends here
