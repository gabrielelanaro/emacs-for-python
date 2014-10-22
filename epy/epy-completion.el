;;; epy-completion.el --- A few common completion tricks

;; Pairing parentheses

;; All languages:
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; Just python
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "'" 'skeleton-pair-insert-maybe)))

;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(require 'auto-complete-config nil t)
(add-to-list 'ac-dictionary-directories (concat epy-install-dir "elpa-to-submit/auto-complete/dict/"))
;; Do What I Mean mode
(setq ac-dwim t)
(ac-config-default)

;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

;; Disabling Yasnippet completion
(when epy-load-yasnippet-p
  (defun epy-snips-from-table (table)
    (with-no-warnings
      (let ((hashtab (ac-yasnippet-table-hash table))
	    (parent (ac-yasnippet-table-parent table))
	    candidates)
	(maphash (lambda (key value)
		   (push key candidates))
		 hashtab)
	(identity candidates))))

  (defun epy-get-all-snips ()
    (require 'yasnippet) ;; FIXME: find a way to conditionally load it
    (let (candidates)
      (maphash
       (lambda (kk vv) (push (epy-snips-from-table vv) candidates)) yas/tables)
      (apply 'append candidates))))

;;(setq ac-ignores (concatenate 'list ac-ignores (epy-get-all-snips)))

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
  (when epy-load-yasnippet-p
    (add-to-list 'ac-sources 'ac-source-yasnippet)))

(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'rope-open-project-hook 'ac-nropemacs-setup)

(provide 'epy-completion)
;;; epy-completion.el ends here
