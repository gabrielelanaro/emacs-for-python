;; This file initializate all the extensions contained in this package

;; Trick to get the filename of the installation directory
(defconst python-collection-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of python-collection"
)

;; Adjust load path to add the following paths
;; yasnippet/
;; plugins/
;; auto-complete

(add-to-list 'load-path
	     (concat python-collection-install-dir "yasnippet"))
(add-to-list 'load-path
	     (concat python-collection-install-dir "plugins"))
(add-to-list 'load-path
	     (concat python-collection-install-dir "auto-complete"))

;;============
;; Extensions 
;;============

;; Yasnippet
(require 'yasnippet)

(yas/initialize)
(yas/load-directory (concat python-collection-install-dir "yasnippet/snippets"))

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

;; Auto-completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
	     (concat python-collection-install-dir "auto-complete/ac-dict"))
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete) ;; To use
						      ;; fuzzy-completion,
						      ;; maybe it's
						      ;; too much slow

;; Auto-completion lightening for python-mode
;; (add-hook 'python-mode-hook
;; 	  (lambda () (setq ac-sources 
;; 			   '(ac-source-filename
;; 			    ac-source-yasnippet
;; 			    ac-source-abbrev
;; 			    ac-source-words-in-same-mode-buffers
;; 			    ac-source-dictionary)
;; 			   )))


;; Auto-fill-mode for python-mode only for comments
(add-hook 'python-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (set (make-local-variable 'fill-nobreak-predicate)
		 (lambda ()
		   (not (eq (get-text-property (point) 'face)
			    'font-lock-comment-face))))))

;; ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ido mode with fuzzy matching
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

; Parentheses Pairing
(setq skeleton-pair t)

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;; Open Next Line
(require 'open-next-line)

;; Eshell tweaks
;; Visual commands like ipython
(add-hook
 'eshell-mode-hook
  (lambda ()
    (setq
     eshell-visual-commands
     (append
      '("mutt" 
	"vim" 
	"screen" 
	"lftp" 
	"ipython" 
	"telnet"
	"ssh")
       eshell-visual-commands))))

;; Virtualenv workon command
(require 'virtualenv)

;; Flymake for python configuration
(require 'python-flymake)

(require 'smart-operator)
;;=====================
;; Keybindings Section
;;=====================

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; calc-mode more comfortable
(global-set-key (kbd "M-c") 'calc-dispatch)

; Ctrl+tab mapped to Alt+tab
(define-key function-key-map [(control tab)] [?\M-\t])