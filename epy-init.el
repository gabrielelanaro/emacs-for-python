;; This file initializate all the extensions contained in this package

;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of python-collection"
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

;(setq pymacs-available
; (require 'pymacs "pymacs" t))

;; Yasnippet
(require 'yasnippet)

(yas/initialize)
(yas/load-directory (concat epy-install-dir "yasnippet/snippets"))

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

;; Auto-completion
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
	     (concat epy-install-dir "auto-complete/ac-dict"))
(ac-config-default)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; Rope, this one is more contrived, we have to check if we have
;; pymacs.

;; First adding to python path custom rope extensions
(setenv "PYTHONPATH"
	(concat 
	 (getenv "PYTHONPATH") ":"
	 (concat epy-install-dir "rope-dist")))

(when (require 'pymacs) 
  (setq pymacs-load-path 
	(list 
	 (concat epy-install-dir "rope-dist/ropemacs/")))
  (pymacs-load "ropemacs" "rope-")
  (load (concat epy-install-dir "completion/ac-ropemacs-config.el"))
  
  ;; Pretty custom, I've patched ropemode and ropemacs to add this
  ;; hook.
  ;;
  ;; There is also a custom hook to find if there is a project and if
  ;; there is activate it. In this way the project is automatically opened.
  (add-hook 'rope-open-project-hook 'ac-nropemacs-setup)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)
  )

;; ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ido mode with fuzzy matching
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Parentheses Pairing
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

(provide 'epy-init)