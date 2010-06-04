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

(add-to-list 'load-path
	     (concat python-collection-install-dir "yasnippet"))
(add-to-list 'load-path
	     (concat python-collection-install-dir "plugins"))


;;============
;; Extensions 
;;============

;; Yasnippet
(require 'yasnippet)

(yas/initialize)
(yas/load-directory (concat python-collection-install-dir "yasnippet/snippets"))

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))

;; Predictive Abbreviation
(require 'pabbrev)
(global-pabbrev-mode)

;; Auto-fill-mode by default in almost all buffers
(setq-default auto-fill-function 'do-auto-fill)

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

; Open Next Line
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

;;=====================
;; Keybindings Section
;;=====================

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; calc-mode more comfortable
(global-set-key (kbd "M-c") 'calc-dispatch)