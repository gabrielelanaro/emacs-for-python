(load-file "/home/eugeneai/.emacs.d/epy-init.el")

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'gnu)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))

(global-set-key (kbd "C-c f") 'fullscreen-toggle)
(add-hook 'after-make-frame-functions 'fullscreen-toggle)
    (defun toggle-fullscreen (&optional f)
      (interactive)
      (let ((current-value (frame-parameter nil 'fullscreen)))
           (set-frame-parameter nil 'fullscreen
                                (if (equal 'fullboth current-value)
                                    (if (boundp 'old-fullscreen) old-fullscreen nil)
                                    (progn (setq old-fullscreen current-value)
                                           'fullboth)))))
    (global-set-key [f11] 'toggle-fullscreen)
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t) 
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

;;Setting up tabbar
(require 'tabbar)
(tabbar-mode)

(menu-bar-mode 1)
;;(tool-bar-mode 1)
;;(scroll-bar-mode 1)
(require 'ido)
(require 'recentf)
(recentf-mode 1)

(custom-set-variables
 '(recentf-max-menu-items 20)
 '(recentf-max-saved-items 50)
 '(recentf-menu-path (quote ("File")))
 '(show-paren-mode t))

;;(require 'dired+)
(require 'highlight-80+)
(require 'window-numbering)
(window-numbering-mode 1)
(setq window-numbering-assign-func
      (lambda () (when (equal (buffer-name) "*Calculator*") 9)))

;;;;; key bindings

(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-z") 'quoted-insert)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-с C-m") 'execute-extended-command)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "<C-kp-add>") 'text-scale-increase)
(define-key global-map (kbd "<C-kp-subtract>") 'text-scale-decrease)
(define-key global-map (kbd "<M-mouse-4>") 'text-scale-increase)
(define-key global-map (kbd "<M-mouse-5>") 'text-scale-decrease)

(global-set-key (kbd "C-x h") 'view-url)
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-other-windows)
(global-set-key [f9] 'split-window-horizontally) 

(setq visible-bell nil)

(setq echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      )

(mouse-wheel-mode t)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

(setq x-select-enable-clipboard t)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t
        ido-max-prospects 10))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 143 :width normal :foundry "unknown" :family "CMU Typewriter Text")))))
