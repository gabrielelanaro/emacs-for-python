(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(load-file (expand-file-name "~/.emacs.d/epy-init.el"))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'gnu) ;; swi
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
				("\\.pro$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))
(add-hook 'prolog-mode-hook 'auto-complete-mode)


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
(add-hook 'LaTeX-mode-hook 'highlight-changes-mode)

;;Setting up tabbar
;;(require 'tabbar)
;;(tabbar-mode)

(menu-bar-mode 0)
;;(tool-bar-mode 0)
;;(scroll-bar-mode 1)
(require 'ido)
;;(require 'recentf)
;;(recentf-mode 1)

;;(require 'dired+)
(require 'highlight-80+)
;;(require 'window-numbering)
;;(window-numbering-mode 1)
;;(setq window-numbering-assign-func
;;      (lambda () (when (equal (buffer-name) "*Calculator*") 9)))

;(add-to-list 'load-path "~/.emacs.d/user/python-mode/")
;(setq py-install-directory "~/.emacs.d/user/python-mode/")
;(require 'python-mode)
;(require 'ipython)

(add-to-list 'load-path ".")
;;(global-font-lock-mode t)
;;(setq font-lock-maximum-decoration t)
(add-to-list 'auto-mode-alist '("\\.zcml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;; key bindings

(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-z") 'quoted-insert)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-с C-m") 'execute-extended-command)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

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
(set-default 'imenu-auto-rescan nil)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;;(setq x-select-enable-clipboard t)

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

(global-set-key [C-f1] 'bookmark-set)
(global-set-key [f1] 'bookmark-jump)

;; Strange colous

;;(setq default-frame-alist (append (list
;;  '(width  . 81)  ; Width set to 81 characters
;;  '(height . 40)) ; Height set to 60 lines
;;  default-frame-alist))

(setq inhibit-startup-message   t)   ; Don't want any startup message
;(setq make-backup-files         nil) ; Don't want any backup files
;(setq auto-save-list-file-name  nil) ; Don't want any .saves files
;(setq auto-save-default         nil) ; Don't want any auto saving

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

;;; lets you use Ido with imenu.
(require 'imenu+)
(add-hook 'python-mode-hook 'imenu-add-defs-to-menubar)
(global-set-key [S-mouse-3] 'imenu)

;;; Set some more

;(setq default-frame-alist (append (list
;  '(width  . 81)  ; Width set to 81 characters
;  '(height . 40)) ; Height set to 60 lines
;  default-frame-alist))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import pdb; pdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)))

;; vala
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

;; (setq linum-format "%4d \u2502 ")


(set-face-background 'region "wheat3") ; Set region background color

(setq linum-format "%4d ")
;; (ac-fuzzy-complete)
;; (ac-use-fuzzy)

(defun python-shell-get-or-create-process ()
  "Get or create an inferior Python process for current buffer and return it."
  (let* ((old-buffer (current-buffer))
         (dedicated-proc-name (python-shell-get-process-name t))
         (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
         (global-proc-name  (python-shell-get-process-name nil))
         (global-proc-buffer-name (format "*%s*" global-proc-name))
         (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
         (global-running (comint-check-proc global-proc-buffer-name))
         (current-prefix-arg 4))
    (when (and (not dedicated-running) (not global-running))
      (if (run-python t (python-shell-parse-command))
          (setq dedicated-running t)
        (setq global-running t)))
    ;; Always prefer dedicated
    (switch-to-buffer old-buffer)
    (get-buffer-process (if dedicated-running
                            dedicated-proc-buffer-name
                          global-proc-buffer-name))))

