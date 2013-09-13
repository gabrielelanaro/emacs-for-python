(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(setq windowed-system (or (eq window-system 'x) (eq window-system 'w32)))
(setq win32-system (eq window-system 'w32))

;; magnars cool setup
;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))
(setq ext-lisp-dir (expand-file-name "extensions" dotfiles-dir))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Settings for currently logged in user
(setq user-settings-dir (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(dolist (project (directory-files ext-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(add-to-list 'load-path ".")


;; Keep emacs Custom-settings in separate file
(if win32-system
    (progn
      (setq custom-file (expand-file-name "custom-w32.el" dotfiles-dir))
      )
    (progn
      (setq custom-file (expand-file-name "custom.el" dotfiles-dir))
      )
    )

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(require 'auto-complete)

;;(global-linum-mode 1)
(global-auto-complete-mode 1)

(if win32-system
    (setenv "PYMACS_PYTHON" "c:/python27/python.exe")
    (setenv "PYMACS_PYTHON" "python2")
)

(load-file (expand-file-name "epy-init.el" dotfiles-dir))

(if
    windowed-system
    (setq linum-format "%4d")
    (setq linum-format "%3d"))

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'gnu) ;; swi
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
				("\\.pro$" . prolog-mode)
                                ("\\.m$" . mercury-mode)
                                ("\\.P$" . prolog-mode)
                                ("\\.tex$" . latex-mode)
                                )
                               auto-mode-alist))
(add-hook 'prolog-mode-hook 'auto-complete-mode)


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t)
(autoload 'tex-mode-flyspell-verify "flyspell" "" t)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\.md" . markdown-mode) auto-mode-alist))

(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\.vapi$" . utf-8))

;;Setting up tabbar
(if
    windowed-system
    (progn
      (require 'tabbar)
      (tabbar-mode)
      (menu-bar-mode 1)
      (require 'recentf)
      (recentf-mode 1)
      (setq
       recentf-menu-path '("File")
       recentf-menu-title        "Recent"
 recentf-max-saved-items 100
 recentf-max-menu-items 20
             )
      (recentf-update-menu-hook)
      (require 'window-numbering)
      (window-numbering-mode 1)
      (setq window-numbering-assign-func
            (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
      (global-font-lock-mode t)
      (setq font-lock-maximum-decoration t)
      )
  (progn
      (menu-bar-mode 0)
    )
)

(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'ido)

;;(require 'dired+)
(require 'highlight-80+)

;(require 'python-mode)
;(require 'ipython)


(add-to-list 'auto-mode-alist '("\\.zcml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;; key bindings

(global-set-key (kbd "C-x e") 'erase-buffer)
(global-set-key (kbd "C-<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "<escape>-<escape>-<escape>"))
(global-set-key (kbd "C-q") 'undo)
(global-set-key (kbd "C-z") 'quoted-insert)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-с C-m") 'execute-extended-command)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x c") 'compile)
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

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

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

(load custom-file)

(if
    windowed-system
    (progn
      ;; This script is set for a `text-scale-mode-step` of `1.04`
      (setq text-scale-mode-step 1.2)
      ;;
      ;; List: `Sub-Zoom Font Heights per text-scale-mode-step`
      ;;   eg.  For a default font-height of 120 just remove the leading `160 150 140 130`
      (defvar sub-zoom-ht (list 160 150 140 130 120 120 110 100 100  90  80  80  80  80  70  70  60  60  50  50  50  40  40  40  30  20  20  20  20  20  20  10  10  10  10  10  10  10  10  10  10   5   5   5   5   5   2   2   2   2   2   2   2   2   1   1   1   1   1   1   1   1   1   1   1   1))
      (defvar sub-zoom-len (safe-length sub-zoom-ht))
      (defvar def-zoom-ht (car sub-zoom-ht))
      (set-face-attribute 'default nil :height def-zoom-ht)



      ;; Adjust line number fonts.

      (setq my-def-linum-text-height
            (face-attribute 'default :height)
            )

      (defun text-scale-adjust-zAp ()
        (interactive)
        (text-scale-adjust 0)
        (set-face-attribute 'linum nil :height my-def-linum-text-height)
        )

      (defun text-scale-decrease-zAp ()
        (interactive)
        (text-scale-decrease 1)
        (set-face-attribute 'linum nil :height my-def-linum-text-height)
        )

      (defun text-scale-increase-zAp ()
        (interactive)
        (text-scale-increase 1)
        (set-face-attribute 'linum nil :height my-def-linum-text-height)
        )

      ;; Zoom font via Numeric Keypad

      (define-key global-map (kbd "<C-kp-add>") 'text-scale-increase-zAp)
      (define-key global-map (kbd "<C-kp-subtract>") 'text-scale-decrease-zAp)
      (define-key global-map (kbd "<C-kp-multiply>") 'text-scale-adjust-zAp)
      (define-key global-map (kbd "<M-mouse-4>") 'text-scale-increase-zAp)
      (define-key global-map (kbd "<M-mouse-5>") 'text-scale-decrease-zAp)
      (define-key global-map (kbd "<M-wheel-up>") 'text-scale-increase-zAp)
      (define-key global-map (kbd "<M-wheel-down>") 'text-scale-decrease-zAp)

      ;; (set-scroll-bar-mode 'right)   ; replace 'right with 'left to place it to the left
      (setq popup-use-optimized-column-computation nil) ; May be tie menu zise to default text size.
      ;; (ac-fuzzy-complete)
      ;; (ac-use-fuzzy)
      ;; (add-hook 'after-make-frame-functions 'fullscreen-toggle)
      (defun toggle-fullscreen (&optional f)
        (interactive)
        (let ((current-value (frame-parameter nil 'fullscreen)))
          (set-frame-parameter nil 'fullscreen
                               (if (equal 'fullboth current-value)
                                   (if (boundp 'old-fullscreen) old-fullscreen nil)
                                 (progn (setq old-fullscreen current-value)
                                        'fullboth)))))
      (global-set-key [f11] 'toggle-fullscreen)
      (global-set-key (kbd "C-c f") 'toggle-fullscreen)
      (defun maximize-window (&optional f)
	(interactive)
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			       '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			       '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

      ;;; (toggle-fullscreen)
      (maximize-window)
      )
  (progn
    (set-face-background 'region "blue") ; Set region background color
    (set-face-foreground 'region "wheat1") ; Set region background color
    (set-face-background 'linum "cyan") ; Set region background color
    (set-face-foreground 'linum "black") ; Set region background color
    )
  )


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


(require 'jump-char)

(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

(defun set-input-method-english ()
  (interactive)
  (if current-input-method (toggle-input-method))
  )

(defun latex-b-slash ()
  (interactive)
  (set-input-method-english)
  (insert "\\")
)

(defun ask-user-latex-command (cmd)
  "Prompt user to enter a string, with input history support."
  (interactive (list (read-string "LaTeX Command:")) )
  ;; (message "String is 「%s」." cmd)
  (insert "\\")
  (insert cmd)
  (insert "{}")(backward-char)
  )

(defun latex-set-b-slash-hack ()
  (interactive)
  (local-set-key (kbd "\\") 'ask-user-latex-command)
  )

(defun dollar-equation ()
  (interactive)
  (set-input-method-english)
  (insert "$$")
  (backward-char)
)

(defun latex-dollar-hack ()
  (interactive)
  (local-set-key (kbd "C-4") 'dollar-equation)
  )

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'latex-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-flyspell)
;;(add-hook 'latex-mode-hook 'highlight-changes-mode)

(global-set-key (kbd "C-<menu>") 'toggle-input-method)
(global-set-key (kbd "<C-apps>") 'toggle-input-method) ;; for windows.

(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
(put 'downcase-region 'disabled nil)


;; auto-language

(defun my-char-lang-guess (arg)
  (let (
        (ch-cat
         (get-char-code-property
          arg
          'general-category)
         )
        )
    (cond
     (
      (or
       (eq ch-cat 'Ll)
       (eq ch-cat 'Lu)
       )
      (car
       (split-string
        (get-char-code-property
         arg
         'name
         )))

      )
     (t "NONE")
     )
    )
  )

(defun my-map (arg)
  (cond
   ((equal arg "NONE")
   "LATIN"
   )
   (t
    arg
    )
   )
  )

(defun my-point-lang-guess ()
  (mapcar #'my-map (my-l-r-props))
)

(defun my-l-r-props ()
  (list
   (my-char-lang-guess (preceding-char))
   (my-char-lang-guess (following-char))
   )
  )

(defun my-scan-to-word ()
  (save-excursion
    (while
        (and
         (not (bobp))
         (or
          (equal
           (my-l-r-props)
           (list "NONE" "NONE")
           )
          )
         )
      (backward-char)
      )
    ;()
    (my-point-lang-guess)
    )
  )

(defun auto-language-environment ()
  (interactive)
  (cond
   (
    (and
     (eq this-command 'self-insert-command)
     (or
      (eq  last-command 'toggle-input-method)
      (eq  last-command 'set-input-method)
      )
     )
    t
    )
   (
    (or
     (eq this-command 'toggle-input-method)
     (eq this-command 'set-input-method)
     )
    t
    )
   (
    (and
     (eq this-command 'self-insert-command)
     (eq last-command 'self-insert-command))
    t
    )
   (
    (equal
     (my-scan-to-word)
     (list "LATIN" "LATIN")
     )
    (set-input-method-english)
    t
    )
   (
    t
    (set-input-method "russian-computer")
    t
    )
   (t
    (set-input-method-english)
    )
   )
  )

(defun latex-12-hacks ()
  (latex-dollar-hack)
  (latex-set-b-slash-hack)
  ;; (add-hook 'post-command-hook 'auto-language-environment)
  )

(add-hook 'latex-mode-hook 'latex-12-hacks)
(global-set-key (kbd "C-`") 'linum-mode)

;; Patching wrong scrolllock behaviour
(defun scroll-lock-next-line (&optional arg)
  "Scroll up ARG lines keeping point fixed."
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-lock-update-goal-column)
  (if (pos-visible-in-window-p (point-max))
      (progn
        (next-line arg)
        (print "vis-p")
        (print arg)
      )
    (progn
      (scroll-up arg)
      (next-line (- arg))
      (print arg)
      (print "not-vis-p")
      )
    )
  (scroll-lock-move-to-column scroll-lock-temporary-goal-column)
  )

(setq-default ispell-program-name "aspell")

(setq ispell-dictionary "english")
(setq ispell-local-dictionary "russian")
(setq flyspell-default-dictionary "russian")

(load "server")
(unless (server-running-p) (server-start))

(setq visible-bell 1)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;(unless (package-installed-p 'scala-mode2)
;  (package-refresh-contents) (package-install 'scala-mode2))
