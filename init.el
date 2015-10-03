(if (eq window-system 'w32)
    (progn
      (if (file-directory-p "c:/GNU/bin")
          (progn
            (add-to-list 'exec-path "c:/GNU/bin")
            )
        )
      (setq url-proxy-services '(("no_proxy" . "172.27.24.")
                                 ("http" . "titan.cyber:ghbdtnbr@172.27.100.5:4444")))

      )
  )

(custom-set-variables
 '(load-prefer-newer t)
 '(epy-load-yasnippet-p t)
 )

(require 'package)

; list the packages you want
(setq package-list '(auctex magit
			    auctex-latexmk
			    ace-jump-mode
			    color-theme
			    cursor-chg
			    d-mode
			    ace-jump-mode
			    ace-jump-buffer
			    ace-jump-zap
			    auto-complete
			    auto-complete-auctex
					; auto-dictionary-readme.txt
					; ediprolog
					; eide
					; enclose
			    fiplr
			    gh
			    goto-last-change
			    jedi
			    lua-mode
			    magit-find-file
			    magit-gh-pulls
			    markdown-mode+
			    markdown-mode
			    python-environment
			    s
			    w3m
			    yasnippet
			    ;rw-ispell
			    ;rw-hunspell
                            ;rw-language-and-country-codes
                            htmlize
))

; list the repositories containing them
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'auctex-latexmk)
(auctex-latexmk-setup)

; (setq-default TeX-master nil)
(custom-set-variables
; '(TeX-install-font-lock 'tex-font-setup)
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-master nil)
 '(TeX-save-query nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server (quote ask)))

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-off-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)

(setq reftex-plug-into-AUCTeX t)

(require 'cursor-chg)
(setq curchg-default-cursor-color "LightSkyBlue1")
(setq curchg-input-method-cursor-color "red")
(setq curchg-default-cursor-type '(hbar . 5))
(change-cursor-mode 1) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle

(setq windowed-system (or (eq window-system 'x) (eq window-system 'w32)))
(setq win32-system (eq window-system 'w32))

(menu-bar-mode 0)
(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/epy"))

;; magnars cool setup
;; Set path to .emacs.d
;;(setq dotfiles-dir (file-name-directory
;;      (or (buffer-file-name) load-file-name)))
(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))


;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))
(setq ext-lisp-dir (expand-file-name "epy/extensions" dotfiles-dir))

;; Set up load path
;;(add-to-list 'load-path dotfiles-dir)
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
(if t ;; windowed-system
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)
      (if win32-system
          (progn
            (setq custom-file (expand-file-name "custom-w32.el" dotfiles-dir))
            )
        (progn
          (setq custom-file (expand-file-name "custom.el" dotfiles-dir))
          )
        )
      )
  (setq custom-file (expand-file-name "custom-nw.el" dotfiles-dir))
  )

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

(require 'auto-complete)

(require 'linum)

;;(global-linum-mode 1)
;; (global-auto-complete-mode 1)
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

(if win32-system
    (setenv "PYMACS_PYTHON" "c:/python27/python.exe")
    (setenv "PYMACS_PYTHON" "python2")
)

(load-file (expand-file-name "epy/epy-init.el" dotfiles-dir))

(if
    t ;;windowed-system
    (setq linum-format "%4d ")
  (progn
    (setq linum-format "%3d ")
    (global-linum-mode 1)
    )
)

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi) ;; swi
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
				("\\.pro$" . prolog-mode)
                                ("\\.m$" . mercury-mode)
                                ("\\.P$" . prolog-mode)
                                ("\\.tex$" . latex-mode)
                                )
                               auto-mode-alist))
(add-hook 'prolog-mode-hook 'auto-complete-mode)
(add-hook 'd-mode-hook 'auto-complete-mode)


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
;;(require 'tabbar)
;;(tabbar-mode)


(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq
;;  recentf-menu-path '("File")
;;  recentf-menu-title "Recent"
  recentf-max-saved-items 200
;;  recentf-max-menu-items 20
  )
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(if
    t ;;windowed-system
    (progn
      ;(require 'tabbar)
      ;(tabbar-mode)
      ;(menu-bar-mode 0)
      (set-fringe-style '(0 . 0)) ; no fringes atall
      (mouse-wheel-mode t)
      (setq window-numbering-assign-func
            (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
      (global-font-lock-mode t)
      (setq font-lock-maximum-decoration t)
      )
)

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 1.0)))) ; just for a while

(setq-default
 default-truncate-lines t
 blink-cursor-alist '((t . hollow))
 user-full-name "Evgeny Cherkashin"
 user-mail-address "eugeneai@irnok.net"
 column-number-mode t
 line-number-mode t
 page-delimiter "^\\s *\n\\s *"
 minibuffer-max-depth nil
 display-time-day-and-date t
 frame-title-format '(buffer-file-name "%f" "%b")
)

;; goto-matching-paren
;; -------------------
;; If point is sitting on a parenthetic character, jump to its match.
;; This matches the standard parenthesis highlighting for determining which
;; one it is sitting on.
;;
(defun goto-matching-paren ()
  "If point is sitting on a parenthetic character, jump to its match."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        ((progn
           (backward-char 1)
           (looking-at "\\s\)")) (forward-char 1) (backward-list 1))))
(define-key global-map (kbd "C-x p p") 'goto-matching-paren) ; Bind to C-z p

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun reconstruct-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)
    )
  (replace-regexp "\\(\\w+\\)-\\s-+\\(\\w+\\)" "\\1\\2" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\s-*вЂ\”" "~--" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\(\\w+\\)-\\(\\w+\\)" "\\1\"=\\2" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\.\\.\\." "\\\\ldots{}" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\[\\([[:digit:]]+\\)\\]" "\\\\cite{b\\1}" nil (line-beginning-position) (line-end-position))
  ;(replace-regexp "\\(\\w\\|\\.\\):" "\\1\\\\,:" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\([тТ]\\.\\)\\s-*\\(\\w\\.\\)" "\\1~\\2" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\([[:upper:]]\\.\\)\\s-*\\([[:upper:]]\\.\\)\\s-+\\([[:upper:]]\\w*\\)" "\\1~\\2~\\3" nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\([[:upper:]]\\.\\)\\s-+\\([[:upper:]]\\w*\\)" "\\1~\\2" nil (line-beginning-position) (line-end-position))
  ;(replace-regexp "\"\\(\\w+\\)" "<<\1" nil (line-beginning-position) (line-end-position))
  ;(replace-regexp "\\(\\w+\\)\"" "\1>>" nil (line-beginning-position) (line-end-position))
  ;(replace-regexp "\"\\(\\.\\)\"" "<<\1>>" nil (line-beginning-position) (line-end-position))
  ;(replace-regexp "\\s-+" "_")
  )

(defun reconstruct-minted ()
  "From cursor till '\end{' performs text cleaning."
  (interactive)
  (let ((endpos (point)))
    (save-excursion
      (goto-char (mark))
      (beginning-of-line)
      (while (< (point) endpos)
        ;; (funcall fun (buffer-substring (line-beginning-position) (line-end-position)))
        (reconstruct-minted-line)
        )
      )
    )
  )

(defun reconstruct-minted-line ()
  (interactive)
  (beginning-of-line)
  (replace-regexp "\\\\textquotedbl{}" "\"" nil (line-beginning-position) (line-end-position))
  (replace-regexp "#doctest:.*$" "" nil (line-beginning-position) (line-end-position))
  (replace-regexp "^\\.\\.\\." "   " nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\~" " " nil (line-beginning-position) (line-end-position))
  (replace-regexp "\\\\" "" nil (line-beginning-position) (line-end-position))
  (replace-regexp "{\\[}" "[" nil (line-beginning-position) (line-end-position))
  (replace-regexp "{\\]}" "]" nil (line-beginning-position) (line-end-position))
  (forward-line 1)
  ; #doctest: +ELLIPSIS
  )

;; Handy key definition
(define-key global-map [f9] 'reconstruct-paragraph)
(define-key global-map [f12] 'reconstruct-minted-line)

(require 'window-numbering)
(window-numbering-mode 1)
(require 'ido)
(require 'highlight-80+)
(highlight-80+-mode)


(add-to-list 'auto-mode-alist '("\\.zcml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;;;; key bindings

(global-set-key (kbd "C-x e") 'erase-buffer)
(global-set-key (kbd "C-<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "<escape>-<escape>-<escape>"))
(global-set-key (kbd "C-q") 'quoted-insert)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "C-<return>") 'open-next-line)

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x h") 'view-url)
(global-set-key (kbd "C-x M-m") 'shell)
;(global-set-key [f7] 'split-window-vertically)
;(global-set-key [f8] 'delete-other-windows)
;(global-set-key [f9] 'split-window-horizontally)

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

(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)
(add-hook 'python-mode-hook (lambda ()(setq skeleton-pair nil)))

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
  (newline-and-indent)
  (highlight-lines-matching-regexp "^[ ]*import pdb; pdb.set_trace()"))

(defun python-add-pubreakpoint ()
  (interactive)
  (newline-and-indent)
  (insert "import pudb; pu.db")
  (newline-and-indent)
  (highlight-lines-matching-regexp "^[ ]*import pudb; pu.db"))

(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map (kbd "C-c C-y") 'python-add-pubreakpoint)))

(add-hook 'python-mode-hook '(lambda ()
                               (define-key python-mode-map (kbd "C-c C-y") 'python-add-pubreakpoint)
                               (electric-indent-local-mode -1)
                               (local-set-key (kbd "C-j") #'newline-and-indent)
                               ))

(defun tex-add-russian-dash ()
  (interactive)
  (insert "~-- "))

;(add-hook 'late-mode-hook (lambda ()
;                (local-set-key (kbd "C-=") #'tex-add-russian-dash)))

(defun tex-add-verb-environment ()
  (interactive)
  (open-next-line 1)
  (insert "{\\tt%")
  (open-next-line 1)(beginning-of-line)
  (insert "\\begin{verbatim}")
  (open-next-line 1)
  (open-next-line 1)
  (insert "\\end{verbatim}%")
  (open-next-line 1)(beginning-of-line)
  (insert "}%")
  (backward-char 2)
  (forward-line -2)
  )

(global-set-key (kbd "C-=") 'tex-add-russian-dash)
(global-set-key (kbd "C-c C-=") 'tex-add-verb-environment)

(defun my-ttt ()
  (erase-buffer)
  (face-remap-add-relative 'default '(
          ; :family "Monospace"
           :height 160 ;Seseg
          ; :height 88
          ))
)

(add-hook 'comint-mode-hook 'my-ttt)
(add-hook 'compilation-mode-hook 'my-ttt)
(add-hook 'gdb-locals-mode-hook 'my-ttt)
(add-hook 'gdb-frames-mode-hook 'my-ttt)
(add-hook 'gdb-registers-mode-hook 'my-ttt)


;; vala
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))

(load custom-file)

(if
    t   ;; windowed-system
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
      (defun toggle-fullscreen-1 (&optional f)
        (interactive)
        (let ((current-value (frame-parameter nil 'fullscreen)))
          (set-frame-parameter nil 'fullscreen
                               (if (equal 'fullboth current-value)
                                   (if (boundp 'old-fullscreen) old-fullscreen nil)
                                 (progn (setq old-fullscreen current-value)
                                        'fullboth)
                                 )
                               ;(menu-bar-mode 0)
                               )
          )
        )
      (global-set-key [f11] 'toggle-frame-fullscreen)
      ;;(global-set-key (kbd "C-c f") 'toggle-fullscreen)
	(if (not win32-system)
	(progn
      (defun maximize-window (&optional f)
	(interactive)
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			       '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
	(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
			       '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

      ;;; (toggle-fullscreen)
      ;;; (maximize-window)
	))
      )
  (progn
    (set-face-background 'region "blue") ; Set region background color
    (set-face-foreground 'region "wheat1") ; Set region background color
    (set-face-background 'linum "gray10") ; Set region background color
    (set-face-foreground 'linum "gold") ; Set region background color
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

;; (set-background-color        "wheat3") ; Set emacs bg color

;;(toggle-fullscreen)

;; Adjust line number fonts.

(defun dollar-equation ()
  (interactive)
  (set-input-method-english)
  (insert "$$")
  (backward-char)
  (set-input-method-english)
)

(defun latex-dollar-hack ()
  (interactive)
  (local-set-key (kbd "C-4") 'dollar-equation)
  )

(defun turn-off-auto-fill ()
  (interactive)
  (auto-fill-mode 0))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'diff-mode-hook 'turn-on-visual-line-mode)
(add-hook 'latex-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-visual-line-mode)
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
  ;; (print last-command this-command)
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
   ((eq this-command 'dollar-equation)
    (set-input-method-english)
    t)
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
  ;(add-hook 'post-command-hook 'auto-language-environment)
  )

(add-hook 'LaTeX-mode-hook 'latex-12-hacks)

(global-set-key (kbd "C-`") 'linum-mode)
(put 'scroll-left 'disabled nil)

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

(load "server")
(unless (server-running-p) (server-start))

(setq visible-bell 1)

(defvar gud-overlay
(let* ((ov (make-overlay (point-min) (point-min))))
(overlay-put ov 'face 'secondary-selection)
ov)
"Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight act)
"Highlight current line."
(let* ((ov gud-overlay)
(bf (gud-find-file true-file)))
(save-excursion
  (set-buffer bf)
  (move-overlay ov (line-beginning-position) (line-end-position)
  (current-buffer)))))

(defun gud-kill-buffer ()
(if (eq major-mode 'gud-mode)
(delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)
(add-hook 'gdb-mode-hook '(lambda ()
                            ;(new-frame)
                            ;(switch-to-buffer "**gdb**")
                            ;(tool-bar-mode 1)
                            (gdb-many-windows)
                            ))
;;-------------------------------------------------------------

(put 'erase-buffer 'disabled nil)

(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))

(require 'goto-last-change)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; Some additional features
(defalias 'qrr 'query-replace-regexp)

;; Workaround rope hooks error

(if (not (functionp 'rope-after-save-actions))
    (defun rope-after-save-actions ())
  )
(if (not (functionp 'rope-before-save-actions))
    (defun rope-before-save-actions ())
)

(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(require 'rw-hunspell)
(add-to-list 'ispell-local-dictionary-alist  '("russian"
        "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
        "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
        "[-]"  nil ("-d" "ru_RU") nil utf-8)
)

(add-to-list 'ispell-local-dictionary-alist  '("english"
       "[A-Za-z]" "[^A-Za-z]"
       "[']"  nil ("-d" "en_US") nil iso-8859-1)
)

;(setq ispell-program-name "hunspell")
(setq ispell-program-name (executable-find "hunspell"))
(setq ispell-really-aspell nil
      ispell-really-hunspell t)
(setq ispell-dictionary "russian") ;"ru_RU_hunspell")
;; The following is set via custom
(custom-set-variables
 '(rw-hunspell-default-dictionary "russian") ;"ru_RU_hunspell")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
 )

(if (eq window-system 'w32)
    (progn
      (custom-set-variables
       '(rw-hunspell-dicpath-list (quote ("c:/GNU/share/hunspell")))
       )
      )
  )

;(setq ispell-program-name "c:/GNU/bin/aspell")
;(setq ispell-program-name "aspell")
;(setq ispell-personal-dictionary "C:/GNU/custom.ispell")

;; (setq ispell-extra-args
;;       '("--data-dir" "C:/GNU/data"
;;         "--dict-dir" "C:/GNU/dict"
;;         )
;;       )

(require 'ispell)

(defun fd-switch-dictionary()
  (interactive)
      (let* ((dic ispell-current-dictionary)
             (change (if (string= dic "russian") "english" "russian")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))

;(require 'ispell-multi)
;(require 'flyspell-babel)

;(autoload 'flyspell-babel-setup "flyspell-babel")
;(add-hook 'latex-mode-hook 'flyspell-babel-setup)

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)
(global-set-key (kbd "<f7>")   'ispell-word)
(put 'upcase-region 'disabled nil)

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; enable a more powerful jump back function from ace jump mode
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; Standard Jedi.el setting

; does not work now.
;(add-hook 'python-mode-hook 'jedi:setup)
;(setq jedi:complete-on-dot t)

;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;; Then open Python file.

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(global-set-key "\C-x\ \C-m" 'magit-status)

(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

;; Multitran dictionary lookup
(defun multitran-lookup-english (keyword)
  (interactive (list (thing-at-point 'word)))
  (switch-to-buffer-other-window
   (eww
   ;; (w3m-goto-url
    (concat "http://multitran.ru/c/m.exe?l1=1&s=" keyword "&%CF%EE%E8%F1%EA=%CF%EE%E8%F1%EA")
    )
   )
  ;;(run-at-time 4 nil 'iconify-frame)
  )

(global-set-key (kbd "C-c m") 'multitran-lookup-english)
