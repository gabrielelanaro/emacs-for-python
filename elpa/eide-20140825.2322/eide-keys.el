;;; eide-keys.el --- Emacs-IDE, keys

;; Copyright (C) 2008-2014 Cédric Marie

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'eide-keys)

(defvar eide-keys-is-editor-configuration-active-flag nil)

(defvar eide-keys-user-f1 nil)
(defvar eide-keys-user-shift-f1 nil)
(defvar eide-keys-user-f2 nil)
(defvar eide-keys-user-shift-f2 nil)
(defvar eide-keys-user-f3 nil)
(defvar eide-keys-user-shift-f3 nil)
(defvar eide-keys-user-f4 nil)
(defvar eide-keys-user-shift-f4 nil)
(defvar eide-keys-user-f5 nil)
(defvar eide-keys-user-shift-f5 nil)
(defvar eide-keys-user-f6 nil)
(defvar eide-keys-user-shift-f6 nil)
(defvar eide-keys-user-f7 nil)
(defvar eide-keys-user-shift-f7 nil)
(defvar eide-keys-user-f8 nil)
(defvar eide-keys-user-shift-f8 nil)
(defvar eide-keys-user-f9 nil)
(defvar eide-keys-user-shift-f9 nil)
(defvar eide-keys-user-f10 nil)
(defvar eide-keys-user-shift-f10 nil)
(defvar eide-keys-user-f11 nil)
(defvar eide-keys-user-shift-f11 nil)
(defvar eide-keys-user-f12 nil)
(defvar eide-keys-user-shift-f12 nil)

(defvar eide-keys-custom-choice
  '(choice (const :tag "Don't override" nil)
           (function-item :tag "Back from definition (tag)" eide-search-back-from-tag)
           (function-item :tag "Go to definition (tag)" eide-search-find-tag-without-prompt)
           (function-item :tag "Go to definition (tag) (with prompt)" eide-search-find-tag-with-prompt)
           (function-item :tag "Go to alternate definition (tag)" eide-search-find-alternate-tag)
           (function-item :tag "Find symbol (cscope)" eide-search-find-symbol-without-prompt)
           (function-item :tag "Find symbol (cscope) (with prompt)" eide-search-find-symbol-with-prompt)
           (function-item :tag "Grep in whole project" eide-search-grep-global-without-prompt)
           (function-item :tag "Grep in whole project (with prompt)" eide-search-grep-global-with-prompt)
           (function-item :tag "Grep in current directory" eide-search-grep-local-without-prompt)
           (function-item :tag "Grep in current directory (with prompt)" eide-search-grep-local-with-prompt)
           (function-item :tag "Update all files" eide-menu-update-buffers)
           (function-item :tag "Close current file" eide-menu-kill-buffer)
           (function-item :tag "Go to previous grep result or compilation error" eide-search-grep-go-to-previous)
           (function-item :tag "Go to next grep result or compilation error" eide-search-grep-go-to-next)
           (function-item :tag "Compile (1)" eide-project-compile-1)
           (function-item :tag "Compile (2)" eide-project-compile-2)
           (function-item :tag "Compile (3)" eide-project-compile-3)
           (function-item :tag "Compile (4)" eide-project-compile-4)
           (function-item :tag "Run (1)" eide-project-run-1)
           (function-item :tag "Run (2)" eide-project-run-2)
           (function-item :tag "Debug (1)" eide-project-debug-1)
           (function-item :tag "Debug (2)" eide-project-debug-2)
           (function-item :tag "Toggle fullscreen mode" eide-windows-toggle-frame-fullscreen-mode)))

(defgroup eide-keys nil "Keys."
  :tag "Keys"
  :group 'eide-emacs-settings)
(defcustom eide-custom-key-f1 'eide-search-back-from-tag "F1 key."
  :tag "F1"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f1 'eide-search-find-alternate-tag "Shift-F1 key."
  :tag "Shift-F1"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f2 'eide-search-find-tag-without-prompt "F2 key."
  :tag "F2"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f2 'eide-search-find-tag-with-prompt "Shift-F2 key."
  :tag "Shift-F2"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f3 'eide-search-find-symbol-without-prompt "F3 key."
  :tag "F3"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f3 'eide-search-find-symbol-with-prompt "Shift-F3 key."
  :tag "Shift-F3"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f4 'eide-search-grep-global-without-prompt "F4 key."
  :tag "F4"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f4 'eide-search-grep-global-with-prompt "Shift-F4 key."
  :tag "Shift-F4"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f5 'eide-menu-update-buffers "F5 key."
  :tag "F5"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f5 'eide-menu-kill-buffer "Shift-F5 key."
  :tag "Shift-F5"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f6 'eide-search-grep-local-without-prompt "F6 key."
  :tag "F6"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f6 'eide-search-grep-local-with-prompt "Shift-F6 key."
  :tag "Shift-F6"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f7 'eide-search-grep-go-to-previous "F7 key."
  :tag "F7"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f7 nil "Shift-F7 key."
  :tag "Shift-F7"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f8 'eide-search-grep-go-to-next "F8 key."
  :tag "F8"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f8 nil "Shift-F8 key."
  :tag "Shift-F8"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f9 'eide-project-compile-1 "F9 key."
  :tag "F9"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f9 'eide-project-compile-2 "Shift-F9 key."
  :tag "Shift-F9"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f10 'eide-project-run-1 "F10 key."
  :tag "F10"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f10 'eide-project-run-2 "Shift-F10 key."
  :tag "Shift-F10"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f11 'eide-windows-toggle-frame-fullscreen-mode "F11 key."
  :tag "F11"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f11 nil "Shift-F11 key."
  :tag "Shift-F11"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-f12 'eide-shell-open "F12 key."
  :tag "F12"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)
(defcustom eide-custom-key-shift-f12 nil "Shift-F12 key."
  :tag "Shift-F12"
  :type eide-keys-custom-choice
  :set 'eide-i-config-set-keys
  :initialize 'custom-initialize-default
  :group 'eide-keys)

(defun eide-i-config-set-keys (param value)
  "Set keys.
- param: customization parameter.
- value: customization value."
  (set-default param value)
  ;; Set all keys (testing param is not very convenient...)
  (eide-keys-apply-emacs-settings))

;; ----------------------------------------------------------------------------
;; FUNCTIONS FOR MOVING
;; ----------------------------------------------------------------------------

(defun eide-i-keys-scroll-right-one-step ()
  "Scroll right in current buffer."
  (interactive)
  (let ((index 0))
    (while (and (not (eolp)) (< index 4))
      (progn (scroll-left) (forward-char) (setq index (1+ index))))))

(defun eide-i-keys-scroll-left-one-step ()
  "Scroll left in current buffer."
  (interactive)
  (let ((index 0))
    (while (and (not (bolp)) (< index 4))
      (progn (scroll-right) (backward-char) (setq index (1+ index))))))

;; ----------------------------------------------------------------------------
;; KEYS BINDINGS
;; ----------------------------------------------------------------------------

;; To delete selected text
;; delete-selection-mode requires Emacs 24
(delete-selection-mode)

;; Cut-copy-paste
;; (impossible to use Windows shortcuts, because Control-c and Control-x have
;; other meanings)
;; Alt-left:  Cut   (Control-x)
;; Alt-down:  Copy  (Control-c)
;; Alt-right: Paste (Control-v)
(global-set-key [M-left]  'kill-region)
(global-set-key [M-down]  'kill-ring-save)
(global-set-key [M-right] 'yank)

;; Cut-copy-paste with mouse
;; Control-mouse-1: Cut   (Control-x)
;; Control-mouse-2: Copy  (Control-c)
;; Control-mouse-3: Paste (Control-v)
(global-unset-key [C-mouse-1])
(global-unset-key [C-mouse-2])
(global-unset-key [C-mouse-3])
(global-set-key [C-down-mouse-1] 'kill-region)
(global-set-key [C-down-mouse-2] 'kill-ring-save)
(global-set-key [C-down-mouse-3] 'yank)

;; Symbol completion
;;(global-set-key [C-tab] 'complete-symbol)

;; Display possible symbols one after the other
(global-set-key [C-tab] 'dabbrev-expand)
;; Display the list of possible symbols (in another window)
;;(global-set-key [S-tab] 'dabbrev-completion)

;; Override find-file, to get default directory from "source" window
(global-set-key "\C-x\C-f" 'eide-windows-find-file)
;; Override save-buffer, to save buffer in "source" window
(global-set-key "\C-x\C-s" 'eide-windows-save-buffer)

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-keys-enable-keys-misc ()
  "Set key bindings for misc features."
  ;; Block hiding
  ;;(global-set-key [C-f1] 'hs-hide-block)
  ;;(global-set-key [C-f2] 'hs-show-block)
  ;;(global-set-key [C-f3] 'hs-hide-all)
  ;;(global-set-key [C-f4] 'hs-show-all)

  (global-set-key [mouse-3] 'eide-windows-handle-mouse-3)
  (global-set-key [M-return] 'eide-windows-show-hide-ide-windows)
  (global-set-key [C-M-return] 'eide-project-open-list)
  (global-set-key [S-down-mouse-3] 'eide-windows-handle-shift-mouse-3)
  ;; Shift + Wheel up (horizontal scrolling)
  (global-set-key [S-mouse-4] 'eide-i-keys-scroll-right-one-step)
  ;; Shift + Wheel down (horizontal scrolling)
  (global-set-key [S-mouse-5] 'eide-i-keys-scroll-left-one-step))

(defun eide-i-keys-disable-keys-misc ()
  "Unset key bindings for misc features."
  ;; Block hiding
  ;;(global-unset-key [C-f1])
  ;;(global-unset-key [C-f2])
  ;;(global-unset-key [C-f3])
  ;;(global-unset-key [C-f4])

  (global-unset-key [mouse-3])
  (global-unset-key [M-return])
  (global-unset-key [C-M-return])
  (global-unset-key [S-down-mouse-3])
  ;; Shift + Wheel up (horizontal scrolling)
  (global-unset-key [S-mouse-4])
  ;; Shift + Wheel down (horizontal scrolling)
  (global-unset-key [S-mouse-5]))

(defun eide-i-keys-enable-keys-for-ediff ()
  "Set key bindings for ediff session."
  (global-set-key [mouse-3] 'eide-compare-quit)
  (global-set-key [M-return] 'eide-compare-quit)
  (global-set-key [f1] 'eide-compare-copy-a-to-b)
  (global-set-key [f2] 'eide-compare-copy-b-to-a)
  (global-set-key [f5] 'eide-compare-update)
  (global-set-key [f7] 'eide-compare-go-to-previous-diff)
  (global-set-key [f8] 'eide-compare-go-to-next-diff))

(defun eide-i-keys-enable-keys-for-gdb ()
  "Set key bindings for gdb session."
  (global-set-key [mouse-3] 'eide-project-debug-mode-stop)
  (global-set-key [M-return] 'eide-project-debug-mode-stop))

(defun eide-i-keys-enable-keys-for-special-buffer ()
  "Set key bindings for configuration editing."
  (global-set-key [mouse-3] 'eide-windows-switch-to-editor-mode)
  (global-set-key [M-return] 'eide-windows-switch-to-editor-mode)
  (global-set-key [C-M-return] 'eide-windows-switch-to-editor-mode))

(global-set-key [mouse-2] 'eide-windows-handle-mouse-2)

;; Disable some mode-line default key bindings (mouse-delete-window and mouse-delete-other-windows)
(global-set-key [mode-line mouse-2] nil)
(global-set-key [mode-line mouse-3] nil)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-keys-save-emacs-settings ()
  "Save Emacs settings (for keys)."
  (setq eide-keys-user-f1 (global-key-binding [f1]))
  (setq eide-keys-user-shift-f1 (global-key-binding [S-f1]))
  (setq eide-keys-user-f2 (global-key-binding [f2]))
  (setq eide-keys-user-shift-f2 (global-key-binding [S-f2]))
  (setq eide-keys-user-f3 (global-key-binding [f3]))
  (setq eide-keys-user-shift-f3 (global-key-binding [S-f3]))
  (setq eide-keys-user-f4 (global-key-binding [f4]))
  (setq eide-keys-user-shift-f4 (global-key-binding [S-f4]))
  (setq eide-keys-user-f5 (global-key-binding [f5]))
  (setq eide-keys-user-shift-f5 (global-key-binding [S-f5]))
  (setq eide-keys-user-f6 (global-key-binding [f6]))
  (setq eide-keys-user-shift-f6 (global-key-binding [S-f6]))
  (setq eide-keys-user-f7 (global-key-binding [f7]))
  (setq eide-keys-user-shift-f7 (global-key-binding [S-f7]))
  (setq eide-keys-user-f8 (global-key-binding [f8]))
  (setq eide-keys-user-shift-f8 (global-key-binding [S-f8]))
  (setq eide-keys-user-f9 (global-key-binding [f9]))
  (setq eide-keys-user-shift-f9 (global-key-binding [S-f9]))
  (setq eide-keys-user-f10 (global-key-binding [f10]))
  (setq eide-keys-user-shift-f10 (global-key-binding [S-f10]))
  (setq eide-keys-user-f11 (global-key-binding [f11]))
  (setq eide-keys-user-shift-f11 (global-key-binding [S-f11]))
  (setq eide-keys-user-f12 (global-key-binding [f12]))
  (setq eide-keys-user-shift-f12 (global-key-binding [S-f12])))

(defun eide-keys-apply-emacs-settings ()
  "Apply Emacs settings (for keys)."
  (if eide-custom-override-emacs-settings
    (progn
      (if eide-custom-key-f1
        (global-set-key [f1] eide-custom-key-f1)
        (global-set-key [f1] eide-keys-user-f1))
      (if eide-custom-key-shift-f1
        (global-set-key [S-f1] eide-custom-key-shift-f1)
        (global-set-key [S-f1] eide-keys-user-shift-f1))
      (if eide-custom-key-f2
        (global-set-key [f2] eide-custom-key-f2)
        (global-set-key [f2] eide-keys-user-f2))
      (if eide-custom-key-shift-f2
        (global-set-key [S-f2] eide-custom-key-shift-f2)
        (global-set-key [S-f2] eide-keys-user-shift-f2))
      (if eide-custom-key-f3
        (global-set-key [f3] eide-custom-key-f3)
        (global-set-key [f3] eide-keys-user-f3))
      (if eide-custom-key-shift-f3
        (global-set-key [S-f3] eide-custom-key-shift-f3)
        (global-set-key [S-f3] eide-keys-user-shift-f3))
      (if eide-custom-key-f4
        (global-set-key [f4] eide-custom-key-f4)
        (global-set-key [f4] eide-keys-user-f4))
      (if eide-custom-key-shift-f4
        (global-set-key [S-f4] eide-custom-key-shift-f4)
        (global-set-key [S-f4] eide-keys-user-shift-f4))
      (if eide-custom-key-f5
        (global-set-key [f5] eide-custom-key-f5)
        (global-set-key [f5] eide-keys-user-f5))
      (if eide-custom-key-shift-f5
        (global-set-key [S-f5] eide-custom-key-shift-f5)
        (global-set-key [S-f5] eide-keys-user-shift-f5))
      (if eide-custom-key-f6
        (global-set-key [f6] eide-custom-key-f6)
        (global-set-key [f6] eide-keys-user-f6))
      (if eide-custom-key-shift-f6
        (global-set-key [S-f6] eide-custom-key-shift-f6)
        (global-set-key [S-f6] eide-keys-user-shift-f6))
      (if eide-custom-key-f7
        (global-set-key [f7] eide-custom-key-f7)
        (global-set-key [f7] eide-keys-user-f7))
      (if eide-custom-key-shift-f7
        (global-set-key [S-f7] eide-custom-key-shift-f7)
        (global-set-key [S-f7] eide-keys-user-shift-f7))
      (if eide-custom-key-f8
        (global-set-key [f8] eide-custom-key-f8)
        (global-set-key [f8] eide-keys-user-f8))
      (if eide-custom-key-shift-f8
        (global-set-key [S-f8] eide-custom-key-shift-f8)
        (global-set-key [S-f8] eide-keys-user-shift-f8))
      (if eide-custom-key-f9
        (global-set-key [f9] eide-custom-key-f9)
        (global-set-key [f9] eide-keys-user-f9))
      (if eide-custom-key-shift-f9
        (global-set-key [S-f9] eide-custom-key-shift-f9)
        (global-set-key [S-f9] eide-keys-user-shift-f9))
      (if eide-custom-key-f10
        (global-set-key [f10] eide-custom-key-f10)
        (global-set-key [f10] eide-keys-user-f10))
      (if eide-custom-key-shift-f10
        (global-set-key [S-f10] eide-custom-key-shift-f10)
        (global-set-key [S-f10] eide-keys-user-shift-f10))
      (if eide-custom-key-f11
        (global-set-key [f11] eide-custom-key-f11)
        (global-set-key [f11] eide-keys-user-f11))
      (if eide-custom-key-shift-f11
        (global-set-key [S-f11] eide-custom-key-shift-f11)
        (global-set-key [S-f11] eide-keys-user-shift-f11))
      (if eide-custom-key-f12
        (global-set-key [f12] eide-custom-key-f12)
        (global-set-key [f12] eide-keys-user-f12))
      (if eide-custom-key-shift-f12
        (global-set-key [S-f12] eide-custom-key-shift-f12)
        (global-set-key [S-f12] eide-keys-user-shift-f12)))
    (progn
      (global-set-key [f1] eide-keys-user-f1)
      (global-set-key [S-f1] eide-keys-user-shift-f1)
      (global-set-key [f2] eide-keys-user-f2)
      (global-set-key [S-f2] eide-keys-user-shift-f2)
      (global-set-key [f3] eide-keys-user-f3)
      (global-set-key [S-f3] eide-keys-user-shift-f3)
      (global-set-key [f4] eide-keys-user-f4)
      (global-set-key [S-f4] eide-keys-user-shift-f4)
      (global-set-key [f5] eide-keys-user-f5)
      (global-set-key [S-f5] eide-keys-user-shift-f5)
      (global-set-key [f6] eide-keys-user-f6)
      (global-set-key [S-f6] eide-keys-user-shift-f6)
      (global-set-key [f7] eide-keys-user-f7)
      (global-set-key [S-f7] eide-keys-user-shift-f7)
      (global-set-key [f8] eide-keys-user-f8)
      (global-set-key [S-f8] eide-keys-user-shift-f8)
      (global-set-key [f9] eide-keys-user-f9)
      (global-set-key [S-f9] eide-keys-user-shift-f9)
      (global-set-key [f10] eide-keys-user-f10)
      (global-set-key [S-f10] eide-keys-user-shift-f10)
      (global-set-key [f11] eide-keys-user-f11)
      (global-set-key [S-f11] eide-keys-user-shift-f11)
      (global-set-key [f12] eide-keys-user-f12)
      (global-set-key [S-f12] eide-keys-user-shift-f12))))

(defun eide-keys-configure-for-editor ()
  "Configure keys for edition mode."
  (setq eide-keys-is-editor-configuration-active-flag t)
  (if eide-project-name
    (progn
      (eide-search-set-tags-and-cscope-state t)
      (eide-project-set-commands-state t))
    (progn
      (eide-search-set-tags-and-cscope-state nil)
      (eide-project-set-commands-state nil)))
  (eide-search-set-grep-state t)
  (eide-menu-set-update-state t)
  (eide-i-keys-enable-keys-misc)
  ;; Set F1-F12 keys
  (eide-keys-apply-emacs-settings))

(defun eide-keys-configure-for-ediff ()
  "Configure keys for ediff session."
  (setq eide-keys-is-editor-configuration-active-flag nil)

  (eide-search-set-tags-and-cscope-state nil)
  (eide-search-set-grep-state nil)
  (eide-menu-set-update-state nil)
  (eide-project-set-commands-state nil)
  (eide-i-keys-disable-keys-misc)
  (eide-i-keys-enable-keys-for-ediff))

(defun eide-keys-configure-for-gdb ()
  "Configure keys for gdb session."
  (setq eide-keys-is-editor-configuration-active-flag nil)

  (eide-search-set-tags-and-cscope-state nil)
  (eide-search-set-grep-state nil)
  (eide-menu-set-update-state nil)
  (eide-project-set-commands-state nil)

  (eide-i-keys-disable-keys-misc)
  (eide-i-keys-enable-keys-for-gdb))

(defun eide-keys-configure-for-special-buffer ()
  "Configure keys for configuration editing."
  (setq eide-keys-is-editor-configuration-active-flag nil)

  (eide-search-set-tags-and-cscope-state nil)
  (eide-search-set-grep-state nil)
  (eide-menu-set-update-state nil)
  (eide-project-set-commands-state nil)

  (eide-i-keys-disable-keys-misc)
  (eide-i-keys-enable-keys-for-special-buffer))

;;; eide-keys.el ends here
