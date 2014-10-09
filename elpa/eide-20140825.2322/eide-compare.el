;;; eide-compare.el --- Emacs-IDE, compare

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

(provide 'eide-compare)

(require 'ediff)
(require 'hideshow) ; for hs-minor-mode

(require 'eide-config)
(require 'eide-keys)
(require 'eide-menu)
(require 'eide-project)
(require 'eide-windows)

(defvar eide-compare-other-project-name nil)
(defvar eide-compare-other-project-directory nil)

(defvar eide-compare-buffer-name nil)
(defvar eide-compare-current-point nil)
(defvar eide-compare-other-buffer-name nil)

;; Ediff
(copy-face 'default 'ediff-even-diff-face-A)
(set-face-background 'ediff-even-diff-face-A "wheat")
(set-face-foreground 'ediff-even-diff-face-A "black")

(copy-face 'default 'ediff-even-diff-face-B)
(set-face-background 'ediff-even-diff-face-B "wheat")
(set-face-foreground 'ediff-even-diff-face-B "black")

(copy-face 'default 'ediff-odd-diff-face-A)
(set-face-background 'ediff-odd-diff-face-A "wheat")
(set-face-foreground 'ediff-odd-diff-face-A "black")

(copy-face 'default 'ediff-odd-diff-face-B)
(set-face-background 'ediff-odd-diff-face-B "wheat")
(set-face-foreground 'ediff-odd-diff-face-B "black")

;; Current difference: what is common or only in one buffer
(copy-face 'default 'ediff-current-diff-face-A)
(set-face-background 'ediff-current-diff-face-A "pink")
(set-face-foreground 'ediff-current-diff-face-A "black")

(copy-face 'default 'ediff-current-diff-face-B)
(set-face-background 'ediff-current-diff-face-B "pink")
(set-face-foreground 'ediff-current-diff-face-B "black")

;; Current difference: what really differs
(copy-face 'default 'ediff-fine-diff-face-A)
(set-face-background 'ediff-fine-diff-face-A "plum")
(set-face-foreground 'ediff-fine-diff-face-A "black")

(copy-face 'default 'ediff-fine-diff-face-B)
(set-face-background 'ediff-fine-diff-face-B "plum")
(set-face-foreground 'ediff-fine-diff-face-B "black")

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-compare-ediff-mode-start ()
  "Start ediff mode."
  (ad-deactivate 'select-window)
  (eide-windows-hide-ide-windows)
  (eide-windows-save-and-unbuild-layout)
  (eide-keys-configure-for-ediff))

(defun eide-i-compare-ediff-mode-stop ()
  "Stop ediff mode."
  (ad-activate 'select-window)
  (eide-keys-configure-for-editor)
  (eide-windows-restore-layout)
  (eide-windows-show-ide-windows))

(defun eide-i-compare-ediff-quit-hook ()
  "Hook for exiting ediff: Close temporary buffer, and restore display."
  ;; Call default hook
  (ediff-cleanup-mess)
  ;; Restore default hook
  (setq ediff-quit-hook 'ediff-cleanup-mess)
  (eide-i-compare-ediff-mode-stop)
  ;; Restore cursor position in the buffer that has been compared
  (set-buffer eide-compare-buffer-name)
  (goto-char eide-compare-current-point)
  ;; Back to current buffer
  (switch-to-buffer eide-current-buffer)
  (kill-buffer eide-compare-other-buffer-name))

(defun eide-i-compare-ediff-buffer-and-file (p-other-buffer-filename p-other-buffer-name-prefix p-buffer-in-left-window-flag p-force-major-mode-flag)
  "Compare a buffer and a file.
Arguments:
- p-other-buffer-filename: name of file to compare.
- p-other-buffer-name-prefix: prefix to add before file buffer name.
- p-buffer-in-left-window-flag: t = buffer | file, nil = file | buffer.
- p-force-major-mode-flag: t = force syntax highlighting for file (necessary for
  \".ref\" or \".new\" files)"
  (eide-i-compare-ediff-mode-start)
  (setq ediff-quit-hook 'eide-i-compare-ediff-quit-hook)
  ;; Hide IDE windows
  (eide-windows-hide-ide-windows)
  ;; Save current position in buffer to compare
  (set-buffer eide-compare-buffer-name)
  (setq eide-compare-current-point (point))

  (if p-force-major-mode-flag
    (let ((l-auto-mode-alist auto-mode-alist))
      ;; Add .ref and .new files in auto-mode-alist (with current buffer major
      ;; mode)
      (push (cons "\\.ref\\'" major-mode) auto-mode-alist)
      (push (cons "\\.new\\'" major-mode) auto-mode-alist)
      (eide-windows-find-file-without-advice p-other-buffer-filename)
      ;; Restore auto-mode-alist
      (setq auto-mode-alist l-auto-mode-alist)
      ;; Turn hide/show mode off, because if emacs is closed before this
      ;; temporary buffer is closed, it will be loaded next time, with an error
      ;; because default major mode is Fundamental
      (when hs-minor-mode
        (hs-minor-mode)))
    (eide-windows-find-file-without-advice p-other-buffer-filename))

  (setq eide-compare-other-buffer-name (concat p-other-buffer-name-prefix eide-compare-buffer-name))
  (rename-buffer eide-compare-other-buffer-name)
  (if p-buffer-in-left-window-flag
    (ediff-buffers eide-compare-buffer-name eide-compare-other-buffer-name)
    (ediff-buffers eide-compare-other-buffer-name eide-compare-buffer-name)))

(defun eide-i-compare-select-control-window ()
  "Select ediff control window (before calling ediff command)."
  (let ((l-control-window nil))
    (with-current-buffer "*Ediff Control Panel*"
      (setq l-control-window ediff-control-window))
    (select-window l-control-window)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-compare-select-another-project (p-project-name p-project-directory)
  "Select another project for comparison. Unselect it if both arguments are nil.
Arguments:
- p-project-name: project name.
- p-project-directory: project directory."
  (setq eide-compare-other-project-name p-project-name)
  (setq eide-compare-other-project-directory p-project-directory))

(defun eide-compare-with-ref-file (p-buffer-name)
  "Compare selected file (\".new\" version) with \".ref\" version.
Argument:
- p-buffer-name: name of buffer to compare."
  (setq eide-compare-buffer-name p-buffer-name)
  (eide-i-compare-ediff-buffer-and-file (concat (buffer-file-name (get-buffer eide-compare-buffer-name)) ".ref") "* (REF) " nil t))

(defun eide-compare-with-new-file (p-buffer-name)
  "Compare selected file (\".ref\" version) with \".new\" version.
Argument:
- p-buffer-name: name of buffer to compare."
  (setq eide-compare-buffer-name p-buffer-name)
  (eide-i-compare-ediff-buffer-and-file (concat (buffer-file-name (get-buffer eide-compare-buffer-name)) ".new") "* (NEW) " t t))

(defun eide-compare-with-other-project (p-buffer-name)
  "Compare selected file with version in another project.
Argument:
- p-buffer-name: name of buffer to compare."
  (setq eide-compare-buffer-name p-buffer-name)
  (let ((l-other-file (concat eide-compare-other-project-directory (substring (buffer-file-name (get-buffer eide-compare-buffer-name)) (length eide-root-directory)))))
    (if (file-exists-p l-other-file)
      (eide-i-compare-ediff-buffer-and-file (concat eide-compare-other-project-directory (substring (buffer-file-name (get-buffer eide-compare-buffer-name)) (length eide-root-directory))) (concat "* (" eide-compare-other-project-name ") ") nil nil)
      (eide-popup-message "This file doesn't exist in the other project."))))

(defun eide-compare-quit ()
  "Quit ediff session."
  (interactive)
  (eide-i-compare-select-control-window)
  (call-interactively 'ediff-quit))

(defun eide-compare-update ()
  "Update diffs."
  (interactive)
  (eide-i-compare-select-control-window)
  (ediff-update-diffs))

(defun eide-compare-go-to-previous-diff ()
  "Go to previous diff."
  (interactive)
  (eide-i-compare-select-control-window)
  (ediff-previous-difference))

(defun eide-compare-go-to-next-diff ()
  "Go to next diff."
  (interactive)
  (eide-i-compare-select-control-window)
  (ediff-next-difference))

(defun eide-compare-copy-a-to-b ()
  "Copy A to B."
  (interactive)
  (eide-i-compare-select-control-window)
  (call-interactively 'ediff-copy-A-to-B))

(defun eide-compare-copy-b-to-a ()
  "Copy B to A."
  (interactive)
  (eide-i-compare-select-control-window)
  (call-interactively 'ediff-copy-B-to-A))

;;; eide-compare.el ends here
