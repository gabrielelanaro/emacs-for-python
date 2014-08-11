;;; eide-vc.el --- Emacs-IDE, version control (svn and git)

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

(provide 'eide-vc)

(require 'vc)

(require 'eide-config)

(defvar eide-vc-show-svn-status-flag nil)
(defvar eide-vc-show-git-status-flag nil)

(defvar eide-vc-svn-diff-full-command nil)
(defvar eide-vc-git-diff-full-command nil)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-show-svn-status 'auto "Show svn status of files in menu."
  :tag "Show svn status"
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "If root directory contains .svn directory" auto))
  :set 'eide-i-vc-custom-set-show-svn-status
  :initialize 'custom-initialize-default
  :group 'eide-version-control)
(defcustom eide-custom-show-git-status 'auto "Show git status of files in menu."
  :tag "Show git status"
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "If root directory contains .git directory" auto))
  :set 'eide-i-vc-custom-set-show-git-status
  :initialize 'custom-initialize-default
  :group 'eide-version-control)
(defcustom eide-custom-vc-diff-command "" "Version control diff command (svn diff --diff-cmd=<command>, git difftool -y --extcmd=<command>). Use default (svn diff, git diff) if empty."
  :tag "Version control diff command"
  :type 'string
  :set 'eide-i-vc-custom-set-vc-diff-command
  :initialize 'custom-initialize-default
  :group 'eide-version-control)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-vc-custom-set-show-svn-status (param value)
  "Set show svn status (eide-vc-show-svn-status-flag).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (when eide-config-ready
    (eide-vc-update-show-svn-status)))

(defun eide-i-vc-custom-set-show-git-status (param value)
  "Set show git status (eide-vc-show-git-status-flag).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (when eide-config-ready
    (eide-vc-update-show-git-status)))

(defun eide-i-vc-custom-set-vc-diff-command (param value)
  "Set vc diff command.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (when eide-config-ready
    (if (string-equal value "")
      (progn
        (setq eide-vc-svn-diff-full-command nil)
        (setq eide-vc-git-diff-full-command nil))
      (progn
        (setq eide-vc-svn-diff-full-command (concat "svn diff --diff-cmd=" value " "))
        (setq eide-vc-git-diff-full-command (concat "git difftool -y --extcmd=" value " "))))))

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-vc-diff (p-backend)
  "Call vc-diff on current buffer with specific backend.
Argument:
- p-backend: vc backend."
  (let ((l-vc-backend (vc-backend buffer-file-name)))
    ;; Temporary switch to specific backend (in case the file is under several version control systems)
    (vc-switch-backend buffer-file-name p-backend)
    (save-excursion
      (vc-diff nil))
    ;; Switch back to previous backend
    (vc-switch-backend buffer-file-name l-vc-backend)))

(defun eide-i-vc-blame (p-backend)
  "Call vc-annotate on current buffer with specific backend.
Argument:
- p-backend: vc backend."
  (let ((l-vc-backend (vc-backend buffer-file-name)))
    ;; Temporary switch to specific backend (in case the file is under several version control systems)
    (vc-switch-backend buffer-file-name p-backend)
    (save-excursion
      (vc-annotate buffer-file-name (vc-working-revision buffer-file-name)))
    ;; Switch back to previous backend
    (vc-switch-backend buffer-file-name l-vc-backend)))

(defun eide-i-vc-revert (p-backend)
  "Call vc-revert-file on current buffer with specific backend.
Argument:
- p-backend: vc backend."
  (let ((l-vc-backend (vc-backend buffer-file-name)))
    ;; Temporary switch to specific backend (in case the file is under several version control systems)
    (vc-switch-backend buffer-file-name p-backend)
    (save-excursion
      (vc-revert-file buffer-file-name))
    ;; Switch back to previous backend
    (vc-switch-backend buffer-file-name l-vc-backend)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-vc-apply-customization ()
  "Apply VC customization."
  (eide-i-vc-custom-set-show-svn-status 'eide-custom-show-svn-status eide-custom-show-svn-status)
  (eide-i-vc-custom-set-show-git-status 'eide-custom-show-git-status eide-custom-show-git-status)
  (eide-i-vc-custom-set-vc-diff-command 'eide-custom-vc-diff-command eide-custom-vc-diff-command))

(defun eide-vc-update-show-svn-status ()
  "Update show svn status."
  (if (equal eide-custom-show-svn-status 'auto)
    (if (file-exists-p (concat eide-root-directory ".svn"))
      (setq eide-vc-show-svn-status-flag t)
      (setq eide-vc-show-svn-status-flag nil))
    (setq eide-vc-show-svn-status-flag eide-custom-show-svn-status))
  (eide-menu-update t t))

(defun eide-vc-update-show-git-status ()
  "Update show git status."
  (if (equal eide-custom-show-git-status 'auto)
    (if (file-exists-p (concat eide-root-directory ".git"))
      (setq eide-vc-show-git-status-flag t)
      (setq eide-vc-show-git-status-flag nil))
    (setq eide-vc-show-git-status-flag eide-custom-show-git-status))
  (eide-menu-update t t))

(defun eide-vc-update-current-buffer-status ()
  "Update current buffer status (modified or not compared to vc repositories)."
  (when eide-vc-show-svn-status-flag
    (make-local-variable 'eide-menu-local-svn-modified-status-flag)
    (setq eide-menu-local-svn-modified-status-flag nil))
  (when eide-vc-show-git-status-flag
    (make-local-variable 'eide-menu-local-git-modified-status-flag)
    (setq eide-menu-local-git-modified-status-flag nil))
  (when (file-exists-p buffer-file-name)
    (let ((l-vc-backend (vc-backend buffer-file-name)))
      (when (and eide-vc-show-svn-status-flag (vc-svn-registered buffer-file-name))
        ;; Temporary switch to SVN backend (in case the file is under several version control systems)
        (vc-switch-backend buffer-file-name 'SVN)
        ;; NB: vc-state doesn't use selected backend, vc-workfile-unchanged-p does!
        (setq eide-menu-local-svn-modified-status-flag (not (vc-workfile-unchanged-p buffer-file-name))))
      (when (and eide-vc-show-git-status-flag (vc-git-registered buffer-file-name))
        ;; Temporary switch to Git backend (in case the file is under several version control systems)
        (vc-switch-backend buffer-file-name 'Git)
        ;; NB: vc-state doesn't use selected backend, vc-workfile-unchanged-p does!
        (setq eide-menu-local-git-modified-status-flag (not (vc-workfile-unchanged-p buffer-file-name))))
      ;; Switch back to previous backend
      (vc-switch-backend buffer-file-name l-vc-backend))))

(defun eide-vc-update-files-status (&optional p-files-list)
  "Update buffers vc status (modified or not).
Argument:
- p-files-list (optional): list of files to update (overrides
  eide-menu-files-list)."
  (when (or eide-vc-show-svn-status-flag eide-vc-show-git-status-flag)
    (save-current-buffer
      (let ((l-files-list nil))
        (if p-files-list
          (setq l-files-list p-files-list)
          (setq l-files-list eide-menu-files-list))
        (dolist (l-buffer-name l-files-list)
          (set-buffer l-buffer-name)
          (eide-vc-update-current-buffer-status))))))

(defun eide-vc-svn-diff ()
  "Execute \"svn diff\" on current buffer."
  (when (and eide-vc-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
    (if eide-vc-svn-diff-full-command
      (start-process-shell-command "svn-diff" nil (concat eide-vc-svn-diff-full-command buffer-file-name))
      (eide-i-vc-diff 'SVN))))

(defun eide-vc-svn-diff-files-in-directory (p-directory-name p-files-list-string)
  "Execute \"svn diff\" on a directory.
Arguments:
- p-directory-name: directory name.
- p-files-list-string: string containing files list."
  (when eide-vc-show-svn-status-flag
    (let ((l-full-directory-name nil))
      (if (string-match "^/" p-directory-name)
        (setq l-full-directory-name p-directory-name)
        (setq l-full-directory-name (concat eide-root-directory p-directory-name)))
      (if eide-vc-svn-diff-full-command
        (start-process-shell-command "svn-diff" nil (concat "cd " l-full-directory-name " && " eide-vc-svn-diff-full-command p-files-list-string))
        (shell-command (concat "cd " l-full-directory-name " && svn diff " p-files-list-string))))))

(defun eide-vc-svn-blame ()
  "Execute \"svn blame\" on current buffer."
  (when eide-vc-show-svn-status-flag
    (eide-i-vc-blame 'SVN)))

(defun eide-vc-svn-revert ()
  "Execute \"svn revert\" on current buffer."
  (when (and eide-vc-show-svn-status-flag eide-menu-local-svn-modified-status-flag)
    (eide-i-vc-revert 'SVN)))

(defun eide-vc-git-diff ()
  "Execute \"git diff\" on current buffer."
  (when (and eide-vc-show-git-status-flag eide-menu-local-git-modified-status-flag)
    (if eide-vc-git-diff-full-command
      (start-process-shell-command "git-diff" nil (concat eide-vc-git-diff-full-command buffer-file-name))
      (eide-i-vc-diff 'Git))))

(defun eide-vc-git-diff-files-in-directory (p-directory-name p-files-list-string)
  "Execute \"git diff\" on a directory.
Arguments:
- p-directory-name: directory name.
- p-files-list-string: string containing files list."
  (when eide-vc-show-git-status-flag
    (let ((l-full-directory-name nil))
      (if (string-match "^/" p-directory-name)
        (setq l-full-directory-name p-directory-name)
        (setq l-full-directory-name (concat eide-root-directory p-directory-name)))
      (if eide-vc-git-diff-full-command
        (start-process-shell-command "git-diff" nil (concat "cd " l-full-directory-name " && " eide-vc-git-diff-full-command p-files-list-string))
        (shell-command (concat "cd " l-full-directory-name " && git diff " p-files-list-string))))))

(defun eide-vc-git-blame ()
  "Execute \"git blame\" on current buffer."
  (when eide-vc-show-git-status-flag
    (eide-i-vc-blame 'Git)))

(defun eide-vc-git-checkout ()
  "Execute \"git checkout\" on current buffer."
  (when (and eide-vc-show-git-status-flag eide-menu-local-git-modified-status-flag)
    (eide-i-vc-revert 'Git)))

;;; eide-vc.el ends here
