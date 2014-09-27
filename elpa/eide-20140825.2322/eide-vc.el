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

(defvar eide-vc-show-status-flag nil)

(defvar eide-vc-svn-diff-full-command nil)
(defvar eide-vc-git-diff-full-command nil)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-show-vc-status 'auto "Show version control status of files in menu."
  :tag "Show svn status"
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (const :tag "If root directory is registered in a version control system (SVN or Git)" auto))
  :set 'eide-i-vc-custom-set-show-vc-status
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

(defun eide-i-vc-custom-set-show-vc-status (param value)
  "Set show VC status (eide-vc-show-status-flag).
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (when eide-config-ready
    (eide-vc-update-show-vc-status)))

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

(defun eide-i-vc-diff-files-in-directory (p-vc-backend p-directory-name p-files-list)
  "Execute VC diff on a directory.
Arguments:
- p-vc-backend: VC backend.
- p-directory-name: directory name.
- p-files-list: files list (string)."
  ;; Checking VC backend internally would require to recreate a buffer file name
  ;; from the arguments. It is much easier to give it in an argument, since it has
  ;; already been checked in order to display a specific command in the menu.
  (when eide-vc-show-status-flag
    (let ((l-full-directory-name nil))
      (if (string-match "^/" p-directory-name)
        (setq l-full-directory-name p-directory-name)
        (setq l-full-directory-name (concat eide-root-directory p-directory-name)))
      (if (equal p-vc-backend 'SVN)
        (if eide-vc-svn-diff-full-command
          (start-process-shell-command "svn-diff" nil (concat "cd " l-full-directory-name " && " eide-vc-svn-diff-full-command p-files-list))
          (shell-command (concat "cd " l-full-directory-name " && svn diff " p-files-list)))
        (if (equal p-vc-backend 'Git)
          (start-process-shell-command "git-diff" nil (concat "cd " l-full-directory-name " && " eide-vc-git-diff-full-command p-files-list))
          (shell-command (concat "cd " l-full-directory-name " && git diff " p-files-list)))))))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-vc-apply-customization ()
  "Apply VC customization."
  (eide-i-vc-custom-set-show-vc-status 'eide-custom-show-vc-status eide-custom-show-vc-status)
  (eide-i-vc-custom-set-vc-diff-command 'eide-custom-vc-diff-command eide-custom-vc-diff-command))

(defun eide-vc-update-show-vc-status ()
  "Update show VC status."
  (if (equal eide-custom-show-vc-status 'auto)
    (if (or (file-exists-p (concat eide-root-directory ".svn"))
            (file-exists-p (concat eide-root-directory ".git")))
      (setq eide-vc-show-status-flag t)
      (setq eide-vc-show-status-flag nil))
    (setq eide-vc-show-status-flag eide-custom-show-vc-status))
  (eide-menu-update t t))

(defun eide-vc-update-current-buffer-status ()
  "Update current buffer status (modified or not compared to vc repositories)."
  (when eide-vc-show-status-flag
    (make-local-variable 'eide-menu-local-vc-modified-status-flag)
    (setq eide-menu-local-vc-modified-status-flag (equal (vc-state buffer-file-name) 'edited))))

(defun eide-vc-update-files-status (&optional p-files-list)
  "Update buffers vc status (modified or not).
Argument:
- p-files-list (optional): list of files to update (overrides
  eide-menu-files-list)."
  (when eide-vc-show-status-flag
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
  (when (and eide-vc-show-status-flag eide-menu-local-vc-modified-status-flag)
    (if eide-vc-svn-diff-full-command
      (start-process-shell-command "svn-diff" nil (concat eide-vc-svn-diff-full-command buffer-file-name))
      (save-excursion
        (vc-diff nil)))))

(defun eide-vc-git-diff ()
  "Execute \"git diff\" on current buffer."
  (when (and eide-vc-show-status-flag eide-menu-local-vc-modified-status-flag)
    (if eide-vc-git-diff-full-command
      (start-process-shell-command "git-diff" nil (concat eide-vc-git-diff-full-command buffer-file-name))
      (save-excursion
        (vc-diff nil)))))

(defun eide-vc-svn-diff-files-in-directory (p-directory-name p-files-list)
  "Execute \"svn diff\" on a directory.
Arguments:
- p-directory-name: directory name.
- p-files-list: files list (string)."
  (eide-i-vc-diff-files-in-directory 'SVN p-directory-name p-files-list))

(defun eide-vc-git-diff-files-in-directory (p-directory-name p-files-list)
  "Execute \"git diff\" on a directory.
Arguments:
- p-directory-name: directory name.
- p-files-list: files list (string)."
  (eide-i-vc-diff-files-in-directory 'Git p-directory-name p-files-list))

(defun eide-vc-blame ()
  "Execute VC blame on current buffer."
  (when eide-vc-show-status-flag
    (save-excursion
      (vc-annotate buffer-file-name (vc-working-revision buffer-file-name)))))

(defun eide-vc-revert ()
  "Execute VC revert/checkout on current buffer."
  (when (and eide-vc-show-status-flag eide-menu-local-vc-modified-status-flag)
    (save-excursion
      (vc-revert-file buffer-file-name))))

;;; eide-vc.el ends here
