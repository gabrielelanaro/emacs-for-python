;;; eide-edit.el --- Emacs-IDE, edit

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

(provide 'eide-edit)

(require 'eide-popup)

;; ----------------------------------------------------------------------------
;; OPTIONS
;; ----------------------------------------------------------------------------

;; When using a file (.ref or .new for example), update file date,
;; so that compilation takes it into account.
(defvar eide-option-touch-files-when-using-flag t)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-edit-get-buffer-status ()
  "Get current buffer status (\"nofile\", \"ref\", \"new\" or \"\")."
  (if (not (file-exists-p buffer-file-name))
    "nofile"
    (if (file-exists-p (concat buffer-file-name ".ref"))
      "new"
      (if (file-exists-p (concat buffer-file-name ".new"))
        "ref"
        ""))))

(defun eide-edit-update-files-status (&optional p-files-list)
  "Update buffers edit status (\"nofile\", \"ref\", \"new\" or \"\").
Argument:
- p-files-list (optional): list of files to update (overrides
  eide-menu-files-list)."
  (save-current-buffer
    (let ((l-files-list nil))
      (if p-files-list
        (setq l-files-list p-files-list)
        (setq l-files-list eide-menu-files-list))
      (dolist (l-buffer-name l-files-list)
        (set-buffer l-buffer-name)
        (make-local-variable 'eide-menu-local-edit-status)
        (setq eide-menu-local-edit-status (eide-edit-get-buffer-status))))))

(defun eide-edit-set-rw ()
  "Set write permission for current file."
  (when buffer-read-only
    (shell-command (concat "chmod +w \"" buffer-file-name "\""))
    (revert-buffer)))

(defun eide-edit-set-r ()
  "Unset write permission for current file."
  (unless buffer-read-only
    (shell-command (concat "chmod -w \"" buffer-file-name "\""))
    (revert-buffer)))

(defun eide-edit-make-ref-file ()
  "Create \".ref\" version of current file, and use \".new\"."
  (when (string-equal eide-menu-local-edit-status "")
    (shell-command (concat "mv \"" buffer-file-name "\" \"" buffer-file-name ".ref\" ; cp \"" buffer-file-name ".ref\" \"" buffer-file-name "\" ; chmod +w \"" buffer-file-name "\""))
    (revert-buffer)))

;;(setq nnn (file-modes buffer-file-name))
;;(setq mmm (logior (file-modes buffer-file-name) 128))) ; = "chmod +w"
;; file-name-sans-extension
;; TODO: utiliser les commandes lisp équivalentes
;; (shell-command (concat "mv " buffer-file-name " " buffer-file-name ".ref ; cp " buffer-file-name ".ref " buffer-file-name " ; chmod +w " buffer-file-name))
;; (rename-file buffer-file-name (concat buffer-file-name ".ref"))
;; (copy-file (concat buffer-file-name ".ref") buffer-file-name)
;; (set-file-modes (logior (file-modes buffer-file-name) 128)) ; = "chmod +w"

(defun eide-edit-use-ref-file ()
  "Use \".ref\" version of current file."
  (when (string-equal eide-menu-local-edit-status "new")
    (shell-command (concat "mv \"" buffer-file-name "\" \"" buffer-file-name ".new\""))
    (shell-command (concat "mv \"" buffer-file-name ".ref\" \"" buffer-file-name "\""))
    (when eide-option-touch-files-when-using-flag
      (shell-command (concat "touch \"" buffer-file-name "\"")))
    (revert-buffer)))

(defun eide-edit-use-new-file ()
  "Use \".new\" version of current file."
  (when (string-equal eide-menu-local-edit-status "ref")
    (shell-command (concat "mv \"" buffer-file-name "\" \"" buffer-file-name ".ref\""))
    (shell-command (concat "mv \"" buffer-file-name ".new\" \"" buffer-file-name "\""))
    (when eide-option-touch-files-when-using-flag
      (shell-command (concat "touch \"" buffer-file-name "\"")))
    (revert-buffer)))

(defun eide-edit-discard-new-file ()
  "Discard \".new\" version of current file."
  (when (string-equal eide-menu-local-edit-status "ref")
    (shell-command (concat "rm -f \"" buffer-file-name ".new\""))
    (revert-buffer)))

(defun eide-edit-restore-ref-file ()
  "Restore \".ref\" version of current file."
  (when (string-equal eide-menu-local-edit-status "new")
    (shell-command (concat "rm -f \"" buffer-file-name "\" ; mv \"" buffer-file-name ".ref\" \"" buffer-file-name "\""))
    (when eide-option-touch-files-when-using-flag
      (shell-command (concat "touch \"" buffer-file-name "\"")))
    (revert-buffer)))

(defun eide-edit-discard-ref-file ()
  "Discard \".ref\" version of current file."
  (when (string-equal eide-menu-local-edit-status "new")
    (shell-command (concat "rm -f \"" buffer-file-name ".ref\""))
    (revert-buffer)))

(defun eide-edit-untabify-and-indent ()
  "Untabify and indent the content of current file."
  (unless buffer-read-only
    (untabify (point-min) (point-max))
    (indent-region (point-min) (point-max) nil)
    (save-buffer)))

(defun eide-edit-delete-trailing-spaces ()
  "Delete all trailing spaces in current file."
  (unless buffer-read-only
    (delete-trailing-whitespace)
    (save-buffer)))

(defun eide-edit-action-on-file (p-function p-buffer-name &optional p-confirmation-message)
  "Do an action on a file.
Arguments:
- p-function: function to call (once the buffer is current).
- p-buffer-name: buffer name.
- p-confirmation-message (optional): string for confirmation message, nil if
  confirmation is not required."
  (when (or (not p-confirmation-message)
            (y-or-n-p (concat "Do you really want to " p-confirmation-message "?")))
    (eide-menu-buffer-update-start p-buffer-name)
    (with-current-buffer p-buffer-name
      (funcall p-function))
    (eide-menu-buffer-update-stop p-buffer-name)))

(defun eide-edit-action-on-directory (p-function p-directory-name &optional p-confirmation-message)
  "Do an action on all open files in a directory.
Arguments:
- p-function: function to call (once the buffer is current).
- p-directory-name: directory name.
- p-confirmation-message (optional): string for confirmation message, nil if
  confirmation is not required."
  (when (or (not p-confirmation-message)
            (y-or-n-p (concat "Do you really want to " p-confirmation-message "?")))
    (eide-menu-directory-update-start p-directory-name)
    (dolist (l-buffer-name eide-menu-files-list)
      (when (eide-menu-is-file-in-directory-p l-buffer-name p-directory-name)
        (with-current-buffer l-buffer-name
          (when (file-exists-p buffer-file-name)
            (funcall p-function)))))
    (eide-menu-directory-update-stop p-directory-name)))

;;; eide-edit.el ends here
