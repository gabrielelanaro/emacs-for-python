;;; eide-popup.el --- Emacs-IDE, popup

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

(provide 'eide-popup)

(require 'eide-compare)
(require 'eide-config)
(require 'eide-vc)

(defvar eide-popup-menu nil)
(defvar eide-popup-menu-actions-list nil)
(defvar eide-popup-menu-separator-flag nil)

(defvar eide-message-dialog
  '(("continue" . "c")))

;; ----------------------------------------------------------------------------
;; OPTIONS
;; ----------------------------------------------------------------------------

(defvar eide-option-menu-buffer-popup-groups-flags nil)

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-popup-menu-init ()
  "Initialize a popup menu."
  (setq eide-popup-menu nil)
  (setq eide-popup-menu-actions-list nil)
  (unless eide-option-menu-buffer-popup-groups-flags
    (setq eide-popup-menu-separator-flag nil)))

(defun eide-i-popup-menu-add-action (p-action-name p-action-function p-enabled-flag)
  "Add an action in action list (for popup menu).
Arguments:
- p-action-name: action name in menu.
- p-action-function: action function.
- p-enabled-flag: t if this action is enabled."
  (when (> (length p-action-name) 120)
    (setq p-action-name (concat (substring p-action-name 0 120) " [...]")))
  (if p-enabled-flag
    (setq eide-popup-menu-actions-list (append (list (cons p-action-name p-action-function)) eide-popup-menu-actions-list))
    (setq eide-popup-menu-actions-list (append (list p-action-name) eide-popup-menu-actions-list))))

(defun eide-i-popup-menu-close-action-list (p-actions-list-name)
  "Add action list to popup menu.
Argument:
- p-actions-list-name: name of actions list."
  (when eide-popup-menu-actions-list
    (if eide-option-menu-buffer-popup-groups-flags
      (setq eide-popup-menu (append (list (cons p-actions-list-name eide-popup-menu-actions-list)) eide-popup-menu))
      (progn
        (if eide-popup-menu-separator-flag
          (setq eide-popup-menu (append (list (cons "-" "-")) eide-popup-menu))
          (setq eide-popup-menu-separator-flag t))
        (setq eide-popup-menu (append eide-popup-menu-actions-list eide-popup-menu)))))
  (setq eide-popup-menu-actions-list nil))

(defun eide-i-popup-menu-open (p-menu-title)
  "Open popup menu.
Argument:
- p-menu-title: title of popup menu."
  (when eide-popup-menu
    (setq eide-popup-menu (reverse eide-popup-menu))

    (unless eide-option-menu-buffer-popup-groups-flags
      (setq eide-popup-menu (list (cons "single group" eide-popup-menu))))

    (let ((l-result (x-popup-menu t (cons p-menu-title eide-popup-menu))))
      (if (bufferp l-result)
        (switch-to-buffer l-result)
        (eval (car (read-from-string l-result)))))))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-popup-message (p-string)
  "Display a message.
Argument:
- p-string: message."
  (x-popup-dialog t (cons p-string eide-message-dialog)))

(defun eide-popup-open-menu-for-directory ()
  "Open a popup menu related to selected directory."
  (interactive)
  (eide-windows-select-menu-window)
  (move-to-window-line (cdr (last (mouse-position))))

  (let ((l-directory-name-in-title (eide-menu-get-directory-name-on-current-line)) (l-directory-name nil))
    (setq l-directory-name (if (string-equal l-directory-name-in-title "./")
                             ""
                             l-directory-name-in-title))
    (eide-i-popup-menu-init)
    (eide-i-popup-menu-add-action "Close all files from this directory" (concat "(eide-menu-directory-close \"" l-directory-name "\")") t)

    (let ((l-buffer-read-only-flag nil) (l-buffer-read-write-flag nil)
          (l-buffer-status-none-flag nil) (l-buffer-status-new-flag nil) (l-buffer-status-ref-flag nil)
          (l-vc-backend nil) (l-buffer-vc-modified-flag nil) (l-vc-modified-files-list ""))
      ;; Parse list of open files, and find the ones located in this
      ;; directory, to check, for every possible property (read only, REF file,
      ;; ...) if at least one of them matches.
      (dolist (l-buffer eide-menu-files-list)
        (when (eide-menu-is-file-in-directory-p l-buffer l-directory-name)
          ;; The buffer is located in the directory
          (with-current-buffer l-buffer
            (unless (string-equal eide-menu-local-edit-status "nofile")
              ;; Check all properties
              (if buffer-read-only
                (setq l-buffer-read-only-flag t)
                (setq l-buffer-read-write-flag t))
              (if (string-equal eide-menu-local-edit-status "")
                (setq l-buffer-status-none-flag t)
                (if (string-equal eide-menu-local-edit-status "new")
                  (setq l-buffer-status-new-flag t)
                  (when (string-equal eide-menu-local-edit-status "ref")
                    (setq l-buffer-status-ref-flag t))))
              (when eide-vc-show-status-flag
                ;; Get VC backend (if not already set)
                (if (not l-vc-backend)
                  (setq l-vc-backend (vc-backend buffer-file-name)))
                (when eide-menu-local-vc-modified-status-flag
                  ;; At least one buffer is modified
                  (setq l-buffer-vc-modified-flag t)
                  ;; Get file name from buffer name (remove <n> if present)
                  (let ((l-index (string-match "<[0-9]+>$" l-buffer)) (l-file-name nil))
                    (if l-index
                      (setq l-file-name (substring l-buffer 0 l-index))
                      (setq l-file-name l-buffer))
                    (setq l-vc-modified-files-list (concat l-vc-modified-files-list " " l-file-name)))))))))
      ;; Actions are enabled only if it can apply to one buffer at least
      ;; "Edit" action list
      (eide-i-popup-menu-add-action "Set all files read/write" (concat "(eide-edit-action-on-directory 'eide-edit-set-rw \"" l-directory-name "\")") l-buffer-read-only-flag)
      (eide-i-popup-menu-add-action "Set all files read only" (concat "(eide-edit-action-on-directory 'eide-edit-set-r \"" l-directory-name "\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-add-action "Backup original files (REF) to work on copies (NEW)" (concat "(eide-edit-action-on-directory 'eide-edit-make-ref-file \"" l-directory-name "\")") l-buffer-status-none-flag)
      (eide-i-popup-menu-add-action "Switch to REF files" (concat "(eide-edit-action-on-directory 'eide-edit-use-ref-file \"" l-directory-name "\")") l-buffer-status-new-flag)
      (eide-i-popup-menu-add-action "Discard REF files" (concat "(eide-edit-action-on-directory 'eide-edit-discard-ref-file \"" l-directory-name "\" \"discard all REF files\")") l-buffer-status-new-flag)
      (eide-i-popup-menu-add-action "Restore REF files" (concat "(eide-edit-action-on-directory 'eide-edit-restore-ref-file \"" l-directory-name "\" \"restore all REF files\")") l-buffer-status-new-flag)
      (eide-i-popup-menu-add-action "Switch to NEW files" (concat "(eide-edit-action-on-directory 'eide-edit-use-new-file \"" l-directory-name "\")") l-buffer-status-ref-flag)
      (eide-i-popup-menu-add-action "Discard NEW files" (concat "(eide-edit-action-on-directory 'eide-edit-discard-new-file \"" l-directory-name "\" \"discard all NEW files\")") l-buffer-status-ref-flag)
      (eide-i-popup-menu-close-action-list "Edit")

      ;; "Clean" action list
      (eide-i-popup-menu-add-action "Untabify and indent all read/write files" (concat "(eide-edit-action-on-directory 'eide-edit-untabify-and-indent \"" l-directory-name "\" \"untabify and indent all read/write files\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-add-action "Delete trailing spaces in all read/write files" (concat "(eide-edit-action-on-directory 'eide-edit-delete-trailing-spaces \"" l-directory-name "\" \"delete trailing spaces in all read/write files\")") l-buffer-read-write-flag)
      (eide-i-popup-menu-close-action-list "Clean")

      ;; VC action list
      (when (and eide-vc-show-status-flag l-vc-backend)
        (if (equal l-vc-backend 'SVN)
          (progn
            (eide-i-popup-menu-add-action "svn diff" (concat "(eide-vc-svn-diff-files-in-directory \"" l-directory-name "\" \"" l-vc-modified-files-list "\")") l-buffer-vc-modified-flag)
            (eide-i-popup-menu-add-action "svn revert (all modified files)" (concat "(eide-edit-action-on-directory 'eide-vc-revert \"" l-directory-name "\" \"revert all modified files\")") l-buffer-vc-modified-flag))
          (if (equal l-vc-backend 'Git)
            (progn
              (eide-i-popup-menu-add-action "git diff" (concat "(eide-vc-git-diff-files-in-directory \"" l-directory-name "\" \"" l-vc-modified-files-list "\")") l-buffer-vc-modified-flag)
              (eide-i-popup-menu-add-action "git checkout (all modified files)" (concat "(eide-edit-action-on-directory 'eide-vc-revert \"" l-directory-name "\" \"checkout all modified files\")") l-buffer-vc-modified-flag))))
        (eide-i-popup-menu-close-action-list "VC")))

    (eide-i-popup-menu-open l-directory-name-in-title)))

(defun eide-popup-open-menu-for-file ()
  "Open a popup menu related to selected file."
  (interactive)
  (eide-windows-select-menu-window)
  (move-to-window-line (cdr (last (mouse-position))))

  (let ((l-buffer (eide-menu-get-buffer-name-on-current-line))
        (l-buffer-status nil) (l-buffer-rw-flag t)
        (l-buffer-vc-backend nil) (l-buffer-vc-modified-flag nil))
    (eide-i-popup-menu-init)

    (with-current-buffer l-buffer
      (setq l-buffer-status eide-menu-local-edit-status)

      ;; Check buffer status (r/w)
      (when buffer-read-only
        (setq l-buffer-rw-flag nil))
      ;; Check version control status
      (when eide-vc-show-status-flag
        (setq l-buffer-vc-backend (vc-backend buffer-file-name))
        (setq l-buffer-vc-modified-flag eide-menu-local-vc-modified-status-flag)))

    ;; "Edit" action list
    (eide-i-popup-menu-add-action "Close" (concat "(eide-menu-file-close \"" l-buffer "\")") t)
    (unless (string-equal l-buffer-status "nofile")
      (if l-buffer-rw-flag
        (eide-i-popup-menu-add-action "Set read only" (concat "(eide-edit-action-on-file 'eide-edit-set-r \"" l-buffer "\")") t)
        (eide-i-popup-menu-add-action "Set read/write" (concat "(eide-edit-action-on-file 'eide-edit-set-rw \"" l-buffer "\")") t))
      (if (string-equal l-buffer-status "ref")
        (eide-i-popup-menu-add-action "Switch to NEW file" (concat "(eide-edit-action-on-file 'eide-edit-use-new-file \"" l-buffer "\")") t)
        (if (string-equal l-buffer-status "new")
          (eide-i-popup-menu-add-action "Switch to REF file" (concat "(eide-edit-action-on-file 'eide-edit-use-ref-file \"" l-buffer "\")") t)
          (eide-i-popup-menu-add-action "Backup original file (REF) to work on a copy (NEW)" (concat "(eide-edit-action-on-file 'eide-edit-make-ref-file \"" l-buffer "\")") t)))
      (if (string-equal l-buffer-status "ref")
        (eide-i-popup-menu-add-action "Discard NEW file" (concat "(eide-edit-action-on-file 'eide-edit-discard-new-file \"" l-buffer "\" \"discard NEW file\")") t)
        (when (string-equal l-buffer-status "new")
          (eide-i-popup-menu-add-action "Discard REF file" (concat "(eide-edit-action-on-file 'eide-edit-discard-ref-file \"" l-buffer "\" \"discard REF file\")") t)
          (eide-i-popup-menu-add-action "Restore REF file" (concat "(eide-edit-action-on-file 'eide-edit-restore-ref-file \"" l-buffer "\" \"restore REF file\")") t))))
    (eide-i-popup-menu-close-action-list "Edit")

    (unless (string-equal l-buffer-status "nofile")
      ;; "Clean" action list
      (eide-i-popup-menu-add-action "Untabify and indent" (concat "(eide-edit-action-on-file 'eide-edit-untabify-and-indent \"" l-buffer "\" \"untabify and indent this file\")") l-buffer-rw-flag)
      (eide-i-popup-menu-add-action "Delete trailing spaces" (concat "(eide-edit-action-on-file 'eide-edit-delete-trailing-spaces \"" l-buffer "\" \"delete trailing spaces\")") l-buffer-rw-flag)
      (eide-i-popup-menu-close-action-list "Clean")

      ;; "Compare" action list
      (if (string-equal l-buffer-status "ref")
        (eide-i-popup-menu-add-action "Compare REF and NEW files" (concat "(eide-compare-with-new-file \"" l-buffer "\")") t)
        (when (string-equal l-buffer-status "new")
          (eide-i-popup-menu-add-action "Compare REF and NEW files" (concat "(eide-compare-with-ref-file \"" l-buffer "\")") t)))
      (when (and eide-compare-other-project-name (not (string-equal eide-root-directory eide-compare-other-project-directory)))
        (let ((l-directory (file-name-directory (buffer-file-name (get-buffer l-buffer)))))
          ;; Check that the file is not out of project
          (unless (string-equal (eide-project-get-short-directory l-directory) l-directory)
            (eide-i-popup-menu-add-action (concat "Compare with project \"" eide-compare-other-project-name "\"") (concat "(eide-compare-with-other-project \"" l-buffer "\")") t))))
      (eide-i-popup-menu-close-action-list "Compare")

      ;; VC action list
      (when (and eide-vc-show-status-flag l-buffer-vc-backend)
        (when l-buffer-vc-modified-flag
          (if (equal l-buffer-vc-backend 'SVN)
            (progn
              (eide-i-popup-menu-add-action "svn diff" (concat "(eide-edit-action-on-file 'eide-vc-svn-diff \"" l-buffer "\")") t)
              (eide-i-popup-menu-add-action "svn revert" (concat "(eide-edit-action-on-file 'eide-vc-revert \"" l-buffer "\" \"revert this file\")") t))
            (if (equal l-buffer-vc-backend 'Git)
              (progn
                (eide-i-popup-menu-add-action "git diff" (concat "(eide-edit-action-on-file 'eide-vc-git-diff \"" l-buffer "\")") t)
                (eide-i-popup-menu-add-action "git checkout" (concat "(eide-edit-action-on-file 'eide-vc-revert \"" l-buffer "\" \"checkout this file\")") t)))))
        (if (equal l-buffer-vc-backend 'SVN)
          (eide-i-popup-menu-add-action "svn blame" (concat "(eide-edit-action-on-file 'eide-vc-blame \"" l-buffer "\")") t)
          (if (equal l-buffer-vc-backend 'Git)
            (eide-i-popup-menu-add-action "git blame" (concat "(eide-edit-action-on-file 'eide-vc-blame \"" l-buffer "\")") t)))
        (eide-i-popup-menu-close-action-list "VC")))

    (eide-i-popup-menu-open l-buffer)))

(defun eide-popup-open-menu-for-search-results ()
  "Open a popup menu to select a buffer to display in \"output\" window."
  (eide-i-popup-menu-init)
  (when eide-menu-grep-results-list
    (dolist (l-grep-result eide-menu-grep-results-list)
      ;; Protect \ in grep search buffer name
      (let ((l-grep-result-parameter (replace-regexp-in-string "\\\\" "\\\\" l-grep-result t t)))
        (eide-i-popup-menu-add-action l-grep-result (concat "(eide-search-view-output-buffer \"" l-grep-result-parameter "\")") t)))
    (eide-i-popup-menu-close-action-list "Grep results"))
  (when eide-menu-cscope-results-list
    (dolist (l-cscope-result eide-menu-cscope-results-list)
      (eide-i-popup-menu-add-action l-cscope-result (concat "(eide-search-view-output-buffer \"" l-cscope-result "\")") t))
    (eide-i-popup-menu-close-action-list "Cscope results"))
  (when eide-menu-man-pages-list
    (dolist (l-man-page eide-menu-man-pages-list)
      (eide-i-popup-menu-add-action l-man-page (concat "(eide-search-view-output-buffer \"" l-man-page "\")") t))
    (eide-i-popup-menu-close-action-list "Man pages"))
  (eide-i-popup-menu-add-action "Compilation" (concat "(eide-search-view-output-buffer \"" eide-compilation-buffer "\")") eide-compilation-buffer)
  (eide-i-popup-menu-add-action "Execution" (concat "(eide-search-view-output-buffer \"" eide-execution-buffer "\")") eide-execution-buffer)
  (eide-i-popup-menu-add-action "Shell" (concat "(eide-search-view-output-buffer \"" eide-shell-buffer "\")") eide-shell-buffer)
  (eide-i-popup-menu-close-action-list "Compilation / Execution / Shell")
  (eide-i-popup-menu-add-action "Debug session" (concat "(gdb-restore-windows)") eide-project-is-gdb-session-running-flag)
  (eide-i-popup-menu-close-action-list "Debug")
  (eide-i-popup-menu-open "Switch to:"))

(defun eide-popup-open-menu-for-search-results-delete ()
  "Open a popup menu to select a search result to delete."
  (eide-i-popup-menu-init)
  (when eide-menu-grep-results-list
    (dolist (l-grep-result eide-menu-grep-results-list)
      ;; Protect \ in grep search buffer name
      (let ((l-grep-result-parameter (replace-regexp-in-string "\\\\" "\\\\" l-grep-result t t)))
        (eide-i-popup-menu-add-action (concat "Delete " l-grep-result) (concat "(eide-search-close-grep-buffer \"" l-grep-result-parameter "\")") t)))
    (when (> (length eide-menu-grep-results-list) 1)
      (eide-i-popup-menu-add-action "Delete all grep results" "(eide-search-close-all-grep-buffers)" t))
    (eide-i-popup-menu-close-action-list "Grep results"))
  (when eide-menu-cscope-results-list
    (dolist (l-cscope-result eide-menu-cscope-results-list)
      (eide-i-popup-menu-add-action (concat "Delete " l-cscope-result) (concat "(eide-search-close-cscope-buffer \"" l-cscope-result "\")") t))
    (when (> (length eide-menu-cscope-results-list) 1)
      (eide-i-popup-menu-add-action "Delete all cscope results" "(eide-search-close-all-cscope-buffers)" t))
    (eide-i-popup-menu-close-action-list "Cscope results"))
  (when eide-menu-man-pages-list
    (dolist (l-man-page eide-menu-man-pages-list)
      (eide-i-popup-menu-add-action (concat "Delete " l-man-page) (concat "(eide-search-close-man-buffer \"" l-man-page "\")") t))
    (when (> (length eide-menu-man-pages-list) 1)
      (eide-i-popup-menu-add-action "Delete all man pages" "(eide-search-close-all-man-buffers)" t))
    (eide-i-popup-menu-close-action-list "Man pages"))
  (eide-i-popup-menu-open "*** DELETE *** search results"))

(defun eide-popup-open-menu-for-search ()
  "Open a popup menu to search for selected text."
  (eide-i-popup-menu-init)
  (let ((l-string (buffer-substring-no-properties (region-beginning) (region-end))))
    (when eide-project-name
      (eide-i-popup-menu-add-action "Go to definition (tag)" (concat "(eide-search-find-tag \"" l-string "\")") t)
      (eide-i-popup-menu-add-action "Find symbol (cscope)" (concat "(eide-search-find-symbol \"" l-string "\")") t)
      (eide-i-popup-menu-add-action "Grep in whole project" (concat "(eide-search-grep-global \"" l-string "\")") t))
    (eide-i-popup-menu-add-action "Grep in current directory" (concat "(eide-search-grep-local \"" l-string "\")") t)
    (eide-i-popup-menu-close-action-list "Search")
    (eide-i-popup-menu-add-action "Read manual (man 1: Executable programs or shell commands)" (concat "(eide-search-read-man \"1 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 2: System calls)" (concat "(eide-search-read-man \"2 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 3: Library calls)" (concat "(eide-search-read-man \"3 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 4: Special files)" (concat "(eide-search-read-man \"4 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 5: File formats and conventions)" (concat "(eide-search-read-man \"5 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 6: Games)" (concat "(eide-search-read-man \"6 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 7: Miscellaneous)" (concat "(eide-search-read-man \"7 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man 8: System administration commands)" (concat "(eide-search-read-man \"8 " l-string "\")") t)
    (eide-i-popup-menu-add-action "Read manual (man -a: All)" (concat "(eide-search-read-man \"-a " l-string "\")") t)
    (eide-i-popup-menu-close-action-list "Man")
    (eide-i-popup-menu-open (concat "Search: " l-string))))

(defun eide-popup-open-menu-for-cleaning ()
  "Open a popup menu to clean selected lines."
  (eide-i-popup-menu-init)
  (eide-i-popup-menu-add-action "Untabify" "(progn (untabify (region-beginning) (region-end)) (save-buffer))" t)
  (eide-i-popup-menu-add-action "Indent" "(progn (indent-region (region-beginning) (region-end) nil) (save-buffer))" t)
  (eide-i-popup-menu-close-action-list "Cleaning")
  (eide-i-popup-menu-open "Clean selection"))

(defun eide-popup-open-menu-for-project ()
  "Open a popup menu related to selected project."
  (interactive)
  (move-to-window-line (cdr (last (mouse-position))))
  (beginning-of-line)
  (let ((l-project-name (buffer-substring-no-properties (point) (line-end-position))) (l-project-dir nil))
    (forward-line)
    (setq l-project-dir (buffer-substring-no-properties (point) (line-end-position)))
    (forward-line -1)
    (eide-i-popup-menu-init)
    (eide-i-popup-menu-add-action "Remove this project from current workspace" "(eide-project-remove-selected-project)" t)
    (eide-i-popup-menu-add-action (if (string-equal l-project-dir eide-compare-other-project-directory) "Unselect this project for comparison" "Select this project for comparison") "(eide-project-select-unselect-for-comparison)" t)
    (eide-i-popup-menu-close-action-list "Project")
    (eide-i-popup-menu-open (concat "Project: " l-project-name))))

;;; eide-popup.el ends here
