;;; eide-project.el --- Emacs-IDE, project

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

(provide 'eide-project)

(require 'compile)
(require 'desktop)
(require 'ansi-color)

(require 'eide-compare)
(require 'eide-config)
(require 'eide-popup)
(require 'eide-search)

;; Check --no-desktop option before it is removed from command-line-args by desktop in after-init-hook
(defvar eide-no-desktop-option nil)
(when (member "--no-desktop" command-line-args)
  (setq eide-no-desktop-option t))

;; Preserve "menu" buffer from desktop-clear
(setq desktop-clear-preserve-buffers (cons "\\* Menu \\*" desktop-clear-preserve-buffers))

;; expand-file-name replaces ~ with /home/<user>
(defvar eide-root-directory (expand-file-name default-directory))
(defvar eide-root-directory-at-startup eide-root-directory)

(defvar eide-project-config-file ".emacs-ide-project.cfg")
(defvar eide-project-notes-file  ".emacs-ide-project.txt")

(defvar eide-project-current-workspace 1)
(defvar eide-project-current-projects-list nil)

(defvar eide-project-name nil)

(defvar eide-project-background-color nil)
(defvar eide-project-foreground-color nil)

(defvar eide-project-gdb-option nil)
(defvar eide-project-tool-bar-mode-before-debug nil)
(defvar eide-project-is-gdb-session-running-flag nil)
(defvar eide-project-is-gdb-session-visible-flag nil)

(if (locate-library "gdb-mi")
  (progn
    (require 'gdb-mi)
    (setq eide-project-gdb-option " -i=mi "))
  (progn
    (require 'gdb-ui) ; deprecated
    (setq eide-project-gdb-option " --annotate=3 ")))

(defvar eide-project-projects-file "~/.emacs-ide/workspace1/projects-list")
(defvar eide-project-projects-buffer-name "* Emacs-IDE projects *")

(defvar eide-project-comparison-project-point nil)

(defvar eide-project-config-target-buffer nil)

(defvar eide-project-commands-enabled-flag nil)

;; Some config values are saved before editing, so that actions
;; can be performed in case they have been modified
(defvar eide-project-old-project-name nil)
(defvar eide-project-old-tags-exclude-value nil)
(defvar eide-project-old-cscope-exclude-files-value nil)
(defvar eide-project-old-cscope-exclude-dirs-value nil)

;; Variables to store the project configuration
;; (to avoid parsing the config file)
(defvar eide-project-init-command nil)
(defvar eide-project-compile-command-1 nil)
(defvar eide-project-compile-command-2 nil)
(defvar eide-project-compile-command-3 nil)
(defvar eide-project-compile-command-4 nil)
(defvar eide-project-run-command-1 nil)
(defvar eide-project-run-command-2 nil)
(defvar eide-project-debug-command nil)
(defvar eide-project-debug-program-1 nil)
(defvar eide-project-debug-program-2 nil)
(defvar eide-project-compile-error-old-path-regexp nil)
(defvar eide-project-compile-error-new-path-string nil)
(defvar eide-project-tags-exclude nil)
(defvar eide-project-cscope-exclude-files nil)
(defvar eide-project-cscope-exclude-dirs nil)
(defvar eide-project-grep-exclude-files nil)
(defvar eide-project-grep-exclude-dirs nil)

;; Config files
(make-face 'eide-project-config-comment-face)
(make-face 'eide-project-config-parameter-face)
(make-face 'eide-project-config-possibilities-face)
(make-face 'eide-project-config-separator-face)
(make-face 'eide-project-config-value-face)
(make-face-bold 'eide-project-config-parameter-face)

;; Projects list
(make-face 'eide-project-project-name-face)
(make-face-bold 'eide-project-project-name-face)
(make-face 'eide-project-project-current-name-face)
(make-face-bold 'eide-project-project-current-name-face)
(make-face 'eide-project-project-comparison-name-face)
(make-face-bold 'eide-project-project-comparison-name-face)

;; ----------------------------------------------------------------------------
;; SETTINGS FOR MAJOR MODE "EMACS-IDE-CONFIG"
;; ----------------------------------------------------------------------------

(define-derived-mode emacs-ide-config-mode fundamental-mode "Emacs-IDE config"
  (setq font-lock-defaults '('(("\\(#.*\\)"      1 'eide-project-config-comment-face) ; comment
                               ("\\(.*\\) = "    1 'eide-project-config-parameter-face) ; parameter
                               (" = "            . 'eide-project-config-separator-face) ; " = "
                               (" = \\(.*\\)"    1 'eide-project-config-value-face))))) ; value

(setq auto-mode-alist (append '(("\\.emacs-ide-project.cfg\\'" . emacs-ide-config-mode)) auto-mode-alist))

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-number-of-workspaces 2 "Number of workspaces (each workspace has got its own list of projects)."
  :tag "Number of workspaces"
  :type '(choice (const 1) (const 2) (const 3) (const 4) (const 5) (const 6) (const 7) (const 8))
  :set 'eide-i-project-custom-set-number-of-workspaces
  :initialize 'custom-initialize-default
  :group 'eide-project)
(defcustom eide-custom-support-ansi-escape-code-in-compilation-buffer t "Support ANSI escape code in compilation buffer."
  :tag "Support ANSI escape code in compilation buffer"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set 'eide-i-project-set-support-for-ansi-escape-code-in-compilation-buffer
  :initialize 'custom-initialize-default
  :group 'eide-project)
(defcustom eide-custom-project-default-init-command "" "This command is called before all 'compile' and 'run' commands."
  :tag "Default init command"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-1 "" "Default compile command (1)."
  :tag "Default compile command (1)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-2 "" "Default compile command (2)."
  :tag "Default compile command (2)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-3 "" "Default compile command (3)."
  :tag "Default compile command (3)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-command-4 "" "Default compile command (4)."
  :tag "Default compile command (4)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-run-command-1 "" "Default run command (1)."
  :tag "Default run command (1)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-run-command-2 "" "Default run command (2)."
  :tag "Default run command (2)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-debug-command "" "Default debug command."
  :tag "Default debug command"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-debug-program-1 "" "Default debug program (1)."
  :tag "Default debug program (1)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-debug-program-2 "" "Default debug program (2)."
  :tag "Default debug program (2)"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-error-old-path-regexp "" "Default compile error old path regexp (used to modify the path of filenames in the compilation buffer)."
  :tag "Default compile error old path regexp"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-compile-error-new-path-string "" "Default compile error new path string (used to modify the path of filenames in the compilation buffer)."
  :tag "Default compile error new path string"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-tags-exclude "" "Default space separated list of patterns (files or directories) to exclude when creating tags."
  :tag "Default tags exclude patterns"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-cscope-exclude-files "" "Default space separated list of files patterns to exclude when creating cscope list of files."
  :tag "Default cscope exclude files patterns"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-cscope-exclude-dirs "" "Default space separated list of directories patterns to exclude when creating cscope list of files."
  :tag "Default cscope exclude directories patterns"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-grep-exclude-files "" "Default space separated list of files patterns to exclude when searching with grep."
  :tag "Default grep exclude files patterns"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)
(defcustom eide-custom-project-default-grep-exclude-dirs "" "Default space separated list of directories patterns to exclude when searching with grep."
  :tag "Default grep exclude directories patterns"
  :type 'string
  :set '(lambda (param value) (set-default param value))
  :group 'eide-project)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-project-set-support-for-ansi-escape-code-in-compilation-buffer (param value)
  "Add/remove a hook on compilation filter to colorize output.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (when (boundp 'compilation-filter-hook)
    (if eide-custom-support-ansi-escape-code-in-compilation-buffer
      (add-hook 'compilation-filter-hook 'eide-i-colorize-compilation-buffer-hook)
      (remove-hook 'compilation-filter-hook 'eide-i-colorize-compilation-buffer-hook))))

(defun eide-i-project-custom-set-number-of-workspaces (param value)
  "Set number of workspaces.
Arguments:
- param: customization parameter.
- value: customization value."
  (set-default param value)
  (when eide-config-ready
    (eide-i-project-create-workspaces)))

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-project-create-workspaces ()
  "Create directories and files for workspaces, if missing."
  (let ((l-workspace-number 1))
    (while (<= l-workspace-number eide-custom-number-of-workspaces)
      (let ((l-workspace-dir nil) (l-projects-list-file nil))
        (setq l-workspace-dir (concat "~/.emacs-ide/workspace" (number-to-string l-workspace-number)))
        ;; "touch" command requires expand-file-name (which replaces ~ with /home/<user>)
        (setq l-projects-list-file (expand-file-name (concat l-workspace-dir "/projects-list")))
        (unless (file-directory-p l-workspace-dir)
          (make-directory l-workspace-dir))
        (unless (file-exists-p l-projects-list-file)
          (shell-command (concat "touch \"" l-projects-list-file "\""))))
      (setq l-workspace-number (+ l-workspace-number 1))))
  (eide-i-project-update-internal-projects-list))

(defun eide-i-project-force-desktop-read-hook ()
  "Hook to be called at startup, to force to read the desktop when after-init-hook
has already been called."
  (unless desktop-file-modtime
    ;; Desktop has not been read: read it now.
    (desktop-read eide-root-directory)))

(defun eide-i-project-update-frame-title ()
  "Update frame title with project name (or root directory if no project)."
  (if eide-project-name
    (setq frame-title-format (concat eide-project-name " - Emacs"))
    (setq frame-title-format (concat eide-root-directory " - Emacs"))))

(when (and (boundp 'compilation-filter-hook)
           (boundp 'compilation-filter-start))
  (defun eide-i-colorize-compilation-buffer-hook ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(defun eide-i-project-set-current-workspace (p-workspace-number)
  "Set current workspace.
Argument:
- p-workspace-number: new workspace number."
  (if (or (not eide-project-name) (and eide-search-tags-available-flag eide-search-cscope-available-flag))
    (when (<= p-workspace-number eide-custom-number-of-workspaces)
      (setq eide-project-current-workspace p-workspace-number)
      ;; Change projects list file
      (setq eide-project-projects-file (concat "~/.emacs-ide/workspace" (number-to-string p-workspace-number) "/projects-list"))
      ;; Restore initial root directory
      (setq eide-project-name nil)
      (setq eide-root-directory eide-root-directory-at-startup)
      ;; Clear the project selected for comparison
      (setq eide-compare-other-project-name nil)
      (setq eide-compare-other-project-directory nil)
      (unless eide-no-desktop-option
        ;; Clear desktop (even if a project is defined)
        (eide-windows-hide-ide-windows)
        (desktop-save-mode -1)
        ;; Close all buffers
        (desktop-clear)
        (setq desktop-dirname nil)
        (eide-menu-update t)
        (eide-windows-show-ide-windows))
      (eide-i-project-update-internal-projects-list)
      ;; Update default directory if current buffer is not visiting a file
      (unless buffer-file-name
        (setq default-directory eide-root-directory)))
    (eide-popup-message "Please wait for tags and cscope list of files to be created...")))

(defun eide-i-project-load (p-startup-flag p-creation-flag)
  "Update environment according to the project in root directory:
- close all files,
- load the project desktop,
- check project information (tags, cscope, configuration),
- add the project (or update its name) in projects list.
Arguments:
- p-startup-flag: t when called from the init.
- p-creation-flag: t when the project is created."
  ;; Get project name from directory
  ;; eide-root-directory:                                                     <...>/current_project/
  ;; directory-file-name removes last "/":                                    <...>/current_project
  ;; file-name-nondirectory retrieves last directory name from complete path: current_project
  (setq eide-project-name (file-name-nondirectory (directory-file-name eide-root-directory)))

  ;; Some options from project configuration are necessary to load a project
  ;; (exclude patterns for tags and cscope list of files, for example).
  ;; It is necessary to rebuild - or simply create - the config file now.
  ;; In case of creation, it will use the default values from customization.

  ;; Close any existing config file, to make sure we will use the right one
  (when (get-buffer eide-project-config-file)
    (kill-buffer eide-project-config-file))
  ;; Rebuild or create project file
  (eide-project-rebuild-config-file)

  ;; Tags and cscope list of files creation is started as soon as possible,
  ;; because it is executed in another process, in parallel with the loading of
  ;; the desktop.

  ;; Create tags if necessary
  (if (file-exists-p (concat eide-root-directory "TAGS"))
    (setq eide-search-tags-available-flag t)
    (eide-search-create-tags))
  ;; Load tags now, otherwise first tag search will take some time...
  ;;(find-file-noselect (concat eide-root-directory "TAGS"))

  (when eide-search-use-cscope-flag
    ;; Create cscope database if necessary
    (if (file-exists-p (concat eide-root-directory "cscope.files"))
      (progn
        (eide-search-update-cscope-status)
        (setq eide-search-cscope-available-flag t)
        (unless (file-exists-p (concat eide-root-directory "cscope.out"))
          (setq eide-search-cscope-update-database-request-pending-flag t)))
      (eide-search-create-cscope-list-of-files)))

  (unless (file-exists-p (concat eide-root-directory eide-project-notes-file))
    ;; Create empty project notes file
    (shell-command (concat "touch " eide-root-directory eide-project-notes-file)))

  ;; Update version control show status
  (eide-vc-update-show-vc-status)

  (unless eide-no-desktop-option
    (unless p-startup-flag
      ;; No need to update menu for every restored buffer
      (ad-deactivate 'switch-to-buffer))
    (if desktop-dirname
      ;; A desktop is already loaded: switch to the new one.
      ;; desktop-change-dir saves the desktop, close all buffers, and read the new desktop.
      (desktop-change-dir eide-root-directory)
      (progn
        ;; Enable desktop save mode: desktop is read and will be saved automatically on exit.
        (desktop-save-mode 1)
        ;; Desktop must be saved without asking (if .emacs.desktop does not exist)
        (setq desktop-save t)
        ;; Set desktop directory (set to nil when desktop save mode is disabled)
        (setq desktop-dirname eide-root-directory)
        (unless (or p-startup-flag p-creation-flag)
          ;; It is necessary to close all buffers before loading the new desktop.
          (desktop-clear)
          (desktop-read eide-root-directory))))
    (unless p-startup-flag
      (ad-activate 'switch-to-buffer)))

  ;; Close any existing TAGS file, to make sure we will use the right one
  (when (get-buffer "TAGS")
    (kill-buffer "TAGS"))
  ;; Use tags-table-list instead of tags-file-name because when switching to
  ;; another project, Emacs asks either to append or to overwrite tags file
  ;; name in the list, and we want to overwrite without asking
  (setq tags-table-list (list (concat eide-root-directory "TAGS")))

  ;; Set cscope root directory
  (when eide-search-use-cscope-flag
    (cscope-set-initial-directory eide-root-directory))

  ;; Close any existing config file, to make sure we will use the right one.
  ;; It was opened and rebuilt at the beginning, but the loading of the desktop
  ;; might have replaced it with another one.
  (when (get-buffer eide-project-config-file)
    (kill-buffer eide-project-config-file))
  ;; Open config file (already rebuilt at the beginning)
  (find-file-noselect (concat eide-root-directory eide-project-config-file))
  ;; Add the project to current workspace
  (eide-project-add-in-list))

(defun eide-i-project-update-internal-projects-list ()
  ;; Create internal projects list
  (setq eide-project-current-projects-list nil)
  (with-current-buffer (find-file-noselect eide-project-projects-file)
    (goto-char (point-min))
    (forward-line)
    (while (not (eobp))
      (push (buffer-substring-no-properties (point) (line-end-position)) eide-project-current-projects-list)
      (forward-line 2))
    (kill-this-buffer)))

(defun eide-i-project-open-selected-project ()
  "Open project on current line."
  (interactive)
  (if (or (not eide-project-name)
          (and eide-search-tags-available-flag
               (or (not eide-search-use-cscope-flag) eide-search-cscope-available-flag)))
    (let ((l-project-dir (progn (beginning-of-line) (forward-line) (buffer-substring-no-properties (point) (line-end-position)))))
      (if (file-directory-p l-project-dir)
        (progn
          ;; Close projects list (so that it can be modified by another Emacs session)
          (kill-this-buffer)
          ;; Restore editor configuration
          (eide-display-set-colors-for-files)
          (eide-keys-configure-for-editor)
          (unless (string-equal l-project-dir eide-root-directory)
            ;; Changing desktop (desktop-change-dir) sometimes unbuild the windows layout!...
            ;; Therefore it is necessary to unbuild it intentionally before loading the new desktop,
            ;; otherwise we get errors for non-existing windows
            (eide-windows-hide-ide-windows)
            ;; Set root directory
            (setq eide-root-directory l-project-dir)
            (eide-project-load-root-directory-content nil)
            (eide-menu-update t))
          (eide-windows-show-ide-windows))
        (when (y-or-n-p "This directory does not exist anymore... Do you want to remove this project from current workspace?")
          (let ((buffer-read-only nil))
            (setq eide-project-current-projects-list (remove l-project-dir eide-project-current-projects-list))
            (when (string-equal l-project-dir eide-compare-other-project-directory)
              ;; Clear the project selected for comparison
              (setq eide-compare-other-project-name nil)
              (setq eide-compare-other-project-directory nil))
            (forward-line -1)
            (delete-region (point) (progn (forward-line 2) (point)))
            (save-buffer)))))
    (eide-popup-message "Please wait for tags and cscope list of files to be created...")))

(defun eide-i-project-set-colors-for-config ()
  "Set colors for config buffer."
  ;; Save current colors
  (setq eide-display-background-color (face-background 'default))
  (setq eide-display-foreground-color (face-foreground 'default))
  (set-background-color eide-project-background-color)
  (set-foreground-color eide-project-foreground-color)
  (set-face-background 'fringe eide-project-background-color))

(defun eide-i-project-get-config-value-if-defined (p-parameter)
  "Get the value of a parameter in a config (current buffer), returns nil if
not defined.
Argument:
- p-parameter: config parameter."
  (goto-char (point-min))
  (if (re-search-forward (concat "^" p-parameter " = ") nil t)
    (buffer-substring-no-properties (point) (line-end-position))
    nil))

(defun eide-i-project-rebuild-config-line (p-parameter p-default-value p-var)
  "Update a line with a parameter and its value (use default value if not
found).
Arguments:
- p-parameter: config parameter.
- p-default-value: config default value.
- p-var: variable in which the value should be saved."
  (let ((l-value nil))
    (with-current-buffer eide-project-config-file
      (setq l-value (eide-i-project-get-config-value-if-defined p-parameter)))
    (unless l-value
      (setq l-value p-default-value))
    (insert p-parameter)
    (insert " = ")
    (insert l-value)
    (insert "\n")
    (set p-var l-value)))

(defun eide-i-project-compile (p-command)
  "Compile project.
Argument:
- p-command: compile command string."
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-root-directory)
  (let ((l-compile-command (eide-project-get-full-command p-command)))
    ;; Compile buffer name will be updated in eide-i-windows-display-buffer-function
    (setq eide-windows-update-output-buffer-id "c")
    (compile l-compile-command))
  (eide-windows-select-source-window t))

(defun eide-i-project-run (p-command)
  "Run project.
Argument:
- p-command: run command string."
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Changing current directory has no effect with shell-command
  ;; Instead, we must change current directory in the command itself
  ;; Command ends with "&" otherwise emacs gets frozen until gdb is closed
  (let ((l-run-command (concat "cd " eide-root-directory " ; " (eide-project-get-full-command p-command) " &")))
    ;; Run buffer name will be updated in eide-i-windows-display-buffer-function
    (setq eide-windows-update-output-buffer-id "r")
    (shell-command l-run-command)))

(defun eide-i-project-debug (p-program)
  "Debug project.
Argument:
- p-program: program string."
  (eide-windows-select-output-window)
  ;; Sometimes does not compile when a grep buffer is displayed
  ;; "compilation finished" is displayed in grep buffer!
  (switch-to-buffer "*results*")
  ;; Change current directory (of unused buffer "*results*")
  (setq default-directory eide-root-directory)
  (let ((l-eide-debug-command (eide-project-get-full-gdb-command p-program)))
    (gdb l-eide-debug-command)))

(defun eide-i-compilation-finished-hook (cur-buffer msg)
  "Change the path of filenames in compilation buffer."
  ;; Check that the process was a compilation (not a grep)
  (when (and eide-compilation-buffer
             (equal cur-buffer (get-buffer eide-compilation-buffer)))
    (unless (string-equal eide-project-compile-error-old-path-regexp "")
      ;; Replace all occurrences in compilation buffer
      (with-current-buffer cur-buffer
        (save-excursion
          (goto-char (point-min))
          (perform-replace eide-project-compile-error-old-path-regexp eide-project-compile-error-new-path-string nil t nil))))))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-project-init ()
  "Initialize project."
  (setq compilation-scroll-output 'first-error)
  (add-hook 'compilation-finish-functions 'eide-i-compilation-finished-hook))

(defun eide-project-set-commands-state (p-state-flag)
  "Disable/enable project commands."
  (setq eide-project-commands-enabled-flag p-state-flag))

(defun eide-project-apply-customization ()
  "Apply project customization."
  (eide-i-project-set-support-for-ansi-escape-code-in-compilation-buffer 'eide-custom-support-ansi-escape-code-in-compilation-buffer eide-custom-support-ansi-escape-code-in-compilation-buffer)
  (eide-i-project-create-workspaces))

(defun eide-project-apply-color-theme ()
  "Apply color theme (for project)."
  (when eide-config-ready
    (if (equal eide-display-color-theme 'dark)
      ;; "Dark" color theme
      (progn
        (setq eide-project-background-color "gray20")
        (setq eide-project-foreground-color "white")
        ;; Config files
        (set-face-foreground 'eide-project-config-comment-face "deep sky blue")
        (set-face-foreground 'eide-project-config-parameter-face "salmon")
        (set-face-foreground 'eide-project-config-possibilities-face "medium sea green")
        (set-face-foreground 'eide-project-config-separator-face "orange red")
        (set-face-background 'eide-project-config-value-face "gray30")
        (set-face-foreground 'eide-project-config-value-face "white")
        ;; Projects list
        (set-face-foreground 'eide-project-project-name-face "sandy brown")
        (set-face-background 'eide-project-project-current-name-face "dark red")
        (set-face-foreground 'eide-project-project-current-name-face "sandy brown")
        (set-face-background 'eide-project-project-comparison-name-face "dark green")
        (set-face-foreground 'eide-project-project-comparison-name-face "sandy brown"))
      ;; "Light" color theme
      (progn
        (setq eide-project-background-color "gray90")
        (setq eide-project-foreground-color "black")
        ;; Config files
        (set-face-foreground 'eide-project-config-comment-face "slate blue")
        (set-face-foreground 'eide-project-config-parameter-face "brown")
        (set-face-foreground 'eide-project-config-possibilities-face "sea green")
        (set-face-foreground 'eide-project-config-separator-face "red")
        (set-face-background 'eide-project-config-value-face "white")
        (set-face-foreground 'eide-project-config-value-face "black")
        ;; Projects list
        (set-face-foreground 'eide-project-project-name-face "red")
        (set-face-background 'eide-project-project-current-name-face "yellow")
        (set-face-foreground 'eide-project-project-current-name-face "red")
        (set-face-background 'eide-project-project-comparison-name-face "light green")
        (set-face-foreground 'eide-project-project-comparison-name-face "red")))))

(defun eide-project-switch-to-workspace-1 ()
  "Switch to workspace 1."
  (interactive)
  (eide-i-project-set-current-workspace 1))

(defun eide-project-switch-to-workspace-2 ()
  "Switch to workspace 2."
  (interactive)
  (eide-i-project-set-current-workspace 2))

(defun eide-project-switch-to-workspace-3 ()
  "Switch to workspace 3."
  (interactive)
  (eide-i-project-set-current-workspace 3))

(defun eide-project-switch-to-workspace-4 ()
  "Switch to workspace 4."
  (interactive)
  (eide-i-project-set-current-workspace 4))

(defun eide-project-switch-to-workspace-5 ()
  "Switch to workspace 5."
  (interactive)
  (eide-i-project-set-current-workspace 5))

(defun eide-project-switch-to-workspace-6 ()
  "Switch to workspace 6."
  (interactive)
  (eide-i-project-set-current-workspace 6))

(defun eide-project-switch-to-workspace-7 ()
  "Switch to workspace 7."
  (interactive)
  (eide-i-project-set-current-workspace 7))

(defun eide-project-switch-to-workspace-8 ()
  "Switch to workspace 8."
  (interactive)
  (eide-i-project-set-current-workspace 8))

(defun eide-project-create ()
  "Create a project in root directory, and add it in projects list."
  (interactive)
  (when (y-or-n-p (concat "Create a project in " eide-root-directory " ?"))
    (eide-windows-select-source-window t)
    ;; Create empty project file
    (shell-command (concat "touch " eide-root-directory eide-project-config-file))
    (eide-i-project-load nil t)
    ;; Update frame title
    (eide-i-project-update-frame-title)
    ;; Update project name in menu
    (eide-menu-update-project-name)
    ;; Update key bindings for project
    (eide-keys-configure-for-editor)))

(defun eide-project-delete ()
  "Delete current project."
  (interactive)
  (when (y-or-n-p (concat "Delete project in " eide-root-directory " ?"))
    ;; Stop creation of tags and cscope list of files (in case it is not finished yet)
    (when eide-search-tags-creation-in-progress-flag
      (delete-process "create-tags"))
    (when eide-search-cscope-creation-in-progress-flag
      (delete-process "create-cscope"))
    (setq eide-search-tags-available-flag nil)
    (setq eide-search-cscope-available-flag nil)
    (setq eide-search-tags-creation-in-progress-flag nil)
    (setq eide-search-cscope-creation-in-progress-flag nil)
    (setq eide-project-name nil)
    (kill-buffer eide-project-config-file)
    (when (get-buffer "TAGS")
      (kill-buffer "TAGS"))
    (shell-command (concat "cd " eide-root-directory " ; rm -f TAGS cscope.files cscope.out .emacs-ide-project.*"))
    ;; Delete desktop file and disable automatic saving
    (when eide-no-desktop-option
      ;; desktop-remove needs desktop-save-mode to be enabled
      (desktop-save-mode 1)
      (setq desktop-dirname eide-root-directory))
    (desktop-remove)
    (desktop-save-mode -1)
    (setq desktop-dirname nil)
    ;; Update frame title and menu (project is inactive now)
    (eide-i-project-update-frame-title)
    (eide-menu-update t)
    ;; Update key bindings for project
    (eide-keys-configure-for-editor)
    ;; Remove from projects list
    (eide-project-remove-from-list)))

(defun eide-project-load-root-directory-content (p-startup-flag)
  "Update environment according to root directory content.
If a project is defined:
- close all files,
- load the project desktop,
- check project information (tags, cscope, configuration),
- add the project (or update its name) in projects list,
- update the display.
Otherwise:
- close all files,
- disable the desktop,
- update the display.
Argument:
- p-startup-flag: t when called from the init."
  ;; Check if a project is defined, and start it.
  ;; NB: It is important to read desktop after mode-hooks have been defined,
  ;; otherwise mode-hooks may not apply.
  (if (file-exists-p (concat eide-root-directory eide-project-config-file))
    (progn
      ;; A project is defined in this directory
      (eide-i-project-load p-startup-flag nil)
      ;; When Emacs-IDE is loaded from a file after init ("emacs -l file.el"),
      ;; the desktop is not read, because after-init-hook has already been called.
      ;; In that case, we need to force to read it (except if --no-desktop option is set).
      ;; The solution is to register a hook on emacs-startup-hook, which is
      ;; called after the loading of file.el.
      ;; Drawback: a file in argument ("emacs -l file.el main.c") will be loaded
      ;; but not displayed, because desktop is read after the loading of main.c
      ;; and selects its own current buffer.
      (when (and p-startup-flag (not eide-no-desktop-option))
        (add-hook 'emacs-startup-hook 'eide-i-project-force-desktop-read-hook)))
    (progn
      ;; There is no project in this directory
      (setq eide-project-name nil)
      (unless eide-no-desktop-option
        (desktop-save-mode -1)
        ;; Close all buffers
        (desktop-clear)
        (setq desktop-dirname nil))))
  ;; Update frame title
  (eide-i-project-update-frame-title)
  ;; Start with "editor" mode
  (eide-keys-configure-for-editor)
  ;; Kill projects list in case it is present in desktop
  (when (get-buffer eide-project-projects-buffer-name)
    (kill-buffer eide-project-projects-buffer-name))
  ;; Close temporary buffers from ediff sessions (if emacs has been closed during
  ;; an ediff session, .emacs.desktop contains temporary buffers (.ref or .new
  ;; files) and they have been loaded in this new emacs session).
  (let ((l-buffer-name-list (mapcar 'buffer-name (buffer-list))))
    (dolist (l-buffer-name l-buffer-name-list)
      (when (or (string-match "^\* (REF)" l-buffer-name) (string-match "^\* (NEW)" l-buffer-name))
        ;; this is a "useless" buffer (.ref or .new)
        (kill-buffer l-buffer-name))))
  ;; Update default directory if current buffer is not visiting a file
  (unless buffer-file-name
    (setq default-directory eide-root-directory))
  ;; Set current buffer
  (setq eide-current-buffer (buffer-name)))

(defun eide-project-change-root ()
  "Change root directory."
  (interactive)
  (if (or (not eide-project-name) (and eide-search-tags-available-flag eide-search-cscope-available-flag))
    (let ((l-do-it t))
      (when (and (not eide-project-name)
                 eide-menu-files-list
                 (not (y-or-n-p "The list of open files will be lost. Do you want to continue?")))
        (setq l-do-it nil))
      (when l-do-it
        (let ((l-ide-windows-visible-flag eide-windows-ide-windows-visible-flag))
          ;; Changing desktop (desktop-change-dir) sometimes unbuild the windows layout!...
          ;; Therefore it is necessary to unbuild it intentionally before loading the new desktop,
          ;; otherwise we get errors for non-existing windows
          (eide-windows-hide-ide-windows)
          (call-interactively 'dired)
          ;; Set root directory (expand-file-name replaces ~ with /home/<user>)
          (setq eide-root-directory (expand-file-name default-directory))
          ;; Exit browsing mode (kill dired buffer)
          (eide-menu-browsing-mode-stop)
          (eide-project-load-root-directory-content nil)
          (eide-menu-update t)
          (when l-ide-windows-visible-flag
            (eide-windows-show-ide-windows)))))
    (eide-popup-message "Please wait for tags and cscope list of files to be created...")))

(defun eide-project-open-list ()
  "Display projects list (full frame), and rebuild internal projects list."
  (interactive)
  (let ((l-do-it t) (l-current-project-marker nil))
    (when (and (not eide-project-name)
               eide-menu-files-list
               (not (y-or-n-p "The list of open files will be lost if you select a project. Do you want to continue?")))
      (setq l-do-it nil))
    (when l-do-it
      ;; The internal projects list will also be rebuilt
      (setq eide-project-current-projects-list nil)
      (setq eide-project-comparison-project-point nil)
      (eide-windows-hide-ide-windows)
      (eide-windows-save-and-unbuild-layout)
      (eide-i-project-set-colors-for-config)
      (eide-keys-configure-for-special-buffer)
      (ad-deactivate 'switch-to-buffer)
      (if (get-buffer eide-project-projects-buffer-name)
        (switch-to-buffer eide-project-projects-buffer-name)
        (progn
          (find-file eide-project-projects-file)
          (rename-buffer eide-project-projects-buffer-name)))
      (goto-char (point-min))
      (forward-line)
      (while (not (eobp))
        (let ((l-project-dir (buffer-substring-no-properties (point) (line-end-position))))
          (forward-line -1)
          (if (string-equal l-project-dir eide-root-directory)
            (progn
              ;; Current project (can't be selected)
              (put-text-property (point) (line-end-position) 'face 'eide-project-project-current-name-face)
              (setq l-current-project-marker (point-marker)))
            (if (and eide-compare-other-project-name
                     (string-equal l-project-dir eide-compare-other-project-directory))
              ;; Project selected for comparison
              (progn
                (setq eide-project-comparison-project-point (point))
                (put-text-property (point) (line-end-position) 'face 'eide-project-project-comparison-name-face))
              ;; Other projects
              (put-text-property (point) (line-end-position) 'face 'eide-project-project-name-face)))
          (put-text-property (point) (line-end-position) 'keymap project-name-map)
          (put-text-property (point) (line-end-position) 'mouse-face 'highlight)
          (push l-project-dir eide-project-current-projects-list)
          (forward-line 3)))
      ;; Clear modified status (text properties don't need to be saved)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (if l-current-project-marker (marker-position l-current-project-marker) (point-min)))
      (ad-activate 'switch-to-buffer))))

(defun eide-project-add-in-list ()
  "Add current project to the projects list of current workspace."
  (interactive)
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (set-buffer (find-file-noselect eide-project-projects-file)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (progn
        ;; This project is already in the list
        (forward-line -1)
        (unless (string-equal eide-project-name (buffer-substring-no-properties (point) (line-end-position)))
          ;; Update the project name
          (delete-region (point) (line-end-position))
          (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-project-project-current-name-face)
          (save-buffer)))
      (progn
        ;; This project is not in the list: let's insert it in the right place
        ;; (root directories in alphabetical order)
        (goto-char (point-min))
        (forward-line)
        (while (and (not (eobp))
                    (string-lessp (buffer-substring-no-properties (point) (line-end-position)) eide-root-directory))
          (forward-line 2))
        (unless (eobp)
          (forward-line -1))
        (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-project-project-current-name-face)
        (insert "\n")
        (insert eide-root-directory)
        (insert "\n")
        (save-buffer)))
    (kill-this-buffer))
  (push eide-root-directory eide-project-current-projects-list))

(defun eide-project-remove-from-list ()
  "Remove current project from the projects list of current workspace."
  (interactive)
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (set-buffer (find-file-noselect eide-project-projects-file)))
    (goto-char (point-min))
    (when (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (forward-line -1)
      (delete-region (point) (progn (forward-line 2) (point)))
      (save-buffer))
    (kill-this-buffer))
  (setq eide-project-current-projects-list (remove eide-root-directory eide-project-current-projects-list))
  (when (string-equal eide-root-directory eide-compare-other-project-directory)
    ;; Clear the project selected for comparison
    (setq eide-compare-other-project-name nil)
    (setq eide-compare-other-project-directory nil)))

(defun eide-project-update-name ()
  "Update current project name in frame title and in the projects list of
current workspace."
  ;; Update frame title
  (eide-i-project-update-frame-title)
  ;; Update projects list
  (save-current-buffer
    (if (get-buffer eide-project-projects-buffer-name)
      (progn
        (set-buffer eide-project-projects-buffer-name)
        (setq buffer-read-only nil))
      (set-buffer (find-file-noselect eide-project-projects-file)))
    (goto-char (point-min))
    (when (re-search-forward (concat "^" eide-root-directory "$") nil t)
      (forward-line -1)
      (delete-region (point) (line-end-position))
      (put-text-property (point) (progn (insert eide-project-name) (point)) 'face 'eide-project-project-current-name-face)
      (save-buffer))
    (kill-this-buffer)))

(defun eide-project-remove-selected-project ()
  "Remove the project on current line from current workspace."
  (interactive)
  (when (y-or-n-p "Do you really want to remove this project? ")
    (let ((buffer-read-only nil))
      (forward-line)
      (let ((l-project-dir (buffer-substring-no-properties (point) (line-end-position))))
        (setq eide-project-current-projects-list (remove l-project-dir eide-project-current-projects-list))
        (when (string-equal l-project-dir eide-compare-other-project-directory)
          ;; Clear the project selected for comparison
          (setq eide-compare-other-project-name nil)
          (setq eide-compare-other-project-directory nil)))
      (forward-line -1)
      (delete-region (point) (progn (forward-line 2) (point)))
      (save-buffer))))

(defun eide-project-select-unselect-for-comparison ()
  "Select/unselect the project on current line for comparison."
  (interactive)
  (let ((buffer-read-only nil) (l-project-name nil) (l-project-dir nil))
    (setq l-project-name (buffer-substring-no-properties (point) (line-end-position)))
    (forward-line)
    (setq l-project-dir (buffer-substring-no-properties (point) (line-end-position)))
    (if (string-equal l-project-dir eide-compare-other-project-directory)
      ;; Unselect
      (eide-compare-select-another-project nil nil)
      ;; Select
      (eide-compare-select-another-project l-project-name l-project-dir))
    (forward-line -1)
    (let ((l-new-point (point)))
      (unless (string-equal l-project-dir eide-root-directory)
        ;; Highlight selected project
        (put-text-property (point) (line-end-position) 'face 'eide-project-project-comparison-name-face))
      (when eide-project-comparison-project-point
        ;; Clear previous selected project
        (save-excursion
          (goto-char eide-project-comparison-project-point)
          (forward-line)
          (let ((l-old-project-dir (buffer-substring-no-properties (point) (line-end-position))))
            (forward-line -1)
            (if (string-equal l-old-project-dir eide-root-directory)
              (put-text-property (point) (line-end-position) 'face 'eide-project-project-current-name-face)
              (put-text-property (point) (line-end-position) 'face 'eide-project-project-name-face)))))
      (setq eide-project-comparison-project-point l-new-point))
    ;; Clear modified status (text properties don't need to be saved)
    (set-buffer-modified-p nil)))

(defun eide-project-rebuild-config-file ()
  "Update project file."
  (save-current-buffer
    ;; Define target config file
    (setq eide-project-config-target-buffer (concat eide-project-config-file "_temp"))

    ;; Open these config files
    (unless (get-buffer eide-project-config-file)
      (find-file-noselect (concat eide-root-directory eide-project-config-file)))
    (get-buffer-create eide-project-config-target-buffer)
    (set-buffer eide-project-config-target-buffer)
    (erase-buffer)

    (insert "# *****************************************************************************\n")
    (insert "# Emacs-IDE project configuration\n")
    (insert "# *****************************************************************************\n\n")
    (insert "# --> Click right to exit this page.\n")
    (insert "# --> To restore the default value of a parameter, delete the line\n")
    (insert "#     (project configuration file is rebuilt when you exit this page).\n\n")

    (with-current-buffer eide-project-config-file
      (setq eide-project-name (eide-i-project-get-config-value-if-defined "project_name")))
    (when (or (not eide-project-name) (string-equal eide-project-name ""))
      ;; Get project name from directory:
      ;; - directory-file-name removes last "/"
      ;; - file-name-nondirectory retrieves last directory name from complete path
      (setq eide-project-name (file-name-nondirectory (directory-file-name eide-root-directory))))
    (insert "project_name = ")
    (insert eide-project-name)
    (insert "\n\n")

    (insert "# Init command is called before all 'compile' and 'run' commands.\n")
    (eide-i-project-rebuild-config-line "init_command"
                                        eide-custom-project-default-init-command
                                        'eide-project-init-command)
    (eide-i-project-rebuild-config-line "compile_command_1"
                                        eide-custom-project-default-compile-command-1
                                        'eide-project-compile-command-1)
    (eide-i-project-rebuild-config-line "compile_command_2"
                                        eide-custom-project-default-compile-command-2
                                        'eide-project-compile-command-2)
    (eide-i-project-rebuild-config-line "compile_command_3"
                                        eide-custom-project-default-compile-command-3
                                        'eide-project-compile-command-3)
    (eide-i-project-rebuild-config-line "compile_command_4"
                                        eide-custom-project-default-compile-command-4
                                        'eide-project-compile-command-4)
    (eide-i-project-rebuild-config-line "run_command_1"
                                        eide-custom-project-default-run-command-1
                                        'eide-project-run-command-1)
    (eide-i-project-rebuild-config-line "run_command_2"
                                        eide-custom-project-default-run-command-2
                                        'eide-project-run-command-2)
    (eide-i-project-rebuild-config-line "debug_command"
                                        eide-custom-project-default-debug-command
                                        'eide-project-debug-command)
    (eide-i-project-rebuild-config-line "debug_program_1"
                                        eide-custom-project-default-debug-program-1
                                        'eide-project-debug-program-1)
    (eide-i-project-rebuild-config-line "debug_program_2"
                                        eide-custom-project-default-debug-program-2
                                        'eide-project-debug-program-2)

    (insert "# In the compilation buffer, in the clickable filenames displayed when warnings or errors occur,\n")
    (insert "# you can replace all occurrences of 'compile_error_old_path_regexp' (a regular expression)\n")
    (insert "# with 'compile_error_new_path_string'\n")
    (insert "# It can be useful if the sources are copied to another place before being compiled,\n")
    (insert "# or if you need to modify a relative path.\n")
    (insert "# In both cases, you want to be able to open the right file when selecting an error.\n")
    (eide-i-project-rebuild-config-line "compile_error_old_path_regexp"
                                        eide-custom-project-default-compile-error-old-path-regexp
                                        'eide-project-compile-error-old-path-regexp)
    (eide-i-project-rebuild-config-line "compile_error_new_path_string"
                                        eide-custom-project-default-compile-error-new-path-string
                                        'eide-project-compile-error-new-path-string)

    (insert "# Space separated list of patterns (files or directories) to exclude when creating tags.\n")
    (insert "# Each <pattern> adds an option --exclude=<pattern> to ctags command.\n")
    (insert "# Examples:\n")
    (insert "# - Use foo pattern to exclude all directories and files named foo.\n")
    (insert "# - Use *foo* pattern to exclude all directories and files containing foo.\n")
    (insert "# - Use some/path/foo pattern to exclude only some/path/foo directory or file.\n")
    (eide-i-project-rebuild-config-line "tags_exclude"
                                        eide-custom-project-default-tags-exclude
                                        'eide-project-tags-exclude)

    (insert "# Space separated list of files patterns to exclude when creating cscope list of files.\n")
    (insert "# Each <pattern> adds an option ! -name \"<pattern>\" to find command.\n")
    (insert "# Examples:\n")
    (insert "# - Use foo pattern to exclude all files named foo.\n")
    (insert "# - Use *foo* pattern to exclude all files containing foo.\n")
    (eide-i-project-rebuild-config-line "cscope_exclude_files"
                                        eide-custom-project-default-cscope-exclude-files
                                        'eide-project-cscope-exclude-files)

    (insert "# Space separated list of directories patterns to exclude when creating cscope list of files.\n")
    (insert "# Each <pattern> adds an option ! -path \"*/<pattern>/*\" to find command.\n")
    (insert "# Examples:\n")
    (insert "# - Use foo pattern to exclude all directories named foo.\n")
    (insert "# - Use *foo* pattern to exclude all directories containing foo.\n")
    (eide-i-project-rebuild-config-line "cscope_exclude_dirs"
                                        eide-custom-project-default-cscope-exclude-dirs
                                        'eide-project-cscope-exclude-dirs)

    (insert "# Space separated list of files patterns to exclude when searching with grep.\n")
    (insert "# Each <pattern> adds an option --exclude=<pattern> to grep command.\n")
    (insert "# Examples:\n")
    (insert "# - Use foo pattern to exclude all files named foo.\n")
    (insert "# - Use *foo* pattern to exclude all files containing foo.\n")
    (eide-i-project-rebuild-config-line "grep_exclude_files"
                                        eide-custom-project-default-grep-exclude-files
                                        'eide-project-grep-exclude-files)

    (insert "# Space separated list of directories patterns to exclude when searching with grep.\n")
    (insert "# Each <pattern> adds an option --exclude-dir=<pattern> to grep command.\n")
    (insert "# Examples:\n")
    (insert "# - Use foo pattern to exclude all directories named foo.\n")
    (insert "# - Use *foo* pattern to exclude all directories containing foo.\n")
    (eide-i-project-rebuild-config-line "grep_exclude_dirs"
                                        eide-custom-project-default-grep-exclude-dirs
                                        'eide-project-grep-exclude-dirs)

    ;; Replace source file by target buffer if different
    (unless (equal (compare-buffer-substrings eide-project-config-file nil nil eide-project-config-target-buffer nil nil) 0)
      (set-buffer eide-project-config-file)
      (erase-buffer)
      (insert-buffer-substring eide-project-config-target-buffer)
      (save-buffer))
    ;; Close temporary buffer
    (kill-buffer eide-project-config-target-buffer)))

(defun eide-project-get-config-value (p-parameter)
  "Get the value of a parameter in project config (empty string if not defined).
Argument:
- p-parameter: config parameter."
  (save-current-buffer
    (unless (get-buffer eide-project-config-file)
      (find-file-noselect (concat eide-root-directory eide-project-config-file)))
    (set-buffer eide-project-config-file)
    (let ((l-value (eide-i-project-get-config-value-if-defined p-parameter)))
      (if l-value
        l-value
        ""))))

(defun eide-project-open-config-file ()
  "Display project file (full frame)."
  (interactive)

  ;; Save some config values (actions are required if they are modified)
  (setq eide-project-old-project-name eide-project-name)
  (setq eide-project-old-tags-exclude-value eide-project-tags-exclude)
  (setq eide-project-old-cscope-exclude-files-value eide-project-cscope-exclude-files)
  (setq eide-project-old-cscope-exclude-dirs-value eide-project-cscope-exclude-dirs)

  (eide-windows-hide-ide-windows)
  (eide-windows-save-and-unbuild-layout)
  (eide-i-project-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat eide-root-directory eide-project-config-file))
  ;; Don't show trailing whitespace in this buffer
  ;; (there is a space at the end of line when the value is empty)
  (setq show-trailing-whitespace nil)
  (goto-char (point-min)))

(defun eide-project-open-notes-file ()
  "Display project notes file (full frame)."
  (interactive)
  (eide-windows-hide-ide-windows)
  (eide-windows-save-and-unbuild-layout)
  (eide-i-project-set-colors-for-config)
  (eide-keys-configure-for-special-buffer)
  (eide-windows-find-file-without-advice (concat eide-root-directory eide-project-notes-file)))

(defun eide-project-get-full-command (p-command)
  "Get full command (init command + compile/run command).
Argument:
- p-command: command string."
  (if (string-equal eide-project-init-command "")
    p-command
    (concat eide-project-init-command " ; " p-command)))

(defun eide-project-get-full-gdb-command (p-program)
  "Get full gdb command (gdb command + gdb option + program name).
Argument:
- p-program: program string."
  (concat eide-project-debug-command eide-project-gdb-option p-program))

(defun eide-project-get-short-gdb-command (p-program)
  "Get short gdb command (short gdb command + gdb option + program name) for popup
menu (hide gdb command path).
Argument:
- p-program: program string."
  (let ((l-short-gdb-command nil))
    (if (string-match "/" eide-project-debug-command)
      (setq l-short-gdb-command (concat "[...]/" (car (last (split-string eide-project-debug-command "/")))))
      (setq l-short-gdb-command eide-project-debug-command))
    (concat l-short-gdb-command eide-project-gdb-option p-program)))

(defun eide-project-get-short-directory (p-directory)
  "Get the path relative to project root directory from absolute path if it is
part of the project (remove root directory from absolute path).
Argument:
- p-directory: directory (absolute path)."
  ;; Remove project base path if the file is part of it (otherwise display full path)
  (if (and (<= (length eide-root-directory) (length p-directory)) (string-equal eide-root-directory (substring p-directory 0 (length eide-root-directory))))
    (substring p-directory (length eide-root-directory))
    p-directory))

(defun eide-project-compile-1 ()
  "Compile project (1st compile command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-compile eide-project-compile-command-1)))

(defun eide-project-compile-2 ()
  "Compile project (2nd compile command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-compile eide-project-compile-command-2)))

(defun eide-project-compile-3 ()
  "Compile project (3rd compile command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-compile eide-project-compile-command-3)))

(defun eide-project-compile-4 ()
  "Compile project (4th compile command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-compile eide-project-compile-command-4)))

(defun eide-project-run-1 ()
  "Run project (1st run command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-run eide-project-run-command-1)))

(defun eide-project-run-2 ()
  "Run project (2nd run command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-run eide-project-run-command-2)))

(defun eide-project-debug-mode-start ()
  "Start debug mode."
  ;; Restore colors (in case user was reading help or config)
  (eide-display-set-colors-for-files)
  (eide-keys-configure-for-gdb)
  (eide-windows-hide-ide-windows)
  (when window-system
    ;; Show gdb toolbar
    ;; NB: eide-project-debug-mode-start may be called twice: do not overwrite
    ;; eide-project-tool-bar-mode-before-debug on second call
    (unless eide-project-is-gdb-session-visible-flag
      (setq eide-project-tool-bar-mode-before-debug tool-bar-mode))
    (tool-bar-mode 1))
  (setq display-buffer-function nil)
  (setq eide-project-is-gdb-session-visible-flag t)
  (setq eide-project-is-gdb-session-running-flag t))

(defun eide-project-debug-mode-stop ()
  "Stop debug mode."
  (interactive)
  (eide-keys-configure-for-editor)
  (eide-windows-show-ide-windows)
  (when window-system
    ;; Hide tool bar if necessary (restore previous state)
    (tool-bar-mode (if eide-project-tool-bar-mode-before-debug 1 -1)))
  (setq display-buffer-function 'eide-i-windows-display-buffer-function)
  (setq eide-project-is-gdb-session-visible-flag nil))

(defun eide-project-debug-1 ()
  "Debug project (1st debug command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-debug eide-project-debug-program-1)))

(defun eide-project-debug-2 ()
  "Debug project (2nd debug command)."
  (interactive)
  (when eide-project-commands-enabled-flag
    (eide-i-project-debug eide-project-debug-program-2)))

;; ----------------------------------------------------------------------------
;; KEYMAPS
;; ----------------------------------------------------------------------------

(setq project-name-map (make-sparse-keymap))
(define-key project-name-map [mouse-1] 'eide-i-project-open-selected-project)
(define-key project-name-map "\r" 'eide-i-project-open-selected-project)
(define-key project-name-map "\d" 'eide-project-remove-selected-project)
(define-key project-name-map "\s" 'eide-project-select-unselect-for-comparison)
(define-key project-name-map [mouse-3] 'eide-popup-open-menu-for-project)

;;; eide-project.el ends here
