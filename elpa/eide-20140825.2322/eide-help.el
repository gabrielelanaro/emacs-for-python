;;; eide-help.el --- Emacs-IDE, help

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

(provide 'eide-help)

(require 'eide-keys)
(require 'eide-windows)

(defvar eide-version "2.0.0+")

(defvar eide-help-background-color nil)
(defvar eide-help-foreground-color nil)

;; Faces
(make-face 'eide-help-title-face)
(make-face 'eide-help-chapter1-face)
(make-face 'eide-help-chapter2-face)

(make-face-bold 'eide-help-title-face)

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-help-insert-header-1 (p-string)
  "Insert chapter title (level 1) in \"help\" buffer.
Argument:
- p-string: chapter title (string)."
  (insert "\n\n\n")
  (put-text-property (point) (progn (insert (concat p-string "\n")) (point)) 'face 'eide-help-chapter1-face)
  (insert "\n"))

(defun eide-i-help-insert-header-2 (p-string)
  "Insert chapter title (level 2) in \"help\" buffer.
Argument:
- p-string: chapter title (string)."
  (insert "\n")
  (put-text-property (point) (progn (insert (concat p-string "\n")) (point)) 'face 'eide-help-chapter2-face))

(defun eide-i-help-set-colors-for-help ()
  "Set colors for \"help\" buffer."
  ;; Save current colors
  (setq eide-display-background-color (face-background 'default))
  (setq eide-display-foreground-color (face-foreground 'default))
  (set-background-color eide-help-background-color)
  (set-foreground-color eide-help-foreground-color)
  (set-face-background 'fringe eide-help-background-color))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-help-apply-color-theme ()
  "Apply color theme (for help)."
  (if (equal eide-display-color-theme 'dark)
    ;; "Dark" color theme
    (progn
      (setq eide-help-background-color "black")
      (setq eide-help-foreground-color "gray95")
      (set-face-background 'eide-help-title-face "indian red")
      (set-face-foreground 'eide-help-title-face "white")
      (set-face-background 'eide-help-chapter1-face "brown")
      (set-face-foreground 'eide-help-chapter1-face "yellow")
      (set-face-background 'eide-help-chapter2-face "dark slate gray")
      (set-face-foreground 'eide-help-chapter2-face "pale green"))
    ;; "Light" color theme
    (progn
      (setq eide-help-background-color "white")
      (setq eide-help-foreground-color "black")
      (set-face-background 'eide-help-title-face "gold")
      (set-face-foreground 'eide-help-title-face "brown")
      (set-face-background 'eide-help-chapter1-face "yellow")
      (set-face-foreground 'eide-help-chapter1-face "red")
      (set-face-background 'eide-help-chapter2-face "lavender")
      (set-face-foreground 'eide-help-chapter2-face "blue"))))

(defun eide-help-open ()
  "Display help (full frame)."
  (interactive)
  (eide-windows-hide-ide-windows)
  (eide-windows-save-and-unbuild-layout)
  (eide-i-help-set-colors-for-help)
  (eide-keys-configure-for-special-buffer)

  (when (get-buffer "* Help *")
    (kill-buffer "* Help *"))
  (switch-to-buffer (get-buffer-create "* Help *"))

  (insert "\n")
  (put-text-property (point) (progn (insert "Emacs-IDE help page\n") (point)) 'face 'eide-help-title-face)
  (insert "\n")

  (insert "(click right to exit this page)\n\n")
  (insert (concat "Emacs-IDE - version " eide-version "\n"))

  (eide-i-help-insert-header-1 "Windows layout")

  (eide-i-help-insert-header-2 "Overview")

  (insert "
With default configuration, windows layout should look like this:

  -----------------------------------------------------------
  |                                         |               |
  |                                         |               |
  |       'source' window                   | 'menu' window |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  |                                         |               |
  -----------------------------------------------------------
  |                                                         |
  |               'output' window                           |
  |                                                         |
  -----------------------------------------------------------

You can resize all windows.
You can modify the layout in configuration.

")

  (eide-i-help-insert-header-1 "Mouse actions")

  (eide-i-help-insert-header-2 "Right click (when no text is selected)")

  (insert "
Right click behaviour depends on mouse position:

In 'source' window:
    Show/hide other windows (to view code full screen).

In 'menu' window:
    Open project popup menu:
    - close all files
    - project creation/configuration
    - project commands (compile, run, debug)
    - tags and cscope update
    - change root directory
    - display projects list
    - switch to another workspace
    - configuration
    - help (this page)

In 'menu' window, over a file name:
    Open file popup menu (see 'Actions on files' below).

In 'menu' window, over a directory name:
    Open directory popup menu (see 'Actions on directories' below).

In 'output' window:
    Open output popup menu (to display existing grep result, cscope result, man
    page, compilation buffer, execution buffer, shell, or debug session).

Shift + right click behaviour depends on mouse position:

In 'output' window:
    Open results deletion popup menu (to delete existing grep result, cscope
    result, or man page).
")

  (eide-i-help-insert-header-2 "Right click (when text is selected)")

  (insert "
If text is selected on a single line:
    Open search popup menu, for selected string (tag, cscope, grep, or man
    page).

If text is selected over several lines:
    Open cleaning popup menu (to untabify or indent selection).
")

  (eide-i-help-insert-header-2 "Left / right click on mode-line file name")

  (insert "
On 'source' window mode-line:
    Switch to previous / next file.

On 'output' window mode-line:
    Switch to previous / next output buffer.
")

  (eide-i-help-insert-header-2 "Middle click")

  (insert "
Middle click behaviour depends on mouse position:

In 'menu' window:
    Display file browser (to open a file).

In other windows:
    Paste (standard behaviour).
")

  (eide-i-help-insert-header-2 "Wheel")

  (insert "
Shift + mouse wheel up/down scrolls right/left.
")

  (eide-i-help-insert-header-1 "Configuration")

  (insert "
Configuration uses Emacs customization. Customized parameters are saved in
'.emacs', in your home directory.
To edit configuration, open project popup menu and select 'Configuration'.

Configuration covers topics such as display, coding rules, and default
parameters for new projects (see '.emacs-ide-project.cfg' below).
")

  (eide-i-help-insert-header-1 "Workspaces / projects")

  (eide-i-help-insert-header-2 "Workspaces")

  (insert "
A workspace is a collection of projects. You can use different workspaces if
you don't want to mix some projects in the same list.
")

  (eide-i-help-insert-header-2 "Projects")

  (insert "
A project is defined by its root directory, which contains the whole source
code tree.
Information and configuration files will be stored in this directory.
")

  (eide-i-help-insert-header-2 "Create a project")

  (insert "
Either launch Emacs from your project root directory, or launch Emacs and open
project popup menu to change root directory.
Then open project popup menu and select 'Create a project in this directory'.
The project is automatically added to the projects list of current workspace.

In your project root directory, several files are created:

- .emacs.desktop:
  List of open files.

- TAGS:
  Tags database.

- cscope.files:
  Cscope list of files (C/C++ files).

- .emacs-ide-project.cfg:
  It defines parameters for this project.
  It is created with default values from configuration ('Default commands for projects').
  If you delete this file, it will be created again with default values.
  If you delete any parameter in this file, it will be restored with default
  value.

- .emacs-ide-project.txt:
  It can be used to write notes about the project, it is not used by Emacs.
  It is created empty.

To edit project configuration ('.emacs-ide-project.cfg'), open project popup
menu and select 'Project configuration'.

To edit project notes ('.emacs-ide-project.txt'), open project popup menu and
select 'Project notes'.
")

  (eide-i-help-insert-header-2 "Open an existing project")

  (insert "
Either launch Emacs from your project root directory, or launch Emacs, open
project popup menu, and select 'Display projects list'.
NB: If your project is not in the list, use 'Change root directory' to open it.
It will be automatically added to the projects list of current workspace.
")

  (eide-i-help-insert-header-2 "Tags and cscope update")

  (insert "
To update tags or cscope, open project popup menu and select appropriate
update action.

When code is changed:
- Tags database ('TAGS') needs to be updated.
- Cscope database ('cscope.out') needs to be updated if 'Emacs settings >
  Search > Update of cscope database' option value is either 'Never (only on
  user request)' or 'When a buffer has been edited or refreshed' (in case a
  file has been modified outside Emacs).

When a file is added or deleted:
- Tags database ('TAGS') needs to be updated.
- Cscope list of files ('cscope.files') needs to be updated ('cscope.out' will
  be updated automatically on next search).
")

  (eide-i-help-insert-header-1 "Actions on files (right click on file name)")

  (eide-i-help-insert-header-2 "Editing with REF files")

  (insert "
When editing a file, you can create a copy, so as to easily switch between
original and modified file, and compare them.

Original version of 'file' is saved as 'file.ref'.
When switching to original file, 'file' becomes 'file.new', and 'file.ref'
becomes 'file'.

File popup menu actions:
- Backup original file (REF) to work on a copy (NEW):
  create a copy of 'file' ('file.ref'), and set read/write permission on
  'file'.
- Switch to REF file: switch to original version ('file.ref').
- Discard REF file: discard original file, and use modified file.
- Restore REF file: discard modified file, and restore original file.
- Switch to NEW file: switch to modified version ('file.new').
- Discard NEW file: discard modified file, and use original file.
- Compare REF and NEW files: compare original and modified files.

File name colour:
- green when modified file is used.
- red when original file is used.
")

  (eide-i-help-insert-header-2 "Other actions on files")

  (insert "
File popup menu actions:
- Set read/write: Set read/write permission on file.
- Set read only: Set read only permission on file.
- Untabify and indent: Clean file (turn tabs into spaces and indent).
- Delete trailing spaces
- Convert end of line: DOS to UNIX
- Convert end of line: UNIX to DOS
If show_svn_status option is set:
- svn diff
- svn revert
If show_git_status option is set:
- git diff
- git checkout

File name colour:
- black when file is read/write.
- grey when file is read only.
If show_svn_status or show_git_status option is set:
- blue when file is modified compared to repository.
")

  (eide-i-help-insert-header-1 "Actions on directories (right click on directory name)")

  (insert "
Actions on files (see above) can be applied to several files - open files
located in the same directory - at once.
An action is enabled in popup menu if it is allowed for at least one open file,
and will be applied to all files for which it is allowed.
")

  (eide-i-help-insert-header-1 "Standard key bindings")

  (insert "
Ctrl-x Ctrl-b ................. list all buffers
Ctrl-x Ctrl-f ................. load a file or open a directory (file browsing)
Ctrl-x Ctrl-s ................. save current file
Ctrl-s ........................ search
Alt-% ......................... replace
Ctrl-_ ........................ undo
Ctrl-g ........................ cancel current command
")

  (eide-i-help-insert-header-1 "New key bindings")

  (eide-i-help-insert-header-2 "Access to the menus")

  (insert"
       Alt - Enter ............ Show/hide menu and output windows
Ctrl - Alt - Enter ............ Enter/exit projects list
")

  (eide-i-help-insert-header-2 "Editing")

  (insert "
    Alt - left ......... Cut
    Alt - down ......... Copy
    Alt - right ........ Paste

Ctrl - left click ...... Cut
Ctrl - middle click .... Copy
Ctrl - right click ..... Paste
")

  (eide-i-help-insert-header-2 "Code browsing with tags/cscope")

  (insert "
NB: F1-F12 key bindings can be customized. The default bindings are shown below.

          F1 .......... Back from symbol definition
          F2 .......... Go to symbol definition (at cursor position, or selected text if any)
  Shift - F2 .......... Go to symbol definition (prompt for symbol)
  Shift - F1 .......... Go to alternative definition (after F2 or Shift - F2)
          F3 .......... Search symbol in whole project at cursor position
  Shift - F3 .......... Search symbol in whole project (prompt for symbol)
")

  (eide-i-help-insert-header-2 "Grep search")

  (insert "
          F4 .......... Search string in whole project at cursor position
  Shift - F4 .......... Search string in whole project (prompt for string)
          F6 .......... Search string in current directory at cursor position
  Shift - F6 .......... Search string in current directory (prompt for string)
")

  ;;(eide-i-help-insert-header-2 "{...} block hiding")

  ;;(insert "
  ;;Ctrl - F1 .......... Hide block
  ;;Ctrl - F2 .......... Show block
  ;;Ctrl - F3 .......... Hide all blocks in current buffer
  ;;Ctrl - F4 .......... Show all blocks in current buffer
  ;;")

  (eide-i-help-insert-header-2 "Display")

  (insert "
          F5 .......... Reload all buffers (and update display)
  Shift - F5 .......... Close current buffer
          F11 ......... Toggle fullscreen mode
")

  (eide-i-help-insert-header-2 "Grep result browsing")

  (insert "
          F7 .......... Go to previous instance
          F8 .......... Go to next instance
")

  (eide-i-help-insert-header-2 "Compilation error browsing")

  (insert "
          F7 .......... Go to previous error
          F8 .......... Go to next error
")

  (eide-i-help-insert-header-2 "Unix Shell commands")

  (insert "
          F9 .......... Compile (1)
  Shift - F9 .......... Compile (2)
          F10 ......... Run (1)
  Shift - F10 ......... Run (2)
          F12 ......... Open shell

NB: Additional compile commands (Compile (3) and Compile (4)) - as well as
debug commands - are not available from the keyboard, but only from project
popup menu.
")

  (eide-i-help-insert-header-1 "Key bindings in menus")

  (eide-i-help-insert-header-2 "Menu window")

  (insert "
       Enter .......... Open file / Go to symbol
       Space .......... Fold/unfold symbols
")

  (eide-i-help-insert-header-2 "Projects list")

  (insert "
       Enter .......... Open project
       Space .......... Select/unselect project for comparison
   Backspace .......... Remove project from workspace
")

  (eide-i-help-insert-header-1 "Windows layout overview during diff session")
  (insert "
  -----------------------------------------------------------
  |                             |                           |
  |                             |                           |
  |     'file A' window         |     'file B' window       |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  |                             |                           |
  -----------------------------------------------------------
  |                     'control' window                    |
  -----------------------------------------------------------
")
  (eide-i-help-insert-header-1 "Standard key bindings in diff session")
  (insert "
These commands must be typed in 'control' window:

! ..................... update diffs
Backspace ............. go to previous diff
Space ................. go to next diff
a ..................... copy highlighted region A --> B
b ..................... copy highlighted region A <-- B
wa .................... save file A
wb .................... save file B
q ..................... quit (y to confirm)
? ..................... display help
")
  (eide-i-help-insert-header-1 "New key bindings in diff session")
  (insert"
F1 .................... copy highlighted region A --> B
F2 .................... copy highlighted region A <-- B
F5 .................... update diffs
F7 .................... go to previous diff
F8 .................... go to next diff
Right click ........... quit (yes to confirm)

")
  (setq buffer-read-only t)
  (goto-char (point-min)))

;;; eide-help.el ends here
