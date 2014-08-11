;;; eide-windows.el --- Emacs-IDE, windows

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

(provide 'eide-windows)

(require 'eide-config)
(require 'eide-menu)

(defvar eide-windows-source-window nil)
(defvar eide-windows-menu-window nil)
(defvar eide-windows-output-window nil)
(defvar eide-windows-window-completion nil)

(defvar eide-windows-ide-windows-visible-flag nil)
(defvar eide-windows-menu-update-request-pending-flag nil)
(defvar eide-windows-menu-update-request-pending-force-rebuild-flag nil)
(defvar eide-windows-menu-update-request-pending-force-update-status-flag nil)

(defvar eide-windows-output-window-buffer nil)
(defvar eide-compilation-buffer nil)
(defvar eide-execution-buffer nil)
(defvar eide-shell-buffer nil)

(defvar eide-windows-output-window-height nil)
(defvar eide-windows-menu-window-width nil)
(defvar eide-windows-configuration nil)

(defvar eide-windows-update-output-buffer-id nil)

(defvar eide-windows-frame-fullscreen-value nil)

(defvar eide-windows-themes-edited-flag nil)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-menu-window-position 'right "Menu window position."
  :tag "Menu window position"
  :type '(choice (const left) (const right))
  :group 'eide-windows)
(defcustom eide-custom-menu-window-height 'half "Menu window height."
  :tag "Menu window height"
  :type '(choice (const half) (const full))
  :group 'eide-windows)

;; ----------------------------------------------------------------------------
;; INTERNAL FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-windows-get-window-for-buffer (p-buffer-name)
  "Get the window in which a buffer should be displayed.
Argument:
- p-buffer-name: buffer name."
  (if eide-keys-is-editor-configuration-active-flag
    (if (string-match "^\*.*" p-buffer-name)
      (if eide-windows-ide-windows-visible-flag
        (if (string-equal eide-menu-buffer-name p-buffer-name)
          eide-windows-menu-window
          eide-windows-output-window)
        ;; Layout is not built => "menu" and "output" windows don't exist
        nil)
      (with-current-buffer p-buffer-name
        (if (or (equal major-mode 'dired-mode)
                (equal major-mode 'Buffer-menu-mode))
          nil
          (if (eide-windows-is-file-special-p p-buffer-name)
            nil
            eide-windows-source-window))))
    nil))

(defun eide-i-windows-display-buffer-function (p-buffer &optional p-not-this-window p-frame)
  "Display a buffer in appropriate window (display-buffer-function).
Returns the window.
Called for: - compile, run, and shell buffers.
            - man pages.
Arguments (same as display-buffer function):
- p-buffer: buffer.
- p-not-this-window (optional): force to display in another window.
- p-frame (optional): frame."
  (let ((l-buffer-name) (l-window)
        (l-selected-window (selected-window))
        (l-browsing-mode-flag nil))
    (if (bufferp p-buffer)
      (setq l-buffer-name (buffer-name p-buffer))
      (setq l-buffer-name p-buffer))
    ;;(message (concat "eide-i-windows-display-buffer-function: " l-buffer-name))
    (with-current-buffer l-buffer-name
      (when (and (not eide-project-is-gdb-session-visible-flag)
                 (or (equal major-mode 'dired-mode)
                     (equal major-mode 'Buffer-menu-mode)))
        (setq l-browsing-mode-flag t)))
    (if l-browsing-mode-flag
      (progn
        (unless eide-menu-browsing-mode-flag
          (eide-menu-browsing-mode-start))
        (setq l-window l-selected-window)
        (set-window-buffer l-window p-buffer))
      (progn
        (if (and (not eide-windows-ide-windows-visible-flag)
                 (string-equal l-buffer-name "*Completions*"))
          (progn
            (setq l-window (get-buffer-window l-buffer-name))
            (unless l-window
              ;; When clicking on directories, completion buffer is closed,
              ;; but its window is not closed: we must use it
              (if (window-live-p eide-windows-window-completion)
                (setq l-window eide-windows-window-completion)
                (progn
                  (select-window eide-windows-source-window)
                  (split-window-vertically)
                  (setq l-window (next-window))
                  (setq eide-windows-window-completion l-window)))))
          (progn
            (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
            (unless l-window
              (setq l-window l-selected-window))))
        (when (and (equal l-window eide-windows-source-window)
                   eide-menu-browsing-mode-flag)
          (eide-menu-browsing-mode-stop))
        (set-window-buffer l-window p-buffer)
        ;; Result buffer name is updated asynchronously
        (when eide-windows-update-output-buffer-id
          (if (string-equal eide-windows-update-output-buffer-id "c")
            (setq eide-compilation-buffer l-buffer-name)
            (if (string-equal eide-windows-update-output-buffer-id "r")
              (setq eide-execution-buffer l-buffer-name)
              (when (string-equal eide-windows-update-output-buffer-id "s")
                (setq eide-shell-buffer l-buffer-name))))
          (setq eide-windows-update-output-buffer-id nil))
        (when (equal l-window eide-windows-source-window)
          ;; Update menu if necessary
          (eide-menu-update nil))
        (when (string-equal l-buffer-name "*Completions*")
          (select-window l-window)
          ;; "Output" window temporarily expands to half or 2/3 of the frame to
          ;; display completions
          (let ((l-completion-height (max (+ (count-lines (point-min) (point-max)) 2) (/ (frame-height) 2))))
            (when (> l-completion-height (/ (frame-height) 2))
              (setq l-completion-height (/ (* (frame-height) 2) 3)))
            (enlarge-window (- l-completion-height (window-height)))))
        (when (string-match "^\*Man .*" l-buffer-name)
          (eide-menu-build-files-lists))))
    ;; Restore selected window
    (select-window l-selected-window)
    ;; Return buffer window
    l-window))

(defadvice select-window (after eide-select-window-advice-after (p-window &optional p-norecord))
  "Override select-window function (advice), to know which window is the active
\"source\" window.
Arguments (same as select-window function):
- p-window: window.
- p-norecord (optional): don't add the buffer to the list of recently selected
ones."
  (unless (or (equal p-window eide-windows-source-window)
              (equal p-window eide-windows-menu-window)
              (equal p-window eide-windows-output-window)
              ;; Exclude minibuffer
              (window-minibuffer-p p-window)
              ;; Exclude any temporary buffer ("*...")
              (string-match "^\*.*" (buffer-name (window-buffer p-window))))
    (ad-deactivate 'select-window)
    (setq eide-windows-source-window p-window)
    (eide-menu-update nil)
    (ad-activate 'select-window)))

(defadvice switch-to-buffer (around eide-switch-to-buffer-advice-around (p-buffer &optional p-norecord p-force-same-window))
  "Override switch-to-buffer function (advice), to display buffer in appropriate
window.
Returns the buffer.
Arguments (same as switch-to-buffer function):
- p-buffer: buffer.
- p-norecord (optional): don't add the buffer to the list of recently selected
ones.
- p-force-same-window (optional): force to display the buffer in the selected
window."
  ;; C-x C-f saves the windows layout before displaying completion buffer,
  ;; and restores it when the file is selected - just before calling switch-to-buffer.
  ;; If the user has changed the windows layout in between, it is lost.
  ;; In particular, if the user has changed IDE windows visibility, is is lost,
  ;; and the internal state is incorrect: we must fix it!
  (if eide-windows-ide-windows-visible-flag
    (unless (get-buffer-window eide-menu-buffer-name)
      ;; IDE windows are supposed to be shown, but an old layout has been restored:
      ;; let's clear internal status.
      (setq eide-windows-ide-windows-visible-flag nil)
      (setq eide-windows-menu-window nil)
      (setq eide-windows-output-window nil))
    (let ((l-menu-window (get-buffer-window eide-menu-buffer-name)))
      (when l-menu-window
        ;; IDE windows are supposed to be hidden, but an old layout has been restored:
        ;; let's update internal status with menu buffer and possible output buffer.
        (setq eide-windows-ide-windows-visible-flag t)
        (setq eide-windows-menu-window l-menu-window)
        (dolist (l-window (window-list))
          (when (string-match "^\*.*" (buffer-name (window-buffer l-window)))
            (setq eide-windows-output-window l-window))))))

  (let ((l-buffer-name) (l-browsing-mode-flag nil) (l-window))
    (if (bufferp p-buffer)
      ;; Get buffer name from buffer
      (setq l-buffer-name (buffer-name p-buffer))
      ;; p-buffer is already a buffer name
      (setq l-buffer-name p-buffer))
    (when l-buffer-name
      ;; l-buffer-name = nil if p-buffer has been killed
      ;; I have to find out how this is possible...
      ;; It happens when opening multiple files with *
      ;;(message (concat "switch-to-buffer: " l-buffer-name))
      (when (get-buffer l-buffer-name)
        (with-current-buffer l-buffer-name
          (when (and (not eide-project-is-gdb-session-visible-flag)
                     (or (equal major-mode 'dired-mode)
                         (equal major-mode 'Buffer-menu-mode)))
            (setq l-browsing-mode-flag t))))
      (if l-browsing-mode-flag
        (progn
          (unless eide-menu-browsing-mode-flag
            (eide-menu-browsing-mode-start))
          ad-do-it
          p-buffer)
        (progn
          (if (eide-windows-is-file-special-p l-buffer-name)
            (progn
              ;; Do not display special files
              (when (string-equal l-buffer-name eide-project-notes-file)
                ;; Project notes file should not be opened with switch-to-buffer advice
                (kill-buffer l-buffer-name))
              ;; Return the current buffer
              (current-buffer))
            (progn
              (setq l-window (eide-i-windows-get-window-for-buffer l-buffer-name))
              (if l-window
                (select-window l-window)
                (setq l-window (selected-window)))
              (when (and (equal l-window eide-windows-source-window)
                         eide-menu-browsing-mode-flag)
                (eide-menu-browsing-mode-stop))
              ad-do-it
              (set-buffer l-buffer-name)
              (if eide-project-is-gdb-session-visible-flag
                (eide-menu-update nil)
                (progn
                  (when eide-search-find-symbol-definition-flag
                    (recenter)
                    (setq eide-search-find-symbol-definition-flag nil))
                  (when (equal l-window eide-windows-source-window)
                    ;; Update menu if necessary
                    (eide-menu-update nil))))
              ;; Select buffer window
              (select-window l-window)
              ;; Return the buffer that it switched to
              p-buffer)))))))

(defun eide-windows-find-file ()
  "Override C-x C-f find-file, to get default directory from buffer in \"source\"
window."
  (interactive)
  (when eide-keys-is-editor-configuration-active-flag
    (eide-windows-select-source-window nil)
    (call-interactively 'find-file)))

(defun eide-windows-save-buffer ()
  "Override C-x C-s save-buffer function, to save buffer in \"source\" window."
  (interactive)
  (let ((l-window (selected-window)))
    (eide-windows-select-source-window nil)
    (save-buffer)
    (eide-menu-update-current-buffer-modified-status)
    (when (equal eide-custom-update-cscope-database 'auto)
      ;; Current buffer has been modified and saved: we must update cscope database
      (setq eide-search-cscope-update-database-request-pending-flag t))
    (select-window l-window)))

(defadvice revert-buffer (after eide-revert-buffer-advice-after (&optional p-ignore-auto p-noconfirm p-preserve-modes))
  "Override revert-buffer function (advice), to update cscope database.
Arguments (same as revert-buffer function):
- p-ignore-auto (optional): ignore auto-save file.
- p-noconfirm (optional): don't ask for confirmation.
- p-preserve-modes (optional): preserve file modes."
  (when (equal eide-custom-update-cscope-database 'auto)
    ;; Current buffer has been updated: we must update cscope database
    (setq eide-search-cscope-update-database-request-pending-flag t)))

(defadvice previous-buffer (around eide-previous-buffer-advice-around)
  "Override previous-buffer function (advice), to select appropriate buffer
according to selected window."
  (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
    ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
    ;; in "source" window, until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while l-do-it-flag
      ad-do-it
      (when (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
                (string-equal (buffer-name) l-starting-from-buffer-name))
        (setq l-do-it-flag nil)))
    (ad-activate 'switch-to-buffer))
  (eide-menu-update nil))

(defadvice next-buffer (around eide-next-buffer-advice-around)
  "Override next-buffer function (advice), to select appropriate buffer according
to selected window."
  (let ((l-window (selected-window)) (l-starting-from-buffer-name (buffer-name)) (l-do-it-flag t))
    ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
    ;; in "source" window, until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while l-do-it-flag
      ad-do-it
      (when (or (equal l-window (eide-i-windows-get-window-for-buffer (buffer-name)))
                (string-equal (buffer-name) l-starting-from-buffer-name))
        (setq l-do-it-flag nil)))
    (ad-activate 'switch-to-buffer))
  (eide-menu-update nil))

(defadvice gdb-setup-windows (before eide-gdb-setup-windows-advice-before)
  "Override gdb-setup-windows function (advice), to unbuild windows layout before
gdb builds its own."
  (eide-project-debug-mode-start))

(defadvice gdb-restore-windows (before eide-gdb-setup-windows-advice-before)
  "Override gdb-restore-windows function (advice), to unbuild windows layout
before gdb builds its own."
  (eide-project-debug-mode-start))

(defun eide-i-windows-window-setup-hook ()
  "Hook to be called once the frame has been resized."
  (eide-config-apply)

  ;; In case Emacs is launched with several files in arguments:
  ;; 1) Close "*Buffer List*"
  (let ((l-buffer (get-buffer "*Buffer List*")))
    (when l-buffer
      (kill-buffer l-buffer)))
  ;; 2) Keep only one source window
  (delete-other-windows)

  (setq eide-windows-source-window (selected-window))
  (setq eide-windows-output-window-height (/ (frame-height) 5))
  (setq eide-windows-menu-window-width (/ (frame-width) 3))
  (when window-system
    (eide-windows-show-ide-windows))
  (ad-activate 'select-window)
  (ad-activate 'switch-to-buffer)
  (ad-activate 'revert-buffer)
  (ad-activate 'previous-buffer)
  (ad-activate 'next-buffer)
  (ad-activate 'gdb-setup-windows)
  (ad-activate 'gdb-restore-windows)
  (setq display-buffer-function 'eide-i-windows-display-buffer-function)
  (eide-windows-skip-unwanted-buffers-in-source-window)
  ;; Create menu content (force to build and to retrieve files status)
  (eide-menu-update t t)

  (when eide-custom-start-maximized
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun eide-i-windows-select-window-at-mouse-position ()
  "Select window at mouse position."
  ;; Select the window where the mouse is
  (let ((l-position (last (mouse-position))))
    (select-window (window-at (car l-position) (cdr l-position)))))

(defun eide-i-windows-is-source-window-selected-p ()
  "Test if selected window is \"source\" window."
  (equal (selected-window) eide-windows-source-window))

(defun eide-i-windows-is-menu-window-selected-p ()
  "Test if selected window is \"menu\" window."
  (equal (selected-window) eide-windows-menu-window))

(defun eide-i-windows-is-output-window-selected-p ()
  "Test if selected window is \"output\" window."
  (equal (selected-window) eide-windows-output-window))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-windows-init ()
  "Initialize windows."
  (add-hook 'window-setup-hook 'eide-i-windows-window-setup-hook))

(defun eide-windows-show-ide-windows ()
  "Show \"menu\" and \"ouput\" windows."
  (unless eide-windows-ide-windows-visible-flag
    (ad-deactivate 'select-window)
    ;; If completion buffer is displayed, let's close its current window
    ;; and display it in new output window.
    (let ((l-completion-window (get-buffer-window "*Completions*")))
      (when l-completion-window
        (delete-window l-completion-window)
        (setq eide-windows-output-window-buffer "*Completions*")))
    ;; Emacs 24 have internal and live windows.
    ;; When showing/hiding the "menu" and "output" windows, it is now possible
    ;; to keep the "source" windows layout unchanged.
    ;; Split to create 2 new windows ("menu" and "output")
    (if (equal eide-custom-menu-window-height 'full)
      ;; "Menu" window uses the whole frame height
      (if (equal eide-custom-menu-window-position 'left)
        ;; Menu on left side
        (progn
          (setq eide-windows-output-window (split-window (frame-root-window) (- eide-windows-output-window-height) 'below))
          (setq eide-windows-menu-window (split-window (frame-root-window) (- eide-windows-menu-window-width) 'left)))
        ;; Menu on right side
        (progn
          (setq eide-windows-output-window (split-window (frame-root-window) (- eide-windows-output-window-height) 'below))
          (setq eide-windows-menu-window (split-window (frame-root-window) (- eide-windows-menu-window-width) 'right))))
      ;; "Output" window uses the whole frame width
      (if (equal eide-custom-menu-window-position 'left)
        ;; Menu on left side
        (progn
          (setq eide-windows-menu-window (split-window (frame-root-window) (- eide-windows-menu-window-width) 'left))
          (setq eide-windows-output-window (split-window (frame-root-window) (- eide-windows-output-window-height) 'below)))
        ;; Menu on right side
        (progn
          (setq eide-windows-menu-window (split-window (frame-root-window) (- eide-windows-menu-window-width) 'right))
          (setq eide-windows-output-window (split-window (frame-root-window) (- eide-windows-output-window-height) 'below)))))

    ;; Temporarily disable switch-to-buffer advice
    ;; It is useless and would check IDE windows while they're being created...
    (ad-deactivate 'switch-to-buffer)

    ;; "Menu" window
    (select-window eide-windows-menu-window)
    (switch-to-buffer eide-menu-buffer-name)
    ;; Make sure the buffer is read-only (in case it has been created again)
    (setq buffer-read-only t)
    ;; This window should be used for this buffer only
    (set-window-dedicated-p eide-windows-menu-window t)
    ;; "Output" window
    (select-window eide-windows-output-window)
    (setq window-min-height 2)
    (switch-to-buffer (get-buffer-create "*results*"))
    (if eide-windows-output-window-buffer
      (switch-to-buffer eide-windows-output-window-buffer)
      (setq eide-windows-output-window-buffer "*results*"))

    ;; Enable switch-to-buffer advice again
    (ad-activate 'switch-to-buffer)

    (select-window eide-windows-source-window)
    (eide-windows-skip-unwanted-buffers-in-source-window)
    (setq eide-windows-ide-windows-visible-flag t)
    ;; Update menu if necessary
    (when eide-windows-menu-update-request-pending-flag
      (eide-menu-update nil))
    (ad-activate 'select-window)))

(defun eide-windows-hide-ide-windows ()
  "Hide \"menu\" and \"output\" windows."
  (when eide-windows-ide-windows-visible-flag
    (ad-deactivate 'select-window)
    (when (and (window-live-p eide-windows-menu-window)
               (window-live-p eide-windows-output-window)
               (window-live-p eide-windows-source-window))
      ;; Remember windows positions only if the layout is complete
      ;; Remember "menu" window width
      (eide-windows-select-menu-window)
      (setq eide-windows-menu-window-width (window-total-width))
      ;; Remember "output" window height
      (eide-windows-select-output-window)
      (setq eide-windows-output-window-height (window-height))
      ;; Remember which result buffer is displayed in "output" window
      (setq eide-windows-output-window-buffer (buffer-name)))
    ;; Close "menu" and "output" windows
    (when (window-live-p eide-windows-menu-window)
      (delete-window eide-windows-menu-window))
    (when (window-live-p eide-windows-output-window)
      (delete-window eide-windows-output-window))
    ;; Current window becomes - if not already - "source" window
    (setq eide-windows-menu-window nil)
    (setq eide-windows-output-window nil)
    (setq eide-windows-source-window (selected-window))
    (setq eide-windows-ide-windows-visible-flag nil)
    (eide-windows-skip-unwanted-buffers-in-source-window)
    (ad-activate 'select-window)))

(defun eide-windows-show-hide-ide-windows ()
  "Show/hide \"menu\" and \"ouput\" windows."
  (interactive)
  (if eide-windows-ide-windows-visible-flag
    (eide-windows-hide-ide-windows)
    (progn
      (eide-windows-show-ide-windows)
      (unless (listp last-nonmenu-event)
        ;; Called from keyboard (see yes-or-no-p): select the "menu" window
        (select-window eide-windows-menu-window)))))

(defun eide-windows-save-and-unbuild-layout ()
  "Save the windows layout and keep only one window."
  (setq eide-windows-configuration (current-window-configuration))
  (select-window eide-windows-source-window)
  (delete-other-windows))

(defun eide-windows-restore-layout ()
  "Restore the windows layout."
  (set-window-configuration eide-windows-configuration)
  (setq eide-windows-source-window (selected-window)))

(defun eide-windows-select-source-window (p-force-build-flag)
  "Select \"source\" window.
Argument:
- p-force-build-flag: t = build windows layout if not visible."
  (when (or eide-windows-ide-windows-visible-flag p-force-build-flag)
    (unless eide-windows-ide-windows-visible-flag
      (eide-windows-show-ide-windows))
    (select-window eide-windows-source-window)))

(defun eide-windows-select-menu-window ()
  "Select \"menu\" window (build windows layout if necessary)."
  (unless eide-windows-ide-windows-visible-flag
    (eide-windows-show-ide-windows))
  (select-window eide-windows-menu-window))

(defun eide-windows-select-output-window ()
  "Select \"output\" window (build windows layout if necessary)."
  (unless eide-windows-ide-windows-visible-flag
    (eide-windows-show-ide-windows))
  (select-window eide-windows-output-window))

(defun eide-windows-is-file-special-p (l-buffer-name)
  "Test if the file is special or not. A special file must not be displayed.
Special files are: tags, cscope files, and emacs-ide hidden files.
Argument:
- l-buffer-name: buffer name."
  (or (string-equal l-buffer-name "TAGS")
      (string-equal l-buffer-name "cscope.files")
      (string-equal l-buffer-name "cscope.out")
      (string-equal l-buffer-name eide-project-config-file)
      (string-equal l-buffer-name eide-project-notes-file)))

(defun eide-windows-skip-unwanted-buffers-in-source-window ()
  "Parse buffers list until an appropriate buffer is found, that can be displayed,
and display it. Current buffer is kept if correct."
  (eide-windows-select-source-window nil)
  (let ((l-should-we-continue t) (l-current-buffer-name (buffer-name)) (l-first-found-buffer-name nil) (l-iteration 0))
    ;; Temporarily disable switch-to-buffer advice: buffers must be displayed
    ;; in "source" window, until a correct one is found
    (ad-deactivate 'switch-to-buffer)
    (while (and (not (equal (eide-i-windows-get-window-for-buffer (buffer-name)) eide-windows-source-window))
                l-should-we-continue
                (< l-iteration 30))
      (progn
        (bury-buffer)
        (if (= l-iteration 0)
          (setq l-first-found-buffer-name (buffer-name))
          (when (string-equal (buffer-name) l-first-found-buffer-name)
            ;; We have parsed the whole buffer list without finding any other
            ;; buffer that fits. Moreover, current buffer cannot be found again
            ;; (because bs-cycle-xxx ignores temporary buffers), which means
            ;; that it is not valid either. Let's display "*scratch*".
            (switch-to-buffer "*scratch*")))
        (when (string-equal (buffer-name) l-current-buffer-name)
          ;; We have parsed the whole buffer list without finding any other
          ;; buffer that fits. If this buffer is valid, let's keep it
          ;; current. Otherwise, let's display "*scratch*".
          (setq l-should-we-continue nil)
          (unless (equal (eide-i-windows-get-window-for-buffer (buffer-name)) eide-windows-source-window)
            (switch-to-buffer "*scratch*")))
        (setq l-iteration (1+ l-iteration))))
    (ad-activate 'switch-to-buffer)
    ;; Update menu (switch-to-buffer advice was disabled)
    (eide-menu-update nil)))

(defun eide-windows-handle-mouse-3 ()
  "Handle mouse-3 (right click) action."
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)
  (if (eq mark-active t)
    ;; Text is selected
    (if (= (count-screen-lines (region-beginning) (region-end) t) 1)
      ;; Text is selected on a single line
      (eide-popup-open-menu-for-search)
      ;; Text is selected on several lines
      (eide-popup-open-menu-for-cleaning))
    ;; No text selected
    (progn
      ;; If windows layout is supposed to be visible, but one of
      ;; the three windows is not visible, first unbuild, to
      ;; force rebuild
      (when (and eide-windows-ide-windows-visible-flag
                 (or (not (window-live-p eide-windows-menu-window))
                     (not (window-live-p eide-windows-output-window))
                     (not (window-live-p eide-windows-source-window))))
        (eide-windows-hide-ide-windows))
      (if (eide-i-windows-is-output-window-selected-p)
        ;; "Output" window: open search results popup menu
        (eide-popup-open-menu-for-search-results)
        (if (eide-i-windows-is-menu-window-selected-p)
          ;; "Menu" window: open global popup menu
          (eval (x-popup-menu t eide-menu-keymap))
          ;; "Source" window
          (if eide-windows-ide-windows-visible-flag
            ;; Hide
            (eide-windows-hide-ide-windows)
            ;; Show
            (progn
              (when eide-menu-browsing-mode-flag
                (eide-menu-browsing-mode-stop))
              ;; Show IDE windows (if not already restored by eide-menu-browsing-mode-stop)
              (eide-windows-show-ide-windows))))))))

(defun eide-windows-handle-mouse-2 ()
  "Handle mouse-2 (middle click) action."
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)
  (if (and eide-windows-ide-windows-visible-flag (eide-i-windows-is-menu-window-selected-p))
    (eide-menu-dired-open)
    (yank)))

(defun eide-windows-handle-shift-mouse-3 ()
  "Handle shift + mouse-3 (right click) action."
  (interactive)
  ;; Select the window where the mouse is
  (eide-i-windows-select-window-at-mouse-position)
  (when (eide-i-windows-is-output-window-selected-p)
    ;; In "output" window, open popup menu to delete search results
    (eide-popup-open-menu-for-search-results-delete)))

(defun eide-windows-switch-to-editor-mode ()
  "Switch to editor mode and build the layout."
  (interactive)
  (when (string-match "^\*Customize.*" (buffer-name))
    (ad-activate 'switch-to-buffer)
    (when eide-windows-themes-edited-flag
      ;; Update color theme for specific faces (in case
      ;; the color theme for source code has changed)
      (eide-display-apply-color-theme)
      (setq eide-windows-themes-edited-flag nil)))
  (if (or (string-equal (buffer-name) "* Help *")
          (string-match "^\*Customize.*" (buffer-name))
          (string-equal (buffer-name) eide-project-projects-buffer-name))
    ;; Close "help", customization, or projects list
    ;; NB: In customization, exit button does not work...
    (kill-this-buffer)
    (if (string-equal (buffer-name) eide-project-config-file)
      ;; Display another buffer (other than ".emacs-ide-project.cfg")
      (progn
        (save-buffer)
        (eide-project-rebuild-config-file)
        ;; Some options requires some actions if the value has been changed
        (when (and eide-project-old-project-name
                   (not (string-equal eide-project-name eide-project-old-project-name)))
          ;; Project name has changed
          (eide-menu-update-project-name)
          (eide-project-update-name)
          (setq eide-project-old-project-name nil))
        (when (and eide-search-tags-exclude-enabled-flag
                   eide-project-old-tags-exclude-value
                   (not (string-equal eide-project-tags-exclude eide-project-old-tags-exclude-value)))
          ;; Tags exclude value has changed
          (if eide-search-tags-creation-in-progress-flag
            (eide-popup-message "Cannot update tags while they are being created...")
            (eide-search-create-tags))
          (setq eide-project-old-tags-exclude-value nil))
        (when (and eide-search-cscope-exclude-enabled-flag
                   (or (and eide-project-old-cscope-exclude-files-value
                            (not (string-equal eide-project-cscope-exclude-files eide-project-old-cscope-exclude-files-value)))
                       (and eide-project-old-cscope-exclude-dirs-value
                            (not (string-equal eide-project-cscope-exclude-dirs eide-project-old-cscope-exclude-dirs-value)))))
          ;; Cscope exclude files or dirs value has changed
          (if eide-search-cscope-creation-in-progress-flag
            (eide-popup-message "Cannot update cscope list of files while it is being created...")
            (eide-search-create-cscope-list-of-files))
          (setq eide-project-old-cscope-exclude-files-value nil)
          (setq eide-project-old-cscope-exclude-dirs-value nil))
        ;; This buffer must not be closed
        (switch-to-buffer eide-current-buffer))
      (when (string-equal (buffer-name) eide-project-notes-file)
        ;; Close ".emacs-ide-project.txt"
        (save-buffer)
        (kill-buffer eide-project-notes-file))))
  (eide-display-set-colors-for-files)
  (eide-keys-configure-for-editor)
  (when eide-menu-browsing-mode-flag
    (eide-menu-browsing-mode-stop))
  (eide-windows-restore-layout)
  (eide-windows-show-ide-windows))

(defun eide-windows-find-file-without-advice (p-file)
  "Load a file without using advice (when \"menu\" buffer must not be updated).
Argument:
- p-file: file."
  ;; find-file advice would change eide-current-buffer
  ;; and menu buffer would be updated with temp files
  (ad-deactivate 'switch-to-buffer)
  (find-file p-file)
  (ad-activate 'switch-to-buffer))

(defun eide-windows-toggle-frame-fullscreen-mode ()
  "Toggle frame fullscreen mode between fullboth and nil or maximized (depending
on previous state)."
  (interactive)
  (let ((l-frame-fullscreen-value (frame-parameter nil 'fullscreen)))
    (if (equal l-frame-fullscreen-value 'fullboth)
      ;; Switch back to previous state (nil or maximized)
      (set-frame-parameter nil 'fullscreen eide-windows-frame-fullscreen-value)
      (progn
        ;; Save current state (nil or maximized)
        (setq eide-windows-frame-fullscreen-value l-frame-fullscreen-value)
        ;; Switch to fullboth mode
        (set-frame-parameter nil 'fullscreen 'fullboth)))))

;; ----------------------------------------------------------------------------
;; KEYMAPS
;; ----------------------------------------------------------------------------

;; Global menu (right click over "menu" window)
(defvar eide-menu-keymap (make-sparse-keymap "Emacs-IDE"))

(define-key eide-menu-keymap [eide-menu-close-all-files]
  '(menu-item "Close all files"
              eide-menu-close-all-files
              :visible eide-menu-files-list))

(define-key-after eide-menu-keymap [sep-close-all-files] '(menu-item "--" nil :visible eide-menu-files-list))

(define-key-after eide-menu-keymap [eide-project-create]
  '(menu-item "Create a project in this directory"
              eide-project-create
              :visible (not eide-project-name)))

(define-key-after eide-menu-keymap [eide-project-compile-1]
  '(menu-item (concat "Compile (1): " (eide-project-get-full-command eide-project-compile-command-1))
              eide-project-compile-1
              :visible (and eide-project-name (not (string-equal eide-project-compile-command-1 "")))))
(define-key-after eide-menu-keymap [eide-project-compile-2]
  '(menu-item (concat "Compile (2): " (eide-project-get-full-command eide-project-compile-command-2))
              eide-project-compile-2
              :visible (and eide-project-name (not (string-equal eide-project-compile-command-2 "")))))
(define-key-after eide-menu-keymap [eide-project-compile-3]
  '(menu-item (concat "Compile (3): " (eide-project-get-full-command eide-project-compile-command-3))
              eide-project-compile-3
              :visible (and eide-project-name (not (string-equal eide-project-compile-command-3 "")))))
(define-key-after eide-menu-keymap [eide-project-compile-4]
  '(menu-item (concat "Compile (4): " (eide-project-get-full-command eide-project-compile-command-4))
              eide-project-compile-4
              :visible (and eide-project-name (not (string-equal eide-project-compile-command-4 "")))))
(define-key-after eide-menu-keymap [eide-project-run-1]
  '(menu-item (concat "Run (1): " (eide-project-get-full-command eide-project-run-command-1))
              eide-project-run-1
              :visible (and eide-project-name (not (string-equal eide-project-run-command-1 "")))))
(define-key-after eide-menu-keymap [eide-project-run-2]
  '(menu-item (concat "Run (2): " (eide-project-get-full-command eide-project-run-command-2))
              eide-project-run-2
              :visible (and eide-project-name (not (string-equal eide-project-run-command-2 "")))))
(define-key-after eide-menu-keymap [eide-project-debug-1]
  '(menu-item (concat "Debug (1): " (eide-project-get-full-gdb-command eide-project-debug-program-1))
              eide-project-debug-1
              :visible (and eide-project-name (not (string-equal eide-project-debug-program-1 "")))))
(define-key-after eide-menu-keymap [eide-project-debug-2]
  '(menu-item (concat "Debug (2): " (eide-project-get-full-gdb-command eide-project-debug-program-2))
              eide-project-debug-2
              :visible (and eide-project-name (not (string-equal eide-project-debug-program-2 "")))))

(define-key-after eide-menu-keymap [sep-project-commands] '(menu-item "--" nil))

(define-key-after eide-menu-keymap [eide-search-create-tags]
  '(menu-item "Update tags"
              eide-search-create-tags
              :visible eide-project-name))
(define-key-after eide-menu-keymap [eide-search-create-cscope-list-of-files]
  '(menu-item "Update cscope list of files"
              eide-search-create-cscope-list-of-files
              :visible (and eide-project-name eide-search-use-cscope-flag)))
(define-key-after eide-menu-keymap [eide-search-update-cscope-database]
  '(menu-item "Update cscope database"
              eide-search-update-cscope-database
              :visible (and eide-project-name eide-search-use-cscope-flag eide-custom-override-emacs-settings (or (not eide-custom-update-cscope-database) (equal eide-custom-update-cscope-database 'auto)))))
(define-key-after eide-menu-keymap [eide-search-toggle-tags-exclude-state]
  '(menu-item "Toggle activation of tags exclude filters"
              eide-search-toggle-tags-exclude-state
              :button (:toggle . eide-search-tags-exclude-enabled-flag)
              :visible (and eide-project-name
                            (not (string-equal eide-project-tags-exclude "")))))
(define-key-after eide-menu-keymap [eide-search-toggle-cscope-exclude-state]
  '(menu-item "Toggle activation of cscope exclude filters"
              eide-search-toggle-cscope-exclude-state
              :button (:toggle . eide-search-cscope-exclude-enabled-flag)
              :visible (and eide-project-name
                            (or (not (string-equal eide-project-cscope-exclude-files ""))
                                (not (string-equal eide-project-cscope-exclude-dirs ""))))))
(define-key-after eide-menu-keymap [eide-search-toggle-grep-exclude-state]
  '(menu-item "Toggle activation of grep exclude filters"
              eide-search-toggle-grep-exclude-state
              :button (:toggle . eide-search-grep-exclude-enabled-flag)
              :visible (and eide-project-name
                            (or (not (string-equal eide-project-grep-exclude-files ""))
                                (not (string-equal eide-project-grep-exclude-dirs ""))))))

(define-key-after eide-menu-keymap [sep-project-search] '(menu-item "--" nil :visible eide-project-name))

(define-key-after eide-menu-keymap [eide-project-open-config-file]
  '(menu-item "Project configuration"
              eide-project-open-config-file
              :visible eide-project-name))
(define-key-after eide-menu-keymap [eide-project-open-notes-file]
  '(menu-item "Project notes"
              eide-project-open-notes-file
              :visible eide-project-name))

(define-key-after eide-menu-keymap [sep-project-config] '(menu-item "--" nil :visible eide-project-name))

(define-key-after eide-menu-keymap [eide-project-delete]
  '(menu-item "Delete this project"
              eide-project-delete
              :visible eide-project-name))

(define-key-after eide-menu-keymap [sep-project-delete] '(menu-item "--" nil :visible eide-project-name))

(define-key-after eide-menu-keymap [eide-project-change-root]
  '(menu-item "Change root directory"
              eide-project-change-root))
(define-key-after eide-menu-keymap [eide-project-open-list]
  '(menu-item (concat "Display projects list (workspace " (number-to-string eide-project-current-workspace) ")")
              eide-project-open-list
              :enable (not (equal (nth 7 (file-attributes eide-project-projects-file)) 0))))
(define-key-after eide-menu-keymap [eide-project-remove-from-list]
  '(menu-item "Remove this project from current workspace"
              eide-project-remove-from-list
              :visible (and eide-project-name (member eide-root-directory eide-project-current-projects-list))))
(define-key-after eide-menu-keymap [eide-project-add-in-list]
  '(menu-item "Add this project in current workspace"
              eide-project-add-in-list
              :visible (and eide-project-name (not (member eide-root-directory eide-project-current-projects-list)))))

(define-key-after eide-menu-keymap [sep-project-selection] '(menu-item "--" nil))

(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-1]
  '(menu-item "Switch to workspace 1"
              eide-project-switch-to-workspace-1
              :visible (> eide-custom-number-of-workspaces 1)
              :enable (not (equal eide-project-current-workspace 1))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-2]
  '(menu-item "Switch to workspace 2"
              eide-project-switch-to-workspace-2
              :visible (>= eide-custom-number-of-workspaces 2)
              :enable (not (equal eide-project-current-workspace 2))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-3]
  '(menu-item "Switch to workspace 3"
              eide-project-switch-to-workspace-3
              :visible (>= eide-custom-number-of-workspaces 3)
              :enable (not (equal eide-project-current-workspace 3))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-4]
  '(menu-item "Switch to workspace 4"
              eide-project-switch-to-workspace-4
              :visible (>= eide-custom-number-of-workspaces 4)
              :enable (not (equal eide-project-current-workspace 4))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-5]
  '(menu-item "Switch to workspace 5"
              eide-project-switch-to-workspace-5
              :visible (>= eide-custom-number-of-workspaces 5)
              :enable (not (equal eide-project-current-workspace 5))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-6]
  '(menu-item "Switch to workspace 6"
              eide-project-switch-to-workspace-6
              :visible (>= eide-custom-number-of-workspaces 6)
              :enable (not (equal eide-project-current-workspace 6))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-7]
  '(menu-item "Switch to workspace 7"
              eide-project-switch-to-workspace-7
              :visible (>= eide-custom-number-of-workspaces 7)
              :enable (not (equal eide-project-current-workspace 7))))
(define-key-after eide-menu-keymap [eide-project-switch-to-workspace-8]
  '(menu-item "Switch to workspace 8"
              eide-project-switch-to-workspace-8
              :visible (= eide-custom-number-of-workspaces 8)
              :enable (not (equal eide-project-current-workspace 8))))

(define-key-after eide-menu-keymap [sep-workspace-selection] '(menu-item "--" nil :visible (> eide-custom-number-of-workspaces 1)))

(define-key-after eide-menu-keymap [eide-config-customize]
  '(menu-item "Customize"
              eide-config-customize))
(define-key-after eide-menu-keymap [eide-config-customize-themes]
  '(menu-item "Customize themes"
              eide-config-customize-themes))
(define-key-after eide-menu-keymap [eide-help-open]
  '(menu-item "Help"
              eide-help-open))

;; Add to menu bar
;;(define-key-after global-map [menu-bar eide-menu] (cons "Emacs-IDE" eide-menu-keymap))

;;; eide-windows.el ends here
