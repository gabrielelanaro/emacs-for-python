;;; cursor-chg.el --- Change cursor dynamically, depending on the context.
;;
;; Filename: cursor-chg.el
;; Description: Change cursor dynamically, depending on the context.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2006-2014, Drew Adams, all rights reserved.
;; Created: Tue Aug 29 11:23:06 2006
;; Version: 20131226.1824
;; X-Original-Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Dec 26 08:39:44 2013 (-0800)
;;           By: dradams
;;     Update #: 208
;; URL: http://www.emacswiki.org/cursor-chg.el
;; Keywords: cursor, accessibility
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This library provides three kinds of changes to the text cursor:
;;
;;  1. When a buffer is read-only or is in overwrite mode, the cursor
;;     type changes to `curchg-overwrite/read-only-cursor-type'.  This
;;     is controlled by command `change-cursor-mode' and user option
;;     `curchg-change-cursor-on-overwrite/read-only-flag'.
;;
;;  2. When an input method is in use, the cursor color changes to
;;     `curchg-input-method-cursor-color'.  This is controlled by
;;     command `change-cursor-mode' and user option
;;     `curchg-change-cursor-on-input-method-flag'.
;;
;;  3. When Emacs is idle, the cursor type changes to
;;     `curchg-idle-cursor-type'.  This is controlled by command
;;     `toggle-cursor-type-when-idle'.
;;
;;  To turn on all three types of cursor change by default, put the
;;  following in your Emacs init file (~/.emacs):
;;
;;    (require 'cursor-chg)  ; Load this library
;;    (change-cursor-mode 1) ; On for overwrite/read-only/input mode
;;    (toggle-cursor-type-when-idle 1) ; On when idle
;;
;;  Note: Library `oneonone.el' provides the same functionality as
;;  library `cursor-chg.el', and more.  If you use library
;;  `oneonone.el', then do NOT also use library `cursor-chg.el'.
;;
;;  Note for Emacs 20: There is a bug in Emacs 20 which can lead to a
;;  fatal error (Emacs crash) when using `query-replace' with
;;  idle-cursor change enabled.  If you use Emacs 20, then consider
;;  using `toggle-cursor-type-when-idle' to disable idle-cursor change
;;  while you use `query-replace'.
;;
;;  User options defined here:
;;
;;    `curchg-change-cursor-on-input-method-flag',
;;    `curchg-change-cursor-on-overwrite/read-only-flag',
;;    `curchg-default-cursor-color', `curchg-default-cursor-type',
;;    `curchg-idle-cursor-type', `curchg-input-method-cursor-color',
;;    `curchg-overwrite/read-only-cursor-type'.
;;
;;  Commands defined here:
;;
;;    `change-cursor-mode', `curchg-change-cursor-when-idle-interval',
;;    `curchg-set-cursor-type', `curchg-toggle-cursor-type-when-idle',
;;    `set-cursor-type', `toggle-cursor-type-when-idle'.
;;
;;  Internal variables defined here:
;;
;;    `curchg-change-cursor-when-idle-p', `curchg-idle-interval',
;;    `curchg-idle-timer', `curchg-last-cursor-type'.
;;
;;  Non-interactive functions defined here:
;;
;;    `curchg-change-cursor-on-input-method',
;;    `curchg-change-cursor-on-overwrite/read-only',
;;    `curchg-change-cursor-to-idle-type',
;;    `curchg-change-cursor-to-idle-type-off'.
;;
;;  Acknowledgements:
;;
;;  The cursor-changing on input method and read-only was inspired by
;;  Juri Linkov <juri@jurta.org>.  Joe Casadonte <joc@netaxs.com>
;;  wrote a similar hook (`joc-cursor-type-set-hook'), which he got
;;  from Steve Kemp...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/01/03 dadams
;;     Added autoload cookies for defcustom and commands.
;; 2006/10/28 dadams
;;     curchg-default-cursor-color, curchg-input-method-cursor-color:
;;       Changed :type to 'color for Emacs 21+.
;; 2006/09/04 dadams
;;     curchg-idle-timer: Cancel beforehand, and cancel after defining.
;;     curchg-toggle-cursor-type-when-idle:
;;       Use curchg-change-cursor-to-idle-type-off on pre-command-hook.
;;       Don't read an event; just turn it on.
;;     Added: curchg-change-cursor-to-idle-type-off.
;; 2006/09/03 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Quite the byte-compiler.
(defvar change-cursor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;------- User Options -------------------------------------

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom change-cursor-mode nil
    "*Toggle changing cursor type and color.
Setting this variable directly does not take effect;
use either \\[customize] or command `change-cursor-mode'."
    :set (lambda (symbol value) (change-cursor-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'cursor :require 'cursor-chg))

;;;###autoload
(defcustom curchg-change-cursor-on-input-method-flag t
  "*Non-nil means to use a different cursor when using an input method."
  :type 'boolean :group 'cursor)

;;;###autoload
(defcustom curchg-change-cursor-on-overwrite/read-only-flag t
  "*Non-nil means use a different cursor for overwrite mode or read-only."
  :type 'boolean :group 'cursor)

;;;###autoload
(defcustom curchg-default-cursor-color (or (cdr (assq 'cursor-color default-frame-alist))
                                           "Red")
  "*Default text cursor color for non-special frames."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'cursor)

;;;###autoload
(defcustom curchg-default-cursor-type 'bar "*Default text cursor type."
  :type 'symbol :group 'cursor)

;;;###autoload
(defcustom curchg-idle-cursor-type 'box
  "*Text cursor type when Emacs is idle."
  :type 'symbol :group 'cursor)

;;;###autoload
(defcustom curchg-input-method-cursor-color "Orange"
  "*Default cursor color if using an input method.
This has no effect if `curchg-change-cursor-on-input-method-flag' is nil."
  :type (if (>= emacs-major-version 21) 'color 'string) :group 'cursor)

;;;###autoload
(defcustom curchg-overwrite/read-only-cursor-type 'box
  "*Default text cursor type for overwrite mode or read-only buffer.
This applies only to non-special frames.  This has no effect if
`curchg-change-cursor-on-overwrite/read-only-flag' is nil."
  :type 'symbol :group 'cursor)


;;------- Internal Variables -------------------------------

(defvar curchg-last-cursor-type curchg-default-cursor-type "Saved last cursor type.")

(defvar curchg-idle-interval 2
  "Number of seconds to wait before changing to alternate cursor type.
The alternate cursor type is `curchg-idle-cursor-type'.
Do NOT change this yourself to change the wait period; instead, use
`\\[curchg-change-cursor-when-idle-interval]'.")

(defvar curchg-idle-timer
  (progn                                ; Cancel to prevent duplication.
    (when (boundp 'curchg-idle-timer) (cancel-timer curchg-idle-timer))
    (run-with-idle-timer curchg-idle-interval t 'curchg-change-cursor-to-idle-type))
  "Timer used to change the cursor to alternate type when Emacs is idle.")

;; Turn it off, by default.  You must use `toggle-cursor-type-when-idle' to turn it on.
(cancel-timer curchg-idle-timer)

(defvar curchg-change-cursor-when-idle-p nil
  "Non-nil means to use an alternate cursor type whenever Emacs is idle.
Do NOT change this yourself; instead, use `\\[toggle-cursor-type-when-idle]'.")


;;------- Commands -----------------------------------------

(unless (fboundp 'set-cursor-type) (defalias 'set-cursor-type 'curchg-set-cursor-type))
;; This is essentially from Juri Linkov <juri@jurta.org>.
;;;###autoload
(defun curchg-set-cursor-type (cursor-type)
  "Set the cursor type of the selected frame to CURSOR-TYPE.
When called interactively, prompt for the type to use.
To get the frame's current cursor type, use `frame-parameters'."
  (interactive
   (list (intern (completing-read "Cursor type: "
                                  (mapcar 'list '("box" "hollow" "bar" "hbar" nil))))))
  (modify-frame-parameters (selected-frame) (list (cons 'cursor-type cursor-type))))

;;;###autoload
(defalias 'toggle-cursor-type-when-idle 'curchg-toggle-cursor-type-when-idle)
;;;###autoload
(defun curchg-toggle-cursor-type-when-idle (&optional arg)
"Turn on or off automatically changing cursor type when Emacs is idle.
When on, use `curchg-idle-cursor-type' whenever Emacs is idle.
With prefix argument, turn on if ARG > 0; else turn off."
  (interactive "P")
  (setq curchg-change-cursor-when-idle-p
        (if arg (> (prefix-numeric-value arg) 0) (not curchg-change-cursor-when-idle-p)))
  (cond (curchg-change-cursor-when-idle-p
         (timer-activate-when-idle curchg-idle-timer)
         (add-hook 'pre-command-hook 'curchg-change-cursor-to-idle-type-off)
         (message "Turned ON changing cursor when Emacs is idle."))
        (t
         (cancel-timer curchg-idle-timer)
         (remove-hook 'pre-command-hook 'curchg-change-cursor-to-idle-type-off)
         (message "Turned OFF changing cursor when Emacs is idle."))))

;;;###autoload
(defun curchg-change-cursor-when-idle-interval (secs)
  "Set wait until automatically change cursor type when Emacs is idle.
Whenever Emacs is idle for this many seconds, the cursor type will
change to `curchg-idle-cursor-type'.

To turn on or off automatically changing the cursor type when idle,
use `\\[toggle-cursor-type-when-idle]."
  (interactive
   "nSeconds to idle, before changing cursor type: ")
  (timer-set-idle-time curchg-idle-timer
                       (setq curchg-idle-interval secs)
                       t))

(if (fboundp 'define-minor-mode)
    ;; Emacs 21 and later.
    (define-minor-mode change-cursor-mode
        "Toggle changing cursor type and color.
With numeric ARG, turn cursor changing on if and only if ARG is positive.

When this mode is on, `curchg-change-cursor-on-input-method' and
`curchg-change-cursor-on-overwrite/read-only-flag' control cursor
changing."
      :init-value nil :global t :group 'frames
      :link `(url-link :tag "Send Bug Report"
              ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
cursor-chg.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
      :link '(url-link :tag "Other Libraries by Drew"
              "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
      :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/cursor-chg.el")
      :link '(url-link :tag "Description"
              "http://www.emacswiki.org/cgi-bin/wiki/ChangingCursorDynamically")
      :link '(emacs-commentary-link :tag "Commentary" "cursor-chg")
      (cond (change-cursor-mode
             (if curchg-change-cursor-on-overwrite/read-only-flag
                 (add-hook 'post-command-hook 'curchg-change-cursor-on-overwrite/read-only)
               (curchg-set-cursor-type curchg-default-cursor-type)
               (remove-hook 'post-command-hook 'curchg-change-cursor-on-overwrite/read-only))
             (if curchg-change-cursor-on-input-method-flag
                 (add-hook 'post-command-hook 'curchg-change-cursor-on-input-method)
               (setq current-input-method nil)
               (curchg-change-cursor-on-input-method)
               (remove-hook 'post-command-hook 'curchg-change-cursor-on-input-method)))
            (t
             (curchg-set-cursor-type curchg-default-cursor-type)
             (setq current-input-method nil)
             (curchg-change-cursor-on-input-method)
             (remove-hook 'post-command-hook 'curchg-change-cursor-on-overwrite/read-only)
             (remove-hook 'post-command-hook 'curchg-change-cursor-on-input-method))))

  ;; Emacs 20
  (defun change-cursor-mode (&optional arg)
    "Toggle changing cursor type and color.
With numeric ARG, turn cursor changing on if and only if ARG is positive.

When this mode is on, `curchg-change-cursor-on-input-method' and
`curchg-change-cursor-on-overwrite/read-only-flag' control cursor
changing."
    (interactive "P")
    (setq change-cursor-mode
          (if arg (> (prefix-numeric-value arg) 0) (not change-cursor-mode)))
    (cond (change-cursor-mode
           (if curchg-change-cursor-on-overwrite/read-only-flag
               (add-hook 'post-command-hook 'curchg-change-cursor-on-overwrite/read-only)
             (curchg-set-cursor-type curchg-default-cursor-type)
             (remove-hook 'post-command-hook 'curchg-change-cursor-on-overwrite/read-only))
           (if curchg-change-cursor-on-input-method-flag
               (add-hook 'post-command-hook 'curchg-change-cursor-on-input-method)
             (setq current-input-method nil)
             (curchg-change-cursor-on-input-method)
             (remove-hook 'post-command-hook 'curchg-change-cursor-on-input-method))
           (message "Change cursor on overwrite/read-only: %s; on input method: %s"
             (if curchg-change-cursor-on-overwrite/read-only-flag "ON" "OFF")
             (if curchg-change-cursor-on-input-method-flag "ON" "OFF")))
          (t
           (curchg-set-cursor-type curchg-default-cursor-type)
           (setq current-input-method nil)
           (curchg-change-cursor-on-input-method)
           (remove-hook 'post-command-hook 'curchg-change-cursor-on-overwrite/read-only)
           (remove-hook 'post-command-hook 'curchg-change-cursor-on-input-method)
           (message "Turned OFF changing cursor on overwrite/read-only and input method")))))


;;------- Non-Interactive Functions ------------------------

;; This is inspired by code from Juri Linkov <juri@jurta.org>.
(defun curchg-change-cursor-on-input-method ()
  "Set cursor type depending on whether an input method is used or not."
  (set-cursor-color (if current-input-method
                        curchg-input-method-cursor-color
                      curchg-default-cursor-color)))

;; This is from Juri Linkov <juri@jurta.org>, with read-only added.
(defun curchg-change-cursor-on-overwrite/read-only ()
  "Set cursor type differently for overwrite mode and read-only buffer.
That is, use one cursor type for overwrite mode and read-only buffers,
and another cursor type otherwise."
  (curchg-set-cursor-type (if (or buffer-read-only overwrite-mode)
                            curchg-overwrite/read-only-cursor-type
                          curchg-default-cursor-type)))

(defun curchg-change-cursor-to-idle-type ()
  "Change the cursor to `curchg-idle-cursor-type' when Emacs is idle."
  (let ((type (cdr (assoc 'cursor-type (frame-parameters)))))
    (unless (eq type curchg-idle-cursor-type)
      (setq curchg-last-cursor-type type)
      (curchg-set-cursor-type curchg-idle-cursor-type))))

(defun curchg-change-cursor-to-idle-type-off ()
  "Turn off changing the cursor to `curchg-idle-cursor-type' when idle."
  (when curchg-last-cursor-type (curchg-set-cursor-type curchg-last-cursor-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cursor-chg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cursor-chg.el ends here
