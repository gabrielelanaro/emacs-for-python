;;; auctex-lua.el --- Lua editing support for AUCTeX
;;
;; Copyright (C) 2013 Sean Allred
;;
;; Author: Sean Allred (seallred@smcm.edu)
;; Version: 20130818.1120
;; X-Original-Version: 1.0
;; Package-Requires ((auctex "11.86") (lua-mode "20130419"))
;; URL: http://github.com/vermiculus/auctex-lua
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to
;;
;;     The Free Software Foundation, Inc.
;;     51 Franklin Street, Fifth Floor
;;     Boston, MA, 02110-1301
;;     USA.
;;
;; Keywords: latex, lua
;;
;;; Commentary:
;;
;; This package provides some basic utilities for working with Lua
;; code from within AUCTeX.  `LaTeX-edit-Lua-code-start' is the entry
;; point of this package; bind it to your favorite key and use it
;; inside of any environment in `LaTeX-Lua-environments'.  To commit
;; your changes to the parent buffer and return to it, simply use
;; `save-buffer' (or whichever key it is bound to).  The contents of
;; the parent buffer will be updated and the Lua buffer will be killed.
;; 
;; Beware!  Editing embedded Lua code is asynchronous.  If you kill
;; the buffer that was editing it, your changes will be lost!  In a
;; future update I will add a `yes-or-no-p' confirmation to killing
;; the buffer, but I've yet to figure that one out.

;;; Code:

(require 'lua-mode)

(defgroup LaTeX-lua nil
  "Lua support in AUCTeX."
  :group 'LaTeX)

;;;###autoload
(defcustom LaTeX-Lua-environments '("luacode" "luacode*")
  "List of environments that will contain Lua code."
  :group 'LaTeX-lua
  :type '(repeat (string)))

(defvar LaTeX-edit-Lua-code-parent-buffer)
(defvar LaTeX-edit-Lua-code-parent-buffer-point)


(defun LaTeX-mark-environment-contents ()
  "Mark the contents of the innermost LaTeX environment.  See
also `LaTeX-find-matching-begin' and `LaTeX-find-matching-end'."
  (interactive)
  ;; Search for the end of the current environment.
  (LaTeX-find-matching-end)
  ;; Then place a mark.
  (push-mark (search-backward "\\end"))
  ;; Search for the beginning of the current environment.
  (LaTeX-find-matching-begin)
  ;; Search for the end of the \begin{...}
  (search-forward "}"))

;;;###autoload
(defun LaTeX-edit-Lua-code-start ()
  "Place Lua code in a separate buffer in `lua-mode'."
  (interactive)
  ;; If we are in a Lua environment,
  (if (member (LaTeX-current-environment) LaTeX-Lua-environments)
      ;; Make a few notes about where we are and what we're looking at
      (let* ((lua-parent-buffer (current-buffer))
             (lua-where-edit-started (point))
             (lua-buffer-name (format "*%s [Lua]*" (buffer-name)))
             (lua-buffer (get-buffer-create lua-buffer-name)) ; prepare buffer
             (lua-code (progn (LaTeX-mark-environment-contents) ; get lua code
                              (buffer-substring-no-properties (point) (mark)))))
        ;; Switch to the Lua buffer we just created.
        (switch-to-buffer-other-window lua-buffer)
        ;; Record some buffer-local variables
        (setq LaTeX-edit-Lua-code-parent-buffer lua-parent-buffer)
        (setq LaTeX-edit-Lua-code-parent-buffer-point lua-where-edit-started)
        ;; and switch this buffer to `lua-mode'
        (lua-mode)
        ;; Set key bindings.
        (local-set-key [remap save-buffer] 'LaTeX-edit-Lua-code-finish)
        ;; Fill the buffer with the Lua code.
        (insert lua-code))
    (message "Not in a Lua code environment.")))

(defun LaTeX-edit-Lua-code-finish ()
  "Dump the contents of the current buffer into the
buffer/environment it was called from, replacing what is
currently in that environment.

Note that this function isn't too smart yet; it does not
intelligently handle a scenario where other editing has taken
place in the parent buffer.  If point has moved into a different
environment, that environment will be replaced instead of its
original one.  Remember, you can always `undo' your changes.  See
`LaTeX-mark-environment-contents'."
  (interactive)
  ;; 'Ensure' this is a Lua code buffer that was made with `LaTeX-edit-Lua-code-start'
  (if (bufferp LaTeX-edit-Lua-code-parent-buffer)
      ;; Grab the Lua code
      (let* ((lua-code (progn (widen)
                              (LaTeX-edit-Lua--chomp
                               (buffer-substring (point-min)
                                                 (point-max))))))
        ;; Kill the Lua buffer
        (kill-buffer-and-window)
        ;; and switch to its parent
        (switch-to-buffer LaTeX-edit-Lua-code-parent-buffer)
        (save-excursion
          ;; Mark the current environment
          (LaTeX-mark-environment-contents)
          ;; and replace it
          (delete-region (point) (mark))
          ;; with the Lua code
          (insert lua-code))
        ;; and return point to its rightful place
        (goto-char LaTeX-edit-Lua-code-parent-buffer-point))
    (message "%s  %s"
             "Something went wrong."
             "Am I *really* in a buffer created with `LaTeX-edit-Lua-code-finish'?")))

;; Adapted from the Elisp Cookbook:
;; http://www.emacswiki.org/emacs/ElispCookbook#toc6
(defun LaTeX-edit-Lua--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\s*.*\\s*" str)
    (setq str (replace-match "" t t str)))
  str)

(provide 'auctex-lua)
;;; auctex-lua.el ends here
