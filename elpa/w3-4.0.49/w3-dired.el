;;; w3-dired.el --- W3 Dired minor mode

;; Copyright (c) 1996-1999, 2007, 2013 Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Created: $Date: 2007/11/15 12:22:34 $
;; Keywords: faces, help, comm, news, mail, processes, mouse, hypermedia

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a minor mode for invoking Emacs/W3 from a dired buffer
;;
;; To enable for all dired buffers, put this in your .emacs file:
;;
;; (add-hook 'dired-mode-hook 'turn-on-w3-dired)

;;; Code:

;; Fixme: should we have both this and url-dired?

(autoload 'dired-get-filename "dired")
(autoload 'w3-open-local "w3")
(autoload 'w3-fetch "w3")

(defvar w3-dired-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b" 'w3-dired-find-file)
    map)
  "Keymap used when browsing directories.")

(defvar w3-dired-minor-mode nil
  "Whether we are in w3-dired-minor-mode")

(make-variable-buffer-local 'w3-dired-minor-mode)

(defun w3-dired-find-file ()
  "In dired, visit the file or directory named on this line, using Emacs-W3."
  (interactive)
  (let ((filename (dired-get-filename)))
    (cond ((string-match "/\\(.*@.*\\):\\(/.*\\)" filename)
	   (w3-fetch (concat "file://" (match-string 1 filename) (match-string 2 filename))))
	  (t
	   (w3-open-local filename)))))

(defun w3-dired-find-file-mouse (event)
  "In dired, visit the file or directory name you click on, using Emacs-W3."
  (interactive "@e")
  (mouse-set-point event)
  (w3-dired-find-file))

;;;###autoload
(defun w3-dired-minor-mode (&optional arg)
  "Minor mode for directory browsing with Emacs-W3."
  (interactive "P")
  (cond
   ((null arg)
    (setq w3-dired-minor-mode (not w3-dired-minor-mode)))
   ((equal 0 arg)
    (setq w3-dired-minor-mode nil))
   (t
    (setq w3-dired-minor-mode t))))

(add-minor-mode 'w3-dired-minor-mode " W3" w3-dired-minor-mode-map)

;;;###autoload
(defun w3-dired-find-file-dired (dir)
  "\"Edit\" directory DIR, but with additional URL-friendly bindings."
  (interactive "DURL Dired (directory): ")
  (find-file dir)
  (w3-dired-minor-mode t))

;;;###autoload
(defun turn-on-w3-dired ()
  "Unconditionally turn on W3 Dired bindings."
  (if (not (eq major-mode 'dired-mode))
      (error "w3-dired-minor-mode only makes sense in dired buffers."))
  (w3-dired-minor-mode t))

(provide 'w3-dired)
