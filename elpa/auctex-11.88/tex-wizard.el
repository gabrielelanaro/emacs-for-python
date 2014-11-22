;;; tex-wizard.el --- Check the TeX configuration

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author: David Kastrup <dak@gnu.org>
;; Keywords: tex, wp, convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This checks through your TeX configuration.  Call M-x TeX-wizard RET

;;; Code:

(defun TeX-wizard nil
  (interactive)
  (switch-to-buffer "*TeX wizard*")
  (let ((wizwin (selected-window))
	(wizbuf (current-buffer)))
    (set-visited-file-name nil)
    (erase-buffer)
    (if (featurep 'tex-site)
	(insert-before-markers "AUCTeX is enabled.  Good.\n")
      (insert-before-markers "\
It appears that AUCTeX is not enabled.  AUCTeX is the main
major mode for editing TeX/LaTeX files.\n")
      (condition-case nil
	  (info-other-window "(AUCTeX)")
	(error (select-window wizwin)
	       (switch-to-buffer wizbuf)
	       (insert-before-markers "(I am unable to find AUCTeX's info file.)\n")))
      (if (prog1 (y-or-n-p "Should I try enabling AUCTeX now?")
	    (select-window wizwin)
	    (switch-to-buffer wizbuf))
	  (condition-case nil
	      (require 'tex-site)
	    (error (insert-before-markers "AUCTeX appears not to be installed.\n")))
	(insert-before-markers "AUCTeX installation imprudently skipped.\n"))
      (when (featurep 'tex-site)
	(when (prog1 (yes-or-no-p (format "Also enable AUCTeX in `%s'" user-init-file))
		(select-window wizwin)
		(switch-to-buffer wizbuf))
	  (write-region "\
;;; Enable AUCTeX
\(require 'tex-site)\n" nil user-init-file t))))
    (if (memq 'turn-on-reftex
	      (if (featurep 'tex-site)
		  (and (boundp 'LaTeX-mode-hook) LaTeX-mode-hook)
		(and (boundp 'latex-mode-hook) latex-mode-hook)))
	(insert-before-markers "RefTeX is enabled.  Good.\n")
      (insert-before-markers "\
It appears that RefTeX is not enabled.  RefTeX is a mode
that will greatly facilitate the management of labels
and bibliographics references.\n")
      (condition-case nil
	  (info-other-window "(RefTeX)")
	(error (select-window wizwin)
	       (switch-to-buffer wizbuf)
	       (insert-before-markers
		"(I am unable to find RefTeX's info file.)\n")))
      (when (prog1 (yes-or-no-p
		    (format "Enable RefTeX in `%s'" user-init-file))
	      (select-window wizwin)
	      (switch-to-buffer wizbuf))
	(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
	(add-hook 'latex-mode-hook 'turn-on-reftex)
	(condition-case nil
	    (write-region "\
;;; Enable RefTeX
\(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
\(add-hook 'latex-mode-hook 'turn-on-reftex)
" nil user-init-file t)
	  (error (insert-before-markers
		  (format "Unable to write to file `%s'\n" user-init-file))))))
    (when (and (featurep 'tex-site)
	       (boundp 'LaTeX-mode-hook)
	       (memq 'turn-on-reftex LaTeX-mode-hook))
      (if (and (boundp 'reftex-plug-into-AUCTeX)
		   reftex-plug-into-AUCTeX)
	  (insert-before-markers
	   "RefTeX appears to be configured for use with AUCTeX.\n")
	(require 'reftex)
	(insert-before-markers "\
It appears that RefTeX is not configured to cooperate with
AUCTeX.  Please configure it using the menus, save for future
sessions, then press the finish button.")
	(customize-variable-other-window 'reftex-plug-into-AUCTeX)
	(set (make-local-variable 'custom-buffer-done-function)
	     (lambda (buff) (kill-buffer buff) (exit-recursive-edit)))
	(recursive-edit)
	(select-window wizwin)
	(switch-to-buffer wizbuf))))
  (insert-before-markers "That's all!\n"))


(provide 'tex-wizard)
;;; tex-wizard.el ends here
