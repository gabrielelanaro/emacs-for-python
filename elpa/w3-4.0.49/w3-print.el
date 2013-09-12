;;; w3-print.el --- Printing support for emacs-w3

;; Copyright (c) 1996 - 1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: fx $
;; Created: $Date: 2001/06/07 17:16:57 $
;; Keywords: faces, help, printing, hypermedia

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

;;; Code:

(require 'w3-vars)

(defvar w3-postscript-print-function 'ps-print-buffer-with-faces
  "*Name of the function to use to print a buffer as PostScript.
This should take no arguments, and act on the current buffer.
Possible values include:
ps-print-buffer-with-faces   - print immediately
ps-spool-buffer-with-faces   - spool for later")

;;;###autoload
(defun w3-print-this-url (&optional url format)
  "Print out the current document"
  (interactive)
  (if (not url) (setq url (url-view-url t)))
  (let* ((completion-ignore-case t)
	 (format (or format
		     (completing-read
		      "Format: "
		      '(("HTML Source")		; The raw HTML code
			("Formatted Text") 	; Plain ASCII rendition
			("PostScript")		; Pretty PostScript
			)
		      nil t))))
    (cond
     ((equal "HTML Source" format)
      (with-current-buffer (generate-new-buffer " *w3-print*")
	(insert w3-current-source)
	(lpr-buffer)))
     ((member form '("Formatted Text" ""))
      (lpr-buffer))
     ((equal "PostScript" format)
      (funcall w3-postscript-print-function)))))

(provide 'w3-print)
