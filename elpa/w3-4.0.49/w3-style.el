;;; w3-style.el --- Emacs/W3 binding style sheet mechanism

;; Copyright (c) 1996-1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: fx $
;; Created: $Date: 2001/10/11 13:04:58 $
;; Keywords: faces, hypermedia

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

;; A style sheet mechanism for emacs-w3
;;
;; This will eventually be able to under DSSSL[-lite] as well as the
;; experimental W3C mechanism

;;; Code:

(require 'font)
(require 'w3-vars)
(eval-when-compile (require 'cl))
(require 'css)

;;;###autoload
(defun w3-handle-style (&optional plist)
  (let ((url (or (plist-get plist 'href)
		 (plist-get plist 'src)
		 (plist-get plist 'uri)))
	(media (intern (downcase (or (plist-get plist 'media) "all"))))
	(type (downcase (or (plist-get plist 'notation) "text/css")))
	(stylesheet nil)
	(cur-sheet w3-current-stylesheet)
	(string (plist-get plist 'data)))
    (if (not (memq media (css-active-device-types)))
	nil				; Not applicable to us!
      (save-excursion
	(cond
	 ((member type '("experimental" "arena" "w3c-style" "css" "text/css"))
	  (setq stylesheet (css-parse url string cur-sheet)))
	 (t
	  (w3-warn 'html "Unknown stylesheet notation: %s" type))))
      (setq w3-current-stylesheet stylesheet))))

;;;###autoload
(defun w3-display-stylesheet (&optional sheet)
  "Display the stylesheet for the current document."
  (interactive)
  (setq sheet (or sheet w3-current-stylesheet w3-user-stylesheet))
  (if (not sheet)
      (error "No stylesheet available!"))
  (css-display sheet))

(provide 'w3-style)
