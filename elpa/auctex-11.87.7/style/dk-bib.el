;;; dk-bib.el --- AUCTeX style for `dk-bib.sty'

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Arne Jørgensen <arne@arnested.dk>
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Prompt for package option for dk-bib.sty.

;;; Code:

(defun LaTeX-dk-bib-package-options nil
  "Prompt for package options for the dk-bib package."
  (let ((options
	 (mapconcat 'identity
		    (TeX-completing-read-multiple
		     "Options: "
		     '(("isbn") ("issn") ("url") ("annote")
		       ("printing") ("apalike") ("fixcitedash=false")
		       ("ordinals2word") ("ordinaldepth=")))
		    ","))
	(depth -1))
    (when (string-match "\\(ordinaldepth=\\)\\([^0-9]\\|$\\)" options)
      (while (or (< depth 0)
		 (> depth 20))
	(setq depth (if (fboundp 'read-number)
			(read-number "Ordinal depth: ")
		      (string-to-number (read-string "Ordinal depth: "))))
	(when (or (< depth 0)
		  (> depth 20))
	  (message "Ordinal depth must be between 0 and 20")
	  (sit-for 1)))
      (setq options (concat
		     (substring options 0 (match-end 1))
		     (number-to-string depth)
		     (substring options (match-end 1)))))
    options))

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; dk-bib.el ends here
