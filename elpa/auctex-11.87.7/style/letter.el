;;; letter.el - Special code for letter style.

;; Copyright (C) 1993, 2012  Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: auctex-devel@gnu.org
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
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

;; You may want to define this in tex-site.el to contain your
;; organizations address.  
(defvar LaTeX-letter-sender-address ""
  "Initial value when prompting for a sender address in the letter style.")

(TeX-add-style-hook "letter"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("letter" LaTeX-env-recipient))
    (TeX-add-symbols
     '("name" "Sender: ") 
     '("address" "Sender address: ")
     '("signature" "Signature: ")
     '("opening" "Opening: ")
     '("closing" "Closing: ")))))

(defun LaTeX-env-recipient (environment)
  "Insert ENVIRONMENT and prompt for recipient and address."
  (let ((sender (read-string "Sender: " (user-full-name)))
	(sender-address (read-string "Sender address: "
				    LaTeX-letter-sender-address))
	(recipient (read-string "Recipient: "))
	(address (read-string "Recipient address: "))
	(signature (read-string "Signature: "))
	(opening (read-string "Opening: "))
	(closing (read-string "Closing: "))
	(date (read-string "Date: " (LaTeX-today))))

    (insert TeX-esc "name" TeX-grop sender TeX-grcl)
    (newline-and-indent)
    (if (not (zerop (length sender-address)))
	(progn
	  (setq LaTeX-letter-sender-address sender-address)
	  (insert TeX-esc "address" TeX-grop sender-address TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length signature)))
	(progn
	  (insert TeX-esc "signature" TeX-grop signature TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length date)))
	(progn
	  (insert TeX-esc "renewcommand" TeX-grop TeX-esc "today" TeX-grcl
		  TeX-grop date TeX-grcl)
	  (newline-and-indent)))
    (newline-and-indent)

    (let ((indentation (current-column)))
      (LaTeX-insert-environment
       environment
       (concat TeX-grop recipient
	       (if (not (zerop (length address)))
		   (concat
		    (if (not (zerop (length recipient)))
			(concat " " TeX-esc TeX-esc " "))
		    address))
	       TeX-grcl))
      (save-excursion			; Fix indentation of address
	(if (search-backward TeX-grcl nil 'move)
	    (let ((addr-end (point-marker)))
	      (if (search-backward TeX-grop nil 'move)
		  (let ((addr-column (current-column)))
		    (while (search-forward
			    (concat TeX-esc TeX-esc)
			    (marker-position addr-end) 'move)
		      (progn
			(newline)
			(indent-to addr-column))))))))
      (insert "\n")
      (indent-to indentation))
    (insert TeX-esc "opening"
	    TeX-grop
	    (if (zerop (length opening))
		(concat TeX-esc " ")
	      opening)
	    TeX-grcl "\n")

    (indent-relative-maybe)
    (save-excursion
      (insert "\n" TeX-esc "closing"
	      TeX-grop
	      (if (zerop (length closing))
		  (concat TeX-esc " ")
		closing)
	      TeX-grcl "\n")
      (indent-relative-maybe))))

(defun LaTeX-today nil
  "Return a string representing todays date according to flavor."
  (interactive)
  (let ((ctime-string (current-time-string))
	(month-alist '(("Jan". "01")
		       ("Feb" . "02")
		       ("Mar" . "03")
		       ("Apr" . "04")
		       ("May" . "05")
		       ("Jun" . "06")
		       ("Jul" . "07")
		       ("Aug" . "08")
		       ("Sep" . "09")
		       ("Oct" . "10")
		       ("Nov" . "11")
		       ("Dec" . "12"))))
    (string-match
     "^\\S-+\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\S-+\\s-+\\(\\S-+\\)"
     ctime-string)
    (let ((year (substring ctime-string (match-beginning 3) (match-end 3)))
	  (month (substring ctime-string (match-beginning 1) (match-end 1)))
	  (day (substring ctime-string (match-beginning 2) (match-end 2))))
      (if (assoc month month-alist)
	  (progn
	    (setq month (cdr (assoc month month-alist)))
	    (if (> 2 (length day))
		(setq day (concat "0" day)))))
      (format "%s-%s-%s" year month day))))

;;; letter.el ends here
