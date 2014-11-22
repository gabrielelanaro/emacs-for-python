;;; dinbrief.el --- Special code for LaTeX-Style dinbrief.

;; Copyright (C) 1994, 2013, 2014  Free Software Foundation, Inc.

;; Author: Werner Fink <werner@suse.de>
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

;;; dinbrief.el - Special code for LaTeX class dinbrief.

;;; Commentary:

;; LaTeX Class: dinbrief.cls

;;; Code:

(require 'tex)

(TeX-add-style-hook "dinbrief"
 (function
  (lambda ()
    (add-hook 'LaTeX-document-style-hook
     'LaTeX-dinbrief-style)
    (LaTeX-add-environments
     '("letter" LaTeX-dinbrief-env-recipient)
     "dinquote")
    (TeX-add-symbols
     '("address" "Absender: ")
     '("postremark" "Postvermerk: ")
     '("date" "Datum: ")
     '("subject" "Betreff: ")
     '("handling" "Behandlungsvermerk: ")
     '("cc" "Verteiler: ")
     '("place" "Heutiger Ort: ")
     "makelabels"
     "nowindowrules"
     "windowrules"
     "nowindowtics"
     "windowtics"
     "disabledraftstandard"
     "enabledraftstandard"
     "centeraddress"
     "normaladdress"
     '("encl" "Anlagen: ")
     '("backaddress" "Retouradresse: ")
     '("signature" "Unterschrift: ")
     '("opening" "Anrede: ")
     '("closing" "Schluss: "))))
 LaTeX-dialect)

(defmacro LaTeX-dinbrief-insert (&rest args)
  "Insert text ignoring active markers."
  `(progn (if (TeX-mark-active) (TeX-deactivate-mark))
     (insert ,@args)))

(defun LaTeX-dinbrief-style ()
  "Insert some useful packages for writing german letters."
  (save-excursion
    (goto-char (point-min)) ; insert before \begin{document}
    (if (re-search-forward ".begin.document." (point-max) t)
        (beginning-of-line 1))
    (open-line 2)
    (indent-relative-maybe)
      (LaTeX-dinbrief-insert TeX-esc "usepackage"
	      LaTeX-optop "latin1,utf8" LaTeX-optcl
	      TeX-grop "inputenc" TeX-grcl)
      (newline-and-indent)
      (LaTeX-dinbrief-insert TeX-esc "usepackage"
	      LaTeX-optop "T1" LaTeX-optcl
	      TeX-grop "fontenc" TeX-grcl)
      (newline-and-indent)
      (LaTeX-dinbrief-insert TeX-esc "usepackage"
	      TeX-grop "ngerman" TeX-grcl)
      (TeX-run-style-hooks "inputenc")
      (TeX-run-style-hooks "fontenc")
      (TeX-run-style-hooks "ngerman")))

(defun LaTeX-dinbrief-env-recipient (environment)
  "Insert ENVIRONMENT and prompt for recipient and address."
  (let (
	(sender (LaTeX-dinbrief-sender))
	(recipient (read-string "Empfänger: "))
	(address (LaTeX-dinbrief-recipient))
	(date (read-string "Datum: " (LaTeX-dinbrief-today)))
	(postremark (read-string "Postvermerk: "))
	(fenster (read-string "Fenster \(ja/nein\): "))
	(vermerk (read-string "Behandlungsvermerk: "))
	(verteil (read-string "Verteiler: "))
	(betreff (read-string "Betreff: "))
	(opening (read-string "Anrede: "))
	(closing (read-string "Schluss: "))
	(signature (read-string "Unterschrift: "))
	(anlage (read-string "Anlagen: ")))

    (if (string= fenster "ja")
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "enabledraftstandard")
	  (newline-and-indent)
	  (LaTeX-dinbrief-insert TeX-esc "centeraddress")
	  (newline-and-indent)
	  (LaTeX-dinbrief-insert TeX-esc "nowindowrules")
	  (newline-and-indent)
	  (LaTeX-dinbrief-insert TeX-esc "windowtics")
	  (newline-and-indent)
	  (let ((retouradr (read-string "Retouradresse: " sender)))
	    (newline-and-indent)
	  (if (not (zerop (length retouradr)))
	      (progn
		(if (TeX-mark-active) (TeX-deactivate-mark))
		(LaTeX-dinbrief-insert TeX-esc "backaddress" TeX-grop retouradr TeX-grcl)
		(newline-and-indent)))))
      (LaTeX-dinbrief-insert TeX-esc "enabledraftstandard")
      (newline-and-indent)
      (LaTeX-dinbrief-insert TeX-esc "centeraddress")
      (newline-and-indent)
      (LaTeX-dinbrief-insert TeX-esc "nowindowrules")
      (newline-and-indent)
      (LaTeX-dinbrief-insert TeX-esc "windowtics"))
      (newline-and-indent)
    (if (not (zerop (length signature)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "signature" TeX-grop signature TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length date)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "date" TeX-grop date TeX-grcl)
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
      (LaTeX-dinbrief-insert "\n")
      (indent-to indentation))
    (if (not (zerop (length postremark)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "postremark" TeX-grop postremark TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length betreff)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "subject" TeX-grop)
	  (LaTeX-dinbrief-insert betreff TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length vermerk)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "handling" TeX-grop vermerk TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length verteil)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "cc" TeX-grop verteil TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length anlage)))
	(progn
	  (LaTeX-dinbrief-insert TeX-esc "encl" TeX-grop anlage TeX-grcl)
	  (newline-and-indent)))
    (LaTeX-dinbrief-insert TeX-esc "opening"
	    TeX-grop
	    (if (zerop (length opening))
		(concat TeX-esc " ")
	      opening)
	    TeX-grcl "\n")

    (indent-relative-maybe)
    (save-excursion
      (LaTeX-dinbrief-insert "\n" TeX-esc "closing"
	      TeX-grop
	      (if (zerop (length closing))
		  (concat TeX-esc " ")
		closing)
	      TeX-grcl "\n")
      (indent-relative-maybe))))

(defun LaTeX-dinbrief-sender ()
  "Read and write the senders address."
  (interactive)
  (let ((name (read-string "Absender: " (user-full-name)))
	(str  (read-string "Meine Strasse:  "))
	(ort  (read-string "Mein Wohnort:  ")))
    (if (not (zerop (length name)))
	(progn
	  (goto-char (point-min)) ; insert before \end{document}
	  (if (re-search-forward ".end.document." (point-max) t)
	     (beginning-of-line 1))
	  (previous-line 1)
	  (LaTeX-dinbrief-insert TeX-esc "address" TeX-grop name)
	  (if (not (zerop (length str)))
	      (progn
		(LaTeX-dinbrief-insert " " TeX-esc TeX-esc)
		(newline-and-indent)
		(LaTeX-dinbrief-insert str)))
	  (if (not (zerop (length ort)))
	      (progn
		(LaTeX-dinbrief-insert " " TeX-esc "par")
		(newline-and-indent)
		(LaTeX-dinbrief-insert ort)))
	  (LaTeX-dinbrief-insert TeX-grcl)
	  (newline-and-indent)
	  (concat name ", " str ", " ort)))))

(defun LaTeX-dinbrief-recipient ()
  "Read and return the recipient address."
  (interactive)
  (let ((str  (read-string "Wohnhaft in Strasse:  "))
	(ort  (read-string "Aus der Ortschaft:  ")))
    (if (not (zerop (length str)))
	(if (not (zerop (length ort)))
	    (concat str " " TeX-esc TeX-esc " " ort)
	  str)
      (if (not (zerop (length ort)))
	  ort))))

(defun LaTeX-dinbrief-today ()
  "Return a string representing todays date according to flavor."
  (interactive)
   (let ((ctime-string (current-time-string))
	(month-alist '(("Jan" . "Januar")
		       ("Feb" . "Februar")
		       ("Mar" . "M\\\"arz")
		       ("Apr" . "April")
		       ("May" . "Mai")
		       ("Jun" . "Juni")
		       ("Jul" . "Juli")
		       ("Aug" . "August")
		       ("Sep" . "September")
		       ("Oct" . "Oktober")
		       ("Nov" . "November")
		       ("Dec" . "Dezember"))))
    (string-match
     "^\\S-+\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\S-+\\s-+\\(\\S-+\\)"
     ctime-string)
    (let ((year (substring ctime-string (match-beginning 3) (match-end 3)))
	  (month (substring ctime-string (match-beginning 1) (match-end 1)))
	  (day (substring ctime-string (match-beginning 2) (match-end 2)))
	  (place (read-string "Heutiger Ort: ")))
      (if (assoc month month-alist)
	  (progn
	    (setq month (cdr (assoc month month-alist)))
	    (if (> 2 (length day))
		(setq day (concat "0" day)))))
      (format "%s, den %s. %s %s" place day month year))))

;;; dinbrief.el ends here
