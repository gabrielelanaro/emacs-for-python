;;; dinbrief.el - Special code for LaTeX-Style dinbrief.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Contributed by Werner Fink <tex@itap.physik.uni-stuttgart.de>
;; Please direct comments to him.

;;; Commentary:

;; LaTeX-Style: dinbrief.sty
;;      Server: rusinfo.rus.uni-stuttgart.de
;;   Directory: /pub/soft/tex/macros/latex/contrib/letters

;;; Code:

(TeX-add-style-hook "dinbrief"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("letter" LaTeX-recipient-hook))
    (TeX-add-symbols
     '("Absender" "Absender: ")
     '("Postvermerk" "Postvermerk: ")
     '("Datum" "Datum: ")
     '("Betreff" "Betreff: ")
     '("Behandlungsvermerk" "Behandlungsvermerk: ")
     '("Verteiler" "Verteiler: ")
     "makelabel" "Retourlabel"
     '("Anlagen" "Anlagen: ")
     '("Fenster" "Fenster \(ja/nein\): ")
     '("Retouradresse" "Retouradresse: ")
     '("signature" "Unterschrift: ")
     '("opening" "Anrede: ")
     '("closing" "Schlu\"s: ")))))

(defun LaTeX-recipient-hook (environment)
  "Insert ENVIRONMENT and prompt for recipient and address."
  (let ((sender (read-string "Absender: " (user-full-name)))
	(recipient (read-string "Empf\"anger: "))
	(address (read-string "Anschrift: "))
	(postvermerk (read-string "Postvermerk: "))
	(date (read-string "Datum: " (LaTeX-today)))
	(betreff (read-string "Betreff: "))
	(vermerk (read-string "Behandlungsvermerk: "))
	(verteil (read-string "Verteiler: "))
	(anlage (read-string "Anlagen: "))
	(opening (read-string "Anrede: "))
	(closing (read-string "Schlu\"s: "))
	(fenster (read-string "Fenster \(ja/nein\): "))
	(signature (read-string "Unterschrift: "))
	)

    (if (not (zerop (length sender)))
	(progn
	  (insert TeX-esc "Absender" TeX-grop sender TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length postvermerk)))
	(progn
	  (insert TeX-esc "Postvermerk" TeX-grop postvermerk TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length betreff)))
	(progn
	  (insert TeX-esc "Betreff" TeX-grop betreff TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length vermerk)))
	(progn
	  (insert TeX-esc "Behandlungsvermerk" TeX-grop vermerk TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length verteil)))
	(progn
	  (insert TeX-esc "Verteiler" TeX-grop verteil TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length anlage)))
	(progn
	  (insert TeX-esc "Anlagen" TeX-grop anlage TeX-grcl)
	  (newline-and-indent)))
    (if (string= fenster "ja")
	(progn
	  (insert TeX-esc "Fenster")
	  (let ((retouradr (read-string "Retouradresse: " (user-full-name))))
	    (newline-and-indent)
	  (if (not (zerop (length retouradr)))
	      (progn
		(insert TeX-esc "Retouradresse" TeX-grop retouradr TeX-grcl)
		(newline-and-indent))))))
    (if (not (zerop (length signature)))
	(progn
	  (insert TeX-esc "signature" TeX-grop signature TeX-grcl)
	  (newline-and-indent)))
    (if (not (zerop (length date)))
	(progn
	  (insert TeX-esc "Datum" TeX-grop date TeX-grcl)
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
	  (day (substring ctime-string (match-beginning 2) (match-end 2))))
      (if (assoc month month-alist)
	  (progn
	    (setq month (cdr (assoc month month-alist)))
	    (if (> 2 (length day))
		(setq day (concat "0" day)))))
      (format "Stuttgart, den %s. %s %s" day month year))))

;;; dinbrief.el ends here
