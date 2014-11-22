;;; alphanum.el --- AUCTeX style for `alphanum.sty'

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Frank Küster <frank@kuesterei.ch>
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

;;; Commentary:

;; This is file alphanum.el, which makes AUCTeX usable with jura.cls
;; and its style file alphanum.sty.
;;
;; Contributed by Frank Küster <frank@kuesterei.ch>. The code for
;; reftex has been written by Carsten Dominik, the maintainer of
;; reftex, but all the errors are mine.

;;; Code:


(defun TeX-arg-none (arg)
  (insert " "))

(defun reftex-get-section-level-alphanum ()
  (save-excursion			; preserve position
    (save-match-data		 ; preserve matching data (important!)
      ;; Go back to the beginning of the sectioning command
      (goto-char (match-beginning 0))
      ;; Define an initial level number, depending on the current macro.
      (let* ((macro (reftex-match-string 3))	  ; "toc" or "sub"
	     (lev (cond ((string= macro "toc") 1) ; min level for "toc"
			((string= macro "sub") 2) ; min level for "sub"
			(t 0)))
	     ;; Make a regular expression which will match sectioning commands
	     ;; and the levelup macro.
	     (re (concat "\\(^[^%]*\\\\levelup\\>\\)"
			 "\\|"
			 "\\(" reftex-section-regexp "\\)")))
	;; Now parse backwards for all sectioning and levelup macros,
	;; and keep track of the relative level changes.
	(while (re-search-backward re nil t)
	  (cond
	   ((match-beginning 1)
	    ;; levelup matched, reduce level counter
	    (setq lev (1- lev)))
	   ((string= (reftex-match-string 4) "toc")
	    ;; a toc entry, nothing changes
	    )
	   ((string= (reftex-match-string 4) "sub")
	    ;; a sub entry, increase level counter
	    (setq lev (1+ lev)))))
	;; return the level
	lev))))

(TeX-add-style-hook
 "alphanum"
 (lambda ()
   (LaTeX-largest-level-set "chapter")
   (TeX-add-symbols '("levelup" TeX-arg-none))
   (make-local-variable 'LaTeX-section-list)
   (LaTeX-section-list-add-locally
    '(("part" 0)
      ;; the levels don't make sense with alphanum, I randomly chose 0...
      ("toc" 0)
      ("sub" 0)) t)
   (setq LaTeX-section-label
	 '(("part" . "part:")
	   ("toc" . "sec:")
	   ("sub" . "sec:")))
   ;;
   ;; ****************** reftex part ******************
   ;; this won't work in multifile documents, but at least there is
   ;; something.

   (if (fboundp 'reftex-add-section-levels)
       (reftex-add-section-levels
	'(("toc" .  reftex-get-section-level-alphanum)
	  ("sub" .  reftex-get-section-level-alphanum)))))
 LaTeX-dialect)

;; Local Variables:
;; coding: iso-8859-1
;; End:
