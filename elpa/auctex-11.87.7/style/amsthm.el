;;; amsthm.el --- Style hook for the AMS-LaTeX amsthm package.

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@strw.leidenuniv.nl>
;; Maintainer: auctex-devel@gnu.org

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

(TeX-add-style-hook "amsthm"
 (function
  (lambda ()
    (LaTeX-add-environments
     '("proof" (lambda (env &rest ignore)
		 (LaTeX-insert-environment 
		  env
		  (let ((heading (read-string "(optional) Heading: ")))
		    (if (string= heading "")
			""
		      (format "[%s]" heading))))))
     )
    (TeX-add-symbols
     '("newtheorem" "Environment name" ["Share numbering with"] "Heading"
       ["Number subordinated in each"])
     '("newtheorem*" "Environment name" "Heading")
     '("theoremstyle" LaTeX-amsthm-complete-theoremstyle)
     ))))

(defun LaTeX-amsthm-complete-theoremstyle (&rest ignore)
  (insert TeX-grop
	  (completing-read  "Style: " '(("plain" . nil)
					("definition" . nil)
					("remark" . nil)))
	  TeX-grcl))

;;; amsthm.el ends here
