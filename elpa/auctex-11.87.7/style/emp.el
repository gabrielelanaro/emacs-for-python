;;; emp.el --- AUCTeX support for emp.sty

;; Copyright (C) 2004, 2005  Free Software Foundation, Inc.

;; Author: Yvon Henel aka TeXnicien de surface <Yvon.Henel@wanadoo.fr>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

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

;; version 1.0 2004-03-04

;;; Code:


(TeX-add-style-hook "emp"
 (function
  (lambda ()
     (TeX-add-symbols "empuse" "empTeX"  "empaddtoTeX"
		      "emprelude" "empaddtoprelude" "unitlength"
 		     )
    (LaTeX-add-environments
     '("empfile" LaTeX-env-empfile)
     '("emp" LaTeX-env-emp-gen)
     '("empdef" LaTeX-env-emp-gen)
     '("empgraph" LaTeX-env-emp-gen)
     '("empcmds")
     ))))

(defun LaTeX-env-emp-gen (environment-name)
   "Ask for file, width and length. Insert environment-name environment
Used for emp, empdef, and empgraph environments."
   (let ((emp-fig-name (read-string "figure name: " ""))
	 (emp-fig-width (read-string "figure width: " "1" ))
	 (emp-fig-height (read-string "figure height: " "1" ))
	 ;;; emp.sty demands a width and a height for each of the
	 ;;; emp, empdef, and empgraph environments
	 ;;; we give them 1 by default
	 ;;; not necessarily the best thing to do?
	 )
     (if (not (zerop (length emp-fig-name)))
	 (progn 
	   (setq LaTeX-emp-fig-name (concat LaTeX-optop emp-fig-name LaTeX-optcl))
	   (LaTeX-insert-environment environment-name LaTeX-emp-fig-name))
	 (LaTeX-insert-environment environment-name))
     (forward-line -1)
     (end-of-line)
     (insert "(" emp-fig-width "," emp-fig-height ")")
     (forward-line 1)
     (indent-according-to-mode)
     ))

(defun LaTeX-env-empfile (optional)
   "Ask for file. Insert empfile environment"
   (let ((empfile (read-string "empfile: " "")))
     (if (not (zerop (length empfile)))
	 (progn 
	   (setq LaTeX-emp-file-name (concat LaTeX-optop empfile LaTeX-optcl))
	   (setq mpost-emp-file-name (concat empfile ".mp"))
	   (LaTeX-insert-environment "empfile" LaTeX-emp-file-name))
       (progn
	 (setq mpost-emp-file-name "\\jobname")
	 (LaTeX-insert-environment "empfile")))
     (if LaTeX-write18-enabled-p
	 (progn
	   (forward-line 1)
	   (end-of-line)
	   (newline-and-indent)
	   (insert "\\immediate\\write18{mpost -tex=latex " mpost-emp-file-name TeX-grcl)
	   (forward-line -2)))))
;;; emp.el ends here
