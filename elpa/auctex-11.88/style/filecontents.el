;;; filecontents.el --- AUCTeX style for `filecontents.sty'

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Mads Jensen <mje@inducks.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2013-01-01
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

;; This file adds support for `filecontents.sty'.  The package removes the two
;; constraints that `filecontents' from LaTeX2e has, namely that the environment
;; must be before the documentclass (or \begin{document} according to some
;; sources), and that existing files are not overwritten.

;;; Code:

(TeX-add-style-hook
 "filecontents"
 (lambda ()
   (LaTeX-add-environments
    '("filecontents" LaTeX-env-filecontents)
    '("filecontents*" LaTeX-env-filecontents)))
 LaTeX-dialect)

(defun LaTeX-env-filecontents (environment)
  "Insert ENVIRONMENT with filename for contents."
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "File: ")
				    TeX-grcl))
  (delete-horizontal-space))

(defvar LaTeX-filecontents-package-options nil
  "Package options for filecontents.")

;; filecontents.el ends here
