;;; biblatex.el --- AUCTeX style for `biblatex.sty'

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2012-11-14
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

;; This file adds support for `biblatex.sty'.

;;; Code:

(TeX-add-style-hook
 "biblatex"
 (lambda ()
   ;; Unfortunately `(member "backend=biber" TeX-active-styles)' does
   ;; not work as a test because "backend=biber" is added to
   ;; `TeX-active-styles' after "biblatex".  So we check the value of
   ;; `LaTeX-biblatex-use-biber' and let the user set it if desired.
   (when LaTeX-biblatex-use-Biber
     (setq LaTeX-using-Biber t))))

;; TODO: Add package options.
(defvar LaTeX-biblatex-package-options nil
  "Package options for the biblatex package.")

;;; biblatex.el ends here
