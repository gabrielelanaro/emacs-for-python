;;; ruby.el --- AUCTeX style for the ruby package.

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2009-01-04
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

;; This file adds support for the ruby package.

;;; Code:

(defvar LaTeX-ruby-package-options
  '("overlap" "nooverlap" "CJK" "latin")
  "Package options for the ruby package.")

(TeX-add-style-hook
 "ruby"
 (lambda ()
   (TeX-add-symbols
    '("rubyoverlap" 0)
    '("rubynooverlap" 0)
    '("rubyCJK" 0)
    '("rubylatin" 0)
    '("rubysize" 0)
    '("rubysep" 0)
    '("ruby" t nil)))
 LaTeX-dialect)

;;; ruby.el ends here
