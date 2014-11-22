;;; amstex.el --- AMS-LaTeX support.

;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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

;; This file is only needed when using AMS-LaTeX 1.1 and LaTeX 2.09.
;; In later versions of LaTeX and AMS-LaTeX this file is never used,
;; because there is no longer a class or package name amstex.
;;
;; As far as AUCTeX is concerned, the old amstex style is fairly
;; similar to the new amsmath package. So we will just run that hook
;; here.
;;
;; amsmath.el should not be loaded, if an AMS-TeX (in contrast to
;; AMS-LaTeX) file is opened.  The commands defined in amsmath.el
;; mostly have no meaning in this case and errors about unknown
;; variables or functions may occur due to latex.el possibly not being
;; loaded.

;;; Code:

(TeX-add-style-hook
 "amstex"
 (function
  (lambda ()
    (unless (memq major-mode '(plain-tex-mode ams-tex-mode))
      (TeX-run-style-hooks "amsmath"))))
 LaTeX-dialect)

(defvar LaTeX-amstex-package-options '("noamsfonts" "psamsfonts" 
				       "intlimits" "nointlimits"
				       "sumlimits" "nosumlimits"
				       "namelimits" "nonamelimits"
				       "leqno" "reqno" "centertags"
				       "tbtags" "fleqn" "righttag"
				       "ctagsplt" "intlim" "nosumlim"
				       "nonamelm")
    "Package options for the amstex package.")

;;; amstex.el ends here.
