;;; varioref.el --- AUCTeX style file with support for varioref.sty

;; Copyright (C) 1999 Free Software Foundation, Inc.

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

(TeX-add-style-hook "varioref"
   (lambda ()
     
     (TeX-add-symbols

      ;; The macros with label arguments
      '("vref" TeX-arg-label)
      '("vpageref" [ "Same page text" ] [ "different page text" ] TeX-arg-label)
      '("fullref" TeX-arg-label)

      ;; And the other macros used for customization
      "reftextbefore" "reftextfacebefore"
      "reftextafter"  "reftextfaceafter"
      "reftextfaraway" "vreftextvario" "vrefwarning")

     ;; Install completion for labels
     (setq TeX-complete-list
	   (append
	    '(("\\\\vref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
	      ("\\\\vpageref\\(\\[[^]]*\\]\\)*{\\([^{}\n\r\\%,]*\\)" 
	       2 LaTeX-label-list "}"))
	    TeX-complete-list))))

(defvar LaTeX-varioref-package-options '("draft" "final" "afrikaans" 
				       "american" "austrian" "naustrian"
				       "brazil" "breton" "catalan" "croatian"
				       "czech" "danish" "dutch" "english"
				       "esperanto" "finnish" "french"
				       "galician" "german" "ngerman" "greek"
				       "italian" "magyar" "norsk" "nynorsk"
				       "polish" "portuges" "romanian"
				       "russian" "slovak" "slovene"
				       "spanish" "swedish" "turkish"
				       "francais" "germanb")
  "Package options for the varioref package.")

;;; varioref.el ends here
