;;; shortvrb.el --- AUCTeX style for `shortvrb.sty'

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2009-12-23
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

;; This file adds support for `shortvrb.sty'.

;; XXX: We might want provide users with the possibility to activate
;; something like this for any file (incl. Plain TeX).  That would
;; bring us one step closer to the goal of displaying texbook.tex
;; without font locking going haywire.

;; FIXME: The code does not work for preview.dtx because in that file
;; the style list is empty.  In its master file, preview.drv, it
;; works, however.  However, even if the style file is loaded by hand,
;; it fails to fontify verbatim text in the documentation parts of the
;; file.

;;; Code:

(defcustom LaTeX-shortvrb-chars nil
  "List of characters toggling verbatim mode.
When your document uses the shortvrb style and you have a
\\MakeShortVrb{\\|} in your file to write verbatim text as
|text|, then set this variable to the list (?|).  Then AUCTeX
fontifies |text| as verbatim.

Preferably, you should do this buffer-locally using a file
variable near the end of your document like so:

  %% Local Variables:
  %% LaTeX-shortvrb-chars: (?|)
  %% End:

When you customize this variable to a non-nil value, then it
becomes the default value meaning that verbatim fontification is
always performed for the characters in the list, no matter if
your document actually defines shortvrb chars using
\\MakeShortVrb."
  :group 'LaTeX-style
  :type '(repeat character))
(put 'LaTeX-shortvrb-chars 'safe-local-variable 'listp)

(TeX-add-style-hook
 "shortvrb"
 (lambda ()
   ;; Fontification
   (when (and LaTeX-shortvrb-chars
	      (fboundp 'font-latex-set-syntactic-keywords)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (let (syntax-alist)
       (dolist (char LaTeX-shortvrb-chars)
	 (add-to-list 'syntax-alist (cons char "|")))
       (font-latex-add-to-syntax-alist syntax-alist))))
 LaTeX-dialect)

;; Don't look for file-local variables before this line, so that the example in
;; the docstring isn't picked up.


;;; shortvrb.el ends here
