;;; url-test.el --- URL parsing test suite

;; Copyright (c) 2013  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This test suite derived from:
;; http://www.ics.uci.edu/%7Efielding/url/test1.html

;; FIXME: This should be moved to emacs/test/automated.

;;; Code:

(defun url-parsing-test-suite ()
  (interactive)
  (let* ((base-url "http://a/b/c/d;p?q")
	 (tests `(
		  ;; Normal examples
		  ("g:h" . "g:h")
		  ("g"   . "http://a/b/c/g")
		  ("./g" . "http://a/b/c/g")
		  ("g/"  . "http://a/b/c/g/")
		  ("/g"  . "http://a/g")
		  ("//g" . "http://g")
		  ("?y"  . "http://a/b/c/?y")
		  ("g?y" . "http://a/b/c/g?y")
		  ("#s"  . "#s")
		  ("g#s" . "http://a/b/c/g#s")
		  ("g?y#s" . "http://a/b/c/g?y#s")
		  (";x"    . "http://a/b/c/;x")
		  ("g;x"   . "http://a/b/c/g;x")
		  ("g;x?y#s" . "http://a/b/c/g;x?y#s")
		  ("."       . "http://a/b/c/")
		  ("./"      . "http://a/b/c/")
		  (".."      . "http://a/b/")
		  ("../"     . "http://a/b/")
		  ("../g"    . "http://a/b/g")
		  ("../.."   . "http://a/")
		  ("../../"  . "http://a/")
		  ("../../g" . "http://a/g")

		  ;; Abnormal examples
		  ("" . ,base-url)
		  ("../../../g" . ("http://a/g" "http://a/../g"))
		  ("../../../../g" . ("http://a/g" "http://a/../../g"))
		  ("/./g" . "http://a/./g")
		  ("/../g" . "http://a/../g")
		  ("g." . "http://a/b/c/g.")
		  (".g" . "http://a/b/c/.g")
		  ("g.." . "http://a/b/c/g..")
		  ("..g" . "http://a/b/c/..g")
		  ("./../g" . "http://a/b/g")
		  ("./g/."  . "http://a/b/c/g/")
		  ("g/./h"  . "http://a/b/c/g/h")
		  ("g/../h" . "http://a/b/c/h")
		  ("g;x=1/./y" . "http://a/b/c/g;x=1/y")
		  ("g;x=1/../y" . "http://a/b/c/y")

		  ;; Query stuff
		  ("g?y/./x" . "http://a/b/c/g?y/./x")
		  ("g?y/../x" . "http://a/b/c/g?y/../x")
		  ("g#s/./x" . "http://a/b/c/g#s/./x")
		  ("g#s/../x" . "http://a/b/c/g#s/../x")

		  ;; RFC1630 lossage
		  ("http:g" . ("http:g" "http://a/b/c/g"))
		  ("http:"  . "http:")
		  )
		)
	 (results nil))
    (setq results (mapcar
		   (lambda (test)
		     (let ((url (url-expand-file-name (car test) base-url))
			   (expected (cdr test))
			   (ok nil))
		       (if (stringp expected)
			   (setq ok (string-equal url expected))
			 (while (and expected (not ok))
			   (if (string= (car expected) url)
			       (setq ok t))
			   (setq expected (cdr expected))))
		       (or ok (list test url))))
		   tests)
	  results (delq t results))
    (if (not results)
	(message "All tests passed successfully!")
      (set-buffer (get-buffer-create "Test Results"))
      (erase-buffer)
      (mapcar
       (lambda (err)
	 (insert (format "Failed to expand `%s' - expecting `%s' - got `%s'\n"
			 (car (car err))
			 (if (stringp (cdr (car err)))
			     (cdr (car err))
			   (mapconcat 'identity (cdr (car err)) "' or `"))
			 (cdr err))))
       results)
      (display-buffer (current-buffer)))))
