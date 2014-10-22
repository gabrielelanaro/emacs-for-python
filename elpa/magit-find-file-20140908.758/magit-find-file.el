;;; magit-find-file.el --- completing-read over all files in Git

;; Copyright (C) 2013 Bradley Wright

;; Author: Bradley Wright <brad@intranation.com>
;; Keywords: git
;; URL: https://github.com/bradleywright/magit-find-file.el
;; Version: 20140908.758
;; X-Original-Version: 2.0.1
;; Package-Requires: ((magit "1.2.0") (dash "2.8.0"))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a Git-powered replacement for something like TextMate's
;; Cmd-t.

;; Uses a configurable completing-read to open any file in the
;; Git repository of the current buffer.

;; Usage:
;;
;;     (require 'magit-find-file) ;; if not using the ELPA package
;;     (global-set-key (kbd "C-c p") 'magit-find-file-completing-read)
;;
;; Customize `magit-completing-read-function' to change the completing
;; read engine used (so it should behave like Magit does for you).
;;
;; Customize `magit-find-file-ignore-extensions' to exclude certain
;; files from completion.  By default all files can be selected.

;;; Code:

(require 'dash)
(require 'magit)

(defgroup magit-find-file nil
  "Use Magit to completing-read over files"
  :prefix "magit-find-file-"
  :group 'tools
  :group 'magit-extensions)

(defcustom magit-find-file-ignore-extensions nil
  "List of file extensions `magit-find-file-completing-read' ignores."
  :group 'magit-find-file
  :type '(repeat string))

(defun magit-find-file-files ()
  (let ((default-directory (magit-get-top-dir)))
    (--remove (member (file-name-extension it)
                      magit-find-file-ignore-extensions)
              (magit-git-lines "ls-files" "--cached"
                               "--other" "--exclude-standard"))))

;;;###autoload
(defun magit-find-file-completing-read ()
  "Use a completing read to open a file from git ls-files."
  (interactive)
  (-if-let (default-directory (magit-get-top-dir))
      (find-file (magit-completing-read
                  (format "Find file in %s"
                          (abbreviate-file-name default-directory))
                  (magit-find-file-files)))
    (error "Not inside a Git repository")))

(provide 'magit-find-file)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; checkdoc-minor-mode: t
;; End:

;;; magit-find-file.el ends here
