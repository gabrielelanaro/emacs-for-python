;;; nose.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Jason Pellerin, Augie Fackler

;; Licensed under the same terms as Emacs.

;; Version: 0.1.1
;; Keywords: nose python testing
;; Created: 04 Apr 2009

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running nosetests on a
;; particular buffer or part of a buffer.

;;; Installation

;; In your emacs config:
;;
;; (require 'nose)
;; ; next line only for people with non-eco non-global test runners
;; ; (add-to-list 'nose-project-names "my/crazy/runner")

;; Note that if your global nose isn't called "nosetests", then you'll want to
;; redefine nose-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'. You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'nose-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;; (defvar nose-use-verbose nil) ; default is t

;; Probably also want some keybindings:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key "\C-ca" 'nosetests-all)
;;             (local-set-key "\C-cm" 'nosetests-module)
;;             (local-set-key "\C-c." 'nosetests-one)
;;             (local-set-key "\C-cpa" 'nosetests-pdb-all)
;;             (local-set-key "\C-cpm" 'nosetests-pdb-module)
;;             (local-set-key "\C-cp." 'nosetests-pdb-one)))

(require 'cl) ;; for "reduce"

(defvar nose-project-names '("eco/bin/test"))
(defvar nose-project-root-files '("setup.py" ".hg" ".git"))
(defvar nose-project-root-test 'nose-project-root)
(defvar nose-global-name "nosetests")
(defvar nose-use-verbose t)

(defun run-nose (&optional tests debug failed)
  "run nosetests"
  (let* ((nose (nose-find-test-runner))
         (where (nose-find-project-root))
         (args (concat (if debug "--pdb" "")
                       " "
                       (if failed "--failed" "")))
         (tnames (if tests tests "")))
    (if (not where)
        (error
         (format (concat "abort: nosemacs couldn't find a project root, "
                         "looked for any of %S") nose-project-root-files)))
    (funcall (if debug
                 'pdb
               '(lambda (command)
                  (compilation-start command
                                     nil
                                     (lambda (mode) (concat "*nosetests*")))))
             (format
              (concat "%s "
                      (if nose-use-verbose "-v " "")
                      "%s -w %s -c %ssetup.cfg %s")
              (nose-find-test-runner) args where where tnames)))
  )

(defun nosetests-all (&optional debug failed)
  "run all tests"
  (interactive)
  (run-nose nil debug failed))

(defun nosetests-failed (&optional debug)
  (interactive)
  (nosetests-all debug t))

(defun nosetests-pdb-all ()
  (interactive)
  (nosetests-all t))

(defun nosetests-module (&optional debug)
  "run nosetests (via eggs/bin/test) on current buffer"
  (interactive)
  (run-nose buffer-file-name debug))

(defun nosetests-pdb-module ()
  (interactive)
  (nosetests-module t))

(defun nosetests-one (&optional debug)
  "run nosetests (via eggs/bin/test) on testable thing
 at point in current buffer"
  (interactive)
  (run-nose (format "%s:%s" buffer-file-name (nose-py-testable)) debug))

(defun nosetests-pdb-one ()
  (interactive)
  (nosetests-one t))

(defun nose-find-test-runner ()
  (message
   (let ((result
          (reduce '(lambda (x y) (or x y))
        (mapcar 'nose-find-test-runner-names nose-project-names))))
     (if result
         result
       nose-global-name))))

(defun nose-find-test-runner-names (runner)
  "find eggs/bin/test in a parent dir of current buffer's file"
  (nose-find-test-runner-in-dir-named
   (file-name-directory buffer-file-name) runner))

(defun nose-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
      ((equal dn "/") nil)
      (t (nose-find-test-runner-in-dir-named
          (file-name-directory (directory-file-name dn))
          runner)))))

(defun nose-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^ \\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun nose-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((funcall nose-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
        (t (nose-find-project-root
             (file-name-directory (directory-file-name dn)))))))

(defun nose-project-root (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  nose-project-root-files)))

(provide 'nose)

;;; nose.el ends here
