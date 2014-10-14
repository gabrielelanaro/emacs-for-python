;;; test-python-environment.el --- Tests for python-environment.el

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-python-environment.el is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; test-python-environment.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-python-environment.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'python-environment)

(defmacro pye-test-with-temp-env (&rest body)
  (declare (debug (&rest form))
           (indent 0))
  (let ((path (make-symbol "path")))
    `(let* ((,path (make-temp-file "pye-test-" t))
            (python-environment-directory ,path))
       (unwind-protect
           (progn ,@body)
         (delete-directory ,path t)))))

(defmacro pye-deftest (name args &rest body)
  "Customized `ert-deftest'.  Bind `python-environment-directory' to a
temporary directory while executing BODY."
  (declare (debug (&define :name test
                           name sexp [&optional stringp]
                           [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  `(ert-deftest ,name ,args
     (pye-test-with-temp-env
       ,@body)))

(defmacro pye-with-mixed-environment (environment &rest body)
  "Mix-in ENVIRONMENT to `process-environment' while executing `BODY'.
ENVIRONMENT is a list whose element is arguments (i.e., list) to `setenv'."
  (declare (debug (sexp &rest form))
           (indent 1))
  `(let ((process-environment (mapcar #'identity process-environment)))
     (mapc (lambda (env) (apply #'setenv env)) ,environment)
     ,@body))

(defun pye-eval-in-subprocess (sexp &optional environment)
  "Evaluate SEXP in Emacs launched as subprocess.  Additional environment
variable can be given as ENVIRONMENT (see `pye-with-mixed-environment')."
  (let ((default-directory (expand-file-name default-directory)))
    ;; Resolution of "~/" will be affected by `environment' if it
    ;; includes "$HOME".  So expand it before
    ;; `pye-with-mixed-environment' to avoid the confusion.
    (pye-with-mixed-environment environment
      (let ((print-length nil)
            (print-level nil))
        (with-temp-buffer
          (let ((code (call-process
                       (concat invocation-directory invocation-name)
                       nil t nil
                       "-Q" "--batch"
                       "--eval" (format "(setq load-path (cons %S '%S))"
                                        default-directory load-path)
                       "--load" (locate-library "test-python-environment")
                       "--eval" (format "%S" sexp))))
            (unless (eq code 0)
              (error "Subprocess terminated with code %S.\nOutput:\n%s"
                     code (buffer-string)))))))))

(defmacro pye-test-with-capture-message (&rest form)
  (declare (debug (&rest form))
           (indent 0))
  `(let ((start (make-marker))
         (message-buffer (get-buffer "*Messages*")))
     (with-current-buffer message-buffer
       (set-marker start (point-max)))
     (progn ,@form)
     (with-current-buffer message-buffer
       (buffer-substring start (point-max)))))

(ert-deftest pye-test-test-with-capture-message ()
  (should (equal (pye-test-with-capture-message
                   (message "test-1")
                   (message "test-2"))
                 "test-1\ntest-2\n")))

(defun pye-test-proc-runner-output-message (proc-runner desired-output)
  (let* ((command '("echo" "DUMMY-ECHO-MESSAGE"))
         (python-environment--verbose t)
         (message-output
          (pye-test-with-capture-message
            (funcall proc-runner "DUMMY-MESSAGE" command))))
    (should (equal message-output desired-output))))

(ert-deftest pye-test-deferred-process-output-message ()
  (pye-test-proc-runner-output-message
   (lambda (msg command)
     (deferred:sync! (python-environment--deferred-process msg command))) "\
DUMMY-MESSAGE...Done
DUMMY-ECHO-MESSAGE

"))

(ert-deftest pye-test-blocking-process-output-message ()
  (pye-test-proc-runner-output-message
   #'python-environment--blocking-process "\
DUMMY-MESSAGE (SYNC)...
DUMMY-ECHO-MESSAGE

DUMMY-MESSAGE (SYNC)...Done
"))

(defun pye-test-deferred-process-should-error ()
  (let (err)
    (deferred:sync!
      (deferred:error
        (python-environment--deferred-process
         "DUMMY-MESSAGE"
         '("false"))
        (lambda (got) (setq err got))))
    (should err)))

(ert-deftest pye-test-deferred-process-error-without-verbose ()
  (let ((python-environment--verbose nil))
    (pye-test-deferred-process-should-error)))

(ert-deftest pye-test-deferred-process-noerror-without-verbose ()
  (let ((python-environment--verbose nil))
    (deferred:sync!
      (python-environment--deferred-process "DUMMY-MESSAGE" '("true")))))

(ert-deftest pye-test-blocking-process-error-without-verbose ()
  (let ((python-environment--verbose nil))
    (should-error
     (python-environment--blocking-process "DUMMY-MESSAGE" '("false")))))

(ert-deftest pye-test-blocking-process-noerror-without-verbose ()
  (let ((python-environment--verbose nil))
    (python-environment--blocking-process "DUMMY-MESSAGE" '("true"))))

(ert-deftest pye-test-deferred-process-error-with-verbose ()
  (let ((python-environment--verbose t))
    (pye-test-deferred-process-should-error)))

(ert-deftest pye-test-deferred-process-noerror-with-verbose ()
  (let ((python-environment--verbose t))
    (deferred:sync!
      (python-environment--deferred-process "DUMMY-MESSAGE" '("true")))))

(ert-deftest pye-test-blocking-process-error-with-verbose ()
  (let ((python-environment--verbose t))
    (should-error
     (python-environment--blocking-process "DUMMY-MESSAGE" '("false")))))

(ert-deftest pye-test-blocking-process-noerror-with-verbose ()
  (let ((python-environment--verbose t))
    (python-environment--blocking-process "DUMMY-MESSAGE" '("true"))))

(pye-deftest pye-test-make-environment-with-non-existing-command ()
  (should-error (python-environment-make nil '("non-existing-command"))))

(pye-deftest pye-test-make-environment ()
  (deferred:sync! (python-environment-make)))

(pye-deftest pye-test-run ()
  (deferred:sync! (python-environment-run '("python" "--version"))))

(pye-deftest pye-test-run-block ()
  (python-environment-run-block '("python" "--version")))

(pye-deftest pye-test-block-error ()
  (should-error (python-environment-run-block '("python" "-c" "1/0"))))

(ert-deftest pye-test-eval-in-subprocess ()
  (pye-eval-in-subprocess '(+ 1 2))
  (should-error (pye-eval-in-subprocess '(error "some error"))))

(pye-deftest pye-test-bare-make-environment ()
  (let ((tmp-home python-environment-directory))
    (pye-eval-in-subprocess '(deferred:sync! (python-environment-make))
                            `(("HOME" ,tmp-home)))
    (should (file-directory-p (expand-file-name
                               ".emacs.d/.python-environments/default"
                               tmp-home)))))

(provide 'test-python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; test-python-environment.el ends here
