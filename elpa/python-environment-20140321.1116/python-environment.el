;;; python-environment.el --- virtualenv API for Emacs Lisp

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Keywords: applications, tools
;; Version: 0.0.2alpha0
;; Package-Requires: ((deferred "0.3.1"))

;; This file is NOT part of GNU Emacs.

;; python-environment.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; python-environment.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with python-environment.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'deferred)

(defconst python-environment-version "0.0.2alpha0")

(defcustom python-environment-directory
  (locate-user-emacs-file ".python-environments")
  "Path to directory to store all Python virtual environments.  A string.

If you want to change the location to, say ``~/.python-environments``,
then set it like this in your Emacs setup file::

    (setq python-environment-directory \"~/.python-environments\")"
  :group 'python-environment)

(defcustom python-environment-default-root-name "default"
  "Default Python virtual environment name.  A string.

This is a name of directory relative to `python-environment-directory'
where default virtual environment locates.
Thus, typically the default virtual environment path is
``~/.emacs.d/.python-environments/default``."
  :group 'python-environment)

(defcustom python-environment-virtualenv
  (list "virtualenv" "--system-site-packages" "--quiet")
  ;; --quiet is required for Windows.  Without it, virtualenv raises
  ;; UnicodeEncodeError
  ;; See: https://github.com/tkf/emacs-jedi/issues/148#issuecomment-38290546
  "``virtualenv`` command to use, including command options.  List of strings.

For example, if you want to use specific Python executable (to
specify Python version), append ``--python`` option like this::

    (setq python-environment-virtualenv
          (append python-environment-virtualenv
                  '(\"--python\" \"PATH/TO/bin/python\")))

I added ``--system-site-packages`` as default, but this is not
mandatory.  If you don't like it, removing does not break
anything (well, theoretically).  For reason why it is default,
see discussion here:
https://github.com/tkf/emacs-python-environment/issues/3"
  :group 'python-environment)

(defvar python-environment--verbose nil)

(defun python-environment--deferred-process (msg command)
  (message "%s..." msg)
  (deferred:$
    (apply #'deferred:process command)
    (deferred:watch it
      (apply-partially
       (lambda (msg output)
         (message "%s...Done" msg)
         (when python-environment--verbose
           (message output)))
       msg))))

(defun python-environment--blocking-process (msg command)
  (message "%s (SYNC)..." msg)
  (let (exit-code output)
    (with-temp-buffer
      (setq exit-code
            (apply #'call-process (car command)
                   nil                ; INFILE (no input)
                   t                  ; BUFFER (output to this buffer)
                   nil                ; DISPLAY (no refresh is needed)
                   (cdr command)))
      (setq output (buffer-string)))
    (when (or python-environment--verbose
              (not (= exit-code 0)))
      (message output))
    (message "%s (SYNC)...Done" msg)
    (unless (= exit-code 0)
      (error "Command %S exits with error code %S." command exit-code))))


(defun python-environment-root-path (&optional root)
  (expand-file-name (or root python-environment-default-root-name)
                    python-environment-directory))

(defun python-environment--make-with-runner (proc-runner root virtualenv)
  (let ((path (convert-standard-filename
               (python-environment-root-path root)))
        (virtualenv (append (or virtualenv python-environment-virtualenv)
                            (when python-environment--verbose
                              (list "--verbose")))))
    (unless (executable-find (car virtualenv))
      (error "Program named %S does not exist." (car virtualenv)))
    (funcall proc-runner
             (format "Making virtualenv at %s" path)
             (append virtualenv (list path)))))

(defun python-environment-make (&optional root virtualenv)
  "Make virtual environment at ROOT asynchronously.

This function does not wait until ``virtualenv`` finishes.
Instead, it returns a deferred object [#]_.  So, if you want to
do some operation after the ``virtualenv`` command finishes, do
something like this::

    (deferred:$
     (python-environment-make)
     (deferred:nextc it (lambda (output) DO-SOMETHING-HERE)))

If ROOT is specified, it is used instead of
`python-environment-default-root-name'.  ROOT can be a relative
path from `python-environment-virtualenv' or an absolute path.

If VIRTUALENV (list of string) is specified, it is used instead of
`python-environment-virtualenv'.

.. [#] https://github.com/kiwanami/emacs-deferred"
  (python-environment--make-with-runner
   #'python-environment--deferred-process
   root virtualenv))

(defun python-environment-make-block (&optional root virtualenv)
  "Blocking version of `python-environment-make'.
I recommend NOT to use this function in interactive commands.
For reason, see `python-environment-run-block'"
  (python-environment--make-with-runner
   #'python-environment--blocking-process
   root virtualenv))

(defun python-environment-exists-p (&optional root)
  "Return non-`nil' if virtualenv at ROOT exists.
See `python-environment-make' for how ROOT is interpreted."
  (let ((bin (python-environment-bin "python" root)))
    (and bin (file-exists-p bin))))

(defun python-environment--existing (root &rest paths)
  (when paths
    (let ((full-path (expand-file-name (car paths)
                                       (python-environment-root-path root))))
      (if (file-exists-p full-path)
          full-path
        (apply #'python-environment--existing root (cdr paths))))))

(defun python-environment-bin (path &optional root)
  "Return full path to \"ROOT/bin/PATH\" or \"ROOT/Scripts/PATH\" if exists.
``Scripts`` is used instead of ``bin`` in typical Windows case.
In Windows, path with extension \".ext\" may be returned.
See `python-environment-make' for how ROOT is interpreted."
  (python-environment--existing root
                                (concat "bin/" path)
                                (concat "Scripts/" path)
                                (concat "Scripts/" path ".exe")))

(defun python-environment-lib (path &optional root)
  "Return full path to \"ROOT/lib/PATH\" or \"ROOT/Lib/PATH\" if exists.
``Lib`` is used instead of ``lib`` in typical Windows case.
See `python-environment-make' for how ROOT is interpreted."
  (python-environment--existing root
                                (concat "lib/" path)
                                (concat "Lib/" path)))

(defun python-environment--run-with-runner (proc-runner command root)
  (funcall proc-runner
           (format "Running: %s" (mapconcat 'identity command " "))
           (cons (python-environment-bin (car command) root)
                 (cdr command))))

(defun python-environment--run-1 (&optional command root)
  (python-environment--run-with-runner
   #'python-environment--deferred-process
   command root))

(defun python-environment--run-block-1 (command root)
  (python-environment--run-with-runner
   #'python-environment--blocking-process
   command root))

(defun python-environment-run (command &optional root virtualenv)
  "Run COMMAND installed in Python virtualenv located at ROOT
asynchronously.

Instead of waiting for COMMAND to finish, a deferred object [#]_
is returned so that you can chain operations.

See `python-environment-make' for how ROOT and VIRTUALENV are
interpreted and how to work with deferred object.

Use `python-environment-run-block' if you want to wait until
the command exit (NOT recommended in interactive command).

.. [#] https://github.com/kiwanami/emacs-deferred"
  (if (python-environment-exists-p root)
      (python-environment--run-1 command root)
    (deferred:$
      (python-environment-make root virtualenv)
      (deferred:nextc it
        (apply-partially
         (lambda (command root _)
           (python-environment--run-1 command root))
         command root)))))

(defun python-environment-run-block (command &optional root virtualenv)
  "Blocking version of `python-environment-run'.
I recommend NOT to use this function in interactive commands.
Emacs users have more important things to than waiting for some
command to finish."
  (unless (python-environment-exists-p root)
    (python-environment-make-block root virtualenv))
  (python-environment--run-block-1 command root))

(provide 'python-environment)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-environment.el ends here
