;; epy-python.el - setup of python stuff
(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") ":"
           (concat epy-install-dir "rope-dist")))

  ;; TODO: We need something like add-to-list?
  (setq pymacs-load-path
        (list
         (concat epy-install-dir "rope-dist/ropemacs/")))

  (pymacs-load "ropemacs" "rope-")
  
  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)

  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
  (add-hook 'python-mode-hook
           (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    )))
  )

;; Python or python mode?
(eval-after-load 'python
  '(progn
     ;; Ropemacs
     (setup-ropemacs)

     ;; Virtualenv Commands
     (autoload 'activate-virtualenv "virtualenv" "Activate a Virtual Environment specified by PATH")
     (autoload 'workon "virtualenv" "Activate a Virtual Environment present using virtualenvwrapper")

     ;; Flymake for python configuration
     (require 'python-flymake)

     (when flymake-enable-pyflakes
       (flymake-add-checker 'flymake-pyflakes-init))

     (when flymake-enable-pylint
       (flymake-add-checker 'flymake-pylint-init))

     (when flymake-enable-pep8
       (flymake-add-checker 'flymake-pep8-init)))
  )

;; Debugger section
;; Load all the python libraries


;; TODO: This is copied from virtualenv package, I must split  in a
;; library to do this sort of things!
(defun virtualenv-append-path (dir var)
  "Append DIR to a path-like varibale VAR, for example:
 (virtualenv-append-path /usr/bin:/bin /home/test/bin) -> /home/test/bin:/usr/bin:/bin"
  (concat (expand-file-name dir)
          path-separator
          var)
  )

 (defun virtualenv-add-to-pythonpath (dir)
  "Add the specified path element to the Emacs PATH"
  (interactive "DEnter directory to be added to PATH: ")
  (setenv "PYTHONPATH"
	  (virtualenv-append-path dir
                                  (getenv "PYTHONPATH"))))

(dolist (package
         '("columnize-0.3.2-py2.6.egg"
           "import_relative-0.1.0-py2.6.egg"
           "pydbgr-0.1.4-py2.6.egg"
           "pyficache-0.1.3-py2.6.egg"
           "tracer-0.2.3-py2.6.egg"))
  (virtualenv-add-to-pythonpath (concat epy-install-dir "pydbgr-bundle/" package)))

;; Load the emacs-dbgr package and dependencies (they are included)
(add-to-list 'load-path (concat epy-install-dir "emacs-dbgr"))
(require 'dbgr)

;; Customizing
(setq flymake-enable-pyflakes nil)
(setq flymake-enable-pylint nil)
(setq flymake-enable-pep8 nil)

;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(provide 'epy-python)
