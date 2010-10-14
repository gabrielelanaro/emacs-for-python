;; epy-python.el - setup of python stuff
(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") ":"
           (concat epy-install-dir "rope-dist")))

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
