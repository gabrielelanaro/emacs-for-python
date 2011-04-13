;; epy-python.el - setup of python stuff
(require 'pymacs (concat epy-install-dir "extensions/pymacs.el"))

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           (concat epy-install-dir "python-libs/")))
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

;; Customizing
(defcustom flymake-enable-pyflakes nil
  "Enable the pyflakes syntax checking, requires pyflakes command in path"
  :type 'boolean
  :options '(t nil))
(defcustom flymake-enable-pylint nil
  "Enable the pylint syntax checking, requires pylint command in path"
  :type 'boolean
  :options '(t nil))
(defcustom flymake-enable-pep8 nil
  "Enable the pep8 syntax checking, requires pep8 command in path"
  :type 'boolean
  :options '(t nil))

;; Python or python mode?
(eval-after-load 'python
  '(progn
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     (setup-ropemacs)

     ;;==================================================
     ;; Virtualenv Commands
     ;;==================================================
     (autoload 'virtualenv-activate "virtualenv"
       "Activate a Virtual Environment specified by PATH" t)
     (autoload 'virtualenv-workon "virtualenv"
       "Activate a Virtual Environment present using virtualenvwrapper" t)
     
     
     ;;==================================================
     ;; Flymake for python configuration
     ;;===================================================
     
     ;; TODO: There is some duplication, that can be removed using macros
     ;; TODO: Implement flymake-remove-checker

     ;; Instructions to add a new checker based on command:
     ;;
     ;; 1) Write an init function, the flymake-command-setup performs some
     ;;    checks and at the end of the option list the filename to process:
     ;;
     ;;   (defun flymake-newchecker-init ()
     ;;      (flymake-command-setup "command" (list "option1" "option2")))
     ;;
     ;; 2) Use the flymake-add-checker function
     ;;
     ;;    (flymake-add-checker flymake-newchecker-init)

     (require 'tramp)
     ;; Utilities that increase legibility and reduce code duplication
     (defun current-file-remotep ()
       "Tell if the file is remote"
       (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))

     (defun flymake-create-copy-file ()
       "Create a copy local file"
       (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                          'flymake-create-temp-inplace)))
         (file-relative-name 
          temp-file 
          (file-name-directory buffer-file-name))))

     (defun flymake-command-setup (command &optional options)
       "Setup the command to be used with flymake, the command
will be called in this way: COMMAND OPTIONS FILE The FILE varible
is passed after the options."
       ;; Make sure it's not a remote buffer or flymake would not work
       (when (not (current-file-remotep)) 
         (list command
               (append options (list (flymake-create-copy-file))))))

     (when (require 'flymake "flymake-patch" t)
       (setq flymake-info-line-regex
             (append flymake-info-line-regex '("unused$" "^redefinition" "used$"))))

     ;; I'm using individual well-defined names to be able to remove them
     ;; in some way

     ;; Init functions!
     (defun flymake-pyflakes-init ()
       (flymake-command-setup "pyflakes"))

     (defun flymake-pep8-init ()
       (flymake-command-setup "pep8"))

     (defun flymake-pylint-init ()
       (flymake-command-setup "python" (list (concat epy-install-dir "scripts/pylint-mod.py"))))

     (defun flymake-disable-python-checkers ()
       "Disable all python checkers"
       (dolist (flymake-checker-init '(flymake-pyflakes-init flymake-pep8-init flymake-pylint-init))
         (remove '("\\.py\\'" flymake-checker-init) 'flymake-allowed-file-name-masks)))

     (defun flymake-add-checker (command)
       "Add the checker specified by the COMMAND list"
       (add-to-list 'flymake-allowed-file-name-masks
                    (list "\\.py\\'" command)))

     ;; Not on all modes, please
     (add-hook 'python-mode-hook 'flymake-find-file-hook)

     
     (when flymake-enable-pyflakes
       (flymake-add-checker 'flymake-pyflakes-init))

     (when flymake-enable-pylint
       (flymake-add-checker 'flymake-pylint-init))

     (when flymake-enable-pep8
       (flymake-add-checker 'flymake-pep8-init)))
  )
;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(provide 'epy-python)
