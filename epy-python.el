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
     
     ;; Instructions to add a new checker based on command:
     ;;
     ;; (flymake-setup-command "command" (list "option1" "option2")))
     ;;

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


     (when (require 'flymake "flymake-patch" t)
       (setq flymake-info-line-regex
             (append flymake-info-line-regex '("unused$" "^redefinition" "used$"))))
     
     (defun flymake-setup-command (command options pattern)
       "Setup a new checker only by specifying its command, options and pattern to apply"
       (add-to-list 'flymake-allowed-file-name-masks
                    (list pattern
                          ;; Define the init function, actually a closure
                          `(lambda ()
                             ;; Make sure it's not a remote buffer or flymake would not work
                             (when (not (current-file-remotep)) 
                               (list ,command
                                     (append ,options (list (flymake-create-copy-file)))))
                             )))
       )
     
     (defun epy-setup-checker (command &optional options)
       "Setup specifically a python checker"
       (flymake-setup-command command options "\\.py\\'"))
     
     (defun flymake-setup-multiple-commands (commandlst pattern)
       "TODO Multiple checker on the same python buffers")
     ;; Not on all modes, please
     (add-hook 'python-mode-hook 'flymake-find-file-hook)
     )
  )
;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

(provide 'epy-python)
