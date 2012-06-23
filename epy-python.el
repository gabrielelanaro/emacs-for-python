;; epy-python.el - setup of python stuff

;; fgallina/python.el
(require 'python (concat epy-install-dir "extensions/python.el"))

;; pymacs
(require 'pymacs (concat epy-install-dir "extensions/pymacs.el"))

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (message "****************************")
  (if (and (getenv "PYTHONPATH") (not (string= (getenv "PYTHONPATH") "")))
      (message "true")
    (message "false"))
  (message "****************************")
  ;; If PYTHONPATH is set and not an empty string
  (if (and (getenv "PYTHONPATH") (not (string= (getenv "PYTHONPATH") "")))
      ;; append at the end with separator
      (setenv "PYTHONPATH"
	      (concat
	       (getenv "PYTHONPATH") path-separator
	       (concat epy-install-dir "python-libs/")))
    ;; else set it without separator
    (setenv "PYTHONPATH"
	    (concat epy-install-dir "python-libs/"))
    )
  
  (pymacs-load "ropemacs" "rope-")
  
  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)

  ;; Configurations
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)


  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"
				      "django.*"))

 

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

;; Ipython integration with fgallina/python.el
(defun epy-setup-ipython ()
  "Setup ipython integration with python-mode"
  (interactive)
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \[[0-9]+\]: "
   python-shell-prompt-output-regexp "Out\[[0-9]+\]: "
   python-shell-completion-setup-code ""
   python-shell-completion-string-code "';'.join(__IP.complete('''%s'''))\n")
  )

;;=========================================================
;; Flymake additions, I have to put this one somwhere else?
;;=========================================================

(defun flymake-create-copy-file ()
  "Create a copy local file"
  (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                     'flymake-create-temp-inplace)))
    (file-relative-name 
     temp-file 
     (file-name-directory buffer-file-name))))     

(defun flymake-command-parse (cmdline)
  "Parses the command line CMDLINE in a format compatible
       with flymake, as:(list cmd-name arg-list)

The CMDLINE should be something like:

 flymake %f python custom.py %f

%f will be substituted with a temporary copy of the file that is
 currently being checked.
"
  (let ((cmdline-subst (replace-regexp-in-string "%f" (flymake-create-copy-file) cmdline)))
    (setq cmdline-subst (split-string-and-unquote cmdline-subst))
    (list (first cmdline-subst) (rest cmdline-subst))
    ))


(when (load-file (concat epy-install-dir "extensions/flymake-patch.el"))
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (load-library "flymake-cursor"))

(defun epy-setup-checker (cmdline)
  (add-to-list 'flymake-allowed-file-name-masks
               (list "\\.py\\'" (apply-partially 'flymake-command-parse cmdline)))
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
     
     
     ;; Not on all modes, please
     (add-hook 'python-mode-hook 'flymake-find-file-hook)


     ;; when we swich on the command line, switch in Emacs
     (desktop-save-mode 1)
     (defun workon-postactivate (virtualenv)
       (require 'virtualenv)
       (virtualenv-activate virtualenv)
       (desktop-change-dir virtualenv))

     
     )
  )
;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; Py3 files
(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-hook 'python-mode-hook '(lambda () 
     (define-key python-mode-map "\C-m" 'newline-and-indent)))

(provide 'epy-python)
