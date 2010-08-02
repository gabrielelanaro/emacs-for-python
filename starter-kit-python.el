;; Starterkitted emacs-for-python
;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
  )

;;
;; Adjust load path to add the following paths
;; yasnippet/
;; plugins/
;; auto-complete

(add-to-list 'load-path
             (concat epy-install-dir "yasnippet"))
(add-to-list 'load-path
             (concat epy-install-dir "plugins"))
(add-to-list 'load-path
             (concat epy-install-dir "auto-complete"))
(add-to-list 'load-path
             (concat epy-install-dir "flymake"))

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

(defun setup-ropemacs-completion ()
  "Setup the ropemacs integration with the auto-complete package"
  ;; Pretty custom, I've patched ropemode and ropemacs to add this
  ;; hook.
  ;;
  ;; There is also a custom hook to find if there is a project and if
  ;; there is activate it. In this way the project is automatically opened.
  (load (concat epy-install-dir "completion/ac-ropemacs-config.el"))
  (add-hook 'rope-open-project-hook 'ac-nropemacs-setup))

(eval-after-load 'python
  '(progn
     ;; Ropemacs
     (setup-ropemacs)
     (add-hook 'auto-complete-mode-hook 'setup-ropemacs-completion)

     ;; Virtualenv Commands
     (autoload 'activate-virtualenv "virtualenv" "Activate a Virtual Environment specified by PATH")
     (autoload 'workon "virtualenv" "Activate a Virtual Environment present using virtualenvwrapper")

     ;; Flymake for python configuration
     (require 'python-flymake))
  )

;; Cython Mode
(autoload 'cython-mode "cython-mode" "Mode for editing Cython source files")

(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))
