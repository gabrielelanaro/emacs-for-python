;; Flymake configuration with python
(require 'tramp)

(when (load "flymake-patch" t)
  
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
    "Setup the command to be used with flymake, the command will be called in this way:
COMMAND OPTIONS FILE
The FILE varible is passed after the options."
    ;; Make sure it's not a remote buffer or flymake would not work
    (when (not (current-file-remotep)) 
      (list command
            (append options (list (flymake-create-copy-file))))))

  (defun flymake-pyflakes-init ()
    (flymake-command-setup "pyflakes"))

  (defun flymake-pep8-init ()
    (flymake-command-setup "pep8"))

  (defun flymake-pylint-init ()
    (flymake-command-setup "python" (list (concat epy-install-dir "scripts/pylint-mod.py"))))

  
  ;; Adding the selected one
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pylint-init))
  
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))

  ;; Not on all modes, please
  (add-hook 'python-mode-hook 'flymake-find-file-hook)

  )

(provide 'python-flymake)
