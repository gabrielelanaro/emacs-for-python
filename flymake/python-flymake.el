;; Flymake configuration with python
;; TODO: Refactorings!
(require 'tramp)

(when (load "flymake-patch" t) 
  (defun flymake-pyflakes-init () 
    ;; Pyflakes stuff
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                         'flymake-create-temp-inplace)) 
             (local-file (file-relative-name 
                          temp-file 
                          (file-name-directory buffer-file-name)))) 
        (list "pyflakes" (list local-file)))))


  (defun flymake-pep8-init () 
    ;; Pyflakes stuff
    ;; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                         'flymake-create-temp-inplace)) 
             (local-file (file-relative-name 
                          temp-file 
                          (file-name-directory buffer-file-name)))) 
        (list "pep8" (list local-file)))))


  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))


  ;; We need additional scripts!! TODO: Moving this in an appropriate
  ;; place like epy-setup.el
  
  (add-to-list 'exec-path (concat epy-install-dir "scripts"))
  (defun flymake-pylint-init () 
    ;; Pyflakes stuff
    ;; Make sure it's not a remote buffer or flymake would not work
    (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                         'flymake-create-temp-inplace)) 
             (local-file (file-relative-name 
                          temp-file 
                          (file-name-directory buffer-file-name)))) 
        (list "python" (list (concat epy-install-dir "scripts/pylint-mod.py") local-file)))))
  
  
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pylint-init))
  )

;; Adding to the variable the regexps that matches warning messages
;;(setq flymake-log-level 3)

(setq flymake-info-line-regex
      (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))



;; Not on all modes, please
(add-hook 'python-mode-hook 'flymake-find-file-hook)

(provide 'python-flymake)
