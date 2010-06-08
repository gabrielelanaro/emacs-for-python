;; Flymake configuration with python
(require 'tramp)
(when (load "flymake" t) 
  (defun flymake-pyflakes-init () 
     ; Make sure it's not a remote buffer or flymake would not work
     (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy 
                         'flymake-create-temp-inplace)) 
             (local-file (file-relative-name 
                          temp-file 
                          (file-name-directory buffer-file-name)))) 
        (list "pyflakes" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks 
               '("\\.py\\'" flymake-pyflakes-init))) 

;; Not on all modes, please
(add-hook 'python-mode-hook 'flymake-find-file-hook)

(provide 'python-flymake)
