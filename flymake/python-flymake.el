;; Flymake configuration with python
;; TODO: There is some duplication, that can be removed using macros
;; TODO: Implement flymake-remove-checker

(require 'tramp)

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


(when (load "flymake-patch" t)
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

;;;;;;;;;;;

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

(provide 'python-flymake)
