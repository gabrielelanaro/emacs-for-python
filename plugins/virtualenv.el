;; This module defines the workon command for using virtualenv inside
;; emacs
(setq workon-home (getenv "WORKON_HOME"))

(defun add-to-PATH (dir)
  "Add the specified path element to the Emacs PATH"
  (interactive "DEnter directory to be added to PATH: ")
  (if (file-directory-p dir)
      (setenv "PATH"
              (concat (expand-file-name dir)
                      path-separator
                      (getenv "PATH")))))

(defun activate-virtualenv (dir)
  (setenv "VIRTUAL_ENV" dir)
  (add-to-PATH (concat dir "/bin"))
  (add-to-list 'exec-path (concat dir "/bin")))

(defun is_virtualenv (dir)
  "Check if a directory is a virtualenv"
  (file-exists-p (concat dir "/bin/activate"))
  )

(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun workon-complete ()
  "return available completions for workon"
  (let 
      ;;Varlist				
      ((filelist (directory-files workon-home t))) ;; List directory
       ;; Let Body
    (mapcar 'file-name-nondirectory
       (filter 'is_virtualenv ;; select virtualenvs
	(filter 'file-directory-p filelist))) ;; select  directories
    )
  )

(defun workon (name)
  "Issue a virtualenvwrapper-like workon command"
  (interactive (list (completing-read "Virtualenv: " (workon-complete))))
  (activate-virtualenv (concat (getenv "WORKON_HOME") "/" name))
  )


(provide 'virtualenv)