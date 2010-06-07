;; Snippets to handle the testing of python modules
;; 
;; Ideas: 
;; - executing preserving the directory
;; - a configuration file to save configuration?

(defun python-exec (command)
  "The following function launches a command. When recalling the
  function you can type just RET and the command is launched with
  the same working directory as before
" 
  (interactive "cType a command: ")
  ;; Global variable
  (setq 'python-exec-dir)
  )

(defun python-exec-last-dir ()
  "Exec a function within the last dir"
)