;; Module to perform python unit tests

;; To parse the error I can use the regex association list defined in
;; compilation-error-regex alist, that matches all, like fname etc...
(defvar unittest-project-root nil "Where to run unittests")

(defun unittest-set-project-root (root)
  "Configure the project from where to run unittests"
  (interactive "DWere to run unittests: ")
  (setq unittest-project-root root)
)

(defun run-unittest-command (command)
  "Runs an unittest command from the unittest-project-root variable."
  (interactive "sInsert command to run: ")
  (when unittest-project-root
    (let (default-directory) 
      (setq default-directory unittest-project-root)
      (shell-command command)))
  )
