;; Project-related functions for emacs-for-python


(defun epy-proj-build-menu ()
  "Build the menu for epy-proj"
  (define-key-after global-map [menu-bar pyproject]
    (cons "Epy"(make-sparse-keymap "Epy"))
    'tools)
  )

;; Tests are stored with a certain plist
;; :name   the dotted name of the test
;; :module the dotted name of the module
;; :root   the directory whose module dotted name is relative

(defun epy-proj-build-test-menu ()
  "Build the sub-menu related to test discovery"
  (interactive)
  
  (pymacs-load "epy-unittest" "epy-unittest-")
  
  (define-key global-map [menu-bar pytests]
    (cons "PyTests" (make-sparse-keymap "PyTests"))
    )

    (let ((tests (epy-unittest-discover default-directory)) testname)
      (dolist (test tests)
	(setq testname (plist-get test ':name))
        (define-key global-map (vector 'menu-bar 'pytests (make-symbol testname))
    	(cons testname `(lambda () (interactive) (epy-proj-run-test ',test)))) ;; It took me all night to write this hackish closure!!!
        )
      )
  )

(defun epy-proj-run-test (test)
  "Take a TEST data structure and run the test"
  ;;(shell-command "python -munittest2")
  (shell-command (concat "echo Running Test " (plist-get test ':name)))
  )
