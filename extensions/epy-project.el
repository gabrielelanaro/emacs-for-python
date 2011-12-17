;; Project-related functions for emacs-for-python
(defun epy-proj-build-menu ()
  "Build the menu for epy-proj"
  (define-key-after global-map [menu-bar pyproject]
    (cons "Epy"(make-sparse-keymap "Epy"))
    'tools)
  )

(defun epy-proj-find-root-dir (&optional startdir)
  "Find the root dir of this file, it's a previous directory that
  has a setup.py file in it"
  
  (unless startdir
    (setq startdir default-directory))
  
  (if (member "setup.py" (directory-files startdir))
      (expand-file-name startdir)
    (if (equal startdir "/")
	nil
      (epy-proj-find-root-dir  (expand-file-name (concat startdir "../")))
      )
    )
  )

;; Tests are stored with a certain plist
;; :name   the dotted name of the test
;; :module the dotted name of the module
;; :root   the directory whose module dotted name is relative
(pymacs-load "epy-unittest" "epy-unittest-")

(defun epy-proj-build-test-menu ()
  "Build the sub-menu related to test discovery"
  (interactive)
  
  (define-key global-map [menu-bar pytests]
    (cons "PyTests" (make-sparse-keymap "PyTests")))

  (let ((tests (epy-unittest-discover (epy-proj-find-root-dir))) testname)
    (dolist (test tests)
      (setq testname (plist-get test ':name))
      (define-key global-map (vector 'menu-bar 'pytests (make-symbol testname))
    	(cons testname `(lambda () (interactive) (epy-proj-run-test ',test)))) ;; It took me all night to write this hackish closure!!!
      )
    )
  )

(defun epy-proj-run-test (test)
  "Take a TEST plist that represents a test and run it using the 
unittest (Python 2.7) utility"
  (let (default-directory)
    (cd (plist-get test ':root))
    (compile (concat "python -munittest " 
		     (plist-get test ':module)
		     "."
		     (plist-get test ':name)))
    )
  )





