;; Project-related functions for emacs-for-python

;; This is an utility
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

  (let ((newmap (make-sparse-keymap))
        (tests (epy-unittest-discover (epy-proj-find-root-dir))) 
	testname)
    
    ;; I'm doing this to have the menu for only this buffer
    (set-keymap-parent newmap (current-local-map))    
    (define-key newmap [menu-bar pytests]
      (cons "PyTests" (make-sparse-keymap "PyTests")))
    (dolist (test tests)
      (setq testname (plist-get test ':name))
      (define-key newmap (vector 'menu-bar 'pytests (make-symbol testname))
    	(cons testname `(lambda () (interactive) (epy-proj-run-test ',test)))) ;; It took me all night to write this hackish closure!!!
      ) 
    (use-local-map newmap))
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

(provide 'epy-project)



