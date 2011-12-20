;; Project-related functions for emacs-for-python

;; This is an utility
(defun epy-proj-find-root-dir (&optional startdir)
  "Find the root dir of this file, it's a previous directory that
  has a setup.py file in it, if it doesn't find nothing return
  the startdir"
  
  (unless startdir
    (setq startdir default-directory))
  
  (if (member "setup.py" (directory-files startdir))
      (expand-file-name startdir)
    (if (equal startdir "/")
	startdir
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
	(rootdir (epy-proj-find-root-dir))
        tests  
	testname
	test
	module)
    
    ;; rootdir is needed to define tests
    (when rootdir
      (setq tests (epy-unittest-discover rootdir))) 
    
    ;; There should be tests otherwise don't make the menu
    (when tests
      ;; I'm doing this instead of simply giving (current-local-map)to
      ;; have the menu for only this buffer, don't know why though.
      (set-keymap-parent newmap (current-local-map))
      (define-key newmap [menu-bar pytests]
	(cons "PyTests" (make-sparse-keymap "PyTests")))
      

      ;; Add each test to the menu
      (dolist (testentry tests)

	(setq module (first testentry))
	
	(define-key newmap (vector 'menu-bar 'pytests (make-symbol module))
	  (cons module (make-sparse-keymap module)))
	
	(define-key newmap (vector 'menu-bar 'pytests (make-symbol module)
				   'runall)
	  (cons "Run All" `(lambda () 
			     (interactive)
			     (epy-proj-run-testmodule ,module ,rootdir)))
	  )

	(dolist (test (car (last testentry)))
	  (setq testname (plist-get test ':name))
	  (define-key newmap (vector 'menu-bar 'pytests (make-symbol module) (make-symbol testname))
	    ;; It took me all night to write this hackish closure!!! 
	    (cons testname `(lambda () (interactive) (epy-proj-run-test ',test)))) ;
	  )
	) 
      (use-local-map newmap))
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

(defun epy-proj-run-testmodule (module rootdir)
  "Take the MODULE as a string and run all the tests defined in it"
  (let (default-directory)
    (cd rootdir)
    (compile (concat "python -munittest " module))
    )
  )  

(provide 'epy-project)



