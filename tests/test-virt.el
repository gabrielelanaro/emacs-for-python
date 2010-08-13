(require 'cl)

(add-to-list 'load-path "~/workspace/emacs-for-python/plugins")
(add-to-list 'load-path "~/workspace/ert")
(require 'ert)
(require 'ert-ui)
(require 'virtualenv)

(defun save-path-fixture (body)
  (let (old-path)
    (unwind-protect
        (progn
          (setq old-path (getenv "PATH"))
          (funcall body))
      (setenv "PATH" old-path))))

(ert-deftest test-add-to-path ()
  "Test add-to-path behaviour"
  (save-path-fixture
   (lambda ()
     (virtualenv-add-to-path "/home/test/bin")
     (should (equal (getenv "PATH")

                    (concat "/home/test/bin" path-separator old-path))))))
;; Functional tests
(ert-deftest test-activate-path ()
  "Activate test:
 The path must change"
  (let (path path-res)
    (virtualenv-activate "~/.virtualenvs/pyusecase")
    (setq path (getenv "PATH"))
    (virtualenv-deactivate)
    (setq path-res (getenv "PATH"))
    (should (equal path (virtualenv-append-path "~/.virtualenvs/pyusecase/bin" path-res)))
    )
  )

(ert-deftest test-activate-functionality ()
  (virtualenv-activate "testvirt")
  (should (equal 0 (shell-command "python -c 'import nose'")))
  (virtualenv-deactivate)
  )

(ert-deftest test-exec-path ()
  (virtualenv-activate "~/.virtualenvs/pyusecase")
  (setq myex exec-path)
  (virtualenv-deactivate)
  (setq myexne exec-path)
  (should-not (eq myex myexne))
  )
