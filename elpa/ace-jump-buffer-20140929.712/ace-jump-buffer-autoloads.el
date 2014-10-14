;;; ace-jump-buffer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (make-ace-jump-buffer-function ace-jump-buffer-with-configuration
;;;;;;  ace-jump-buffer-in-one-window ace-jump-buffer-other-window
;;;;;;  ace-jump-buffer) "ace-jump-buffer" "ace-jump-buffer.el" (21565
;;;;;;  7823 283786 347000))
;;; Generated autoloads from ace-jump-buffer.el

(autoload 'ace-jump-buffer "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode'.

\(fn)" t nil)

(autoload 'ace-jump-buffer-other-window "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode' in other window.

\(fn)" t nil)

(autoload 'ace-jump-buffer-in-one-window "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode' in one window.

\(fn)" t nil)

(autoload 'ace-jump-buffer-with-configuration "ace-jump-buffer" "\
Quickly hop to buffer with `ace-jump-mode' with selected configuration.

\(fn)" t nil)

(autoload 'make-ace-jump-buffer-function "ace-jump-buffer" "\
Create a `bs-configuration' and interactive defun using NAME that displays buffers
that don't get rejected by the body of BUFFER-LIST-REJECT-FILTER.

\(fn NAME &rest BUFFER-LIST-REJECT-FILTER)" nil t)

(put 'make-ace-jump-buffer-function 'lisp-indent-function '1)

;;;***

;;;### (autoloads nil nil ("ace-jump-buffer-pkg.el") (21565 7823
;;;;;;  345388 326000))

;;;***

(provide 'ace-jump-buffer-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ace-jump-buffer-autoloads.el ends here
