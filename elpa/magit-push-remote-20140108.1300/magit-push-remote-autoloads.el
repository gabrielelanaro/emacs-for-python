;;; magit-push-remote-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "magit-push-remote" "magit-push-remote.el"
;;;;;;  (21575 47014 621317 964000))
;;; Generated autoloads from magit-push-remote.el

(defvar magit-push-remote-mode nil "\
Non-nil if Magit-Push-Remote mode is enabled.
See the command `magit-push-remote-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-push-remote-mode'.")

(custom-autoload 'magit-push-remote-mode "magit-push-remote" nil)

(autoload 'magit-push-remote-mode "magit-push-remote" "\
Push remote support for Magit.

\(fn &optional ARG)" t nil)

(autoload 'magit-pushr-push "magit-push-remote" "\
Push the current branch to a remote repository.

With a single prefix argument ask the user what branch to push to.
With two or more prefix arguments also ask the user what remote to
push to.  Otherwise determine the remote and branch as described
below.  If the remote cannot be determined ask the user.  If the
remote branch cannot be determined push without specifing the remote
branch explicitly.

When `magit-push-remote-mode' is turned on and the current repository
has a push-remote use that.  See `magit-push-remote-mode' for how the
push-remote is determined.

Otherwise use the remote and branch specified by the git-config(1)
options `branch.<name>.remote' and `branch.<name>.merge'.

\(fn ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magit-push-remote-autoloads.el ends here
