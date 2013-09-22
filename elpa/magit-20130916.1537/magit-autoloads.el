;;; magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (magit-status) "magit" "magit.el" (21048 23426
;;;;;;  865227 261000))
;;; Generated autoloads from magit.el

(autoload 'magit-status "magit" "\
Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which Git
repository to use even if `default-directory' is under Git
control.  Two prefix arguments means to ignore `magit-repo-dirs'
when asking for user input.

\(fn DIR)" t nil)

;;;***

;;;### (autoloads (magit-blame-mode) "magit-blame" "magit-blame.el"
;;;;;;  (21048 23426 618557 146000))
;;; Generated autoloads from magit-blame.el

(autoload 'magit-blame-mode "magit-blame" "\
Display blame information inline.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (global-magit-wip-save-mode magit-wip-save-mode
;;;;;;  magit-wip-mode) "magit-wip" "magit-wip.el" (21048 23426 971895
;;;;;;  421000))
;;; Generated autoloads from magit-wip.el

(defvar magit-wip-mode nil "\
Non-nil if Magit-Wip mode is enabled.
See the command `magit-wip-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-wip-mode'.")

(custom-autoload 'magit-wip-mode "magit-wip" nil)

(autoload 'magit-wip-mode "magit-wip" "\
In Magit log buffers; give wip refs a special appearance.

\(fn &optional ARG)" t nil)

(autoload 'magit-wip-save-mode "magit-wip" "\
Magit support for committing to a work-in-progress ref.

When this minor mode is turned on and a file is saved inside a
writable git repository then it is also committed to a special
work-in-progress ref.

\(fn &optional ARG)" t nil)

(defvar global-magit-wip-save-mode nil "\
Non-nil if Global-Magit-Wip-Save mode is enabled.
See the command `global-magit-wip-save-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-magit-wip-save-mode'.")

(custom-autoload 'global-magit-wip-save-mode "magit-wip" nil)

(autoload 'global-magit-wip-save-mode "magit-wip" "\
Toggle Magit-Wip-Save mode in all buffers.
With prefix ARG, enable Global-Magit-Wip-Save mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Wip-Save mode is enabled in all buffers where
`turn-on-magit-wip-save' would do it.
See `magit-wip-save-mode' for more information on Magit-Wip-Save mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("magit-bisect.el" "magit-cherry.el" "magit-compat.el"
;;;;;;  "magit-key-mode.el" "magit-pkg.el") (21048 23427 86605 407000))

;;;***

(provide 'magit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magit-autoloads.el ends here
