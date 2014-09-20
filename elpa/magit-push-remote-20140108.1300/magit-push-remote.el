;;; magit-push-remote.el --- push remote support for Magit
;; Version: 20140108.1300

;; Copyright (C) 2012-2014  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120613
;; Package-Requires: ((magit "1.3.0"))
;; Homepage: https://github.com/tarsius/magit-push-remote
;; Keywords: convenience

;; This file is not part of Magit.
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides support for an additional default remote
;; which when pushing is used instead of the "merge" default specified
;; by the git-config(1) option `branch.<name>.remote'.

;; Together `branch.<name>.remote' and `branch.<name>.merge' set the
;; default used by git-pull(1) and git-push(1).  Like their git
;; counterparts `magit-push' and `magit-pull' use these options. So
;; does `magit-status' which displays commits not pushed to or not
;; pulled from the default remote/branch.

;; This works nicely if commits most often flow like this:
;;
;;   +------------+            +------------+
;;   |   remote   | -- pull -> |   local    |
;;   |    repo    | <- push -- |    repo    |
;;   +------------+            +------------+

;; But it is inconventient if commits most often flow through your
;; local repository like this:
;;
;;   +------------+            +------------+            +------------+
;;   |  upstream  | -- pull -> |   local    |            |    your    |
;;   |    repo    |            |    repo    | -- push -> |   public   |
;;   |            |            +------------+            |    repo    |
;;   |            | <- merge pull reguest -------------- |            |
;;   +------------+                                      +------------+

;; This package modifies Magit to automatically detect whether the
;; latter workflow is used; and if so provide additional information
;; related to that "personal" or "push" remote and push to it by
;; default.

;; When `magit-push-remote-mode' is turned on and the repository has a
;; push-remote `magit-push' and `magit-push-tags' now by default push
;; to the push-remote, and `magit-status' shows information related to
;; both the push and pull (Git's default) remote.

;; This is done by REDEFINING `magit-push-tags' and adding specialized
;; functions to the hooks `magit-status-sections-hook' and
;; `magit-push-hook'.

;; To enable this turn on the global `magit-push-remote-mode' and
;; select the push-remote either per repository or globally using the
;; git variable `magit.pushremote'.
;;
;;   (magit-push-remote-mode 1)
;;
;;   git config --global magit.pushremote <REMOTE_NAME>  # or
;;   git config magit.pushremote <REMOTE_NAME>

;;; Code:

(require 'magit)

;;;###autoload
(define-minor-mode magit-push-remote-mode
  "Push remote support for Magit."
  :lighter ""
  :require 'magit-push-remote
  :global t
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with Magit"))
  (cond
   (magit-push-remote-mode
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-pushr-insert-remote
                            'magit-insert-status-remote-line t)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-pushr-insert-unpulled
                            'magit-insert-unpulled-commits t)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-pushr-insert-unpushed
                            'magit-insert-unpulled-commits t)
    (magit-add-section-hook 'magit-push-hook
                            'magit-pushr-push
                            'magit-push-dwim))
   (t
    (remove-hook 'magit-status-sections-hook 'magit-pushr-insert-remote)
    (remove-hook 'magit-status-sections-hook 'magit-pushr-insert-unpulled)
    (remove-hook 'magit-status-sections-hook 'magit-pushr-insert-unpushed)
    (remove-hook 'magit-push-hook 'magit-pushr-push))))

;; REDEFINE `magit-push-tags' DEFINED IN `magit.el'.
;;
(defun magit-push-tags ()
  "Push tags to a remote repository.

With a prefix argument or when the remote cannot be determined as
described below ask the user what remote to push to.

When `magit-push-remote-mode' is turned on and the repository has
a push-remote push to that.  See `magit-push-remote-mode' for how
the push-remote is determined.

Otherwise push to the remote specified by the git-config(1)
option `branch.<name>.remote' if set; else \"origin\" if it
exists; or if only one remote is configured use that."
  (interactive)
  (let* ((branch      (magit-get-current-branch))
         (remotes     (magit-git-lines "remote"))
         (pull-remote (and branch (magit-get-remote branch)))
         (push-remote (and magit-push-remote-mode
                           pull-remote
                           (magit-get-push-remote branch)))
         (remote      (or push-remote
                          pull-remote
                          (car (member "origin" remotes))
                          (and (= (length remotes) 1)
                               (car remotes)))))
    (when (or current-prefix-arg (not remote))
      (setq remote (magit-read-remote "Push to remote" remote)))
    (magit-run-git-async "push" remote "--tags")))

;;;###autoload
(defun magit-pushr-push (arg)
  "Push the current branch to a remote repository.

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
options `branch.<name>.remote' and `branch.<name>.merge'."
  (interactive "P")
  (let* ((branch (or (magit-get-current-branch)
                     (error "Don't push a detached head.  That's gross")))
         (pull-remote (magit-get-remote branch))
         (push-remote (and magit-push-remote-mode
                           pull-remote
                           (magit-get-push-remote branch)))
         (push-remote (unless (equal push-remote pull-remote) push-remote))
         (auto-remote (or push-remote pull-remote))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote))
         (auto-branch (cond ((equal used-remote push-remote)
                             (magit-get-push-remote-branch branch))
                            ((equal used-remote pull-remote)
                             (magit-get "branch" branch "merge"))))
         (used-branch (if (>= (prefix-numeric-value arg) 16)
                          (magit-read-remote-branch
                           (format "Push %s as branch" branch)
                           used-remote auto-branch)
                        auto-branch)))
    (cond ;; Pushing to what's already configured.
          ((and auto-branch
                (equal push-remote used-remote)
                (equal auto-branch used-branch)))
          ;; Setting upstream because of magit-custom-options.
          ((member "-u" magit-custom-options))
          ;; Two prefix arguments; ignore magit-set-upstream-on-push.
          ((>= (prefix-numeric-value arg) 16)
           (and (yes-or-no-p "Set upstream while pushing? ")
                (setq magit-custom-options
                      (cons "-u" magit-custom-options))))
          ;; Pushing to the push-remote; don't set upstream and don't
          ;; refuse; completely ignoring magit-set-upstream-on-push.
          ;; But it is still possible to override this using two prefix
          ;; arguments or magit-custom-options.
          ((equal push-remote auto-remote))
          ;; Else honor magit-set-upstream-on-push.
          ((eq magit-set-upstream-on-push 'refuse)
           (error "Not pushing since no upstream has been set."))
          ((or (eq magit-set-upstream-on-push 'dontask)
               (and (eq magit-set-upstream-on-push t)
                    (yes-or-no-p "Set upstream while pushing? ")))
           (setq magit-custom-options (cons "-u" magit-custom-options))))
    (magit-run-git-async
     "push" "-v" used-remote
     (if used-branch (format "%s:%s" branch used-branch) branch)
     magit-custom-options)))

(defun magit-pushr-insert-remote ()
  (let* ((branch (magit-get-current-branch))
         (remote (magit-get-push-remote branch)))
    (when remote
      (magit-insert-line-section (line)
        (concat "Push: "
                (magit-format-tracked-line
                 remote (magit-get-push-remote-branch branch)))))))

(defun magit-pushr-insert-unpulled ()
  (let* ((branch (magit-get-tracked-branch))
         (remote (magit-get-push-remote branch)))
    (when remote
      (magit-git-insert-section
          (unpulled (format "Unpulled commits @ %s:" remote))
          (apply-partially 'magit-wash-log 'unique)
        "log" "--format=format:* %h %s" (magit-diff-abbrev-arg)
        (format "HEAD..%s/%s" remote
                (magit-get-push-remote-branch branch))))))

(defun magit-pushr-insert-unpushed ()
  (let* ((branch (magit-get-tracked-branch))
         (remote (magit-get-push-remote branch)))
    (when remote
      (magit-git-insert-section
          (unpushed (format "Unpushed commits @ %s:" remote))
          (apply-partially 'magit-wash-log 'unique)
        "log" "--format=format:* %h %s" (magit-diff-abbrev-arg)
        (format "%s/%s..HEAD" remote
                (magit-get-push-remote-branch branch))))))

(defun magit-get-push-remote (branch)
  (let ((pull-remote (magit-get "branch" branch "remote"))
        (push-remote
         (car (member (or (magit-get "branch" branch "pushremote")
                          (magit-get "magit.defaultpushremote"))
                      (magit-git-lines "remote")))))
    (unless (or (string= push-remote "")
                (equal push-remote pull-remote))
      push-remote)))

(defun magit-get-push-remote-branch (branch)
  (let ((remote-branch (magit-get "branch" branch "push")))
    (save-match-data
      (if (and remote-branch
               (string-match "^refs/heads/\\(.+\\)" remote-branch))
          (match-string 1 remote-branch)
        branch)))) ; always default to the local name

(provide 'magit-push-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-push-remote.el ends here
