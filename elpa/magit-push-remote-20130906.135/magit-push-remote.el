;;; magit-push-remote.el --- push remote support for Magit

;; Copyright (C) 2012-2013  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20120613
;; Version: 0.4.1
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

;; This package modifies magit to automatically detect whether the
;; latter workflow is used; and if so provide additional information
;; related to that "personal" or "push" remote and push to it by
;; default.

;; Loading this library redefines the commands `magit-push',
;; `magit-push-tags', and `magit-refresh-status'.

;; When `magit-push-remote-mode' is turned on and the repository has a
;; push-remote `magit-push' and `magit-push-tags' now by default push
;; to the push-remote.  Otherwise they behave mostly like the original
;; versions defined in `magit.el'.  (The `magit-push' defined here
;; actually differs a bit in that it is more carefull about when to
;; --set-upstream.)

;; When `magit-push-remote-mode' is turned on and the repository has a
;; push-remote `magit-status' shows information related to both the
;; push and pull (git's default) remote.  Otherwise it behaves like
;; the version in `magit.el'.

;; `magit-push-remote-mode' should be turned on in all Magit buffers;
;;
;;   (add-hook 'magit-mode-hook 'turn-on-magit-push-remote)

;; The push-remote is determined based on it's name.  A good name is
;; e.g. your username.  Again it makes sense to set this globally:
;;
;;   git config --global magit.defaultpushremote <REMOTE_NAME>

;; If you want to use a different name in some repositories, that is
;; also possible:
;;
;;   git config magit.pushremote <REMOTE_NAME>

;; Now read `magit-push's doc-string and you are ready to go.

;;; Code:

(require 'magit)
(require 'magit-remote-utils)

;;;###autoload
(define-minor-mode magit-push-remote-mode
  "Push remote support for Magit."
  :lighter "" :retire 'magit-push-remote
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit")))

;;;###autoload
(defun turn-on-magit-push-remote ()
  "Unconditionally turn on `magit-push-remote-mode'."
  (magit-push-remote-mode 1))

;; REDEFINE `magit-push-tags' DEFINED IN `magit.el'.
;;
(magit-define-command push-tags ()
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

;; REDEFINE `magit-push' DEFINED IN `magit.el'.
;;
(magit-define-command push ()
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
options `branch.<name>.remote' and `branch.<name>.merge'.

This function is redefined in `magit-push-remote.el' replacing the
original definition in `magit.el'.  When `magit-push-remote-mode' is
off or the repository has no push-remote then the only difference is
that for older Git versions setting the upstream might not work."
  (interactive)
  (let* ((branch (or (magit-get-current-branch)
                     (error "Don't push a detached head.  That's gross")))
         (pull-remote (magit-get-remote branch))
         (push-name   (and magit-push-remote-mode
                           pull-remote
                           (magit-get-push-remote branch)))
         (push-remote (unless (equal push-name pull-remote) push-name))
         (auto-remote (or push-remote pull-remote))
         (used-remote
          (if (or current-prefix-arg
                  (not auto-remote))
              (magit-read-remote (format "Push %s to remote" branch)
                                 auto-remote)
            auto-remote))
         (auto-remote-branch
          (cond ((equal used-remote push-remote)
                 (magit-get-push-remote-branch branch))
                ((equal used-remote pull-remote)
                 (magit-get "branch" branch "merge"))))
         (used-remote-branch
          (if (>= (prefix-numeric-value current-prefix-arg) 16)
              (concat "refs/heads/"
                      (magit-read-remote-branch
                       used-remote
                       (format "Push %s as branch" branch)
                       auto-remote-branch))
            auto-remote-branch)))
    (cond
     ;; Pushing to what's already configured.
     ((and (equal push-remote        used-remote)
           (equal auto-remote-branch used-remote-branch)
           auto-remote-branch))
     ;; Setting upstream because of magit-custom-options.
     ((member "-u" magit-custom-options))
     ;; Two prefix arguments; ignore magit-set-upstream-on-push.
     ((>= (prefix-numeric-value current-prefix-arg) 16)
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
    (apply 'magit-run-git-async "push" "-v" used-remote
           (if used-remote-branch
               (format "%s:%s" branch used-remote-branch)
             branch)
           magit-custom-options)
    ;; In older Git versions before 1.7.0 -u did set the remote but
    ;; not the remote branch.
    (when (and used-remote-branch (member "-u" magit-custom-options))
      (magit-set used-remote-branch "branch" branch "merge"))))

;; REDEFINE `magit-refresh-status' DEFINED IN `magit.el'.
;;
(defun magit-refresh-status ()
  (magit-create-buffer-sections
    (magit-with-section 'status nil
      (let* ((branch (magit-get-current-branch))
             (remote (and branch (magit-get "branch" branch "remote")))
             (remote-rebase (and branch (magit-get-boolean "branch" branch "rebase")))
             (remote-branch (or (and branch (magit-remote-branch-for branch)) branch))
             (remote-string (magit-remote-string remote remote-branch remote-rebase))
             (push-remote (and magit-push-remote-mode
                               (magit-get-push-remote branch)))
             (push-remote-branch (and push-remote
                                      (magit-get-push-remote-branch branch)))
             (push-remote-string (and push-remote
                                      (magit-remote-string
                                       push-remote push-remote-branch nil)))
             (head (magit-format-commit "HEAD" "%h %s"))
             (no-commit (not head))
             (merge-heads (magit-file-lines (magit-git-dir "MERGE_HEAD")))
             (rebase (magit-rebase-info))
             (staged (or no-commit (magit-anything-staged-p))))
        (when remote
          (if push-remote
              (progn
                (magit-insert-status-line "Pull" remote-string)
                (magit-insert-status-line "Push" push-remote-string))
            (magit-insert-status-line "Merge" remote-string)))
        (magit-insert-status-line
         "Local"
         (concat (propertize (magit--bisect-info-for-status branch)
                             'face 'magit-branch)
                 " " (abbreviate-file-name default-directory)))
        (magit-insert-status-line
         "Head" (if no-commit "nothing commited (yet)" head))
        (magit-insert-status-tags-line)
        (when merge-heads
          (magit-insert-status-line
           "Merging"
           (concat
            (mapconcat 'identity (mapcar 'magit-name-rev merge-heads) ", ")
            "; Resolve conflicts, or press \"m A\" to Abort")))
        (when rebase
          (magit-insert-status-line
           "Rebasing"
           (apply 'format
                  "onto %s (%s of %s); Press \"R\" to Abort, Skip, or Continue"
                  rebase))
          (when (nth 3 rebase)
            (magit-insert-status-line
             "Stopped"
             (magit-format-commit (nth 3 rebase) "%h %s"))))
        (insert "\n")
        (magit-git-exit-code "update-index" "--refresh")
        (magit-insert-stashes)
        (magit-insert-untracked-files)
        (magit-insert-pending-changes)
        (magit-insert-pending-commits)
        (when magit-push-remote-mode
          (magit-insert-push-remote-unpulled-commits
           push-remote push-remote-branch))
        (let ((staged (or no-commit (magit-anything-staged-p))))
          (magit-insert-unstaged-changes)
          (magit-insert-staged-changes staged no-commit))
        (magit-insert-unpulled-commits remote remote-branch)
        (if push-remote
            (progn
              (magit-insert-pull-remote-unmerged-commits
               remote remote-branch)
              (magit-insert-push-remote-unpushed-commits
               push-remote push-remote-branch))
          (magit-insert-unpushed-commits remote remote-branch)))))
  (run-hooks 'magit-refresh-status-hook))

;; The next four expressions haven't made it into Magit yet.

;; These aren't the default values that will be used in Magit.
(defvar magit-status-insert-tags-line t)
(defvar magit-status-tags-line-subject 'head)

(defun magit-insert-status-tags-line ()
  (when magit-status-insert-tags-line
    (let* ((current-tag (magit-get-current-tag t))
           (next-tag (magit-get-next-tag t))
           (both-tags (and current-tag next-tag t))
           (tag-subject (eq magit-status-tags-line-subject 'tag)))
      (when (or current-tag next-tag)
        (magit-insert-status-line
         (if both-tags "Tags" "Tag")
         (concat
          (and current-tag (apply 'magit-format-status-tag-sentence
                                  tag-subject current-tag))
          (and both-tags ", ")
          (and next-tag (apply 'magit-format-status-tag-sentence
                               (not tag-subject) next-tag))))))))

(defun magit-format-status-tag-sentence (behindp tag cnt &rest ignored)
  (concat (propertize tag 'face 'magit-tag)
          (and (> cnt 0)
               (concat (if (eq magit-status-tags-line-subject 'tag)
                           (concat " (" (propertize (format "%s" cnt)
                                                    'face 'magit-branch))
                         (format " (%i" cnt))
                       " " (if behindp "behind" "ahead") ")"))))

(defun magit-get-push-remote (branch)
  (let ((pull-remote (magit-get "branch" branch "remote"))
        (push-remote
         (car (member (or (magit-get "branch" branch "pushremote")
                          (magit-get "magit.defaultpushremote"))
                      (magit-git-lines "remote")))))
    (unless (or (string= push-remote "")
                (equal push-remote pull-remote))
      push-remote)))

(magit-define-inserter push-remote-unpulled-commits (remote remote-branch)
  (when remote
    (apply #'magit-git-section
           'unpulled
           (format "Unpulled commits @ %s:" remote)
           'magit-wash-log "log"
           (append magit-git-log-options
                   (list (format "HEAD..%s/%s" remote remote-branch))))))

(defun magit-insert-unpushed-commits-internal (remote remote-branch display)
  (when remote
    (apply #'magit-git-section
           'unpushed
           display
           'magit-wash-log "log"
           (append magit-git-log-options
                   (list (format "%s/%s..HEAD" remote remote-branch))))))

(magit-define-inserter pull-remote-unmerged-commits (remote remote-branch)
  (magit-insert-unpushed-commits-internal
   remote remote-branch "Unmerged commits:"))

(magit-define-inserter push-remote-unpushed-commits (remote remote-branch)
  (magit-insert-unpushed-commits-internal
   remote remote-branch "Unpushed commits:"))

(provide 'magit-push-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-push-remote.el ends here
