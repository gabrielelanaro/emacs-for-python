;;; magit-remote-utils.el --- git remote utilities

;; Copyright (C) 2012-2013  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

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

;; Loading library `magit-push-remote' redefines some existing Magit
;; commands.  This library just defines some new functions which are
;; used by the former but could also by useful if you *don't* want
;; any commands to be redefined.

;;; Code:

(require 'magit)

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

(defun magit-get-push-remote/branch (&optional branch verify)
  "Return the remote-tracking branch of BRANCH used for pushing.
Return a string of the form \"REMOTE/BRANCH\".

If optional BRANCH is nil return the remote-tracking branch of
the current branch.  If optional VERIFY is non-nil verify that
the remote branch exists; else return nil."
  (progn
    (let (remote remote-branch remote/branch)
      (and (or branch (setq branch (magit-get-current-branch)))
           (setq remote (magit-get-push-remote branch))
           (setq remote-branch (magit-get-push-remote-branch branch))
           (setq remote/branch (concat remote "/" remote-branch))
           (or (not verify)
               (= 0 (magit-git-exit-code "rev-parse" "--verify"
                                         remote/branch)))
           remote/branch))))

(defun magit-get-pull-remote/branch (&optional branch verify)
  "Return the remote-tracking branch of BRANCH used for pulling.
Return a string of the form \"REMOTE/BRANCH\".

If optional BRANCH is nil return the remote-tracking branch of
the current branch.  If optional VERIFY is non-nil verify that
the remote branch exists; else return nil."
  (save-match-data
    (let (remote remote-branch remote/branch)
      (and (or branch (setq branch (magit-get-current-branch)))
           (setq remote (magit-get "branch" branch "remote"))
           (setq remote-branch (magit-get "branch" branch "merge"))
           (string-match "^refs/heads/\\(.+\\)" remote-branch)
           (setq remote/branch (concat remote "/"
                                       (match-string 1 remote-branch)))
           (or (not verify)
               (= 0 (magit-git-exit-code "rev-parse" "--verify"
                                         remote/branch)))
           remote/branch))))

(provide 'magit-remote-utils)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-remote-utils.el ends here
