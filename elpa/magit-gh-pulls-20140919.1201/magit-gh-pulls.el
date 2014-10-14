;;; magit-gh-pulls.el --- GitHub pull requests extension for Magit

;; Copyright (C) 2011-2014 Yann Hodique, Alexander Yakushev

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: tools
;; Version: 20140919.1201
;; X-Original-Version: 0.4
;; URL: https://github.com/sigma/magit-gh-pulls
;; Package-Requires: ((emacs "24") (gh "0.4.3") (magit "1.1.0") (pcache "0.2.3") (s "1.6.1"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a Magit extension for manipulating GitHub pull requests

;; No configuration is needed in the repository if any of your remotes contain a
;; URL to Github's remote repository. If for some reason you don't have any
;; Github remotes in your config, you can specify username and repository
;; explicitly:

;; $ git config magit.gh-pulls-repo <user>/<repo> # your github repository

;; Add these lines to your init.el:

;; (require 'magit-gh-pulls.el)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; There are currently 4 bindings for pull requests:
;; # g g --- refreshes the list of pull requests
;; # g f --- fetches the commits associated with the pull request at point
;; # g b --- helps you creating a topic branch from a review request
;; # g m --- merges the PR on top of the current branch

;; Then, you can do whatever you want with the commit objects associated with
;; the pull request (merge, cherry-pick, diff, ...)

;;; Code:

(require 'eieio)

(require 'magit)
(require 'gh-pulls)
(require 'pcache)
(require 's)

(defun magit-gh-pulls-get-api ()
  (gh-pulls-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-pulls-get-repo-from-config ()
  (let* ((cfg (magit-get "magit" "gh-pulls-repo")))
    (when cfg
      (let* ((split (split-string cfg "/")))
        (cons (car split) (cadr split))))))

(defun magit-gh-pulls-parse-url (url)
  (let ((creds (cond
                ((s-matches? "github.com:" url)
                 (s-match "github.com:\\(.+\\)/\\([^.]+\\)\\(.git\\)?$" url))

                ((s-matches? "^https?://github.com" url)
                 (s-match "^https://github.com/\\(.+\\)/\\([^./]+\\)\\(.git\\)?/?$" url))
                ((s-matches? "git://github.com/" url)
                 (s-match "git://github.com/\\(.+\\)/\\([^.]+\\)\\(.git\\)?$" url)))))
    (when creds
      (cons (cadr creds) (caddr creds)))))

(defun magit-gh-pulls-guess-repo-from-origin ()
  (let ((creds nil))
    (dolist (remote (magit-git-lines "remote") creds)
      (let ((parsed (magit-gh-pulls-parse-url
                     (magit-get "remote" remote "url"))))
        (when parsed
          (setq creds parsed))))))

(defun magit-gh-pulls-guess-repo ()
  (or (magit-gh-pulls-get-repo-from-config)
      (magit-gh-pulls-guess-repo-from-origin)))

(defun magit-gh-pulls-insert-gh-pulls ()
  (condition-case print-section
      (progn
        (let* ((repo (magit-gh-pulls-guess-repo)))
          (when repo
            (let* ((api (magit-gh-pulls-get-api))
                   (user (car repo))
                   (proj (cdr repo))
                   (stubs (oref (gh-pulls-list api user proj) :data))
                   (branch (magit-get-current-branch)))
              (when (> (length stubs) 0)
                (magit-with-section (section stubs 'pulls "Pull Requests:" t)
                  (dolist (stub stubs)
                    (let* ((id (oref stub :number))
                           (req (oref (gh-pulls-get api user proj id) :data))
                           (base-sha (oref (oref req :base) :sha))
                           (base-ref (oref (oref req :base) :ref))
                           (head-sha (oref (oref req :head) :sha))
                           ;; branch has been deleted in the meantime...
                           (invalid (equal (oref (oref req :head) :ref) head-sha))
                           (have-commits
                            (and (eql 0 (magit-git-exit-code "cat-file" "-e" base-sha))
                                 (eql 0 (magit-git-exit-code "cat-file" "-e" head-sha))))
                           (applied (and have-commits
                                         (not (magit-git-string
                                               "rev-list"
                                               "--cherry-pick" "--right-only"
                                               (format "HEAD...%s" head-sha)
                                               "--not"
                                               (format "%s" base-sha)))))
                           (header (concat (format "\t[%s@%s] " id
                                                   (if (string= base-ref branch)
                                                       (propertize base-ref
                                                                   'face 'magit-branch)
                                                     base-ref))
                                           (propertize (format "%s\n" (oref req :title))
                                                       'face (cond (applied 'widget-inactive)
                                                                   (have-commits 'default)
                                                                   (invalid 'error)
                                                                   (t 'italic)))))
                           (info (list user proj id)))
                      (cond (have-commits
                             (magit-with-section (section pull info)
                               (insert header)
                               (when (and have-commits (not applied))
                                 (magit-git-insert-section (request)
                                     (apply-partially 'magit-wash-log 'unique)
                                   "log" "--format=format:%h %s" (format "%s..%s" base-sha head-sha)))))
                            (invalid (magit-with-section (section invalid-pull info)
                                       (insert header)))
                            (t (magit-with-section (section unfetched-pull info)
                                 (insert header))))))
                  (when (> (length stubs) 0)
                    (insert "\n"))))))))
    (error nil)))

(defun magit-gh-pulls-guess-topic-name (req)
  (let ((user (oref (oref req :user) :login))
        (topic (oref (oref req :head) :ref)))
    (format "%s/%s" user topic)))

(defun magit-gh-pulls-create-branch ()
  (interactive)
  (magit-section-action pr-create-branch (info)
    (pull
     (let* ((api (magit-gh-pulls-get-api))
            (req (oref (apply 'gh-pulls-get api info) :data))
            (branch (read-from-minibuffer
                     "Branch name: " (magit-gh-pulls-guess-topic-name req)))
            (base (magit-read-rev "Branch base: "
                                  (oref (oref req :base) :ref))))
       (magit-create-branch branch base)
       (magit-merge (oref (oref req :head) :sha))))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-merge-pull-request ()
  (interactive)
  (magit-section-action pr-merge (info)
    (pull
     (let* ((api (magit-gh-pulls-get-api))
            (req (oref (apply 'gh-pulls-get api info) :data))
            (branch (magit-gh-pulls-guess-topic-name req))
            (base (oref (oref req :base) :ref)))
       (magit-create-branch branch base)
       (magit-merge (oref (oref req :head) :sha))
       (magit-checkout base)
       (magit-merge branch)
       (magit-delete-branch branch)))
    (unfetched-pull
     (error "Please fetch pull request commits first"))
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-fetch-commits ()
  (interactive)
  (magit-section-action pr-fetch-commits (info)
    (unfetched-pull
     (let* ((api (magit-gh-pulls-get-api))
            (req (oref (apply 'gh-pulls-get api info) :data))
            (head (oref req :head)))
       (magit-run-git "fetch" (oref (oref head :repo) :git-url)
                      (oref head :ref))))
    (pull nil)
    (invalid-pull
     (error "This pull request refers to invalid reference"))))

(defun magit-gh-pulls-purge-cache ()
  (let* ((api (magit-gh-pulls-get-api))
         (cache (oref api :cache))
         (repo (magit-gh-pulls-guess-repo)))
    (pcache-map cache (lambda (k v)
                        (when (string-match
                               (format "/repos/%s/%s/" (car repo) (cdr repo))
                               (car k))
                          (pcache-invalidate cache k))))))

(defun magit-gh-pulls-reload ()
  (interactive)
  (let ((creds (magit-gh-pulls-guess-repo)))
    (if (not (and creds (car creds) (cdr creds)))
        (message "Remote repository is not configured or incorrect.")
      (magit-gh-pulls-purge-cache)
      (magit-refresh))))

(easy-menu-define magit-gh-pulls-extension-menu
  nil
  "GitHub Pull Requests extension menu"
  '("GitHub Pull Requests"
    :visible magit-gh-pulls-mode
    ["Reload pull request" magit-gh-pulls-reload]
    ["Create pull request branch" magit-gh-pulls-create-branch]
    ["Fetch pull request commits" magit-gh-pulls-fetch-commits]
    ))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-gh-pulls-extension-menu)

(defvar magit-gh-pulls-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "# g b") 'magit-gh-pulls-create-branch)
    (define-key map (kbd "# g f") 'magit-gh-pulls-fetch-commits)
    (define-key map (kbd "# g g") 'magit-gh-pulls-reload)
    (define-key map (kbd "# g m") 'magit-gh-pulls-merge-pull-request)
    map))

(defvar magit-gh-pulls-mode-lighter " Pulls")

;;;###autoload
(define-minor-mode magit-gh-pulls-mode "Pull requests support for Magit"
  :lighter  magit-gh-pulls-mode-lighter
  :require 'magit-gh-pulls
  :keymap  'magit-gh-pulls-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-gh-pulls-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-pulls-insert-gh-pulls
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-pulls-insert-gh-pulls))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-gh-pulls ()
  "Unconditionally turn on `magit-pulls-mode'."
  (magit-gh-pulls-mode 1))

(provide 'magit-gh-pulls)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-gh-pulls.el ends here
