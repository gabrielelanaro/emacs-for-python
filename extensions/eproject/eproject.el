;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eproject.el --- project workspaces for emacs
;;
;; Copyright (C) 2008-2010 grischka
;;
;; Author: grischka -- grischka@users.sourceforge.net
;; Created: 24 Jan 2008
;; Version: 0.4
;;
;; This program is free software, released under the GNU General
;; Public License (GPL, version 2). For details see:
;;
;;     http://www.fsf.org/licenses/gpl.html
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-configurable items:

(defvar prj-keybindings '(
  ([f5]         eproject-setup-toggle  always)
  ([M-right]    eproject-nextfile)
  ([M-left]     eproject-prevfile)
  ([C-f5]       eproject-dired)
  )
  "Key bindings in eproject"
  )

(defvar prj-default-config '(
  ("Make"       "make" "f9")
  ("Clean"      "make clean" "C-f9")
  ("Run"        "echo run what" "f8")
  ("Stop"       "-e eproject-killtool" "C-f8")
  ("---")
  ("Configure"  "./configure")
  ("---")
  ("Explore Project" "nautilus --browser `pwd` &")
  ("XTerm In Project" "xterm &")
  )
  "*The default tools menu for new projects in eproject."
  )

(defvar prj-autotracking t
  "*Should eproject automatically add/remove files to/from the project (nil/t)")
  ; To apply, close and reopen the project.

(defvar prj-rename-buffers t
  "*Should eproject rename buffers to project-relative filenames (nil/t)")

(defvar prj-set-default-directory nil
  "*Should eproject set the project directory as default-directory
for all project files (nil/t).")

(defvar prj-set-framepos nil
  "*Should eproject restore the last frame position/size (nil/t).")

(defvar prj-set-compilation-frame nil
  "*Should eproject show compilation output in the other frame (nil/t).")
  
(defvar prj-set-multi-isearch nil
  "*Should eproject setup multi-isearch in the project files (nil/t).")

;; End of user-configurable items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; There is a global file (~/.emacs.d/eproject.lst)
(defun prj-globalfile ()
  (expand-file-name "eproject.lst"
     (if (boundp 'user-emacs-directory) 
         user-emacs-directory
         "~/.emacs.d/"
         )))

;; with the list of all projects
(defvar prj-list)

;; and the project that was open in the last session (if any)
(defvar prj-last-open nil)

;; and the frame coords from last session
(defvar prj-frame-pos nil)

;; eproject version that created the config file
(defvar prj-version nil)

;; Here is a function to reset these
(defun prj-init ()
  (setq prj-version nil)
  (setq prj-list nil)
  (setq prj-last-open nil)
  (setq prj-frame-pos nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Each project has a directory

(defvar prj-directory)

;; with a configuration files in it
(defvar prj-default-cfg "eproject.cfg")

;; This file defines:

;; the list of files
(defvar prj-files)

;; the current file
(defvar prj-curfile)

;; an alist of settings
(defvar prj-config)

;; a list of tools
(defvar prj-tools)

;; a list of utility functions (feature incomplete)
(defvar prj-functions nil)

;; directory to run commands, default to prj-directory
(defvar prj-exec-directory)

;; The current project
(defvar prj-current)

;; A list with generated functions for each tool
(defvar prj-tools-fns)

;; A list with files removed from the project
(defvar prj-removed-files)

;; Here is a function to reset/close the project
(defun prj-reset ()
  (setq prj-version nil)
  (setq prj-current nil)
  (setq prj-directory nil)
  (setq prj-exec-directory nil)
  (setq prj-files nil)
  (setq prj-removed-files nil)
  (setq prj-curfile nil)
  (setq prj-config nil)
  (setq prj-tools-fns nil)
  (setq prj-tools (copy-tree prj-default-config))
  (prj-reset-functions)
  )

(defun prj-reset-functions ()
  (dolist (l prj-functions)
    (if (eq (car l) 'setq)
        (makunbound (cadr l))
      (fmakunbound (cadr l))
      ))
  (setq prj-functions nil)
  )

(defun prj-set-functions (s)
  (prj-reset-functions)
  (setq prj-functions s)
  (dolist (l s) (eval l))
  )

;; Some more variables:

;; the frame that exists on startup
(defvar prj-initial-frame nil)

;; this is put into minor-mode-alist
(defvar eproject-mode t)

;; where this file is in
(defvar eproject-directory)

;; eproject version that created the files
(defvar eproject-version "0.4")

;; Configuration UI
(eval-and-compile
  (defun eproject-setup-toggle () (interactive))
  (defun eproject-setup-quit () (interactive))
  (defun prj-config-get-result (s))
  (defun prj-config-reset ())
  (defun prj-config-print ())
  (defun prj-config-parse ())
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small functions

(defun caddr (l) (car (cddr l)))

(defun prj-del-list (l e)
  (let ((a (assoc (car e) l)))
    (if a
        (delq a l)
      l)))

(defun prj-add-list (l e)
  (nconc (prj-del-list l e) (list e))
  )

(defun prj-next-file (l e)
  (and (setq e (assoc (car e) l))
       (cadr (memq e l))
       ))

(defun prj-prev-file (l e)
  (prj-next-file (reverse l) e)
  )

 ; replace a closed file, either by the previous or the next.
(defun prj-otherfile (l f)
  (or (prj-prev-file l f)
      (prj-next-file l f)
      ))

;; make relative path, but only up to the second level of ..
(defun prj-relative-path (f)
  (let ((r (file-relative-name f prj-directory)))
    (if (string-match "^\\.\\.[/\\]\\.\\.[/\\]\\.\\.[/\\]" r)
        f
      r
      )))

;; friendly truncate filename
(defun prj-shortname (s)
  (let ((l (length s)) (x 30) n)
    (cond ((>= x l) s)
          ((progn
             (setq x (- x 3))
             (setq n (length (file-name-nondirectory s)))
             (if (< n l) (setq n (1+ n)))
             (>= x n)
             )
           (concat (substring s 0 (- x n)) "..." (substring s (- n)))
           )
          ((= n l)
           (concat (substring s 0 x) "...")
           )
          (t
           (concat "..." (substring s (- n) (- (- x 3) n)) "...")
           ))))

(defun prj-settitle ()
  (modify-frame-parameters
   nil
   (list (cons 'title
               (and prj-current
                    (format "emacs - %s" (car prj-current))
                    )))))

(defun eproject-addon (f)
  (concat eproject-directory f)
  )

(defun prj-goto-line (n)
  (goto-char 1)
  (beginning-of-line n)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write configuration to file

(defun prj-print-list (s fp)
  (let ((v (eval s)))
    (setq v (list 'setq s
      (if (and (atom v) (null (and (symbolp v) v)))
          v
          (list 'quote v)
          )))
    ;;(print v fp)
    (pp v fp) (princ "\n" fp)
    ))

(defun prj-create-file (filename)
  (let ((fp (generate-new-buffer filename)))
    (princ ";; -*- mode: Lisp; -*-\n\n" fp)
    fp))

(defun prj-close-file (fp)
  (with-current-buffer fp
    (condition-case nil
      (and t (write-region nil nil (buffer-name fp) nil 0))
      (error nil)
      ))
  (kill-buffer fp)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Save global project list and initial frame sizes

(defun prj-loadlist ()
  (prj-init)
  (load (prj-globalfile) t t)
  (setq prj-version eproject-version)
  )

(defun prj-get-frame-pos (f)
  (mapcar
   (lambda (parm) (cons parm (frame-parameter f parm)))
   '(top left width height)
   ))

(defun prj-savelist ()
  (let ((g (prj-globalfile)) fp)
    (unless (file-exists-p g)
      (make-directory (file-name-directory g) t)
      )
    (setq prj-last-open (car prj-current))
    (when (frame-live-p prj-initial-frame)
      (setq prj-frame-pos (prj-get-frame-pos prj-initial-frame))
      )
    (setq fp (prj-create-file g))
    (when fp
      (prj-print-list 'prj-version fp)
      (prj-print-list 'prj-list fp)
      (prj-print-list 'prj-last-open fp)
      (prj-print-list 'prj-frame-pos fp)
      (prj-close-file fp)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load/Save local per-project configuration file

(defun prj-update-config ()
  (let ((d (prj-get-directory prj-current))
        (e (prj-getconfig "exec-root"))
        )
    (if e (setq d (expand-file-name e d)))
    (setq prj-exec-directory (file-name-as-directory d))
    ))

(defun prj-get-directory (a)
  (file-name-as-directory (expand-file-name (cadr a)))
  )

(defun prj-get-cfg ()
  (expand-file-name (or (caddr prj-current) prj-default-cfg) prj-directory)
  )

(defun prj-get-buffer (a)
  (cond ((buffer-live-p (cdr a))
         (cdr a)
         )
        (prj-directory
         (get-file-buffer (expand-file-name (car a) prj-directory))
         )))

(defun prj-loadconfig (a)
  (let (lf e)
    (prj-reset)
    (setq prj-current a)
    (setq prj-directory (prj-get-directory a))
    (when (file-regular-p (setq lf (prj-get-cfg)))
      (load lf nil t)
      (setq prj-curfile
            (or (assoc prj-curfile prj-files)
                (car prj-files)
                ))
      )
    (if (setq e (prj-getconfig "project-name"))
        (setcar a e)
        (prj-setconfig "project-name" (car a))
        )
    (prj-update-config)
    (prj-set-functions prj-functions)
    (setq prj-version eproject-version)
    ))

(defun prj-saveconfig ()
  (when prj-current
    (let (w c b files)
      (prj-removehooks)
      (setq w (selected-window))
      (setq c (window-buffer w))
      (dolist (a prj-files)
        (setq b (prj-get-buffer a))
        (cond (b
               (set-window-buffer w b t)
               (with-current-buffer b
                 (let ((s (line-number-at-pos (window-start w)))
                       (p (line-number-at-pos (window-point w)))
                       )
                   (push (list (car a) s p) files)
                   )))
              ((consp (cdr a))
               (push a files)
               )
              (t
               (push (list (car a)) files)
               )))
      (set-window-buffer w c t)
      (prj-addhooks)
      (let ((fp (prj-create-file (prj-get-cfg)))
            (prj-curfile (car prj-curfile))
            (prj-files (nreverse files))
            )
        (when fp
          (prj-print-list 'prj-version fp)
          (prj-print-list 'prj-config fp)
          (prj-print-list 'prj-tools fp)
          (prj-print-list 'prj-files fp)
          (prj-print-list 'prj-curfile fp)
          (prj-print-list 'prj-functions fp)
          (prj-close-file fp)
          ))
      )))

(defun prj-saveall ()
  (prj-saveconfig)
  (prj-savelist)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The core functions:  Open / Close / Add / Remove  Project

(defun eproject-open (a)
  "Open another project."
  (interactive
   (list
    (or (prj-config-get-result 'p)
        (completing-read "Open Project: " (mapcar 'car prj-list))
        )))
  (unless (consp a)
    (let ((b (assoc a prj-list)))
      (unless b
        (error "No such project: %s" a)
        )
      (setq a b)
      ))
  (setq a (or (car (member a prj-list)) a))
  (unless (eq a prj-current)
    (unless (file-directory-p (prj-get-directory a))
      (error "No such directory: %s" (cadr a))
      )
    (setq prj-list (cons a (delq a prj-list)))
    (eproject-close)
    (prj-loadconfig a)
    )
  (prj-addhooks)
  (prj-setup-all)
  (prj-isearch-setup)
  (unless (prj-edit-file prj-curfile)
    (eproject-dired)
    ))

(defun eproject-close ()
  "Close the current project."
  (interactive)
  (when prj-current
    (prj-saveconfig)
    (prj-removehooks)
    (let (f)
      (unwind-protect
          (progn
            (save-some-buffers nil)
            (eproject-killbuffers t)
            (setq f t)
            )
        (or f (prj-addhooks))
        ))
    (prj-reset)
    (prj-config-reset)
    (prj-setup-all)
    (prj-isearch-setup)
    ))

(defun eproject-killbuffers (&optional from-project)
  "If called interactively kills all buffers that do not belong to project files"
  (interactive)
  (let (l b)
    (dolist (a prj-files)
      (setq b (prj-get-buffer a))
      (if b (setq l (cons (list b) l)))
      )
    (dolist (b (buffer-list))
      (when (eq (consp (assoc b l)) from-project)
        (kill-buffer b)
        ))))

(defun eproject-add (dir &optional name cfg)
  "Add a new or existing project to the list."
  (interactive
   (let (d n f)
    (setq d (read-directory-name "Add project in directory: " prj-directory nil t))
    (setq n (file-name-nondirectory (directory-file-name d)))
    (setq n (read-string "Project name: " n))
    (setq f (read-string "Project file: " prj-default-cfg))
    (list d n f)
    ))
  (when dir
    (setq dir (directory-file-name dir))
    (unless name 
      (setq name (file-name-nondirectory dir))
      )
    (when (and cfg (string-equal cfg prj-default-cfg))
      (setq cfg nil)
      )
    (let ((a (if cfg (list name dir cfg) (list name dir))))
      (push a prj-list)
      (eproject-open a)
      )))

(defun eproject-remove (a)
  "Remove a project from the list."
  (interactive
   (list
    (or (prj-config-get-result 'p)
        (completing-read "Remove project: " (mapcar 'car prj-list))
        )))
  (unless (consp a)
    (let ((b (assoc a prj-list)))
      (unless b
        (error "No such project: %s" a)
        )
      (setq a b)
      ))
  (when (progn
          (beep)
          (prog1 (y-or-n-p (format "Remove \"%s\"? " (car a)))
            (message "")
            ))
    (setq prj-list (prj-del-list prj-list a))
    (prj-setup-all)
    ))

(defun eproject-save ()
  "Save the project configuration to file."
  (interactive)
  (prj-config-parse)
  (prj-config-print)
  (prj-saveall)
  )

(defun eproject-revert ()
  "Reload the project configuration from file."
  (interactive)
  (prj-loadlist)
  (if prj-current
      (prj-loadconfig prj-current)
    )
  (prj-setup-all)
  )

(defun eproject-addfile (f)
  "Add a file to the current project."
  (interactive
   (and prj-current
        (list
         (read-file-name "Add file to project: " nil nil t nil)
         )))
  (unless prj-current (error "No project open"))
  (prj-insert-file f (prj-config-get-result 'f))
  (prj-config-print)
  (prj-setmenu)
  )

(defun eproject-removefile (a)
  "Remove a file from the current project."
  (interactive (prj-get-existing-file-1 "Remove file from project: "))
  (setq a (prj-get-existing-file-2 a))
  (prj-remove-file a)
  )

(defun eproject-visitfile (a)
  "Visit a file from the current project."
  (interactive (prj-get-existing-file-1 "Visit file: "))
  (setq a (prj-get-existing-file-2 a))
  (prj-edit-file a)
   )

(defun prj-get-existing-file-1 (msg)
  (and prj-current
       (list
        (or (prj-config-get-result 'f)
            (completing-read msg (mapcar 'car prj-files))
            ))))

(defun prj-get-existing-file-2 (a)
   (unless prj-current (error "No project open"))
   (if (consp a)
       a
     (let ((b (assoc (prj-relative-path a) prj-files)))
       (unless b (error "No such file in project: %s" a))
       b
       )))

(defun eproject-help ()
  "Show the eproject README."
  (interactive)
  (view-file (eproject-addon "eproject.txt"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook functions to track opening/closing files from emacs

(defun prj-addhooks ()
  (when prj-autotracking
    (add-hook 'kill-buffer-hook 'prj-kill-buffer-hook)
    (add-hook 'find-file-hook 'prj-find-file-hook)
    (add-hook 'window-configuration-change-hook 'prj-wcc-hook)
    ))

(defun prj-removehooks ()
  (remove-hook 'window-configuration-change-hook 'prj-wcc-hook)
  (remove-hook 'find-file-hook 'prj-find-file-hook)
  (remove-hook 'kill-buffer-hook 'prj-kill-buffer-hook)
  )

(defun prj-wcc-hook ()
  (dolist (w (window-list))
    (prj-register-buffer (window-buffer w))
    ))

(defun prj-find-file-hook ()
  (run-with-idle-timer 0.2 nil 'prj-wcc-hook)
  )

(defun prj-kill-buffer-hook ()
  (let ((b (current-buffer)) a)
    (if (setq a (rassq b prj-files))
        (prj-remove-file a t)
        (if (setq a (rassq b prj-removed-files))
            (setq prj-removed-files (delq a prj-removed-files))
          ))))

(defun prj-register-buffer (b)
  (let (f a)
    (setq f (buffer-file-name b))
    (when (and f t) ;;(not (string-match "^\\." (file-name-nondirectory f))))
      (setq a (rassq b prj-files))
      (unless a
        (setq a (prj-insert-file f nil t))
        (when a
          (prj-init-buffer a b)
          ))
      (when (and a (null (eq a prj-curfile)))
        (setq prj-curfile a)
        (prj-setmenu)
        ))
    a))

(defun prj-insert-file (f &optional after on-the-fly)
  (let ((r (prj-relative-path f)) a m)
    (setq a (assoc r prj-files))
    (unless (or a (and on-the-fly (assoc r prj-removed-files)))
      (setq a (list r))
      (setq m (memq (or after prj-curfile) prj-files))
      (if m
          (setcdr m (cons a (cdr m)))
          (setq prj-files (prj-add-list prj-files a))
          )
      (setq prj-removed-files (prj-del-list prj-removed-files a))
      (message "Added to project: %s" r)
      )
    a))

(defun prj-remove-file (a &optional on-the-fly)
  (let ((n (prj-otherfile prj-files a)) b)
    (setq prj-files (prj-del-list prj-files a))
    (when (eq prj-curfile a)
      (setq prj-curfile n)
      )
    (unless on-the-fly
        (setq prj-removed-files (prj-add-list prj-removed-files a))
        )
    (unless (prj-config-print)
      (prj-edit-file prj-curfile)
      )
    (prj-setmenu)
    (message "Removed from project: %s" (car a))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit another file

(defun prj-init-buffer (a b)
  (with-current-buffer b
    (when prj-rename-buffers
      (rename-buffer (car a) t)
      )
    (when prj-set-default-directory
      (cd prj-directory)
      ))
  (setcdr a b)
  )

(defun prj-find-file (a)
  (when a
    (let (b pos f)
      (setq b (prj-get-buffer a))
      (unless b
        (prj-removehooks)
        (setq f (expand-file-name (car a) prj-directory))
        (setq b (find-file-noselect f))
        (prj-addhooks)
        (when (and b (consp (cdr a)))
          (setq pos (cdr a))
          ))
      (when b
        (prj-init-buffer a b)
        (cons b pos)
        ))))

(defun prj-edit-file (a)
  (let ((f (prj-find-file a)))
    (when f
      (eproject-setup-quit)
      (switch-to-buffer (car f))
      (prj-restore-edit-pos (cdr f) (selected-window))
      (prj-setmenu)
      ;;(message "dir: %s" default-directory)
      )
    (setq prj-curfile a)
    ))

(defun prj-restore-edit-pos (pos w)
  (let ((top (car pos)) (line (cadr pos)))
    (when (and (numberp top) (numberp line))
      (prj-goto-line top)
      (set-window-start w (point))
      (prj-goto-line line)
      )))

(defun prj-select-window (w)
  (let (focus-follows-mouse)
    (select-window w)
    (select-frame-set-input-focus (window-frame w))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choose next/previous file

(defun eproject-nextfile ()
  "Switch to the next file that belongs to the current project."
  (interactive)
  (prj-switch-file 'prj-next-file 'next-buffer)
  )

(defun eproject-prevfile ()
  "Switch to the previous file that belongs to the current project."
  (interactive)
  (prj-switch-file 'prj-prev-file 'previous-buffer)
  )

(defun prj-switch-file (fn1 fn2)
  (let ((a (rassoc (current-buffer) prj-files)))
    (cond (a
           (prj-edit-file (or (funcall fn1 prj-files a) a))
           )
          (prj-curfile
           (prj-edit-file prj-curfile)
           )
          (t
           (funcall fn2)
           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set key shortcuts

(defun prj-setkeys ()
  (let ((f (consp prj-current))
        (a (assoc 'eproject-mode minor-mode-map-alist))
        (map (make-sparse-keymap))
        )
    (if a
        (setcdr a map)
        (push (cons 'eproject-mode map) minor-mode-map-alist)
        )
    (dolist (k prj-keybindings)
      (when (or f (eq (caddr k) 'always))
        (define-key map (car k) (cadr k))
        ))

    (when f
      (let ((n 0) fn s)
        (dolist (a prj-tools)
          (unless (setq fn (nth n prj-tools-fns))
            (setq fn (list 'lambda))
            (setq prj-tools-fns (nconc prj-tools-fns (list fn)))
            )
          (setcdr fn `(() (interactive) (prj-run-tool ',a)))
          (setq n (1+ n))
          (when (setq s (caddr a))
            (define-key map (prj-parse-key s) (and f fn))
            ))))))

(defun prj-parse-key (s)
  (read
   (if (string-match "[a-z][a-z0-9]+$" s)
       (concat "[" s "]")
       (concat "\"\\" s "\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set menus

(defun prj-list-sorted ()
  (sort (append prj-list nil)
        #'(lambda (a b) (string-lessp (car a) (car b)))
        ))

(defun prj-setmenu ()
  (let ((f (consp prj-current)) m1 m2 m3)

    (setq m1
          `(("Open" open ,@(prj-menulist-maker prj-list prj-current 'prj-menu-open))
            ("Add/Remove" other
             ("Add ..." "Add new or existing project to the list" . eproject-add)
             ("Remove ..." "Remove project from the list" . eproject-remove)
             ,@(and f '(("Close" "Close current project" . eproject-close)))
             ("--")
             ("Setup" "Enter the project setup area." . eproject-setup-toggle)
             ("Help" "View eproject.txt" . eproject-help)
             )
            ))
    (when f
      (nconc m1 (cons '("--") (prj-menulist-maker prj-tools nil prj-tools-fns)))
      (setq m2
            `(("Dired" "Browse project directory in Dired - Use 'a' to add file(s) to the project" . eproject-dired)
              ("--")
              ,@(prj-menulist-maker prj-files prj-curfile 'prj-menu-edit)
              )))

    (prj-menu-maker
     global-map
     `((buffer "Project" project ,@m1)
       (file "List" list ,@m2)
       )
     '(menu-bar)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-menu-edit ()
  (interactive)
  (let ((a (nth last-command-event prj-files)))
    (if a (prj-edit-file a))
    ))

(defun prj-menu-open ()
  (interactive)
  (let ((a (nth last-command-event prj-list)))
    (if a (eproject-open (car a)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-menu-maker (map l v)
  (let ((e (list nil)))
    (setq v (append v e))
    (dolist (k (reverse l))
      (let (s a)
        (when (symbolp (car k))
          (setq a (pop k))
          )
        (cond
         ((numberp (car k))
          (setcar e (pop k))
          )
         ((and (consp (cdr k)) (symbolp (cadr k)))
          (setcar e (cadr k))
          (setq s (cddr k))
          (setq k (and s (cons (car k) (make-sparse-keymap (car k)))))
          )
         (t
          (setcar e (intern (downcase (car k))))
          ))
        (if a
            (define-key-after map (vconcat v) k a)
            (define-key map (vconcat v) k)
            )
        (if s (prj-menu-maker map s v))
        ))))

(defun prj-copy-head (l n)
  (let (r)
    (while (and l (> n 0))
      (push (pop l) r)
      (setq n (1- n))
      )
    (nreverse r)
    ))

(defun prj-split-list (l n)
  (let (r)
    (while l
      (push (prj-copy-head l n) r)
      (setq l (nthcdr n l))
      )
    (nreverse r)
    ))

(defun prj-menulist-maker (l act fns)
  (let (r (w 30) s (m 0) (n 0) k)
    (cond
     ((< (length l) w)
      (prj-menulist-maker-1 (list l fns n) act)
      )
     (t
      ;; menu too long; split into submenus
      (setq s (prj-split-list l w))
      (setq k (prj-menulist-maker-1 (list (append (pop s) '(("--"))) fns n) act))
      (setq r (nreverse k))
      (dolist (l s)
        (when (consp fns)
          (setq fns (nthcdr w fns))
          )
        (setq n (+ n w))
        (setq k (prj-menulist-maker-1 (list l fns n) act))
        (push (cons (concat (prj-shortname (caar l)) " ...")
                    (cons (intern (format "m_%d" (setq m (1+ m))))
                          k)) r)
        )
      (nreverse r)
      ))))

(defun prj-menulist-maker-1 (l act)
  (let (r e f s i n a)
    (while (car l)
      (setq a (caar l))
      (setcar l (cdar l))
      (setq n (caddr l))
      (setcar (cddr l) (1+ n))
      (setq f (if (consp (cadr l))
                  (prog1 (car (cadr l)) (setcar (cdr l) (cdr (cadr l))))
                  (cadr l)))

      (setq i (car a))
      (unless (string-match "^ *#" i)
        (setq s (if (and (consp (cdr a)) (stringp (cadr a))) (cadr a) i))
        (cond ((equal ">" i)
               (setq e (cons s (cons (intern s) (prj-menulist-maker-1 l act))))
               (setq r (cons e r))
               )
              ((equal "<" i)
               (setq l nil)
               )
              (t
               (setq i (prj-shortname i))
               (setq e (cons n (if (eq a act)
                                   `(menu-item ,i ,f :button (:toggle . t) :help ,s)
                                 (cons i (cons s f)))))
               (setq r (cons e r))
               )))
      )
    (nreverse r)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run make and other commands

(defun prj-compilation-in-frame (cmd)
  (let ((bn "*compilation*") w h b c f)
    (unless (get-buffer-window bn t)
      (setq b (get-buffer-create bn))
      (setq f (frame-list))
      (cond ((cdr f)
             (setq w (frame-first-window (car f)))
             (delete-other-windows w)
             )
            (t
             (setq h (/ (* 70 (frame-height)) 100))
             (delete-other-windows w)
             (setq w (split-window w h))
             ))
      (set-window-buffer w b)
      ))
  (let ((display-buffer-reuse-frames t) (f (selected-frame)))
    (compile cmd)
    (select-frame-set-input-focus f)
    ))


(defun prj-option-in (arg fun)
  (let ((old-dir default-directory))
    (cd arg)
    (save-excursion (funcall fun))
    (cd old-dir)))

(defun prj-option-virt (arg fun)
  (virtualenv-activate arg)
  (funcall fun)
  (virtualenv-deactivate))

(defun prj-option-workon (arg fun)
  (virtualenv-workon arg)
  (funcall fun)
  (virtualenv-deactivate))

;;(devar prj-run-options '(("in" prj-option-in) ("virt" prj-option-virt) ("workon" prj-option-workon)))

;; Defining prj-run-new
;; special case of the -e
;; wrap all in a cd new dir cd old dir
;; TODO: for each of the option
;; - match the option
;; - apply partially the option to the context
;; - save the resulting context in a list
;;
;; then
;; apply the multiple contexts to the command
(defun prj-run-new (cmd)
  (dolist (opt prj-run-options)
    (if string-match (concat "^" (first opt) cmd)
      (add-to-list contexts )))

  )

(defun prj-run (cmd)
  (cond ((string-match "^-e +" cmd)
         (setq cmd (read (substring cmd (match-end 0))))
         (unless (commandp cmd)
           (setq cmd `(lambda () (interactive) ,cmd))
           )
         (command-execute cmd)
         )
        ((let ((b (current-buffer)) 
               (old-dir default-directory) 
               (new-dir (or prj-directory "."))
               )

           ;; There is some code duplication but I wasn't able to find
           ;; a way to implement a sort of 'context manager' in lisp
           (when (string-match "^-in +\\([^[:space:]]+\\) +" cmd)
             (setq new-dir (match-string-no-properties 1 cmd))
             (setq cmd (substring cmd (match-end 0)))
             )
           
           ;; Virt command
           (when (string-match "^-virt +\\([^[:space:]]+\\) +" cmd)
             (save-match-data
               (cd new-dir)
               (virtualenv-activate (match-string-no-properties 1 cmd))
               (cd old-dir))
             (setq cmd (substring cmd (match-end 0)))
             )
           ;; Workon command
           (when (string-match "^-workon +\\([^[:space:]]+\\) +" cmd)
             (save-match-data
               (cd new-dir)
               (virtualenv-workon (match-string-no-properties 1 cmd))
               (cd old-dir))
             (setq cmd (substring cmd (match-end 0)))
             )
           
           (when prj-exec-directory
             (setq new-dir (expand-file-name new-dir prj-exec-directory))
             )
           (cd new-dir)

           
           (cond ((string-match "\\(.+\\)& *$" cmd)
                  (start-process-shell-command 
                   "eproject-async" nil (match-string 1 cmd))
                  (message (match-string 1 cmd))
                  )
                 (prj-set-compilation-frame
                  (prj-compilation-in-frame cmd)
                  )
                 (t
                  (compile cmd)
                  ))

           (with-current-buffer b (cd old-dir))

           ))))
      
(defun prj-run-tool (a)
  (unless (string-match "^--+$" (car a))
    (prj-run (or (cadr a) (car a)))
    ))

(defun eproject-killtool ()
  (interactive)
  (let ((bn "*compilation*") w0 w1)
    (when (setq w1 (get-buffer-window bn t))
      (when (fboundp 'kill-compilation)
        (setq w0 (selected-window))
        (select-window w1)
        (kill-compilation)
        (select-window w0)
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run grep on project files

(defun eproject-grep (command-args)
  "Run the grep command on all the project files."
  (interactive
   (progn
     (require 'grep)
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer
              "Run grep on project files: "
              (if current-prefix-arg default grep-command)
              nil
              nil
              'grep-history
              (if current-prefix-arg nil default)
              )))))
  (let ((b (current-buffer)) (old-dir default-directory))
    (dolist (f (mapcar 'car prj-files))
      (setq command-args (concat command-args " " f))
      )
    (when prj-directory (cd prj-directory))
    (grep command-args)
    (with-current-buffer b (cd old-dir))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add files to the project with dired

(require 'dired)

(defun prj-dired-addfiles ()
  (interactive)
  (when prj-current
    (let ((n 0) a)
      (dolist (f (dired-get-marked-files))
        (setq a (prj-insert-file f))
        (unless (cdr a)
          (setq n (1+ n))
          (setq prj-curfile a)
          ))
      (if (> n 1) (message "Added to project: %d file(s)" n))
      (prj-setmenu)
      )))

(defun eproject-dired ()
  "Start a dired window with the project directory."
  (interactive)
  (when prj-directory
    (eproject-setup-quit)
    ;;(message "Use 'a' to add marked or single files to the project.")
    (dired prj-directory)
    (let ((map dired-mode-map))
      (define-key map "a" 'prj-dired-addfiles)
      (define-key map [menu-bar operate command] '("Add to Project"
        "Add current or marked file(s) to project" . prj-dired-addfiles))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-setup-all ()
  (prj-setkeys)
  (prj-setmenu)
  (prj-settitle)
  (prj-config-print)
)

(defun prj-getconfig (n)
  (let ((a (cdr (assoc n prj-config))))
    (and (stringp a) a)
    ))

(defun prj-setconfig (n v)
  (let ((a (assoc n prj-config)))
    (unless a
      (setq a (list n))
      (setq prj-config (nconc prj-config (list a)))
      )
    (setcdr a v)
    ))

(defun prj-on-kill ()
  (prj-saveall)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isearch in all project files

(defun prj-isearch-function (b wrap)
  (let (a)
    (or b (setq b (current-buffer)))
    (cond (wrap
           (if isearch-forward
               (setq a (car prj-files))
               (setq a (car (last prj-files)))
               ))
          ((setq a (rassoc b prj-files))
           (if isearch-forward
               (setq a (prj-next-file prj-files a))
               (setq a (prj-prev-file prj-files a))
               )
            ))
    (car (prj-find-file a))
    ;; (print `(prj-isearch (wrap . ,wrap) ,b ,d) (get-buffer "*Messages*"))
    ))

(defun prj-isearch-setup ()
  (cond ((and prj-set-multi-isearch prj-current)
         (setq multi-isearch-next-buffer-function 'prj-isearch-function)
         (setq multi-isearch-pause 'initial)
         (add-hook 'isearch-mode-hook 'multi-isearch-setup)
         )
        (t
         (remove-hook 'isearch-mode-hook 'multi-isearch-setup)
         )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize

(defun prj-startup-delayed ()
  ;; load UI support
  (load (eproject-addon "eproject-config") nil t)

  ;; When no projects are specified yet, load the eproject project itself.
  (unless prj-list
    (load (eproject-addon prj-default-cfg))
    )

  ;; no project so far
  (prj-reset)
  (prj-setup-all)
  (add-hook 'kill-emacs-hook 'prj-on-kill)

  ;; inhibit open last project when a file was on the commandline
  (unless (buffer-file-name (window-buffer))
    (when prj-last-open

      ;; open last project
      (eproject-open prj-last-open)

      ;; emacs bug: deferred jit-lock is dropped if run from idle timer
      (and jit-lock-mode jit-lock-defer-time (jit-lock-function (point)))

      ;; restore frame position
      (when (and prj-set-framepos prj-frame-pos prj-initial-frame)
        (modify-frame-parameters prj-initial-frame prj-frame-pos)
        ;; emacs bug: when it's too busy it doesn't set frames correctly.
        (sit-for 0.2)
        ))))

(defun prj-command-line-switch (option)
  (setq prj-last-open (pop argv))
  (setq inhibit-startup-screen t)
  )

(defun eproject-startup ()
  ;; where is this file
  (if load-file-name
      (setq eproject-directory (file-name-directory load-file-name)))
  (if (boundp 'prj-list)
    (progn
      (load (eproject-addon "eproject-config"))
      (prj-setup-all))
    (progn
      (prj-loadlist)
      (when prj-last-open (setq inhibit-startup-screen t))
      (when (display-graphic-p) (setq prj-initial-frame (selected-frame)))
      (push '("project" . prj-command-line-switch) command-switch-alist)
      (run-with-idle-timer 0.1 nil 'prj-startup-delayed)
      )))

;;;###autoload(require 'eproject)
(provide 'eproject)
(eproject-startup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eproject.el ends here
