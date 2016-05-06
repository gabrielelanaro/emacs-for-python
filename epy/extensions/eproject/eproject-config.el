;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; eproject-config.el --- project workspaces for emacs --- UI part
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

;; buffer
(defvar prj-buffer nil)
;; keymap
(defvar prj-browse-map nil)
;; overlays
(defvar prj-hilight-bar nil)
(defvar prj-hilight-bar-2 nil)
;; flag
(defvar prj-edit-mode nil)

;; tabs
(defvar prj-groups)
(defvar prj-active-group nil)
(defvar prj-group-top nil)
(defvar prj-group-left nil)
(defvar prj-group-tab nil)

;; tab menus
(defvar prj-links)

;; quick search
(defvar prj-qs-face nil)
(defvar prj-qs-str nil)
(defvar prj-qs-len nil)
(defvar prj-qs-pos nil)

;; from eproject.el
(defvar prj-list)
(defvar prj-current)
(defvar prj-files)
(defvar prj-curfile)
(defvar prj-config)
(defvar prj-tools)
;; also
(declare-function prj-setconfig "eproject")
(declare-function prj-getconfig "eproject")
(declare-function prj-setup-all "eproject")
(declare-function prj-remove-file "eproject")
(declare-function caddr "eproject")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro p-get (e)
  `(plist-get ,(car e) ',(cdr e))
  )
(defmacro p-set (e v)
  `(plist-put ,(car e) ',(cdr e) ,v)
  )
(defmacro p-call (e &rest args)
  `(funcall (plist-get ,(car e) ',(cdr e)) ,@args)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show/Hide the *eproject* buffer

(defun eproject-setup ()
  "Show the configuration buffer."
  (interactive)
  (let ((map (make-keymap)))

    (substitute-key-definition
      'self-insert-command
      'prj-qsearch
      map
      global-map
      )

    (dolist (k '(
        ("\t" . prj-next-button)
        ([tab] . prj-next-button)
        ("\e\t" . prj-prev-button)
        ([S-tab] . prj-prev-button)
        ([backtab] . prj-prev-button)

        ([left] . prj-move-left)
        ([right] . prj-move-right)
        ([backspace] . prj-qsearch)
        ([delete] . prj-qsearch)
        ([127] . prj-qsearch)
        ([return] . prj-enter)

        ([32] . eproject-edit)
        ([escape] . eproject-setup-quit)

        ([down-mouse-1] . prj-mouse)
        ([down-mouse-2] . prj-mouse)
        ([mouse-1] . prj-mouse)
        ([mouse-2] . prj-mouse)
        ([mouse-3] . ignore)
        ([drag-mouse-1] . ignore)
        ))
      (define-key map (car k) (cdr k))
      )

    (cond ((buffer-live-p prj-buffer)
           (switch-to-buffer prj-buffer)
           )
          (t
           (unless prj-buffer
             (add-hook 'post-command-hook 'prj-post-command-hook)
             )
           (prj-config-init)
           (setq prj-buffer (get-buffer-create "*eproject*"))
           (switch-to-buffer prj-buffer)
           ))

    (setq prj-browse-map map)
    (prj-qs-clear)
    (unless prj-edit-mode
      (use-local-map map)
      (prj-config-print)
      )
    ))

(defun eproject-setup-quit ()
  "Kill the configuration buffer."
  (interactive)
  (let ((alive (buffer-live-p prj-buffer)))
    (cond ((and alive prj-edit-mode)
           (bury-buffer prj-buffer)
           )
          (t
           (when alive
             (kill-buffer prj-buffer)
             )
           (remove-hook 'post-command-hook 'prj-post-command-hook)
           (setq prj-buffer nil)
           ))))

(defun eproject-setup-toggle ()
  "Show/hide the project configuration browser."
  (interactive)
  (if (prj-config-active)
      (eproject-setup-quit)
    (eproject-setup)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edit mode

(defun eproject-edit ()
  (interactive)
  (if (eq 'u (car prj-active-group)) (emacs-lisp-mode))
  (let ((map (make-sparse-keymap)))
    (define-key map [escape] 'eproject-edit-quit)
    (setq prj-edit-mode t)
    (prj-qs-clear)
    (use-local-map map)
    (prj-config-print)
    ))

(defun eproject-edit-quit ()
  (interactive)
  (if (eq 'u (car prj-active-group)) (fundamental-mode))
  (prj-config-parse)
  (use-local-map prj-browse-map)
  (setq prj-edit-mode nil)
  (setq cursor-type nil)
  (prj-set-hilite-bar)
  (prj-setup-all)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prj-config-active ()
  (eq (current-buffer) prj-buffer)
  )

(defun prj-save-window-pos ()
  (p-set (prj-active-group . :pos)
     (list
      (window-start)
      (- (line-number-at-pos) prj-group-top)
      )))

(defun prj-config-reset ()
  (dolist (s prj-groups)
    (p-set (s . :pos) (list 1 0))
    )
  (setq prj-active-group (car prj-groups))
  )

(defun prj-config-init ()
  (dolist (v '(
      prj-buffer
      prj-browse-map
      prj-hilight-bar
      prj-hilight-bar-2
      prj-edit-mode
      ))
    (set v nil)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read back the configuration after edits

(defun prj-config-parse ()
  (when (and (prj-config-active) prj-edit-mode)
    (with-current-buffer prj-buffer
      (save-excursion
        (let ((s (p-get (prj-active-group . :scan))) l r e)
          (prj-goto-line prj-group-top)
          (if (eq 'u (car prj-active-group))
              (setq l (read (concat
                             "(("
                             (buffer-substring-no-properties (point) (point-max))
                             "))")))
              (progn
                (while (< (point) (point-max))
                  (setq e (line-end-position))
                  (setq r
                    (if (and s (posix-search-forward (car s) e t))
                        (apply (cdr s) nil)
                        (and (re-search-forward "^ *\\(.*[^ :]\\)[ :]*$" e t)
                             (list (match-string-no-properties 1))
                             )))
                  (if r (setq l (cons r l)))
                  (forward-line 1)
                  )
                (setq l (nreverse l))
                ))
          (p-call (prj-active-group . :parse) l)
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The project config window

;; (makunbound 'prj-groups) (makunbound 'prj-links)

(defvar prj-groups `(

   (p nil
      :title "Projects"
      :comment "All projects on a list"
      :pos (1 0)
      :list prj-list
      :exec eproject-open
      :print ,(lambda (a p)
               (prj-link (car a) nil a)
               (prj-link-2 nil p (cadr a))
               (and (caddr a) (prj-link-2 nil p (caddr a)))
               )
      :scan ("^ *\\([^ :]+\\) *: *\\([^ ]+\\) *\\( +: *\\([^ ]+\\)\\)? *$" .
              ,(lambda ()
               (let ((a (match-string-no-properties 1))
                     (b (match-string-no-properties 2))
                     (c (match-string-no-properties 4))
                     )
                (cons a (cons b (and c (list c))))
                )))
      :parse ,(lambda (s)
               (dolist (a s)
                 (unless (cadr a)
                   (error "Error: Project directory empty: %s" (car a))
                   )
                 (when prj-current
                   (when (string-equal (cadr a) (cadr prj-current))
                     (setq prj-current a)
                     (prj-setconfig "project-name" (car a))
                     )))
               (setq prj-list s)
               )
      :menu (add remove open close)
      )

   (f nil
      :title "Files"
      :comment "The files that belong to the project"
      :pos (1 0)
      :list prj-files
      :exec eproject-visitfile
      :print ,(lambda (a p)
               (prj-link (car a) nil a)
               )
      :scan nil
      :parse ,(lambda (s)
                (let (b)
                  (dolist (l s)
                    (setcdr l (cdr (assoc (car l) prj-files)))
                    )
                  (dolist (a prj-files)
                    (if (setq b (assoc (car a) s))
                        (if (eq a prj-curfile) (setq prj-curfile b))
                        (prj-remove-file a)
                        ))
                  (setq prj-files s)
                  ))
      :menu (add-file remove-file visit-file)
      )

   (t nil
      :title "Tools"
      :comment "Configurable tools and keyboard shortcuts"
      :pos (1 0)
      :list prj-tools
      :exec prj-run-tool
      :print ,(lambda (a p)
               (prj-link (car a) nil a)
               (when (caddr a)
                 (unless prj-edit-mode
                   (insert-char 32 (- (- prj-group-tab 12) (- (point) p)))
                   )
                 (insert " (" (caddr a) ")")
                 )
               (prj-link-2 nil p (cadr a))
               )
      :scan ("^ *\\([^(:]*[^(: ]\\) *\\(([^ ):]+)\\)?\\( *: *\\(.*[^ ]\\)?\\)? *$" .
              ,(lambda ()
                 (let ((a (match-string-no-properties 1))
                       (b (match-string-no-properties 2))
                       (c (match-string-no-properties 4))
                       )
                   (list a c (and b (substring b 1 -1)))
                   )))
      :parse ,(lambda (s)
               (setq prj-tools s)
               )
      :menu ()
      )

   (s nil
      :title "Settings"
      :comment "Project options"
      :pos (1 0)
      :list prj-config
      :exec eproject-edit
      :print ,(lambda (a p)
               (prj-link-2 (car a) p (cdr a))
               )
      :scan ("^ *\\([^ :]+\\) *: *\\(.*[^ ]\\)? *$" .
              ,(lambda ()
                 (list (match-string-no-properties 1)
                       (match-string-no-properties 2)
                       )))
      :parse ,(lambda (s)
               (dolist (l s) (setcdr l (cadr l)))
               (let ((prj-config s) n)
                 (setq n (prj-getconfig "project-name"))
                 (unless (> (length n) 0)
                   (error "Error: Project name empty.")
                   )
                 (when prj-current
                   (setcar prj-current n)
                   ))
               (setq prj-config s)
               (prj-update-config)
               )
      :menu ()
      )

;;;    (u nil
;;;       :title "Functions"
;;;       :comment "ELisP Utitlities"
;;;       :pos (1 0)
;;;       :list prj-functions
;;;       :exec eproject-edit
;;;       :print ,(lambda (a p)
;;;                (pp a (current-buffer))
;;;                )
;;;       :parse ,(lambda (s)
;;;                 (prj-set-functions s)
;;;                )
;;;       :menu ()
;;;       )
   ))


(defvar prj-links '(

   ;; projects
   (add "Add" "Add new or existing project to the list"
         eproject-add
         )
   (remove "Remove" "Remove a project from the the list"
         eproject-remove
         )
   (open "Open" "Open a Project"
         eproject-open
         )
   (close "Close" "Close the current project"
         eproject-close
         )

   ;; files
   (add-file "Add" "Add a file to the project"
         eproject-addfile
         )
   (remove-file "Remove" "Remove file from project"
         eproject-removefile
         )
   (dired "Dired" "Browse project directory - Use 'a' in dired to add file(s) to the project"
         eproject-dired
         )
   (visit-file "Visit" "Visit this file"
         eproject-visitfile
         )

   ;; edit mode
   (edit "Edit" "Edit this list (spacebar)"
         eproject-edit
         )
   (quit-edit "Quit" "Quit edit mode (escape)"
         eproject-edit-quit
         )
   (revert "Revert" "Revert all configuration to last saved state"
         eproject-revert
         )
   (save "Save" "Save the configuration now"
         eproject-save
         )

   ;; other
   (help "Help" "View the 'eproject' documentation."
         eproject-help
         )
   (quit "Quit" "Quit configuration area"
      eproject-setup-quit
      )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print the config

(defun prj-config-print ()
  (when (prj-config-active)
    (let (x f a n title l p (inhibit-read-only t) active)

      (setq buffer-read-only nil)
      (buffer-disable-undo)
      (erase-buffer)

      (setq prj-group-left (if prj-edit-mode 0 1))
      (setq prj-group-tab (+ 26 prj-group-left))
      (setq active
            (or prj-active-group
                (setq prj-active-group (car prj-groups))
                ))
      (insert "\n")
      (setq n 1)
      (dolist (s prj-groups)
        (setq f (eq s active))
        (when (or f (and prj-current (null prj-edit-mode)))
          (setq title (p-get (s . :title)))
          (insert-char 32 n)
          (cond (f
                 (setq p (point))
                 (insert title)
                 (prj-make-hilite-bar 'prj-hilight-bar-2 p (point))
                 )
                (t
                 (prj-link title (p-get (s . :comment)) s t)
                 ))
          (setq n 2)
          ))

      (dolist (s prj-links)
        (prj-define-shortcut nil (cadr s) 'ignore)
        )
      (dolist (s prj-groups)
        (prj-define-shortcut nil (symbol-name (car s)) 'prj-key-set-group)
        )
      (insert "  -")
      (dolist (id (if prj-edit-mode '(revert save quit-edit) '(edit help quit)))
         (insert "  ")
         (prj-link-3 id nil)
         )
      (insert "\n\n  -")
      (when prj-current
        (insert " " (car prj-current) " ")
        )
      (insert "-")
      (unless prj-edit-mode
        (dolist (id (p-get (active . :menu)))
          (insert "  ")
          (prj-link-3 id nil)
          )
        )
      (insert "\n\n")

      (when prj-edit-mode
        (add-text-properties (point-min) (point)
           '(read-only t intangible t front-sticky t rear-nonsticky t))
        )

      (setq prj-group-top (line-number-at-pos))

      (prj-print-items
        (p-get (active . :print))
        (eval (p-get (active . :list)))
        prj-group-left
       )

      (setq p (p-get (active . :pos)))
      (set-window-start (get-buffer-window prj-buffer) (car p))
      (prj-goto-line (+ prj-group-top (cadr p)))
      (unless (eobp)
        (forward-char prj-group-left)
        )
      (unless (pos-visible-in-window-p)
        (recenter (/ (window-height) 5))
        )
      (set-buffer-modified-p nil)
      (cond (prj-edit-mode
             (buffer-enable-undo)
             (setq cursor-type 'box)
             )
            (t
             (prj-set-hilite-bar)
             (setq buffer-read-only t)
             (setq cursor-type nil)
             ))
      t
      )))

(defun prj-print-items (fn items tab)
  (dolist (a items)
    (when (stringp (car a))
      (unless (and (string-match "^ *#" (car a)) (null prj-edit-mode))
        (insert-char 32 tab)
        ))
    (funcall fn a (- (point) tab))
    (insert "\n")
    ))

(defun prj-link (text help &optional fn top)
  (if (and prj-edit-mode (null help))
      (insert text)
      (let ((p (point)) (f (if top 'link)))
        (insert-text-button
         text
         'help-echo help
         'action 'prj-action
         'class (or fn 'link)
         'follow-link t
         'face f
         'mouse-face 'link
         )
        (when (or f help)
          (add-text-properties p (1+ p) '(face (:foreground "blue" :underline t)))
          )
        )))

(defun prj-link-2 (a p b)
  (if a (insert a))
  (insert-char 32 (- prj-group-tab 1 (- (point) p)))
  (if b (insert " : " b) (insert " :"))
  )

(defun prj-link-3 (id f)
  (let ((a (assq id prj-links)))
    (when a
      (prj-link (cadr a) (caddr a) a f)
      (prj-define-shortcut nil (cadr a) (nth 3 a))
    )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project selection and configuration

(defun prj-action (b)
  (let ((a (button-get b 'class)))
    (cond ((memq a prj-links)
           (command-execute (nth 3 a))
           )
          ((memq a prj-groups)
           (setq prj-active-group a)
           (prj-config-print)
           )
          (t
           (p-call (prj-active-group . :exec) a)
           ))))

(defun prj-key-set-group ()
  (interactive)
  (let ((c (intern (char-to-string (logand last-input-event 255)))) s)
      (when (setq s (assoc c prj-groups))
        (setq prj-active-group s)
        (prj-config-print)
        )))

(defun prj-define-shortcut (map s fn)
  (let ((c (logior (aref s 0) 32)))
    (define-key
      (or map (current-local-map))
      (read (format "\"\\M-%c\"" c))
      fn
      )))

(defun prj-config-get-result (id)
  (and (prj-config-active)
       (eq id (car prj-active-group))
       (nth (cadr (p-get (prj-active-group . :pos)))
            (eval (p-get (prj-active-group . :list)))
            )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab between buttons and move files up/down

(defun prj-next-button ()
  (interactive)
  (if prj-qs-pos
      (prj-qs-next 1)
    ))

(defun prj-prev-button ()
  (interactive)
  (if prj-qs-pos
      (prj-qs-next -1)
  ))

(defun prj-move-left ()
  (interactive)
  (prj-move-to -1)
  )

(defun prj-move-right ()
  (interactive)
  (prj-move-to 1)
  )

(defun prj-move-to (d &optional cycle)
  (let ((n 0) (x 0))
    (dolist (s prj-groups)
      (if (eq s prj-active-group)
          (setq x n))
      (setq n (1+ n))
      )
    (setq x (+ x d))
    (unless prj-current (setq n 1))
    (if cycle
        (if (< x 0) (setq x (1- n)) (if (>= x n) (setq x 0)))
        (setq x (max 0 (min (1- n) x)))
        )
    (setq prj-active-group (nth x prj-groups))
    (prj-config-print)
    ))

(defun prj-enter ()
  (interactive)
  (let (a b)
    (and (setq b (button-at (point)))
         (setq a (button-get b 'action))
         (funcall a b)
         )))

(defun prj-mouse ()
  (interactive)
  ;;(message "LC: %s" (prin1-to-string last-input-event))
  (let ((i last-input-event) p b a x y tp)
    (when (consp i)
      (select-window (car (cadr i)))
      (setq p (nth 5 (cadr i)))
      (setq tp (nth 6 (cadr i)))
      (setq y (+ (cdr tp) (line-number-at-pos (window-start))))
      (setq x (+ (car tp) 1))
      (if (>= y prj-group-top)
          (prj-goto-line y)
        )
      (and (memq (car i) '(mouse-1 mouse-2))
           (setq b (button-at p))
           (setq a (button-get b 'action))
           (funcall a b)
           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A hook to maintain the selection bar

(defun prj-post-command-hook ()
  (and
   (prj-config-active)
   (prj-set-hilite-bar)
   ))

(defun prj-set-hilite-bar ()
  (unless prj-edit-mode
    ;;(message "LC: %s" (prin1-to-string (cons this-command last-input-event)))
    (let (n m a c e p)
      (setq m (length (eval (p-get (prj-active-group . :list)))))
      (setq p (line-number-at-pos))
      (setq n (max prj-group-top
                   (min (line-number-at-pos)
                        (1- (+ prj-group-top m))
                        )))
      (prj-goto-line n)
      (if (< p n)
          (set-window-start nil (point-min))
        )
      (unless (eobp)
        (setq a (point))
        (forward-char prj-group-left)
        (setq e (line-end-position))
        (when (< (setq c (+ a prj-group-tab)) e)
          (save-excursion
            (if (re-search-forward " *:" e t)
                (setq e (1- (match-end 0)))
              )))
        (while (= (char-after) 32)
          (forward-char 1)
          )
        (prj-make-hilite-bar 'prj-hilight-bar (point) e)
        (prj-save-window-pos)
        ))))

(defun prj-make-hilite-bar (s a e)
  (let (b)
    (if (and (boundp s) (setq b (eval s)))
        (move-overlay b a e)
        (overlay-put
           (set s (make-overlay a e))
           'face '(:background "grey90" :foreground "blue")
           ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick Search

(defun prj-qsearch ()
  (interactive)
  (setq prj-qs-str
        (cond ((member last-command-event '(backspace 127))
               (substring prj-qs-str 0 (max 0 (1- (length prj-qs-str))))
               )
              ((eq last-command-event 'delete)
               ""
               )
              (t
               (concat prj-qs-str (char-to-string last-command-event))
               )))
  (prj-qs-next 0)
  )

(defun prj-qs-clear ()
  (when prj-qs-face
    (delete-overlay prj-qs-face)
    )
  (setq prj-qs-face nil)
  (setq prj-qs-pos nil)
  (setq prj-qs-str "")
  (setq prj-qs-len 0)
  )

(defun prj-qs-find (s f p)
  (save-excursion
    (let (r fn beg end start limit)
      (setq s (concat
               "^[[:space:]]*\\([^[:space:]]*[/\\]\\)?\\("
               (regexp-quote s)
               "\\)[^/\\[:space:]]*\\([[:space:]]\\|$\\)"
               ))

      (prj-goto-line prj-group-top)
      (setq beg (point))
      (setq end (point-max))
      (goto-char (max p beg))

      (if (>= f 0)
          (setq fn 're-search-forward
                start beg
                limit end
                )
          (setq fn 're-search-backward
                start end
                limit beg
                ))

      (catch 'loop
        (while t
          (beginning-of-line (max 1 (1+ f)))
          (cond ((funcall fn s limit t)
                 (throw 'loop (match-beginning 2))
                 )
                (r
                 (throw 'loop nil)
                 )
                ((setq r t)
                 (goto-char start)
                 )))))))

(defun prj-qs-next (f)
  (let (k l p a e n s)
    (setq p prj-qs-pos)
    (setq l prj-qs-len)
    (setq s prj-qs-str)
    (prj-qs-clear)

    (setq k (length s))
    (if (= k 0)
        (setq l k)
        (progn
          (if (setq n (prj-qs-find s f (or p (point))))
              (setq p n l k)
              (setq s (substring s 0 l))
              )
          (message "Quick search: %s" s)
          ))

    (when p
      (goto-char (+ p l))
      (prj-set-hilite-bar)
      (when (> l 0)
        (setq prj-qs-face (make-overlay p (+ p l)))
        (overlay-put prj-qs-face 'face '(:background "white" :box "black"))

        (setq prj-qs-pos p)
        (setq prj-qs-len l)
        (setq prj-qs-str s)

        (when (setq e (read-key-sequence nil))
          (setq e (listify-key-sequence e))
          (setq  unread-command-events (nconc e unread-command-events))
          (unless (lookup-key prj-browse-map (vconcat e) t)
            (prj-qs-clear)
            ))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eproject-config.el ends here
