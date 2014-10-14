;;; grizzl-read.el --- A fuzzy completing-read backed by grizzl.

;; Copyright © 2013 Chris Corbyn
;;
;; Author:   Chris Corbyn <chris@w3style.co.uk>
;; URL:      https://github.com/d11wtq/grizzl
;; Version:  0.1.2
;; Keywords: convenience, usability

;; This file is NOT part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

;;; --- Commentary

;; grizzl-read.el provides an implementation of the built-in Emacs
;; completing-read function, except it is backed by the grizzl fuzzy
;; search index. The goals are similar to ido-mode and helm, but grizzl
;; is heavily optimized for large data-sets, and as-such uses a
;; persistent fuzzy search index in its algorithm.
;;
;; The indexing and searching algorithm itself is defined in grizzl-core.el
;; with grizzl-read.el simply wrapping the search in a minibuffer with a
;; minor-mode defined.
;;
;; ---- Usage
;;
;; Call `grizzl-completing-read' with an index returned by
;; `grizzl-make-index':
;;
;;    (defvar *index* (grizzl-make-index '("one" "two" "three")))
;;    (grizzl-completing-read "Number: " index)
;;
;; When the user hits ENTER, either one of the strings is returned on
;; success, or nil of nothing matched.
;;
;; The arrow keys can be used to navigate within the results.
;;

(eval-when-compile
  (require 'cl-lib))

;;; --- Configuration Variables

(defvar *grizzl-read-max-results* 10
  "The maximum number of results to show in `grizzl-completing-read'.")

;;; --- Runtime Processing Variables

(defvar *grizzl-current-result* nil
  "The search result in `grizzl-completing-read'.")

(defvar *grizzl-current-selection* 0
  "The selected offset in `grizzl-completing-read'.")

;;; --- Minor Mode Definition

(defvar *grizzl-keymap* (make-sparse-keymap)
  "Internal keymap used by the minor-mode in `grizzl-completing-read'.")

(define-key *grizzl-keymap* (kbd "<up>")   'grizzl-set-selection+1)
(define-key *grizzl-keymap* (kbd "C-p")    'grizzl-set-selection+1)
(define-key *grizzl-keymap* (kbd "<down>") 'grizzl-set-selection-1)
(define-key *grizzl-keymap* (kbd "C-n")    'grizzl-set-selection-1)

(define-minor-mode grizzl-mode
  "Toggle the internal mode used by `grizzl-completing-read'."
  nil
  " Grizzl"
  *grizzl-keymap*)

;;; --- Public Functions

;;;###autoload
(defun grizzl-completing-read (prompt index)
  "Performs a completing-read in the minibuffer using INDEX to fuzzy search.
Each key pressed in the minibuffer filters down the list of matches."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq *grizzl-current-result* nil)
        (setq *grizzl-current-selection* 0)
        (grizzl-mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *grizzl-current-result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *grizzl-current-result*))
                        (grizzl-display-result index prompt)))
             (exitfun (lambda ()
                        (grizzl-mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer ">>> ")
    (grizzl-selected-result index)))

;;;###autoload
(defun grizzl-selected-result (index)
  "Get the selected string from INDEX in a `grizzl-completing-read'."
  (elt (grizzl-result-strings *grizzl-current-result* index
                              :start 0
                              :end   *grizzl-read-max-results*)
       (grizzl-current-selection)))

;;;###autoload
(defun grizzl-set-selection+1 ()
  "Move the selection up one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection 1))

;;;###autoload
(defun grizzl-set-selection-1 ()
  "Move the selection down one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection -1))

;;; --- Private Functions

(defun grizzl-move-selection (delta)
  "Move the selection by DELTA rows in `grizzl-completing-read'."
  (setq *grizzl-current-selection* (+ (grizzl-current-selection) delta))
  (when (not (= (grizzl-current-selection) *grizzl-current-selection*))
    (beep)))

(defun grizzl-display-result (index prompt)
  "Renders a series of overlays to list the matches in the result."
  (let* ((matches (grizzl-result-strings *grizzl-current-result* index
                                         :start 0
                                         :end   *grizzl-read-max-results*)))
    (delete-all-overlays)
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n%s\n"
                         (mapconcat 'identity
                                    (grizzl-map-format-matches matches)
                                    "\n")
                         (grizzl-format-prompt-line prompt)))
    (set-window-text-height nil (max 3 (+ 2 (length matches))))))

(defun grizzl-map-format-matches (matches)
  "Convert the set of string MATCHES into propertized text objects."
  (if (= 0 (length matches))
      (list (propertize "-- NO MATCH --" 'face 'outline-3))
    (cdr (cl-reduce (lambda (acc str)
                      (let* ((idx (car acc))
                             (lst (cdr acc))
                             (sel (= idx (grizzl-current-selection))))
                        (cons (1+ idx)
                              (cons (grizzl-format-match str sel) lst))))
                    matches
                    :initial-value '(0)))))

(defun grizzl-format-match (match-str selected)
  "Default match string formatter in `grizzl-completing-read'.

MATCH-STR is the string in the selection list and SELECTED is non-nil
if this is the current selection."
  (let ((margin (if selected "> "            "  "))
        (face   (if selected 'diredp-symlink 'default)))
    (propertize (format "%s%s" margin match-str) 'face face)))

(defun grizzl-format-prompt-line (prompt)
  "Returns a string to render a full-width prompt in `grizzl-completing-read'."
  (let* ((count (grizzl-result-count *grizzl-current-result*))
         (match-info (format " (%d candidate%s) ---- *-"
                             count (if (= count 1) "" "s"))))
    (concat (propertize (format "-*%s *-" prompt) 'face 'modeline-inactive)
            (propertize " "
                        'face    'modeline-inactive
                        'display `(space :align-to (- right
                                                      ,(1+ (length match-info)))))
            (propertize match-info 'face 'modeline-inactive))))

(defun grizzl-current-selection ()
  "Get the currently selected index in `grizzl-completing-read'."
  (let ((max-selection
         (min (1- *grizzl-read-max-results*)
              (1- (grizzl-result-count *grizzl-current-result*)))))
    (max 0 (min max-selection *grizzl-current-selection*))))

(provide 'grizzl-read)

;;; grizzl-read.el ends here
