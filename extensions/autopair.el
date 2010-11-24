;;; autopair.el --- Automagically pair braces and quotes like TextMate

;; Copyright (C) 2009,2010 Joao Tavora

;; Author: Joao Tavora <joaotavora [at] gmail.com>
;; Keywords: convenience, emulations
;; X-URL: http://autopair.googlecode.com
;; URL: http://autopair.googlecode.com
;; EmacsWiki: AutoPairs
;; Version: 0.4
;; Revision: $Rev$ ($LastChangedDate$)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Another stab at making braces and quotes pair like in
;; TextMate:
;; 
;; * Opening braces/quotes are autopaired;
;; * Closing braces/quotes are autoskipped;
;; * Backspacing an opening brace/quote autodeletes its adjacent pair.
;; * Newline between newly-opened brace pairs open an extra indented line.
;;
;; Autopair deduces from the current syntax table which characters to
;; pair, skip or delete.
;;
;;; Installation:
;;
;;   (require 'autopair)
;;   (autopair-global-mode) ;; to enable in all buffers
;;
;; To enable autopair in just some types of buffers, comment out the
;; `autopair-global-mode' and put autopair-mode in some major-mode
;; hook, like:
;;
;; (add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
;;
;; Alternatively, do use `autopair-global-mode' and create
;; *exceptions* using the `autopair-dont-activate' local variable,
;; like:
;;
;; (add-hook 'c-mode-common-hook #'(lambda () (setq autopair-dont-activate t)))
;;
;;; Use:
;;
;; The extension works by rebinding the braces and quotes keys, but
;; can still be minimally intrusive, since the original binding is
;; always called as if autopair did not exist.
;;
;; The decision of which keys to actually rebind is taken at
;; minor-mode activation time, based on the current major mode's
;; syntax tables. To achieve this kind of behaviour, an emacs
;; variable `emulation-mode-map-alists' was used.
;;
;; If you set `autopair-pair-criteria' and `autopair-skip-criteria' to
;; 'help-balance (which, by the way, is the default), braces are not
;; autopaired/autoskiped in all situations; the decision to autopair
;; or autoskip a brace is taken according to the following table:
;;
;;  +---------+------------+-----------+-------------------+
;;  | 1234567 | autopair?  | autoskip? | notes             |
;;  +---------+------------+-----------+-------------------+
;;  |  (())   |  yyyyyyy   |  ---yy--  | balanced          |
;;  +---------+------------+-----------+-------------------+
;;  |  (()))  |  ------y   |  ---yyy-  | too many closings |
;;  +---------+------------+-----------+-------------------+
;;  |  ((())  |  yyyyyyy   |  -------  | too many openings |
;;  +---------+------------+-----------+-------------------+
;;
;; The table is read like this: in a buffer with 7 characters laid out
;; like the first column, an "y" marks points where an opening brace
;; is autopaired and in which places would a closing brace be
;; autoskipped.
;;
;; Quote pairing tries to support similar "intelligence", but is less
;; deterministic. Some inside-string or inside-comment situations may
;; not always behave how you intend them to.
;;
;; The variable `autopair-autowrap' tells autopair to automatically
;; wrap the selection region with the delimiters you're trying to
;; insert. This is done conditionally based of syntaxes of the two
;; ends of the selection region. It is compatible with `cua-mode's
;; typing-deletes-selection behaviour. This feature is probably still
;; a little unstable, hence `autopair-autowrap' defaults to nil.
;;
;; If you find the paren-blinking annoying, turn `autopair-blink' to
;; nil.
;;
;; For lisp-programming you might also like `autopair-skip-whitespace'.
;;
;; For further customization have a look at `autopair-dont-pair',
;; `autopair-handle-action-fns' and `autopair-extra-pair'.
;;
;; `autopair-dont-pair' lets you define special cases of characters
;; you don't want paired.  Its default value skips pairing
;; single-quote characters when inside a comment literal, even if the
;; language syntax tables does pair these characters.
;;
;; (defvar autopair-dont-pair `(:string (?') :comment  (?'))
;;
;; As a further example, to also prevent the '{' (opening brace)
;; character from being autopaired in C++ comments use this in your
;; .emacs.
;;
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;                (push ?{
;;                      (getf autopair-dont-pair :comment))))
;;
;; `autopair-handle-action-fns' lets you override/extend the actions
;; taken by autopair after it decides something must be paired,skipped
;; or deleted. To work with triple quoting in python mode, you can use
;; this for example:
;;
;; (add-hook 'python-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-python-triple-quote-action))))
;;
;; It's also useful to deal with latex's mode use of the "paired
;; delimiter" syntax class.
;;
;; (add-hook 'latex-mode-hook
;;           #'(lambda ()
;;               (set (make-local-variable 'autopair-handle-action-fns)
;;                    (list #'autopair-default-handle-action
;;                          #'autopair-latex-mode-paired-delimiter-action))))
;;
;; `autopair-extra-pairs' lets you define extra pairing and skipping
;; behaviour for pairs not programmed into the syntax table. Watch
;; out, this is work-in-progress, a little unstable and does not help
;; balancing at all. To have '<' and '>' pair in c++-mode buffers, but
;; only in code, use:
;;
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (getf autopair-extra-pairs :code))))
;;
;; if you program in emacs-lisp you might also like the following to
;; pair backtick and quote
;;
;; (add-hook 'emacs-lisp-mode-hook
;;           #'(lambda ()
;;               (push '(?` . ?')
;;                     (getf autopair-extra-pairs :comment))
;;               (push '(?` . ?')
;;                     (getf autopair-extra-pairs :string))))
;;
;;; Bugs:
;;
;; * Quote pairing/skipping inside comments is not perfect...
;;
;; * See the last section on monkey-patching for the `defadvice'
;;   tricks used to make `autopair-autowrap' work with `cua-mode' and
;;   `delete-selection-mode'.
;;
;;; Credit:
;;
;; Thanks Ed Singleton for early testing.
;;
;;; Code:

;; requires
(require 'cl)

;; variables
(defvar autopair-pair-criteria 'help-balance
  "How to decide whether to pair opening brackets or quotes.

Set this to 'always to always pair, or 'help-balance to be more
criterious when pairing.")

(defvar autopair-skip-criteria 'help-balance
  "How to decide whether to skip closing brackets or quotes.

Set this to 'always to always skip, or 'help-balance to be more
criterious when skipping.")

(defvar autopair-emulation-alist nil
  "A dinamic keymap for autopair set mostly from the current
  syntax table.")

(defvar autopair-dont-activate nil
  "If non-nil `autopair-global-mode' does not activate in buffer")
(make-variable-buffer-local 'autopair-dont-activate)

(defvar autopair-extra-pairs nil
  "Extra pairs for which to use pairing.

It's a Common-lisp-style even-numbered property list, each pair
of elements being of the form (TYPE , PAIRS). PAIRS is a mixed
list whose elements are cons cells, which look like cells look
like (OPENING . CLOSING). Autopair pairs these like
parenthesis. 

TYPE can be one of:

:string : whereby PAIRS will be considered only when inside a
          string literal

:comment : whereby PAIRS will be considered only when inside a comment

:code : whereby PAIRS will be considered only when outisde a
        string and a comment.

:everywhere : whereby PAIRS will be considered in all situations

In Emacs-lisp, this might be useful

(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (setq autopair-extra-pairs `(:comment ((?`. ?'))))))


Note that this does *not* work for single characters,
e.x. characters you want to behave as quotes.  See the
docs/source comments for more details.")

(make-variable-buffer-local 'autopair-extra-pairs)

(defvar autopair-dont-pair `(:string (?') :comment  (?'))
  "Characters for which to skip any pairing behaviour.

This variable overrides `autopair-pair-criteria' and
`autopair-extra-pairs'. It does not
  (currently) affect the skipping behaviour.

It's a Common-lisp-style even-numbered property list, each pair
of elements being of the form (TYPE , CHARS). CHARS is a list of
characters and TYPE can be one of:

:string : whereby characters in CHARS will not be autopaired when
          inside a string literal

:comment : whereby characters in CHARS will not be autopaired when
          inside a comment

:never : whereby characters in CHARS won't even have their
         bindings replaced by autopair's. This particular option
         should be used for troubleshooting and requires
         `autopair-mode' to be restarted to have any effect.")
(make-variable-buffer-local 'autopair-dont-pair)

(defvar autopair-action nil
  "Autopair action decided on by last interactive autopair command, or nil.

When autopair decides on an action this is a list whose first
three elements are (ACTION PAIR POS-BEFORE).

ACTION is one of `opening', `insert-quote', `skip-quote',
`backspace', `newline' or `paired-delimiter'. PAIR is the pair of
the `last-input-event' character, if applicable. POS-BEFORE is
value of point before action command took place .")


(defvar autopair-wrap-action nil
  "Autowrap action decided on by autopair, if any.

When autopair decides on an action this is a list whose first
three elements are (ACTION PAIR POS-BEFORE REGION-BEFORE).

ACTION can only be `wrap' currently. PAIR and POS-BEFORE
delimiter are as in `autopair-action'. REGION-BEFORE is a cons
cell with the bounds of the region before the command takes
place")

(defvar autopair-handle-action-fns '()
  "Autopair handlers to run *instead* of the default handler.

Each element is a function taking three arguments (ACTION, PAIR
and POS-BEFORE), which are the three elements of the
`autopair-action' variable, which see.

If non-nil, these functions are called *instead* of the single
function `autopair-default-handle-action', so use this variable
to specify special behaviour. To also run the default behaviour,
be sure to include `autopair-default-handle-action' in the
list, or call it from your handlers.")
(make-variable-buffer-local 'autopair-handle-action-fns)

(defvar autopair-handle-wrap-action-fns '()
  "Autopair wrap handlers to run *instead* of the default handler.

Each element is a function taking four arguments (ACTION, PAIR, 
POS-BEFORE and REGION-BEFORE), which are the three elements of the
`autopair-wrap-action' variable, which see.

If non-nil, these functions are called *instead* of the single
function `autopair-default-handle-wrap-action', so use this
variable to specify special behaviour. To also run the default
behaviour, be sure to include `autopair-default-handle-wrap-action' in
the list, or call it in your handlers.")
(make-variable-buffer-local 'autopair-handle-wrap-action-fns)

;; minor mode and global mode
;;
(define-globalized-minor-mode autopair-global-mode autopair-mode autopair-on)

(defun autopair-on () (unless (or buffer-read-only autopair-dont-activate) (autopair-mode 1)))

(define-minor-mode autopair-mode
  "Automagically pair braces and quotes like in TextMate."
  nil " pair" nil
  (cond (autopair-mode
         ;; Setup the dynamic emulation keymap
         ;;
         (let ((map (make-sparse-keymap)))
           (define-key map [remap delete-backward-char] 'autopair-backspace)
           (define-key map [remap backward-delete-char-untabify] 'autopair-backspace)
           (define-key map (kbd "<backspace>") 'autopair-backspace)
           (define-key map [backspace] 'autopair-backspace)
           (define-key map (kbd "DEL") 'autopair-backspace)
           (define-key map (kbd "RET") 'autopair-newline)
           (dotimes (char 256) ;; only searches the first 256 chars,
                               ;; TODO: is this enough/toomuch/stupid?
             (unless (member char
                             (getf autopair-dont-pair :never))
               (let* ((syntax-entry (aref (syntax-table) char))
                      (class (and syntax-entry
                                  (syntax-class syntax-entry)))
                      (pair (and syntax-entry
                                 (cdr syntax-entry))))
                 (cond ((eq class (car (string-to-syntax "(")))
                        ;; syntax classes "opening parens" and "close parens"
                        (define-key map (string char) 'autopair-insert-opening)
                        (define-key map (string pair) 'autopair-skip-close-maybe))
                       ((eq class (car (string-to-syntax "\"")))
                        ;; syntax class "string quote
                        (define-key map (string char) 'autopair-insert-or-skip-quote))
                       ((eq class (car (string-to-syntax "$")))
                        ;; syntax class "paired-delimiter" 
                        ;;
                        ;; Apropos this class, see Issues 18, 25 and
                        ;; elisp info node "35.2.1 Table of Syntax
                        ;; Classes". The fact that it supresses
                        ;; syntatic properties in the delimited region
                        ;; dictates that deciding to autopair/autoskip
                        ;; can't really be as clean as the string
                        ;; delimiter.
                        ;;
                        ;; Apparently, only `TeX-mode' uses this, so
                        ;; the best is to bind this to
                        ;; `autopair-insert-or-skip-paired-delimiter'
                        ;; which defers any decision making to
                        ;; mode-specific post-command handler
                        ;; functions.
                        ;;
                        (define-key map (string char) 'autopair-insert-or-skip-paired-delimiter))))))
           ;; read `autopair-extra-pairs'
           (dolist (pairs-list (remove-if-not #'listp autopair-extra-pairs))
             (dolist (pair pairs-list)
               (define-key map (string (car pair)) 'autopair-extra-insert-opening)
               (define-key map (string (cdr pair)) 'autopair-extra-skip-close-maybe)))

           (set (make-local-variable 'autopair-emulation-alist) (list (cons t map))))

         (setq autopair-action nil)
         (setq autopair-wrap-action nil)
         (add-hook 'emulation-mode-map-alists 'autopair-emulation-alist 'append)
         (add-hook 'post-command-hook 'autopair-post-command-handler 'append 'local))
        (t
         (setq autopair-emulation-alist nil)
         (remove-hook 'emulation-mode-map-alists 'autopair-emulation-alist)
         (remove-hook 'post-command-hook         'autopair-post-command-handler 'local))))

;; helper functions
;;
(defun autopair-syntax-ppss ()
  "Calculate syntax info relevant to autopair.

A list of four elements is returned:

- SYNTAX-INFO is either the result `syntax-ppss' or the result of
  calling `parse-partial-sexp' with the appropriate
  bounds (previously calculated with `syntax-ppss'.

- WHERE-SYM can be one of the symbols :string, :comment or :code.

- QUICK-SYNTAX-INFO is always the result returned by `syntax-ppss'.

- BOUNDS are the boudaries of the current string or comment if
  we're currently inside one."
  (let* ((quick-syntax-info (syntax-ppss))
         (string-or-comment-start (nth 8 quick-syntax-info)))
    (cond (;; inside a string, recalculate
           (nth 3 quick-syntax-info)
           (list (parse-partial-sexp (1+ string-or-comment-start) (point))
                 :string
                 quick-syntax-info
                 (cons string-or-comment-start
                       (condition-case nil
                           (scan-sexps string-or-comment-start 1)
                         (error nil)))))
          ((nth 4 quick-syntax-info)
           (list (parse-partial-sexp (1+ (nth 8 quick-syntax-info)) (point))
                 :comment
                 quick-syntax-info))
          (t
           (list quick-syntax-info
                 :code
                 quick-syntax-info)))))

(defun autopair-find-pair (delim)
  (when (and delim
             (integerp delim))
    (let ((syntax-entry (aref (syntax-table) delim)))
      (cond ((eq (syntax-class syntax-entry) (car (string-to-syntax "(")))
             (cdr syntax-entry))
            ((or (eq (syntax-class syntax-entry) (car (string-to-syntax "\"")))
                 (eq (syntax-class syntax-entry) (car (string-to-syntax "$"))))
             delim)
            ((eq (syntax-class syntax-entry) (car (string-to-syntax ")")))
             (cdr syntax-entry))
            (autopair-extra-pairs
             (some #'(lambda (pair-list)
                       (some #'(lambda (pair)
                                 (cond ((eq (cdr pair) delim) (car pair))
                                       ((eq (car pair) delim) (cdr pair))))
                             pair-list))
                   (remove-if-not #'listp autopair-extra-pairs)))))))

(defun autopair-calculate-wrap-action ()
  (when (region-active-p)
    (save-excursion
      (let* ((region-before (cons (region-beginning)
                                  (region-end)))
             (point-before (point))
             (start-syntax (syntax-ppss (car region-before)))
             (end-syntax   (syntax-ppss (cdr region-before))))
        (when (and (eq (nth 0 start-syntax) (nth 0 end-syntax))
                   (eq (nth 3 start-syntax) (nth 3 end-syntax)))
          (list 'wrap (or (second autopair-action)
                          (autopair-find-pair last-input-event))
                point-before
                region-before))))))

(defun autopair-fallback (&optional fallback-keys)
  (let* ((autopair-emulation-alist nil)
         (beyond-cua (let ((cua--keymap-alist nil))
                       (or (key-binding (this-single-command-keys))
                           (key-binding fallback-keys))))
         (beyond-autopair (or (key-binding (this-single-command-keys))
                              (key-binding fallback-keys))))
    (when autopair-autowrap
      (setq autopair-wrap-action (autopair-calculate-wrap-action)))
    
    (setq this-original-command beyond-cua)
    ;; defer to "paredit-mode" if that is installed and running
    (when (and (featurep 'paredit)
               (string-match "paredit" (symbol-name beyond-cua)))
      (setq autopair-action nil))
    (let ((cua-delete-selection (not autopair-autowrap))
          (blink-matching-paren (not autopair-action)))
      (call-interactively beyond-autopair))))

(defvar autopair-autowrap nil
  "If non-nil autopair attempts to wrap the selected region.

This is also done in an optimistic \"try-to-balance\" fashion.")

(defvar autopair-skip-whitespace nil
  "If non-nil also skip over whitespace when skipping closing delimiters.

This will be most useful in lisp-like languages where you want
lots of )))))....")

(defvar autopair-blink (if (boundp 'blink-matching-paren)
                           blink-matching-paren
                         t)
  "If non-nil autopair blinks matching delimiters.")

(defvar autopair-blink-delay 0.1
  "Autopair's blink-the-delimiter delay.")

(defun autopair-document-bindings (&optional fallback-keys)
  (concat
   "Works by scheduling possible autopair behaviour, then calls
original command as if autopair didn't exist"
   (when (eq this-command 'describe-key)
     (let* ((autopair-emulation-alist nil)
            (command (or (key-binding (this-single-command-keys))
                         (key-binding fallback-keys))))
       (when command
         (format ", which in this case is `%s'" command))))
   "."))

(defun autopair-escaped-p (syntax-info)
  (nth 5 syntax-info))

(defun autopair-exception-p (where-sym exception-where-sym blacklist &optional fn)
  (and (or (eq exception-where-sym :everywhere)
           (eq exception-where-sym where-sym))
       (member last-input-event
               (if fn
                   (mapcar fn (getf blacklist exception-where-sym))
                 (getf blacklist exception-where-sym)))))

(defun autopair-up-list (syntax-info &optional closing)
  "Try to uplist as much as possible, moving point.

Return nil if something prevented uplisting.

Otherwise return a cons of char positions of the starting
delimiter and end delimiters of the last list we just came out
of. If we aren't inside any lists return a cons of current point.

If inside nested lists of mixed parethesis types, finding a
matching parenthesis of a mixed-type is considered OK (non-nil is
returned) and uplisting stops there."
  (condition-case nil
      (let ((howmany (car syntax-info))
            (retval (cons (point)
                          (point))))
        (while (and (> howmany 0)
                    (condition-case err
                        (progn
                          (scan-sexps (point) (- (point-max)))
                          (error err))
                      (error (let ((opening (and closing
                                                 (autopair-find-pair closing))))
                               (setq retval (cons (fourth err)
                                                  (point)))
                               (or (not opening)
                                   (eq opening (char-after (fourth err))))))))
          (goto-char (scan-lists (point) 1 1))
          (decf howmany))
        retval)
    (error nil)))

;; interactive commands and their associated predicates
;; 
(defun autopair-insert-or-skip-quote ()
  (interactive)
  (let* ((syntax-triplet (autopair-syntax-ppss))
         (syntax-info (first syntax-triplet))
         (where-sym (second syntax-triplet))
         (orig-info (third syntax-triplet))
         ;; inside-string may the quote character itself or t if this
         ;; is a "generically terminated string"
         (inside-string (and (eq where-sym :string)
                             (fourth orig-info)))
         (escaped-p (autopair-escaped-p syntax-info))
         
         )
    (cond (;; decides whether to skip the quote...
           ;;
           (and (not escaped-p)
                (eq last-input-event (char-after (point)))
                (or
                 ;; ... if we're already inside a string and the
                 ;; string starts with the character just inserted,
                 ;; or it's a generically terminated string
                 (and inside-string
                      (or (eq inside-string t)
                          (eq last-input-event inside-string)))
                 ;; ... if we're in a comment and ending a string
                 ;; (the inside-string criteria does not work
                 ;; here...)
                 (and (eq where-sym :comment)
                      (condition-case nil
                          (eq last-input-event (char-after (scan-sexps (1+ (point)) -1)))
                        (error nil)))))
           (setq autopair-action (list 'skip-quote last-input-event (point))))
          (;; decides whether to pair, i.e do *not* pair the quote if...
           ;;
           (not
            (or
             escaped-p
             ;; ... inside a generic string
             (eq inside-string t)
             ;; ... inside an unterminated string started by this char
             (autopair-in-unterminated-string-p syntax-triplet)
             ;; ... uplisting forward causes an error which leaves us
             ;; inside an unterminated string started by this char
             (condition-case err
                 (progn (save-excursion (up-list)) nil)
               (error
                (autopair-in-unterminated-string-p (save-excursion
                                                     (goto-char (fourth err))
                                                     (autopair-syntax-ppss)))))
             (autopair-in-unterminated-string-p (save-excursion
                                                  (goto-char (point-max))
                                                  (autopair-syntax-ppss)))
             ;; ... comment-disable or string-disable are true here.
             ;; The latter is only useful if we're in a string
             ;; terminated by a character other than
             ;; `last-input-event'.
             (some #'(lambda (sym)
                       (autopair-exception-p where-sym sym autopair-dont-pair))
                   '(:comment :string))))
           (setq autopair-action (list 'insert-quote last-input-event (point)))))
    (autopair-fallback)))

  (put 'autopair-insert-or-skip-quote 'function-documentation
     '(concat "Insert or possibly skip over a quoting character.\n\n"
              (autopair-document-bindings)))

(defun autopair-in-unterminated-string-p (autopair-triplet)
  (and (eq last-input-event (fourth (third autopair-triplet)))
       (condition-case nil (progn (scan-sexps (ninth (third autopair-triplet)) 1) nil) (error t))))     


(defun autopair-insert-opening ()
  (interactive)
  (when (autopair-pair-p)
    (setq autopair-action (list 'opening (autopair-find-pair last-input-event) (point))))
  (autopair-fallback))
(put 'autopair-insert-opening 'function-documentation
     '(concat "Insert opening delimiter and possibly automatically close it.\n\n"
              (autopair-document-bindings)))

(defun autopair-skip-close-maybe ()
  (interactive)
  (when (autopair-skip-p)
    (setq autopair-action (list 'closing (autopair-find-pair last-input-event) (point))))
  (autopair-fallback))
(put 'autopair-skip-close-maybe 'function-documentation
     '(concat "Insert or possibly skip over a closing delimiter.\n\n"
               (autopair-document-bindings)))

(defun autopair-backspace ()
  (interactive)
    (when (char-before)
      (setq autopair-action (list 'backspace (autopair-find-pair (char-before)) (point))))
  (autopair-fallback (kbd "DEL")))
(put 'autopair-backspace 'function-documentation
     '(concat "Possibly delete a pair of paired delimiters.\n\n"
              (autopair-document-bindings (kbd "DEL"))))

(defun autopair-newline ()
  (interactive)
  (let ((pair (autopair-find-pair (char-before))))
    (when (eq (char-after) pair)
      (setq autopair-action (list 'newline pair (point))))
    (autopair-fallback (kbd "RET"))))
(put 'autopair-newline 'function-documentation
     '(concat "Possibly insert two newlines and place point after the first, indented.\n\n"
              (autopair-document-bindings (kbd "RET"))))

(defun autopair-skip-p ()
  (interactive)
  (let* ((syntax-triplet (autopair-syntax-ppss))
         (syntax-info (first syntax-triplet))
         (orig-point (point)))
    (cond ((eq autopair-skip-criteria 'help-balance)
           (save-excursion
             (let ((pos-pair (autopair-up-list syntax-info last-input-event)))
               ;; if `autopair-up-list' returned something valid, we
               ;; probably want to skip but only if on of the following is true.
               ;;
               ;; 1. it returned a cons of equal values (we're not inside any list
               ;;
               ;; 2. up-listing stopped at a list that contains our original point
               ;;
               ;; 3. up-listing stopped at a list that does not
               ;;    contain out original point but its starting
               ;;    delimiter matches the one we expect.
               (and pos-pair
                    (or (eq (car pos-pair) (cdr pos-pair))
                        (< orig-point (cdr pos-pair))
                        (eq (char-after (car pos-pair))
                            (autopair-find-pair last-input-event)))))))
          ((eq autopair-skip-criteria 'need-opening)
           (save-excursion
             (condition-case err
                 (progn
                   (backward-list)
                   t)
               (error nil))))
          (t
           t))))

(defun autopair-pair-p ()
  (let* ((syntax-triplet (autopair-syntax-ppss))
         (syntax-info (first syntax-triplet))
         (where-sym (second syntax-triplet))
         (orig-point (point)))
    (and (not (some #'(lambda (sym)
                        (autopair-exception-p where-sym sym autopair-dont-pair))
                    '(:string :comment :code :everywhere)))
         (cond ((eq autopair-pair-criteria 'help-balance)
                (and (not (autopair-escaped-p syntax-info))
                     (save-excursion
                       (let ((pos-pair (autopair-up-list syntax-info))
                             (prev-point (point-max))
                             (expected-closing (autopair-find-pair last-input-event)))
                         (condition-case err
                             (progn
                               (while (not (eq prev-point (point)))
                                 (setq prev-point (point))
                                 (forward-sexp))
                               t)
                           (error
                            ;; if `forward-sexp' (called byp
                            ;; `autopair-forward') returned an error.
                            ;; typically we don't want to autopair,
                            ;; unless one of the following occurs:
                            ;; 
                            (cond (;; 1. The error is *not* of type "containing
                                   ;;    expression ends prematurely", which means
                                   ;;    we're in the "too-many-openings" situation
                                   ;;    and thus want to autopair.
                                   (not (string-match "prematurely" (second err)))  
                                   t)
                                  (;; 2. We stopped at a closing parenthesis. Do
                                   ;; autopair if we're in a mixed parens situation,
                                   ;; i.e. the last list jumped over was started by
                                   ;; the paren we're trying to match
                                   ;; (`last-input-event') and ended by a different
                                   ;; parens, or the closing paren we stopped at is
                                   ;; also different from the expected. The second
                                   ;; `scan-lists' places point at the closing of the
                                   ;; last list we forwarded over.
                                   ;; 
                                   (condition-case err
                                       (prog1
                                           (eq (char-after (scan-lists (point) -1 0))
                                               last-input-event)
                                         (goto-char (scan-lists (point) -1 -1)))
                                     (error t))
                                   
                                   (or
                                    ;; mixed () ] for input (, yes autopair
                                    (not (eq expected-closing (char-after (third err))))
                                    ;; mixed (] ) for input (, yes autopair
                                    (not (eq expected-closing (char-after (point))))
                                    ;; ()) for input (, not mixed
                                    ;; hence no autopair
                                    ))
                                  (t
                                   nil))
                            ;; (eq (fourth err) (point-max))
                            ))))))
               ((eq autopair-pair-criteria 'always)
                t)
               (t
                (not (autopair-escaped-p)))))))

;; post-command-hook stuff
;;
(defun autopair-post-command-handler ()
  "Performs pairing and wrapping based on `autopair-action' and
`autopair-wrap-action'. "
  (when (and autopair-wrap-action
             (notany #'null autopair-wrap-action))
    
    (if autopair-handle-wrap-action-fns
        (condition-case err
            (mapc #'(lambda (fn)
                      (apply fn autopair-wrap-action))
                  autopair-handle-wrap-action-fns)
          (error (progn
                   (message "[autopair] error running custom `autopair-handle-wrap-action-fns', switching autopair off")
                   (autopair-mode -1))))
      (apply #'autopair-default-handle-wrap-action autopair-wrap-action))
    (setq autopair-wrap-action nil))
  
  (when (and autopair-action
             (notany #'null autopair-action))
    (if autopair-handle-action-fns
        (condition-case err
            (mapc #'(lambda (fn)
                      (funcall fn (first autopair-action) (second autopair-action) (third autopair-action)))
                  autopair-handle-action-fns)
          (error (progn
                   (message "[autopair] error running custom `autopair-handle-action-fns', switching autopair off")
                   (autopair-mode -1))))
      (apply #'autopair-default-handle-action autopair-action))
    (setq autopair-action nil)))

(defun autopair-blink-matching-open ()
  (let ((blink-matching-paren autopair-blink)
        (show-paren-mode nil)
        (blink-matching-delay autopair-blink-delay))
    (blink-matching-open)))

(defun autopair-blink (&optional pos)
  (when autopair-blink
  (if pos
      (save-excursion
        (goto-char pos)
        (sit-for autopair-blink-delay))
    (sit-for autopair-blink-delay))))

(defun autopair-default-handle-action (action pair pos-before)
  ;;(message "action is %s" action)
  (cond (;; automatically insert closing delimiter
         (and (eq 'opening action)
              (not (eq pair (char-before))))
         (insert pair)
         (autopair-blink)
         (backward-char 1))
        (;; automatically insert closing quote delimiter
         (eq 'insert-quote action)
         (insert pair)
         (autopair-blink)
         (backward-char 1))
        (;; automatically skip oper closer quote delimiter
         (and (eq 'skip-quote action)
              (eq pair (char-after (point))))
         (delete-char 1)
         (autopair-blink-matching-open))
        (;; skip over newly-inserted-but-existing closing delimiter
         ;; (normal case)
         (eq 'closing action)
         (let ((skipped 0))
           (when autopair-skip-whitespace
             (setq skipped (save-excursion (skip-chars-forward "\s\n\t"))))
           (when (eq last-input-event (char-after (+ (point) skipped)))
             (unless (zerop skipped) (autopair-blink (+ (point) skipped)))
             (delete-char (1+ skipped))
             (autopair-blink-matching-open))))
        (;; autodelete closing delimiter
         (and (eq 'backspace action)
              (eq pair (char-after (point))))
         (delete-char 1))
        (;; opens an extra line after point, then indents
         (and (eq 'newline action)
              (eq pair (char-after (point))))
         (save-excursion
           (newline-and-indent))
         (indent-according-to-mode)
         (when (or (and (boundp 'global-hl-line-mode)
                        global-hl-line-mode)
                   (and (boundp 'hl-line-mode)
                        hl-line-mode))
           (hl-line-unhighlight) (hl-line-highlight)))))

(defun autopair-default-handle-wrap-action (action pair pos-before region-before)
  "Default handler for the wrapping action in `autopair-wrap'"
  (when (eq 'wrap action)
    (let ((reverse-selected (= (car region-before) pos-before)))
      (cond
       ((eq 'opening (first autopair-action))
        ;; (message "wrap-opening!")
        (cond (reverse-selected
               (goto-char (1+ (cdr region-before)))
               (insert pair)
               (autopair-blink)
               (goto-char (1+ (car region-before))))
              (t
               (delete-backward-char 1)
               (insert pair)
               (goto-char (car region-before))
               (insert last-input-event)))
        (setq autopair-action nil) )
       (;; wraps
        (eq 'closing (first autopair-action))
        ;; (message "wrap-closing!")
        (cond (reverse-selected
               (delete-backward-char 1)
               (insert pair)
               (goto-char (1+ (cdr region-before)))
               (insert last-input-event))
              (t
               (goto-char (car region-before))
               (insert pair)
               (autopair-blink)
               (goto-char (+ 2 (cdr region-before)))))
        (setq autopair-action nil))
       ((eq 'insert-quote (first autopair-action))
        (cond (reverse-selected
               (goto-char (1+ (cdr region-before)))
               (insert pair)
               (autopair-blink))
              (t
               (goto-char (car region-before))
               (insert last-input-event)
               (autopair-blink)))
        (setq autopair-action nil))
       (reverse-selected
        (delete-backward-char 1)
        (goto-char (cdr region-before))
        (insert last-input-event))))))


;; example python triple quote helper
;;
(defun autopair-python-triple-quote-action (action pair pos-before)
  (cond ((and (eq 'insert-quote action)
              (>= (point) 3)
              (string= (buffer-substring (- (point) 3)
                                         (point))
                       (make-string 3 pair)))
         (save-excursion (insert (make-string 2 pair))))
        ((and (eq 'backspace action)
              (>= (point) 2)
              (<= (point) (- (point-max) 2))
              (string= (buffer-substring (- (point) 2)
                                         (+ (point) 2))
                       (make-string 4 pair)))
         (delete-region (- (point) 2)
                        (+ (point) 2)))
        ((and (eq 'skip-quote action)
              (<= (point) (- (point-max) 2))
              (string= (buffer-substring (point)
                                         (+ (point) 2))
                       (make-string 2 pair)))
         (forward-char 2))
        (t
         t)))

;; example latex paired-delimiter helper 
;;
(defun autopair-latex-mode-paired-delimiter-action (action pair pos-before)
  "Pair or skip latex's \"paired delimiter\" syntax in math mode."
  (when (eq action 'paired-delimiter)
    (when (eq (char-before) pair)
      (if (and (eq (get-text-property pos-before 'face) 'tex-math)
               (eq (char-after) pair))
          (cond ((and (eq (char-after) pair)
                      (eq (char-after (1+ (point))) pair))
                 ;; double skip
                 (delete-char 1)
                 (forward-char))
                ((eq (char-before pos-before) pair)
                 ;; doube insert
                 (insert pair)
                 (backward-char))
                (t
                 ;; simple skip
                 (delete-char 1)))
        (insert pair)
        (backward-char)))))

;; Commands and predicates for the autopair-extra* feature 
;;

(defun autopair-extra-insert-opening ()
  (interactive)
  (when (autopair-extra-pair-p)
    (setq autopair-action (list 'opening (autopair-find-pair last-input-event) (point))))
  (autopair-fallback))
(put 'autopair-extra-insert-opening 'function-documentation
     '(concat "Insert (an extra) opening delimiter and possibly automatically close it.\n\n"
              (autopair-document-bindings)))

(defun autopair-extra-skip-close-maybe ()
  (interactive)
  (when (autopair-extra-skip-p)
    (setq autopair-action (list 'closing last-input-event (point))))
  (autopair-fallback))
(put 'autopair-extra-skip-close-maybe 'function-documentation
     '(concat "Insert or possibly skip over a (and extra) closing delimiter.\n\n"
              (autopair-document-bindings)))

(defun autopair-extra-pair-p ()
  (let* ((syntax-triplet (autopair-syntax-ppss))
         (syntax-info (first syntax-triplet))
         (where-sym (second syntax-triplet)))
    (some #'(lambda (sym)
              (autopair-exception-p where-sym sym autopair-extra-pairs #'car))
          '(:everywhere :comment :string :code))))

(defun autopair-extra-skip-p ()
  (let* ((syntax-triplet (autopair-syntax-ppss))
         (syntax-info (first syntax-triplet))
         (where-sym (second syntax-triplet))
         (orig-point (point)))
    (and (eq (char-after (point)) last-input-event)
         (some #'(lambda (sym)
                   (autopair-exception-p where-sym sym autopair-extra-pairs #'cdr))
               '(:comment :string :code :everywhere))
         (save-excursion
           (condition-case err
               (backward-sexp (point-max))
             (error
              (goto-char (third err))))
           (search-forward (make-string 1 (autopair-find-pair last-input-event))
                           orig-point
                           'noerror)))))

;; Commands and tex-mode specific handler functions for the "paired
;; delimiter" syntax class.
;; 
(defun autopair-insert-or-skip-paired-delimiter ()
  " insert or skip a character paired delimiter"
  (interactive)
  (setq autopair-action (list 'paired-delimiter last-input-event (point)))
  (autopair-fallback))

(put 'autopair-insert-or-skip-paired-delimiter 'function-documentation
     '(concat "Insert or possibly skip over a character with a syntax-class of \"paired delimiter\"."
              (autopair-document-bindings)))



;; monkey-patching: Compatibility with delete-selection-mode and cua-mode
;;
;; Ideally one would be able to use functions as the value of the
;; 'delete-selection properties of the autopair commands. The function
;; would return non-nil when no wrapping should/could be performed.
;;
;; Until then use some `defadvice' i.e. monkey-patching
;;
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)

(defun autopair-should-autowrap ()
  (let ((name (symbol-name this-command)))
    (and autopair-mode
         (not (eq this-command 'autopair-backspace))
         (string-match "^autopair" (symbol-name this-command))
         (autopair-calculate-wrap-action))))

(defadvice cua--pre-command-handler-1 (around autopair-override activate)
  "Don't actually do anything if autopair is about to autowrap. "
  (unless (autopair-should-autowrap) ad-do-it))

(defadvice delete-selection-pre-hook (around autopair-override activate)
  "Don't actually do anything if autopair is about to autowrap. "
  (unless (autopair-should-autowrap) ad-do-it))


(provide 'autopair)
;;; autopair.el ends here
;;
