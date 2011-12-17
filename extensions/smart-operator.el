;;; smart-operator.el --- Insert operators with surrounding spaces smartly

;; Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010, 2011 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 2.0a
;; Url: http://xwl.appspot.com/ref/smart-operator.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This extension tries to insert operators with surrounding spaces smartly.
;; e.g., `=' becomes ` = ', `+=' becomes ` += '.  This is handy for writing
;; C-style sources.

;; To use, put this file to your load-path and the following to your
;; ~/.emacs:
;;             (require 'smart-operator)
;;
;; Then `M-x smart-operator-mode' for toggling this minor mode.

;;; Acknowledgements

;; Nikolaj Schumacher <n_schumacher@web.de>, for suggesting
;; reimplementing as a minor mode and providing an initial patch for
;; that.

;;; Code:

(require 'cc-mode)

;;; smart-operator minor mode

(defvar smart-operator-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "=" 'smart-operator-=)
    (define-key keymap "<" 'smart-operator-<)
    (define-key keymap ">" 'smart-operator->)
    (define-key keymap "%" 'smart-operator-%)
    (define-key keymap "+" 'smart-operator-+)
    (define-key keymap "-" 'smart-operator--)
    (define-key keymap "*" 'smart-operator-*)
    (define-key keymap "/" 'smart-operator-/)
    (define-key keymap "&" 'smart-operator-&)
    (define-key keymap "|" 'smart-operator-self-insert-command)
    ;; (define-key keymap "!" 'smart-operator-self-insert-command)
    (define-key keymap ":" 'smart-operator-:)
    (define-key keymap "?" 'smart-operator-?)
    (define-key keymap "," 'smart-operator-\,)
    (define-key keymap "~" 'smart-operator-~)
    (define-key keymap "." 'smart-operator-.)
    keymap)
  "Keymap used my `smart-operator-mode'.")

(defvar smart-operator-double-space-docs t
  "Enable double spacing of . in document lines - e,g, type '.' => get '.  '")

(defvar smart-operator-docs nil
  "Enable smart-operator in strings and comments")

;;;###autoload
(define-minor-mode smart-operator-mode
  "Insert operators with surrounding spaces smartly."
  nil " _+_" smart-operator-mode-map)

;;;###autoload
(defun smart-operator-mode-on ()
  "Turn on `smart-operator-mode'.  "
  (smart-operator-mode 1))

;;;###autoload
(defun smart-operator-self-insert-command (arg)
  "Insert the entered operator plus surrounding spaces."
  (interactive "p")
  (smart-operator-insert (string last-command-event)))

(defvar smart-operator-list
  '("=" "<" ">" "%" "+" "-" "*" "/" "&" "|" "!" ":" "?" "," "."))

(defun smart-operator-insert (op &optional only-where newline-&-indent)
  "See `smart-operator-insert-1'."
  (delete-horizontal-space)
  (cond ((and (smart-operator-lispy-mode?)
              (not (smart-operator-document-line?)))
         (smart-operator-lispy op))
        ((and
          (not smart-operator-docs)
          (smart-operator-document-line?))
         (smart-operator-insert-1 op 'middle))
        (t
         (smart-operator-insert-1 op only-where)))
  (if newline-&-indent (newline-and-indent)))

(defun smart-operator-insert-1 (op &optional only-where)
  "Insert operator OP with surrounding spaces.
e.g., `=' becomes ` = ', `+=' becomes ` += '.

When `only-where' is 'after, we will insert space at back only;
when `only-where' is 'before, we will insert space at front only;
when `only-where' is 'middle, we will not insert space."
  (case only-where
    ((before) (insert " " op))
    ((middle) (insert op))
    ((after) (insert op " "))
    (t
     (let ((begin? (bolp)))
       (unless (or (looking-back (regexp-opt smart-operator-list)
                                 (line-beginning-position))
                   begin?)
         (insert " "))
       (insert op " ")
       (when begin?
         (indent-according-to-mode))))))

(defun smart-operator-c-types ()
  (concat c-primitive-type-key "?"))

(defun smart-operator-document-line? ()
  (memq (syntax-ppss-context (syntax-ppss)) '(comment string)))

(defun smart-operator-lispy-mode? ()
  (memq major-mode '(emacs-lisp-mode
                     lisp-mode
                     lisp-interaction-mode
                     scheme-mode)))

(defun smart-operator-lispy (op)
  "We're in a Lisp-ish mode, so let's look for parenthesis.
Meanwhile, if not found after ( operators are more likely to be function names,
so let's not get too insert-happy."
  (cond
   ((save-excursion
      (backward-char 1)
      (looking-at "("))
    (if (equal op ",")
        (smart-operator-insert-1 op 'middle)
      (smart-operator-insert-1 op 'after)))
   ((equal op ",")
    (smart-operator-insert-1 op 'before))
   (t
    (smart-operator-insert-1 op 'middle))))


;;; Fine Tunings

(defun smart-operator-< ()
  "See `smart-operator-insert'."
  (interactive)
  (cond
   ((or (and c-buffer-is-cc-mode
             (looking-back
              (concat "\\("
                      (regexp-opt
                       '("#include" "vector" "deque" "list" "map" "stack"
                          "multimap" "set" "hash_map" "iterator" "template"
                          "pair" "auto_ptr" "static_cast"
                          "dynmaic_cast" "const_cast" "reintepret_cast"

                          "#import"))
                      "\\)\\ *")
              (line-beginning-position)))
        (eq major-mode 'sgml-mode))
    (insert "<>")
    (backward-char))
   (t
    (smart-operator-insert "<"))))

(defun smart-operator-: ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         (if (looking-back "\\?.+")
             (smart-operator-insert ":")
           (smart-operator-insert ":" 'middle)))
        ((memq major-mode '(haskell-mode))
         (smart-operator-insert ":"))
        ((memq major-mode '(clojure-mode))
         (smart-operator-insert ":" 'before))
        ((and (memq major-mode '(python-mode))
              (looking-back "\\(if\\|while\\|def\\|class\\|except\\|for\\|try\\|else\\).*"))
         (smart-operator-insert ":" 'middle t))
        ((memq major-mode '(ruby-mode erlang-mode))
         (insert ":"))
        (t
         (smart-operator-insert ":" 'after))))

(defun smart-operator-\, ()
  "See `smart-operator-insert'."
  (interactive)
  (smart-operator-insert "," 'after))

(defun smart-operator-. ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and smart-operator-double-space-docs
          (smart-operator-document-line?))
         (smart-operator-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (or (and c-buffer-is-cc-mode
                      (looking-back "[a-z]"))
                 (and
                  (memq major-mode '(python-mode ruby-mode))
                  (looking-back "[a-z\)]"))
                 (and
                  (memq major-mode '(js-mode js2-mode))
                  (looking-back "[a-z\)$_]"))))
             (insert "."))
        ((memq major-mode '(cperl-mode perl-mode ruby-mode))
         ;; Check for the .. range operator
         (if (looking-back ".")
               (insert ".")
           (insert " . ")))
        (t
         (smart-operator-insert "." 'after)
         (insert " "))))

(defun smart-operator-& ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         ;; ,----[ cases ]
         ;; | char &a = b; // FIXME
         ;; | void foo(const int& a);
         ;; | char *a = &b;
         ;; | int c = a & b;
         ;; | a && b;
         ;; `----
         (cond ((looking-back (concat (smart-operator-c-types) " *" ))
                (smart-operator-insert "&" 'after))
               ((looking-back "= *")
                (smart-operator-insert "&" 'before))
               (t
                (smart-operator-insert "&"))))
        (t
         (smart-operator-insert "&"))))

(defun smart-operator-* ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a * b;
         ;; | char *a;
         ;; | char **b;
         ;; | (*a)->func();
         ;; | *p++;
         ;; | *a = *b;
         ;; `----
         (cond ((looking-back (concat (smart-operator-c-types) " *" ))
                (smart-operator-insert "*" 'before))
               ((looking-back "\\* *")
                (smart-operator-insert "*" 'middle))
               ((looking-back "^[ (]*")
                (smart-operator-insert "*" 'middle)
                (indent-according-to-mode))
               ((looking-back "= *")
                (smart-operator-insert "*" 'before))
               (t
                (smart-operator-insert "*"))))
        (t
         (smart-operator-insert "*"))))

(defun smart-operator-> ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and c-buffer-is-cc-mode (looking-back " - "))
         (delete-char -3)
         (insert "->"))
        (t
         (smart-operator-insert ">"))))

(defun smart-operator-+ ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and c-buffer-is-cc-mode (looking-back "\\+ *"))
         (when (looking-back "[a-zA-Z0-9_] +\\+ *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (smart-operator-insert "+" 'middle)
         (indent-according-to-mode))
        (t
         (smart-operator-insert "+"))))

(defun smart-operator-- ()
  "See `smart-operator-insert'."
  (interactive)
  (cond ((and c-buffer-is-cc-mode (looking-back "\\- *"))
         (when (looking-back "[a-zA-Z0-9_] +\\- *")
           (save-excursion
             (backward-char 2)
             (delete-horizontal-space)))
         (smart-operator-insert "-" 'middle)
         (indent-according-to-mode))
        (t
         (smart-operator-insert "-"))))

(defun smart-operator-? ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         (smart-operator-insert "?"))
        (t
         (smart-operator-insert "?" 'after))))

(defun smart-operator-% ()
  "See `smart-operator-insert'."
  (interactive)
  (cond (c-buffer-is-cc-mode
         ;; ,----
         ;; | a % b;
         ;; | printf("%d %d\n", a % b);
         ;; `----
         (if (and (looking-back "\".*")
                  (not (looking-back "\",.*")))
             (insert "%")
           (smart-operator-insert "%")))
        ;; If this is a comment or string, we most likely
        ;; want no spaces - probably string formatting
        ((and (memq major-mode '(python-mode))
                    (smart-operator-document-line?))
               (insert "%"))
        (t
         (smart-operator-insert "%"))))

(defun smart-operator-~ ()
  "See `smart-operator-insert'."
  (interactive)
  ;; First class regex operator =~ langs
  (cond ((memq major-mode '(ruby-mode perl-mode cperl-mode))
         (if (looking-back "= ")
             (progn
               (delete-char -2)
               (insert "=~ "))
           (insert "~")))
        (t
         (insert "~"))))

(defun smart-operator-/ ()
  "See `smart-operator-insert'."
  (interactive)
  ;; *nix shebangs #!
  (cond ((and (eq 1 (line-number-at-pos))
              (save-excursion
                (move-beginning-of-line nil)
                (looking-at "#!")))
         (insert "/"))
        (t
         (smart-operator-insert "/"))))

(defun smart-operator-= ()
  "See `smart-operator-insert'."
  (interactive)
  ;; PEP 8 dictates that KW args or default parameters
  ;; get entered as foo(baz=bar)
  (cond ((and (memq major-mode '(python-mode))
              (looking-back "\([a-zA-Z0-9, =]+"))
         (insert "="))
        (t
         (smart-operator-insert "="))))

(provide 'smart-operator)

;;; smart-operator.el ends here
