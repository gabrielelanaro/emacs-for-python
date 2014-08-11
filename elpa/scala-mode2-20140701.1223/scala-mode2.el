;;; scala-mode2.el --- Major mode for editing scala

;; Copyright (c) 2012 Heikki Vesalainen
;; For information on the License, see the LICENSE file
;; URL: http://github.com/hvesalai/scala-mode2
;; Based on Scala Language Specification (SLS) Version 2.9

(require 'scala-mode2-lib)
(require 'scala-mode2-syntax)
(require 'scala-mode2-paragraph)
(require 'scala-mode2-indent)
(require 'scala-mode2-fontlock)
(require 'scala-mode2-map)
(require 'scala-mode2-sbt)

;; Tested only for emacs 24
(unless (<= 24 emacs-major-version)
  (error
   (format "The Scala mode has been tested only on Emacs version 24.2 (and not your Emacs version %s.%s)"  
           emacs-major-version  emacs-minor-version)))

(defgroup scala nil
  "A programming mode for the Scala language 2.9"
  :group 'languages)

(defmacro scala-mode:make-local-variables (&rest quoted-names)
  (cons 'progn (mapcar #'(lambda (quoted-name) `(make-local-variable ,quoted-name)) quoted-names)))

(defun scala-mode:find-tag ()
  "Determine default tag to search for, based on text at point.
If there is no plausible default, return nil."
  (let (from to)
    (when (and (progn
                 ;; Look at text around `point'.
                 (save-excursion
                   (if (< 0 (skip-chars-backward scala-syntax:opchar-group))
                       (if (= (char-before) ?_)
                           (skip-syntax-backward "w_"))
                     (skip-syntax-backward "w_"))
                   (setq from (point)))
                 (save-excursion
                   (skip-syntax-forward "w_.") (setq to (point)))
                 (save-excursion
                   (ignore-errors (scala-syntax:backward-sexp)) (setq from (max from (point))))
                 (save-excursion
                   (goto-char from)
                   (ignore-errors (scala-syntax:forward-sexp)) (setq to (min to (point))))
                 (> to from))
               (save-excursion
                 (goto-char from)
                 (and (looking-at scala-syntax:id-re)
                      (not (looking-at scala-syntax:keywords-unsafe-re)))))
      (buffer-substring-no-properties from to))))


(defun scala-mode:forward-sexp-function (&optional count)
  (unless count (setq count 1))
  (if (< count 0)
      (dotimes (n (abs count))
        (scala-syntax:backward-sexp))
    (dotimes (n count)
      (scala-syntax:forward-sexp))))

;;;###autoload
(defun scala-mode:set-scala-syntax-mode ()
  "Sets the syntax-table and other realted variables for the current buffer to those of scala-mode. Can be used to make some other major mode (such as sbt-mode) use scala syntax-table."
  (set-syntax-table scala-syntax:syntax-table)
  (scala-mode:make-local-variables
   'syntax-propertize-function
   'parse-sexp-lookup-properties
   'forward-sexp-function)  

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)
  (setq syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t
        forward-sexp-function           'scala-mode:forward-sexp-function))

;;;###autoload
(define-derived-mode scala-mode prog-mode "Scala"
  "Major mode for editing scala code.

When started, runs `scala-mode-hook'. 

\\{scala-mode-map}" 
  :syntax-table scala-syntax:syntax-table
;  :group                               
;  :abbrev

  (scala-mode:make-local-variables
   'post-self-insert-hook
   'syntax-propertize-function
   'font-lock-syntactic-face-function
   'font-lock-defaults
   'paragraph-start
   'paragraph-separate
   'parse-sexp-lookup-properties
   'fill-paragraph-function
   'adaptive-fill-function
   'adaptive-fill-first-line-regexp
   'comment-start
   'comment-end
   'comment-start-skip
   'comment-column
   'comment-multi-line
   'forward-sexp-function
   'find-tag-default-function
   'indent-line-function
   'fixup-whitespace
   'delete-indentation
   'indent-tabs-mode)

  (add-hook 'syntax-propertize-extend-region-functions
            'scala-syntax:propertize-extend-region)
  (setq scala-mode:debug-messages       nil

        syntax-propertize-function      'scala-syntax:propertize
        parse-sexp-lookup-properties    t

        ;; TODO: font-lock
        font-lock-defaults              '(scala-font-lock:keywords
                                          nil)
        font-lock-syntactic-face-function 'scala-font-lock:syntactic-face-function

        ;; TODO: beginning-of-defun-function, end-of-defun-function

        ;; comments
        paragraph-start                 scala-paragraph:paragraph-start-re
        paragraph-separate              scala-paragraph:paragraph-separate-re
        fill-paragraph-function         'scala-paragraph:fill-paragraph
        adaptive-fill-function          'scala-paragraph:fill-function
        adaptive-fill-first-line-regexp scala-paragraph:fill-first-line-re
        comment-start                   "// "
        comment-end                     ""
        comment-start-skip              "\\(//+\\|/\\*+\\)[ \t]*"
        comment-column                  0
        comment-multi-line              t

        forward-sexp-function           'scala-mode:forward-sexp-function
        find-tag-default-function       'scala-mode:find-tag
        indent-line-function            'scala-indent:indent-line
        fixup-whitespace                'scala-indent:fixup-whitespace
        delete-indentation              'scala-indent:join-line
        indent-tabs-mode                nil
        )
  (use-local-map scala-mode-map)
  ;; add indent functionality to some characters
  (scala-mode-map:add-remove-indent-hook)
  (scala-mode-map:add-self-insert-hooks)
)

;; Attach .scala files to the scala-mode
;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))
  (modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8))

(provide 'scala-mode2)
;;; scala-mode2.el ends here
