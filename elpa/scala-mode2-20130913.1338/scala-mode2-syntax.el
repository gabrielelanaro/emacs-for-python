;;;; scala-mode-syntax.el - Major mode for editing scala, syntax
;;; Copyright (c) 2012 Heikki Vesalainen
;;; For information on the License, see the LICENSE file

;;; Based on Scala Language Specification (SLS) Version 2.9

;;;;
;;;; Scala syntax regular expressions
;;;;

;;; Based on the Scala language specification 2.9.  Note: order is not
;;; the same as in the document, as here things are declared before
;;; used.

;;; A note on naming. Things that end with '-re' are regular
;;; expressions.  Things that end with '-group' are regular expression
;;; character groups without the enclosing [], i.e. they are not
;;; regular expressions, but can be used in declaring one.

;; single letter matching groups (Chapter 1)
(defconst scala-syntax:hexDigit-group "0-9A-Fa-f")
(defconst scala-syntax:UnicodeEscape-re (concat "\\\\u[" scala-syntax:hexDigit-group "]\\{4\\}"))

(defconst scala-syntax:upper-group "[:upper:]\\$") ;; missing _ to make ids work
(defconst scala-syntax:upperAndUnderscore-group (concat "_" scala-syntax:upper-group ))
(defconst scala-syntax:lower-group "[:lower:]")
(defconst scala-syntax:letter-group (concat scala-syntax:lower-group scala-syntax:upper-group)) ;; TODO: add Lt, Lo, Nl
(defconst scala-syntax:digit-group "0-9")
(defconst scala-syntax:letterOrDigit-group (concat
                                            scala-syntax:upperAndUnderscore-group
                                            scala-syntax:lower-group
                                            scala-syntax:digit-group))
(defconst scala-syntax:opchar-safe-group "!%&*+/?\\\\^|~-") ;; TODO: Sm, So
(defconst scala-syntax:opchar-unsafe-group "#:<=>@")
(defconst scala-syntax:opchar-group (concat scala-syntax:opchar-unsafe-group
                                            scala-syntax:opchar-safe-group))

;; Scala delimiters (Chapter 1), but no quotes
(defconst scala-syntax:delimiter-group ".,;")

;; Integer Literal (Chapter 1.3.1)
(defconst scala-syntax:nonZeroDigit-group "1-9")
(defconst scala-syntax:octalDigit-group "0-7")
(defconst scala-syntax:decimalNumeral-re
  (concat "0"
          "\\|[" scala-syntax:nonZeroDigit-group "][" scala-syntax:digit-group "]*"))
(defconst scala-syntax:hexNumeral-re (concat "0x[" scala-syntax:hexDigit-group "]+"))
(defconst scala-syntax:octalNumeral-re (concat "0[" scala-syntax:octalDigit-group "]+"))
(defconst scala-syntax:integerLiteral-re (concat "-?" ;; added from definition of literal
                                                 "\\(" scala-syntax:hexNumeral-re
                                                 "\\|" scala-syntax:octalNumeral-re
                                                 "\\|" scala-syntax:decimalNumeral-re
                                                 "\\)[Ll]?"))


;; Floating Point Literal (Chapter 1.3.2)
(defconst scala-syntax:exponentPart-re (concat "\\([eE][+-]?[" scala-syntax:digit-group "]+\\)"))
(defconst scala-syntax:floatType-re "[fFdD]")
(defconst scala-syntax:floatingPointLiteral-re
  (concat "-?" ;; added from definition of literal
          "\\([" scala-syntax:digit-group "]+\\.[" scala-syntax:digit-group "]*"
          scala-syntax:exponentPart-re "?" scala-syntax:floatType-re "?"
          "\\|" "\\.[" scala-syntax:digit-group "]+"
          scala-syntax:exponentPart-re "?" scala-syntax:floatType-re "?"
          "\\|" "[" scala-syntax:digit-group "]+" scala-syntax:exponentPart-re
          "\\|" "[" scala-syntax:digit-group "]+" scala-syntax:floatType-re "\\)"))

(defconst scala-syntax:number-safe-start-re
  (concat "[^_" scala-syntax:letter-group "]"))

;; Boolean Literals (Chapter 1.3.3)
(defconst scala-syntax:booleanLiteral-re "true|false")

;; Escape Sequences (Chapter 1.3.6)
(defconst scala-syntax:escapeSequence-re "\\\\['btnfr\"\\\\]")

;; Octal Escape Sequences (Chapter 1.3.6)
(defconst scala-syntax:octalEscape-re (concat "\\\\[" scala-syntax:octalDigit-group "\\]\\{1,3\\}"))

;; Character Literals (Chapter 1.3.4)
(defconst scala-syntax:characterLiteral-re
  (concat "\\('\\)\\(" "[^\\\\]" ;; should be just printable char, but this is faster
          "\\|" scala-syntax:escapeSequence-re
          "\\|" scala-syntax:octalEscape-re
          "\\|" scala-syntax:UnicodeEscape-re "\\)\\('\\)"))

(defconst scala-syntax:string-escape-re
  (concat scala-syntax:escapeSequence-re
          "\\|" scala-syntax:octalEscape-re
          "\\|" scala-syntax:UnicodeEscape-re))

;; String Literals (Chapter 1.3.5)
(defconst scala-syntax:stringElement-re
  (concat "\\(" "[^\n\"\\\\]"
          "\\|" scala-syntax:string-escape-re  "\\)"))
(defconst scala-syntax:oneLineStringLiteral-re (concat "\\(\"\\)" scala-syntax:stringElement-re "*\\(\"\\)"))
(defconst scala-syntax:multiLineStringLiteral-start-re
  "\\(\"\\)\"\"\\(\"?\"?[^\"]\\)*")
(defconst scala-syntax:multiLineStringLiteral-end-re
  "\"\"+\\(\"\\)")
(defconst scala-syntax:multiLineStringLiteral-re
  (concat scala-syntax:multiLineStringLiteral-start-re
          scala-syntax:multiLineStringLiteral-end-re))
(defconst scala-syntax:stringLiteral-re
  (concat "\\(" scala-syntax:multiLineStringLiteral-re
          "\\|" scala-syntax:oneLineStringLiteral-re "\\)" ))

;; If you change this or any of the used regex, be sure to
;; maintain this or update propertize function acordingly:
;; group 1 = char start, 3 = char end
;; group 4 = multi-line string start, 6 = end
;; group 7 = string start, 9 = end
(defconst scala-syntax:relaxed-char-and-string-literal-re
  (concat scala-syntax:characterLiteral-re
          "\\|" scala-syntax:multiLineStringLiteral-start-re
          "\\(?:" scala-syntax:multiLineStringLiteral-end-re "\\)?"
          "\\|\\(\"\\)" "\\(\\\\.\\|[^\"\n\\]\\)*" "\\(\"\\)"))

;; Identifiers (Chapter 1.1)
(defconst scala-syntax:op-re
  (concat "[" scala-syntax:opchar-group "]+" ))
(defconst scala-syntax:idrest-re
  ;; Eagerness of regexp causes problems with _. The following is a workaround,
  ;; but the resulting regexp matches only what SLS demands.
  (concat "\\(" "[_]??" "[" scala-syntax:letter-group scala-syntax:digit-group "]+" "\\)*"
          "\\(" "_+" scala-syntax:op-re "\\|" "_" "\\)?"))
(defconst scala-syntax:varid-re (concat "[" scala-syntax:lower-group "]" scala-syntax:idrest-re))
(defconst scala-syntax:capitalid-re (concat "[" scala-syntax:upperAndUnderscore-group "]" scala-syntax:idrest-re))
;; alphaid introduce by SIP11
(defconst scala-syntax:alphaid-re (concat "\\(" "[" scala-syntax:lower-group scala-syntax:upperAndUnderscore-group "]" scala-syntax:idrest-re "\\)"))
(defconst scala-syntax:plainid-re (concat "\\(" scala-syntax:alphaid-re "\\|" scala-syntax:op-re "\\)"))
;; stringlit is referred to, but not defined Scala Language Specification 2.9
;; we define it as consisting of anything but '`' and newline
(defconst scala-syntax:stringlit-re "[^`\n\r]")
(defconst scala-syntax:quotedid-re (concat "`" scala-syntax:stringlit-re "`"))
(defconst scala-syntax:id-re (concat "\\(" scala-syntax:plainid-re
                              "\\|" scala-syntax:quotedid-re "\\)"))
(defconst scala-syntax:id-first-char-group
  (concat scala-syntax:lower-group
          scala-syntax:upperAndUnderscore-group
          scala-syntax:opchar-group))

;; Symbol literals (Chapter 1.3.7)
(defconst scala-syntax:symbolLiteral-re
  ;; must end with non-' to not conflict with scala-syntax:characterLiteral-re
  (concat "\\('" scala-syntax:plainid-re "\\)\\([^']\\|$\\)"))

;; Literals (Chapter 1.3)
(defconst scala-syntax:literal-re
  (concat "\\(" scala-syntax:integerLiteral-re
          "\\|" scala-syntax:floatingPointLiteral-re
          "\\|" scala-syntax:booleanLiteral-re
          "\\|" scala-syntax:characterLiteral-re
          "\\|" scala-syntax:stringLiteral-re
          "\\|" scala-syntax:symbolLiteral-re
          "\\|" "null" "\\)"))

;; Paths (Chapter 3.1)
;; emacs has a problem with these regex, don't use them
;; (defconst scala-syntax:classQualifier-re (concat "[[]" scala-syntax:id-re "[]]"))
;; (defconst scala-syntax:stableId-re
;;   (concat "\\(\\(" "this"
;;           "\\|" "super" scala-syntax:classQualifier-re
;;           "\\|" scala-syntax:id-re
;;           "\\)\\.\\)*"
;;           scala-syntax:id-re))
;; (defconst scala-syntax:path-re
;;   (concat "\\(" scala-syntax:stableId-re
;;           "\\|" "\\(" scala-syntax:id-re "\\." "\\)?" "this" "\\)"))

(defun scala-syntax:looking-at-super ()
  (save-excursion
    (when (looking-at "\\<super\\>")
      (let ((beg (match-beginning 0)))
        (when (and (goto-char (match-end 0))
                   (or (when (= (char-after) ?.)
                         (forward-char)
                         t)
                       (and (when (and (not (eobp)) (= (char-after) ?\[))
                              (forward-char)
                              t)
                            (progn (scala-syntax:skip-forward-ignorable)
                                   (looking-at scala-syntax:id-re))
                            (progn (goto-char (match-end 0))
                                   (scala-syntax:skip-forward-ignorable)
                                   (when (and (not (eobp)) (= (char-after) ?\]))
                                     (forward-char)
                                     t))
                            (when (and (not (eobp)) (= (char-after) ?.))
                              (forward-char)
                              t)))
                   (looking-at scala-syntax:id-re))
          (set-match-data `(,beg ,(match-end 0)))
          t)))))

(defun scala-syntax:looking-at-stableIdOrPath (&optional path-p beg)
  (unless beg (setq beg (point)))
  (save-excursion
    (cond ((looking-at "\\<this\\>")
           (goto-char (match-end 0))
           (if (and (not (eobp)) (= (char-after) ?.))
               (progn (forward-char)
                      (scala-syntax:looking-at-stableIdOrPath path-p beg))
             path-p))
          ((or (scala-syntax:looking-at-super)
               (and (not (or (looking-at scala-syntax:keywords-unsafe-re)
                             (scala-syntax:looking-at-reserved-symbol nil)))
                    (looking-at scala-syntax:id-re)))
           (goto-char (match-end 0))
           (if (and (not (eobp)) (= (char-after) ?.))
               (progn (forward-char)
                      (scala-syntax:looking-at-stableIdOrPath path-p beg))
             (set-match-data `(,beg ,(match-end 0)))
             (point))))))

(defun scala-syntax:looking-at-simplePattern-beginning ()
  (or (looking-at "[_(]")
      (looking-at scala-syntax:literal-re)
      (scala-syntax:looking-at-stableIdOrPath)))

;;;
;;; Other regular expressions
;;;

(defconst scala-syntax:preamble-start-re
  "\#\!")

(defconst scala-syntax:preamble-end-re
  "\!\\(\#\\)[ \t]*$")

(defconst scala-syntax:empty-line-re
  "^\\s *$")

(defconst scala-syntax:comment-start-re
  "/[/*]")

(defconst scala-syntax:end-of-code-line-re
  (concat "\\([ ]\\|$\\|" scala-syntax:comment-start-re "\\)")
  "A special regexp that can be concatenated to an other regular
  expression when used with scala-syntax:looking-back-token. Not
  meaningfull in other contexts.")

(defconst scala-syntax:path-keywords-unsafe-re
  (regexp-opt '("super" "this") 'words))

(defconst scala-syntax:path-keywords-re
  (concat "\\(^\\|[^`]\\)\\(" scala-syntax:path-keywords-unsafe-re "\\)"))

(defconst scala-syntax:value-keywords-unsafe-re
  (regexp-opt '("false" "null" "true") 'words))

(defconst scala-syntax:value-keywords-re
  (concat "\\(^\\|[^`]\\)\\(" scala-syntax:value-keywords-unsafe-re "\\)"))

(defconst scala-syntax:other-keywords-unsafe-re
  (regexp-opt '("abstract" "case" "catch" "class" "def" "do" "else" "extends"
                "final" "finally" "for" "forSome" "if" "implicit" "import"
                "lazy" "match" "new" "object" "override" "package" "private"
                "protected" "return" "sealed" "throw" "trait" "try" "type"
                "val" "var" "while" "with" "yield") 'words))

(defconst scala-syntax:other-keywords-re
  (concat "\\(^\\|[^`]\\)\\(" scala-syntax:other-keywords-unsafe-re "\\)"))

(defconst scala-syntax:keywords-unsafe-re
  (concat "\\(" scala-syntax:path-keywords-unsafe-re
          "\\|" scala-syntax:value-keywords-unsafe-re
          "\\|" scala-syntax:other-keywords-unsafe-re
          "\\)"))

;; TODO: remove
;; (defconst scala-syntax:keywords-re
;;   (concat "\\(^\\|[^`]\\)\\(" scala-syntax:value-keywords-unsafe-re
;;           "\\|" scala-syntax:path-keywords-unsafe-re
;;           "\\|" scala-syntax:other-keywords-unsafe-re "\\)"))


(defconst scala-syntax:after-reserved-symbol-underscore-re
  (concat "$\\|" scala-syntax:comment-start-re
          "\\|[^" scala-syntax:letterOrDigit-group "]"))

(defconst scala-syntax:reserved-symbol-underscore-re
  ;; reserved symbol _
  (concat "\\(^\\|[^" scala-syntax:letterOrDigit-group "]\\)"
          "\\(_\\)"
          "\\(" scala-syntax:after-reserved-symbol-underscore-re "\\)"))

(defconst scala-syntax:reserved-symbols-unsafe-re
  ;; reserved symbols. The regexp is unsafe as it does not
  ;; check the context.
  "\\([:#@\u21D2\u2190]\\|=>?\\|<[:%!?\\-]\\|>:\\)" )

(defconst scala-syntax:double-arrow-unsafe-re
  "\\(=>\\|\u21D2\\)")

(defconst scala-syntax:after-reserved-symbol-re
  (concat "\\($\\|" scala-syntax:comment-start-re
          "\\|[^" scala-syntax:opchar-group "]\\)"))

(defconst scala-syntax:reserved-symbols-re
  ;; reserved symbols and XML starts ('<!' and '<?')
  (concat "\\(^\\|[^" scala-syntax:opchar-group "]\\)"
          scala-syntax:reserved-symbols-unsafe-re
          "\\(" scala-syntax:after-reserved-symbol-re "\\)"))

(defconst scala-syntax:colon-re
  (concat "\\(^\\|[^" scala-syntax:opchar-group "]\\)"
          "\\(:\\)"
          "\\(" scala-syntax:after-reserved-symbol-re "\\)"))


(defconst scala-syntax:modifiers-re
  (regexp-opt '("override" "abstract" "final" "sealed" "implicit" "lazy"
                "private" "protected") 'words))

(defconst scala-syntax:body-start-re
  (concat "=" scala-syntax:end-of-code-line-re)
  "A regexp for detecting if a line ends with '='")

(defconst scala-syntax:list-keywords-re
  (regexp-opt '("var" "val" "import") 'words)
  ("Keywords that can start a list"))

(defconst scala-syntax:multiLineStringLiteral-end-re
  "\"\"+\\(\"\\)")

(defconst scala-syntax:case-re
  "\\<case\\>")

(defconst scala-syntax:for-re
  "\\<for\\>")

(defconst scala-syntax:class-or-object-re
  (regexp-opt '("class" "object") 'words))


;;;;
;;;; Character syntax table and related syntax-propertize functions
;;;;

;;; The syntax table relies havily on the syntax-propertize-functions being
;;; run. Hence this syntax requires at least emacs 24, which introduced
;;; this new facility.

(defvar scala-syntax:syntax-table nil
  "Syntax table used in `scala-mode' buffers.")
(when (not scala-syntax:syntax-table)
  (let ((syntab (make-syntax-table)))
    ;; 1. start by reseting the syntax table: only (){}[] are
    ;; parentheses, so all others marked as parentheses in the parent
    ;; table must be marked as symbols, nothing is a punctuation
    ;; unless otherwise stated
    (map-char-table
     #'(lambda (key value)
         (when (or (= (syntax-class value) 4) ; open
                   (= (syntax-class value) 5) ; close
                   (= (syntax-class value) 1)) ; punctuation
           (modify-syntax-entry key "_" syntab)))
     (char-table-parent syntab))

    ;; Below 'space', everything is either illegal or whitespace.
    ;; Consider as whitespace, unless otherwise stated below.
    (modify-syntax-entry '(0 . 32) " " syntab)

    ;; The scala parentheses
    (modify-syntax-entry ?\( "()" syntab)
    (modify-syntax-entry ?\[ "(]" syntab)
    (modify-syntax-entry ?\{ "(}" syntab)
    (modify-syntax-entry ?\) ")(" syntab)
    (modify-syntax-entry ?\] ")[" syntab)
    (modify-syntax-entry ?\} "){" syntab)

    ;; _ is upper-case letter, but will be modified to be symbol
    ;; constituent when in reserved symbol position by
    ;; syntax-propertize-function
    (modify-syntax-entry ?\_ "w" syntab)

    ;; by default all opchars are punctuation, but they will be
    ;; modified by syntax-propertize-function to be symbol
    ;; constituents when a part of varid or capitalid
    (dolist (char (mapcar 'identity "!#%&*+/:<=>?@^|~-\u21D2\u2190")) ;; TODO: Sm, So
      (modify-syntax-entry char "." syntab))

    ;; for clarity, the \ is alone here and not in the string above
    (modify-syntax-entry ?\\ "." syntab)

    ;; scala strings cannot span lines, so we mark
    ;; " as punctuation, but do the real stuff
    ;; in syntax-propertize-function for properly
    ;; formatted strings.
    (modify-syntax-entry ?\" "." syntab)

    ;; backquote is given paired delimiter syntax so that
    ;; quoted ids are parsed as one sexp. Fontification
    ;; is done separately.
    (modify-syntax-entry ?\` "$" syntab)

    ;; ' is considered an expression prefix, since it can
    ;; both start a Symbol and is a char quote. It
    ;; will be given string syntax by syntax-propertize-function
    ;; for properly formatted char literals.
    (modify-syntax-entry ?\' "'" syntab)

    ;; punctuation as specified by SLS
    (modify-syntax-entry ?\. "." syntab)
    (modify-syntax-entry ?\; "." syntab)
    (modify-syntax-entry ?\, "." syntab)

    ;; comments
    ;; the `n' means that comments can be nested
    (modify-syntax-entry ?\/  ". 124b" syntab)
    (modify-syntax-entry ?\*  ". 23n"   syntab)
    (modify-syntax-entry ?\n  "> b" syntab)
    (modify-syntax-entry ?\r  "> b" syntab)

    (setq scala-syntax:syntax-table syntab)))

(defun scala-syntax:propertize-extend-region (start end)
  "See syntax-propertize-extend-region-functions"
  ;; nothing yet
  nil)

(defmacro scala-syntax:put-syntax-table-property (match-group value)
  "Add 'syntax-table entry 'value' to the region marked by the
match-group 'match-group'"
  `(put-text-property (match-beginning ,match-group)
                      (match-end ,match-group)
                      'syntax-table
                      ,value))

(defun scala-syntax:propertize-char-and-string-literals (start end)
  "Mark start and end of character literals as well as one-line
and multi-line string literals. One-line strings and characters
use syntax class 7 (string quotes), while multi-line strings are
marked with 15 (generic string delimiter). Multi-line string
literals are marked even if they are unbalanced. One-line string
literals have to be balanced to get marked. This means invalid
characters and one-line strings will not be fontified."

  (let* ((string-state (nth 3 (syntax-ppss start)))
         (unbalanced-p (eq string-state t)))

    (if (and string-state (not unbalanced-p))
        ;; a normal string is open, let's de-propertize
        (remove-text-properties start end '(syntax-table nil))
      (save-excursion
        (goto-char start)
        ;; close the closing for the unbalanced multi-line literal
        (when (and unbalanced-p
                   (re-search-forward scala-syntax:multiLineStringLiteral-end-re end t))
          (scala-syntax:put-syntax-table-property 1 '(15 . nil)))
        ;; match any balanced one-line or multi-line literals
        (catch 'break
          (while (re-search-forward
                  scala-syntax:relaxed-char-and-string-literal-re end t)
            ;; Expects the following groups:
            ;; group 1 = char start, 3 = char end
            ;; group 4 = multi-line string start, 6 = end
            ;; group 7 = string start, 9 = end
            (cond
             ((match-beginning 1)
              (scala-syntax:put-syntax-table-property 1 '(7 . nil))
              (scala-syntax:put-syntax-table-property 3 '(7 . nil)))
             ((match-beginning 4) ;; start of multi-line literal
              (scala-syntax:put-syntax-table-property 4 '(15 . nil))
              (if (match-beginning 6)
                  ;; balanced multi-line
                  (scala-syntax:put-syntax-table-property 6 '(15 . nil))
                ;; un-balanced multi-line
                (throw 'break nil)))
             ((or
               ;; normal string, content is not empty
               (match-beginning 8)
               ;; empty string at line end
               (= (match-end 9) (line-end-position))
               ;; no " after empty string
               (not (= (char-after (match-end 10)) ?\")))
              (when (save-excursion
                      (goto-char (match-beginning 7))
                      ;; really valid?
                      (looking-at-p scala-syntax:oneLineStringLiteral-re))
                (scala-syntax:put-syntax-table-property 7 '(7 . nil))
                (scala-syntax:put-syntax-table-property 9 '(7 . nil))))
             (t (throw 'break nil)))))))))

(defun scala-syntax:propertize-shell-preamble (start end)
  "Mark a shell preamble pair (#!/!#) at the beginning of a script as a comment."
  (save-excursion
    (let ((comment-start (nth 8 (syntax-ppss))))
      (goto-char start)
      (when (and (= start 1)
                 (looking-at scala-syntax:preamble-start-re))
        (scala-syntax:put-syntax-table-property 0 '(11 . nil))
        (setq comment-start 1))
      (when (and (eq comment-start 1)
                 (goto-char comment-start)
                 (looking-at scala-syntax:preamble-start-re)
                 (re-search-forward scala-syntax:preamble-end-re end t))
        (scala-syntax:put-syntax-table-property 1 '(12 . nil))))))

(defun scala-syntax:propertize-underscore-and-idrest (start end)
  "Mark all underscores (_) as symbol constituents (syntax 3) or
upper case letter (syntax 2). Also mark opchars in idrest as
symbol constituents (syntax 3)"
  (save-excursion
    (goto-char start)
    (while (re-search-forward "_" end t)
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0)))
        (put-text-property
         match-beg match-end 'syntax-table
         (if (= match-beg (line-beginning-position))
             (if (looking-at scala-syntax:after-reserved-symbol-underscore-re)
                 '(3 . nil) ; symbol constituent
               '(2 . nil)) ; word syntax
           (save-excursion
             (goto-char (1- match-beg))
             (if (looking-at scala-syntax:reserved-symbol-underscore-re)
                 '(3 . nil) ; symbol constituent
               ;; check for opchars that should be marked as symbol constituents (3)
               (goto-char match-end)
               (when (looking-at scala-syntax:op-re)
                 (scala-syntax:put-syntax-table-property 0 '(3 . nil)))
               '(3 . nil))))))))) ;; symbol constituent syntax (3) also for the '_'


(defun scala-syntax:propertize (start end)
  "See syntax-propertize-function"
  (scala-syntax:propertize-char-and-string-literals start end)
  (scala-syntax:propertize-shell-preamble start end)
  (scala-syntax:propertize-underscore-and-idrest start end))


;;;;
;;;; Syntax navigation functions
;;;;

(defun scala-syntax:beginning-of-code-line ()
  (interactive)
  "Move to the beginning of code on the line, or to the end of
the line, if the line is empty. Return the new point.  Not to be
called on a line whose start is inside a comment, i.e. a comment
begins on the previous line and continues past the start of this
line."
  ;; TODO: make it work even if the start IS inside a comment
  (beginning-of-line)
  (let ((eol (line-end-position))
        (pos (point)))

    (while (and (forward-comment 1)
                (< (point) eol))
      (setq pos (point)))
    ;; Now we are either on a different line or at eol.
    ;; Pos is the last point one the starting line.
    (if (> (point) eol)
        (goto-char pos)
      (skip-syntax-forward " " eol)
      (point))))

(defun scala-syntax:looking-at-varid-p (&optional point)
  "Return true if looking-at varid, and it is not the start of a
stableId"
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:skip-forward-ignorable)
    (let ((case-fold-search nil))
      (when (looking-at scala-syntax:varid-re)
        (save-match-data
          (if (or (= (char-after (match-end 0)) ?.)
                  (looking-at "\\<\\(this\\|super\\)\\>"))
              nil
            t))))))

(defun scala-syntax:looking-at-empty-line-p ()
  (save-excursion
    (or (bolp)
        (skip-syntax-forward " >" (1+ (line-end-position))))
    (looking-at scala-syntax:empty-line-re)))

(defun scala-syntax:looking-at-reserved-symbol (re &optional point)
  (interactive)
  (unless re (setq re scala-syntax:reserved-symbols-unsafe-re))
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:skip-forward-ignorable)
    (and (looking-at re)
         (goto-char (match-end 0))
         (looking-at-p scala-syntax:after-reserved-symbol-re))))

(defun scala-syntax:looking-at-case-p (&optional point)
  (save-excursion
    (when point (goto-char point))
    (scala-syntax:skip-forward-ignorable)
    (and (looking-at scala-syntax:case-re)
         (goto-char (match-end 0))
         (scala-syntax:skip-forward-ignorable)
         (not (looking-at-p scala-syntax:class-or-object-re)))))

(defun scala-syntax:looking-back-empty-line-p ()
  "Return t if the previous line is empty"
  (save-excursion
    (skip-syntax-backward " " (line-beginning-position))
    (and (bolp)
         (forward-line -1)
         (looking-at-p scala-syntax:empty-line-re))))

(defun scala-syntax:skip-forward-ignorable ()
  "Moves forward over ignorable whitespace and comments. A
completely empty line is not ignorable and will not be mobed over."
  (interactive)
  (save-match-data
    (while (and (not (scala-syntax:looking-at-empty-line-p))
                (forward-comment 1)))
    (skip-syntax-forward " " (line-end-position))))

(defun scala-syntax:skip-backward-ignorable ()
  "Move backwards over ignorable whitespace and comments. A
completely empty line is not ignorable and will not be moved
over. Returns the number of points moved (will be negative)."
  (save-match-data
    (while (and (not (scala-syntax:looking-back-empty-line-p))
                (forward-comment -1)))
    (skip-syntax-backward " " (line-beginning-position))))

(defun scala-syntax:looking-at (re)
  "Return the end position of the matched re, if the current
position is followed by it, or nil if not. All ignorable comments
and whitespace are skipped before matching."
  (save-excursion
    (scala-syntax:skip-forward-ignorable)
    (looking-at re)))

(defun scala-syntax:looking-back-token (re &optional max-chars)
  "Return the start position of the token matched by re, if the
current position is preceeded by it, or nil if not. All ignorable
comments and whitespace are ignored, i.e. does not search past an
empty line. Expects to be outside of comment. A limit for the
search is calculated based on max-chars. The function won't look
further than max-chars starting after skipping any ignorable."
  (save-excursion
    ;; skip back all comments
    (scala-syntax:skip-backward-ignorable)
    (let ((end (point))
          (limit (when max-chars (- (point) max-chars))))
      ;; skip back punctuation or ids (words and related symbols and delimiters)
      (if (or (/= 0 (skip-chars-backward scala-syntax:delimiter-group limit))
              (/= 0 (skip-syntax-backward "." limit))
              (/= 0 (skip-syntax-backward "(" limit))
              (/= 0 (skip-syntax-backward ")" limit))
              (/= 0 (skip-syntax-backward "w_'$" limit)))
          (if (looking-at re) (point) nil)
        nil))))

(defun scala-syntax:backward-parameter-groups ()
  "Move back over all parameter groups to the start of the first
one."
  (save-match-data
    (while (scala-syntax:looking-back-token "[])]" 1)
      (backward-list))))

(defun scala-syntax:forward-parameter-groups ()
  "Move back over all parameter groups to the end of the last
one."
  (save-match-data
    (while (scala-syntax:looking-at "[[(]")
      (forward-list))))

(defun scala-syntax:forward-modifiers ()
  "Move forward over any modifiers."
  (save-match-data
    (while (scala-syntax:looking-at scala-syntax:modifiers-re)
      (scala-syntax:forward-sexp)
      (when (scala-syntax:looking-at "[")
        (forward-list)))))

(defun scala-syntax:looking-back-else-if-p ()
  ;; TODO: rewrite using (scala-syntax:if-skipped (scala:syntax:skip-backward-else-if))
  (save-excursion
    (if (and (scala-syntax:looking-back-token "\\s)" 1)
             (backward-list)
             (prog1 (scala-syntax:looking-back-token "if")
               (goto-char (match-beginning 0)))
             (prog1 (scala-syntax:looking-back-token "else")
               (goto-char (match-beginning 0))))
        (point) nil)))

(defun scala-syntax:newlines-disabled-p (&optional point)
  "Return true if newlines are disabled at the current point (or
point 'point') as specified by SLS chapter 1.2"
  ;; newlines are disabled if
  ;; - in '()' or '[]'
  ;; - between 'case' and '=>'
  ;; - XML mode (not implemented here)
  (unless point (setq point (point)))
  (save-excursion
    (let* ((state (syntax-ppss point))
           (parenthesisPos (nth 1 state)))
      (when parenthesisPos ;; if no parenthesis, then this cannot be a case block either
        (goto-char parenthesisPos)
        (or
         ;; the trivial cases of being inside ( or [
         (= (char-after) ?\()
         (= (char-after) ?\[)
         ;; else we have to see about case
         (progn
           (forward-char)
           (forward-comment (buffer-size))
           (skip-syntax-forward " >")
           (when (looking-at scala-syntax:case-re)
             (let ((limit (match-beginning 0)))
               (goto-char (or (nth 8 state) point))
               ;; go to the start of => or 'case'
               (while (> (point) limit)
                 (scala-syntax:backward-sexp)
                 (when (or (looking-at scala-syntax:case-re)
                           (scala-syntax:looking-at-reserved-symbol
                            scala-syntax:double-arrow-unsafe-re))
                   (setq limit (point))))
               ;; unless we found '=>', check if we found 'case' (but
               ;; 'case class' or 'case object')
               (unless (scala-syntax:looking-at-reserved-symbol
                        scala-syntax:double-arrow-unsafe-re)
                 (scala-syntax:forward-sexp)

                 (and (<= (point) point) ;; check that we were inside in the first place
                      (progn (scala-syntax:skip-forward-ignorable)
                             (not (looking-at scala-syntax:class-or-object-re)))))))))))))

(defun scala-syntax:forward-sexp ()
  "Move forward one scala expression. It can be: paramter list (value or type),
id, reserved symbol, keyword, block, or literal. Delimiters (.,;)
and comments are skipped silently. Position is placed at the
beginning of the skipped expression."
  (interactive)
  ;; emacs knows how to properly skip: lists, varid, capitalid,
  ;; strings, symbols, chars, quotedid. What we have to handle here is
  ;; most of all ids made of op chars

  ;; skip comments, whitespace and scala delimiter chars .,; so we
  ;; will be at the start of something interesting
  (forward-comment (buffer-size))
  (while (< 0 (+ (skip-syntax-forward " ")
                 (skip-chars-forward scala-syntax:delimiter-group))))

  ;; emacs can handle everything but opchars
  (when (= (skip-syntax-forward ".") 0)
    (goto-char (or (scan-sexps (point) 1) (buffer-end 1)))))

(defun scala-syntax:backward-sexp ()
  "Move backward one scala expression. It can be: parameter
  list (value or type), id, reserved symbol, keyword, block, or
  literal. Delimiters (.,;) and comments are skipped
  silently. Position is placed at the beginning of the skipped
  expression."
  (interactive)
  ;; for implementation comments, see scala-syntax:forward-sexp
  (forward-comment (- (buffer-size)))
  (while (> 0 (+ (skip-syntax-backward " ")
                 (skip-chars-backward scala-syntax:delimiter-group))))

  (when (= (skip-syntax-backward ".") 0)
    (goto-char (or (scan-sexps (point) -1) (buffer-end -1)))
    (backward-prefix-chars)))

(defun scala-syntax:has-char-before (char end)
  (save-excursion
    (while (and (< (point) end)
                (or (bobp)
                    (/= (char-before) char)))
      (scala-syntax:forward-sexp))
    (when (= (char-before) char)
      (scala-syntax:skip-forward-ignorable)
      (> end (point)))))

(defun scala-syntax:search-backward-sexp (re)
  "Searches backward sexps until it reaches re, empty line or ;.
If re is found, point is set to beginning of re and the position
is returned, otherwise nil is returned"
  (let ((found (save-excursion
                 (while (not (or (bobp)
                                 (scala-syntax:looking-back-empty-line-p)
                                 (scala-syntax:looking-back-token "[;,]")
                                 (looking-at re)))
                   (scala-syntax:backward-sexp))
                 (if (looking-at re)
                     (point)
                   nil))))
    (when found (goto-char found))))

(defun scala-syntax:list-p (&optional point)
  "Returns the start of the list, if the current point (or point
'point') is on the first line of a list element > 1, or nil if
not. A list must be either enclosed in parentheses or start with
'val', 'var' or 'import'."
  (save-excursion
    ;; first check that the previous line ended with ','
    (when point (goto-char point))
    (scala-syntax:beginning-of-code-line)
    (when (scala-syntax:looking-back-token "," 1)
      (goto-char (match-beginning 0))
      (let ((parenpoint (nth 1 (syntax-ppss))))
        (if (and parenpoint (or (= (char-after parenpoint) ?\()
                                (= (char-after parenpoint) ?\[)))
            (1+ parenpoint)
          (ignore-errors ; catches when we get at parentheses
            (while (not (or (bobp)
                            (looking-at scala-syntax:list-keywords-re)
                            (scala-syntax:looking-back-empty-line-p)
                            (scala-syntax:looking-back-token ";")))
              (scala-syntax:backward-sexp)))
          (when (looking-at scala-syntax:list-keywords-re)
            (goto-char (match-end 0))))))))

(provide 'scala-mode2-syntax)
