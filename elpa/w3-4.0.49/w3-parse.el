;;; w3-parse.el --- Parse HTML and/or SGML for Emacs W3 browser

;; Copyright © 1993-1997, 2013 Free Software Foundation, Inc.

;; Author: Joe Wells <jbw@cs.bu.edu>
;; Created on: Sat Sep 30 17:25:40 1995

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;;
;; Trying to make the best of an evil speed hack.
;;

;; Explanation:

;; Basically, this file provides one big function (w3-parse-buffer) and
;; some data structures.  However, to avoid code redundancy, I have broken
;; out some common subexpressions of w3-parse-buffer into separate
;; functions.  I have declared these separate functions with "defsubst" so
;; they will be inlined into w3-parse-buffer.  Also, I have defined them
;; within eval-when-compile forms, so no definitions will be emitted into
;; the .elc file for these separate functions.  (They will work normally
;; when the uncompiled file is loaded.)

;; Each of these subfunctions use some scratch variables in a purely local
;; fashion.  In good software design, I would declare these variables as
;; close to their use as possible with "let".  However, "let"-binding
;; variables is *SLOW* in Emacs Lisp, even when compiled.  Since each of
;; these functions is executed one or more time during each iteration of
;; the main loop, I deemed this too expensive.  So the main function does
;; the "let"-binding of these variables.  However, I still want to declare
;; them close to their use, partially to keep the compiler from crying
;; "Wolf!" when there is no danger (well, maybe a little danger :-), so I
;; define some macros for this purpose.

;; Also, there are some variables which are updated throughout the file
;; (remember this is really all one function).  Some of the code which
;; updates them is located inside the subfunctions.  So that the compiler
;; will not complain, these variables are defined with defvar.

(require 'w3-vars)
(require 'url-parse)
(require 'url-history)
(autoload 'url-expand-file-name "url-expand")

(eval-when-compile (require 'cl))

(eval-when-compile
  (defvar w3-p-s-var-list nil
    "A list of the scratch variables used by functions called by
w3-parse-buffer which it is w3-parse-buffer's responsibility to
\"let\"-bind.")

  (defmacro w3-p-s-var-def (var)
    "Declare VAR as a scratch variable which w3-parse-buffer must
\"let\"-bind."
    (or (memq var w3-p-s-var-list)
        (setq w3-p-s-var-list (cons var w3-p-s-var-list)))
    `(defvar ,var))

  (defmacro w3-p-s-let-bindings (&rest body)
    "\"let\"-bind all of the variables in w3-p-s-var-list in BODY."
    (declare (indent 0) (debug t))
    `(let ,w3-p-s-var-list
         ,@body)))

(defvar w3-p-d-current-element)
(put 'w3-p-d-current-element 'variable-documentation
     "Information structure for the current open element.")
  
(defvar w3-p-d-exceptions)
(put 'w3-p-d-exceptions 'variable-documentation
     "Alist specifying elements (dis)allowed because of an (ex|in)clusion
exception of some containing element (not necessarily the immediately
containing element).  Each item specifies a transition for an element
which overrides that specified by the current element's content model.
Each item is of the form (TAG ACTION *same ERRORP).")
  
(defvar w3-p-d-in-parsed-marked-section)
(put 'w3-p-d-in-parsed-marked-section 'variable-documentation
     "Are we in a parsed marked section so that we have to scan for \"]]>\"?")

(defvar w3-p-d-non-markup-chars)
(put 'w3-p-d-non-markup-chars 'variable-documentation
     "The characters that do not indicate the start of markup, in the format
for an argument to skip-chars-forward.")

(defvar w3-p-d-null-end-tag-enabled)
(put 'w3-p-d-null-end-tag-enabled 'variable-documentation
     "Is the null end tag (\"/\") enabled?")

(defvar w3-p-d-open-element-stack)
(put 'w3-p-d-open-element-stack 'variable-documentation
     "A stack of the currently open elements, with the innermost enclosing
element on top and the outermost on bottom.")

(defvar w3-p-d-shortrefs)
(put 'w3-p-d-shortrefs 'variable-documentation
     "An alist of the magic entity reference strings in the current
between-tags region and their replacements.  Each item is of the format
\(REGEXP . REPLACEMENT-STRING\).  Although in SGML shortrefs normally name
entities whose value should be used as the replacement, we have
preexpanded the entities for speed.  We have also regexp-quoted the
strings to be replaced, so they can be used with looking-at.  This should
never be in an element's overrides field unless
w3-p-d-shortref-chars is also in the field.")
  
(defvar w3-p-d-shortref-chars)
(put 'w3-p-d-shortref-chars 'variable-documentation
     "A string of the characters which can start shortrefs in the current
between-tags region.  This must be in a form which can be passed to
skip-chars-forward and must contain exactly the characters which start the
entries in w3-p-d-shortrefs.  If this variable is mentioned in the
overrides field of an element, its handling is magical in that the
variable w3-p-d-non-markup-chars is saved to the element's undo-list and
updated at the same time.  This should never be in an element's overrides
field unless w3-p-d-shortrefs is also in the field.")
  
(defvar w3-p-d-tag-name)
(put 'w3-p-d-tag-name 'variable-documentation
     "Name of tag we are looking at, as an Emacs Lisp symbol.
Only non-nil when we are looking at a tag.")

(defvar w3-p-d-end-tag-p)
(put 'w3-p-d-end-tag-p 'variable-documentation
     "Is the tag we are looking at an end tag?
Only non-nil when we are looking at a tag.")

;;;
;;; HTML syntax error messages.
;;;

(eval-when-compile

  (defvar w3-p-d-debug-url)
  (put 'w3-p-d-debug-url 'variable-documentation
       "Whether to print the URL being parsed before an error messages.
Only true for the first error message.")

  ;; The level parameter indicates whether the error is (1) very
  ;; serious, must be displayed to all users, (2) invalid HTML, but the
  ;; user should only be told if the user has indicated interest, or (3)
  ;; valid HTML which is bad because it appears to rely on the way certain
  ;; browsers will display it, which should only be displayed to the user
  ;; if they have really asked for it.
  
  (defmacro w3-debug-html (&rest body)
    "Emit a warning message.
These keywords may be used at the beginning of the arguments:
  :mandatory-if sexp -- force printing if sexp evaluates non-nil.
  :bad-style         -- do not print unless w3-debug-html is 'style.
  :outer             -- do not include the current element in the element
                        context we report. 
  :nocontext         -- do not include context where error detected.
The remaining parameters are treated as the body of a progn, the value of
which must be a string to use as the error message."
    (let (mandatory-if bad-style outer nocontext condition)
      (while (memq (car body) '(:mandatory-if :bad-style :outer :nocontext))
        (cond ((eq ':mandatory-if (car body))
               (setq mandatory-if (car (cdr body)))
               (setq body (cdr (cdr body))))
              ((eq ':bad-style (car body))
               (setq bad-style t)
               (setq body (cdr body)))
              ((eq ':nocontext (car body))
               (setq nocontext t)
               (setq body (cdr body)))
              ((eq ':outer (car body))
               (setq outer t)
               (setq body (cdr body)))))
      (setq condition (if bad-style
                          '(eq 'style w3-debug-html)
                        'w3-debug-html))
      (if mandatory-if
          (setq condition
                `(or ,mandatory-if
                       ,condition)))
      `(if ,condition
             (let ((message (progn ,@body)))
               (if message
                   (w3-debug-html-aux message
                                      ,@(if nocontext
                                              (list outer nocontext)
                                            (if outer '(t)))))))))

  ;; This is unsatisfactory.
  (put 'w3-debug-html 'lisp-indent-function 0)
  
  (put 'w3-debug-html 'edebug-form-spec
       '([&rest &or ":nocontext" ":outer" [":mandatory-if" form] ":bad-style"]
         &rest form))
  )

(defun w3-debug-html-aux (message &optional outer nocontext)
  (push (if nocontext
            message
          (concat message
                  ;; Display context information for each error
                  ;; message.
                  "\n  Containing elements: "
                  (w3-open-elements-string (if outer 1))
                  (concat
                   "\n  Text around error: "
                   (save-restriction
                     (widen)
                     (progn
                       (insert "*ERROR*")
                       (prog1
                           (w3-quote-for-string
                            (buffer-substring 
                             (max (- (point) 27) (point-min))
                             (min (+ (point) 20) (point-max))))
                         (delete-char -7))))))) w3-current-badhtml))

(defun w3-quote-for-string (string)
  (with-current-buffer (get-buffer-create " w3-quote-whitespace")
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (insert "\"")
    (while (progn
             (skip-chars-forward "^\"\\\t\n\r")
             (not (eobp)))
      (insert "\\" (cdr (assq (char-after (point)) '((?\" . "\"")
                                                     (?\\ . "\\")
                                                     (?\t . "t")
                                                     (?\n . "n")
                                                     (?\r . "r")))))
      (delete-char 1))
    (insert "\"")
    (buffer-string)))


;;;
;;; General entity references and numeric character references.
;;;

;; *** I18N HTML support?

;; It's perhaps better to use a suitable display table for these
;; things.  -- fx
(defconst w3-invalid-sgml-char-replacement
  `((128 "euro" 8364) ;; U+20AC  EURO SIGN
    (130 "," 8218) ;; U+201A  SINGLE LOW-9 QUOTATION MARK
    (131 "_f" 402) ;; U+0192  LATIN SMALL LETTER F WITH HOOK
    (132 ",,"8222) ;; U+201E  DOUBLE LOW-9 QUOTATION MARK
    (133 "..." 8230) ;; U+2026  HORIZONTAL ELLIPSIS
    (134 "(dagger)" 8224) ;; U+2020  DAGGER
    (135 "(double dagger)" 8225) ;; U+2021  DOUBLE DAGGER
    (136 ?^ 710) ;; U+02C6  MODIFIER LETTER CIRCUMFLEX ACCENT
    (137 "%o" 8240) ;; U+2030  PER MILLE SIGN
    (138 "S\\v" 352) ;; U+0160  LATIN CAPITAL LETTER S WITH CARON
    (139 ?\< 8249) ;; U+2039  SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    (140 "OE" 338) ;; U+0152  LATIN CAPITAL LIGATURE OE
    (142 "Z\\v" 381) ;; U+017D  LATIN CAPITAL LETTER Z WITH CARON
    (145 ?\` 8216) ;; U+2018  LEFT SINGLE QUOTATION MARK
    (146 ?\' 8217) ;; U+2019  RIGHT SINGLE QUOTATION MARK
    (147 "``" 8220) ;; U+201C  LEFT DOUBLE QUOTATION MARK
    (148 "''" 8221) ;; U+201D  RIGHT DOUBLE QUOTATION MARK
    (149 ?o 8226) ;; U+2022  BULLET
    (150 ?- 8211) ;; U+2013  EN DASH
    (151 "--" 8212) ;; U+2014  EM DASH
    (152 ?~ 732) ;; U+02DC  SMALL TILDE
    (153 "(TM)" 8482) ;; U+2122  TRADE MARK SIGN
    (154 "s\\v" 353) ;; U+0161  LATIN SMALL LETTER S WITH CARON
    (155 ?\> 8250) ;; U+203A  SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    (156 "oe" 339) ;; U+0153  LATIN SMALL LIGATURE OE
    (158 "z\\v" 382) ;; U+017E  LATIN SMALL LETTER Z WITH CARON
    (159 "Y\\.." 376) ;; U+0178  LATIN CAPITAL LETTER Y WITH DIAERESIS
    )
  "Replacements for SGML numeric character references between 128 and 159.
\(Such entities are not valid graphic charcters and are assumed to
come from the cp1252 character set rather than Unicode.)  This is an
alist indexed by numeric code.  The cdr of each element is a list of
an ASCII substitute and the Unicode for the cp1252 character.")

(defalias 'w3-int-to-char
  (if (fboundp 'int-to-char)            ; XEmacs
      (lambda (c)
        (cond
         ((characterp c)
          c)
         ((char-int-p c)
          (int-to-char c))
         (t
          ?~)))
    #'identity))

;; For older Mule-UCS.  This is from Mule-UCS 0.84.
(if (and (not (fboundp 'decode-char))
         (fboundp 'mucs-get-representation-decoding-backend))
    (defun decode-char (representation object &optional restriction)
      "Return a character represented by OBJECT in view of REPRESENTATION.
Return nil if OBJECT cannot be mapped to only one character.
Available representation list can be obtained by mucs-representation-list.
Optional argument RESTRICTION specifies a way to map OBJECT to
a character.  Its interpretation depends on the given
REPRESENTATION.  If not specified, the default restriction of REPRESENTATION
is used."
      (let ((fs (mucs-get-representation-decoding-backend
                 representation restriction))
            ret)
        (while
            (and fs
                 (not (setq ret
                            (funcall
                             (car fs)
                             representation object restriction))))
          (setq fs (cdr fs)))
        ret)))

(defun w3-resolve-numeric-char (code)
  "Return a representation of the numeric character reference CODE.
This may be a string or a character.  CODE is always interpreted as a
Unicode.  A Unicode character is returned if function `decode-char' is
available.  Codes in the range [128,160] are substituted using
`w3-invalid-sgml-char-replacement'."
  ;; Maybe fall back to something like `(format "&%d;" code)' instead
  ;; of ?~.
  (if (fboundp 'decode-char)
      (progn (if (and (< code 160) (> code 128))
                 (setq code
                       (or (nth 2 (assq code w3-invalid-sgml-char-replacement))
                           code)))
             (or (decode-char 'ucs code) ?~))
    (w3-int-to-char (cond ((<= code 127)
                           code)
                          ((<= code 255)
                           (if (fboundp 'make-char)
                               (make-char 'latin-iso8859-1 (- code 128))
                             code))
                          (t ?~)))))

(let ((html-entities w3-html-entities))
  (while html-entities
    (put (car (car html-entities)) 'html-entity-expansion
	 (cons 'CDATA (if (integerp (cdr (car html-entities)))
                          (let ((ent (w3-resolve-numeric-char
                                      (cdr (car html-entities)))))
                            (unless (stringp ent)
                              (char-to-string ent)))
			(cdr (car html-entities)))))
    (setq html-entities (cdr html-entities))))

;; These are the general entities in HTML 3.0 in terms of which the math
;; shortrefs are defined:
;; 
;;   <!ENTITY REF1   STARTTAG   "SUP">
;;   <!ENTITY REF2   ENDTAG     "SUP">
;;   <!ENTITY REF3   STARTTAG   "SUB">
;;   <!ENTITY REF4   ENDTAG     "SUB">
;;   <!ENTITY REF5   STARTTAG   "BOX">
;;   <!ENTITY REF6   ENDTAG     "BOX">
;; 
;; We're ignoring them because these names should really be local to the
;; DTD and not visible in the document.  They might change at any time in
;; future HTML standards.

;; <!--Entities for language-dependent presentation (BIDI and contextual analysis) -->
;; <!ENTITY zwnj CDATA "&#8204;"-- zero width non-joiner-->
;; <!ENTITY zwj  CDATA "&#8205;"-- zero width joiner-->
;; <!ENTITY lrm  CDATA "&#8206;"-- left-to-right mark-->
;; <!ENTITY rlm  CDATA "&#8207;"-- right-to-left mark-->

;; Entity names are case sensitive!

;; & should only be recognized when followed by letter or # and
;; digit or # and letter.

(eval-when-compile (defvar w3-invalid-sgml-char-replacement))
(eval-when-compile

  (w3-p-s-var-def w3-p-s-entity)
  (w3-p-s-var-def w3-p-s-pos)
  (w3-p-s-var-def w3-p-s-num)
  ;; Destroys free variables:
  ;;   w3-p-s-entity, w3-p-s-pos, w3-p-s-num
  ;; Depends on case-fold-search being t.
  (defsubst w3-expand-entity-at-point-maybe ()
    ;; We are looking at a &.
    ;; Only &A or &#1 or &#A syntax is special.
    (cond
     ((and (looking-at "&\\([a-z][-a-z0-9.]*\\)[\ ;\n]?") ; \n should be \r
           ;; We are looking at a general entity reference, maybe undefined.
           (setq w3-p-s-entity
                 (get 
                  (intern (buffer-substring (match-beginning 1) (match-end 1)))
                  'html-entity-expansion)))

      ;; If the reference was undefined, then for SGML, we should really
      ;; issue a warning and delete the reference.  However, the HTML
      ;; standard (contradicting the SGML standard) says to leave the
      ;; undefined reference in the text.
    
      ;; We are looking at a defined general entity reference.
      (replace-match "")
      (cond ((eq 'CDATA (car w3-p-s-entity))
             ;; Leave point after expansion so we don't rescan it.
             (insert (cdr w3-p-s-entity)))
            ((memq (car w3-p-s-entity) '(nil STARTTAG ENDTAG MS MD))
             ;; nil is how I mark ordinary entities.
             ;; The replacement text gets rescanned for all of these.
             (setq w3-p-s-pos (point))
             (insert (cdr (assq (car w3-p-s-entity)
                                '((nil . "")
                                  (STARTTAG . "<")
                                  (ENDTAG . "</")
                                  (MS . "<![")
                                  (MD . "<!"))))
                     (cdr w3-p-s-entity)
                     (cdr (assq (car w3-p-s-entity)
                                '((nil . "")
                                  (STARTTAG . ">")
                                  (ENDTAG . ">")
                                  (MS . "]]>")
                                  (MD . ">")))))
             (goto-char w3-p-s-pos)
             ;; *** Strictly speaking, if we parse anything from the
             ;; replacement text, it must end before the end of the
             ;; replacement text.
             )
            ((eq 'SDATA (car w3-p-s-entity))
             (insert "[Unimplemented SDATA \"%s\"]" (cdr w3-p-s-entity)))
            ((eq 'PI (car w3-p-s-entity))
             ;; We are currently ignoring processing instructions.
             ;; *** Strictly speaking, we should issue a warning if this
             ;; occurs in a attribute value.
             )
            (t
             ;; *** We don't handle external entities yet.
             (error "[Unimplemented entity: \"%s\"]" w3-p-s-entity))))

;;; What was this regexp supposed to be?
;;;     ((looking-at "&#[0-9][0-9]*\\([\   ;\n]?\\)") ; \n should be \r
     ((looking-at "&#[0-9]+\\([ ;\n]?\\)") ; \n should be \r
      ;; We are looking at a numeric character reference.
      ;; Ensure the number is already terminated by a semicolon or carriage
      ;; return so we can use "read" to get it as a number quickly.
      (cond ((= (match-beginning 1) (match-end 1))
             ;; This is very uncommon, so we don't have to be quick here but
             ;; rather correct.
             (save-excursion
               (goto-char (match-end 0)) ; same as match-end 1
               (insert ?\;))
             ;; Set up the match data properly
             (looking-at "&#[0-9]+;")))
      (forward-char 2)
      (setq w3-p-s-num (read (current-buffer)))
      ;; Always leave point after the expansion of a numeric character
      ;; reference, like it were a CDATA entity.  Don't zap a
      ;; delimiter other than `;'.
      (if (eq ?\; (char-before (match-end 0)))
          (replace-match "")
        (replace-match (match-string 1))
        (backward-char 1))
      ;; The condition-case is probably not necessary now.
      (condition-case ()
          (insert (w3-resolve-numeric-char w3-p-s-num))
        (error (insert "~"))))
     ((looking-at "&#x\\([0-9a-f]+\\)\\([ ;\n]?\\)")
      ;; Similarly to above, but for hex numbers.
      (cond ((= (match-beginning 2) (match-end 2))
             (save-excursion
               (goto-char (match-end 0))
               (insert ?\;))
             (looking-at "&#x[0-9a-f]+;")))
      (setq w3-p-s-num (string-to-number (match-string 1) 16))
      (if (eq ?\; (char-before (match-end 0)))
          (replace-match "")
        (replace-match (match-string 2))
        (backward-char 1))
      (condition-case ()
          (insert (w3-resolve-numeric-char w3-p-s-num))
        (error (insert "~"))))
     ((looking-at "&#\\(re\\|rs\\|space\\|tab\\)[\ ;\n]?") ; \n should be \r
      (replace-match (assq (upcase (char-after (+ 3 (point))))
                           '(;; *** Strictly speaking, record end should be
                             ;; carriage return.
                             (?E . "\n") ; RE
                             ;; *** And record start should be line feed.
                             (?S . "")  ; RS
                             (?P . " ") ; SPACE
                             (?A . "\t")))) ; TAB
      ;; Leave point after the expansion of a character reference, so it
      ;; doesn't get rescanned.
      ;; *** Strictly speaking, we should issue a warning for &#foo; if foo
      ;; is not a function character in the SGML declaration.
      )
   
     ((eq ?& (char-after (point)))
      ;; We are either looking at an undefined reference or a & that does
      ;; not start a reference (in which case we should not have been called).
      ;; Skip over the &.
      (forward-char 1))
   
     (t
      ;; What is the code doing calling us if we're not looking at a "&"?
      (error "this should never happen"))))

  )


;;;
;;; Syntax table used in markup declarations.
;;;

(defvar w3-sgml-md-syntax-table
  (let ((table (make-syntax-table))
        (items '(
                 (0   "."    255)       ; clear everything
                 (?\r " ")
                 (?\t " ")
                 (?\n " ")
                 (32  " ")              ; space
                 (?<  "\(>")
                 (?>  "\)<")
                 (?\( "\(\)")
                 (?\) "\)\(")
                 (?\[ "\(\]")
                 (?\] "\)\[")
                 (?\" "\"")
                 (?\' "\"")
                 (?a  "w"    ?z)
                 (?A  "w"    ?Z)
                 (?0  "w"    ?9)
                 (?.  "w")
                 ;; "-" can be a character in a NAME, but it is also used in
                 ;; "--" as both a comment start and end within SGML
                 ;; declarations ("<!"  ... ">").  In HTML, it is only used
                 ;; as a NAME character in the parameter entities
                 ;; Content-Type, HTTP-Method, and style-notations and in
                 ;; the attribute name http-equiv and in the notation names
                 ;; dsssl-lite and w3c-style.  We would like to be able to
                 ;; train Emacs to skip over these kinds of comments with
                 ;; forward-sexp and backward-sexp.  Is there any way to
                 ;; teach Emacs how to do this?  It doesn't seem to be the
                 ;; case.
                 (?-  "w")
                 )))
    (while items
      (let* ((item (car items))
             (char (car item))
             (syntax (car (cdr item)))
             (bound (or (car-safe (cdr-safe (cdr item)))
                        char)))
        (while (<= char bound)
          (modify-syntax-entry char syntax table)
          (setq char (1+ char))))
      (setq items (cdr items)))
    table)
  "A syntax table for parsing SGML markup declarations.")


;;;
;;; Element information data type.
;;;

;;   The element information data type is used in two ways:
;;
;;     * To store the DTD, there is one element record for each element in
;;       the DTD.
;;
;;     * To store information for open elements in the current parse tree.
;;       Each such element is initialized by copying the element record
;;       from the DTD.  This means that values in the fields can not be
;;       destructively altered, although of course the fields can be
;;       changed.

;;   The cells in this vector are:
;;
;;   name: the element's name (a generic identifier).
;;
;;   end-tag-name: a symbol whose name should be the result of prefixing
;;   the generic-identifier with a slash.  This is a convenience value for
;;   interfacing with the display engine which expects a stream of start
;;   and end tags in this format rather than a tree.
;;
;;   content-model: a data structure describing what elements or character
;;   data we expect to find within this element.  This is either a symbol
;;   listed here:
;;
;;     EMPTY: no content, no end-tag allowed.
;;     CDATA: all data characters until "</[a-z]" is seen.
;;     XCDATA: special non-SGML-standard mode which includes all data
;;       characters until "</foo" is seen where "foo" is the name of this
;;       element. (for XMP and LISTING)
;;     XXCDATA: special non-SGML-standard mode which includes all data
;;       until end-of-entity (end-of-buffer for us). (for PLAINTEXT)
;;     RCDATA: all data characters until "</[a-z]" is seen, except that
;;       entities are expanded first, although the expansions are not
;;       scanned for end-tags.
;;     XINHERIT: special non-SGML-standard mode which means to use the
;;       content model of the containing element instead.
;;  
;;   or a vector of this structure:
;;
;;     [(INCLUDES INCSPACEP (((TAG ...) . TRANSITION) ...) DEFAULT) ...]
;;
;;   where INCLUDES is of the format:
;;
;;     (TAG ...)
;;
;;   where each TRANSITION is one of these:
;;
;;     (ACTION NEW-STATE ERRORP)
;;     (ACTION NEW-STATE)
;;     (ACTION)
;;    
;;   where DEFAULT is one of these:
;;
;;     nil  or  TRANSITION
;;
;;   where the meaning of the components is:
;;
;;     INCLUDES is a list of tags for which the transition (*include *same
;;     nil) applies.
;;
;;     DEFAULT if non-nil is a transition that should be taken when
;;     matching any possibility not explicitly listed in another
;;     TRANSITION, except for data characters containing only whitespace.
;;
;;     INCSPACEP specifies how to handle data characters which include
;;     only whitespace characters.  The value is non-nil to indicate
;;     (*include *same nil) or nil to indicate (*discard *same nil).
;;    
;;     TAG is a symbol corresponding to the start-tag we are looking at,
;;     or *data when seeing character data that includes at least one
;;     non-space character.
;;
;;     ACTION is one of:
;;       *close: Close this element and try again using content model of
;;           enclosing element.  (Note that this does not apply to the
;;           case of an element being closed by its own end-tag.)
;;       *include: Process new element as subelement of this one or
;;           include data characters directly.
;;       *discard: Discard a start-tag or data characters.
;;       *retry: Try again after processing NEW-STATE and ERRORP.
;;       ELEMENT: Open ELEMENT (with default attributes), then try again
;;           using its content model. 
;;
;;     NEW-STATE (optional, default *same) is the index of the state to
;;     move to after processing the element or one of these:
;;       *same: no state change occurs.
;;       *next: change the current state + 1.
;;     The initial state is 0.  NEW-STATE does not matter if ACTION is
;;     *close.
;;    
;;     ERRORP (optional, default nil) if non-nil indicates this transition
;;     represents an error.  The error message includes this value if it
;;     is a string.
;;
;;   If no matching transition is found, the default transition is
;;   (*discard *same "not allowed here").
;;
;;   overrides: An alist of pairs of the form (VAR REPLACEP . VALUE).
;;   When this element is opened, the old value of VAR is saved in the
;;   undo-list.  If REPLACEP is non-nil, then VAR gets value VALUE,
;;   otherwise VAR gets value (append VALUE (symbol-value VAR)).  Useful
;;   values for VAR are:
;;
;;     w3-p-d-exceptions: See doc string.
;;  
;;     w3-p-d-shortrefs: See doc string.
;;
;;     w3-p-d-shortref-chars: See doc string.
;;
;;   end-tag-omissible: Whether it is legal to omit the end-tag of this
;;   element.  If an end-tag is inferred for an element whose end tag is
;;   not omissible, an error message is given.
;;
;;   state: The current state in the content model.  Preset to the initial
;;   state of 0.
;;
;;   undo-list: an alist of of former values of local variables
;;   of w3-parse-buffer to restore upon closing this element.  Each
;;   item on the list is of the format (VAR . VALUE-TO-RESTORE). 
;;
;;   attributes: an alist of attributes and values.  Each item on
;;   this list is of the format (ATTRIBUTE-NAME . VALUE).  Each
;;   ATTRIBUTE-NAME is a symbol and each attribute value is a
;;   string.
;;
;;   content: a list of the accumulated content of the element.  While the
;;   element is open, the list is in order from latest to earliest,
;;   otherwise it is in order from earliest to latest.  Each member is
;;   either a string of data characters or a list of the form (NAME
;;   ATTRIBUTES CONTENT), where NAME is the subelement's name, ATTRIBUTES
;;   is an alist of the subelement's attribute names (lowercase symbols)
;;   and their values (strings), and CONTENT is the subelement's content.

(eval-when-compile

  (defconst w3-element-fields
    '(name end-tag-name content-model state overrides undo-list
           content attributes end-tag-omissible deprecated))

  (let* ((fields w3-element-fields)
         (index (1- (length fields))))
    (while fields
      (let* ((field (symbol-name (car fields)))
             (get-sym (intern (concat "w3-element-" field)))
             (set-sym (intern (concat "w3-set-element-" field))))
        (eval `(progn
                   (defmacro ,get-sym (element)
                     (list 'aref element ,index))
                   (defmacro ,set-sym (element value)
                     (list 'aset element ,index value)))))
      (setq fields (cdr fields))
      (setq index (1- index))))

  (defmacro w3-make-element ()
    (list 'make-vector (length w3-element-fields) nil))

  ;; *** move this to be with DTD declaration.
  (defmacro w3-fresh-element-for-tag (tag)
    `(copy-sequence
        (or (get ,tag 'html-element-info)
            (error "unimplemented element %s"
                   (w3-sgml-name-to-string ,tag)))))

  ;; *** move this to be with DTD declaration.
  (defmacro w3-known-element-p (tag)
    `(get ,tag 'html-element-info))
  
  (defsubst w3-sgml-name-to-string (sym)
    (upcase (symbol-name sym)))
  
  )


;;;
;;; Parse tree manipulation.
;;;

;;    ;; Find the name of the previous element or a substring of the
;;    ;; preceding data characters.
;;    (let ((content (w3-element-content (car stack))))
;;      (while content
;;        (cond
;;         ((and (stringp (car content))
;;               (not (string-match "\\`[ \t\n\r]*\\'" (car content))))
;;          (setq prior-item (car content))
;;          ;; Trim trailing whitespace
;;          (if (string-match "\\(.*[^ \t\n\r]\\)[ \t\n\r]*\\'" prior-item)
;;              (setq prior-item (substring prior-item 0 (match-end 1))))
;;          (if (> (length prior-item) 8)
;;              (setq prior-item (concat "..." (substring prior-item -8))))
;;          (setq prior-item (w3-quote-for-string prior-item))
;;          (setq prior-item (concat "\(after " prior-item "\)"))
;;          (setq content nil))
;;         ((and (consp (car content))
;;               (symbolp (car (car content))))
;;          (setq prior-item
;;                (concat "\(after "
;;                        (w3-sgml-name-to-string (car (car content)))
;;                        "\)"))
;;          (setq content nil))
;;         (t
;;          (setq content (cdr content))))))

;; Only used for HTML debugging.
(defun w3-open-elements-string (&optional skip-count)
  (let* ((stack (nthcdr (or skip-count 0)
                        (cons w3-p-d-current-element
                              w3-p-d-open-element-stack)))
         ;;(prior-item "(at start)")
         result)
    ;; Accumulate the names of the enclosing elements.
    (while stack
      (let ((element (w3-element-name (car stack))))
        (if (eq '*holder element)
            nil
          ;; Only include *DOCUMENT if there are no other elements.
          (if (or (not (eq '*document element))
                  (null result))
              (setq result (cons (w3-sgml-name-to-string element)
                                 result)))))
      (setq stack (cdr stack)))
    (setq result (mapconcat 'identity result ":"))
    (if result
        ;;(concat
         result
        ;; prior-item)
      "[nowhere!]")))

;; *** This doesn't really belong here, but where?
(eval-when-compile
  (defmacro w3-invalid-sgml-chars ()
    "Characters not allowed in an SGML document using the reference
concrete syntax (i.e. HTML).  Returns a string in the format expected by
skip-chars-forward."
    "\000-\010\013\014\016-\037\177-\237"))

(eval-when-compile
  ;; Uses:
  ;;   w3-p-d-null-end-tag-enabled, w3-p-d-in-parsed-marked-section,
  ;;   w3-p-d-shortref-chars
  ;; Modifies free variable:
  ;;   w3-p-d-non-markup-chars
  (defsubst w3-update-non-markup-chars ()
    (setq w3-p-d-non-markup-chars
          (concat "^&<"
                  (w3-invalid-sgml-chars)
                  (if w3-p-d-null-end-tag-enabled "/" "")
                  (if w3-p-d-in-parsed-marked-section "]" "")
                  (or w3-p-d-shortref-chars ""))))
)

(eval-when-compile
  (w3-p-s-var-def w3-p-s-overrides)
  (w3-p-s-var-def w3-p-s-undo-list)
  (w3-p-s-var-def w3-p-s-var)
  ;; Uses free variables:
  ;;   w3-p-d-non-markup-chars
  ;; Modifies free variables:
  ;;   w3-p-d-current-element, w3-p-d-open-element-stack
  ;; Destroys free variables:
  ;;   w3-p-s-overrides, w3-p-s-undo-list, w3-p-s-var
  (defsubst w3-open-element (tag attributes)

    ;; Push new element on stack.
    (setq w3-p-d-open-element-stack (cons w3-p-d-current-element
                                          w3-p-d-open-element-stack))
    (setq w3-p-d-current-element (w3-fresh-element-for-tag tag))
    
    ;; Warn if deprecated or obsolete.
    (if (w3-element-deprecated w3-p-d-current-element)
        (w3-debug-html :outer
          (format "%s element %s."
                  (if (eq 'obsolete
                          (w3-element-deprecated w3-p-d-current-element))
                      "Obsolete"
                    "Deprecated")
                  (w3-sgml-name-to-string
                   (w3-element-name w3-p-d-current-element)))))
    
    ;; Store attributes.
    ;; *** we are not handling #CURRENT attributes (HTML has none).
    (w3-set-element-attributes w3-p-d-current-element attributes)
    ;; *** Handle default attribute values.
    ;; *** Fix the attribute name for unnamed values.  Right now they will
    ;; be in the attribute list as items of the format (VALUE . VALUE) where
    ;; both occurrences of VALUE are the same.  The first one needs to be
    ;; changed to the proper attribute name by consulting the DTD.
    ;; ********************
  
    ;; Handle syntax/semantics overrides of new current element.
    (cond ((w3-element-overrides w3-p-d-current-element)
           (setq w3-p-s-overrides
                 (w3-element-overrides w3-p-d-current-element))
           (setq w3-p-s-undo-list nil)
           (while w3-p-s-overrides
             (setq w3-p-s-var (car (car w3-p-s-overrides)))
             (setq w3-p-s-undo-list
                   (cons (cons w3-p-s-var
                               (symbol-value w3-p-s-var))
                         w3-p-s-undo-list))
             (set w3-p-s-var (if (car (cdr (car w3-p-s-overrides)))
                                 (cdr (cdr (car w3-p-s-overrides)))
                               (append (cdr (cdr (car w3-p-s-overrides)))
                                       (symbol-value w3-p-s-var))))
             ;; *** HACK HACK.
             ;; Magic handling of w3-p-d-shortref-chars.
             (cond ((eq 'w3-p-d-shortref-chars w3-p-s-var)
                    (setq w3-p-s-undo-list
                          (cons (cons 'w3-p-d-non-markup-chars
                                      w3-p-d-non-markup-chars)
                                w3-p-s-undo-list))
                    (w3-update-non-markup-chars)))
             (setq w3-p-s-overrides (cdr w3-p-s-overrides)))
           (w3-set-element-undo-list w3-p-d-current-element
                                     w3-p-s-undo-list)))
  
    ;; Handle content-model inheritance.  (Very non-SGML!)
    (if (eq 'XINHERIT (w3-element-content-model w3-p-d-current-element))
        (w3-set-element-content-model
         w3-p-d-current-element 
         (w3-element-content-model (car w3-p-d-open-element-stack))))
  
    )
  )

;; The protocol for handing items to the display engine is as follows.
;;
;; For an element, send (START-TAG . ATTS), each member of the content,
;; and (END-TAG . nil) if the element is allowed to have an end tag.
;;
;; For data characters, send (text . DATA-CHARACTERS).
;;
;; Exceptions:
;;
;; For PLAINTEXT, STYLE, XMP, TEXTAREA send:
;; (START-TAG . ((data . DATA-CHARACTERS) . ATTS)).
;;
;; *** This requires somehow eliminating any subelements of the TEXTAREA
;; element.  TEXTAREA can contain subelements in HTML 3.0.
;;
;; For LISTING, send (text . DATA-CHARACTERS).  (Is this really correct or
;; is this perhaps a bug in the old parser?)  I'm ignoring this for now.

(eval-when-compile
  (w3-p-s-var-def w3-p-s-undo-list)
  (w3-p-s-var-def w3-p-s-content)
  (w3-p-s-var-def w3-p-s-end-tag)
  ;; Modifies free variables:
  ;;   w3-p-d-current-element, w3-p-d-open-element-stack
  ;; Accesses free variables:
  ;;   w3-p-d-tag-name, w3-p-d-end-tag-p
  ;; Destroys free variables:
  ;;   w3-p-s-undo-list, w3-p-s-content, w3-p-s-end-tag
  (defsubst w3-close-element (&optional inferred)
    ;; inferred: non-nil if the end-tag of the current element is being
    ;; inferred due to the presence of content not allowed in the current
    ;; element.  If t, then the tag causing this is in w3-p-d-tag-name and
    ;; w3-p-d-end-tag-p.
    ;; (OLD: ... otherwise it is a symbol indicating the start-tag
    ;; of an element or *data or *space indicating data characters.)
    
    (cond ((and inferred
                (not (w3-element-end-tag-omissible w3-p-d-current-element)))
           (w3-debug-html
             (format "</%s> end-tag not omissible (required due to %s)"
                     (w3-sgml-name-to-string
                      (w3-element-name w3-p-d-current-element))
                     (cond ((eq t inferred)
                            (format (if w3-p-d-end-tag-p
                                        "</%s> end-tag"
                                      "start-tag for %s")
                                    (w3-sgml-name-to-string
                                     w3-p-d-tag-name)))
                           ;; *** Delete this functionality?
                           ((memq inferred '(*space *data))
                            "data characters")
                           ((symbolp inferred)
                            (format "start-tag for %s"
                                    (w3-sgml-name-to-string inferred)))
                           )))))
    
    ;; Undo any variable bindings of this element.
    (cond ((w3-element-undo-list w3-p-d-current-element)
           (setq w3-p-s-undo-list
                 (w3-element-undo-list w3-p-d-current-element))
           (while w3-p-s-undo-list
             (set (car (car w3-p-s-undo-list))
                  (cdr (car w3-p-s-undo-list)))
             (setq w3-p-s-undo-list (cdr w3-p-s-undo-list)))))
  
    (setq w3-p-s-end-tag
          (w3-element-end-tag-name w3-p-d-current-element))
  
    ;; Fix up the content of the current element in preparation for putting
    ;; it in the parent.
    ;; Remove trailing newline from content, if there is one, otherwise send
    ;; any trailing data character item to display engine.
    (setq w3-p-s-content (w3-element-content w3-p-d-current-element))
    (cond ((null w3-p-s-content))
          ((equal "\n" (car w3-p-s-content))
           (setq w3-p-s-content (cdr w3-p-s-content)))
          )
  
    (cond ;; *** Handle LISTING the way the old parser did.
          ((eq 'EMPTY (w3-element-content-model w3-p-d-current-element))
           ;; Do nothing, can't have an end tag.
           )
          (t
           ;; Normal case.
           (if (null w3-p-s-content)
               (w3-debug-html
                 :bad-style :outer
                 ;; Don't warn for empty TD elements or empty A elements
                 ;; with no HREF attribute.
                 ;; *** Crude hack that should really be encoded in the
                 ;; element database somehow.
                 (if (or (not (memq (w3-element-name w3-p-d-current-element)
                                    '(a td)))
                         (assq 'href
                               (w3-element-attributes w3-p-d-current-element)))
                     (format "Empty %s element."
                             (w3-sgml-name-to-string
                              (w3-element-name w3-p-d-current-element))))))))
    
    ;; Put the current element in the proper place in its parent.
    ;; This will cause an error if we overpop the stack.
    (w3-set-element-content
     (car w3-p-d-open-element-stack) 
     (cons (list (w3-element-name w3-p-d-current-element)
                 (w3-element-attributes w3-p-d-current-element)
                 (nreverse w3-p-s-content))
           (w3-element-content (car w3-p-d-open-element-stack))))
  
    ;; Pop the stack.
    (setq w3-p-d-current-element (car w3-p-d-open-element-stack))
    (setq w3-p-d-open-element-stack (cdr w3-p-d-open-element-stack)))

  )


;;;
;;; A pseudo-DTD for HTML.
;;;

;; (eval-when-compile
;;   ;; This works around the following bogus compiler complaint:
;;   ;;   While compiling the end of the data in file w3-parse.el:
;;   ;;     ** the function w3-expand-parameters is not known to be defined.
;;   ;; This is a bogus error.  Anything of this form will trigger this message:
;;   ;;   (eval-when-compile (defun xyzzy () (xyzzy)))
;;   (defun w3-expand-parameters (_pars _data) nil))

(eval-when-compile
  (defun w3-expand-parameters (pars data)
    (cond ((null data)
           nil)
          ((consp data)
           ;; This has to be written carefully to avoid exceeding the
           ;; maximum lisp function call nesting depth.
           (let (result)
             (while (consp data)
               (let ((car-exp (w3-expand-parameters pars (car data))))
                 (setq result
                       (if (and (symbolp (car data))
                                (not (eq car-exp (car data)))
                                ;; An expansion occurred.
                                (listp car-exp))
                           ;; The expansion was a list, which we splice in.
                           (condition-case err
                               (append (reverse car-exp) result)
                             (wrong-type-argument
                              (if (eq 'listp (nth 1 err))
                                  ;; Wasn't really a "list" since the last
                                  ;; cdr wasn't nil, so don't try to splice
                                  ;; it in.
                                  (cons car-exp result)
                                (signal (car err) (cdr err)))))
                         (cons car-exp result))))
               (setq data (cdr data)))
             (append (nreverse result)
                     (w3-expand-parameters pars data))))
          ((symbolp data)
           (let ((sym-exp (cdr-safe (assq data pars))))
             (if sym-exp
                 (w3-expand-parameters pars sym-exp)
               data)))
          ((vectorp data)
           (let ((i 0)
                 (result (copy-sequence data)))
             (while (< i (length data))
               (aset result i
                     (w3-expand-parameters pars (aref data i)))
               (setq i (1+ i)))
             result))
          (t
           data))))

(eval-when-compile
  (defun w3-unfold-dtd (items)
    (let (result)
      (while items
        (let* ((item (car items))
               (names (car item))
               (content-model
                (or (cdr-safe (assq 'content-model item))
                    (error "impossible")))
               (overrides (cdr-safe (assq 'overrides item)))
               (end-tag-omissible
                (or (cdr-safe (assq 'end-tag-omissible item))
                    ;; *** Is this SGML standard?
                    (eq 'EMPTY content-model)))
               (deprecated (cdr-safe (assq 'deprecated item)))
               element
               name)
          (while names
            (setq name (car names))
            (setq names (cdr names))

            ;; Create and initialize the element information data
            ;; structure.
            (setq element (w3-make-element))
            (w3-set-element-name element name)
            (w3-set-element-end-tag-name
             element 
             (intern (concat "/" (symbol-name name))))
            (w3-set-element-state element 0)
            (w3-set-element-content-model element content-model)
            (w3-set-element-end-tag-omissible element end-tag-omissible)
            
            (or (memq deprecated '(nil t obsolete))
                (error "impossible"))
            (w3-set-element-deprecated element deprecated)
            
            ;; Inclusions and exclusions are specified differently in the
            ;; human-coded DTD than in the format the implementation uses.
            ;; The human-coded version is designed to be easy to edit and to
            ;; work with w3-expand-parameters while the internal version is
            ;; designed to be fast.  We have to translate here.  This work
            ;; is repeated for every element listed in `names' so that the
            ;; exclusion exception error messages can be accurate.
            (let ((inclusions (cdr-safe (assq 'inclusions item)))
                  (exclusions (cdr-safe (assq 'exclusions item)))
                  (exclusion-mode '*close)
                  (exclusion-message 
                   (format "%s exclusion" (w3-sgml-name-to-string name)))
                  exceptions)
              (while inclusions
                (setq exceptions (cons (cons (car inclusions)
                                             '(*include *same nil))
                                       exceptions))
                (setq inclusions (cdr inclusions)))
              (while exclusions
                (cond ((memq (car exclusions) '(*discard *include *close))
                       (setq exclusion-mode (car exclusions)))
                      ((stringp (car exclusions))
                       (setq exclusion-message (car exclusions)))
                      (t
                       (setq exceptions (cons (list (car exclusions)
                                                    exclusion-mode
                                                    '*same
                                                    exclusion-message)
                                              exceptions))))
                (setq exclusions (cdr exclusions)))
              (let ((overrides (if exceptions
                                   (cons (cons 'w3-p-d-exceptions
                                               (cons nil exceptions))
                                         overrides)
                                 overrides)))
                (w3-set-element-overrides element overrides)))
            
            (setq result (cons (cons name element) result))))
        (setq items (cdr items)))
      result)))

;; Load the HTML DTD.
;; <URL:ftp://ds.internic.net/rfc/rfc1866.txt>
;; *** Be sure to incorporate rfc1867 when attribute-checking is added.
;; *** Write function to check sanity of the content-model forms.
;; *** I18N: Add Q, BDO, SPAN
(dolist
    (pair
     ;; The purpose of this complexity is to speed up loading by
     ;; pre-evaluating as much as possible at compile time.
     (eval-when-compile
       (w3-unfold-dtd
        (w3-expand-parameters
         '(
           (%headempty . (link base meta range))
           (%headmisc . (script))
           (%head-deprecated . (nextid))

           ;; client-side imagemaps
           (%imagemaps . (area map))
           (%input.fields . (input select textarea keygen label))
           ;; special action is taken for %text inside %body.content in the
           ;; content model of each element.
           (%body.content . (%heading %block style hr div address %imagemaps))

           (%heading . (h1 h2 h3 h4 h5 h6))

           ;; Emacs-w3 extensions
           (%emacsw3-crud  . (pinhead flame cookie yogsothoth hype peek))

           (%block . (p %list dl form %preformatted 
                        %blockquote isindex fn table fig note
                        multicol center %block-deprecated %block-obsoleted))
           (%list . (ul ol))
           (%preformatted . (pre))
           (%blockquote . (bq))
           (%block-deprecated . (dir menu blockquote))
           (%block-obsoleted . (xmp listing))
       
           ;; Why is IMG in this list?
           (%pre.exclusion . (*include img *discard tab math big small sub sup))
       
           (%text . (*data b %notmath sub sup %emacsw3-crud %input.fields))
           (%notmath . (%special %font %phrase %misc))
           (%font . (i u s strike tt big small sub sup font
                       roach secret wired)) ;; B left out for MATH
           (%phrase . (em strong dfn code samp kbd var cite blink))
           (%special . (a nobr img applet object font basefont br script style map math tab span bdo))
           (%misc . (q lang au person acronym abbrev ins del))
       
           (%formula . (*data %math))
           (%math . (box above below %mathvec root sqrt array sub sup
                         %mathface))
           (%mathvec . (vec bar dot ddot hat tilde))
           (%mathface . (b t bt))

           (%mathdelims . (over atop choose left right of))

           ;; What the hell?  This takes BODYTEXT?????  No way!
           (%bq-content-model . [(nil
                                  nil
                                  (((bodytext) *include *next))
                                  (bodytext *next))
                                 (nil
                                  nil
                                  (((credit) *include *next))
                                  nil)
                                 (nil nil nil nil)
                                 ])

           ;; non-default bad HTML handling.
           (%in-text-ignore . ((p %heading) *discard *same error))
           )
         '(
           ;; A dummy element that will contain *document.
           ((*holder)
            (content-model . [(nil nil nil nil)]))
           ;; The root of the parse tree.  We start with a pseudo-element
           ;; named *document for convenience.
           ((*document)
            (content-model . [(nil nil (((html) *include *next)) (html *next))
                              (nil
                               nil
                               nil
                               (*include *same "after document end"))])
            (end-tag-omissible . t))
           ;; HTML O O (HEAD, BODY)
           ((html)
            (content-model . [(nil
                               nil
                               (((head) *include *next))
                               (head *next))
                              (nil
                               nil
                               (((body) *include *next)
                                ;; Netscape stuff
                                ((frameset) *include 4)
                                )
                               (body *next))
                              (nil
                               nil
                               (((plaintext) *include *next))
                               (*retry *next))
                              (nil
                               nil
                               nil
                               (*include *same "after BODY"))
                              (nil
                               nil
                               nil
                               (*include *same "after FRAMESET"))
                              ])
            (end-tag-omissible . t))
           ((head)
            (content-model . [((title isindex %headempty %headmisc
                                      style %head-deprecated)
                               nil
                               nil
                               ;; *** Should only close if tag can
                               ;; legitimately follow head.  So many can that
                               ;; I haven't bothered to enumerate them.
                               (*close))])
            (end-tag-omissible . t))
           ;; SCRIPT - - (#PCDATA)
           ((script)
            (content-model . XCDATA     ; not official, but allows
                                        ; comment hiding of script, and also
                                        ; idiots that use '</' in scripts.
                           ))
           ;; TITLE - - (#PCDATA)
           ((title)
            (content-model . RCDATA     ; not official
                           ;; [((*data) include-space nil nil)]
                           ))
           ;; STYLE - O (#PCDATA)
           ;; STYLE needs to be #PCDATA to allow omitted end tag.  Bleagh.
           ((style)
            (content-model . CDATA)
            (end-tag-omissible . t))
           ((body)
            (content-model . [((banner) nil nil (*retry *next))
                              ((bodytext) nil nil (bodytext *next))
                              (nil nil (((plaintext) *close)) nil)])
            (inclusions . (spot))
            (end-tag-omissible . t))
           ;; Do I really want to include BODYTEXT?  It has something to do
           ;; with mixed content screwing things up, and I don't understand
           ;; it.  Wait!  It's used by BQ!
           ((bodytext)
            (content-model . [((%body.content)
                               nil
                               ;; Push <P> before data characters.  Non-SGML.
                               (((%text) p)
                                ;; Some stupid sites put meta tags in the
                                ;; middle of their documents.  Sigh.
                                ;; Allow it, but bitch and moan.
                                ((meta) *include *same "not allowed here")
                                ;; Closing when seeing CREDIT is a stupidity
                                ;; caused by BQ's sharing of BODYTEXT.  BQ
                                ;; should have its own BQTEXT.
                                ((credit plaintext) *close))
                               nil)
                              ])
            (end-tag-omissible . t))
           ((div banner center multicol)
            (content-model . [((%body.content)
                               nil
                               ;; Push <P> before data characters.  Non-SGML.
                               (((%text) p))
                               nil)]))
           ((address)
            (content-model . [((p)
                               nil
                               ;; Push <P> before data characters.  Non-SGML.
                               (((%text) p))
                               nil)]))
           ((%heading)
            (content-model . [((%text)
                               include-space
                               ((%in-text-ignore))
                               nil)]))
           ((span bdo)
            (content-model . [((%text)
                               include-space
                               nil
                               nil)])
            )
           ((p)
            (content-model . [((%text)
                               include-space
                               nil
                               ;; *** Should only close if tag can
                               ;; legitimately follow P.  So many can that I
                               ;; don't bother to enumerate here.
                               (*close))])
            (end-tag-omissible . t))
           ((ul ol)
            (content-model . [((lh)
                               nil
                               (((li) *include *next))
                               (*retry *next))
                              ((p)
                               nil
                               nil
                               (*retry *next))
                              ((li)
                               nil
                               ;; Push <LI> before data characters or block
                               ;; elements.
                               ;; Non-SGML.
                               ( ;; ((p) b *same nil)
                                ((%text %block) li *same error))
                               nil)]))
           ((lh)
            (content-model . [((%text)
                               include-space
                               (((dd dt li) *close)
                                (%in-text-ignore))
                               nil)])
            (end-tag-omissible . t))
           ((dir menu)
            (content-model . [((li)
                               nil
                               (((%text) li *same error))
                               nil)])
            (exclusions . (%block)))
           ((li)
            (content-model . [((%block)
                               nil
                               (((li) *close)
                                ;; Push <P> before data characters.  Non-SGML.
                                ((%text) p))
                               nil)])
            (end-tag-omissible . t)
            ;; Better bad HTML handling.
            ;; Technically, there are a few valid documents that this will
            ;; hose, because you can have H1 inside FORM inside LI.  However,
            ;; I don't think that should be allowed anyway.
            (exclusions . (*discard "not allowed here" %heading)))
           ((dl)
            (content-model . [((lh)
                               nil
                               (((dt dd) *include *next))
                               (*retry *next))
                              ((dt dd)
                               nil
                               ;; Push <DD> before data characters or block
                               ;; items.
                               ;; Non-SGML.
                               (((%text %block) dd *same error))
                               nil)]))
           ((dt)
            (content-model . [((%text)
                               include-space
                               (((dd dt) *close)
                                (%in-text-ignore))
                               nil)])
            (end-tag-omissible . t))
           ;; DD is just like LI, but we treat it separately because it can be
           ;; followed by a different set of elements.
           ((dd)
            (content-model . [((%block)
                               nil
                               (((dt dd) *close)
                                ;; Push <P> before data characters.  Non-SGML.
                                ((%text) p))
                               nil)])
            (end-tag-omissible . t)
            ;; See comment with LI.
            (exclusions . (*discard "not allowed here" %heading)))
           ((pre)
            (content-model . [((%text hr)
                               include-space
                               ((%in-text-ignore))
                               nil)])
            (exclusions . (%pre.exclusion)))
           ;; BLOCKQUOTE deprecated, BQ okay
           ((bq)
            (content-model . %bq-content-model))
           ((blockquote)
            (content-model . %bq-content-model)
            ;; BLOCKQUOTE is deprecated in favor of BQ in the HTML 3.0 DTD.
            ;; However, BQ is not even mentioned in the HTML 2.0 DTD.  So I
            ;; don't think we can enable this yet:
            ;;(deprecated . t)
            )
           ((fn note)
            (content-model . [((%body.content)
                               nil
                               ;; Push <P> before data characters.  Non-SGML.
                               (((%text) p))
                               nil)]))
           ((fig)
            (content-model . [((overlay) nil nil (*retry *next))
                              (nil
                               nil
                               (((caption) *include *next))
                               (*retry *next))
                              (nil
                               nil
                               (((figtext) *include *next)
                                ((credit) *retry *next))
                               ;; *** Should only do this for elements that
                               ;; can be in FIGTEXT.
                               (figtext *next))
                              (nil nil (((credit) *include *next)) nil)
                              (nil nil nil nil)]))
           ((caption credit)
            (content-model . [((%text)
                               nil
                               ((%in-text-ignore))
                               nil)]))
           ((figtext)
            (content-model . [((%body.content)
                               nil
                               ;; Push <P> before data characters.  Very non-SGML.
                               (((%text) p)
                                ((credit) *close))
                               nil)])
            (end-tag-omissible . t))
           ((%emacsw3-crud basefont)
            (content-model . EMPTY))
           ;; FORM - - %body.content -(FORM) +(INPUT|KEYGEN|SELECT|TEXTAREA)
           ((form)
            ;; Same as BODY.  Ugh!
            (content-model . [((%body.content %text)
                               nil
                               ;; Push <P> before data characters.  Non-SGML.
                               nil
                               nil)])
            (exclusions . (form))
            (inclusions . (input select textarea keygen label)))
           ;; *** Where is the URL describing this?
           ((label)
            (content-model . [((%text)
                               include-space
                               nil
                               nil)])
            ;; *** These are already included, no need to repeat.
            ;;(inclusions . (input select textarea))
            ;; *** Is a LABEL allowed inside a LABEL?  I assume no.
            (exclusions . (label))
            ;; The next line just does the default so is unneeded:
            ;;(end-tag-omissible . nil)
            )
           ;; SELECT - - (OPTION+) -(INPUT|KEYGEN|TEXTAREA|SELECT)>
           ;; *** This should be -(everything).
           ((select)
            (content-model . [((option) nil nil nil)])
            (exclusions . (input label select keygen textarea)))
           ;; option - O (#PCDATA)
           ;; needs to be #PCDATA to allow omitted end tag.
           ((option)
            ;; I'd like to make this RCDATA to avoid problems with inclusions
            ;; like SPOT, but that would conflict with the omitted end-tag, I
            ;; think.
            (content-model . [((*data)
                               include-space
                               (((option) *close))
                               nil)])
            (end-tag-omissible . t))
           ;; TEXTAREA - - (#PCDATA) -(INPUT|TEXTAREA|KEYGEN|SELECT)
           ((textarea)
            ;; Same comment as for OPTION about RCDATA.
            (content-model . XCDATA) ;;;[((*data) include-space nil nil)])
            (exclusions . (input select label keygen textarea)))
           ((hr br img isindex input keygen overlay wbr spot tab
                %headempty %mathdelims)
            (content-model . EMPTY))
           ((nextid)
            (content-model . EMPTY)
            (deprecated . t))
           ((a)
            (content-model . [((%text)
                               include-space
                               (((%heading)
                                 *include *same "deprecated inside A")
                                ;; *** I haven't made up my mind whether this
                                ;; is a good idea.  It can result in a lot of
                                ;; bad formatting if the A is *never* closed.
                                ;;((p) *discard *same error)
                                )
                               nil)])
            (exclusions . (a)))
           ((b font %font %phrase %misc nobr)
            (content-model . [((%text)
                               include-space
                               ((%in-text-ignore))
                               nil)]))
           ((plaintext)
            (content-model . XXCDATA)
            (end-tag-omissible . t)
            (deprecated . obsolete))
           ((xmp listing)
            (content-model . XCDATA)
            (deprecated . obsolete))
           ;; Latest table spec (as of Nov. 13 1995) is at:
           ;; <URL:ftp://ds.internic.net/internet-drafts/draft-ietf-html-tables-03.txt>
           ((table)
            (content-model . [(nil
                               nil
                               (((caption) *include *next)
                                ((%text) tr *same error)
                                ((col colgroup thead tfoot tbody tr) *retry *next))
                               (*retry *next)) ;error handling
                              ((col colgroup)
                               nil
                               (((thead tfoot tbody tr) *retry *next))
                               (*retry *next)) ;error handling
                              (nil
                               nil
                               (((thead) *include *next)
                                ((tfoot tbody tr) *retry *next))
                               (*retry *next)) ;error handling
                              (nil
                               nil
                               (((tfoot) *include *next)
                                ((tbody tr) *retry *next))
                               (*retry *next)) ;error handling
                              ((tbody)
                               nil
                               (((tr) tbody *same)
                                ((td th) tr *same)
                                ;; error handling
                                ((%body.content) tbody *same error))
                               nil)]))
           ((colgroup)
            (content-model . [((col)
                               nil
                               (((colgroup thead tfoot tbody tr) *close))
                               nil)])
            (end-tag-omissible . t))
           ((col)
            (content-model . EMPTY))
           ((thead)
            (content-model . [((tr)
                               nil
                               (((tfoot tbody) *close)
                                ;; error handling
                                ((%body.content) tr *same error))
                               nil)])
            (end-tag-omissible . t))
           ((tfoot tbody)
            (content-model . [((tr)
                               nil
                               (((tbody) *close)
                                ;; error handling
                                ((td th) tr *same error)
                                ((%body.content) tr *same error))
                               nil)])
            (end-tag-omissible . t))
           ((tr)
            (content-model . [((td th)
                               nil
                               (((tr tfoot tbody) *close)
                                ;; error handling
                                ((%body.content %text) td *same error))
                               nil)])
            (end-tag-omissible . t))
           ((td th)
            ;; Arrgh!  Another %body.content!!!  Stupid!!!
            (content-model . [((%body.content)
                               nil
                               (((td th tr tfoot tbody) *close)
                                ;; Push <P> before data characters.  Non-SGML.
                                ((%text) p))
                               nil)])
            (end-tag-omissible . t))
           ((math)
            (content-model . [((*data) include-space nil nil)])
            (overrides .
                       ((w3-p-d-shortref-chars t . "\{_^")
                        (w3-p-d-shortrefs t . (("\\^" . "<sup>")
                                               ("_" . "<sub>")
                                               ("{" . "<box>")))))
            (inclusions . (%math))
            (exclusions . (%notmath)))
           ((sup)
            (content-model . [((%text)
                               include-space
                               ((%in-text-ignore))
                               nil)])
            (overrides .
                       ((w3-p-d-shortref-chars t . "\{_^")
                        (w3-p-d-shortrefs t . (("\\^" . "</sup>")
                                               ("_" . "<sub>")
                                               ("{" . "<box>"))))))
           ((sub)
            (content-model . [((%text)
                               include-space
                               ((%in-text-ignore))
                               nil)])
            (overrides .
                       ((w3-p-d-shortref-chars t . "\{_^")
                        (w3-p-d-shortrefs t . (("\\^" . "<sup>")
                                               ("_" . "</sub>")
                                               ("{" . "<box>"))))))
           ((box)
            (content-model . [((%formula)
                               include-space
                               (((left) *include 1)
                                ((over atop choose) *include 2)
                                ((right) *include 3))
                               nil)
                              ((%formula)
                               include-space
                               (((over atop choose) *include 2)
                                ((right) *include 3))
                               nil)
                              ((%formula)
                               include-space
                               (((right) *include 3))
                               nil)
                              ((%formula) include-space nil nil)])
            (overrides .
                       ((w3-p-d-shortref-chars t . "{}_^")
                        (w3-p-d-shortrefs t . (("\\^" . "<sup>")
                                               ("_" . "<sub>")
                                               ("{" . "<box>")
                                               ("}" . "</box>"))))))
           ((above below %mathvec t bt sqrt)
            (content-model . [((%formula) include-space nil nil)]))
           ;; ROOT has a badly-specified content-model in HTML 3.0.
           ((root)
            (content-model . [((%formula)
                               include-space
                               (((of) *include *next))
                               nil)
                              ((%formula) include-space nil nil)]))
           ((of)
            (content-model . [((%formula) include-space nil nil)])
            ;; There is no valid way to infer a missing end-tag for OF.  This
            ;; is bizarre.
            (end-tag-omissible . t))
           ((array)
            (content-model . [((row) nil nil nil)]))
           ((row)
            (content-model . [((item) nil (((row) *close)) nil)])
            (end-tag-omissible . t))
           ((item)
            (content-model . [((%formula)
                               include-space
                               (((row item) *close))
                               nil)])
            (end-tag-omissible . t))
           ;; The old parser would look for the </EMBED> end-tag and include
           ;; the contents between <EMBED> and </EMBED> as the DATA attribute
           ;; of the EMBED start-tag.  However, it did not require the
           ;; </EMBED> end-tag and did nothing if it was missing.  This is
           ;; completely impossible to specify in SGML.
           ;;
           ;; See
           ;; <URL:http://www.eit.com/goodies/lists/www.lists/www-html.1995q3/0603.html>  
           ;;
           ;; Questions: Does EMBED require the end-tag?  How does NOEMBED fit
           ;; into this?  Where can EMBED appear?
           ;;
           ;; Nov. 25 1995: a new spec for EMBED (also an I-D):
           ;; <URL:http://www.cs.princeton.edu/~burchard/www/interactive/>
           ;;
           ;; Here is my guess how to code EMBED:
           ((embed)
            (content-model . [((noembed) nil nil (*close))]))
           ((noembed)
            (content-model . [((%body.content) ; hack hack hack
                               nil
                               (((%text) p))
                               nil)]))
           ;;
           ;; FRAMESET is a Netscape thing.
           ;; <URL:http://www.eit.com/goodies/lists/www.lists/www-html.1995q3/0588.html>
           ((frameset)
            (content-model . [((noframes frame frameset) nil nil nil)]))
           ((noframes)
            (content-model . [((%body.content)
                               nil
                               ;; Push <P> before data characters.  Non-SGML.
                               (((%text) p))
                               nil)]))
           ((frame)
            (content-model . EMPTY))
           ;;
           ;; APPLET is a Java thing.
           ;; OBJECT is a cougar thing
           ;; <URL:http://java.sun.com/JDK-beta/filesinkit/README>
           ((applet object)
            ;; I really don't want to add another ANY content-model.
            (content-model . XINHERIT)
            (inclusions . (param)))
           ((param)
            (content-model . EMPTY))
           ;; backward compatibility with old Java.
           ((app)
            (content-model . EMPTY))
           ;; Client-side image maps.
           ;; <URL:ftp://ds.internic.net/internet-drafts/draft-seidman-clientsideimagemap-01.txt>
           ;; *** The only problem is that I don't know in what elements MAP
           ;; can appear, so none of this is reachable yet.
           ((map)
            (content-model . [((area) nil nil nil)]))
           ((area)
            (content-model . EMPTY))
           )))))
  (put (car pair) 'html-element-info (cdr pair)))


;;;
;;; Omitted tag inference using state transition tables.
;;;

(eval-when-compile

  (w3-p-s-var-def w3-p-s-includep)
  (w3-p-s-var-def w3-p-s-state-transitions)
  (w3-p-s-var-def w3-p-s-transition)
  (w3-p-s-var-def w3-p-s-tran-list)
  (w3-p-s-var-def w3-p-s-content-model)
  (w3-p-s-var-def w3-p-s-except)
  (w3-p-s-var-def w3-p-s-baseobject)
  (w3-p-s-var-def w3-p-s-btdt)
  ;; Uses free variables:
  ;;   w3-p-d-current-element, w3-p-d-exceptions
  ;; Destroys free variables:
  ;;   w3-p-s-includep, w3-p-s-state-transitions, w3-p-s-transition,
  ;;   w3-p-s-tran-list, w3-p-s-content-model, w3-p-s-except
  ;; Returns t if the element or data characters should be included.
  ;; Returns nil if the element or data characters should be discarded.
  (defsubst w3-grok-tag-or-data (tag-name)
    (while
        (cond
         ((symbolp (setq w3-p-s-content-model
                         (w3-element-content-model w3-p-d-current-element)))
          (or (and (memq w3-p-s-content-model
                         '(CDATA RCDATA XCDATA XXCDATA))
                   (memq tag-name '(*data *space)))
              ;; *** Implement ANY.
              (error "impossible content model lossage"))
          (setq w3-p-s-includep t)
          ;; Exit loop.
          nil)
         (t
          ;; We have a complex content model.
          ;; Cache some data from the element info structure.  Format is:
          ;;   (INCLUDES INCSPACEP (((TAG ...) . TRANSITION) ...) DEFAULT)
          (setq w3-p-s-state-transitions
                (aref w3-p-s-content-model
                      (w3-element-state w3-p-d-current-element)))
        
          ;; Optimize the common cases.
          (cond
           ((eq '*space tag-name)
            ;; Optimizing the (*space *discard *same nil) transition.
            (setq w3-p-s-includep (car (cdr w3-p-s-state-transitions)))
            ;; Don't loop.
            nil)
           ((and (not (setq w3-p-s-except
                            (assq tag-name w3-p-d-exceptions)))
                 (memq tag-name (car w3-p-s-state-transitions)))
            ;; Equivalent to a transition of (TAG *include *same nil).
            ;; So we are done, return t to caller.
            (setq w3-p-s-includep t)
            ;; Exit loop.
            nil)
           (t
            ;; The general case.
            (cond
             ;; Handle inclusions and exclusions.
             (w3-p-s-except
              (setq w3-p-s-transition (cdr w3-p-s-except)))
             ;; See if the transition is in the complex transitions
             ;; component.
             ((progn
                (setq w3-p-s-tran-list
                      (car (cdr (cdr w3-p-s-state-transitions))))
                (setq w3-p-s-transition nil)
                (while w3-p-s-tran-list
                  (cond ((memq tag-name (car (car w3-p-s-tran-list)))
                         ;; We've found a transition.
                         (setq w3-p-s-transition
                               (cdr (car w3-p-s-tran-list)))
                         (setq w3-p-s-tran-list nil))
                        (t
                         (setq w3-p-s-tran-list (cdr w3-p-s-tran-list)))))
                ;; Check if we found it.
                w3-p-s-transition)
              ;; body of cond clause empty
              )
             ;; Try finding the transition in the DEFAULT component of the
             ;; transition table, but avoid doing this for unknown elements,
             ;; always use the default-default for them.
             ((and (or (eq '*data tag-name)
                       (w3-known-element-p tag-name))
                   (setq w3-p-s-transition
                         (nth 3 w3-p-s-state-transitions)))
              ;; body of cond clause empty
              )
             (t
              ;; Supply a default-default transition.
              (if (not (or (eq '*data tag-name)
                           (w3-known-element-p tag-name)))
                  (setq w3-p-s-transition
                        '(*discard *same "unknown element"))

                ;; Decide whether to *close or *discard
                ;; based on whether this element would be
                ;; accepted as valid in an open ancestor.
                (let ((open-list w3-p-d-open-element-stack)
                      (all-end-tags-omissible
                       (w3-element-end-tag-omissible w3-p-d-current-element))
                      state-transitions tran-list)
                  (if (catch 'found
                        (while open-list
                          (setq state-transitions
                                (aref (w3-element-content-model
                                       (car open-list))
                                      (w3-element-state (car open-list))))
                          (if (memq tag-name (car state-transitions))
                              (throw 'found t))
                          (setq tran-list (nth 2 state-transitions))
                          (while tran-list
                            (cond ((memq tag-name (car (car tran-list)))
                                   (if (not (nth 3 (car tran-list)))
                                       ;; Not an error transition.
                                       (throw 'found t))
                                   (setq tran-list nil))
                                  (t
                                   (setq tran-list (cdr tran-list)))))
                          ;; The input item is not accepted in this
                          ;; ancestor.  Try again in next ancestor.
                          (or (w3-element-end-tag-omissible (car open-list))
                              (setq all-end-tags-omissible nil))
                          (setq open-list (cdr open-list)))
                        nil)
                      (setq w3-p-s-transition
                            (if (w3-element-end-tag-omissible
                                 w3-p-d-current-element)
                                (if all-end-tags-omissible
                                    ;; Probably indicates a need to debug
                                    ;; the DTD state-transition tables.
                                    '(*close *same
                                             "missing transition in DTD?")
                                  ;; Error will be reported later.
                                  '(*close *same))
                              '(*close *same "not allowed here")))
                    (setq w3-p-s-transition
                          '(*discard *same "not allowed here")))))))
            
            ;; We have found a transition to take.  The transition is of
            ;; the format (ACTION NEW-STATE ERRORP) where the latter two
            ;; items are optional.
            
            ;; First, handle any state-change.
            (or (memq (car-safe (cdr w3-p-s-transition)) '(nil *same))
                (w3-set-element-state
                 w3-p-d-current-element 
                 (if (eq '*next (car-safe (cdr w3-p-s-transition)))
                     (1+ (w3-element-state w3-p-d-current-element))
                   (car-safe (cdr w3-p-s-transition)))))
          
            ;; Handle any error message.
            (if (car-safe (cdr-safe (cdr w3-p-s-transition)))
                (w3-debug-html 
                  :mandatory-if (and (eq '*data tag-name)
                                     (eq '*discard (car w3-p-s-transition)))
                  (format "Bad %s [%s], %s"
                          (if (eq '*data tag-name)
                              "data characters"
                            (concat "start-tag "
                                    (w3-sgml-name-to-string tag-name)))
                          (if (stringp (car (cdr (cdr w3-p-s-transition))))
                              (car (cdr (cdr w3-p-s-transition)))
                            "not allowed here")
                          (let ((action (car w3-p-s-transition)))
                            (cond ((eq '*discard action)
                                   "discarding bad item")
                                  ((eq '*close action)
                                   (concat "inferring </"
                                           (w3-sgml-name-to-string
                                            (w3-element-name
                                             w3-p-d-current-element))
                                           ">"))
                                  ((eq '*include action)
                                   "including bad item anyway")
                                  ((eq '*retry action)
                                   "*retry ??? you shouldn't see this")
                                  (t
                                   (concat "inferring <"
                                           (w3-sgml-name-to-string action)
                                           ">")))))))
            
            ;; Handle the action.
            (cond
             ((eq '*include (car w3-p-s-transition))
              (setq w3-p-s-includep t)
              ;; Exit loop.
              nil)
             ((eq '*close (car w3-p-s-transition))
              ;; Perform end-tag inference.
              (w3-close-element)        ; don't pass parameter
              ;; Loop and try again in parent element's content-model.
              t)
             ((eq '*discard (car w3-p-s-transition))
              (setq w3-p-s-includep nil)
              ;; Exit loop.
              nil)
             ((eq '*retry (car w3-p-s-transition))
              ;; Loop and try again after state change.
              t)
             ((symbolp (car w3-p-s-transition))
              ;; We need to open another element to contain the text,
              ;; probably a <P> (look in the state table).
              (w3-open-element (car w3-p-s-transition) nil)
              ;; Now we loop and try again in the new element's
              ;; content-model.
              t)
             (t
              (error "impossible transition")))))))
    
      ;; Empty while loop body.
      )
  
    ;; Return value to user indicating whether to include or discard item:
    ;;   t   ==> include
    ;;   nil ==> discard
    w3-p-s-includep)

  )


;;;
;;; Main parser.
;;;

(defvar w3-last-parse-tree nil
  "Used for debugging only.  Stores the most recently computed parse tree
\(a tree, not a parse tag stream\).")

(defun w3-display-parse-tree (&optional ptree)
  (interactive)
  (with-output-to-temp-buffer "W3 HTML Parse Tree"
    (set-buffer standard-output)
    (emacs-lisp-mode)
    (require 'pp)
    (pp (or ptree w3-last-parse-tree))))

(defalias 'w3-display-last-parse-tree 'w3-display-parse-tree)

;; For compatibility with the old parser interface.
(defalias 'w3-preparse-buffer 'w3-parse-buffer)

(defcustom w3-parse-hooks nil
  "*List of hooks to be run before parsing."
  :type 'hook
  :group 'w3-display
  :options '(w3-parse-munge-ethiopic-text) ; too exotic for a default
  )

(defun w3-parse-munge-ethiopic-text ()
  "Treat marked-up regions using `ethio-sera-to-fidel-marker'.
Do nothing in non-Mule or unibyte session."
  (when (and (featurep 'mule)
             (boundp 'default-enable-multibyte-characters)
             default-enable-multibyte-characters)
    (ethio-sera-to-fidel-marker)))

(defalias 'w3-char-int
  (if (fboundp 'char-int) #'char-int #'identity))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; %                                                    %
;; % This is the *ONLY* valid entry point in this file! %
;; %       DO NOT call any of the other functions!      %
;; %                                                    %
;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defun w3-slow-parse-buffer (&optional buff)
  "Parse contents of BUFF as HTML.
BUFF defaults to the current buffer.
Destructively alters contents of BUFF.
Returns a data structure containing the parsed information."
  (if (not w3-setup-done) (w3-do-setup))
  (with-current-buffer (or buff (setq buff (current-buffer)))
    (let ((old-syntax-table (syntax-table)))
      (set-syntax-table w3-sgml-md-syntax-table)
      (buffer-disable-undo (current-buffer))
      (widen)                           ; sanity checking
      (goto-char (point-max))
      (insert "\n")
      (goto-char (point-min))
      (setq case-fold-search t)         ; allows smaller regexp patterns

      (run-hooks 'w3-parse-hooks)       ;

      (goto-char (point-min))
  
      ;; *** Should premunge line boundaries.
      ;; ********************
  
      (let* (
             ;; Speed hack, see the variable doc string.
             (gc-cons-threshold (if (> w3-gc-cons-threshold-multiplier 0)
                                    (* w3-gc-cons-threshold-multiplier
                                       gc-cons-threshold)
                                  gc-cons-threshold))

             ;; Used to determine if we made any progress since the last loop.
             (last-loop-start (point-min))
        
             ;; How many iterations of the main loop have occurred.  Used only
             ;; to send messages to the user periodically, since this function
             ;; can take some time.
             (loop-count 0)

             ;; Precomputing the loop-invariant parts of this for speed.
             (status-message-format
              (if url-show-status
                  (format "Parsed %%3d%%%% of %d..." (- (point-max) (point-min)))))
         
             ;; Use a float value for 100 if possible, otherwise integer.
             ;; Determine which we can use outside of the loop for speed.
             (one-hundred (funcall (if (fboundp 'float) 'float 'identity) 100))
         
             ;; The buffer which contains the HTML we are parsing.  This
             ;; variable is used to avoid using the more expensive
             ;; save-excursion.
             ;; (parse-buffer (current-buffer))
         
             ;; Points to start of region of text since the previous tag.
             (between-tags-start (point-min))
         
             ;; Points past end of region of text since the previous tag.  Only
             ;; non-nil when the region has been completely determined and is
             ;; ready to be processed.
             between-tags-end
         
             ;; See doc string.
             w3-p-d-tag-name
         
             ;; See doc string.
             w3-p-d-end-tag-p
         
             ;; Is the tag we are looking at a null-end-tag-enabling
             ;; start-tag?
             net-tag-p
         
             ;; Attributes of the tag we are looking at.  An alist whose items
             ;; are pairs of the form (SYMBOL . STRING).
             tag-attributes
         
             ;; Points past end of attribute value we are looking at.  Points
             ;; past the syntactic construct, not the value of the attribute,
             ;; which may be at (1- attribute-value-end).
             attribute-value-end
         
             ;; Points past end of tag we are looking at.
             tag-end
         
             ;; See doc string.
             (w3-p-d-current-element (w3-fresh-element-for-tag '*document))
         
             ;; See doc string.
             (w3-p-d-open-element-stack (list (w3-fresh-element-for-tag '*holder)))
         
             ;; ***not implemented yet***
             (marked-section-undo-stack nil)
         
             ;; See doc string.
             (w3-p-d-debug-url t)
         
             ;; Any of the following variables with the comment ";*NESTED*"
             ;; are syntactic or semantic features that were introduced by
             ;; some containing element or marked section which will be undone
             ;; when we close that element or marked section.
         
             ;; See doc string.
             (w3-p-d-non-markup-chars nil) ;*NESTED*
         
             ;; See doc string.
             (w3-p-d-null-end-tag-enabled nil) ;*NESTED*
         
             ;; See doc string.
             (w3-p-d-in-parsed-marked-section nil) ;*NESTED*
         
             ;; See doc string.
             (w3-p-d-shortrefs nil)     ;*NESTED*
         
             ;; See doc string.
             (w3-p-d-shortref-chars nil) ;*NESTED*
         
             ;; ******* maybe not needed.
             ;; 
             ;; ;; Are we recognizing start-tags?
             ;; (recognizing-start-tags t)     ;*NESTED*
             ;; 
             ;; ;; Are we recognizing end-tags?  If this is non-nil and not t,
             ;; ;; then only the end tag of the current open element is
             ;; ;; recognized.
             ;; (recognizing-end-tags t)       ;*NESTED*
         
             ;; See doc string.
             (w3-p-d-exceptions nil)    ;*NESTED*
         
             ;; Scratch variables used in this function
             ref attr-name attr-value content-model content open-list
             )
        ;; Scratch variables used by macros and defsubsts we call.
        (w3-p-s-let-bindings
         (w3-update-non-markup-chars)
         (setq w3-p-s-baseobject (copy-sequence url-current-object))
         ;; Main loop.  Handle markup as follows:
         ;;
         ;; non-empty tag: Handle the region since the previous tag as PCDATA,
         ;; RCDATA, CDATA, if allowed by syntax.  Then handle the tag.
         ;;
         ;; general entity (&name;): expand it and parse the result.
         ;;
         ;; shortref (_, {, }, and ^ in math stuff): Expand it and parse the
         ;; result.
         ;;
         ;; SGML marked section (<![ keywords [ conditional-text ]]>): Either
         ;; strip the delimiters and parse the result or delete.
         ;;
         ;; comment: Delete.
         ;;
         ;; empty tag (<>, </>): Handle as the appropriate tag.
         ;;
         ;; markup declaration (e.g. <!DOCTYPE ...>): Delete.
         ;;
         ;; SGML processing instruction (<?name>): Delete.
         ;;
         (while
             ;; Continue as long as we processed something last time and we
             ;; have more to process.
             (prog1 
                 (not (and (= last-loop-start (point))
                           (eobp)))
               (setq last-loop-start (point)))
      
           ;; Display progress messages if asked and/or do incremental display
           ;; of results
           (cond ((= 0 (% (setq loop-count (1+ loop-count)) 40))
                  (if status-message-format
                      (message status-message-format
                               ;; Percentage of buffer processed.
                               (/ (* (point) one-hundred) (point-max))))))
      
           ;; Go to next interesting thing in the buffer.
           (skip-chars-forward w3-p-d-non-markup-chars)
      
           ;; We are looking at a markup-starting character, and invalid
           ;; character, or end of buffer.
           (cond

            ((eq ?< (char-after (point)))

             ;; We are looking at a tag, comment, markup declaration, SGML marked
             ;; section, SGML processing instruction, or non-markup "<".
             (forward-char)
             (cond

              ;; jbw 2001-11-02: added possibility of of ":" in element
              ;; name to handle Microsoft-generated XHTML.
              ((looking-at "/?\\([a-z][-a-z0-9.:]*\\)")
               ;; We are looking at a non-empty tag.

               ;; Downcase it in the buffer, to save creation of a string
               (downcase-region (match-beginning 1) (match-end 1))
               (setq w3-p-d-tag-name
                     (intern (buffer-substring (match-beginning 1)
                                               (match-end 1))))
               (setq w3-p-d-end-tag-p (eq ?/ (char-after (point)))
                     between-tags-end (1- (point)))
               (goto-char (match-end 0))
          
               ;; Read the attributes from a start-tag.
               (if w3-p-d-end-tag-p
                   (if (looking-at "[ \t\r\n/]*[<>]")
                       nil
                     ;; This is in here to deal with those idiots who stick
                     ;; attribute/value pairs on end tags.  *sigh*
                     (w3-debug-html "Evil attributes on end tag.")
                     (skip-chars-forward "^>"))
           
                 ;; Attribute values can be:
                 ;;   "STRING"   where STRING does not contain the double quote
                 ;;   'STRING'   where STRING does not contain the single quote
                 ;;   name-start character, *name character
                 ;;   *name character
                 ;;   Digit, +name character
                 ;;   +Digit
                 ;; or a SPACE-separated list of one of the last four
                 ;; possibilities (there is a comment somewhere that this is a
                 ;; misinterpretation of the grammar, so we ignore this
                 ;; possibility).
                 (while
                     (looking-at
                      (eval-when-compile
                        (concat
                         ;; Leading whitespace.
                         "[ \n\r\t,]*"
                         ;; The attribute name, possibly with a bad syntax
                         ;; component.
                         ;; jbw 2001-11-02: added possibility of ":" to
                         ;; next line to handle Microsoft-generated XHTML.
                         "\\([a-z_][-a-z0-9.]*\\(\\([_:][-a-z0-9._:]*\\)?\\)\\)"
                         ;; Trailing whitespace and perhaps an "=".
                         "[ \n\r\t]*\\(\\(=[ \n\r\t]*\\)?\\)")))
               
                   (cond ((/= (match-beginning 2) (match-end 2))
                          (w3-debug-html
                           :nocontext
                           (format "Bad attribute name syntax: %s"
                                   (buffer-substring (match-beginning 1)
                                                     (match-end 1))))))

                   ;; Downcase it in the buffer, to save creation of a string
                   (downcase-region (match-beginning 1) (match-end 1))
                   (setq attr-name
                         (intern (buffer-substring (match-beginning 1)
                                                   (match-end 1))))
                   (goto-char (match-end 0))
                   (cond
                    ((< (match-beginning 4) (match-end 4))
                     ;; A value was specified (e.g. ATTRIBUTE=VALUE).
                     (cond
                      ((looking-at
                        (eval-when-compile
                          (concat
                           ;; Comma separated list of literals with double quotes
                           ;; (bad HTML).
                           "\"\\([^\"]*\\(\"[ \n\r\t]*,[ \n\r\t]*\"[^\"]*\\)+\\)\""
                           "\\|"
                           ;; Comma separated list of literals with single quotes
                           ;; (bad HTML).
                           "'\\([^']*\\('[ \n\r\t]*,[ \n\r\t]*'[^']*\\)+\\)'"
                           "\\|"
                           ;; Literal with double quotes.
                           "\"\\([^\"]*\\)\""
                           "\\|"
                           ;; Literal with single quotes.
                           "'\\([^']*\\)'"
                           "\\|"
                           ;; Handle bad HTML conflicting with NET-enabling
                           ;; start-tags.
                           "\\([^ \t\n\r>]+/[^ \t\n\r>]+\\)[ \t\n\r>]"
                           "\\|"
                           ;; SGML NAME-syntax attribute value.
                           "\\([-a-z0-9.]+\\)[ \t\n\r></]"
                           )))
                       (cond
                        ((or (match-beginning 5)
                             (match-beginning 6)
                             (match-beginning 1)
                             (match-beginning 3))
                         (if (or (match-beginning 1)
                                 (match-beginning 3))
                             (w3-debug-html
                              :nocontext
                              (format "Badly quoted attribute value: %s"
                                      (match-string 0))))
                         ;; We have an attribute value literal.
                         (narrow-to-region (1+ (match-beginning 0))
                                           (1- (match-end 0)))
                         ;; Delete (bad) extra quotes from comma separated list.
                         (cond
                          ((match-beginning 1)
                           (while (progn (skip-chars-forward "^\"") (not (eobp)))
                             (delete-char 1))
                           (goto-char (point-min)))
                          ((match-beginning 3)
                           (while (progn (skip-chars-forward "^'") (not (eobp)))
                             (delete-char 1))
                           (goto-char (point-min))))
                     
                         ;; In attribute value literals, EE and RS are ignored
                         ;; and RE and SEPCHAR characters sequences are
                         ;; replaced by SPACEs.
                         ;;
                         ;; (There is no way right now to get RS into one of
                         ;; these so that it can be ignored.  This is due to
                         ;; our using Unix line-handling conventions.)
                         (skip-chars-forward "^&\t\n\r")
                         (if (eobp)
                             nil
                           ;; We must expand entities and replace RS, RE,
                           ;; and SEPCHAR.
                           (goto-char (point-min))
                           (while (progn
                                    (skip-chars-forward "^&")
                                    (not (eobp)))
                             (w3-expand-entity-at-point-maybe))
                           (subst-char-in-region (point-min) (point-max) ?\t ? )
                           (subst-char-in-region (point-min) (point-max) ?\n ? ))
                         ;; Set this after we have changed the size of the
                         ;; attribute.
                         (setq attribute-value-end (1+ (point-max))))
                        ((match-beginning 8)
                         (setq attribute-value-end (match-end 8))
                         (narrow-to-region (point) attribute-value-end))
                        ((match-beginning 7)
                         (setq attribute-value-end (match-end 7))
                         (narrow-to-region (point) attribute-value-end)
                         ;; Horribly illegal non-SGML handling of bad
                         ;; HTML on the net.  This can break valid HTML.
                         (setq attr-value (buffer-substring (point)
                                                            (match-end 7)))
                         (w3-debug-html :nocontext
                                        (format "Evil attribute value syntax: %s"
                                                (buffer-substring (point-min) (point-max)))))
                        (t
                         (error "impossible attribute value"))))
                      ((memq (char-after (point)) '(?\" ?'))
                       ;; Missing terminating quote character.
                       (narrow-to-region (point)
                                         (progn
                                           (forward-char 1)
                                           (skip-chars-forward "^ \t\n\r'\"<>")
                                           (setq attribute-value-end (point))))
                       (w3-debug-html :nocontext
                                      (format "Attribute value missing end quote: %s"
                                              (buffer-substring (point-min) (point-max))))
                       (narrow-to-region (1+ (point-min)) (point-max)))
                      (t
                       ;; We have a syntactically invalid attribute value.  Let's
                       ;; make a best guess as to what the author intended.
                       (narrow-to-region (point)
                                         (progn
                                           (skip-chars-forward "^ \t\n\r'\"<>")
                                           (setq attribute-value-end (point))))
                       (w3-debug-html :nocontext
                                      (format "Bad attribute value syntax: %s"
                                              (buffer-substring (point-min) (point-max))))))
                     ;; Now we have isolated the attribute value.  We need to
                     ;; munge the value depending on the syntax of the
                     ;; attribute.
                     ;; *** Right now, we only implement the necessary munging
                     ;; for CDATA attributes, which is none.  I'm not sure why
                     ;; this happens to work for other attributes right now.
                     ;; For any other kind of attribute, we are supposed to
                     ;; * smash case
                     ;; * remove leading/trailing whitespace
                     ;; * smash multiple space sequences into single spaces
                     ;; * verify the syntax of each token
                     (setq attr-value (buffer-substring (point-min) (point-max)))
                     (case attr-name
                       (class
                        (setq attr-value (split-string attr-value "[ ,]+")))
                       (align
                        (if (string-match "^[ \t\r\n]*\\(.*\\)[ \t\r\n]*$"
                                          attr-value)
                            (setq attr-value (downcase
                                              (substring attr-value
                                                         (match-beginning 1)
                                                         (match-end 1))))
                          (setq attr-value (downcase attr-value)))
                        (setq attr-value (intern attr-value)))
                       ((src href)
                        ;; I should expand URLs here
                        )
                       (otherwise nil)
                       )
                     (widen)
                     (goto-char attribute-value-end))
                    (t
                     ;; No value was specified, in which case NAME should be
                     ;; taken as ATTRIBUTE=NAME where NAME is one of the
                     ;; enumerated values for ATTRIBUTE.
                     ;; We assume here that ATTRIBUTE is the same as NAME.
                     ;; *** Another piece of code will fix the attribute name if it
                     ;; is wrong.
                     (setq attr-value (symbol-name attr-name))))
             
                   ;; Accumulate the attributes.
                   (setq tag-attributes (cons (cons attr-name attr-value)
                                              tag-attributes)))

                 (if (and (eq w3-p-d-tag-name 'img)
                          (not (assq 'alt tag-attributes)))
                     (w3-debug-html :bad-style
                                    :outer
                                    "IMG element has no ALT attribute"))
                 (cond
                  ((and (eq w3-p-d-tag-name 'base)
                        (setq w3-p-s-baseobject
                              (or (assq 'src tag-attributes)
                                  (assq 'href tag-attributes))))
                   (setq w3-p-s-baseobject (url-generic-parse-url
                                            (cdr w3-p-s-baseobject))))
                  ((setq w3-p-s-btdt (or (assq 'src tag-attributes)
                                         (assq 'background tag-attributes)
                                         (assq 'codebase tag-attributes)
                                         (assq 'href tag-attributes)
                                         (assq 'action tag-attributes)))
                   (setcdr w3-p-s-btdt (url-expand-file-name (cdr w3-p-s-btdt)
                                                             w3-p-s-baseobject))
                   (setq w3-p-s-btdt (if (url-have-visited-url (cdr w3-p-s-btdt))
                                         ":visited"
                                       ":link"))
                   (if (assq 'class tag-attributes)
                       (setcdr (assq 'class tag-attributes)
                               (cons w3-p-s-btdt
                                     (cdr (assq 'class tag-attributes))))
                     (setq tag-attributes (cons (cons 'class (list w3-p-s-btdt))
                                                tag-attributes))))
                  )
                 (if (not (eq w3-p-d-tag-name 'input))
                     nil
                   (setq w3-p-s-btdt (concat ":"
                                             (downcase
                                              (or (cdr-safe
                                                   (assq 'type tag-attributes))
                                                  "text"))))
                   (if (assq 'class tag-attributes)
                       (setcdr (assq 'class tag-attributes)
                               (cons w3-p-s-btdt
                                     (cdr (assq 'class tag-attributes))))
                     (setq tag-attributes (cons (cons 'class (list w3-p-s-btdt))
                                                tag-attributes))))
                 )
          
               ;; Process the end of the tag.
               (skip-chars-forward " \t\n\r")
               (cond ((eq ?> (char-after (point)))
                      ;; Ordinary tag end.
                      (forward-char 1))
                     ;; jbw 2001-06-25: added next sexp to make XHTML
                     ;; masquerading as HTML work.  This is a crude
                     ;; disgusting hack which happens to make many of
                     ;; the common cases work.  One thing it does not
                     ;; handle is if the input contains <br></br> which
                     ;; is legal XHTML.  Probably to handle that we need
                     ;; to set a flag if we see an XML declaration and
                     ;; then treat the EMPTY content model differently
                     ;; below.
                     ((looking-at "/>")
                      (forward-char 2)
                      (or ;; XHTML-style empty tag
                       (let ((html-element-info (get w3-p-d-tag-name 'html-element-info)))
                         (and html-element-info
                              (eq 'EMPTY
                                  (w3-element-content-model
                                   html-element-info)))) 
                       ;; XHTML empty element which is not ordinarily
                       ;; empty.  Simulate by inserting an end tag.
                       (save-excursion
                         (insert "</" (symbol-name w3-p-d-tag-name) ">"))))
                     ((and (eq ?/ (char-after (point)))
                           (not w3-p-d-end-tag-p))
                      ;; This is a NET-enabling start-tag.
                      (setq net-tag-p t)
                      (forward-char 1))
                     ((eq ?< (char-after (point)))
                      ;; *** Strictly speaking, the following text has to
                      ;; lexically be STAGO or ETAGO, which means that it
                      ;; can't match some other lexical unit.
                      ;; Unclosed tag.
                      nil)
                     (t
                      ;; Syntax error.
                      (w3-debug-html
                       (format "Bad unclosed %s%s tag"
                               (if w3-p-d-end-tag-p "/" "")
                               (w3-sgml-name-to-string w3-p-d-tag-name)))))
            
               (setq tag-end (point)))
           
              ((looking-at "/?>")
               ;; We are looking at an empty tag (<>, </>).
               (setq w3-p-d-end-tag-p (eq ?/ (char-after (point))))
               (setq w3-p-d-tag-name (if w3-p-d-end-tag-p
                                         (w3-element-name w3-p-d-current-element)
                                       ;; *** Strictly speaking, if OMITTAG NO, then
                                       ;; we should use the most recently closed tag.
                                       ;; But OMITTAG YES in HTML and I'm lazy.
                                       (w3-element-name w3-p-d-current-element)))
               (setq tag-attributes nil)
               ;; *** Make sure this is not at top level.
               (setq between-tags-end (1- (point)))
               (setq tag-end (match-end 0)))
         
              ;; *** In SGML, <(doctype)element> is valid tag syntax.  This
              ;; cannot occur in HTML because the CONCUR option is off in the
              ;; SGML declaration.
         
              ((looking-at "!--")
               ;; We found a comment, delete to end of comment.
               (delete-region
                (1- (point))
                (progn
                  (forward-char 1)
                  ;; Skip over pairs of -- ... --.
                  ;;
                  ;; This can cause us to hit a stack overflow in the regexp
                  ;; engine.  And I'm not sure its correct anyway.  Lets just
                  ;; always fall back to the (semi) non-SGML way of dealing
                  ;; with comments.  WMP  12/24/97
;;;               (if (looking-at "\\(--[^-]*\\(-[^-]+\\)*--[ \t\r\n]*\\)+>")
;;;                   (goto-char (match-end 0))
;;;                 ;; Syntax error!
;;;                 (w3-debug-html
;;;                   "Bad comment (unterminated or unbalanced \"--\" pairs)")
;;;                 (forward-char 2)
;;;                 (or (re-search-forward "--[ \t\r\n]*>" nil t)
;;;                     (search-forward ">" nil t)))
                  (forward-char 2)
                  (or (re-search-forward "--[ \t\r\n]*>" nil t)
                      (search-forward ">" nil t))
                  (point))))
           
              ((looking-at "!>\\|\\?[^>]*>")
               ;; We are looking at an empty comment or a processing
               ;; instruction.  Delete it.
               (replace-match "")
               (delete-char -1))

              ((looking-at "![a-z]")
               ;; We are looking at a markup declaration.  Delete it.
               ;; *** Technically speaking, to handle valid HTML I think we
               ;; need to handle "<!USEMAP ... >" declarations.  In the future,
               ;; to handle general SGML, we should parse "<!DOCTYPE ... >"
               ;; declarations as well (which can contain other declarations).
               ;; In the very distant future, perhaps we will handle "<!SGML
               ;; ... >" declarations.
               ;; *** Should warn if it's not SGML, DOCTYPE, or USEMAP.
               (backward-char 1)
               (delete-region
                (point)
                (progn
                  (condition-case nil
                      (forward-sexp 1)
                    (error
                     ;; *** This might not actually be bad syntax, but might
                     ;; instead be a -- ... -- comment with unbalanced
                     ;; parentheses somewhere inside the declaration.  Handling
                     ;; this properly would require full parsing of markup
                     ;; declarations, a goal for the future.
                     (w3-debug-html "Bad <! syntax.")
                     (skip-chars-forward "^>")
                     (if (eq ?> (char-after (point)))
                         (forward-char))))
                  (point))))
         
              ((looking-at "!\\\[\\(\\([ \t\n\r]*[a-z]+\\)+[ \t\n\r]*\\)\\\[")
               ;; We are looking at a marked section.
               ;; *** Strictly speaking, we should issue a warning if the
               ;; keywords are invalid or missing or if the "[" does not follow.
               ;; We must look at the keywords to understand how to parse it.
               ;; *** Strictly speaking, we should perform parameter entity
               ;; substitution on the keywords first.
               (goto-char (match-beginning 1))
               (insert ?\))
               (goto-char (1- (match-beginning 0)))
               (delete-char 3)
               (insert ?\()
               (backward-char 1)
               (let* ((keywords (read (current-buffer)))
                      ;; Multiple keywords may appear, but only the most
                      ;; significant takes effect.  Rank order is IGNORE, CDATA,
                      ;; RCDATA, INCLUDE, and TEMP.  INCLUDE and TEMP have the
                      ;; same effect.
                      (keyword (car-safe (cond ((memq 'IGNORE keywords))
                                               ((memq 'CDATA keywords))
                                               ((memq 'RCDATA keywords))
                                               ((memq 'INCLUDE keywords))
                                               ((memq 'TEMP keywords))))))
                 (or (eq ?\[ (char-after (point)))
                     ;; I probably shouldn't even check this, since it is so
                     ;; impossible.
                     (error "impossible ??"))
                 (forward-char 1)
                 (delete-region (1- (match-beginning 0)) (point))
                 (cond ((eq 'IGNORE keyword)
                        ;; Scan forward skipping over matching <![ ... ]]>
                        ;; until we find an unmatched "]]>".
                        (let ((ignore-nesting 1)
                              (start-pos (point)))
                          (while (> ignore-nesting 0)
                            (if (re-search-forward "<!\\\\\[\\|\]\]>" nil t)
                                (setq ignore-nesting
                                      (if (eq ?> (preceding-char))
                                          (1- ignore-nesting)
                                        (1+ ignore-nesting)))
                              (w3-debug-html
                               "Unterminated IGNORE marked section.")
                              (setq ignore-nesting 0)
                              (goto-char start-pos)))
                          (delete-region start-pos (point))))
                       ((eq 'CDATA keyword)
                        (error "***unimplemented***"))
                       ((eq 'RCDATA keyword)
                        (error "***unimplemented***"))
                       ((memq keyword '(INCLUDE TEMP))
                        (error "***unimplemented***")))))
              ((and (looking-at "!")
                    w3-netscape-compatible-comments)
               ;; Horribly illegal non-SGML handling of bad HTML on the net.
               ;; This can break valid HTML.
               ;; This arises because Netscape discards anything looking like
               ;; "<!...>".  So people expect they can use this construct as
               ;; a comment.
               (w3-debug-html "Evil <! comment syntax.")
               (backward-char 1)
               (delete-region
                (point)
                (progn
                  (skip-chars-forward "^>")
                  (if (eq ?> (char-after (point)))
                      (forward-char))
                  (point))))
              (t
               ;; This < is not a markup character.  Pretend we didn't notice
               ;; it at all.  We have skipped over the < already, so just loop
               ;; again.
               )))
       
            ((eq ?& (char-after (point)))
             (w3-expand-entity-at-point-maybe))

            ((and (eq ?\] (char-after (point)))
                  w3-p-d-in-parsed-marked-section
                  (looking-at "]]>"))
             ;; *** handle the end of a parsed marked section.
             (error "***unimplemented***"))

            ((and (eq ?/ (char-after (point)))
                  w3-p-d-null-end-tag-enabled)
             ;; We are looking at a null end tag.
             (setq w3-p-d-end-tag-p t)
             (setq between-tags-end (point))
             (setq tag-end (1+ (point)))
             (setq w3-p-d-tag-name (w3-element-name w3-p-d-current-element)))
       
            ;; This can be slow, since we'll hardly ever get here.
            ;; *** Strictly speaking, I think we're supposed to handle
            ;; shortrefs that begin with the same characters as other markup,
            ;; preferring the longest match.
            ;; I will assume that shortrefs never begin with <, &, \], /.
            ((setq ref (catch 'found-shortref
                         (let ((refs w3-p-d-shortrefs))
                           (while refs
                             (if (looking-at (car (car refs)))
                                 (throw 'found-shortref (cdr (car refs))))
                             (setq refs (cdr refs))))))
             ;; We are looking at a shortref for which there is an
             ;; expansion defined in the current syntax.  Replace with the
             ;; expansion, leaving point at the beginning so it will be parsed
             ;; on the next loop.
             ;; *** eek.  This is wrong if the shortref is for an entity with
             ;; CDATA syntax which should not be reparsed for tags.
             (replace-match "")
             (let ((pt (point)))
               (insert ref)
               (goto-char pt)))
         
            ((looking-at (eval-when-compile
                           (concat "[" (w3-invalid-sgml-chars) "]")))
             (w3-debug-html
              (format "Invalid SGML character: %c" (char-after (point))))
             ;; Probably cp1252 or some such without proper MIME spec...
             (insert (w3-resolve-numeric-char
                      (w3-char-int (char-after (point)))))
             (delete-char 1))
            ((eobp)
             ;; We have finished the buffer.  Make sure we process the last
             ;; piece of text, if any.
             (setq between-tags-end (point))
             ;; We have to test what's on the element stack because this
             ;; piece of code gets executed twice.
             (cond ((not (eq '*holder (w3-element-name w3-p-d-current-element)))
                    ;; This forces the calculation of implied omitted end tags.
                    (setq w3-p-d-tag-name '*document)
                    (setq w3-p-d-end-tag-p t)
                    (setq tag-end (point)))))
         
            (t
             (error "unreachable code, this can't happen")))
        
           ;; If we have determined the boundaries of a non-empty between-tags
           ;; region of text, then handle it.
           (cond
            (between-tags-end
             (cond
              ((< between-tags-start between-tags-end)
               ;; We have a non-empty between-tags region.

               ;; We check if it's entirely whitespace, because we record the
               ;; transitions for whitespace separately from those for
               ;; data with non-whitespace characters.
               (goto-char between-tags-start)
               (skip-chars-forward " \t\n\r" between-tags-end)
               (cond
                ((w3-grok-tag-or-data (prog1 
                                          (if (= between-tags-end (point))
                                              '*space
                                            '*data)
                                        (goto-char between-tags-end)))
                 ;; We have to include the text in the current element's
                 ;; contents.  If this is the first item in the current
                 ;; element's contents, don't include a leading newline if
                 ;; there is one.  Add a trailing newline as a separate text
                 ;; item so that it can be removed later if it turns out to
                 ;; be the last item in the current element's contents when
                 ;; the current element is closed.
                 ;; *** We could perform this test before calling
                 ;; w3-grok-tag-or-data, but it's not clear which will be
                 ;; faster in practice.
                 (or (setq content (w3-element-content w3-p-d-current-element))
                     ;; *** Strictly speaking, in SGML the record end is
                     ;; carriage return, not line feed.
                     (if (eq ?\n (char-after between-tags-start))
                         (setq between-tags-start (1+ between-tags-start))))
                 (if (= between-tags-start (point))
                     ;; Do nothing.
                     nil
                   ;; We are definitely going to add data characters to the
                   ;; content.
                   (cond
                    ((and (= ?\n (preceding-char))
                          (/= between-tags-start (1- (point))))
                     (setq content (cons (buffer-substring between-tags-start
                                                           (1- (point)))
                                         content))
                     (setq content (cons "\n" content)))
                    (t
                     (setq content (cons (buffer-substring between-tags-start
                                                           (point))
                                         content))))
                   (w3-set-element-content w3-p-d-current-element content))))))
          
             (setq between-tags-end nil)))
      
           ;; If the previous expression modified (point), then it went to
           ;; the value of between-tags-end.
      
           ;; If we found a start or end-tag, we need to handle it.
           (cond
            (w3-p-d-tag-name
        
             ;; Move past the tag and prepare for next between-tags region.
             (goto-char tag-end)
             (setq between-tags-start (point))
        
             (cond
              (w3-p-d-end-tag-p
               ;; Handle an end-tag.
               (if (eq w3-p-d-tag-name (w3-element-name w3-p-d-current-element))
                   (w3-close-element)
                 ;; Handle the complex version.  We have to search up (down?)
                 ;; the open element stack to find the element that matches (if
                 ;; any).  Then we close all of the elements.  On a conforming
                 ;; SGML document this can do no wrong and it's not
                 ;; unreasonable on a non-conforming document.
            
                 ;; Can't safely modify stack until we know the element we want
                 ;; to find is in there, so work with a copy.
                 (setq open-list w3-p-d-open-element-stack)
                 (while (and open-list
                             (not (eq w3-p-d-tag-name
                                      (w3-element-name (car open-list)))))
                   (setq open-list (cdr open-list)))
                 (cond (open-list
                        ;; We found a match.  Pop elements.
                        ;; We will use the following value as a sentinel.
                        (setq open-list (cdr open-list))
                        (while (not (eq open-list w3-p-d-open-element-stack))
                          (w3-close-element t))
                        (w3-close-element))
                       (t
                        ;; Bogus end tag.
                        (w3-debug-html
                         (format "Unmatched end-tag </%s>"
                                 (w3-sgml-name-to-string w3-p-d-tag-name)))))))
              (t
               ;; Handle a start-tag.
               (cond
                ;; Check if the new element is allowed in the current element's
                ;; content model.
                ((w3-grok-tag-or-data w3-p-d-tag-name)
                 (w3-open-element w3-p-d-tag-name tag-attributes)
            
                 ;; Handle NET-enabling start tags.
                 (cond ((and net-tag-p
                             (not w3-p-d-null-end-tag-enabled))
                        ;; Save old values.
                        (w3-set-element-undo-list 
                         w3-p-d-current-element 
                         (cons (cons 'w3-p-d-non-markup-chars
                                     w3-p-d-non-markup-chars)
                               (cons '(w3-p-d-null-end-tag-enabled . nil)
                                     (w3-element-undo-list w3-p-d-current-element))))
                        ;; Alter syntax.
                        (setq w3-p-d-null-end-tag-enabled t)
                        (w3-update-non-markup-chars)))
            
                 (setq content-model
                       (w3-element-content-model w3-p-d-current-element))
            
                 ;; If the element does not have parsed contents, then we
                 ;; can find its contents immediately.
                 (cond
                  ((memq content-model '(EMPTY CDATA XCDATA XXCDATA RCDATA))
                   (cond
                    ((eq 'EMPTY content-model)
                     (w3-close-element))
                    ((eq 'CDATA content-model)
                     ;; CDATA: all data characters until an end-tag.  We'll
                     ;; process the end-tag on the next loop.
                     (if (re-search-forward (if w3-p-d-null-end-tag-enabled
                                                "</[a-z>]\\|/"
                                              "</[a-z>]")
                                            nil 'move)
                         (goto-char (match-beginning 0))))
                    ((eq 'XCDATA content-model)
                     ;; XCDATA: special non-SGML-standard mode which includes
                     ;; all data characters until "</foo" is seen where "foo"
                     ;; is the name of this element (for XMP and LISTING).
                     (if (search-forward 
                          (concat "</" (symbol-name
                                        (w3-element-name w3-p-d-current-element)))
                          nil 'move)
                         (goto-char (match-beginning 0))))
                    ((eq 'XXCDATA content-model)
                     ;; XXCDATA: special non-SGML-standard mode which includes
                     ;; all data until end-of-entity (end-of-buffer for us)
                     ;; (for PLAINTEXT).
                     (goto-char (point-max)))
                    ((eq 'RCDATA content-model)
                     ;; RCDATA: all data characters until end-tag is seen,
                     ;; except that entities are expanded first, although the
                     ;; expansions are _not_ scanned for end-tags, although the
                     ;; expansions _are_ scanned for further entity
                     ;; references.
                     (while (progn
                              (if (re-search-forward (if w3-p-d-null-end-tag-enabled
                                                         "</[a-z>]\\|[/&]"
                                                       "</[a-z>]\\|&")
                                                     nil 'move)
                                  (goto-char (match-beginning 0)))
                              (eq ?& (char-after (point))))
                       (w3-expand-entity-at-point-maybe)))))))
                (t
                 ;; The element is illegal here.  We'll just discard the start
                 ;; tag as though we never saw it.
                 ))))
        
             (setq w3-p-d-tag-name nil)
             (setq w3-p-d-end-tag-p nil)
             (setq net-tag-p nil)
             (setq tag-attributes nil)
             (setq tag-end nil)))
        
           ;; End of main while loop.
           )
    
         ;; We have finished parsing the buffer!
         (if status-message-format
             (message "%sdone" (format status-message-format 100)))
    
         ;; *** For debugging, save the true parse tree.
         ;; *** Make this look inside *DOCUMENT.
         (setq w3-last-parse-tree
               (w3-element-content w3-p-d-current-element))

         (set-syntax-table old-syntax-table)
         (w3-element-content w3-p-d-current-element)
         )))))

(require 'w3-fast-parse)

(defun w3-parse-buffer (&optional buff)
  "Parse contents of BUFF as HTML.
BUFF defaults to the current buffer.
Destructively alters contents of BUFF.
Returns a data structure containing the parsed information."
  (fset 'w3-parse-buffer (if nil ;; (w3-fast-parse-find-tidy-program)
                             #'w3-fast-parse-buffer
                           #'w3-slow-parse-buffer))
  (w3-parse-buffer buff))



(provide 'w3-parse)

;; Local variables:
;; indent-tabs-mode: nil
;; end:
