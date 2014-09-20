;;; d-mode.el --- D Programming Language mode for (X)Emacs
;;;               Requires a cc-mode of version 5.30 or greater

;; Author:  2007 William Baxter
;; Contributors:  Andrei Alexandrescu
;; Contributors:  Russel Winder
;; Maintainer:  Russel Winder
;; Created:  March 2007
;; Date:  2014-02-06
;; Version: 20140206.1054
;; X-Original-Version:  2.0.7-SNAPSHOT
;; Keywords:  D programming language emacs cc-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Usage:
;; Put these lines in your init file.
;;   (autoload 'd-mode "d-mode" "Major mode for editing D code." t)
;;   (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))
;;
;; cc-mode version 5.30 or greater is required.
;; You can check your cc-mode with the command M-x c-version.
;; You can get the latest version of cc-mode at http://cc-mode.sourceforge.net
;;
;; Commentary:
;;   This mode supports most of D's syntax, including nested /+ +/
;;   comments and backquote `string literals`.
;;
;;   This mode has been dubbed "2.0" because it is a complete rewrite
;;   from scratch.  The previous d-mode was based on cc-mode 5.28 or
;;   so.  This version is based on the cc-mode 5.30 derived mode
;;   example by Martin Stjernholm, 2002.
;;
;;
;; TODO:
;;   Issues with this code are managed via the project issue management
;;   on GitHub: https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/issues?state=open
;;
;;
;; History:
;;   History is tracked in the Git repository rather than in this file.
;;   See https://github.com/Emacs-D-Mode-Maintainers/Emacs-D-Mode/commits/master
;;

;;----------------------------------------------------------------------------
;; Code:

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
;; Comment out 'when-compile part for debugging
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts)
)

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'd-mode 'java-mode))

;; D has pointers
(c-lang-defconst c-type-decl-prefix-key
  d (concat "\\("
		   "[*\(]"
		   "\\|"
		   (c-lang-const c-type-decl-prefix-key)
		   "\\)"
		   "\\([^=]\\|$\\)"))

;; D has fixed arrays
(c-lang-defconst c-opt-type-suffix-key
  d "\\(\\[[^]]*\\]\\|\\.\\.\\.\\)")

(c-lang-defconst c-identifier-ops
  ;; For recognizing "~this", ".foo", and "foo.bar.baz" as identifiers
  d '((prefix "~")(prefix ".")(left-assoc ".")))

(c-lang-defconst c-after-id-concat-ops
  ;; Also for handling ~this
  d '("~"))

(c-lang-defconst c-string-escaped-newlines
  ;; Set to true to indicate the D handles backslash escaped newlines in strings
  d t)

(c-lang-defconst c-multiline-string-start-char
  ;; Set to true to indicate that D doesn't mind raw embedded newlines in strings
  d t)

(c-lang-defconst c-opt-cpp-prefix
  ;; Preprocssor directive recognizer.  D doesn't have cpp, but it has #line
  d "\\s *#\\s *")

(c-lang-defconst c-cpp-message-directives d nil)
(c-lang-defconst c-cpp-include-directives d nil)
(c-lang-defconst c-opt-cpp-macro-define d nil)
(c-lang-defconst c-cpp-expr-directives d nil)
(c-lang-defconst c-cpp-expr-functions d nil)

(c-lang-defconst c-assignment-operators
  ;; List of all assignment operators.
  d  '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" ">>>=" "&=" "^=" "^^="
       "|=" "~="))

(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
 d (append '("/+" "+/" "..." ".." "!" "*" "&")
	    (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-block-comment-starter d "/*")
(c-lang-defconst c-block-comment-ender   d "*/")

(c-lang-defconst c-comment-start-regexp  d "/[*+/]")
(c-lang-defconst c-block-comment-start-regexp d "/[*+]")
(c-lang-defconst c-literal-start-regexp
  ;; Regexp to match the start of comments and string literals.
  d "/[*+/]\\|\"\\|`")
;;(c-lang-defconst c-comment-prefix-regexp d "//+\\|\\**")

(c-lang-defconst c-doc-comment-start-regexp
 ;; doc comments for D use "///",  "/**" or doxygen's "/*!" "//!"
 d "/\\*[*!]\\|//[/!]")

(c-lang-defconst c-block-prefix-disallowed-chars
  ;; Allow ':' for inherit list starters.
  d (set-difference (c-lang-const c-block-prefix-disallowed-chars)
				 '(?:)))

;;----------------------------------------------------------------------------

;; Built-in basic types
(c-lang-defconst c-primitive-type-kwds
  d '("bit" "bool" "byte" "ubyte" "char" "delegate" "double" "float"
      "function" "int" "long" "ubyte" "short" "uint" "ulong" "ushort"
      "cent" "ucent" "real" "ireal" "ifloat" "creal" "cfloat" "cdouble"
      "wchar" "dchar" "void" "string" "wstring" "dstring"))

;; Keywords that can prefix normal declarations of identifiers
(c-lang-defconst c-modifier-kwds
  d '("__gshared" "abstract" "const" "deprecated" "extern"
      "final" "in" "out" "inout" "lazy" "mixin" "override" "private"
      "protected" "public" "ref" "scope" "shared" "static" "synchronized"
      "volatile" "__vector"))

(c-lang-defconst c-class-decl-kwds
  ;; Keywords introducing declarations where the following block (if any)
  ;; contains another declaration level that should be considered a class.
  d '("class" "struct" "union" "interface" "template"))

;; (c-lang-defconst c-brace-list-decl-kwds
;;   d '("enum"))

(c-lang-defconst c-type-modifier-kwds
  d '("__gshared" "const" "inout" "lazy" "shared" "volatile"
      "invariant" "enum" "__vector"))

(c-lang-defconst c-type-prefix-kwds
  ;; Keywords where the following name - if any - is a type name, and
  ;; where the keyword together with the symbol works as a type in
  ;; declarations.  In this case, like "mixin foo!(x) bar;"
  d    '("mixin" "align"))

;;(c-lang-defconst c-other-block-decl-kwds
;;  ;; Keywords where the following block (if any) contains another
;;  ;; declaration level that should not be considered a class.
;;  ;; Each of these has associated offsets e.g.
;;  ;;   'with-open', 'with-close' and 'inwith'
;;  ;; that can be customized individually
;;  ;;   TODO: maybe also do this for 'static if' ?  in/out?
;;  ;;   TODO: figure out how to make this work properly
;;  d '("with" "version" "extern"))

(c-lang-defconst c-typedef-decl-kwds
 d (append (c-lang-const c-typedef-decl-kwds)
	    '("typedef" "alias")))

(c-lang-defconst c-decl-hangon-kwds
  d '("export"))

(c-lang-defconst c-protection-kwds
  ;; Access protection label keywords in classes.
  d '("deprecated" "static" "extern" "final" "synchronized" "override"
      "abstract" "scope" "const" "inout" "shared" "__gshared"
      "private" "package" "protected" "public" "export"))

;;(c-lang-defconst c-postfix-decl-spec-kwds
;;  ;Keywords introducing extra declaration specifiers in the region
;;  ;between the header and the body (i.e. the "K&R-region") in
;;  ;declarations.
;;; This doesn't seem to have any effect.  They aren't exactly "K&R-regions".
;;  d '("in" "out" "body"))

(c-lang-defconst c-type-list-kwds
  d '("import"))

(c-lang-defconst c-ref-list-kwds
  d '("module"))

(c-lang-defconst c-colon-type-list-kwds
  ;; Keywords that may be followed (not necessarily directly) by a colon
  ;; and then a comma separated list of type identifiers.
  d  '("class" "enum" "interface"))

(c-lang-defconst c-paren-nontype-kwds
  ;;Keywords that may be followed by a parenthesis expression that doesn't
  ;; contain type identifiers.
  d '("version" "debug" "extern" "macro" "mixin"))

(c-lang-defconst c-paren-type-kwds
  ;; Keywords that may be followed by a parenthesis expression containing
  ;; type identifiers separated by arbitrary tokens.
  d  '("delete" "throw"))

(c-lang-defconst c-block-stmt-1-kwds
  ;; Statement keywords followed directly by a substatement.
  d '("do" "else" "finally" "try" "in" "out" "body"))

(c-lang-defconst c-block-stmt-2-kwds
  ;; Statement keywords followed by a paren sexp and then by a substatement.
  d '("for" "if" "switch" "while" "catch" "synchronized" "scope"
      "foreach" "foreach_reverse" "with" "unittest"
      "else static if" "else"))

(c-lang-defconst c-simple-stmt-kwds
  ;; Statement keywords followed by an expression or nothing.
  d '("break" "continue" "goto" "return" "throw"))

(c-lang-defconst c-paren-stmt-kwds
  ;; Statement keywords followed by a parenthesis expression that
  ;; nevertheless contains a list separated with ';' and not ','."
  d '("for" "foreach" "foreach_reverse"))

(c-lang-defconst c-asm-stmt-kwds
  ;; Statement keywords followed by an assembler expression.
  d '("asm"))

(c-lang-defconst c-label-kwds
  ;; Keywords introducing colon terminated labels in blocks.
  d '("case" "default"))

(c-lang-defconst c-before-label-kwds
  ;; Keywords that might be followed by a label identifier.
  d    '("goto" "break" "continue"))

(c-lang-defconst c-constant-kwds
  ;; Keywords for constants.
  d '("null" "true" "false"))

(c-lang-defconst c-primary-expr-kwds
  ;; Keywords besides constants and operators that start primary expressions.
  d '("this" "super"))

(c-lang-defconst c-inexpr-class-kwds
  ;; Keywords that can start classes inside expressions.
  d    nil)

(c-lang-defconst c-inexpr-brace-list-kwds
  ;; Keywords that can start brace list blocks inside expressions.
  d    nil)

(c-lang-defconst c-other-decl-kwds
  d '("module" "import"))

(c-lang-defconst c-other-kwds
  ;; Keywords not accounted for by any other `*-kwds' language constant.
  d '("__gshared" "__traits" "assert" "cast" "is" "nothrow" "pure" "ref"
      "sizeof" "typeid" "typeof"))


(defcustom d-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in D mode.
   Each list item should be a regexp matching a single identifier."
  :group 'd-mode)

(defconst d-font-lock-keywords-1 (c-lang-const c-matchers-1 d)
  "Minimal highlighting for D mode.")

(defconst d-font-lock-keywords-2 (c-lang-const c-matchers-2 d)
  "Fast normal highlighting for D mode.")

(defconst d-font-lock-keywords-3 (c-lang-const c-matchers-3 d)
  "Accurate normal highlighting for D mode.")

(defvar d-font-lock-keywords d-font-lock-keywords-3
  "Default expressions to highlight in D mode.")

(defvar d-mode-syntax-table nil
  "Syntax table used in d-mode buffers.")
(or d-mode-syntax-table
    (setq d-mode-syntax-table
	 (let ((table (funcall (c-lang-const c-make-mode-syntax-table d))))
	   ;; Make it recognize D `backquote strings`
	   (modify-syntax-entry ?` "\"" table)

	   ;; Make it recognize D's nested /+ +/ comments
	   (modify-syntax-entry ?+  ". 23n"   table)
	   table)))

(defvar d-mode-abbrev-table nil
  "Abbreviation table used in d-mode buffers.")
(c-define-abbrev-table 'd-mode-abbrev-table
  ;; Use the abbrevs table to trigger indentation actions
  ;; on keywords that, if they occur first on a line, might alter the
  ;; syntactic context.
  ;; Syntax for abbrevs is:
  ;; ( pattern replacement command initial-count)
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar d-mode-map ()
  "Keymap used in d-mode buffers.")
(if d-mode-map
    nil
  (setq d-mode-map (c-make-inherited-keymap))
  ;; Add bindings which are only useful for D
  ;; (define-key d-mode-map "\C-c\C-e"  'd-cool-function)
  )

(c-lang-defconst c-mode-menu
  ;; The definition for the mode menu.  The menu title is prepended to
  ;; this before it's fed to `easy-menu-define'.
  t `(["Comment Out Region"     comment-region
       (c-fn-region-is-active-p)]
      ["Uncomment Region"       (comment-region (region-beginning)
						(region-end) '(4))
       (c-fn-region-is-active-p)]
      ["Indent Expression"      c-indent-exp
       (memq (char-after) '(?\( ?\[ ?\{))]
      ["Indent Line or Region"  c-indent-line-or-region t]
      ["Fill Comment Paragraph" c-fill-paragraph t]
      "----"
      ["Backward Statement"     c-beginning-of-statement t]
      ["Forward Statement"      c-end-of-statement t]
      "----"
      ("Toggle..."
       ["Syntactic indentation" c-toggle-syntactic-indentation
	:style toggle :selected c-syntactic-indentation]
       ["Electric mode"         c-toggle-electric-state
	:style toggle :selected c-electric-flag]
       ["Auto newline"          c-toggle-auto-newline
	:style toggle :selected c-auto-newline]
       ["Hungry delete"         c-toggle-hungry-state
	:style toggle :selected c-hungry-delete-key]
       ["Subword mode"          c-subword-mode
	:style toggle :selected (and (boundp 'c-subword-mode)
                                     c-subword-mode)])))

(easy-menu-define d-menu d-mode-map "D Mode Commands"
  (cons "D" (c-lang-const c-mode-menu d)))

(defconst d-imenu-method-name-pattern
  (concat
   "^\\s-*"
   "\\(?:[_a-z@]+\\s-+\\)*"             ; qualifiers
   "\\([][_a-zA-Z0-9.*!]+\\)\\s-+"      ; type
   "\\([_a-zA-Z0-9]+\\)\\s-*"           ; function name
   "\\(?:([^)]*)\\s-*\\)?"              ; type arguments
   "([^)]*)\\s-*"                       ; arguments
   "\\(?:[a-z]+\\s-*\\)?"               ; pure/const etc.
   "\\(?:;\\|[ \t\n]*\\(?:if\\|{\\)\\)")) ; ';' or 'if' or '{'

(defun d-imenu-method-index-function ()
  (and
   (let ((pt))
     (setq pt (re-search-backward d-imenu-method-name-pattern nil t))
     ;; The method name regexp will match lines like
     ;; "return foo(x);" or "static if(x) {"
     ;; so we exclude type name 'static' or 'return' here
     (while (let ((type (match-string 1)))
              (and pt type
                   (save-match-data
                     (string-match (c-lang-const c-regular-keywords-regexp) type))))
       (setq pt (re-search-backward d-imenu-method-name-pattern nil t)))
     pt)
   ;; Do not count invisible definitions.
   (let ((invis (invisible-p (point))))
     (or (not invis)
         (progn
           (while (and invis
                       (not (bobp)))
             (setq invis (not (re-search-backward
                               d-imenu-method-name-pattern nil 'move))))
           (not invis))))))

(defvar d-imenu-generic-expression
  `(("*Classes*" "^\\s-*\\<class\\s-+\\([a-zA-Z0-9_]+\\)" 1)
	("*Interfaces*" "^\\s-*\\<interface\\s-+\\([a-zA-Z0-9_]+\\)" 1)
	("*Structs*" "^\\s-*\\<struct\\s-+\\([a-zA-Z0-9_]+\\)" 1)
	("*Templates*" "^\\s-*\\(?:mixin\\s-+\\)?\\<template\\s-+\\([a-zA-Z0-9_]+\\)" 1)
    (nil d-imenu-method-index-function 2)))

;;----------------------------------------------------------------------------
;;;###autoload (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; For compatibility with Emacs < 24
(defalias 'd-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode d-mode d-parent-mode "D"
  "Major mode for editing code written in the D Programming Language.
See http://www.digitalmars.com/d for more information about the D language.
The hook `c-mode-common-hook' is run with no args at mode
initialization, then `d-mode-hook'.

Key bindings:
\\{d-mode-map}"
  (c-initialize-cc-mode t)
  (setq local-abbrev-table d-mode-abbrev-table
        abbrev-mode t)
  (use-local-map d-mode-map)
  (c-init-language-vars d-mode)
  (c-common-init 'd-mode)
  (easy-menu-add d-menu)
  (c-run-mode-hooks 'c-mode-common-hook 'd-mode-hook)
  (c-update-modeline)
  (cc-imenu-init d-imenu-generic-expression))

;; Hideous hacks!
;;
;; * auto/immutable: If we leve them in c-modifier-kwds (like
;;   c++-mode) then in the form "auto var;" var will be highlighted in
;;   type name face. Moving auto/immutable to font-lock-add-keywords
;;   lets cc-mode seeing them as a type name, so the next symbol can
;;   be fontified as a variable.
;;
;; * public/protected/private appear both in c-modifier-kwds and in
;;   c-protection-kwds. This causes cc-mode to fail parsing the first
;;   declaration after an access level label (because cc-mode trys to
;;   parse them as modifier but will fail due to the colon). But
;;   unfortunately we cannot remove them from either c-modifier-kwds
;;   or c-protection-kwds. Removing them from the former causes valid
;;   syntax like "private int foo() {}" to fail. Removing them from
;;   the latter cause indentation of the access level labels to
;;   fail. The solution used here is to use font-lock-add-keywords to
;;   add back the syntax highlight.

(defconst d-var-decl-pattern "^[ \t]*\\(?:[_a-zA-Z0-9]+[ \t\n]+\\)*\\([_a-zA-Z0-9.!]+\\)\\(?:\\[[^]]*\\]\\|\\*\\)?[ \t\n]+\\([_a-zA-Z0-9]+\\)[ \t\n]*[;=]")
(defconst d-fun-decl-pattern "^[ \t]*\\(?:[_a-zA-Z0-9]+[ \t\n]+\\)*\\([_a-zA-Z0-9.!]+\\)\\(?:\\[[^]]*\\]\\|\\*\\)?[ \t\n]+\\([_a-zA-Z0-9]+\\)[ \t\n]*(")
(defmacro d-try-match-decl (regex)
  `(let ((pt))
     (setq pt (re-search-forward ,regex limit t))
     (while (let ((type (match-string 1)))
              (and pt type
                   (save-match-data
                     (string-match (c-lang-const c-regular-keywords-regexp) type))))
       (setq pt (re-search-forward ,regex limit t)))
     pt))
(defun d-match-var-decl (limit)
  (d-try-match-decl d-var-decl-pattern))
(defun d-match-fun-decl (limit)
  (d-try-match-decl d-fun-decl-pattern))
(defun d-match-auto (limit)
  (c-syntactic-re-search-forward "\\<\\(auto\\|immutable\\)\\>" limit t))

(font-lock-add-keywords
 'd-mode
 '((d-match-auto 1 font-lock-keyword-face t)
   (d-match-var-decl (1 font-lock-type-face) (2 font-lock-variable-name-face))
   (d-match-fun-decl (1 font-lock-type-face) (2 font-lock-function-name-face)))
 t)


(provide 'd-mode)

;;; d-mode.el ends here
