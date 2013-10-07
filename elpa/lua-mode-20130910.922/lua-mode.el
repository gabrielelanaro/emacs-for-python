;;; lua-mode.el --- a major-mode for editing Lua scripts

;; Author: 2011-2013 immerrr <immerrr+lua@gmail.com>
;;         2010-2011 Reuben Thomas <rrt@sc3d.org>
;;         2006 Juergen Hoetzel <juergen@hoetzel.info>
;;         2004 various (support for Lua 5 and byte compilation)
;;         2001 Christian Vogler <cvogler@gradient.cis.upenn.edu>
;;         1997 Bret Mogilefsky <mogul-lua@gelatinous.com> starting from
;;              tcl-mode by Gregor Schmid <schmid@fb3-s7.math.tu-berlin.de>
;;              with tons of assistance from
;;              Paul Du Bois <pld-lua@gelatinous.com> and
;;              Aaron Smith <aaron-lua@gelatinous.com>.
;;
;; URL:         http://immerrr.github.com/lua-mode
;; Version: 20130910.922
;; X-Original-Version:     20130419
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;; Keywords: languages, processes, tools

;; This field is expanded to commit SHA, date & associated heads/tags during
;; archive creation.
;; Revision: $Format:%h (%cD %d)$
;;

;;; Commentary:

;; Thanks to d87 <github.com/d87> for an idea of highlighting lua
;; builtins/numbers

;; Thanks to Vedat Hallac <github.com/vhallac> for sharing some of
;; his fixes and updates to core indentation logics

;; Thanks to Rafael Sanchez <rafael@cornerdimension.com> for patch
;; adding lua-mode to interpreter-mode-alist

;; Thanks to Leonardo Etcheverry <leo@kalio.net> for enabling
;; narrow-to-defun functionality

;; Thanks to Tobias Polzin <polzin@gmx.de> for function indenting
;; patch: Indent "(" like "{"

;; Thanks to Fabien <fleutot@gmail.com> for imenu patches.

;; Thanks to Simon Marshall <simonm@mail.esrin.esa.it> and Olivier
;; Andrieu <oandrieu@gmail.com> for font-lock patches.

;; Additional font-lock highlighting and indentation tweaks by
;; Adam D. Moss <adam@gimp.org>.

;; INSTALLATION:

;; To install, just copy this file into a directory on your load-path
;; (and byte-compile it). To set up Emacs to automatically edit files
;; ending in ".lua" or with a lua hash-bang line using lua-mode add
;; the following to your init file:
;;
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Usage

;; Lua-mode supports c-mode style formatting and sending of
;; lines/regions/files to a Lua interpreter. An interpreter (see
;; variable `lua-default-application') will be started if you try to
;; send some code and none is running. You can use the process-buffer
;; (named after the application you chose) as if it were an
;; interactive shell. See the documentation for `comint.el' for
;; details.

;; Lua-mode works with Hide Show minor mode (see ``hs-minor-mode``).

;; Key-bindings

;; To see all the keybindings for Lua mode, look at `lua-setup-keymap'
;; or start `lua-mode' and type `\C-h m'.
;; The keybindings may seem strange, since I prefer to use them with
;; lua-prefix-key set to nil, but since those keybindings are already used
;; the default for `lua-prefix-key' is `\C-c', which is the conventional
;; prefix for major-mode commands.

;; You can customise the keybindings either by setting `lua-prefix-key'
;; or by putting the following in your .emacs
;;      (define-key lua-mode-map <your-key> <function>)
;; for all the functions you need.


;;; Code:
(eval-when-compile
  (require 'cl))

(require 'comint)

(eval-and-compile
  ;; Backward compatibility for Emacsen < 24.1
  (defalias 'lua--prog-mode
    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

  (defalias 'lua--cl-assert
    (if (fboundp 'cl-assert) 'cl-assert 'assert))

  (defalias 'lua--cl-labels
    (if (fboundp 'cl-labels) 'cl-labels 'flet))

  ;; for Emacsen < 22.1
  (defalias 'lua--with-no-warnings
    (if (fboundp 'with-no-warnings) 'with-no-warnings 'progn))

  ;; provide backward compatibility for Emacs < 23.2
  ;; called-interactively-p receives an argument starting from Emacs 23.2
  ;; In Emacs 22 & Emacs 23.1 it didn't expect an argument
  ;; In Emacs 21 it was called interactively-p
  (condition-case nil
      (progn (called-interactively-p nil)
             ;; if first call succeeds, make lua-called-interactively-p an alias
             (defalias 'lua--called-interactively-p 'called-interactively-p))

    (wrong-number-of-arguments
     ;; wrong number of arguments means it's 22.1 <= Emacs < 23.2
     ;;
     ;; Newer and smarter Emacsen will warn about obsolete functions
     ;; and/or wrong number of arguments. Turning these warnings off,
     ;; since it's backward-compatibility-oriented code anyway.
     (lua--with-no-warnings
       (defun lua--called-interactively-p (kind)
         "Return t if containing function was called interactively.

This function provides lua-mode backward compatibility for
pre-23.2 Emacsen."
         (if (eq kind 'interactive)
             (interactive-p)
           (called-interactively-p)))))

    ;; if not, it's probably < 22.1, provide partial compatibility
    ;;
    ;; Once again, turning obsolete-function warnings off (see above).
    (error
     (lua--with-no-warnings
       (defun lua--called-interactively-p (&rest opts)
         "Return t if containing function was called interactively.

This function provides lua-mode backward compatibility for pre-22
Emacsen."
         (interactive-p)))))

  ;; backward compatibility for Emacsen < 23.3
  ;; Emacs 23.3 introduced with-silent-modifications macro
  (if (fboundp 'with-silent-modifications)
      (defalias 'lua--with-silent-modifications 'with-silent-modifications)

    (defmacro lua--with-silent-modifications (&rest body)
      "Execute BODY, pretending it does not modifies the buffer.

This is a reimplementation of macro `with-silent-modifications'
for Emacsen that doesn't contain one (pre-23.3)."
      `(let ((old-modified-p (buffer-modified-p))
            (inhibit-modification-hooks t)
            (buffer-undo-list t))

        (unwind-protect
            ,@body
          (set-buffer-modified-p old-modified-p))))))

;; Local variables
(defgroup lua nil
  "Major mode for editing lua code."
  :prefix "lua-"
  :group 'languages)

(defcustom lua-indent-level 3
  "Amount by which Lua subexpressions are indented."
  :type 'integer
  :group 'lua)

(defcustom lua-comment-start "-- "
  "Default value of `comment-start'."
  :type 'string
  :group 'lua)

(defcustom lua-comment-start-skip "-- "
  "Default value of `comment-start-skip'."
  :type 'string
  :group 'lua)

(defcustom lua-default-application "lua"
  "Default application to run in lua subprocess."
  :type 'string
  :group 'lua)

(defcustom lua-default-command-switches (list "-i")
  "Command switches for `lua-default-application'.
Should be a list of strings."
  :type '(repeat string)
  :group 'lua)

(defcustom lua-always-show t
  "*Non-nil means display lua-process-buffer after sending a command."
  :type 'boolean
  :group 'lua)

(defcustom lua-search-url-prefix "http://www.lua.org/manual/5.1/manual.html#pdf-"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'lua)

(defvar lua-process nil
  "The active Lua subprocess")

(defvar lua-process-buffer nil
  "Buffer used for communication with Lua subprocess")

(defun lua--customize-set-prefix-key (prefix-key-sym prefix-key-val)
  (lua--cl-assert (eq prefix-key-sym 'lua-prefix-key))
  (set prefix-key-sym (if (and prefix-key-val (> (length prefix-key-val) 0))
                          ;; read-kbd-macro returns a string or a vector
                          ;; in both cases (elt x 0) is ok
                          (elt (read-kbd-macro prefix-key-val) 0)))
  (if (fboundp 'lua-prefix-key-update-bindings)
      (lua-prefix-key-update-bindings)))

(defcustom lua-prefix-key "\C-c"
  "Prefix for all lua-mode commands."
  :type 'string
  :group 'lua
  :set 'lua--customize-set-prefix-key
  :get '(lambda (sym)
          (let ((val (eval sym))) (if val (single-key-description (eval sym)) ""))))

(defvar lua-mode-menu (make-sparse-keymap "Lua")
  "Keymap for lua-mode's menu.")

(defvar lua-prefix-mode-map
  (eval-when-compile
    (let ((result-map (make-sparse-keymap)))
      (mapc (lambda (key_defn)
              (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
            '(("C-l" . lua-send-buffer)
              ("C-f" . lua-search-documentation)))
      result-map))
  "Keymap that is used to define keys accessible by `lua-prefix-key'.

If the latter is nil, the keymap translates into `lua-mode-map' verbatim.")

(defvar lua-mode-map
  (let ((result-map (make-sparse-keymap))
        prefix-key)
    (mapc (lambda (key_defn)
            (define-key result-map (read-kbd-macro (car key_defn)) (cdr key_defn)))
          ;; here go all the default bindings
          ;; backquote enables evaluating certain symbols by comma
          `(("}" . lua-electric-match)
            ("]" . lua-electric-match)
            (")" . lua-electric-match)))
    (define-key result-map [menu-bar lua-mode] (cons "Lua" lua-mode-menu))

    ;; FIXME: see if the declared logic actually works
    ;; handle prefix-keyed bindings:
    ;; * if no prefix, set prefix-map as parent, i.e.
    ;;      if key is not defined look it up in prefix-map
    ;; * if prefix is set, bind the prefix-map to that key
    (if (boundp 'lua-prefix-key)
        (define-key result-map (vector lua-prefix-key) lua-prefix-mode-map)
      (set-keymap-parent result-map lua-prefix-mode-map))
    result-map)
  "Keymap used in lua-mode buffers.")

(defvar lua-electric-flag t
  "If t, electric actions (like automatic reindentation) will happen when an electric
 key like `{' is pressed")
(make-variable-buffer-local 'lua-electric-flag)

(defcustom lua-prompt-regexp "[^\n]*\\(>[\t ]+\\)+$"
  "Regexp which matches the Lua program's prompt."
  :type  'regexp
  :group 'lua)

(defcustom lua-traceback-line-re
  "^\\(?:[\t ]*\\|.*>[\t ]+\\)\\([^\n\t ]+\\):\\([0-9]+\\):"
  "Regular expression that describes tracebacks and errors."
  :type 'regexp
  :group 'lua)

(defcustom lua-indent-string-contents nil
  "If non-nil, contents of multiline string will be indented.
Otherwise leading amount of whitespace on each line is preserved."
  :group 'lua
  :type 'boolean)

(defcustom lua-jump-on-traceback t
  "*Jump to innermost traceback location in *lua* buffer.  When this
variable is non-nil and a traceback occurs when running Lua code in a
subprocess, jump immediately to the source code of the innermost
traceback location."
  :type 'boolean
  :group 'lua)

(defcustom lua-mode-hook nil
  "Hooks called when Lua mode fires up."
  :type 'hook
  :group 'lua)

(defvar lua-region-start (make-marker)
  "Start of special region for Lua communication.")

(defvar lua-region-end (make-marker)
  "End of special region for Lua communication.")

(defvar lua-emacs-menu
  '(["Restart With Whole File" lua-restart-with-whole-file t]
    ["Kill Process" lua-kill-process t]
    ["Hide Process Buffer" lua-hide-process-buffer t]
    ["Show Process Buffer" lua-show-process-buffer t]
    ["Beginning Of Proc" lua-beginning-of-proc t]
    ["End Of Proc" lua-end-of-proc t]
    ["Set Lua-Region Start" lua-set-lua-region-start t]
    ["Set Lua-Region End" lua-set-lua-region-end t]
    ["Send Lua-Region" lua-send-lua-region t]
    ["Send Current Line" lua-send-current-line t]
    ["Send Region" lua-send-region t]
    ["Send Proc" lua-send-proc t]
    ["Send Buffer" lua-send-buffer t]
    ["Search Documentation" lua-search-documentation t])
  "Emacs menu for Lua mode.")

;; the whole defconst is inside eval-when-compile, because it's later referenced
;; inside another eval-and-compile block
(eval-and-compile
  (defconst
    lua--builtins
    (let*
        ((modules
          '("_G" "_VERSION" "assert" "collectgarbage" "dofile" "error" "getfenv"
            "getmetatable" "ipairs" "load" "loadfile" "loadstring" "module"
            "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen" "rawset"
            "require" "select" "setfenv" "setmetatable" "tonumber" "tostring"
            "type" "unpack" "xpcall" "self"
            ("bit32" . ("arshift" "band" "bnot" "bor" "btest" "bxor" "extract"
                        "lrotate" "lshift" "replace" "rrotate" "rshift"))
            ("coroutine" . ("create" "resume" "running" "status" "wrap" "yield"))
            ("debug" . ("debug" "getfenv" "gethook" "getinfo" "getlocal"
                        "getmetatable" "getregistry" "getupvalue" "getuservalue"
                        "setfenv" "sethook" "setlocal" "setmetatable"
                        "setupvalue" "setuservalue" "traceback" "upvalueid"
                        "upvaluejoin"))
            ("io" . ("close" "flush" "input" "lines" "open" "output" "popen"
                     "read" "stderr" "stdin" "stdout" "tmpfile" "type" "write"))
            ("math" . ("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cosh"
                       "deg" "exp" "floor" "fmod" "frexp" "huge" "ldexp" "log"
                       "log10" "max" "min" "modf" "pi" "pow" "rad" "random"
                       "randomseed" "sin" "sinh" "sqrt" "tan" "tanh"))
            ("os" . ("clock" "date" "difftime" "execute" "exit" "getenv"
                     "remove"  "rename" "setlocale" "time" "tmpname"))
            ("package" . ("config" "cpath" "loaded" "loaders" "loadlib" "path"
                          "preload" "searchers" "searchpath" "seeall"))
            ("string" . ("byte" "char" "dump" "find" "format" "gmatch" "gsub"
                         "len" "lower" "match" "rep" "reverse" "sub" "upper"))
            ("table" . ("concat" "insert" "maxn" "pack" "remove" "sort" "unpack"
                        )))))

      ;; This code uses \\< and \\> to delimit builtin symbols instead of
      ;; \\_< and \\_>, because -- a necessity -- '.' syntax class is hacked
      ;; to 'symbol' and \\_> won't detect a symbol boundary in 'foo.bar' and
      ;; -- sufficiency -- conveniently, underscore '_' is hacked to count as
      ;; word constituent, but only for font-locking. Neither of these hacks
      ;; makes sense to me, I'm going to wipe them out as soon as I'm sure
      ;; that indentation won't get hurt. --immerrr
      ;;
      (lua--cl-labels
       ((module-name-re (x)
                        (concat "\\(?1:\\<"
                                (if (listp x) (car x) x)
                                "\\>\\)"))
        (module-members-re (x) (if (listp x)
                                   (concat "\\(?:[ \t]*\\.[ \t]*"
                                           "\\<\\(?2:"
                                           (regexp-opt (cdr x))
                                           "\\)\\>\\)?")
                                 "")))

       (concat
        ;; common prefix:
        ;; - beginning-of-line
        ;; - or neither of [ '.', ':' ] to exclude "foo.string.rep"
        ;; - or concatenation operator ".."
        "\\(?:^\\|[^:. \t]\\|[.][.]\\)"
        ;; optional whitespace
        "[ \t]*"
        "\\(?:"
        ;; any of modules/functions
        (mapconcat (lambda (x) (concat (module-name-re x)
                                       (module-members-re x)))
                   modules
                   "\\|")
        "\\)"))))

  "A regexp that matches lua builtin functions & variables.

This is a compilation of 5.1 and 5.2 builtins taken from the
index of respective Lua reference manuals.")

(eval-and-compile
  (defun lua-make-delimited-matcher (elt-regexp sep-regexp end-regexp)
    "Construct matcher function for `font-lock-keywords' to match a sequence.

It's supposed to match sequences with following EBNF:

ELT-REGEXP { SEP-REGEXP ELT-REGEXP } END-REGEXP

The sequence is parsed one token at a time.  If non-nil is
returned, `match-data' will have one or more of the following
groups set according to next matched token:

1. matched element token
2. unmatched garbage characters
3. misplaced token (i.e. SEP-REGEXP when ELT-REGEXP is expected)
4. matched separator token
5. matched end token

Blanks & comments between tokens are silently skipped.
Groups 6-9 can be used in any of argument regexps."
    (lexical-let*
        ((delimited-matcher-re-template
          "\\=\\(?2:.*?\\)\\(?:\\(?%s:\\(?4:%s\\)\\|\\(?5:%s\\)\\)\\|\\(?%s:\\(?1:%s\\)\\)\\)")
         ;; There's some magic to this regexp. It works as follows:
         ;;
         ;; A. start at (point)
         ;; B. non-greedy match of garbage-characters (?2:)
         ;; C. try matching separator (?4:) or end-token (?5:)
         ;; D. try matching element (?1:)
         ;;
         ;; Simple, but there's a trick: pt.C and pt.D are embraced by one more
         ;; group whose purpose is determined only after the template is
         ;; formatted (?%s:):
         ;;
         ;; - if element is expected, then D's parent group becomes "shy" and C's
         ;;   parent becomes group 3 (aka misplaced token), so if D matches when
         ;;   an element is expected, it'll be marked with warning face.
         ;;
         ;; - if separator-or-end-token is expected, then it's the opposite:
         ;;   C's parent becomes shy and D's will be matched as misplaced token.
         (elt-expected-re (format delimited-matcher-re-template
                                  3 sep-regexp end-regexp "" elt-regexp))
         (sep-or-end-expected-re (format delimited-matcher-re-template
                                         "" sep-regexp end-regexp 3 elt-regexp)))

      (lambda (end)
        (let* ((prev-elt-p (match-beginning 1))
               (prev-sep-p (match-beginning 4))
               (prev-end-p (match-beginning 5))

               (regexp (if prev-elt-p sep-or-end-expected-re elt-expected-re))
               (comment-start (lua-comment-start-pos (syntax-ppss)))
               (parse-stop end))

          ;; If token starts inside comment, or end-token was encountered, stop.
          (when (and (not comment-start)
                     (not prev-end-p))
            ;; Skip all comments & whitespace. forward-comment doesn't have boundary
            ;; argument, so make sure point isn't beyond parse-stop afterwards.
            (while (and (< (point) end)
                        (forward-comment 1)))
            (goto-char (min (point) parse-stop))

            ;; Reuse comment-start variable to store beginning of comment that is
            ;; placed before line-end-position so as to make sure token search doesn't
            ;; enter that comment.
            (setq comment-start
                  (lua-comment-start-pos
                   (save-excursion
                     (parse-partial-sexp (point) parse-stop
                                         nil nil nil 'stop-inside-comment)))
                  parse-stop (or comment-start parse-stop))

            ;; Now, let's match stuff.  If regular matcher fails, declare a span of
            ;; non-blanks 'garbage', and the next iteration will start from where the
            ;; garbage ends.  If couldn't match any garbage, move point to the end
            ;; and return nil.
            (or (re-search-forward regexp parse-stop t)
                (re-search-forward "\\(?1:\\(?2:[^ \t]+\\)\\)" parse-stop 'skip)
                (prog1 nil (goto-char end)))))))))

(defconst lua-local-defun-regexp
  ;; Function matchers are very crude, need rewrite at some point.
  (rx (or (seq (regexp "\\(?:\\_<function\\_>\\)")
               (* blank)
               (? (regexp "\\(?1:\\_<[[:alpha:]][[:alnum:]]*\\_>\\)"))
               (regexp "\\(?2:.*\\)"))
          (seq (? (regexp "\\(?1:\\_<[[:alpha:]][[:alnum:]]*\\_>\\)"))
               (* blank) "=" (* blank)
               (regexp "\\(?:\\_<function\\_>\\)")
               (regexp "\\(?2:.*\\)")))))

(defvar lua-font-lock-keywords
  `(;; highlight the hash-bang line "#!/foo/bar/lua" as comment
    ("^#!.*$" . font-lock-comment-face)

    ;; Builtin constants
    (,(rx symbol-start (or "true" "false" "nil") symbol-end)
     . font-lock-constant-face)

    ;; Keywords
    (,(rx symbol-start
          (or "and" "break" "do" "else" "elseif" "end"
              "for" "function" "if" "in" "local" "not"
              "or" "repeat" "return" "then" "until"
              "while")
          symbol-end)
     . font-lock-keyword-face)

    ;; Highlight lua builtin functions and variables
    (,lua--builtins
     (1 font-lock-builtin-face) (2 font-lock-builtin-face nil noerror))

    ;; hexadecimal numbers
    ("\\_<0x[[:xdigit:]]+\\_>" . font-lock-constant-face)

    ;; regular numbers
    ;;
    ;; This regexp relies on '.' being symbol constituent. Whenever this
    ;; changes, the regexp needs revisiting --immerrr
    (, (rx symbol-start
           ;; make a digit on either side of dot mandatory
           (or (seq (+ num) (? ".") (* num))
               (seq (* num) (? ".") (+ num)))
           (? (regexp "[eE][+-]?") (+ num))
           symbol-end)
       . font-lock-constant-face)

    ("^[ \t]*\\_<for\\_>"
     (,(lua-make-delimited-matcher "\\_<[[:alpha:]_][[:alnum:]_]*\\_>" ","
                                   "\\(?:\\_<in\\_>\\|=\\(?:[^=]\\|$\\)\\)")
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    ;; Handle local variable/function names
    ;;  local blalba, xyzzy =
    ;;        ^^^^^^  ^^^^^
    ;;
    ;;  local function foobar(x,y,z)
    ;;                 ^^^^^^
    ;;  local foobar = function(x,y,z)
    ;;        ^^^^^^
    ("^[ \t]*\\_<local\\_>"
     (0 font-lock-keyword-face)

     ((lambda (end)
        (re-search-forward
         (rx point (* blank) (regexp ,lua-local-defun-regexp)) end t))
      nil nil
      (1 font-lock-function-name-face nil noerror))

     (,(lua-make-delimited-matcher "\\_<[[:alpha:]_][[:alnum:]_]*\\_>" "," 
                                   "=\\(?:[^=]\\|$\\)")
      nil nil
      (1 font-lock-variable-name-face nil noerror)
      (2 font-lock-warning-face t noerror)
      (3 font-lock-warning-face t noerror)))

    ;; Function matchers are very crude, need rewrite at some point.
    ;; Function name declarations.
    ("^[ \t]*\\_<function\\_>[ \t]+\\([[:alnum:]_]+\\(?:\\.[[:alnum:]_]+\\)*\\(?::[[:alnum:]_]+\\)?\\)"
     (1 font-lock-function-name-face))

    ;; Function matchers are very crude, need rewrite at some point.
    ;; Handle function names in assignments
    ("^[ \t]*\\([[:alnum:]_]+\\(?:\\.[[:alnum:]_]+\\)*\\(?::[[:alnum:]_]+\\)?\\)[ \t]*=[ \t]*\\_<function\\_>"
     (1 font-lock-function-name-face)))

  "Default expressions to highlight in Lua mode.")

(defvar lua-imenu-generic-expression
  ;; This regexp matches expressions which look like function
  ;; definitions, but are not necessarily allowed by Lua syntax.  This
  ;; is done on purpose to avoid frustration when making a small error
  ;; might cause a function get hidden from imenu index. --immerrr
  '((nil "^[ \t]*\\(?:local[ \t]+\\)?function[ \t]+\\([[:alnum:]_:.]+\\)" 1)
    (nil "^[ \t]*\\(?:local[ \t]+\\)?\\(\\_<[[:alnum:]_:.]+\\_>\\)[ \t]*=\[ \t]*\\_<function\\_>" 1))
  "Imenu generic expression for lua-mode.  See `imenu-generic-expression'.")

(defvar lua-sexp-alist '(("then" . "end")
                         ("function" . "end")
                         ("do" . "end")))

(defvar lua-mode-abbrev-table nil
  "Abbreviation table used in lua-mode buffers.")

(define-abbrev-table 'lua-mode-abbrev-table
  ;; Emacs 23 introduced :system property that prevents abbrev
  ;; entries from being written to file specified by abbrev-file-name
  ;;
  ;; Emacs 22 and earlier had this functionality implemented
  ;; by simple nil/non-nil flag as positional parameter
  (if (>= emacs-major-version 23)
      '(("end"    "end"    lua-indent-line :system t)
        ("else"   "else"   lua-indent-line :system t)
        ("elseif" "elseif" lua-indent-line :system t))
    '(("end"    "end"      lua-indent-line nil 'system)
      ("else"   "else"     lua-indent-line nil 'system)
      ("elseif" "elseif"   lua-indent-line nil 'system))))

(eval-and-compile
  (defalias 'lua-make-temp-file
    (if (fboundp 'make-temp-file)
        'make-temp-file
      (lambda (prefix &optional dir-flag) ;; Simple implementation
        (expand-file-name
         (make-temp-name prefix)
         (if (fboundp 'temp-directory)
             (temp-directory)
           temporary-file-directory))))))

(defvar lua-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; main comment syntax: begins with "--", ends with "\n"
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?\n ">")

    ;; main string syntax: bounded by ' or "
    (modify-syntax-entry ?\' "\"")
    (modify-syntax-entry ?\" "\"")

    ;; single-character binary operators: punctuation
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?~ ".")

    ;; '.' character might be better as punctuation, as in C, but this way you
    ;; can treat table index as symbol, e.g. `io.string'
    (modify-syntax-entry ?. "_")
    (syntax-table))
  "`lua-mode' syntax table.")

;;;###autoload
(define-derived-mode lua-mode lua--prog-mode "Lua"
  "Major mode for editing Lua code."
  :abbrev-table lua-mode-abbrev-table
  :syntax-table lua-mode-syntax-table
  :group 'lua

  (setq comint-prompt-regexp lua-prompt-regexp)
  (make-local-variable 'lua-default-command-switches)
  (set (make-local-variable 'font-lock-defaults)
       `(lua-font-lock-keywords ;; keywords
         nil                    ;; keywords-only
         nil                    ;; case-fold
         ;; Not sure, why '_' is a word constituent only when font-locking.
         ;; --immerrr
         ((?_ . "w")) ;; syntax-alist
         nil          ;; syntax-begin
         ;; initialize font-lock buffer-local variables
         (font-lock-syntactic-keywords  . lua-font-lock-syntactic-keywords)
         (font-lock-extra-managed-props . (syntax-table))
         (parse-sexp-lookup-properties  . t)
         ;; initialize the rest of buffer-local variables
         (beginning-of-defun-function   . lua-beginning-of-proc)
         (end-of-defun-function         . lua-end-of-proc)
         (indent-line-function          . lua-indent-line)
         (comment-start                 . ,lua-comment-start)
         (comment-start-skip            . ,lua-comment-start-skip)
         (imenu-generic-expression      . ,lua-imenu-generic-expression)))

  ;; setup menu bar entry (XEmacs style)
  (if (and (featurep 'menubar)
           (boundp 'current-menubar)
           (fboundp 'set-buffer-menubar)
           (fboundp 'add-menu)
           (not (assoc "Lua" current-menubar)))
      (progn
        (set-buffer-menubar (copy-sequence current-menubar))
        (add-menu nil "Lua" lua-emacs-menu)))
  ;; Append Lua menu to popup menu for Emacs.
  (if (boundp 'mode-popup-menu)
      (setq mode-popup-menu
            (cons (concat mode-name " Mode Commands") lua-emacs-menu)))

  ;; hideshow setup
  (unless (assq 'lua-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 `(lua-mode
                   ,(regexp-opt (mapcar 'car lua-sexp-alist) 'words) ;start
                   ,(regexp-opt (mapcar 'cdr lua-sexp-alist) 'words) ;end
                   nil lua-forward-sexp))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defun lua-electric-match (arg)
  "Insert character and adjust indentation."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if lua-electric-flag
      (lua-indent-line))
  (blink-matching-open))

;; private functions

(defun lua-prefix-key-update-bindings ()
  (let (old-cons)
    (if (eq lua-prefix-mode-map (keymap-parent lua-mode-map))
        ;; if prefix-map is a parent, delete the parent
        (set-keymap-parent lua-mode-map nil)
      ;; otherwise, look for it among children
      (if (setq old-cons (rassoc lua-prefix-mode-map lua-mode-map))
          (delq old-cons lua-mode-map)))

    (if (null lua-prefix-key)
        (set-keymap-parent lua-mode-map lua-prefix-mode-map)
      (define-key lua-mode-map (vector lua-prefix-key) lua-prefix-mode-map))))

(defun lua-set-prefix-key (new-key-str)
  "Changes `lua-prefix-key' properly and updates keymaps

This function replaces previous prefix-key binding with a new one."
  (interactive "sNew prefix key (empty string means no key): ")
  (lua--customize-set-prefix-key 'lua-prefix-key new-key-str)
  (message "Prefix key set to %S"  (single-key-description lua-prefix-key))
  (lua-prefix-key-update-bindings))

(defun lua-string-p (&optional pos)
  "Returns true if the point is in a string."
  (save-excursion (elt (syntax-ppss pos) 3)))

(defun lua-comment-start-pos (parsing-state)
  "Return position of comment containing current point.

If point is not inside a comment, return nil."
  (and parsing-state (nth 4 parsing-state) (nth 8 parsing-state)))

(defun lua-comment-p (&optional pos)
  "Returns true if the point is in a comment."
  (save-excursion (elt (syntax-ppss pos) 4)))

(defun lua-comment-or-string-p (&optional pos)
  "Returns true if the point is in a comment or string."
  (save-excursion (let ((parse-result (syntax-ppss pos)))
                    (or (elt parse-result 3) (elt parse-result 4)))))

(defun lua-comment-or-string-start-pos (&optional pos)
  "Returns start position of string or comment which contains point.

If point is not inside string or comment, return nil."
  (save-excursion (elt (syntax-ppss pos) 8)))

;; They're propertized as follows:
;; 1. generic-comment
;; 2. generic-string
;; 3. equals signs
(defconst lua-ml-begin-regexp
  "\\(?:\\(?1:-\\)-\\[\\|\\(?2:\\[\\)\\)\\(?3:=*\\)\\[")


(defun lua-try-match-multiline-end (end)
  "Try to match close-bracket for multiline literal around point.

Basically, detect form of close bracket from syntactic
information provided at point and re-search-forward to it."
  (let ((comment-or-string-start-pos (lua-comment-or-string-start-pos)))
    ;; Is there a literal around point?
    (and comment-or-string-start-pos
         ;; It is, check if the literal is a multiline open-bracket
         (save-excursion
           (goto-char comment-or-string-start-pos)
           (looking-at lua-ml-begin-regexp))

         ;; Yes it is, look for it matching close-bracket.  Close-bracket's
         ;; match group is determined by match-group of open-bracket.
         (re-search-forward
          (format "]%s\\(?%s:]\\)"
                  (match-string-no-properties 3)
                  (if (match-beginning 1) 1 2))
          end 'noerror))))


(defun lua-try-match-multiline-begin (limit)
  "Try to match multiline open-brackets.

Find next opening long bracket outside of any string/comment.
If none can be found before reaching LIMIT, return nil."

  (let (last-search-matched)
    (while
        ;; This loop will iterate skipping all multiline-begin tokens that are
        ;; inside strings or comments ending either at EOL or at valid token.
        (and (setq last-search-matched
                   (re-search-forward lua-ml-begin-regexp limit 'noerror))

             ;; (1+ (match-beginning 0)) is required to handle triple-hyphen
             ;; '---[[' situation: regexp matches starting from the second one,
             ;; but it's not yet a comment, because it's a part of 2-character
             ;; comment-start sequence, so if we try to detect if the opener is
             ;; inside a comment from the second hyphen, it'll fail.  But the
             ;; third one _is_ inside a comment and considering it instead will
             ;; fix the issue. --immerrr
             (lua-comment-or-string-start-pos (1+ (match-beginning 0)))))

    last-search-matched))

(defun lua-match-multiline-literal-bounds (limit)
  ;; First, close any multiline literal spanning from previous block. This will
  ;; move the point accordingly so as to avoid double traversal.
  (or (lua-try-match-multiline-end limit)
      (lua-try-match-multiline-begin limit)))

(defvar lua-font-lock-syntactic-keywords
  '((lua-match-multiline-literal-bounds
     (1 "!" nil noerror)
     (2 "|" nil noerror))))

(defun lua-indent-line ()
  "Indent current line for Lua mode.
Return the amount the indentation changed by."
  (let (indent
        (case-fold-search nil)
        ;; save point as a distance to eob - it's invariant w.r.t indentation
        (pos (- (point-max) (point))))
    (back-to-indentation)
    (if (lua-comment-or-string-p)
        (setq indent (lua-calculate-string-or-comment-indentation)) ;; just restore point position
      (setq indent (max 0 (lua-calculate-indentation nil))))

    (when (not (equal indent (current-column)))
      (delete-region (line-beginning-position) (point))
      (indent-to indent))

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    indent))

(defun lua-calculate-string-or-comment-indentation ()
  "This function should be run when point at (current-indentation) is inside string"
  (if (and (lua-string-p) (not lua-indent-string-contents))
      ;; if inside string and strings aren't to be indented, return current indentation
      (current-indentation)

    ;; At this point, we know that we're inside comment, so make sure
    ;; close-bracket is unindented like a block that starts after
    ;; left-shifter.
    (let ((left-shifter-p (looking-at "\\s *\\(?:--\\)?\\]\\(?1:=*\\)\\]")))
      (save-excursion
        (goto-char (lua-comment-or-string-start-pos))
        (+ (current-indentation)
           (if (and left-shifter-p
                    (looking-at (format "--\\[%s\\["
                                        (match-string-no-properties 1))))
               0
             lua-indent-level))))))

(defun lua-find-regexp (direction regexp &optional limit ignore-p)
  "Searches for a regular expression in the direction specified.
Direction is one of 'forward and 'backward.
By default, matches in comments and strings are ignored, but what to ignore is
configurable by specifying ignore-p. If the regexp is found, returns point
position, nil otherwise.
ignore-p returns true if the match at the current point position should be
ignored, nil otherwise."
  (let ((ignore-func (or ignore-p 'lua-comment-or-string-p))
        (search-func (if (eq direction 'forward)
                         're-search-forward 're-search-backward))
        (case-fold-search nil))
    (catch 'found
      (while (funcall search-func regexp limit t)
        (if (and (not (funcall ignore-func (match-beginning 0)))
                 (not (funcall ignore-func (match-end 0))))
            (throw 'found (point)))))))

(defconst lua-block-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "function" "repeat" "then"
                   "else" "elseif" "end" "until") t)
     "\\_>\\)\\|"
     (regexp-opt '("{" "(" "[" "]" ")" "}") t))))

(defconst lua-block-token-alist
  '(("do"       "\\<end\\>"   "\\<for\\|while\\>"                       middle-or-open)
    ("function" "\\<end\\>"   nil                                       open)
    ("repeat"   "\\<until\\>" nil                                       open)
    ("then"     "\\<\\(e\\(lse\\(if\\)?\\|nd\\)\\)\\>" "\\<\\(else\\)?if\\>" middle)
    ("{"        "}"           nil                                       open)
    ("["        "]"           nil                                       open)
    ("("        ")"           nil                                       open)
    ("if"       "\\<then\\>"  nil                                       open)
    ("for"      "\\<do\\>"    nil                                       open)
    ("while"    "\\<do\\>"    nil                                       open)
    ("else"     "\\<end\\>"   "\\<then\\>"                              middle)
    ("elseif"   "\\<then\\>"  "\\<then\\>"                              middle)
    ("end"      nil           "\\<\\(do\\|function\\|then\\|else\\)\\>" close)
    ("until"    nil           "\\<repeat\\>"                            close)
    ("}"        nil           "{"                                       close)
    ("]"        nil           "\\["                                     close)
    (")"        nil           "("                                       close))
  "This is a list of block token information blocks.
Each token information entry is of the form:
  KEYWORD FORWARD-MATCH-REGEXP BACKWARDS-MATCH-REGEXP TOKEN-TYPE
KEYWORD is the token.
FORWARD-MATCH-REGEXP is a regexp that matches all possble tokens when going forward.
BACKWARDS-MATCH-REGEXP is a regexp that matches all possble tokens when going backwards.
TOKEN-TYPE determines where the token occurs on a statement. open indicates that the token appears at start, close indicates that it appears at end, middle indicates that it is a middle type token, and middle-or-open indicates that it can appear both as a middle or an open type.")

(defconst lua-indentation-modifier-regexp
  ;; The absence of else is deliberate, since it does not modify the
  ;; indentation level per se. It only may cause the line, in which the
  ;; else is, to be shifted to the left.
  (concat
   "\\(\\_<"
   (regexp-opt '("do" "function" "repeat" "then" "if" "else" "elseif" "for" "while") t)
   "\\_>\\|"
   (regexp-opt '("{" "(" "["))
   "\\)\\|\\(\\_<"
   (regexp-opt '("end" "until") t)
   "\\_>\\|"
   (regexp-opt '("]" ")" "}"))
   "\\)")
  )

(defun lua-get-block-token-info (token)
  "Returns the block token info entry for TOKEN from lua-block-token-alist"
  (assoc token lua-block-token-alist))

(defun lua-get-token-match-re (token-info direction)
  "Returns the relevant match regexp from token info"
  (cond
   ((eq direction 'forward) (cadr token-info))
   ((eq direction 'backward) (caddr token-info))
   (t nil)))

(defun lua-get-token-type (token-info)
  "Returns the relevant match regexp from token info"
   (cadddr token-info))

(defun lua-backwards-to-block-begin-or-end ()
  "Move backwards to nearest block begin or end.  Returns nil if not successful."
  (interactive)
  (lua-find-regexp 'backward lua-block-regexp))

(defun lua-find-matching-token-word (token &optional direction)
  (let* ((token-info (lua-get-block-token-info token))
         (match-type (lua-get-token-type token-info))
         ;; If we are on a middle token, go backwards. If it is a middle or open,
         ;; go forwards
         (search-direction (or direction
                               (if (or (eq match-type 'open)
                                       (eq match-type 'middle-or-open))
                                   'forward
                                 'backward)
                               'backward))
         (match (lua-get-token-match-re token-info search-direction))
         maybe-found-pos)
    ;; if we are searching forward from the token at the current point
    ;; (i.e. for a closing token), need to step one character forward
    ;; first, or the regexp will match the opening token.
    (if (eq search-direction 'forward) (forward-char 1))
    (catch 'found
      ;; If we are attempting to find a matching token for a terminating token
      ;; (i.e. a token that starts a statement when searching back, or a token
      ;; that ends a statement when searching forward), then we don't need to look
      ;; any further.
      (if (or (and (eq search-direction 'forward)
                   (eq match-type 'close))
              (and (eq search-direction 'backward)
                   (eq match-type 'open)))
          (throw 'found nil))
      (while (lua-find-regexp search-direction lua-indentation-modifier-regexp)
        ;; have we found a valid matching token?
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (let ((found-type (lua-get-token-type
                             (lua-get-block-token-info found-token))))
            (if (not (and match (string-match match found-token)))
                ;; no - then there is a nested block. If we were looking for
                ;; a block begin token, found-token must be a block end
                ;; token; likewise, if we were looking for a block end token,
                ;; found-token must be a block begin token, otherwise there
                ;; is a grammatical error in the code.
                (if (not (and
                          (or (eq match-type 'middle)
                              (eq found-type 'middle)
                              (eq match-type 'middle-or-open)
                              (eq found-type 'middle-or-open)
                              (eq match-type found-type))
                          (lua-find-matching-token-word found-token
                                                        search-direction)))
                    (when maybe-found-pos
                      (goto-char maybe-found-pos)
                      (throw 'found maybe-found-pos)))
              ;; yes.
              ;; if it is a not a middle kind, report the location
              (when (not (or (eq found-type 'middle)
                             (eq found-type 'middle-or-open)))
                (throw 'found found-pos))
              ;; if it is a middle-or-open type, record location, but keep searching.
              ;; If we fail to complete the search, we'll report the location
              (when (eq found-type 'middle-or-open)
                (setq maybe-found-pos found-pos))
              ;; Cannot use tail recursion. too much nesting on long chains of
              ;; if/elseif. Will reset variables instead.
              (setq token found-token)
              (setq token-info (lua-get-block-token-info token))
              (setq match (lua-get-token-match-re token-info search-direction))
              (setq match-type (lua-get-token-type token-info))))))
      maybe-found-pos)))

(defun lua-goto-matching-block-token (&optional parse-start direction)
  "Find block begion/end token matching the one at the point.
This function moves the point to the token that matches the one
at the current point. Returns the point position of the first character of
the matching token if successful, nil otherwise."
  (if parse-start (goto-char parse-start))
  (let ((case-fold-search nil))
    (if (looking-at lua-indentation-modifier-regexp)
        (let ((position (lua-find-matching-token-word (match-string 0)
                                                      direction)))
          (and position
               (goto-char position))))))

(defun lua-goto-matching-block (&optional noreport)
  "Go to the keyword balancing the one under the point.
If the point is on a keyword/brace that starts a block, go to the
matching keyword that ends the block, and vice versa."
  (interactive)
  ;; search backward to the beginning of the keyword if necessary
  (if (eq (char-syntax (following-char)) ?w)
      (re-search-backward "\\_<" nil t))
  (let ((position (lua-goto-matching-block-token)))
    (if (and (not position)
             (not noreport))
        (error "Not on a block control keyword or brace")
      position)))

(defun lua-forward-line-skip-blanks (&optional back)
  "Move 1 line forward (back if BACK is non-nil) skipping blank lines.

Moves point 1 line forward (or backward) skipping lines that contain
no Lua code besides comments. The point is put to the beginning of
the line.

Returns final value of point as integer or nil if operation failed."
  (catch 'found
    (while t
      (unless (eql (forward-line (if back -1 1)) 0)    ;; 0 means success
        (throw 'found nil))
      (unless (or (looking-at "\\s *\\(--.*\\)?$")
                  (lua-comment-or-string-p))
        (throw 'found (point))))))

(eval-when-compile
  (defconst lua-operator-class
    "-+*/^.=<>~"))

(defconst lua-cont-eol-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("and" "or" "not" "in" "for" "while"
                   "local" "function" "if" "until" "elseif" "return") t)
     "\\_>\\|"
     "\\(^\\|[^" lua-operator-class "]\\)"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\)"
     "\\s *\\=")
    )
  "Regexp that matches the ending of a line that needs continuation

This regexp starts from eol and looks for a binary operator or an unclosed
block intro (i.e. 'for' without 'do' or 'if' without 'then') followed by
an optional whitespace till the end of the line.")

(defconst lua-cont-bol-regexp
  (eval-when-compile
    (concat
     "\\=\\s *"
     "\\(\\_<"
     (regexp-opt '("and" "or" "not") t)
     "\\_>\\|"
     (regexp-opt '("+" "-" "*" "/" "^" ".." "==" "=" "<" ">" "<=" ">=" "~=") t)
     "\\($\\|[^" lua-operator-class "]\\)"
     "\\)")

    )
  "Regexp that matches a line that continues previous one

This regexp means, starting from point there is an optional whitespace followed
by Lua binary operator. Lua is very liberal when it comes to continuation line,
so we're safe to assume that every line that starts with a binop continues
previous one even though it looked like an end-of-statement.")

(defun lua-last-token-continues-p ()
  "Returns true if the last token on this line is a continuation token."
  (let ((line-begin (line-beginning-position))
        (line-end (line-end-position)))
    (save-excursion
      (end-of-line)
      ;; we need to check whether the line ends in a comment and
      ;; skip that one.
      (while (lua-find-regexp 'backward "-" line-begin 'lua-string-p)
        (if (looking-at "--")
            (setq line-end (point))))
      (goto-char line-end)
      (re-search-backward lua-cont-eol-regexp line-begin t))))

(defun lua-first-token-continues-p ()
  "Returns true if the first token on this line is a continuation token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      ;; if first character of the line is inside string, it's a continuation
      ;; if strings aren't supposed to be indented, `lua-calculate-indentation' won't even let
      ;; the control inside this function
      (re-search-forward lua-cont-bol-regexp line-end t))))

(defconst lua-block-starter-regexp
  (eval-when-compile
    (concat
     "\\(\\_<"
     (regexp-opt '("do" "while" "repeat" "until" "if" "then"
                   "else" "elseif" "end" "for" "local") t)
     "\\_>\\)")))

(defun lua-first-token-starts-block-p ()
  "Returns true if the first token on this line is a block starter token."
  (let ((line-end (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward (concat "\\s *" lua-block-starter-regexp) line-end t))))

(defun lua-is-continuing-statement-p (&optional parse-start)
  "Return non-nil if the line continues a statement.
More specifically, return the point in the line that is continued.
The criteria for a continuing statement are:

* the last token of the previous line is a continuing op,
  OR the first token of the current line is a continuing op

"
  (let ((prev-line nil))
    (save-excursion
      (if parse-start (goto-char parse-start))
      (save-excursion (setq prev-line (lua-forward-line-skip-blanks 'back)))
      (and prev-line
           (not (lua-first-token-starts-block-p))
           (or (lua-first-token-continues-p)
               (and (goto-char prev-line)
                    ;; check last token of previous nonblank line
                    (lua-last-token-continues-p)))))))

(defun lua-make-indentation-info-pair (found-token found-pos)
  "This is a helper function to lua-calculate-indentation-info. Don't
use standalone."
  (cond
   ;; function is a bit tricky to indent right. They can appear in a lot ot
   ;; different contexts. Until I find a shortcut, I'll leave it with a simple
   ;; relative indentation.
   ;; The special cases are for indenting according to the location of the
   ;; function. i.e.:
   ;;       (cons 'absolute (+ (current-column) lua-indent-level))
   ;; TODO: Fix this. It causes really ugly indentations for in-line functions.
   ((string-equal found-token "function")
    (cons 'relative lua-indent-level))

   ;; block openers
   ((member found-token (list "{" "(" "["))
	 (save-excursion
	   ;; expression follows -> indent at start of next expression
       ;; Last token on the line -> simple relative indent
	   (if (and (not (search-forward-regexp "[[:space:]]--" (line-end-position) t))
                (search-forward-regexp "[^[:space:]]" (line-end-position) t))
           (cons 'absolute (1- (current-column)))
         (cons 'relative lua-indent-level))))

   ;; These are not really block starters. They should not add to indentation.
   ;; The corresponding "then" and "do" handle the indentation.
   ((member found-token (list "if" "for" "while"))
    (cons 'relative 0))
   ;; closing tokens follow: These are usually taken care of by
   ;; lua-calculate-indentation-override.
   ;; elseif is a bit of a hack. It is not handled separately, but it needs to
   ;; nullify a previous then if on the same line.
   ((member found-token (list "until" "elseif"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (if (and (lua-goto-matching-block-token found-pos 'backward)
                 (= line (line-number-at-pos)))
            (cons 'remove-matching 0)
          (cons 'relative 0)))))

   ;; else is a special case; if its matching block token is on the same line,
   ;; instead of removing the matching token, it has to replace it, so that
   ;; either the next line will be indented correctly, or the end on the same
   ;; line will remove the effect of the else.
   ((string-equal found-token "else")
     (save-excursion
       (let ((line (line-number-at-pos)))
         (if (and (lua-goto-matching-block-token found-pos 'backward)
                  (= line (line-number-at-pos)))
             (cons 'replace-matching (cons 'relative lua-indent-level))
                   (cons 'relative lua-indent-level)))))

   ;; Block closers. If they are on the same line as their openers, they simply
   ;; eat up the matching indentation modifier. Otherwise, they pull
   ;; indentation back to the matching block opener.
   ((member found-token (list ")" "}" "]" "end"))
    (save-excursion
      (let ((line (line-number-at-pos)))
        (lua-goto-matching-block-token found-pos 'backward)
        (if (/= line (line-number-at-pos))
            (lua-calculate-indentation-info (point))
          (cons 'remove-matching 0)))))

   ;; Everything else. This is from the original code: If opening a block
   ;; (match-data 1 exists), then push indentation one level up, if it is
   ;; closing a block, pull it one level down.
   ('other-indentation-modifier
    (cons 'relative (if (nth 2 (match-data))
                        ;; beginning of a block matched
                        lua-indent-level
                      ;; end of a block matched
                      (- lua-indent-level))))))

(defun  lua-add-indentation-info-pair (pair info)
  "Add the given indentation info pair to the list of indentation information.
This function has special case handling for two tokens: remove-matching,
and replace-matching. These two tokens are cleanup tokens that remove or
alter the effect of a previously recorded indentation info.

When a remove-matching token is encountered, the last recorded info, i.e.
the car of the list is removed. This is used to roll-back an indentation of a
block opening statement when it is closed.

When a replace-matching token is seen, the last recorded info is removed,
and the cdr of the replace-matching info is added in its place. This is used
when a middle-of the block (the only case is 'else') is seen on the same line
the block is opened."
  (cond
   ( (listp (cdr-safe pair))
     (nconc pair info))
   ( (eq 'remove-matching (car pair))
     ; Remove head of list
     (cdr info))
   ( (eq 'replace-matching (car pair))
     ; remove head of list, and add the cdr of pair instead
     (cons (cdr pair) (cdr info)))
   ( t
     ; Just add the pair
     (cons pair info))))

(defun lua-calculate-indentation-info-1 (indentation-info bound)
  "Helper function for `lua-calculate-indentation-info'.

Return list of indentation modifiers from point to BOUND."
  (while (lua-find-regexp 'forward lua-indentation-modifier-regexp
                          bound)
    (let ((found-token (match-string 0))
          (found-pos (match-beginning 0))
          (found-end (match-end 0))
          (data (match-data)))
      (setq indentation-info
            (lua-add-indentation-info-pair
             (lua-make-indentation-info-pair found-token found-pos)
             indentation-info))))
  indentation-info)


(defun lua-calculate-indentation-info (&optional parse-end)
  "For each block token on the line, computes how it affects the indentation.
The effect of each token can be either a shift relative to the current
indentation level, or indentation to some absolute column. This information
is collected in a list of indentation info pairs, which denote absolute
and relative each, and the shift/column to indent to."
  (let ((combined-line-end (line-end-position))
        indentation-info)

    (while (lua-is-continuing-statement-p)
      (lua-forward-line-skip-blanks 'back))

    ;; calculate indentation modifiers for the line itself
    (setq indentation-info (list (cons 'absolute (current-indentation))))

    (back-to-indentation)
    (setq indentation-info
          (lua-calculate-indentation-info-1
           indentation-info (min parse-end (line-end-position))))

    ;; and do the following for each continuation line before PARSE-END
    (while (and (eql (forward-line 1) 0)
                (<= (point) parse-end))

      ;; handle continuation lines:
      (if (lua-is-continuing-statement-p)
          ;; if it's the first continuation line, add one level
          (unless (eq (car (car indentation-info)) 'continued-line)
            (push (cons 'continued-line lua-indent-level) indentation-info))

        ;; if it's the first non-continued line, subtract one level
        (when (eq (car (car indentation-info)) 'continued-line)
          (pop indentation-info)))

      ;; add modifiers found in this continuation line
      (setq indentation-info
            (lua-calculate-indentation-info-1
             indentation-info (min parse-end (line-end-position)))))

    indentation-info))


(defun lua-accumulate-indentation-info (info)
  "Accumulates the indentation information previously calculated by
lua-calculate-indentation-info. Returns either the relative indentation
shift, or the absolute column to indent to."
  (let ((info-list (reverse info))
        (type 'relative)
        (accu 0))
    (mapc (lambda (x)
            (setq accu (if (eq 'absolute (car x))
                           (progn (setq type 'absolute)
                                  (cdr x))
                         (+ accu (cdr x)))))
          info-list)
    (cons type accu)))

(defun lua-calculate-indentation-block-modifier (&optional parse-end)
  "Return amount by which this line modifies the indentation.
Beginnings of blocks add lua-indent-level once each, and endings
of blocks subtract lua-indent-level once each. This function is used
to determine how the indentation of the following line relates to this
one."
  (let (indentation-info)
    (save-excursion
      ;; First go back to the line that starts it all
      ;; lua-calculate-indentation-info will scan through the whole thing
      (let ((case-fold-search nil))
        (setq indentation-info
              (lua-accumulate-indentation-info
               (lua-calculate-indentation-info parse-end)))))

    (if (eq (car indentation-info) 'absolute)
        (- (cdr indentation-info) (current-indentation))
      (cdr indentation-info))))


(eval-when-compile
  (defconst lua--function-name-rx
    '(seq symbol-start
          (+ (any alnum "_"))
          (* "." (+ (any alnum "_")))
          (? ":" (+ (any alnum "_")))
          symbol-end)
    "Lua function name regexp in `rx'-SEXP format."))


(defconst lua--left-shifter-regexp
  (eval-when-compile
    (rx
     ;; This regexp should answer the following questions:
     ;; 1. is there a left shifter regexp on that line?
     ;; 2. where does block-open token of that left shifter reside?
     ;;
     ;; NOTE: couldn't use `group-n' keyword of `rx' macro, because it was
     ;; introduced in Emacs 24.2 only, so for the sake of code clarity the named
     ;; groups don't really match anything, they just report the position of the
     ;; match.
     (or (seq (regexp "\\_<local[ \t]+") (regexp "\\(?1:\\)function\\_>"))
         (seq (eval lua--function-name-rx) (* blank) (regexp "\\(?1:\\)[{(]"))
         (seq (or
               ;; assignment statement prefix
               (seq (* nonl) (not (any "<=>~")) "=" (* blank))
               ;; return statement prefix
               (seq word-start "return" word-end (* blank)))
              (regexp "\\(?1:\\)")
              ;; right hand side
              (or "{"
                  "function"
                  (seq
                   (eval lua--function-name-rx) (* blank)
                   (regexp "\\(?1:\\)") (any "({")))))))

  "Regular expression that matches left-shifter expression.

Left-shifter expression is defined as follows.  If a block
follows a left-shifter expression, its contents & block-close
token should be indented relative to left-shifter expression
indentation rather then to block-open token.

For example:
   -- 'local a = ' is a left-shifter expression
   -- 'function' is a block-open token
   local a = function()
      -- block contents is indented relative to left-shifter
      foobarbaz()
   -- block-end token is unindented to left-shifter indentation
   end

The following left-shifter expressions are currently handled:
1. local function definition with function block, begin-end
2. function call with arguments block, () or {}
3. assignment/return statement with
   - table constructor block, {}
   - function call arguments block, () or {} block
   - function expression a.k.a. lambda, begin-end block.")


(defun lua-point-is-after-left-shifter-p ()
  "Check if point is right after a left-shifter expression.

See `lua--left-shifter-regexp' for description & example of
left-shifter expression. "
  (save-excursion
    (let ((old-point (point)))
      (back-to-indentation)
      (and
       (/= (point) old-point)
       (looking-at lua--left-shifter-regexp)
       (= old-point (match-end 1))))))



(defun lua-calculate-indentation-override (&optional parse-start)
  "Return overriding indentation amount for special cases.

If there's a sequence of block-close tokens starting at the
beginning of the line, calculate indentation according to the
line containing block-open token for the last block-close token
in the sequence.

If not, return nil."
  (let (case-fold-search token-info block-token-pos)
    (save-excursion
      (if parse-start (goto-char parse-start))

      (back-to-indentation)
      (unless (lua-comment-or-string-p)
        (while
            (and (looking-at lua-indentation-modifier-regexp)
                 (setq token-info (lua-get-block-token-info (match-string 0)))
                 (not (eq 'open (lua-get-token-type token-info))))
          (setq block-token-pos (match-beginning 0))
          (goto-char (match-end 0))
          (skip-syntax-forward " " (line-end-position)))

        (when (lua-goto-matching-block-token block-token-pos 'backward)
          ;; Exception cases: when the start of the line is an assignment,
          ;; go to the start of the assignment instead of the matching item
          (if (lua-point-is-after-left-shifter-p)
              (current-indentation)
            (current-column)))))))

(defun lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code."
  (save-excursion
    (let ((continuing-p (lua-is-continuing-statement-p))
          (cur-line-begin-pos (line-beginning-position)))
      (or
       ;; when calculating indentation, do the following:
       ;; 1. check, if the line starts with indentation-modifier (open/close brace)
       ;;    and if it should be indented/unindented in special way
       (lua-calculate-indentation-override)

       (when (lua-forward-line-skip-blanks 'back)
         ;; the order of function calls here is important. block modifier
         ;; call may change the point to another line
         (let* ((modifier
                 (lua-calculate-indentation-block-modifier cur-line-begin-pos)))
           (+ (current-indentation) modifier)))

       ;; 4. if there's no previous line, indentation is 0
       0))))

(defun lua-beginning-of-proc (&optional arg)
  "Move backward to the beginning of a lua proc (or similar).

With argument, do it that many times.  Negative arg -N
means move forward to Nth following beginning of proc.

Returns t unless search stops due to beginning or end of buffer."
  (interactive "P")
  (or arg (setq arg 1))

  (while (and (> arg 0)
              (re-search-backward "^function[ \t]" nil t))
    (setq arg (1- arg)))

  (while (and (< arg 0)
              (re-search-forward "^function[ \t]" nil t))
    (beginning-of-line)
    (setq arg (1+ arg)))

  (zerop arg))

(defun lua-end-of-proc (&optional arg)
  "Move forward to next end of lua proc (or similar).
With argument, do it that many times.  Negative argument -N means move
back to Nth preceding end of proc.

This function just searches for a `end' at the beginning of a line."
  (interactive "P")
  (or arg
      (setq arg 1))
  (let ((found nil)
        (ret t))
    (if (and (< arg 0)
             (not (bolp))
             (save-excursion
               (beginning-of-line)
               (eq (following-char) ?})))
        (forward-char -1))
    (while (> arg 0)
      (if (re-search-forward "^end" nil t)
          (setq arg (1- arg)
                found t)
        (setq ret nil
              arg 0)))
    (while (< arg 0)
      (if (re-search-backward "^end" nil t)
          (setq arg (1+ arg)
                found t)
        (setq ret nil
              arg 0)))
    (if found
        (progn
          (beginning-of-line)
          (forward-line)))
    ret))

(defun lua-start-process (&optional name program startfile &rest switches)
  "Start a lua process named NAME, running PROGRAM.
PROGRAM defaults to NAME, which defaults to `lua-default-application'.
When called interactively, switch to the process buffer."
  (interactive)
  (or switches
      (setq switches lua-default-command-switches))
  (setq name (or name lua-default-application))
  (setq program (or program name))
  (setq lua-process-buffer (apply 'make-comint name program startfile switches))
  (setq lua-process (get-buffer-process lua-process-buffer))
  ;; wait for prompt
  (with-current-buffer lua-process-buffer
    (while (not (lua-prompt-line))
      (accept-process-output (get-buffer-process (current-buffer)))
      (goto-char (point-max))))
  ;; when called interactively, switch to process buffer
  (if (lua--called-interactively-p 'any)
      (switch-to-buffer lua-process-buffer)))

(defun lua-kill-process ()
  "Kill lua subprocess and its buffer."
  (interactive)
  (if lua-process-buffer
      (kill-buffer lua-process-buffer)))

(defun lua-set-lua-region-start (&optional arg)
  "Set start of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-start (or arg (point))))

(defun lua-set-lua-region-end (&optional arg)
  "Set end of region for use with `lua-send-lua-region'."
  (interactive)
  (set-marker lua-region-end (or arg (point))))

(defun lua-send-current-line ()
  "Send current line to lua subprocess, found in `lua-process'.
If `lua-process' is nil or dead, start a new process first."
  (interactive)
  (lua-send-region (line-beginning-position) (line-end-position)))

(defun lua-send-defun (pos)
  "Send the function definition around point to lua subprocess."
  (interactive "d")
  (save-excursion
    (let ((start (if (save-match-data (looking-at "^function[ \t]"))
                     ;; point already at the start of "function".
                     ;; We need to handle this case explicitly since
                     ;; lua-beginning-of-proc will move to the
                     ;; beginning of the _previous_ function.
                     (point)
                   ;; point is not at the beginning of function, move
                   ;; there and bind start to that position
                   (lua-beginning-of-proc)
                   (point)))
          (end (progn (lua-end-of-proc) (point))))

      ;; make sure point is in a function defintion before sending to
      ;; the subprocess
      (if (and (>= pos start) (< pos end))
          (lua-send-region start end)
        (error "Not on a function definition")))))

(defun lua-send-region (start end)
  "Send region to lua subprocess."
  (interactive "r")
  ;; make temporary lua file
  (let ((tempfile (lua-make-temp-file "lua-"))
        (last-prompt nil)
        (prompt-found nil)
        (lua-stdin-line-offset (count-lines (point-min) start))
        (lua-stdin-buffer (current-buffer))
        current-prompt )
    (write-region start end tempfile)
    (or (and lua-process
             (comint-check-proc lua-process-buffer))
        (lua-start-process lua-default-application))
    ;; kill lua process without query
    (if (fboundp 'process-kill-without-query)
        (process-kill-without-query lua-process))
    ;; send dofile(tempfile)
    (with-current-buffer lua-process-buffer
      (goto-char (point-max))
      (setq last-prompt (point-max))
      (comint-simple-send (get-buffer-process (current-buffer))
                          (format "dofile(\"%s\")"
                                  (replace-regexp-in-string "\\\\" "\\\\\\\\" tempfile)))
      ;; wait for prompt
      (while (not prompt-found)
        (accept-process-output (get-buffer-process (current-buffer)))
        (goto-char (point-max))
        (setq prompt-found (and (lua-prompt-line) (< last-prompt (point-max)))))
      ;; remove temp. lua file
      (delete-file tempfile)
      (lua-postprocess-output-buffer lua-process-buffer last-prompt lua-stdin-line-offset)
      (if lua-always-show
          (display-buffer lua-process-buffer)))))

(defun lua-postprocess-output-buffer (buf start &optional lua-stdin-line-offset)
  "Highlight tracebacks found in buf. If an traceback occurred return
t, otherwise return nil.  BUF must exist."
  (let ((lua-stdin-line-offset (or lua-stdin-line-offset 0))
        line file bol err-p)
    (with-current-buffer buf
      (goto-char start)
      (beginning-of-line)
      (if (re-search-forward lua-traceback-line-re nil t)
          (setq file (match-string 1)
                line (string-to-number (match-string 2)))))
    (when (and lua-jump-on-traceback line)
      (beep)
      ;; FIXME: highlight
      (lua-jump-to-traceback file line lua-stdin-line-offset)
      (setq err-p t))
    err-p))

(defun lua-jump-to-traceback (file line lua-stdin-line-offset)
  "Jump to the Lua code in FILE at LINE."
  ;; sanity check: temporary-file-directory
  (if (string= (substring file 0 3)  "...")
      (message "Lua traceback output truncated: customize 'temporary-file-directory' or increase 'LUA_IDSIZE' in 'luaconf.h'.")
    (let ((buffer (cond ((or (string-equal file tempfile) (string-equal file "stdin"))
                         (setq line (+ line lua-stdin-line-offset))
                         lua-stdin-buffer)
                        (t (find-file-noselect file)))))
      (pop-to-buffer buffer)
      ;; Force Lua mode
      (if (not (eq major-mode 'lua-mode))
          (lua-mode))
      ;; FIXME: fix offset when executing region
      (goto-char (point-min)) (forward-line (1- line))
      (message "Jumping to error in file %s on line %d" file line))))

(defun lua-prompt-line ()
  (save-excursion
    (save-match-data
      (forward-line 0)
      (if (looking-at comint-prompt-regexp)
          (match-end 0)))))

(defun lua-send-lua-region ()
  "Send preset lua region to lua subprocess."
  (interactive)
  (or (and lua-region-start lua-region-end)
      (error "lua-region not set"))
  (or (and lua-process
           (comint-check-proc lua-process-buffer))
      (lua-start-process lua-default-application))
  (comint-simple-send lua-process
                      (buffer-substring lua-region-start lua-region-end)
                      )
  (if lua-always-show
      (display-buffer lua-process-buffer)))

(defalias 'lua-send-proc 'lua-send-defun)

;; FIXME: This needs work... -Bret
(defun lua-send-buffer ()
  "Send whole buffer to lua subprocess."
  (interactive)
  (lua-send-region (point-min) (point-max)))

(defun lua-restart-with-whole-file ()
  "Restart lua subprocess and send whole file as input."
  (interactive)
  (lua-kill-process)
  (lua-start-process lua-default-application)
  (lua-send-buffer))

(defun lua-show-process-buffer ()
  "Make sure `lua-process-buffer' is being displayed."
  (interactive)
  (display-buffer lua-process-buffer))

(defun lua-hide-process-buffer ()
  "Delete all windows that display `lua-process-buffer'."
  (interactive)
  (delete-windows-on lua-process-buffer))

(defun lua-search-documentation ()
  "Search Lua documentation for the word at the point."
  (interactive)
  (browse-url (concat lua-search-url-prefix (current-word t))))

(defun lua-toggle-electric-state (&optional arg)
  "Toggle the electric indentation feature.
Optional numeric ARG, if supplied, turns on electric indentation when
positive, turns it off when negative, and just toggles it when zero or
left out."
  (interactive "P")
  (let ((num_arg (prefix-numeric-value arg)))
    (setq lua-electric-flag (cond ((or (null arg)
                                       (zerop num_arg)) (not lua-electric-flag))
                                  ((< num_arg 0) nil)
                                  ((> num_arg 0) t))))
  (message "%S" lua-electric-flag))

(defun lua-forward-sexp (&optional count)
  "Forward to block end"
  (interactive "p")
  (save-match-data
    (let* ((count (or count 1))
           (block-start (mapcar 'car lua-sexp-alist))
           (block-end (mapcar 'cdr lua-sexp-alist))
           (block-regex (regexp-opt (append  block-start block-end) 'words))
           current-exp
           )
      (while (> count 0)
        ;; skip whitespace
        (skip-chars-forward " \t\n")
        (if (looking-at (regexp-opt block-start 'words))
            (let ((keyword (match-string 1)))
              (lua-find-matching-token-word keyword nil))
          ;; If the current keyword is not a "begin" keyword, then just
          ;; perform the normal forward-sexp.
          (forward-sexp 1))
        (setq count (1- count))))))


;; menu bar

(define-key lua-mode-menu [restart-with-whole-file]
  '("Restart With Whole File" .  lua-restart-with-whole-file))
(define-key lua-mode-menu [kill-process]
  '("Kill Process" . lua-kill-process))

(define-key lua-mode-menu [hide-process-buffer]
  '("Hide Process Buffer" . lua-hide-process-buffer))
(define-key lua-mode-menu [show-process-buffer]
  '("Show Process Buffer" . lua-show-process-buffer))

(define-key lua-mode-menu [end-of-proc]
  '("End Of Proc" . lua-end-of-proc))
(define-key lua-mode-menu [beginning-of-proc]
  '("Beginning Of Proc" . lua-beginning-of-proc))

(define-key lua-mode-menu [send-lua-region]
  '("Send Lua-Region" . lua-send-lua-region))
(define-key lua-mode-menu [set-lua-region-end]
  '("Set Lua-Region End" . lua-set-lua-region-end))
(define-key lua-mode-menu [set-lua-region-start]
  '("Set Lua-Region Start" . lua-set-lua-region-start))

(define-key lua-mode-menu [send-current-line]
  '("Send Current Line" . lua-send-current-line))
(define-key lua-mode-menu [send-region]
  '("Send Region" . lua-send-region))
(define-key lua-mode-menu [send-proc]
  '("Send Proc" . lua-send-proc))
(define-key lua-mode-menu [send-buffer]
  '("Send Buffer" . lua-send-buffer))
(define-key lua-mode-menu [search-documentation]
  '("Search Documentation" . lua-search-documentation))


(provide 'lua-mode)

;;; lua-mode.el ends here
