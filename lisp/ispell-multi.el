;; ispell-multi.el -- multiple ispell processes and multiple flyspell languages
;;
;; Copyright (C) 2005 P J Heslin
;;
;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/Software/Emacs
;; Version: 1.2
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; If you do not have a copy of the GNU General Public License, you
;; can obtain one by writing to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Overview:
;;
;; ispell-multi.el enables Emacs to keep a number of ispell processes
;; alive in order to spell-check text efficiently in multiple
;; languages, and it provides a hook that tells flyspell to switch
;; languages depending on the value of a particular text property.
;;
;; Normally, ispell.el only ever keeps one ispell/aspell process
;; alive.  So if you have one buffer in which an English local
;; dictionary is used and another in which a German dictionary is
;; used, the ispell process will be killed and restarted every time
;; you run ispell in the other buffer.  This is not really a problem,
;; since doing a spellcheck is infrequent and slow anyway.  If you are
;; using flyspell-mode, however, the ispell process will be restarted
;; every time you switch buffers.  Even this may not matter too much
;; to many people, since switching buffers is also a somewhat slow
;; operation.
;;
;; Where the need for multiple ispell processes becomes really acute
;; is in buffers that have multiple languages in them and a way of
;; telling flyspell to switch local dictionaries depending on where
;; point is.  In this case, the starting and stopping of ispell
;; processes very visibly impedes the fluid movement of the cursor.
;;
;; I have written two packages that provide this sort of behavior.
;; One is flyspell-xml-lang.el, which tells flyspell what the local
;; language is in xml files depending on xml:lang attributes, and
;; another is flyspell-babel.el, which does the same with Babel
;; commands in LaTeX files.
;;
;; The present package modifies ispell.el (via defadvice) so that
;; multiple ispell processes are kept alive to check different
;; languages.  It requires version 3.6 of ispell, so users of Emacs
;; 21.3 and earlier will have to upgrade.  This has only been tested
;; with GNU Emacs.
;;
;; To install this package, just put it somewhere in your load-path
;; and put a (require 'ispell-multi) statement in your .emacs file.

;;; Using ispell-multi
;;
;; If all you want to do is to change the behavior of ispell so that
;; it uses multiple ispell processes for different buffers, then
;; (require 'ispell-multi) is all you need to do.  The rest of this
;; section is for those who want to modify flyspell to switch
;; languages within a buffer.  See flyspell-xml-lang.el and
;; flyspell-babel.el for examples of the usage of all of the
;; facilities described below.
;;
;; Flyspell-mode provides a hook that runs before checking each word;
;; this allows you to change the value of ispell-local-dictionary to a
;; different language, depending on the context.  If you have a
;; package that parses a buffer and figures out what languages are in
;; it and where they are, you can tell flyspell about it by setting
;; the text property `ispell-multi-lang' to the correct ispell
;; language (this can be any value that ispell-change-dictionary
;; accepts).  Your package should set the value of the buffer-local
;; variable `flyspell-generic-check-word-predicate' to the symbol
;; `ispell-multi-verify'; do this *after* you have turned on
;; flyspell-mode.

;; If you the set the buffer-local variable
;; `ispell-multi-nil-callback' to a symbol, the associated
;; function will be called every time flyspell is called to check a
;; word, and the `ispell-multi-lang' text property returns nil.  This
;; function can be used to parse the buffer incrementally and set the
;; text-property lazily as the user moves through the buffer.
;;
;; Since the parser callback is only invoked when the text-property is
;; nil, there is a possibility that the already-set text-property and
;; the changed contents of the buffer will get out of sync.  To fix
;; this you will probably also want to arrange for the parser to be
;; run by an idle-timer.  You can arrange for this by calling the
;; function `ispell-multi-idler-setup' with a single argument, giving
;; the delay.  This will run the function specified by
;; `ispell-multi-idler-callback' after the specified idle time.
;;
;; If you want to indicate that some text should not be spell-checked,
;; set the `ispell-multi-lang' text property to the string "void".  To
;; indicate a reversion to the default ispell dictionary, use the
;; string "default".

;;; Aspell vs. Ispell
;;
;; ispell.el and ispell-multi.el will work happily with aspell instead
;; of ispell, since the former can emulate the latter.  Aspell has
;; many advantages over ispell, including a very large selection of
;; language dictionaries, and it is much better able to suggest the
;; correct spelling (which is quite handy when using
;; flyspell-auto-correct-previous-word).  If you prefer to use aspell,
;; you can put the following into your .emacs:
;;     
;;    (setq ispell-program-name "aspell")
;;    (setq ispell-really-aspell t)
;;    (setq ispell-extra-args '("--sug-mode=fast"))
;;
;; The first two lines tell ispell.el to run aspell instead of ispell,
;; and the third line tells aspell not to use its default algorithm
;; for suggesting spellings, but to use a faster one; the default is
;; very accurate, but can be a bit slow for use with flyspell.  If
;; this is not fast enough, try "ultra" instead of "fast" (but even
;; ultra mode is still two times slower than ispell).
;;
;; If you are installing a new aspell dictionary that ispell.el does
;; not know about, you will have to add it to
;; ispell-local-dictionary-alist; see the documentation of
;; flyspell-xml-lang.el for an example.

;;; Bugs and Limitations
;;
;; If you change ispell dictionaries by using the function
;; `ispell-change-dictionary', then an ispell process will be killed,
;; where it would not have been if you had simply set
;; ispell-local-dictionary.  That's because this package just modifies
;; the way ispell deals with local variables like
;; ispell-local-dictionary; it doesn't touch the
;; ispell-change-dictionary function.  Maybe it should.  The necessary
;; ispell process will be re-started next time you need it, so this is
;; not really a bug so much as a slight performance issue.
;;
;; We try to share ispell processes between buffers, so that a single
;; process can service all buffers or regions in a given language.
;; But if you put buffer-local variables that modify the behavior of
;; ispell for a given buffer (such as LocalWords), then that buffer's
;; ispell processes will not be shared.
;;
;; flyspell-large-region, which is the fast mode of flyspell that it
;; uses when checking the entirety of a large buffer, does not work at
;; all, since it depends on launching a single ispell process for this
;; purpose and so cannot cope with multiple languages.  For this
;; reason, flyspell-large-region should be disabled in buffers using
;; this package.
;;
;; It might have been nice to put in here the code to inspect a
;; text-property to find out the language of the text, so that ispell
;; (as opposed to flyspell) would obey the property and change
;; dictionary accordingly.  This won't work, though, since
;; ispell-region works on a line-by-line basis, which would fail in
;; the case of a mid-line language-switch.

;;; Changes
;;
;; 1.0 Pre-release
;; 1.1 Worked around ispell-current-dictionary / ispell-dictionary
;;     inconsistency in Emacs CVS / stand-alone ispell.el
;; 1.2 Protect better against errors when switching to an undefined or
;;     uninstalled dictionary -- errors in post-command-hook will
;;     switch off flyspell, cua, etc.
;; 1.3 Changed process management so that an ispell process without
;;     buffer-local modifications will only be killed when all buffers
;;     that have used that process have been killed.

(require 'ispell)
;; For Emacs 21.3, we have to use an updated ispell.el (3.6 or from
;; Emacs CVS), and for some reason we may have to load it again to get
;; ispell-dictionary-alist set properly.
(unless (assoc "english" ispell-dictionary-alist)
  (load "ispell"))
;; For updated ispell.el with emacs < 21.3.5
(when (not (fboundp 'set-process-query-on-exit-flag))
  (defun set-process-query-on-exit-flag (one two) ()))

; In current Emacs CVS, the variable ispell-current-dictionary is used
; to indicate the dictionary associated with the current ispell
; process, while in the current, separately distributed version of
; ispell.el (3.7beta), this variable does not exist, and
; ispell-dictionary serves this purpose.
(defvar ispell-multi-current-dictionary-var
  (if (boundp 'ispell-current-dictionary)
      'ispell-current-dictionary
    'ispell-dictionary))

(defvar ispell-multi-dict nil
  "The language that this package thinks is current in this
  buffer.  We don't set ispell-local-dictionary directly, since
  we want that to contain the default fall-back in case
  ispell-multi-dict is not set.")
(make-variable-buffer-local 'ispell-multi-dict)


(defvar ispell-multi-lang-process nil
  "Alist mapping languages to ispell processes.  Only for
  processes without any buffer-local modifications")

(defvar ispell-multi-lang-process-local nil
  "As ispell-multi-lang-process, but a buffer-local alist, to use
  for processes with buffer-local modifications")
(make-variable-buffer-local 'ispell-multi-lang-process-local)

(defvar ispell-multi-flyspell-verify-default nil
  "The original value of `flyspell-generic-check-word-predicate',
  before it was overridden in order to invoke this package; taken
  from the the `flyspell-mode-predicate' property of the major
  mode name.")
(make-variable-buffer-local 'ispell-multi-flyspell-verify-default)

(defvar ispell-multi-nil-callback nil
  "Buffer local variable that indicates a function to call when
  flyspell is checking a word and the text property
  `ispell-multi-lang' is nil.  This function will normally set
  that property at point and for some of the text in the
  neighborhood")
(make-variable-buffer-local 'ispell-multi-nil-callback)

(defvar ispell-multi-idler-callback nil
  "Buffer local variable that indicates a function to call when
  idle. Can be used to parse part or all of the buffer.")
(make-variable-buffer-local 'ispell-multi-idler-callback)

(defvar ispell-multi-verbose nil
  "If non-nil, print diagnostic messages about switching dictionaries")

(defvar ispell-multi-valid-dictionary-list nil
  "Cached value of ispell-valid-dictionary-list.")
(when (fboundp 'ispell-valid-dictionary-list)
  (setq ispell-multi-valid-dictionary-list
        (ispell-valid-dictionary-list)))

(defvar ispell-multi-bad-language nil
  "A list of languages that ispell has choked on.  After the
  first attempt, we stop trying to start ispell processes for
  this language, since flyspell will try to do so incessantly and
  cursor motion will get sluggish")

;; Reset on re-load
(setq ispell-multi-bad-language nil)

;; This is our hook into ispell.el.
(defadvice ispell-accept-buffer-local-defs (around ispell-multi-advice activate)
  "Advice that changes ispell to enable multiple ispell processes."
  ;; Bind ispell-local-dictionary
  (let* ((ispell-local-dictionary
          (if (or (equal ispell-multi-dict "void")
                  (equal ispell-multi-dict "default")
                  (null ispell-multi-dict))
              ispell-local-dictionary
            ispell-multi-dict))
         (local-mods (ispell-multi-buffer-local-modifications-p))
         (alist (if local-mods
                    'ispell-multi-lang-process-local
                  'ispell-multi-lang-process))
         (stored-process (cdr (assoc ispell-local-dictionary (symbol-value alist)))))

    ;; Store the currently running process if we haven't already
    (when (and ispell-process
               (eq (process-status ispell-process) 'run))
      (when (not (rassq ispell-process (symbol-value alist)))
        (set alist (cons (cons (symbol-value ispell-multi-current-dictionary-var) ispell-process)
                         (symbol-value alist))))
      ;; Make a note that this buffer has used this process
      (when (not (memq (current-buffer)
                       (process-get ispell-process 'ispell-multi-buffers)))
        (process-put ispell-process 'ispell-multi-buffers
                     (cons (current-buffer)
                           (process-get ispell-process 'ispell-multi-buffers)))))

    ;; Do we already have a process for this language?
    (if (and stored-process
             (eq (process-status stored-process) 'run))
        (progn
          (setq ispell-process stored-process)
          ;; When ispell-current-dictionary / ispell-dictionary is
          ;; the same as ispell-local-dictionary, ispell.el will
          ;; refrain from killing the process
          (set ispell-multi-current-dictionary-var ispell-local-dictionary))
      
      ;; This is to fool ispell into not killing the old process when
      ;; it starts the new one.  But we don't want a new process if
      ;; the current one is correct, or if the default dict is void.
      (unless (or (equal (symbol-value ispell-multi-current-dictionary-var) ispell-local-dictionary)
                  (equal ispell-local-dictionary "void"))
        (setq ispell-process nil))
      
      ;; Possibly start a new process
      (unless (equal ispell-local-dictionary "void")  ; ensure against error
        ad-do-it))))


(defun ispell-multi-kill-processes-hook ()
  "Kill orphaned ispell processes."
  (dolist (proc (process-list))
    (let* ((old-list (process-get proc 'ispell-multi-buffers))
           (new-list (delete (current-buffer) old-list)))
      (when (and old-list (not new-list)
                 (eq (process-status proc) 'run))
        (setq ispell-process proc)
        (ispell-kill-ispell))))      
  (ispell-multi-processes-alist-cleanup))

(add-hook 'kill-buffer-hook 'ispell-multi-kill-processes-hook)

(defun ispell-multi-processes-alist-cleanup ()
  "Remove any defunct processes from the global alist"
  (let ((newlist))
    (while ispell-multi-lang-process
      (when (eq (process-status (cdar ispell-multi-lang-process)) 'run)
        (setq newlist (cons (car ispell-multi-lang-process) newlist)))
      (setq ispell-multi-lang-process (cdr ispell-multi-lang-process)))
    (setq ispell-multi-lang-process (nreverse newlist))))
  
(defvar ispell-multi-local-regexp
  (mapconcat 'regexp-quote (list ispell-dictionary-keyword
                                 ispell-pdict-keyword
                                 ispell-words-keyword) "\\|"))

(defun ispell-multi-buffer-local-modifications-p ()
  (save-excursion
    (goto-char (point-max))
    (re-search-backward ispell-multi-local-regexp nil t)))

(defun ispell-multi-verify ()
  ;; Possibly initialize value of default function
  (unless ispell-multi-flyspell-verify-default
    (setq ispell-multi-flyspell-verify-default
          (or (get major-mode 'flyspell-mode-predicate)
              'none)))
  (let ((do-check t)
        ;; NB. Adding text properties via the callback will call
        ;; after-change-functions, to which flyspell adds a function
        ;; that can trigger an infinite loop: flyspell-word changes
        ;; the buffer, which can add a value to flyspell-changes, so
        ;; that list never goes to nil
        (after-change-functions nil))
    ;; Don't switch language if we're not supposed to check this bit anyway
    (when (not (eq ispell-multi-flyspell-verify-default 'none))
      (setq do-check (funcall ispell-multi-flyspell-verify-default)))
    (when do-check
      (let* ((current-position (point))
             (lang (get-text-property current-position 'ispell-multi-lang)))
        (when (and (not lang)
                   ispell-multi-nil-callback)
          (ispell-multi-message "parsing ...")
          (save-excursion
            (funcall ispell-multi-nil-callback))
          (ispell-multi-message "finished parsing.")
          (setq lang (get-text-property current-position 'ispell-multi-lang)))
        (when lang
          (unless (string= ispell-multi-dict lang)
            (let ((old-lang ispell-multi-dict))
              (cond
               ((string= lang "void")
                (setq do-check nil)
;                (ispell-multi-message "current dictionary is void: not checking")
                )
               ((and ispell-multi-valid-dictionary-list
                     (member lang ispell-multi-valid-dictionary-list)
                     (not (member lang ispell-multi-bad-language)))
                (ispell-multi-message (concat "dictionary changing to: " lang))
                (setq ispell-multi-dict lang)
                ;; Be paranoid, since this can be called from a post-command hook
                (condition-case nil
                    (ispell-accept-buffer-local-defs)
                  (error (progn
                           (ispell-multi-message
                            (concat "Error: ispell didn't like language " lang) t)
                           (setq ispell-multi-bad-language (cons lang ispell-multi-bad-language))
                           (setq do-check nil)
                           (setq ispell-multi-dict old-lang)))))
               (t
                (ispell-multi-message
                 (concat "Warning: no dictionary defined for " lang))
                (setq do-check nil))))))))
    do-check))

(defvar ispell-multi-ticker 0)
(defvar ispell-multi-old-point 0)
(make-variable-buffer-local 'ispell-multi-ticker)
(make-variable-buffer-local 'ispell-multi-old-point)

(defun ispell-multi-idler ()
  (when (and flyspell-mode
             (eq flyspell-generic-check-word-predicate 'ispell-multi-verify)
             (not (= (buffer-modified-tick) ispell-multi-ticker))
             (not (= (point) ispell-multi-old-point)))
    (let ((old-lang (get-text-property (point) 'ispell-multi-lang))
          new-lang)
      (when ispell-multi-idler-callback
        (ispell-multi-message "parsing (idle) ...")
        (let ((after-change-functions nil))
          (save-excursion
            (funcall ispell-multi-idler-callback)))
        (ispell-multi-message "finished parsing.")
        (setq new-lang (get-text-property (point) 'ispell-multi-lang))
        (unless (or (equal old-lang new-lang)
                    (equal new-lang "void"))
          (setq ispell-multi-dict new-lang)
          (ispell-accept-buffer-local-defs)))
      (setq ispell-multi-ticker (buffer-modified-tick))
      (setq ispell-multi-old-point (point)))))

(defvar ispell-multi-idle-timer nil)
(defun ispell-multi-idler-setup (delay)
  (unless ispell-multi-idle-timer
    (setq ispell-multi-idle-timer
          (run-with-idle-timer 5 t 'ispell-multi-idler))))

(defun ispell-multi-idler-cancel ()
    (cancel-timer ispell-multi-idle-timer)
    (setq ispell-multi-idle-timer nil))

(defun ispell-multi-unhack-flyspell-modeline ()
  "Remove the flyspell modeline entry"
  (setq minor-mode-alist
        (delq (assq 'flyspell-mode minor-mode-alist) minor-mode-alist)))
  
(defun ispell-multi-hack-flyspell-modeline ()
  "Add a modeline entry for flyspell that indicates the current
  language in parentheses."
  (ispell-multi-unhack-flyspell-modeline)
  (setq minor-mode-alist
        (cons '(flyspell-mode
                (:eval
                 (let ((lang (get-text-property (point) 'ispell-multi-lang)))
                   (concat flyspell-mode-line-string
                           (when lang
                             (concat " (" (capitalize lang) ")")))))) minor-mode-alist)))

(defun ispell-multi-message (mess &optional force)
  (when (or ispell-multi-verbose force)
    (message "ispell-multi -- %s" mess)))

(provide 'ispell-multi)

