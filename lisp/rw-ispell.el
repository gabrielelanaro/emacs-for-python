;;; rw-ispell.el --- additional functions for ispell.el
;;
;; Copyright (C) 2009 Ralf Wachinger
;;
;; Author: Ralf Wachinger <rwachinger@gmx.de>
;; Version: 0.1
;; Keywords: ispell
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Associating freely personal dictionaries with
;; general dictionaries for Ispell, Aspell and Hunspell,
;; creating personal dictionary files if necessary, and
;; changing general and personal dictionaries at the same time.
;;
;; Save rw-ispell.el in a convenient directory, preferably in
;; your `load-path'. Add the following to your `user-init-file':
;;
;;   (require 'rw-ispell)
;;
;; When the setup starts:
;; a) keyboard: 'M-x rw-ispell-set-up-pdicts RET'
;; b) menubar: Tools --> Spell Checking --> Set up Personal Dictionaries
;; c) automatically when the spellchecker is used the first time
;; d) `user-init-file', after setting the user options:
;;    (rw-ispell-set-up-pdicts)
;;
;; Todo:
;; Settings in `ispell-message-dictionary-alist' are not considered yet.
;; Possibly full integration in ispell.el.
;;
;;; Change Log:
;;
;; 2009-03-20 (0.1)
;;    Initial Release.
;;
;;; Code:

(require 'ispell)
(require 'easymenu)

;; User options.

(defgroup rw-ispell nil
  "Additional ispell customization options."
  :group 'ispell)

(defcustom rw-ispell-language-pdict-alist nil
  "*List used to select a new personal dictionary
according to a dictionary regexp, normally a part of the dictionary name.
It consists of pairs (REGEXP . DICTIONARY).
If the REGEXP of the last pair is an empty string,
then DICTIONARY is the default personal dictionary.
If DICTIONARY is a file name without path, user's home directory is taken.
E.g. you may use the following value:
  '((\"^en\" . \"~/.pdict_english\")
    (\"^de\" . \"~/.pdict_deutsch\")
    (\"\" . \"~/.pdict_default\"))"
  :type '(repeat (cons regexp string))
  :group 'rw-ispell)

(defcustom rw-ispell-create-pdict-files nil
  "*Create empty personal dictionary files, when they don't exist.
Needed when the spellchecker can't create the files by itself."
  :type 'boolean
  :group 'rw-ispell)

;; Internal.

(defvar rw-ispell-is-set-up nil
  "Flag if the personal dictionaries are set up.")

(defun rw-ispell-create-pdict-file (element)
  "Create an empty personal dictionary file, if it doesn't exist already."
  (let ((file (expand-file-name (cdr element) "~/")))
    (unless (file-exists-p file)
      (when rw-ispell-create-pdict-files
        (with-temp-file file t)
        (message "Created file %s" file)))))

;; User functions.

(defun rw-ispell-set-up-pdicts ()
  "Set up personal dictionaries according to `rw-ispell-language-pdict-alist'."
  (interactive)
  (ispell-check-version)
  (let ((default-pdict (or (assoc-default "" rw-ispell-language-pdict-alist)
                           (getenv "WORDLIST")
                           "~/.personal_dictionary")))
    ;; At least one personal dictionary.
    (add-to-list 'rw-ispell-language-pdict-alist
                 (cons "" default-pdict) 'append)
    ;; Absolute paths for every dictionary.
    (setq rw-ispell-language-pdict-alist
          (mapcar #'(lambda (pair)
                      (cons (car pair) (expand-file-name (cdr pair) "~/")))
                  rw-ispell-language-pdict-alist))
    ;; A file for every dictionary must exist.
    (mapc #'rw-ispell-create-pdict-file rw-ispell-language-pdict-alist)
    ;; A global dictionary must be set.
    (setq ispell-personal-dictionary
          (expand-file-name
           (assoc-default "" rw-ispell-language-pdict-alist))))
  (setq rw-ispell-is-set-up t))

(add-hook 'ispell-initialize-spellchecker-hook
          'rw-ispell-set-up-pdicts)

(defun rw-ispell-change-dictionary (dict &optional arg)
  "Change to dictionary DICT for Ispell, and to
associated personal dictionary according to `rw-ispell-language-pdict-alist'.
With a prefix arg, set it \"globally\", for all buffers.
Without a prefix arg, set it \"locally\", just for this buffer.

By just answering RET you can find out what the current dictionary is."
  (interactive
   (list (completing-read
          "Use new dictionary (RET for current, SPC to complete): "
          (and (fboundp 'ispell-valid-dictionary-list)
               (mapcar 'list (ispell-valid-dictionary-list)))
          nil t)
         current-prefix-arg))
  ;; General dictionary.
  (ispell-change-dictionary dict arg)
  ;; Personal dictionary.
  (unless (equal dict "")
    (let ((pdict (assoc-default
                  dict
                  rw-ispell-language-pdict-alist
                  #'string-match)))
      (when pdict
        (if arg
            (setq ispell-personal-dictionary pdict)
          (setq ispell-local-pdict pdict))))))

(defun rw-ispell-change-personal-dictionary (dict &optional arg)
  "Change to personal dictionary DICT for the spellchecker.
With a prefix arg, set it \"globally\", for all buffers.
Without a prefix arg, set it \"locally\", just for this buffer."
  (interactive
   (list 
    (completing-read
     "Use new personal dictionary (RET for current, SPC to complete): "
     (mapcar 'cdr rw-ispell-language-pdict-alist)
     nil t)
    current-prefix-arg))
  ;; This relies on completing-read's bug of returning "" for no match
  (cond ((equal dict "")
         (message "Using %s personal dictionary %s"
                  (if arg "global" "local")
                  (if arg ispell-personal-dictionary ispell-local-pdict)))
        (t
         (if arg
             (setq ispell-personal-dictionary dict)
           (setq ispell-local-pdict dict))
         (message "%s personal dictionary set to %s"
                  (if arg "Global" "Local")
                  dict))))

(easy-menu-add-item
 ispell-menu-map '()
 ["Set up Personals Dictionaries" rw-ispell-set-up-pdicts
  :visible (not rw-ispell-is-set-up)])

;; Menu items analogous to "Change dictionary..."

(easy-menu-add-item
 ispell-menu-map '()
 ["Change Dictionary with Personal..." rw-ispell-change-dictionary
  :visible rw-ispell-is-set-up])

(easy-menu-add-item
 ispell-menu-map '()
 ["Change Personal Dictionary..." rw-ispell-change-personal-dictionary
  :visible rw-ispell-is-set-up])

(provide 'rw-ispell)

;;; rw-ispell.el ends here.
