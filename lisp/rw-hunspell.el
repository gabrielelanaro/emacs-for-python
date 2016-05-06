;;; rw-hunspell.el --- special functions for Hunspell in ispell.el
;;
;; Copyright (C) 2009 Ralf Wachinger
;;
;; Author: Ralf Wachinger <rwachinger@gmx.de>
;; Version: 0.2
;; Keywords: ispell
;; Compatibility: GNU Emacs 23.x
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
;; Additions for Hunspell, which find all existing Hunspell dictionaries
;; in the given directories, generate a special alist for Hunspell, and
;; optionally create a special menu for selecting the dictionaries.
;;
;; Save rw-hunspell.el in a convenient directory, preferably in
;; your `load-path'. Add the following to your `user-init-file':
;;
;;   (require 'rw-hunspell)
;;
;; When the creation starts:
;; a) keyboard: 'M-x rw-hunspell-setup RET'
;; b) menubar: Tools --> Spell Checking --> Set up Hunspell
;; c) automatically when Hunspell is used the first time
;; d) `user-init-file', after setting the user options: (rw-hunspell-setup)
;; e) when `rw-ispell-change-dictionary' (see rw-ispell.el) is called
;;
;; ESSENTIAL: `ispell-program-name' must be set to the Hunspell program name.
;; ATTENTION: Hunspell is not supported by ispell.el before GNU Emacs 23.x.
;; `ispell-dictionary' can be set, in addition to the default dictionary.
;; `ispell-local-dictionary-alist' can be set, as manual list in addition to
;; or overriding the automatically generated `rw-hunspell-dictionary' alist.
;;
;; Todo:
;; Make the program more dynamic, particularly the dictionary menu.
;; Parsing MS Windows locales from environmental variable LANG, e. g. "DEU".
;; Possibly full integration in ispell.el, analogous to the functions
;;   ispell-find-aspell-dictionaries and ispell-aspell-find-dictionary.
;;
;;; Change Log:
;;
;; 2009-03-29 (0.2)
;;
;;    * function definitions for `canonicalize-coding-system-name'
;;      and `coding-system-from-name' added. These are new functions
;;      in the CVS-Emacs from 2009-01-27, rw-hunspell.el needs it.
;;      They will be removed, when the stable Emacs-23.1 comes out.
;;
;; 2009-03-20 (0.1)
;;    Initial Release.
;;
;;; Code:

(require 'ispell)
(require 'easymenu)

;; User options.
;; This options must be set before Hunspell runs for the first time.

(defgroup rw-hunspell nil
  "Hunspell customization options."
  :group 'ispell)

(defcustom rw-hunspell-dicpath-list nil
  "*List of dictionary directories for Hunspell.
If not set, the directories from environmental variable DICPATH are taken."
  :type '(repeat string)
  :group 'rw-hunspell)

(defcustom rw-hunspell-default-dictionary "en_US"
  "*Default dictionary for Hunspell, e. g. \"en_US\" (basic file name)
or \"en_US_Hunspell\" (generated dictionary name). If not set,
the dictionary from environmental variables DICTIONARY or LANG are taken."
  :type 'string
  :group 'rw-hunspell)

(defcustom rw-hunspell-make-dictionary-menu nil
  "*Make menu with all found dictionaries when non-nil.
Needs rw-language-and-country-codes.el for full language and country names."
  :type 'boolean
  :group 'rw-hunspell)

(defcustom rw-hunspell-use-rw-ispell nil
  "*Use `rw-ispell-change-dictionary' when non-nil.
Needs rw-ispell.el when non-nil."
  :type 'boolean
  :group 'rw-hunspell)

(defcustom rw-hunspell-delete-dictionary-base-alist t
  "*Delete `ispell-dictionary-base-alist' for the emacs session when non-nil.
That alist is not useful for Hunspell, because it needs other parameters."
  :type 'boolean
  :group 'rw-hunspell)

;; Internal.

(defvar rw-hunspell-dictionary-alist nil
  "Automatically set, do not set manually.
List of automatically generated dictionaries with recognized encoding.
It has the same format as `ispell-dictionary-alist'.")

(defvar rw-hunspell-no-encoding-recognized-alist nil
  "Automatically set, do not set manually.
List of dictionaries, for which emacs can't recognize the encoding.
It has the same format as `ispell-dictionary-alist'.")

;; For Emacs-23.0-Versions before 2009-01-27.
;; CVSWeb URLs:
;; http://cvs.savannah.gnu.org/viewcvs/emacs/lisp/international/mule-cmds.el?cvsroot=emacs&r1=1.355&r2=1.356
(when (not (and (fboundp 'canonicalize-coding-system-name)
                (fboundp 'coding-system-from-name)))
  ;; Canonicalize the coding system name NAME by removing some prefixes
  ;; and delimiter characters.  Support function of
  ;; coding-system-from-name.
  (defun canonicalize-coding-system-name (name)
    (if (string-match "^iso[-_ ]?[0-9]" name)
        ;; "iso-8859-1" -> "8859-1", "iso-2022-jp" ->"2022-jp"
        (setq name (substring name (1- (match-end 0)))))
    (let ((idx (string-match "[-_ /]" name)))
      ;; Delete "-", "_", " ", "/" but do distinguish "16-be" and "16be".
      (while idx
        (if (and (>= idx 2)
                 (eq (string-match "16-[lb]e$" name (- idx 2))
                     (- idx 2)))
            (setq idx (string-match "[-_ /]" name (match-end 0)))
          (setq name (concat (substring name 0 idx) (substring name (1+ idx)))
                idx (string-match "[-_ /]" name idx))))
      name))

  (defun coding-system-from-name (name)
    "Return a coding system whose name matches with NAME (string or symbol)."
    (let (sym)
      (if (stringp name) (setq sym (intern name))
        (setq sym name name (symbol-name name)))
      (if (coding-system-p sym)
          sym
        (let ((eol-type
               (if (string-match "-\\(unix\\|dos\\|mac\\)$" name)
                   (prog1 (intern (match-string 1 name))
                     (setq name (substring name 0 (match-beginning 0)))))))
          (setq name (canonicalize-coding-system-name (downcase name)))
          (catch 'tag
            (dolist (elt (coding-system-list))
              (if (string= (canonicalize-coding-system-name (symbol-name elt))
                           name)
                  (throw 'tag (if eol-type (coding-system-change-eol-conversion
                                            elt eol-type)
                                elt))))))))))

(defun rw-hunspell-find-dictionaries ()
  "Find Hunspell's dictionaries."
  (if (and (boundp 'ispell-really-hunspell)
           ispell-really-hunspell)
      (let ((dictionary-directories
             (if rw-hunspell-dicpath-list
                 (mapcar #'file-name-as-directory
                         rw-hunspell-dicpath-list)
               (if (getenv "DICPATH")
                   (mapcar #'file-name-as-directory
                           (split-string (getenv "DICPATH") path-separator))
                 (list))))
            (hunspell-program-directory
             (if (file-name-absolute-p ispell-program-name)
                 (file-name-directory ispell-program-name)
               (if (executable-find ispell-program-name)
                   (file-name-directory (executable-find ispell-program-name))
                 nil)))
            (dictionaries (list)))
        (add-to-list 'dictionary-directories hunspell-program-directory)
        (dolist (directory dictionary-directories)
          (setq dictionaries
                (append
                 dictionaries
                 (mapcar #'file-name-sans-extension
                         (directory-files directory t ".+\\.dic")))))
        dictionaries)
    nil))

(defun rw-hunspell-make-dictionary-alist ()
  "Make `rw-hunspell-dictionary-alist' for Hunspell."
  (dolist (dictionary (rw-hunspell-find-dictionaries))
    (condition-case ()
        ;; Only for *.dic files with *.aff files.
        ;; In the OpenOffice dictionary directory there are
        ;; spellchecker dictionaries with files *.aff und *.dic
        ;; for every dictionary, this dictionaries are included.
        ;; Moreover, there are hyphenation dictionaries with files hyph*.dic
        ;; without files *.aff, this dictionaries are not included.
        (when (file-exists-p (concat dictionary ".aff"))
          (let (;; Encoding and wordchars are read from the *.aff file.
                (encoding "")
                (wordchars "")
                ;; Unique dictionary name
                (dictionary-name
                 (concat (file-name-nondirectory dictionary)
                         "_" (file-name-nondirectory
                              (directory-file-name
                               (file-name-directory dictionary))))))
            (with-temp-buffer
              (insert-file-contents (concat dictionary ".aff"))
              ;; Encoding declaration line, e. g. "SET ISO8859-1"
              (when (search-forward-regexp "^SET " nil t)
                (setq encoding
                      (car (last (split-string
                                  (buffer-substring
                                   (point)
                                   (progn (end-of-line) (point)))))))))
            (when (coding-system-from-name encoding)
              (with-temp-buffer
                (let ((coding-system-for-read
                       (coding-system-from-name encoding)))
                  (insert-file-contents (concat dictionary ".aff")))
                (setq wordchars
                      ;; Wordchars (correspond to otherchars) declaration line.
                      ;; There are *.aff-files which do not define wordchars.
                      (if (search-forward-regexp "^WORDCHARS " nil t)
                          (regexp-opt
                           (mapcar
                            'char-to-string
                            (car (last (split-string
                                        (buffer-substring
                                         (point)
                                         (progn (end-of-line) (point))))))))
                        ""))))
            ;; Entry for every found dictionary with recognized encoding.
            (when (coding-system-from-name encoding)
              (add-to-list
               'rw-hunspell-dictionary-alist
               (list dictionary-name
                     "[[:alpha:]]"
                     "[^[:alpha:]]"
                     wordchars
                     t
                     (list "-d" dictionary)
                     nil
                     (coding-system-from-name encoding))))
            ;; Encoding, that emacs can't recognize.
            (unless (coding-system-from-name encoding)
              (add-to-list
               'rw-hunspell-no-encoding-recognized-alist
               (list (concat dictionary-name "_" encoding)
                     "[[:alpha:]]"
                     "[^[:alpha:]]"
                     wordchars
                     t
                     (list "-d" dictionary)
                     nil
                     'raw-text)))))
      (file-error
       nil)))
  (rw-hunspell-make-default-dictionary-entry)
  (when (and rw-hunspell-dictionary-alist
             rw-hunspell-delete-dictionary-base-alist)
    (setq ispell-dictionary-base-alist nil)))

(defun rw-hunspell-make-default-dictionary-entry ()
  "Make a default dictionary entry for the specified dictionary."
  (catch 'found
    (let ((locale (car (split-string (getenv "LANG") "[.@]"))))
      (dolist (entry (append ispell-local-dictionary-alist
                             rw-hunspell-dictionary-alist))
        (let* ((name (or (car entry) "default"))
               (full-file-name (car (last (nth 5 entry))))
               (file-name (file-name-nondirectory full-file-name))
               (wordchars (nth 3 entry))
               (encoding (nth 7 entry)))
          (when (or (string= name "default")
                    (string= rw-hunspell-default-dictionary name)
                    (string= rw-hunspell-default-dictionary file-name)
                    (and (not rw-hunspell-default-dictionary)
                         (string= (or (getenv "DICTIONARY") locale)
                                  file-name)))
            (add-to-list
             'rw-hunspell-dictionary-alist
             (list nil
                   "[[:alpha:]]"
                   "[^[:alpha:]]"
                   wordchars
                   t
                   (list "-d" full-file-name)
                   nil
                   (coding-system-from-name encoding)))
            (throw 'found t)))))))

(defun rw-hunspell-make-dictionary-menu ()
  "Make menu with all automatically found and manually set dictionaries."
  (let (menu-local
        menu-global)
    ;; Automatically generated and manually set dictionaries.
    (dolist (entry (append ispell-local-dictionary-alist
                           rw-hunspell-dictionary-alist))
      (let* ((name (or (car entry) "default"))
             (file-name (file-name-nondirectory (car (last (nth 5 entry)))))
             ;; Long names for dictionaries in the menu.
             (long-name
              (concat
               (if (string= name "default") "- " "")
               (if (fboundp 'rw-lacc-replace-code-in-string)
                   (concat (capitalize
                            (rw-lacc-replace-code-in-string file-name))
                           " (" name ")")
                 name)
               (if (string= name "default") " -" ""))))
        (setq menu-global
              (append menu-global
                      (list
                       (vector
                        long-name
                        (if (and rw-hunspell-use-rw-ispell
                                 (fboundp 'rw-ispell-change-dictionary))
                            (list 'rw-ispell-change-dictionary name t)
                          (list 'ispell-change-dictionary name t))
                        :style 'toggle
                        :selected (list
                                   'string= 'ispell-dictionary name)))))
        (setq menu-local
              (append menu-local
                      (list
                       (vector
                        long-name
                        (if (and rw-hunspell-use-rw-ispell
                                 (fboundp 'rw-ispell-change-dictionary))
                            (list 'rw-ispell-change-dictionary name)
                          (list 'ispell-change-dictionary name))
                        :style 'toggle
                        :selected (list
                                   'string= 'ispell-local-dictionary name)))))))
    (setq menu-global (sort menu-global
                            #'(lambda (element1 element2)
                                (string< (aref element1 0) (aref element2 0)))))
    (push "Select global dictionary" menu-global)
    (setq menu-local (sort menu-local
                           #'(lambda (element1 element2)
                               (string< (aref element1 0) (aref element2 0)))))
    (push "Select local dictionary" menu-local)
    (easy-menu-add-item ispell-menu-map '() menu-global)
    (easy-menu-add-item ispell-menu-map '() menu-local)
    (easy-menu-add-item
     ispell-menu-map '("Select global dictionary")
     ["" nil
      :label (format "Global personal dictionary: %s"
                     (file-name-nondirectory
                      (or ispell-personal-dictionary "none")))])
    (easy-menu-add-item
     ispell-menu-map '("Select local dictionary")
     ["" nil
      :label (format "Local personal dictionary: %s"
                     (file-name-nondirectory
                      (or ispell-local-pdict "none")))])))

;; User functions.

(defun rw-hunspell-setup ()
  "Generate hunspell dictionary alist and menu, if they don't exist."
  (interactive)
  (unless rw-hunspell-dictionary-alist
    (ispell-check-version)
    (unless (boundp 'ispell-really-hunspell)
      (error "Hunspell is not supported on %s" (emacs-version)))
    (unless (and (boundp 'ispell-really-hunspell) ispell-really-hunspell)
      (error "Current spellchecker is not Hunspell, ispell-program-name is %s"
             ispell-program-name))
    (rw-hunspell-make-dictionary-alist)
    (when rw-hunspell-make-dictionary-menu
      (rw-hunspell-make-dictionary-menu))))

(easy-menu-add-item
 ispell-menu-map '()
 ["Set up Hunspell" rw-hunspell-setup
  :visible (not rw-hunspell-dictionary-alist)])

;; Hooks.

(defun rw-hunspell-setup-hook ()
  "Set up all for hunspell.
This hook is run when hunspell is used for the first time."
  (rw-hunspell-setup)
  (setq ispell-base-dicts-override-alist rw-hunspell-dictionary-alist))

(add-hook 'ispell-initialize-spellchecker-hook
          'rw-hunspell-setup-hook)

(provide 'rw-hunspell)

;;; rw-hunspell.el ends here.
