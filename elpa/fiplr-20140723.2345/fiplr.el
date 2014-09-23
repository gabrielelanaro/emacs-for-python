;;; fiplr.el --- Fuzzy finder for files in a project.

;; Copyright © 2013 Chris Corbyn
;;
;; Author: Chris Corbyn <chris@w3style.co.uk>
;; URL: https://github.com/d11wtq/fiplr
;; Version: 0.2.8
;; Keywords: convenience, usability, project

;; This file is NOT part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

;;; --- Commentary

;; Overview:
;;
;; Fiplr makes it really easy to find files anywhere within your entire
;; project by using a cached directory tree and delegating to grizzl.el
;; while you search the tree.
;;
;;   M-x fiplr-find-file
;;
;; By default it looks through all the parent directories of the file you're
;; editing until it finds a .git, .hg, .bzr or .svn directory. You can
;; customize this list of root markers by setting `fiplr-root-markers'.
;;
;;   (setq fiplr-root-markers '(".git" ".svn"))
;;
;; Some files are ignored from the directory tree because they are not text
;; files, or simply to speed up the search. The default list can be
;; customized by setting `fiplr-ignored-globs'.
;;
;;   (setq fiplr-ignored-globs '((directories (".git" ".svn"))
;;                               (files ("*.jpg" "*.png" "*.zip" "*~"))))
;;
;; These globs are used by the UNIX `find' command's -name flag.
;;
;; Usage:
;;
;;   Find files:        M-x fiplr-find-file
;;   Find directories:  M-x fiplr-find-directory
;;   Clear caches:      M-x fiplr-clear-cache
;;
;; For convenience, bind "C-x f" to `fiplr-find-file':
;;
;;   (global-set-key (kbd "C-x f") 'fiplr-find-file)
;;
;; Because fiplr caches the project tree, you may sometimes wish to clear the
;; cache while searching. Use "C-c r" to do this.

(eval-when-compile
  (require 'cl-lib)
  (require 'grizzl))

;;; --- Package Configuration

(defvar *fiplr-caches* '((files) (directories))
  "Internal caches used by fiplr.")

(defvar *fiplr-default-root-markers* '(".git" ".svn" ".hg" ".bzr")
  "A list of files/directories to look for that mark a project root.")

(defvar *fiplr-default-ignored-globs*
  '((directories (".git" ".svn" ".hg" ".bzr"))
    (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip")))
  "An alist of files and directories to exclude from searches.")

(defgroup fiplr nil
  "Configuration options for fiplr - find in project."
  :group 'convenience)

(defcustom fiplr-root-markers *fiplr-default-root-markers*
  "A list of files or directories that are found at the root of a project."
  :type    '(repeat string)
  :group   'fiplr)

(defcustom fiplr-ignored-globs *fiplr-default-ignored-globs*
  "An alist of glob patterns to exclude from search results."
  :type    '(alist :key-type symbol :value-type (repeat string))
  :group   'fiplr)

(defcustom fiplr-list-files-function 'fiplr-list-files
  "A function receiving DIR, TYPE and IGNORED, returning a list of files.

DIR is the directory under which to locate files (recursively).
TYPE is one of the symboles 'FILES or 'DIRECTORIES.
IGNORED is an alist of glob patterns to exclude. Its keys are 'DIRECTORIES
and 'FILES, so that entire directories can be excluded.

This setting allows for cross-platform compatibility by abstracting away the
details of locating files in a directory tree. The default uses a GNU/BSD
compatible `find' command.

This function is only invoked once, when building the search index."
  :type    'function
  :group   'fiplr)

;;; --- Public Functions

;;;###autoload
(defun fiplr-find-file ()
  "Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'."
  (interactive)
  (fiplr-find-file-in-directory (fiplr-root) fiplr-ignored-globs))

;;;###autoload
(defun fiplr-find-file-other-window ()
  "Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'.  The
file is opened using `find-file-other-window'."
  (interactive)
  (fiplr-find-file-in-directory (fiplr-root) fiplr-ignored-globs
                                #'find-file-other-window))

;;;###autoload
(defun fiplr-find-file-other-frame ()
  "Runs a completing prompt to find a file from the project.
The root of the project is the return value of `fiplr-root'.  The
file is opened using `find-file-other-frame'."
  (interactive)
  (fiplr-find-file-in-directory (fiplr-root) fiplr-ignored-globs
                                #'find-file-other-frame))

;;;###autoload
(defun fiplr-find-directory ()
  "Runs a completing prompt to find a directory from the project.
The root of the project is the return value of `fiplr-root'."
  (interactive)
  (fiplr-find-directory-in-directory (fiplr-root) fiplr-ignored-globs))

;;;###autoload
(defun fiplr-find-directory-other-window ()
  "Runs a completing prompt to find a directory from the project.
The root of the project is the return value of `fiplr-root'.  The
directory is opened using `dired-other-window'."
  (interactive)
  (fiplr-find-directory-in-directory (fiplr-root) fiplr-ignored-globs
                                     #'dired-other-window))

;;;###autoload
(defun fiplr-find-directory-other-frame ()
  "Runs a completing prompt to find a directory from the project.
The root of the project is the return value of `fiplr-root'.  The
directory is opened using `dired-other-frame'."
  (interactive)
  (fiplr-find-directory-in-directory (fiplr-root) fiplr-ignored-globs
                                     #'dired-other-frame))

;;;###autoload
(defun fiplr-clear-cache ()
  "Clears the internal caches used by fiplr so the project is searched again."
  (interactive)
  (setq *fiplr-caches*
        (list (list 'files)
              (list 'directories))))

;;; --- Minor Mode Definition

(defvar *fiplr-keymap* (make-sparse-keymap)
  "Internal keymap used by the minor-mode in fiplr.")

(define-key *fiplr-keymap* (kbd "C-c r")   'fiplr-reload-list)

(define-minor-mode fiplr-mode
  "Toggle the internal mode used by fiplr."
  nil
  " fiplr"
  *fiplr-keymap*)

;;; --- Private Macros

(defmacro fiplr-cache (type)
  "Get the internal cache used by fiplr for files of TYPE."
  `(cdr (assoc ,type *fiplr-caches*)))

;;; --- Private Functions

(defun fiplr-root ()
  "Locate the root of the project by walking up the directory tree.
The first directory containing one of fiplr-root-markers is the root.
If no root marker is found, the current working directory is used."
  (let ((cwd (if (buffer-file-name)
                 (directory-file-name
                  (file-name-directory (buffer-file-name)))
               (file-truename "."))))
    (or (fiplr-find-root cwd fiplr-root-markers)
        cwd)))

(defun fiplr-find-root (path root-markers)
  "Tail-recursive part of project-root."
  (let* ((this-dir (file-name-as-directory (file-truename path)))
         (parent-dir (expand-file-name (concat this-dir "..")))
         (system-root-dir (expand-file-name "/")))
    (cond
     ((fiplr-root-p path root-markers) this-dir)
     ((equal system-root-dir this-dir) nil)
     (t (fiplr-find-root parent-dir root-markers)))))

(defun fiplr-anyp (pred seq)
  "True if any value in SEQ matches PRED."
  (catch 'found
    (cl-map nil (lambda (v)
                  (when (funcall pred v)
                    (throw 'found v)))
            seq)))

(defun fiplr-root-p (path root-markers)
  "Predicate to check if the given directory is a project root."
  (let ((dir (file-name-as-directory path)))
    (fiplr-anyp (lambda (marker)
                  (file-exists-p (concat dir marker)))
                root-markers)))

(defun fiplr-list-files-shell-command (type path ignored-globs)
  "Builds the `find' command to locate all project files & directories.

PATH is the base directory to recurse from.
IGNORED-GLOBS is an alist with keys 'DIRECTORIES and 'FILES."
  (let* ((type-abbrev
          (lambda (assoc-type)
            (cl-case assoc-type
              ('directories "d")
              ('files "f"))))
         (name-matcher
          (lambda (glob)
            (mapconcat 'identity
                       `("-name" ,(shell-quote-argument glob))
                       " ")))
         (grouped-name-matchers
          (lambda (type)
            (mapconcat 'identity
                       `(,(shell-quote-argument "(")
                         ,(mapconcat (lambda (v) (funcall name-matcher v))
                                     (cadr (assoc type ignored-globs))
                                     " -o ")
                         ,(shell-quote-argument ")"))
                       " ")))
         (matcher
          (lambda (assoc-type)
            (mapconcat 'identity
                       `(,(shell-quote-argument "(")
                         "-type"
                         ,(funcall type-abbrev assoc-type)
                         ,(funcall grouped-name-matchers assoc-type)
                         ,(shell-quote-argument ")"))
                       " "))))
    (mapconcat 'identity
               `("find"
                 "-L"
                 ,(shell-quote-argument (directory-file-name path))
                 ,(funcall matcher 'directories)
                 "-prune"
                 "-o"
                 "-not"
                 ,(funcall matcher 'files)
                 "-type"
                 ,(funcall type-abbrev type)
                 "-print")
               " ")))

(defun fiplr-list-files (type path ignored-globs)
  "Expands to a flat list of files/directories found under PATH.
The first parameter TYPE is the symbol 'DIRECTORIES or 'FILES."
  (let* ((prefix (file-name-as-directory (file-truename path)))
         (prefix-length (length prefix))
         (list-string
          (shell-command-to-string (fiplr-list-files-shell-command
                                    type
                                    prefix
                                    ignored-globs))))
    (reverse (cl-reduce (lambda (acc file)
                          (if (> (length file) prefix-length)
                              (cons (substring file prefix-length) acc)
                            acc))
                        (split-string list-string "[\r\n]+" t)
                        :initial-value '()))))

(defun fiplr-reload-list ()
  "Clear caches and reload the file listing."
  (interactive)
  (when (minibufferp)
    (exit-minibuffer))
  (fiplr-clear-cache)
  (funcall last-command))

(defun fiplr-report-progress (n total)
  "Show the number of files processed in the message area."
  (when (= 0 (mod n 1000))
    (message (format "Indexing (%d/%d)" n total))))

(defun fiplr-find-file-in-directory
    (path ignored-globs &optional find-file-function)
  "Locate a file under the specified PATH.
If the directory has been searched previously, the cache is used.
Use FIND-FILE-FUNCTION to open the selected file, or `find-file'
if FIND-FILE-FUNCTION is `nil'."
  (let* ((root-dir (file-name-as-directory path))
         (index (fiplr-get-index 'files root-dir ignored-globs))
         (file (minibuffer-with-setup-hook
                   (lambda ()
                     (fiplr-mode 1))
                 (grizzl-completing-read (format "Find in project (%s)" root-dir)
                                         index))))
    (if (eq this-command 'fiplr-reload-list) ; exited for reload
        (fiplr-reload-list)
      (funcall (or find-file-function #'find-file)
               (concat root-dir file)))))

(defun fiplr-find-directory-in-directory
    (path ignored-globs &optional dired-function)
  "Locate a directory and run dired under the specified PATH.
If the directory has been searched previously, the cache is used.
Use DIRED-FUNCTION to open the selected file, or `dired' if
DIRED-FUNCTION is `nil'."
  (let* ((root-dir (file-name-as-directory path))
         (index (fiplr-get-index 'directories root-dir ignored-globs))
         (dir (minibuffer-with-setup-hook
                  (lambda ()
                    (fiplr-mode 1))
                (grizzl-completing-read (format "Dired in project (%s)" root-dir)
                                        index))))
    (if (eq this-command 'fiplr-reload-list) ; exited for reload
        (fiplr-reload-list)
      (funcall (or dired-function #'dired) (concat root-dir dir)))))

(defun fiplr-get-index (type path ignored-globs)
  "Internal function to lazily get a fiplr fuzzy search index."
  (let ((fiplr-cache-key (cons path ignored-globs)))
    (unless (assoc fiplr-cache-key (fiplr-cache type))
      (message (format "Scanning... (%s)" path))
      (push (cons fiplr-cache-key
                  (grizzl-make-index (funcall fiplr-list-files-function
                                              type
                                              path
                                              ignored-globs)
                                     :progress-fn #'fiplr-report-progress))
            (fiplr-cache type)))
    (cdr (assoc fiplr-cache-key (fiplr-cache type)))))

(provide 'fiplr)

;;; fiplr.el ends here
