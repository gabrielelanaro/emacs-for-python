;;; virtualenv.el --- Switching virtual python enviroments seamlessly

;; Copyright (C) 2010 Gabriele Lanaro

;; Author: Gabriele Lanaro <gabriele.lanaro@gmail.com>
;; Version: 0.1
;; Url: 

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

;; 


(setq workon-home (getenv "WORKON_HOME"))

(defun virtualenv-add-to-path (dir)
  "Add the specified path element to the Emacs PATH"
  (interactive "DEnter directory to be added to PATH: ")
  (setenv "PATH"
	  (concat (expand-file-name dir)
		  path-separator
		  (getenv "PATH"))))

(defun virtualenv-current ()
  "barfs the current activated virtualenv"
  (message 'virtualenv-name)
  )

(defun virtualenv-activate (dir)
  "Activate the virtualenv located in DIR"
  (interactive "DVirtualenv Directory: ")
  
  ;; Storing old variables
  (setq virtualenv-old-path (getenv "PATH"))
  (setq virtualenv-old-exec-path exec-path)
  
  (setenv "VIRTUAL_ENV" dir)
  (virtualenv-add-to-path (concat dir "/bin"))
  (add-to-list 'exec-path (concat dir "/bin"))
  
  (setq virtualenv-name (file-name-nondirectory dir))
  )

(defun virtualenv-deactivate ()
  "Deactivate the current virtual enviroment"
  (interactive)
  
  ;; Restoring old variables
  (setenv "PATH" virtualenv-old-path)
  (setq exec-path virtualenv-old-exec-path)

  (setq virtualenv-name nil)
  )

(defun is_virtualenv (dir)
  "Check if a directory is a virtualenv"
  (file-exists-p (concat dir "/bin/activate"))
  )

(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun workon-complete ()
  "return available completions for workon"
  (let 
      ;;Varlist				
      ((filelist (directory-files workon-home t))) ;; List directory
       ;; Let Body
    (mapcar 'file-name-nondirectory
       (filter 'is_virtualenv ;; select virtualenvs
	(filter 'file-directory-p filelist))) ;; select  directories
    )
  )

(defun workon (name)
  "Issue a virtualenvwrapper-like workon command"
  (interactive (list (completing-read "Virtualenv: " (workon-complete))))
  (virtualenv-activate (concat (getenv "WORKON_HOME") "/" name))
  )


(provide 'virtualenv)