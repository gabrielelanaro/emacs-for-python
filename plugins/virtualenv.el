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
  (if (file-directory-p dir)
      (setenv "PATH"
              (concat (expand-file-name dir)
                      path-separator
                      (getenv "PATH")))))

(defun virtualenv-name-buffer (buffer name)
  "Assign the virtualenv NAME to the BUFFER, you can fetch the
name with the virtualenv-name local variable"
  (with-current-buffer buffer
    (rename-buffer (concat (buffer-name) "(" name ")"))    
    (make-local-variable 'virtualenv-name)
    (setq 'virtualenv-name name)
    )
  )

(defun virtualenv-unname-buffer (buffer)
  "Remove the assignment of the virtualenv name from the BUFFER"
  (with-current-buffer buffer
    (string-match (format "(%s)$" virtualenv-name) (buffer-name))
    (rename-buffer (replace-match "" nil t (buffer-name)))
    (kill-local-variable 'virtualenv-name)
    )
  )


(defun virtualenv-activate (dir)
  "Activate the virtualenv located in DIR"
  (interactive "dVirtualenv Directory: ")
  
  ;; This is definitively a risky operation
  (make-local-variable 'process-environment)
  (make-local-variable 'exec-path)
  
  (setenv "VIRTUAL_ENV" dir)
  (virtualenv-add-to-path (concat dir "/bin"))
  (add-to-list 'exec-path (concat dir "/bin"))

  (virtualenv-name-buffer (current-buffer) (file-name-nondirectory dir))
  )

(defun virtualenv-deactivate ()
  "Deactivate the current virtual enviroment"
  (interactive)
  (kill-local-variable 'process-environment)
  (kill-local-variable 'exec-path)
  
  (virtualenv-unname-buffer (current-buffer))
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