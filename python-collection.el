;; This file initializate all the stuff

;; Trick to get the filename of the installation directory
(defconst python-collection-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of python-collection"
)
(add-to-list 'load-path
	     (concat python-collection-install-dir "yasnippet"))
(require 'yasnippet)

(yas/initialize)
(yas/load-directory (concat python-collection-install-dir "yasnippet/snippets"))

(setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt))
