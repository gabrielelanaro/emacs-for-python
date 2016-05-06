;; This file is a great big shortcut for all the features contained in emacs-for-python

;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

(add-to-list 'load-path epy-install-dir)
(require 'epy-setup)
(require 'epy-python)
(require 'epy-completion)
(require 'epy-editing)
(require 'epy-nose)
(require 'epy-bindings)



(provide 'epy-init)
