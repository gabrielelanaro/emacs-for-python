;;; highlight-indentation.el --- Function for highlighting indentation
;; Author: Anton Johansson <anton.johansson@gmail.com> - http://antonj.se
;; Created: Dec 15 23:42:04 2010
;; URL: https://github.com/antonj/Highlight-Indentation-for-Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;; Customize `highlight-indent-face' to suit your theme.

;;; Code:

(defface highlight-indent-face
  ;; Fringe has non intrusive color in most color-themes
  '((t :inherit fringe))
  "Basic face for highlighting indentation guides."
  :group 'basic-faces)

;; Used buffer-local to toggle on-off
(setq-default highlight-indent-active nil)
;; Needed to to remove font-lock-keywords
(setq-default highlight-indent-offset 4)

(defun highlight-indentation (&optional indent-width)
  "Toggle highlight indentation.
Optional argument INDENT-WIDTH specifies which indentation
level (spaces only) should be highlighted, if omitted
indent-width will be guessed from current major-mode"
  (interactive "P")
  (when (not highlight-indent-active)
    (set (make-local-variable 'highlight-indent-offset)
         (if indent-width
             indent-width
           ;; Set indentation offset according to major mode
           (cond ((eq major-mode 'python-mode)
                  (if (boundp 'python-indent)
                      python-indent
                    python-indent-offset))
                 ((eq major-mode 'ruby-mode)
                  ruby-indent-level)
                 ((eq major-mode 'nxml-mode)
                  nxml-child-indent)
                 ((local-variable-p 'c-basic-offset)
                  c-basic-offset)
                 (t
                  (default-value 'highlight-indent-offset))))))
  (let ((re (format "\\( \\) \\{%s\\}" (- highlight-indent-offset 1))))
    (if highlight-indent-active
        (progn ;; Toggle off
          (set (make-local-variable 'highlight-indent-active) nil)
          (font-lock-remove-keywords nil `((,re (1 'highlight-indent-face))))
          (message "highlight-indentation OFF"))
      (progn ;; Toggle on
        (set (make-local-variable 'highlight-indent-active) t)
        (font-lock-add-keywords nil `((,re (1 'highlight-indent-face))))
        (message (format "highlight-indentation with indent-width %s"
                         highlight-indent-offset))))
    (font-lock-fontify-buffer)))

(provide 'highlight-indentation)

;;; highlight-indentation.el ends here
