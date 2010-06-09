;; ropemacs Integration with auto-completion
;; Feature that can be added:
;; - (document . rope-get-doc) to get the doc for the symbol completed!
;; - Calltips (complete after a trigger (, maybe)

(defun ac-ropemacs-candidates ()
  (mapcar (lambda (completion)
      (concat ac-prefix completion))
    (rope-completions)))

(defun ac-nropemacs-get-doc (symbol)
  "Return the doc at point, currently it doesn't use symbol"
  (rope-get-doc))


(ac-define-source nropemacs
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")))

(ac-define-source nropemacs-dot
  '((candidates . ac-ropemacs-candidates)
    (symbol     . "p")
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-nropemacs-setup ()
  (setq ac-sources (append '(ac-source-nropemacs
                             ac-source-nropemacs-dot) ac-sources)))

;; extended ropemacs

(defun ac-eropemacs-candidates ()
  (mapcar (lambda (proposal)
          (destructuring-bind (name doc type) proposal
            (list (concat ac-prefix name) doc
                  (if type (substring type 0 1) nil))))
        (rope-extended-completions)))

(defun ac-eropemacs-document (item) (car  item))
(defun ac-eropemacs-symbol   (item) (cadr item))

(ac-define-source extended-ropemacs
  '((candidates . ac-eropemacs-candidates)
;;    (document   . ac-eropemacs-document)
    (symbol     . ac-eropemacs-symbol)))

(ac-define-source extended-ropemacs-dot
  '((candidates . ac-eropemacs-candidates)
;;    (document   . ac-eropemacs-document)
    (symbol     . ac-eropemacs-symbol)
    (prefix     . c-dot)
    (requires   . 0)))

(defun ac-eropemacs-setup ()
  (setq ac-sources (append '(ac-source-extended-ropemacs
                             ac-source-extended-ropemacs-dot) ac-sources)))

(defun ac-ropemacs-setup ()
  (if (functionp 'rope-extended-completions)
      (add-hook 'python-mode-hook 'ac-eropemacs-setup)
    (add-hook 'python-mode-hook 'ac-nropemacs-setup)))
