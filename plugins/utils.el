(defun utils-set-font (fontspec)
  "Set a font using the standard xft font specification like:Monospace-8"
  (interactive "sFont-spec: ")
  (setq default-frame-alist '((font-backend . "xft")
			      (font . fontspec)
			      (cursor-color . "black")))
  )

(provide 'utils)