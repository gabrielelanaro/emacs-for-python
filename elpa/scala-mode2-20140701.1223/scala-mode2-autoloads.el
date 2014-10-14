;;; scala-mode2-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (scala-mode scala-mode:set-scala-syntax-mode) "scala-mode2"
;;;;;;  "scala-mode2.el" (21480 10070 85158 144000))
;;; Generated autoloads from scala-mode2.el

(autoload 'scala-mode:set-scala-syntax-mode "scala-mode2" "\
Sets the syntax-table and other realted variables for the current buffer to those of scala-mode. Can be used to make some other major mode (such as sbt-mode) use scala syntax-table.

\(fn)" nil nil)

(autoload 'scala-mode "scala-mode2" "\
Major mode for editing scala code.

When started, runs `scala-mode-hook'.

\\{scala-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(scala\\|sbt\\)\\'" . scala-mode))

(modify-coding-system-alist 'file "\\.\\(scala\\|sbt\\)\\'" 'utf-8)

;;;***

;;;### (autoloads nil nil ("scala-mode2-fontlock.el" "scala-mode2-indent.el"
;;;;;;  "scala-mode2-lib.el" "scala-mode2-map.el" "scala-mode2-paragraph.el"
;;;;;;  "scala-mode2-pkg.el" "scala-mode2-sbt.el" "scala-mode2-syntax.el")
;;;;;;  (21480 10070 169911 76000))

;;;***

(provide 'scala-mode2-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; scala-mode2-autoloads.el ends here
