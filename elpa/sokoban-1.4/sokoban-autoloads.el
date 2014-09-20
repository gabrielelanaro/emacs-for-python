;;; sokoban-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (sokoban) "sokoban" "sokoban.el" (21480 10061 278280
;;;;;;  628000))
;;; Generated autoloads from sokoban.el

(autoload 'sokoban "sokoban" "\
Sokoban.

Push the blocks onto the target squares.

sokoban-mode keybindings:
   \\<sokoban-mode-map>
\\[sokoban-start-game]	Starts a new game of Sokoban
\\[sokoban-restart-level]	Restarts the current level
\\[sokoban-goto-level]	Jumps to a specified level
\\[sokoban-move-left]	Move one square to the left
\\[sokoban-move-right]	Move one square to the right
\\[sokoban-move-up]	Move one square up
\\[sokoban-move-down]	Move one square down

\(fn)" t nil)

(unless (featurep 'xemacs) (define-key-after (lookup-key global-map [menu-bar tools games]) [sokoban] '(menu-item "Sokoban" sokoban) 'snake))

;;;***

;;;### (autoloads nil nil ("sokoban-pkg.el") (21480 10061 438289
;;;;;;  556000))

;;;***

(provide 'sokoban-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sokoban-autoloads.el ends here
