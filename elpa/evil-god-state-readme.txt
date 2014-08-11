This is an evil-mode state for using god-mode.

It provides a command `evil-execute-in-god-state' that switches to
`god-local-mode' for the next command. I bind it to ","

    (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)

for an automatically-configured leader key.

Since `evil-god-state' includes an indicator in the mode-line, you may want
to use `diminish' to keep your mode-line uncluttered, e.g.

    (add-hook 'evil-god-start-hook (lambda () (diminish 'god-local-mode)))
    (add-hook 'evil-god-stop-hook (lambda () (diminish-undo 'god-local-mode)))
