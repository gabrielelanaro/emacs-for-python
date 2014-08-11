Enclose is a minor mode that encloses cursor within punctuation
pairs. For example, hitting the key "(" will insert "(" and ")" and
place the cursor in between.

To use Enclose mode, make sure that this file is in Emacs load-path:
  (add-to-list 'load-path "/path/to/directory/or/file")

Then require enclose:
  (require 'enclose)

To start enclose mode:
  (enclose-mode t) or M-x enclose-mode

If you only want enclose mode active in some modes, use hooks:
  (add-hook 'ruby-mode-hook 'enclose-mode)

Or if you want to activate it in all buffers, use the global mode:
  (enclose-global-mode t)

When enclose mode is active, pressing any key, that is a key, in the
`enclose-table' hash, will insert the pair and place the cursor in
between. At this point the cursor is in focus, meaning that
pressing DEL would remove both punctuations around the cursor and
pressing the closing key will jump over the right punctuation.
Moving the cursor when in focus, unfocus the cursor.

Hitting the DEL key in focus removes both punctuations around the
cursor, when `enclose-remove-pair' is set to t, which is
default. When this variable is nil, only the left punctuation is
removed.

Keys that encloses cursor are defined in `enclose-table'. You can
add and remove new triggers by using the functions
`enclose-add-encloser' and `enclose-remove-encloser' respectively.
  (enclose-add-encloser "`" "`")
  (enclose-remove-encloser "(")

Some modes may have conflicting key bindings with enclose. To
avoid conflicts, the list `enclose-except-modes' contains names
of modes where enclose should not be activated (note, only in
the global mode). You can add new modes like this:
  (add-to-list 'enclose-except-modes 'conflicting-mode)
