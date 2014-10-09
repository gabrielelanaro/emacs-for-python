Company is a modular completion mechanism.  Modules for retrieving completion
candidates are called back-ends, modules for displaying them are front-ends.

Company comes with many back-ends, e.g. `company-elisp'.  These are
distributed in separate files and can be used individually.

Place company.el and the back-ends you want to use in a directory and add the
following to your .emacs:
(add-to-list 'load-path "/path/to/company")
(autoload 'company-mode "company" nil t)

Enable company-mode with M-x company-mode.  For further information look at
the documentation for `company-mode' (C-h f company-mode RET)

If you want to start a specific back-end, call it interactively or use
`company-begin-backend'.  For example:
M-x company-abbrev will prompt for and insert an abbrev.

To write your own back-end, look at the documentation for `company-backends'.
Here is a simple example completing "foo":

(defun company-my-backend (command &optional arg &rest ignored)
  (pcase command
    (`prefix (when (looking-back "foo\\>")
              (match-string 0)))
    (`candidates (list "foobar" "foobaz" "foobarbaz"))
    (`meta (format "This value is named %s" arg))))

Sometimes it is a good idea to mix several back-ends together, for example to
enrich gtags with dabbrev-code results (to emulate local variables).
To do this, add a list with both back-ends as an element in company-backends.
