(defcustom TeX-macro-global (TeX-macro-global)
  "Directories containing the site's TeX macro and style files."
  :group 'TeX-file
  :type '(repeat (directory :format "%v")))

(defcustom TeX-printer-list
  '(("Default"
     ;; Print to the (unnamed) default printer.  If there is a DVI
     ;; file print via Dvips.  If not, pass the output file (which
     ;; should then be a Postscript or PDF file) directly to lpr.
     "{ test -e %s.dvi && %(o?)dvips -f %r %s | lpr; } || lpr %o"
     ;; Show the queue for the (unnamed) default printer.
     "lpq"))
  "List of available printers.

The first element of each entry is the printer name.

The second element is the command used to print to this
printer.  It defaults to the value of `TeX-print-command' when nil.

The third element is the command used to examine the print queue for
this printer.  It defaults to the value of `TeX-queue-command' similarly.

Any occurrence of `%p' in the second or third element is expanded to
the printer name given in the first element, then ordinary expansion
is performed as specified in `TeX-expand-list'.

If this list is empty, only `TeX-print-command' and `TeX-queue-command'
get consulted."
  :group 'TeX-command
  :type '(repeat (group (string :tag "Name")
			(option (group :inline t
				       :extra-offset -4
				       (choice :tag "Print"
					       (const :tag "default")
					       (string :format "%v"))
				       (option (choice :tag "Queue"
						       (const :tag "default")
						       (string
							:format "%v"))))))))


;; This is the major configuration variable.  Most sites will only
;; need to change the second string in each entry, which is the name
;; of a command to send to the shell.  If you use other formatters
;; like AMSLaTeX or AMSTeX, you can add those to the list.  See
;; TeX-expand-list for a description of the % escapes

(defcustom TeX-command-list
  `(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil
     (plain-tex-mode ams-tex-mode texinfo-mode) :help "Run plain TeX")
    ("LaTeX" "%`%l%(mode)%' %t"
     TeX-run-TeX nil
     (latex-mode doctex-mode) :help "Run LaTeX")
	;; Not part of standard TeX.
    ("Makeinfo" "makeinfo %t" TeX-run-compile nil
     (texinfo-mode) :help "Run Makeinfo with Info output")
    ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
     (texinfo-mode) :help "Run Makeinfo with HTML output")
    ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t"
     TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX")
    ;; support for ConTeXt  --pg
    ;; first version of ConTeXt to support nonstopmode: 2003.2.10
    ("ConTeXt" "texexec --once --texutil %(execopts)%t"
     TeX-run-TeX nil (context-mode) :help "Run ConTeXt once")
    ("ConTeXt Full" "texexec %(execopts)%t"
     TeX-run-TeX nil
     (context-mode) :help "Run ConTeXt until completion")
    ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
    ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
    ,(if (or window-system (getenv "DISPLAY"))
	'("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
       '("View" "dvi2tty -q -w 132 %s" TeX-run-command t t
	 :help "Run Text viewer"))
    ("Print" "%p" TeX-run-command t t :help "Print the file")
    ("Queue" "%q" TeX-run-background nil t :help "View the printer queue"
     :visible TeX-queue-command)
    ("File" "%(o?)dvips %d -o %f " TeX-run-command t t
     :help "Generate PostScript file")
    ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
    ("Check" "lacheck %s" TeX-run-compile nil (latex-mode)
     :help "Check LaTeX file for correctness")
    ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t
     :help "Spell-check the document")
    ("Clean" "TeX-clean" TeX-run-function nil t
     :help "Delete generated intermediate files")
    ("Clean All" "(TeX-clean t)" TeX-run-function nil t
     :help "Delete generated intermediate and output files")
    ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))
  "List of commands to execute on the current document.

Each element is a list, whose first element is the name of the command
as it will be presented to the user.

The second element is the string handed to the shell after being
expanded.  The expansion is done using the information found in
`TeX-expand-list'.

The third element is the function which actually start the process.
Several such hooks has been defined:

TeX-run-command: Start up the process and show the output in a
separate buffer.  Check that there is not two commands running for the
same file.  Return the process object.

TeX-run-format: As `TeX-run-command', but assume the output is created
by a TeX macro package.  Return the process object.

TeX-run-TeX: For TeX output.

TeX-run-interactive: Run TeX or LaTeX interactively.

TeX-run-BibTeX: For BibTeX output.

TeX-run-Biber: For Biber output.

TeX-run-compile: Use `compile' to run the process.

TeX-run-shell: Use `shell-command' to run the process.

TeX-run-discard: Start the process in the background, discarding its
output.

TeX-run-background: Start the process in the background, show output
in other window.

TeX-run-silent: Start the process in the background.

TeX-run-discard-foreground: Start the process in the foreground,
discarding its output.

TeX-run-function: Execute the Lisp function or function call
specified by the string in the second element.  Consequently,
this hook does not start a process.

TeX-run-discard-or-function: If the command is a Lisp function,
execute it as such, otherwise start the command as a process,
discarding its output.

To create your own hook, define a function taking three arguments: The
name of the command, the command string, and the name of the file to
process.  It might be useful to use `TeX-run-command' in order to
create an asynchronous process.

If the fourth element is non-nil, the user will get a chance to
modify the expanded string.

The fifth element indicates in which mode(s) the command should be
present in the Command menu.  Use t if it should be active in any
mode.  If it should only be present in some modes, specify a list with
the respective mode names.

Any additional elements get just transferred to the respective menu entries."
  :group 'TeX-command
  :type '(repeat (group :value ("" "" TeX-run-command nil t)
			(string :tag "Name")
			(string :tag "Command")
			(choice :tag "How"
				:value TeX-run-command
				(function-item TeX-run-command)
				(function-item TeX-run-format)
				(function-item TeX-run-TeX)
				(function-item TeX-run-interactive)
				(function-item TeX-run-BibTeX)
				(function-item TeX-run-Biber)
				(function-item TeX-run-compile)
				(function-item TeX-run-shell)
				(function-item TeX-run-discard)
				(function-item TeX-run-background)
				(function-item TeX-run-silent)
				(function-item TeX-run-discard-foreground)
				(function-item TeX-run-function)
				(function-item TeX-run-discard-or-function)
				(function :tag "Other"))
			(boolean :tag "Prompt")
			(choice :tag "Modes"
				(const :tag "All" t)
				(set (const :tag "Plain TeX" plain-tex-mode)
				     (const :tag "LaTeX" latex-mode)
				     (const :tag "DocTeX" doctex-mode)
				     (const :tag "ConTeXt" context-mode)
				     (const :tag "Texinfo" texinfo-mode)
				     (const :tag "AmSTeX" ams-tex-mode)))
			(repeat :tag "Menu elements" :inline t sexp))))
