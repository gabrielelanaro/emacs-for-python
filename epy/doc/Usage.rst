Typical Workflow
================

Intro
-----

Emacs-for-python pacakage is configured to be upstarted quickly, you shouln't spend your time in configuring and tweaking your emacs environment (and searching for appropriate extensions).

Most of the selected extensions are "intelligent", they aren't intrusive and they don't touch your focus.

After installing the package (it's a matter of load-file), fire up emacs.

Starting
--------

To open a new file you can proceed as usual, with the `C-x C-f` keystrokes, you would see the ido-mode prompt with fuzzy completion enabled, it's fairly intuitive, type some keywords and you will be searching throught files. It remembers your choices and prompt you for the most used files.

Editing
-------

After opening your python module you will have automatically activated some useful things:

- flymake error checking, errors are displayed automatically with a red line, hover your mouse over the line to see an error message.

- auto-completion it complete words in the current buffers, unfortunately I haven't found a good way to include "intellisense".


There are also editing facilities:

- *snippets* with yasnippet (with modified, more python-compliant snippets). Type the start of a word, like "class", press TAB and it expands itself in a snippet. It's integrated with auto-completion, they don't collide.

- *autoclosing brackets*, type a bracket, it will be closed automatically. Select a region of text, type an opening bracket, the text is wrapped between brackets automatically. You can't live without that.

- *open-next-line*: type `C-o` inside a sentence, you will be teleported to the next line, with the correct indentation.

After editing the files you may want to execute the code.  I would not recommend the execution facilities of "python-mode", instead I suggest you to act in this way::

  M-x compile
  <interactive> python whathever_test_to_do

In this way the tracebacks are automatically highlighted and you can re-run easily the command (Be careful to the working directory!)

Very Good! Test are all green, you may want to register your changes to version control::

  C-x v v

This will select the right thing to do, if the file is unregistered, it will be registered, else it will commit and prompt you for logging message. If you have other needs, I suggest you to spawn a shell and use it.

Others
------

Need to do 2+2?::
  
  M-c q

Will prompt you for an expression, the result is in the kill ring, you can yank it in the text with `C-y`

Cut/Copy/Paste from external applications is easy, the keys are remapped with the `super` key::
  
  Cut:   s-x
  Copy:  s-c
  Paste: s-v

The buffer are improved with the same ido interface plus Ibuffer, it's much colorful and has nice sorting functionalities.

Can't go with `M-Tab` keys (the desktop keep switching application)?  No problem it's remapped with `C-Tab`.
