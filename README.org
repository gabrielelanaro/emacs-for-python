* Welcome to the best collection of emacs extensions ever
I'm collecting and customizing the perfect environment for python
development, using the most beautiful emacs customization to obtain a
really modern and exciting (yet stable) way to edit text files.

In the package are included also a lot of other packages and
configurations, it's an upstart for clean emacs installations, these
configuration however are very similar to emacs-starter-kit and I
suggest you to give it a try, emacs-for-python is designed to work
with it (instruction below).

To get in contact or ask for help you have many possibilities:

github issue tracker: [[https://github.com/gabrielelanaro/emacs-for-python/issues]]

google+ page: [[https://plus.google.com/108723367526390492977/]]

email me: [[mailto:gabriele.lanaro@gmail.com]]

** Features 
The package is split into various files, in this way it's very easy
to include what you really need.

*** Python Related

    - Ropemacs (Custom) : A refactoring library
    - Auto Completion with rope (Custom) : Tested to work well with Rope
    - Flymake + pyflakes (Custom): Highlight errors on the fly, improved
      from the standard version
    - Virtualenv : Original, In-development tool for using virtualenv in
      emacs.
    - Custom Yasnippet Snippets
    - Cython Mode: a mode for highlighting cython files
    - latest python.el (fgallina/python.el)
    - nosetests integration (nose.el)

*** Editing Packages
    
    - Open Next Line: C-o will open a line under the cursor (very
      similar to vim o)
    - Auto Completion: Wonderful package for auto complete anything in
      emacs
    - Yasnippet: Snippets on steroids
    - Parentheses pairing (Custom): Bug-free parentheses pairing using
      skeletons.
    - smart-operator: Automatically adds spaces around arithmetic operators.

*** Configurations

    - ido activation
    - ibuffer instead of buffer menu
    - eshell configuration

*** Keybindings

    - Copy-Cut-Paste from clipboard with Super-C Super-X Super-V    
    - Calc Mode remapping to M-c
    - M-Tab remapped to C-Tab to avoid collisions with the desktop
      manager
    - nose keybindings

** Installation

Put the emacs-for-python directory in .emacs.d directory (or any
other), the add this line in your .emacs, it will enable all the
features of emacs-for-python package (editing configuration,
keybindings, python stuff etc...)

: (load-file "/path/to/emacs-for-python/epy-init.el")

You may want to enable only some of the features provided by
emacs-for-python, in this case just enable some of them:

: (add-to-list 'load-path "path/to/emacs-for-python/") ;; tell where to load the various files
: (require 'epy-setup)      ;; It will setup other loads, it is required!
: (require 'epy-python)     ;; If you want the python facilities [optional]
: (require 'epy-completion) ;; If you want the autocompletion settings [optional]
: (require 'epy-editing)    ;; For configurations related to editing [optional]
: (require 'epy-bindings)   ;; For my suggested keybindings [optional]
: (require 'epy-nose)       ;; For nose integration

For further information and usage suggestions check:

- [[https://github.com/gabrielelanaro/emacs-for-python/wiki]]
- [[http://wiki.github.com/gabrielelanaro/emacs-for-python/workflow]]

There is another nice tutorial on how to install emacs-for-python
here:
[[http://maddemcode.com/python/emacs-and-python-the-definitive-answer/]] 

*** Configuring the _flymake_ checker
New in version 0.3:
I changed the way you configure your checkers, now there is no
predefined checker and you have to configure it by your own.

The good news is that it is extremely simple to configure your own
command, for example to configure the pyflakes checker install it and
put in your customization file (.emacs):

: (epy-setup-checker "pyflakes %f")

Obviously you can substitute the ``"pyflakes %f"`` with whatever you
want, you just have to keep in mind that ``%f`` will be substituted
with the file which is being checked.

*** Adding the django snippets
The django snippets comes as an option, you can load them using:

: (epy-django-snippets)

As a command using M-x or putting it in your .emacs.
*** ipython integration 
Ipython can be integrated with python-mode. Clicking on the
Python->Start interpreter menu entry will spawn an ipython shell
instead of a python one. To enable the feature put this line in your .emacs:

: (epy-setup-ipython)

*** line highlighting
You may want to enable this feature with the color you prefer, to do
so, drop one of the following lines in your .emacs
: (global-hl-line-mode t) ;; To enable
: (set-face-background 'hl-line "black") ;; change with the color that you like
:                                        ;; for a list of colors: http://raebear.net/comp/emacscolors.html
*** Highlight Indentation
Interesting way to have a visual clue on how the code is indented:

[[https://github.com/gabrielelanaro/emacs-for-python/raw/master/doc/highlight_line_ind.png]]

: (require 'highlight-indentation)
: (add-hook 'python-mode-hook 'highlight-indentation)


*** disabling the auto-pairing of parenthesis
To disable the auto-pairing, drop the following line:
: (setq skeleton-pair nil) 

*** Nosetests keybindings
First of all, install nosetests. Then open a python file in your project and:
``C-c a``: run all test
``C-c m``: run tests in the module
``C-c .``: run test under cursor

``C-c p a``, ``C-c p m``, ``C-c p .``: same as above but drop in pdb when there's an error (Very useful!)
*** Disabling ropemacs
Just add the line to your .emacs
: (setq epy-enable-ropemacs nil)

** Installation with emacs-starter-kit

I've built up a little fork of emacs starter kit that is greatly
integrated with emacs-for-python, it seems the best solution to me!

The repo is at this address: [[http://github.com/gabrielelanaro/emacs-starter-kit]]

Remember that customization to emacs-starter-kit are done to a
custom.el file, and so you can substitute your distribution of
emacs-starter-kit and you have to copy just your customization file.

** License

This is mostly a distribution with some little tweaks, with respect of
the licenses, my tweaks are distributed according to the term of the
GNU GPL license, described in the file COPYING.

Generally each file has its own license and copyright notice (most, if
not all of packages are GPL), the license of rope (GPL) is in
rope-dist/COPYING.
