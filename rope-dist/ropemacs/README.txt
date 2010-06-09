=========================
 ropemacs, rope in emacs
=========================

Ropemacs is an emacs mode that uses rope_ library to provide features
like python refactorings and code-assists.  You should install rope_
library and pymacs_ before using ropemacs.

.. _rope: http://rope.sf.net/
.. _pymacs: http://pymacs.progiciels-bpi.ca/pymacs.html


New Features
============

``rope-find-occurrences`` sets ``next-error-function``.  That means
compilation mode keys like ``C-x \``` work for occurrences buffer,
too.

Also there is a bug in pymacs 23 and 24-beta1 that makes python reach
maximum recursion after interrupting a pymacs command; a patch is
included in the docs folder.


Setting Up
==========

After installing pymacs, add these lines to your ``~/.emacs`` file::

  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")

Note that rope and ropemacs should be in your ``PYTHONPATH`` for this
to work.

Also note that ropemacs may redefine some standard Emacs and your custom key
bindings.  To prevent this, put the following example lines to your
``~/.emacs`` *before* the lines presented above:

  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-p")

See keybinding_ and variables_ sections for more details.

Loading Lazily
--------------

If you want to load ropemacs only when you really need it, you can use
a function like this in your ``~/.emacs``::

  (defun load-ropemacs ()
    "Load pymacs and ropemacs"
    (interactive)
    (require 'pymacs)
    (pymacs-load "ropemacs" "rope-")
    ;; Automatically save project python buffers before refactorings
    (setq ropemacs-confirm-saving 'nil)
  )
  (global-set-key "\C-xpl" 'load-ropemacs)

And execute ``load-ropemacs`` (or use ``C-x p l``) whenever you want
to use ropemacs.


Not Installing
--------------

If you don't want to install rope library and ropemacs you can extract
them somewhere and add these lines to your ``.emacs``::

  ;; Add this before loading pymacs if you haven't installed rope and ropemacs
  (setq pymacs-load-path '("/path/to/rope"
                           "/path/to/ropemacs"))


Multiple Python Versions
------------------------

Rope needs at least Python2.5.  If you have older versions of Python
you can use ``PYMACS_PYTHON`` environment variable.  You can add::

  (setenv "PYMACS_PYTHON" "python2.5")

to force pymacs to use Python2.5.


Ropemacs Minor Mode
-------------------

Ropemacs registers its local keys when ``ropemacs-mode`` is enabled.
By default it is enabled using ``python-mode`` hook (this hook is
available if you are using Emacs' ``python.el`` or XEmacs'
``python-mode.el``).  If you want to enable it in other major modes
either execute ``ropemacs-mode`` manually or call it in some other
hook.


Getting Started
===============

Refactoring Dialog
------------------

Ropemacs refactorings use a special kind of dialog.  When you start a
refactoring, you'll be asked to confirm saving modified python
buffers; you can change it by using ``ropemacs-confirm-saving``
variable.  Adding ``(setq ropemacs-confirm-saving 'nil)`` to your
``.emacs`` file, will make emacs save them without asking.

After that depending on the refactoring, you'll be asked about the
essential information a refactoring needs to know (like the new name
in rename refactoring).  You can skip it by prefixing the refactoring;
this can be useful when using batchset command (described later).

Next you'll see the base prompt of a refactoring dialog that shows
something like "Choose what to do".  By entering the name of a
refactoring option you can set its value.  After setting each option
you'll be returned back to the base prompt.  Finally, you can ask rope
to perform, preview or cancel the refactoring.

See keybinding_ section and try the refactorings yourself.


Finding Files
-------------

By using ``rope-find-file`` (``C-x p f`` by default), you can search
for files in your project.  When you complete the minibuffer you'll
see all files in the project; files are shown as their reversed paths.
For instance ``projectroot/docs/todo.txt`` is shown like
``todo.txt<docs``.  This way you can find files faster in your
project.  ``rope-find-file-other-window`` (``C-x p 4 f``) opens the
file in the other window.  With prefix, these commands show python
files only.


Code-Assist
-----------

``rope-code-assist`` command (``M-/`` by default) will let you select
from a list of completions.  If prefixed (``C-u M-/``), ropemacs
inserts the common prefix, automatically.  If a numeric argument is
given, ropemacs will insert the common prefix for that many of the
first proposals.

``rope-lucky-assist`` command (``M-?``) does not ask anything;
instead, it inserts the first proposal.  By prefixing it, you can
choose which proposal to insert.  ``C-u 1 M-?`` uses the second
propsal, for instance.

Here::

  xxaa = None
  xxab = None
  xxba = None
  xxbb = None

  x^

consider cursor is at ``^`` position.  This table shows what happens
when code-assist commands are used:

============  ==========  =======================
Key           Inserts     Minibuffer Completions
============  ==========  =======================
M-/                       xxaa, xxab, xxba, xxbb
C-u M-/       x           xxaa, xxab, xxba, xxbb
C-u 2 M-/     xa          xxaa, xxab
M-?           xaa
C-u 1 M-/     xab
C-u 3 M-/     xbb
============  ==========  =======================

Note that minibuffer completions are shown by pressing tab key at the
completion prompt.  Also you can use ``rope-completions`` lisp function
to get the list of completions.


Finding Occurrences
-------------------

The find occurrences command (``C-c f`` by default) can be used to
find the occurrences of a python name.  If ``unsure`` option is
``yes``, it will also show unsure occurrences; unsure occurrences are
indicated with a ``?`` mark in the end.


Dialog ``batchset`` Command
---------------------------

When you use ropemacs dialogs there is a command called ``batchset``.
It can set many options at the same time.  After selecting this
command from dialog base prompt, you are asked to enter a string.

``batchset`` strings can set the value of configs in two ways.  The
single line form is like this::

  name1 value1
  name2 value2

That is the name of config is followed its value.  For multi-line
values you can use::

  name1
   line1
   line2

  name2
   line3

Each line of the definition should start with a space or a tab.  Note
that blank lines before the name of config definitions are ignored.

``batchset`` command is useful when performing refactorings with long
configs, like restructurings::

  pattern ${pycore}.create_module(${project}.root, ${name})

  goal generate.create_module(${project}, ${name})

  imports
   from rope.contrib import generate

  args
   pycore: type=rope.base.pycore.PyCore
   project: type=rope.base.project.Project

.. ignore the two-space indents

This is a valid ``batchset`` string for restructurings.  When using
batchset, you usually want to skip initial questions.  That can be
done by prefixing refactorings.

Just for the sake of completeness, the reverse of the above
restructuring can be::

  pattern ${create_module}(${project}, ${name})

  goal ${project}.pycore.create_module(${project}.root, ${name})

  args
   create_module: name=rope.contrib.generate.create_module
   project: type=rope.base.project.Project


Enabling Autoimport
-------------------

Ropemacs can propose and automatically import global names in other
modules.  But this feature is disabled by default.  Before using it,
you should add::

  (setq ropemacs-enable-autoimport 't)

to your ``~/.emacs`` file.  After enabling, rope maintains a cache of
global names for each project.  It updates the cache only when modules
are changed; if you want to cache all your modules at once, use
``rope-generate-autoimport-cache``.  It will cache all of the modules
inside the project plus those whose names are listed in
``ropemacs-autoimport-modules`` list::

  # add the name of modules you want to autoimport
  (setq ropemacs-autoimport-modules '("os" "shutil"))

Now if you are in a buffer that contains::

  rmtree

and you execute ``ropemacs-auto-import`` you'll end up with::

  from shutil import rmtree
  rmtree

Also ``rope-code-assist`` and ``rope-lucky-assist`` propose
auto-imported names by using ``name : module`` style.  Selecting them
will import the module automatically.


Filtering Resources
-------------------

Some refactorings, restructuring and find occurrences take an option
called resources.  This option can be used to limit the resources on
which a refactoring should be applied.

It uses a simple format: each line starts with either '+' or '-'.
Each '+' means include the file (or its children if it's a folder)
that comes after it.  '-' has the same meaning for exclusion.  So
using::

  +rope
  +ropetest
  -rope/contrib

means include all python files inside ``rope`` and ``ropetest``
folders and their subfolder, but those that are in ``rope/contrib``.
Or::

  -ropetest
  -setup.py

means include all python files inside the project but ``setup.py`` and
those under ``ropetest`` folder.


Variables
---------

* ``ropemacs-confirm-saving``: If non-nil, you have to confirm saving all
  modified python files before refactorings; otherwise they are saved
  automatically. Defaults to ``t``.
* ``ropemacs-codeassist-maxfixes``: The maximum number of syntax errors
  to fix for code assists.  The default value is ``1``.
* ``ropemacs-separate-doc-buffer``: Should ``rope-show-doc`` use a
  separate buffer or the minibuffer.  Defaults to ``t``.
* ``ropemacs-guess-project``: If non-nil, ropemacs tries to guess and
  open the project that contains the file on which a rope command is
  performed when no project is already opened.

* ``ropemacs-enable-autoimport``: Shows whether to enable autoimport.
  Defaults to ``nil``.
* ``ropemacs-autoimport-modules``: The name of modules whose global
  names should be cached.  ``rope-generate-autoimport-cache`` reads
  this list and fills its cache.
* ``ropemacs-autoimport-underlineds``: If set, autoimport will cache
  names starting with underlines, too.

These variables change the keybinding.  They should be set before
loading ropemacs.

* ``ropemacs-local-prefix``: The prefix for ropemacs refactorings.
  Defaults to ``C-c r``.
* ``ropemacs-global-prefix``: The prefix for ropemacs project commands
  Defaults to ``C-x p``.
* ``ropemacs-enable-shortcuts``: Shows whether to bind ropemacs
  shortcuts keys.  Defaults to ``t``.


Keybinding
----------

Uses almost the same keybinding as ropeide.  Note that global commands
have a ``C-x p`` prefix and local commands have a ``C-c r`` prefix.
You can change that (see variables_ section).

================  ============================
Key               Command
================  ============================
C-x p o           rope-open-project
C-x p k           rope-close-project
C-x p f           rope-find-file
C-x p 4 f         rope-find-file-other-window
C-x p u           rope-undo
C-x p r           rope-redo
C-x p c           rope-project-config
C-x p n [mpfd]    rope-create-(module|package|file|directory)
                  rope-write-project

C-c r r           rope-rename
C-c r l           rope-extract-variable
C-c r m           rope-extract-method
C-c r i           rope-inline
C-c r v           rope-move
C-c r x           rope-restructure
C-c r u           rope-use-function
C-c r f           rope-introduce-factory
C-c r s           rope-change-signature
C-c r 1 r         rope-rename-current-module
C-c r 1 v         rope-move-current-module
C-c r 1 p         rope-module-to-package

C-c r o           rope-organize-imports
C-c r n [vfcmp]   rope-generate-(variable|function|class|module|package)

C-c r a /         rope-code-assist
C-c r a g         rope-goto-definition
C-c r a d         rope-show-doc
C-c r a f         rope-find-occurrences
C-c r a ?         rope-lucky-assist
C-c r a j         rope-jump-to-global
C-c r a c         rope-show-calltip
                  rope-analyze-module

                  rope-auto-import
                  rope-generate-autoimport-cache
===============   ============================


Shortcuts
---------

Some commands are used very frequently; specially the commands in
code-assist group.  You can define your own shortcuts like this::

  (define-key ropemacs-local-keymap "\C-cg" 'rope-goto-definition)

Ropemacs itself comes with a few shortcuts:

================  ============================
Key               Command
================  ============================
M-/               rope-code-assist
M-?               rope-lucky-assist
C-c g             rope-goto-definition
C-c d             rope-show-doc
C-c f             rope-find-occurrences
================  ============================

These shortcuts will be used only when ropemacs-enable-shortcuts is
non-nil (it is enabled by default).  Note that in order to disable these
shortcuts, the value of ropemacs-enable-shortcuts should be set *before*
loading ropemacs::

  (setq ropemacs-enable-shortcuts 'nil)


Contributing
============

Send your bug reports, feature requests and patches to `rope-dev (at)
googlegroups.com`_.

.. _`rope-dev (at) googlegroups.com`: http://groups.google.com/group/rope-dev


License
=======

This program is under the terms of GPL (GNU General Public License).
Have a look at ``COPYING`` file for more information.
