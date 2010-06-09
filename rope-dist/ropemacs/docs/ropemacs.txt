=========================
 ropemacs, rope in emacs
=========================

Ropemacs is a plugin for performing python refactorings in emacs.  It
uses rope_ library and pymacs_.

You should install `rope`_ library and pymacs_ before using ropemacs.
You can download ropemacs from `project download page`_.

.. _rope: http://rope.sf.net/


Features
========

* Supports many of the refactorings that are supported by rope_
  library:

  * Rename
  * Extract method/local variable
  * Move class/function/module/package/method
  * Inline method/local variable/parameter
  * Restructuring
  * Change signature
  * ...

* Other refactoring-related features

  * Previewing refactorings
  * Undo/redo refactorings
  * Showing refactoring progress

* Code-assists

  * Code-completion
  * Goto definition
  * Show pydoc
  * Find occurrences
  * Organize imports (remove unused and duplicate imports and sort them)
  * Generating python elements


Source Repository
=================

The repository version needs ropemode (which was once part of
ropemacs); in order to use the repository version of ropemacs you need
to put ropemode in your ``PYTHONPATH``.  Note that ropemode is
included in released packages.

Ropemacs:

* repo url: http://bitbucket.org/agr/ropemacs
* snapshot: http://bitbucket.org/agr/ropemacs/get/tip.gz

Ropemode:

* repo url: http://bitbucket.org/agr/ropemode
* snapshot: http://bitbucket.org/agr/ropemode/get/tip.gz


Feedback
========

Send your bug reports, feature requests and patches to `rope-dev (at)
googlegroups.com`_.


License
=======

Ropemacs is under the terms of GNU GPL (GNU General Public License).

.. _project download page: http://sf.net/projects/rope/files
.. _`rope-dev (at) googlegroups.com`: http://groups.google.com/group/rope-dev
.. _pymacs: http://pymacs.progiciels-bpi.ca/pymacs.html
.. _Mercurial: http://selenic.com/mercurial
