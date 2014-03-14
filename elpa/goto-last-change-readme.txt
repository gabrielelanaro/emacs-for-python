After installing goto-last-change.el in a `load-path' directory and
compiling it with `M-x byte-compile-file', load it with
	(require 'goto-last-change)
or autoload it with
	(autoload 'goto-last-change "goto-last-change"
	  "Set point to the position of the last change." t)

You may also want to bind a key to `M-x goto-last-change', e.g.
	(global-set-key "\C-x\C-\\" 'goto-last-change)

goto-last-change.el was written in response to to the following:

From: Dan Jacobson <jidanni@jidanni.org>
Newsgroups: gnu.emacs.bug
Subject: function to go to spot of last change
Date: Sun, 15 Jun 2003 00:15:08 +0000 (UTC)
Sender: news <news@main.gmane.org>
Message-ID: <mailman.7910.1055637181.21513.bug-gnu-emacs@gnu.org>
NNTP-Posting-Host: monty-python.gnu.org


Why of course, a function to get the user to the spot of last changes
in the current buffer(s?), that's what emacs must lack.

How many times have you found yourself mosying [<-not in spell
checker!?] thru a file when you wonder, where the heck was I just
editing?  Well, the best you can do is hit undo, ^F, and undo again,
to get back.  Hence the "burning need" for the additional function,
which you might name the-jacobson-memorial-function, due to its brilliance.
--
http://jidanni.org/ Taiwan(04)25854780
