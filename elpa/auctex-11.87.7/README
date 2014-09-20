This is the README file for the AUCTeX distribution.

     Copyright (C) 2008 Free Software Foundation, Inc.

     Copying and distribution of this file, with or without
     modification, are permitted in any medium without royalty provided
     the copyright notice and this notice are preserved.

Introduction to AUCTeX
**********************

This file gives a brief overview of what AUCTeX is.  It is *not* an
attempt to document AUCTeX.  Real documentation for AUCTeX is available
in the manual, which should be available as an info file after
installation.

AUCTeX is a comprehensive customizable integrated environment for
writing input files for TeX, LaTeX, ConTeXt, Texinfo, and docTeX using
Emacs or XEmacs.

It supports you in the insertion of macros, environments, and sectioning
commands by providing completion alternatives and prompting for
parameters.  It automatically indents your text as you type it and lets
you format a whole file at once.  The outlining and folding facilities
provide you with a focused and clean view of your text.

AUCTeX lets you process your source files by running TeX and related
tools (such as output filters, post processors for generating indices
and bibliographies, and viewers) from inside Emacs.  AUCTeX lets you
browse through the errors TeX reported, while it moves the cursor
directly to the reported error, and displays some documentation for that
particular error.  This will even work when the document is spread over
several files.

One component of AUCTeX that LaTeX users will find attractive is
preview-latex, a combination of folding and in-source previewing that
provides true "What You See Is What You Get" experience in your
sourcebuffer, while letting you retain full control.  For more
information, see further below.

More detailed information about the features and usage of AUCTeX can be
found in the AUCTeX manual.  You can access it from within Emacs by
typing 'C-h i d m auctex <RET>'.  If you prefer the standalone info
reader, issue the command 'info auctex' in a terminal.

AUCTeX is written entirely in Emacs Lisp, and hence you can easily add
new features for your own needs.  It is a GNU project and distributed
under the 'GNU General Public License Version 3'.

The most recent version is always available at
<http://ftp.gnu.org/pub/gnu/auctex/>.

WWW users may want to check out the AUCTeX page at
<http://www.gnu.org/software/auctex/>.

For comprehensive information about how to install AUCTeX read the file
'INSTALL' or 'INSTALL.windows', respectively.

If you are considering upgrading AUCTeX, the recent changes are
described in the 'CHANGES' file.

If you want to discuss AUCTeX with other users or its developers, there
are several mailing lists you can use.

Send a mail with the subject "subscribe" to <auctex-request@gnu.org> in
order to join the general discussion list for AUCTeX.  Articles should
be sent to <auctex@gnu.org>.  In a similar way, you can subscribe to the
<info-auctex@gnu.org> list for just getting important announcements
about AUCTeX.  The list <bug-auctex@gnu.org> is for bug reports which
you should usually file with the 'M-x TeX-submit-bug-report <RET>'
command.  If you want to address the developers of AUCTeX themselves
with technical issues, they can be found on the discussion list
<auctex-devel@gnu.org>.
preview-latex in a nutshell
***************************

Does your neck hurt from turning between previewer windows and the
source too often?  This AUCTeX component will render your displayed
LaTeX equations right into the editing window where they belong.

The purpose of preview-latex is to embed LaTeX environments such as
display math or figures into the source buffers and switch conveniently
between source and image representation.

1 What use is it?
*****************

WYSIWYG (what you see is what you get) sometimes is considered all the
rage, sometimes frowned upon.  Do we really want it?  Wrong question.
The right question is _what_ we want from it.  Except when finetuning
the layout, we don't want to use printer fonts for on-screen text
editing.  The low resolution and contrast of a computer screen render
all but the coarsest printer fonts (those for low-quality newsprint)
unappealing, and the margins and pagination of the print are not wanted
on the screen, either.  On the other hand, more complex visual
compositions like math formulas and tables can't easily be taken in when
seen only in the source.  preview-latex strikes a balance: it only uses
graphic renditions of the output for certain, configurable constructs,
does this only when told, and then right in the source code.  Switching
back and forth between the source and preview is easy and natural and
can be done for each image independently.  Behind the scenes of
preview-latex, a sophisticated framework of other programs like
'dvipng', Dvips and Ghostscript are employed together with a special
LaTeX style file for extracting the material of interest in the
background and providing fast interactive response.

2 Activating preview-latex
**************************

After installation, the package may need to be activated (and remember
to activate AUCTeX too).  In XEmacs, and in any prepackaged versions
worth their salt, activation should be automatic upon installation.  If
this seems not the case, complain to your installation provider.

The usual activation (if it is not done automatically) would be

     (load "preview-latex.el" nil t t)

If you still don't get a "Preview" menu in LaTeX mode in spite of AUCTeX
showing its "Command", your installation is broken.  One possible cause
are duplicate Lisp files that might be detectable with '<M-x>
list-load-path-shadows <RET>'.

3 Getting started
*****************

Once activated, preview-latex and its documentation will be accessible
via its menus (note that preview-latex requires AUCTeX to be loaded).
When you have loaded a LaTeX document (a sample document 'circ.tex' is
included in the distribution, but most documents including math and/or
figures should do), you can use its menu or 'C-c C-p C-d' (for
'Preview/Document').  Previews will now be generated for various objects
in your document.  You can use the time to take a short look at the
other menu entries and key bindings in the 'Preview' menu.  You'll see
the previewed objects change into a roadworks sign when preview-latex
has determined just what it is going to preview.  Note that you can
freely navigate the buffer while this is going on.  When the process is
finished you will see the objects typeset in your buffer.

It is a bad idea, however, to edit the buffer before the roadworks signs
appear, since that is the moment when the correlation between the
original text and the buffer locations gets established.  If the buffer
changes before that point of time, the previews will not be placed where
they belong.  If you do want to change some obvious error you just
spotted, we recommend you stop the background process by pressing 'C-c
C-k'.

To see/edit the LaTeX code for a specific object, put the point (the
cursor) on it and press 'C-c C-p C-p' (for 'Preview/at point').  It will
also do to click with the middle mouse button on the preview.  Now you
can edit the code, and generate a new preview by again pressing 'C-c C-p
C-p' (or by clicking with the middle mouse button on the icon before the
edited text).

If you are using the 'desktop' package, previews will remain from one
session to the next as long as you don't kill your buffer.  If you are
using XEmacs, you will probably need to upgrade the package to the
newest one; things are being fixed just as I am writing this.

4 Basic modes of operation
**************************

preview-latex has a number of methods for generating its graphics.  Its
default operation is equivalent to using the 'LaTeX' command from
AUCTeX.  If this happens to be a call of PDFLaTeX generating PDF output
(you need at least AUCTeX 11.51 for this), then Ghostscript will be
called directly on the resulting PDF file.  If a DVI file gets produced,
first Dvips and then Ghostscript get called by default.

The image type to be generated by Ghostscript can be configured with

     M-x customize-variable RET preview-image-type RET

The default is 'png' (the most efficient image type).  A special setting
is 'dvipng' in case you have the 'dvipng' program installed.  In this
case, 'dvipng' will be used for converting DVI files and Ghostscript
(with a 'PNG' device) for converting PDF files.  'dvipng' is much faster
than the combination of Dvips and Ghostscript.  You can get downloads,
access to its CVS archive and further information from its project site
(http://savannah.nongnu.org/projects/dvipng).

5 More documentation
********************

After the installation, documentation in the form of an info manual will
be available.  You can access it with the standalone info reader with

     info preview-latex

or by pressing 'C-h i d m preview-latex <RET>' in Emacs.  Once
preview-latex is activated, you can instead use 'C-c C-p <TAB>' (or the
menu entry 'Preview/Read documentation').

Depending on your installation, a printable manual may also be available
in the form of 'preview-latex.dvi' or 'preview-latex.ps'.

Detailed documentation for the LaTeX style used for extracting the
preview images is placed in 'preview.dvi' in a suitable directory during
installation; on typical teTeX-based systems,

     texdoc preview

will display it.

6 Availability
**************

The preview-latex project is now part of AUCTeX and accessible as part
of the AUCTeX project page (http://savannah.gnu.org/projects/auctex).
You can get its files from the AUCTeX download area
(ftp://ftp.gnu.org/pub/gnu/auctex).  As of AUCTeX 11.81, preview-latex
should already be integrated into AUCTeX, so no separate download will
be necessary.

You will also find '.rpm' files there for Fedora and possibly SuSE.
Anonymous CVS is available as well.

7 Contacts
**********

Bug reports should be sent by using 'M-x preview-report-bug <RET>', as
this will fill in a lot of information interesting to us.  If the
installation fails (but this should be a rare event), report bugs to
<bug-auctex@gnu.org>.

There is a general discussion list for AUCTeX which also covers
preview-latex, look at <http://lists.gnu.org/mailman/listinfo/auctex>.
For more information on the mailing list, send a message with just the
word "help" as subject or body to <auctex-request@gnu.org>.  For the
developers, there is the <auctex-devel@gnu.org> list; it would probably
make sense to direct feature requests and questions about internal
details there.  There is a low-volume read-only announcement list
available to which you can subscribe by sending a mail with "subscribe"
in the subject to <info-auctex-request@gnu.org>.

Offers to support further development will be appreciated.  If you want
to show your appreciation with a donation to the main developer, you can
do so via PayPal to <dak@gnu.org>, and of course you can arrange for
service contracts or for added functionality.  Take a look at the 'TODO'
list for suggestions in that area.
