;;; bib-cite.el --- test
;; bib-cite.el - Display \cite, \ref or \label / Extract refs from BiBTeX file.

;; Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2001, 2003, 2004, 2005
;; Free Software Foundation

;; Author:    Peter S. Galbraith <psg@debian.org>
;; Created:   06 July 1994
;; Version:   3.28  (Feb 23 2005)
;; Keywords:  bibtex, cite, auctex, emacs, xemacs

;;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; LCD Archive Entry:
;; bib-cite|Peter Galbraith|GalbraithP@dfo-mpo.gc.ca|
;; Display \cite, \ref or \label / Extract refs from BiBTeX file.|
;; 21-May-1997|3.01|~/misc/bib-cite.el.gz|

;; ----------------------------------------------------------------------------
;;; Commentary:
;; This minor-mode is used in various TeX modes to display or edit references
;; associated with \cite commands, or matching \ref and \label commands.

;; New versions of this package (if they exist) may be found at:
;;   http://people.debian.org/~psg/elisp/bib-cite.el
;; and in AUCTeX's Git archive at
;;   http://git.savannah.gnu.org/cgit/auctex.git

;; Operating Systems:
;;  Works in unix, DOS and OS/2.  Developped under Linux.

;; AUCTeX users:
;;  AUCTeX is a super-charged LaTeX mode for emacs. Get it at:
;;
;;    ftp://ftp.gnu.org/pub/gnu/auctex/
;;
;;  WWW users may want to check out the AUCTeX page at
;;    http://www.gnu.org/software/auctex/
;;
;;  bib-cite.el is included in the AUCTeX distribution.  Therefore, if
;;  you use AUCTeX and didn't obtained bib-cite.el separately, make sure
;;  that you are actually using the more recent version.

;; RefTeX users:
;;  RefTeX is a package with similar functions to bib-cite.
;;    http://www.astro.uva.nl/~dominik/Tools/reftex/
;;  RefTeX is bundled and preinstalled with Emacs since version 20.2.
;;  It was also bundled with XEmacs 19.16--20.x.
;;
;;  I suggest that you use RefTeX to help you type-in text as it's functions
;;  are better suited to this task than bib-cite, and use bib-cite's features
;;  when you proof-read the text.
;;  If you wish bib-cite to use RefTeX's reftex-view-crossref command to
;;  display and find \label's and \cite bibliography entries, set the variable
;;  bib-cite-use-reftex-view-crossref to t.

;; MS-DOS users:
;;  Multifile documents are supported by bib-cite by using etags (TAGS files)
;;  which contains a bug for MSDOS (at least for emacs 19.27 it does).
;;  Get the file
;;    http://people.debian.org/~psg/elisp/bib-cite.etags-bug-report
;;  to see what patches to make to etags.c to fix it.

;; Description:
;; ~~~~~~~~~~~
;; This package is used in various TeX modes to display or edit references
;; associated with \cite commands, or matching \eqref, \ref and \label
;; commands  (so I actually overstep BiBTeX bounds here...).
;;
;; These are the functions:
;;
;;    bib-display bib-display-mouse
;;                           - Display citation, \ref or \label under point
;;    bib-find    bib-find-mouse
;;                           - Edit citation, \ref or \label under point
;;    bib-find-next          - Find next occurrence of a \ref or \eqref
;;    bib-make-bibliography  - Make BiBTeX file containing only cite keys used.
;;    bib-apropos            - Search BiBTeX source files for keywords.
;;    bib-etags              - Refreshes (or builds) the TAGS files for
;;                             multi-file documents.
;;    bib-create-auto-file   - Used in bibtex-mode to create cite key
;;                             completion .el file for AUCTeX.
;;    bib-highlight-mouse    - Highlight \cite, \ref and \label commands in
;;                             green when the mouse is over them.

;; About Cite Commands and related functions:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  Various flavors of \cite commands are allowed (as long as they contain
;;  the word `cite') and they may optionally have bracketed [] options.
;;  Bibtex Cross-references are displayed, and @string abbreviations are
;;  substituted or included.
;;
;;  The \cite text is found (by emacs) in the bibtex source files listed in the
;;  \bibliography command.  The BiBTeX files can be located in a search path
;;  defined by an environment variable (typically BIBINPUTS, but you can change
;;  this).
;;
;;  All citations used in a buffer can also be listed in a new bibtex buffer by
;;  using bib-make-bibliography.  This is useful to make a bibtex file for a
;;  document from a large bibtex database.  In this case, cross-references are
;;  included, as well as the @string commands used. The @string abbreviations
;;  are not substituted.
;;
;;  The bibtex files can also be searched for entries matching a regular
;;  expression using bib-apropos.

;; Usage instructions:
;; ~~~~~~~~~~~~~~~~~~
;;  bib-display   Bound to Mouse-3 when specially highlighted.
;;                In Hyperbole, bound to the Assist key.
;;                Bound to `\C-c b d'
;;
;;   bib-display will show the bibtex entry or the corresponding label or
;;   ref commands from anywhere within a document.
;;    With cursor on the \cite command itslef
;;        -> display all citations of the cite command from the BiBTeX source.
;;    With cursor on a particular cite key within the brackets
;;        -> display that citation's text from the BiBTeX source file(s).
;;
;;       Example:
;;
;;       \cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
;;         ^Cursor -> Display-all-citations    ^Cursor -> Display-this-citation
;;
;;    With cursor on a \label command
;;        -> Display first matching \ref command in the document
;;    With cursor on a \ref command
;;        -> Display environment associated with the matching \label command.
;;
;;   Finding a ref or label within a multi-file document requires a TAGS file,
;;   which is automatically generated for you.  This enables you to then use
;;   any tags related emacs features.
;;
;;  bib-find      Bound to Mouse-2 when specially highlighted.
;;                In Hyperbole, bound to the Action key.
;;                Bound to `\C-c b f'
;;
;;    bib-find will select the buffer and move point to the BiBTeX source file
;;    at the proper citation for a cite command, or move point to anywhere
;;    within a document for a label or ref command.  The ref chosen is the
;;    first occurrance within a document (using a TAGS file).  If point is
;;    moved within the same buffer, mark is set before the move and a message
;;    stating so is given.  If point is moved to another file, this is done in
;;    a new window using tag functions.
;;
;;    The next occurrence of a \ref or \eqref command may be found by invoking
;;    bib-find-next, usually bound to `C-c b n'.
;;
;;    For multi-file documents, you must be using AUCTeX (so that bib-cite can
;;    find the master file) and all \input and \include commands must be first
;;    on a line (not preceeded by any non-white text).
;;
;;  bib-make-bibliography:     Bound to `\C-c b m'
;;
;;   Extract citations used in the current document from the \bibliography{}
;;   file(s).  Put them into a new suitably-named buffer.  In a AUCTeX
;;   multi-file document, the .aux files are used to find the cite keys (for
;;   speed).  You will be warned if these are out of date.
;;
;;   This buffer is not saved to a file.  It is your job to save it to whatever
;;   name you wish.  Note that AUCTeX has a unique name space for LaTeX and
;;   BiBTeX files, so you should *not* name the bib file associated with
;;   example.tex as example.bib!  Rather, name it something like
;;   example-bib.bib.
;;
;;  bib-apropos:               Bound to `\C-c b a'
;;
;;   Searches the \bibliography{} file(s) for entries containing a keyword and
;;   display them in the *help* buffer.  You can trim down your search by using
;;   bib-apropos in the *Help* buffer after the first invocation.  the current
;;   buffer is also searched for keyword matches if it is in bibtex-mode.
;;
;;   It doesn't display cross-references nor does it substitute or display
;;   @string commands used.  It could easily be added, but it's faster this
;;   way.  Drop me a line if this would be a useful addition.
;;
;;   If you find yourself entering a cite command and have forgotten which key
;;   you want, but have entered a few initial characters as in `\cite{Gal',
;;   then invoke bib-apropos.  It will take that string (in this case `Gal') as
;;   an initial response to the apropos prompt.  You are free to edit it, or
;;   simply press carriage return.
;;
;;  bib-etags:                 Bound to `\C-c b e'
;;
;;   Creates a TAGS file for AUCTeX's multi-file document (or refreshes it).
;;   This is used by bib-find when editing multi-file documents.  The TAGS file
;;   is created automatically, but it isn't refreshed automatically.  So if
;;   bib-find can't find something, try running bib-etags again.
;;
;;  bib-create-auto-file:
;;
;;   Use this when editing a BiBTeX buffer to generate the AUCTeX .el file
;;   which tell emacs about all its cite keys.  I've added this command to
;;   bibtex-mode pull-down menu.
;;
;;  bib-highlight-mouse:       Bound to `\C-c b h'
;;
;;   Highlights \cite, \ref and \label commands in green when the mouse is over
;;   them.  By default, a call to this function is added to LaTeX-mode-hook
;;   (via bib-cite-initialize) if you set bib-highlight-mouse-t to true.  But
;;   you may want to run this command to refresh the highlighting for newly
;;   edited text.

;; Installation instructions:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~
;;  bib-cite is a minor-mode, so you could invoke it in a LaTeX-mode hook.
;;  e.g. If you are using AUCTeX (http://www.gnu.org/software/auctex/), you
;;  could use:
;;
;;   (autoload 'turn-on-bib-cite "bib-cite")
;;   (add-hook 'LaTeX-mode-hook 'turn-on-bib-cite)
;;
;;  If you are using Emacs' regular LaTeX-mode, use instead:
;;
;;   (autoload 'turn-on-bib-cite "bib-cite")
;;   (add-hook 'latex-mode-hook 'turn-on-bib-cite)
;;
;;  bib-cite can be used with AUCTeX, or stand-alone.  If used with AUCTeX on a
;;  multi-file document (and AUCTeX's parsing is used), then all \bibliography
;;  commands in the document will be found and used.
;;  ---
;;  The following variable can be unset (like shown) to tell bib-cite to
;;  not give advice messages about which commands to use to find the next
;;  occurrence of a search:
;;
;;    (setq bib-novice nil)
;;  ---
;;  If you wish bib-cite to use RefTeX's reftex-view-crossref command to
;;  display and find \label's and \cite bibliography entries, set the variable
;;  bib-cite-use-reftex-view-crossref to t:
;;
;;    (setq bib-cite-use-reftex-view-crossref t)
;;  ---
;;  The following variable determines whether we will attempt to highlight
;;  citation, ref and label commands in green when they are under the
;;  mouse.  When highlighted, the mouse keys work to call bib-display
;;  (bound to [mouse-3]) and bib-find (bound to [mouse-2]).  If you use a
;;  mode other than LaTeX-mode, you'll want to call bib-highlight-mouse with
;;  a hook (See how we do this at the end of this file with the add-hook
;;  command).
;;
;;    (setq bib-highlight-mouse-t nil)
;;  ---
;;  The variable bib-switch-to-buffer-function sets the function used to
;;  select buffers (if they differ from the original) in bib-cite commands
;;  bib-make-bibliography, bib-display, bib-find
;;  You may use `switch-to-buffer' `switch-to-buffer-other-window' or
;;  `switch-to-buffer-other-frame'.
;;  ---
;;  If you use DOS or OS/2, you may have to set the following variable:
;;
;;    (setq bib-dos-or-os2-variable t)
;;
;;    if bib-cite.el fails to determine that you are using DOS or OS/2.
;;  Try `C-h v bib-dos-or-os2-variable' to see if it needs to be set manually.
;;  ---
;;  bib-cite needs to call the etags program with its output file option
;;  and also with the append option (usually -a).
;;  I figured that DOS and OS/2 would use "etags /o=" instead of the unix
;;  variant "etags -o ", but users have reported differently.  So while the
;;  unix notation is used here, you can reset it if you need to like so:
;;
;;    (setq bib-etags-command  "etags  /r='/.*\\\(eq\|page\|[fvF]\)ref.*/' /o=")
;;    (setq bib-etags-append-command
;;                             "etags  /r='/.*\\\(eq\|page\|[fvF]\)ref.*/' /a /o=")
;;  ---
;;  For multi-file documents, a TAGS file is generated by etags.
;;  By default, its name is TAGS.  You can change this like so:
;;
;;    (setq bib-etags-filename "TAGSLaTeX")
;;  ---
;;  If your environment variable to find BiBTeX files is not BIBINPUTS, then
;;  reset it with the following variable (here, assuming it's TEXBIB instead):
;;
;;    (setq bib-bibtex-env-variable "TEXBIB")
;;
;;  Note that any directory ending in a double slash will cause bib-cite to
;;  search recursively through subdirectories for your .bib files.  This can
;;  be slow, so use this judiciously.
;;  e.g.  setenv BIBINPUTS .:/home/psg/LaTeX/bibinputs//
;;        -> all directories below /home/psg/LaTeX/bibinputs/ will be
;;           searched.
;;
;;  If your bibtex setup works but Emacs can't see the environment variable
;;  correctly (Check `C-h v process-environment'), then customize the
;;  variable `bib-cite-inputs'  (e.g. `M-x customize-variable bib-cite-imputs')
;;
;;  ---
;;  If you do not wish bib-display to substitute @string abbreviations,
;;  then set the following variable like so:
;;
;;    (setq bib-substitute-string-in-display nil)
;;  ---
;;  Warnings are given when @string abbreviations are not defined in your bib
;;  files.  The exception is for months, usually defined in style files. If you
;;  use other definitions in styles file (e.g. journals), then you may add them
;;  to the `bib-substitute-string-in-display' list variable.

;; If you find circumstances in which this package fails, please let me know.

;; Things for me to do in later versions:
;; - treat @Strings correctly, not just in isolation.
;; - use  `kpsewhich -expand-path='$BIBINPUTS'`  instead of BIBINPUTS.
;; - jmv@di.uminho.pt (Jose Manuel Valenca) wants:
;;   - prompt for \cite as well as \label and \ref
;;     (and use AUCTeX's completion list)
;;   - implement string concatenation, with #[ \t\n]*STRING_NAME
;;   - Create new command to substitute @string text in any bibtex buffer.
;; ----------------------------------------------------------------------------
;;; Change log:
;; V3.28 Feb 23 2005 - Ralf Angeli
;;  - Some doc fixes in the commentary section.
;; V3.27 Feb 09 2005 - PSG
;;  - Patch from Peter Heslin.  TeX-master can now have symbol values.
;; V3.26 Aug 06 2004 - Reiner Steib
;;  - Changed URL of AUCTeX. Use "AUCTeX", not "auc-tex" (skipped Change log).
;; V3.25 Feb 15 2004 - PSG
;;  - Check existence of font-lock-unset-defaults; no longer defined in CVS
;;    Emacs. Thanks to Adrian Lanz for reporting the problem.
;; V3.24 Oct 28 2003 - PSG
;;  - bib-cite-file-directory-p: new function to replace ff-paths code.
;; V3.23 Oct 09 2003 - PSG
;;  - some checkdoc cleanup; not yet complete.
;; V3.22 Sep 17 2003 - PSG
;;  - bib-cite-aux-inputs:  new defcustom.
;;  - minor cleanup for `match-string'.
;; V3.21 Sep 08 2003 - PSG
;;  - Ripping out off-topic imenu code.
;; V3.20 Aug 14 2003 - PSG
;;  - psg-checkfor-file-list: Allow for relative directoties as entries in
;;    BIBINPUTS.
;;  - bib-cite-inputs:  new defcustom equivalent to BIBINPUTS.
;;  - bib-label-help-echo-format: fixed defcustom.
;;  - psg-list-env: code cleanup.
;;  - trailing whitespace cleanup.
;; V3.19 Apr 06 2003 - PSG
;;    Remove code that ran when defcustom not present.
;;    Remove hilit19 obsolete code.
;; V3.18 Mar 27 2003 - Bruce Ravel <ravel@phys.washington.edu>
;;    Play well with the varioref and fancyref latex styles (vref, fref, Fref).
;; V3.17 May 01 2001 - (RCS V1.38)
;;  - XEmacs has imenu after all.
;; V3.16 Dec 20 99 - (RCS V1.37)
;;  - Added customize support.
;; V3.15 Dec 20 99 - (RCS V1.36)
;;  - Removed stupid debugging code that I had left in.
;; V3.14 Dec 20 99 -
;;  - New variable bib-ref-regexp for \ref regexp to match \label constructs
;;    and added \pageref. (RCS V1.34)
;;  - Edited bib-etags-command snd bib-etags-append-command to match.
;; V3.13 Dec 20 99 - (RCS V1.32)
;;  - License changed to GPL.
;;  - Kai Engelhardt <ke@socs.uts.edu.au> bib-master-file takes .ltx extension
;;  - imenu--create-LaTeX-index-for-document and bib-document-TeX-files
;;    edited to accept .ltx extension.
;;  - Michael Steiner <steiner@cs.uni-sb.de> added journals to @string
;;    abbrevs and contributed `member-cis' to complaces @strings in a
;;    case-insensitive manner.
;; V3.12 Dec 10 98 - Bruce Ravel <bruce.ravel@nist.gov> (RCS V1.30)
;;    Fixed bib-label-help.
;; V3.11 Oct 06 98 - PSG (RCS V1.29)
;;    Quote \ character fot replace-match;
;;    Applies to: @String{JGR = "J. Geophys.\ Res."}
;; V3.10 Sep 21 98 - Matt Hodges <mph1002@cam.ac.uk> (RCS V1.28)
;;    Removed instance of expand-file-name due to new behaviour in Emacs-20.3.
;; V3.09 Sep 09 98 - PSG (RCS V1.27)
;;    Added support for \eqref; Added bib-find-next.
;; V3.08 Aug 20 98 - PSG (RCS V1.26)
;;    Fixed imenu bug (prev-pos (point-max))
;; V3.07 Nov 20 97 - Christoph Wedler <wedler@fmi.uni-passau.de>  (RCS V1.24)
;;    bib-ext-list variable made permanent-local, otherwise VC registration
;;    would use two extents for each reference etc. This was not a visible bug.
;; V3.06 Nov 12 97 - PSG (RCS V1.23)
;;    Support use of reftex's reftex-view-crossref command.
;; V3.05 Nov 12 97 - Vladimir Alexiev <vladimir@cs.ualberta.ca> (RCS V1.22)
;;    regexp-quote the bibliography keys so a key like galbraith+kelley97 works
;; V3.04 Aug 25 97 - Christoph Wedler  <wedler@fmi.uni-passau.de> (RCS V1.20)
;;    (bib-highlight-mouse): Would bug out on detached extents,
;;    e.g. when killing a whole citation.
;; V3.03 Jul 16 97 - Christoph Wedler  <wedler@fmi.uni-passau.de> (RCS V1.18)
;;    turn-on-bib-cite back to non-interactive.
;; V3.02 Jul 11 97 - Christoph Wedler  <wedler@fmi.uni-passau.de> (RCS V1.17)
;;    * auctex/bib-cite.el (turn-on-bib-cite): Make interactive.
;;    Argument to `bib-cite-minor-mode' is 1.
;;    (bib-label-help-echo-format): New variable.
;;    (bib-label-help-echo): New function.
;;    (bib-label-help): Addition argument format.
;;    (bib-highlight-mouse): Set extent property `help-echo' for XEmacs.
;; V3.01 May 22 97 - Diego Calvanese <calvanes@dis.uniroma1.it> (RCS V1.16)
;;    bib-make-bibliography handles commas separated citations in aux files.
;; V3.00 May 16 97 - Peter Galbraith (RCS V1.15)
;;    bib-cite is now a minor mode.
;; V2.32 Apr 30 97 - Anders Stenman <stenman@isy.liu.se> (RCS V1.14)
;;  - Support for balloon-help.
;; V2.31 Mar 20 97 - Christoph Wedler <wedler@fmi.uni-passau.de> (RCS V1.12)
;;  - Better fontification of help buffer as bibtex or latex for XEmacs.
;; V2.30 Feb 10 97 - Peter Galbraith (RCS V1.11)
;;  - Better fontification of help buffer as bibtex or latex.
;; V2.29 Jan 29 97 - Peter Galbraith (RCS V1.10)
;;  - imenu looks for `\label{stuff}' instead of `\label'
;; V2.28 Jan 22 97 - Peter Galbraith (RCS V1.9)
;;  - Bug in bib-create-auto-file.
;; V2.27 Dec 31 96 - Peter Galbraith (RCS V1.8)
;;  - allow spaces between cite keys.
;;  - Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;     Allow () delimiters as well as {}.
;;     Better check on bibtex-menu
;;     Erase *bibtex-bibliography* buffer.
;; V2.26 Sep 24 96 - Peter Galbraith (RCS V1.7)
;;  imenu bug fix.
;; V2.25 Sep 23 96 - Anders Stenman <stenman@isy.liu.se> (RCS V1.6)
;;  XEmacs bib-cite-fontify-help-as-latex bug fix.
;; V2.24 Aug 19 96 - Peter Galbraith (RCS V1.3)
;;  XEmacs bug fix, minor defvars - Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; V2.23 Aug 13 96 - Peter Galbraith (RCS V1.2)
;;   XEmacs - Add bib-cite entries to bibtex-mode popup menu.
;; V2.22 July 22 96 - Peter Galbraith (RCS V1.1)
;;   local-map has `m' for bib-make-bibliography instead of `b'
;;   set-buffer-menubar in XEmacs so that menu disappears after use.
;; V2.21 July 12 96 - Peter Galbraith
;;   Define `\C-c b' keymap for both plain tex and auctex, in XEmacs and emacs.
;;   Separate menu-bar menu in gnu emacs.
;;   font-lock support for bib-display'ed citations (bibtex fontification)
;;    and for matching \ref{} and \labels (latex fontification).
;;   buffer-substring-no-properties in bib-apropos
;;    (bug in completing-read with mouse faces)
;;   imenu-sort-function made local and nil.
;;   imenu--LaTeX-name-and-position fixed for section name containing "\"
;;   Various other things... (whitespace within label strings, etc...)
;; V2.20 June 25 96 - Peter Galbraith
;;   imenu fixed for emacs-19.31.
;; V2.19 May 13 96
;;  PSG:
;;  - @string substitution fixed; bib-edit-citation fixed when buffer exists;
;;  Christoph Wedler <wedler@fmi.uni-passau.de>:
;;  - Added bib-switch-to-buffer-function
;;  - (setq tags-always-exact nil) for xemacs
;;  - removed eval-after-load foe xemacs
;; V2.18 May 06 96 - PSG
;;   New eval-after-load from Fred Devernay <Frederic.Devernay@sophia.inria.fr>
;; V2.17 May 03 96 - PSG
;;   Fixed bug introduced in V2.16, reported by Dennis Dams <wsindd@win.tue.nl>
;; V2.16 May 02 96 - Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; - somewhat compatible with Hyperbole by binding bib-find and bib-display to
;;   the Action and Assist keys inside the bib-highlight-mouse-keymap.
;; - makes more liberal provisions for users with a tty.
;; V2.15 Apr 09 96 -
;; - fix "Buffer read-only" error caused by mouse-face text properties
;;   patch by Piet van Oostrum <piet@cs.ruu.nl>
;; - Use tmm non-X menu, patch by Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; - input{file.txt} would not work.
;;   bug report: David Kastrup <dak@pool.informatik.rwth-aachen.de>
;; V2.14 Feb 26 96 - PSG - define eval-after-load for xemacs
;; Frederic Devernay's <Frederic.Devernay@sophia.inria.fr> suggestion.
;; V2.13 Feb 08 96 - Peter Galbraith - Fixed recursive use of bib-apropos.
;; V2.12 Jan 19 96 - Peter Galbraith
;;   emacs-19.30's [down-mouse-1] is defined (rather than [mouse-1]), so
;;   bib-highlight-mouse-keymap now has [down-mouse-1] defined to override it.
;; V2.11  Nov 21 95 - Peter Galbraith
;; - Fixed bib-create-auto-file when bib file loaded before LaTeX file.
;; - Michal Mnuk's better imenu labels menu <Michal.Mnuk@risc.uni-linz.ac.at>
;; - [mouse-1] and [mouse-2] key defs for highlighted regions.
;; - Improve X menus.
;; - Skip over style files in bib-document-TeX-files.
;; - Add menus and mouse highlighting for xemacs
;;   Anders Stenman <stenman@isy.liu.se> Dima Barsky <D.Barsky@ee.surrey.ac.uk>
;; - Check bib-use-imenu before calling LaTeX-hook-setq-imenu.
;;   From: Kurt Hornik <hornik@ci.tuwien.ac.at>
;; - Remove mouse face properties before inserting new ones.
;;   From: Peter Whaite <peta@Whippet.McRCIM.McGill.EDU>
;; V2.10  Aug 17 95 - Peter Galbraith - fatal bugs in bib-make-bibliography.
;; V2.09  Jul 19 95 - Peter Galbraith
;;   - Had introduced bug in search-directory-tree. synced with ff-paths.el.
;; V2.08  Jul 13 95 - Peter Galbraith
;;     Fred Douglis <douglis@research.att.com> says etags should be required
;; V2.07  Jul 04 95 - Peter Galbraith
;;   - Minor changes with filename manipulations (careful with DOS...)
;;   - Problem if auc-tex not already loaded -> LaTeX-mode-map
;; V2.06  Jul 03 95 - Peter Galbraith - Added recursion through BIBINPUTS path.
;; V2.05  Jun 22 95 - Peter Galbraith  Bug: Hanno Wirth <wirth@jake.igd.fhg.de>
;;   bib-get-citations would truncate @String{KEY ="J. {\"u} Res."}
;; V2.04  Jun 19 95 - Peter Galbraith -
;;   - use bibtex-mode syntax table in bib buffer, else bib-apropos truncates
;;     an article if it contains an unbalanced closing parenthesis.
;;   - bib-highlight-mouse would mark a buffer modified
;; V2.03  May 16 95 - Peter Galbraith -
;;   auc-tec menu compatible with old "AUC TeX" pull-down name
;; V2.02  May 10 95 - Peter Galbraith -
;;   bug report by Bodo Huckestein <bh@thp.Uni-Koeln.DE> (getenv env) under DOS
;; V2.01  Mar 27 95 - Peter Galbraith - No imenu on xemacs; check BIBINPUT also
;; V2.00  Mar 27 95 - Peter Galbraith
;;   - bib-find and bib-display replace bib-edit-citation and
;;      bib-display-citation
;;   - bib-apropos now take initial guess from start of cite argument at point.
;;   - Multi-file support for bib-make-bibliography using .aux files.
;;   - \label and \ref functionality for bib-find and bib-display:
;;     - \label may appear within an \begin\end or to label a (sub-)section.
;;     - Cursor on \label, goto first \ref, set next i-search to pattern.
;;     - Cursor on \ref, goto \label or display it's environment or section.
;;     - Works on hidden code!
;; V1.08  Jan 16 95 - Peter Galbraith
;;     bib-apropos can be used within *Help* buffer to trim a search.
;; V1.07  Dec 13 94 - Peter Galbraith
;;   - Fixed: multi-line @string commands in non-inserted display.
;;   - Fixed: quoted \ character in @string commands.
;;   - BiBTeX comments should not affect bib-cite
;;   - Fixed bib-apropos (from Christoph Wedler <wedler@fmi.uni-passau.de>)
;;      Faster now, and avoids infinite loops.
;;   - Added bib-edit-citation to edit a bibtex files about current citation.
;;   - Allow space and newlines between citations: \cite{ entry1, entry2}
;;   - Added bib-substitute-string-in-display,  bib-string-ignored-warning
;;     and bib-string-regexp.
;;   - bib-display-citation (from Markus Stricker <stricki@vision.ee.ethz.ch>)
;;      Could not find entry with trailing spaces
;; V1.06  Nov 20 94 - Peter Galbraith
;;   - Fixed bib-apropos for:
;;        hilighting without invoking bibtex mode.
;;        display message when no matches found.
;;        would search only last bib file listed (forgot to `goto-char 1')
;;   - Fixed bib-make-bibliography that would only see first citation in a
;;     multi-key \cite command (found by Michail Rozman <roz@physik.uni-ulm.de>
;;   - bib-make-bibliography didn't see \cite[A-Z]* commands.
;;     Found by Richard Stanton <stanton@haas.berkeley.edu>
;;     **************************************************
;;   - * Completely rewritten code to support crossrefs *
;;     **************************************************
;;   - autodetection of OS/2 and DOS for bib-dos-or-os2-variable
;;   - Created bib-display-citation-mouse
;;   - bib-apropos works in bibtex-mode on the current buffer
;;   - bibtex entry may have comma on next line (!)
;;       @ARTICLE{Kiryati-91
;;         , YEAR          = {1991    }
;;         ...
;; V1.05  Nov 02 94 - Peter Galbraith
;;   - bug fix by rossmann@TI.Uni-Trier.DE (Jan Rossmann)
;;     for (boundp 'TeX-check-path) instead of fboundp.  Thanks!
;;   - Translate environment variable set by bib-bibtex-env-variable.
;;     (suggested by Richard Stanton <stanton@haas.berkeley.edu>)
;;   - add bib-dos-or-os2-variable to set environment variable path separator
;;   - Add key-defs for any tex-mode and auc-tex menu-bar entries.
;;       [in auc-tec TeX-mode-map is common to both TeX and LaTeX at startup
;;        (but TeX-mode-map is only copied to LaTeX-mode-map at initilisation)
;;        in plain emacs, use tex-mode-map for both TeX and LaTeX.]
;;   - Add key def for bibtex-mode to create auc-tex's parsing file.
;;   - Fix bugs found by <thompson@loon.econ.wisc.edu>
;;     - fix bib-get-citation for options
;;     - fix bib-get-citation for commas preceeded citation command
;;     - better regexp for citations and their keys.
;;     - Added @string support for any entry (not just journal entries).
;;       (I had to disallow numbers in @string keys because of years.
;;        Is that ok?)
;;   - added bib-apropos
;; V1.04  Oct 24 94 - Peter Galbraith
;;   - Don't require dired-aux, rather define the function we need from it.
;;   - Regexp-quote the re-search for keys.
;;   - Name the bib-make-bibliography buffer diffently than LaTeX buffer
;;     because auc-tex's parsing gets confused if same name base is used.
;; V1.03  Oct 24 94 - Peter Galbraith - require dired-aux for dired-split
;; V1.02  Oct 19 94 - Peter Galbraith
;;   - If using auc-tex with parsing activated, use auc-tex's functions
;;     to find all \bibliography files in a multi-file document.
;;   - Find bib files in pwd, BIBINPUTS environment variable path and
;;     TeX-check-path elisp variable path.
;;   - Have the parser ignore \bibliography that is on a commented `%' line.
;;     (patched by Karl Eichwalder <karl@pertron.central.de>)
;;   - Allow for spaces between entry type and key in bib files:
;;     (e.g  @Article{  key} )
;;     (suggested by Nathan E. Doss <doss@ERC.MsState.Edu>)
;;   - Allows options in \cite command (e.g. agu++ package \cite[e.g.][]{key})
;;   - Includes @String{} abbreviations for `journal' entries
;; V1.01 July 07 94 - Peter Galbraith - \bibliography command may have list of
;;                                      BibTeX files.  All must be readable.
;; V1.00 July 06 94 - Peter Galbraith - Created
;; ----------------------------------------------------------------------------
;;; Code:

(eval-when-compile (require 'cl))

(defgroup bib-cite nil
  "bib-cite, LaTeX minor-mode to display \\cite, \\ref and \\label commands."
  :group 'tex)

(defcustom bib-cite-use-reftex-view-crossref nil
  "*Non-nil means, RefTeX will be used to find cross references.
When this variable is non-nil, both `bib-find' and `bib-display' will
call a function in RefTeX to find or display the cross reference of a
\\ref or \\cite macro at point."
  :group 'bib-cite
  :type 'boolean)

(defcustom bib-novice t
  "*Give advice to novice users about what commands to use next."
  :group 'bib-cite
  :type 'boolean)

(defcustom bib-switch-to-buffer-function 'switch-to-buffer
  "*Function used to select buffers if they differ from the original.
You may use `switch-to-buffer' `switch-to-buffer-other-window' or
`switch-to-buffer-other-frame'."
  :group 'bib-cite
  :type '(choice (function-item switch-to-buffer)
		 (function-item switch-to-buffer-other-window)
		 (function-item switch-to-buffer-other-frame)))

(defcustom bib-highlight-mouse-t t
  "*Call bib-highlight-mouse from `LaTeX-mode-hook' to add green highlight."
  :group 'bib-cite
  :type 'boolean)

(defcustom bib-label-help-echo-format "button2 finds %s, button3 displays %s"
  "*Format string for info if the mouse is over LaTeX commands.
If nil, do not display info."
  :group 'bib-cite
  :type '(radio (const :tag "Don't display info" nil)
		 string))

(defcustom bib-bibtex-env-variable "BIBINPUTS"
  "*Environment variable setting the path where BiBTeX input files are found.
BiBTeX 0.99b manual says this should be TEXBIB.
Another version says it should BSTINPUTS.  I don't know anymore!

The colon character (:) is the default path separator in unix, but you may
use semi-colon (;) for DOS or OS/2 if you set bib-dos-or-os2-variable to t."
  :group 'bib-cite
  :type 'string)

(defcustom bib-cite-inputs nil
  "*List of directories to search for .bib files.
This is in addition to those listed in the environment variable specified by
`bib-bibtex-env-variable'."
  :group 'bib-cite
  :type '(repeat (file :format "%v")))

(defcustom bib-cite-aux-inputs nil
  "*List of directories to search for .aux files.
MiKTeX has the LaTeX option -aux-directory to store .aux files in an alternate
directory.  You may set this variable to let bib-cite find these .aux files."
  :group 'bib-cite
  :type '(repeat (file :format "%v")))

(defcustom bib-dos-or-os2-variable (or (equal 'emx system-type)
				       (equal 'ms-dos system-type))
  ;; Under OS/2 system-type equals emx
  ;; Under DOS  system-type equals ms-dos
  "*Whether you use DOS or OS/2 for bib-make-bibliography/bib-display.

It tells bib-make-bibliography and bib-display to translate
the BIBINPUTS environment variable using the \";\" character as
a path separator and to translate DOS' backslash to slash.

e.g. Use a path like \"c:\\emtex\\bibinput;c:\\latex\\bibinput\"

\(You can change the environment variable which is searched by setting the  elisp variable bib-bibtex-env-variable)"
  :group 'bib-cite
  :type 'boolean)

(defcustom bib-etags-command "etags -r '/.*\\\\\\(eq\\|page\\|[fvF]\\)ref.*/' -o "
  "*Variable for the etags command and its output option.
In unix, this is usually \"etags -r '/.*\\\(eq\|page\|[fvF]\)ref.*/' -o \"
\(we use the -r option to tell etags to list AMS-LaTeX's \\eqref command.)
In DOS and OS/2, this *may* be different, e.g. using slashes like \"etags /o=\"
If so, set it this variable."
  :group 'bib-cite
  :type 'string)

(defcustom bib-etags-append-command "etags -r '/.*\\\\\\(eq\\|page\\|[fvF]\\)ref.*/' -a -o "
  "*Variable for the etags command and its append and output option.
In unix, this is usually \"etags -r '/.*\\\(eq\|page\|[fvF]\)ref.*/' -a -o \"
In DOS and OS/2, this *may* be \"etags /a /o=\"  If so, set it this variable."
  :group 'bib-cite
  :type 'string)

(defcustom bib-etags-filename "TAGS"
   "*Variable for the filename generated by etags, by defaults this TAGS.
but you may want to change this to something like TAGSLaTeX such that it can
coexist with some other tags file in your master file directory."
  :group 'bib-cite
  :type 'string)

(defcustom bib-ref-regexp "\\\\\\(eq\\|page\\|[fvF]\\)?ref"
  "*Regular expression for \\ref LaTeX commands that have a matching \\label.
without the curly bracket.

If you change this variable and you use multi-file documents, make sure you
also edit the variables bib-etags-command and bib-etags-append-command."
  :group 'bib-cite
  :type 'regexp)

(defcustom bib-substitute-string-in-display t
  "*Determines if bib-display will substitute @string definitions.
If t, then the @string text is substituted.
If nil, the text is not substituted but the @string entry is included."
  :group 'bib-cite
  :type 'boolean)

(defcustom bib-string-ignored-warning
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "sept" "oct" "nov"
    "dec" "acmcs" "acta" "cacm" "ibmjrd" "ibmjs" "ieeese" "ieeetcad"
    "ieeetc" "ipl" "jacm" "jcss" "scp" "sicomp" "tcs" "tocs" "tods" "tog"
    "toms" "toois" "toplas" )
  "*@string abbreviations for which a warning is not given if not defined.
These are usually month abbreviations (or journals) defined in a style file."
  :group 'bib-cite
  :type '(repeat (string :tag "Entry")))

;;<<<<<<User-Modifiable variables end here.

(defvar bib-ref-regexpc (concat bib-ref-regexp "{")
  "*Regular expression for \\ref LaTeX commands that have a matching \\label.
A opening curly bracket is appended to the regexp.")

(defvar bib-cite-is-XEmacs
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

(defvar bib-cite-minor-mode nil)

(defvar bib-highlight-mouse-keymap (make-sparse-keymap)
  "Keymap for mouse bindings in highlighted texts in bicite.")

(defvar bib-ext-list nil
  "Xemacs buffer-local list of bib-cite extents.")
(make-variable-buffer-local 'bib-ext-list)
(put 'bib-ext-list 'permanent-local t)

(defvar bib-cite-minor-mode-menu nil)

;;;###autoload
(defun bib-cite-minor-mode (arg)
  "Toggle bib-cite mode.
When bib-cite mode is enabled, citations, labels and refs are highlighted
when the mouse is over them.  Clicking on these highlights with [mouse-2]
runs bib-find, and [mouse-3] runs bib-display."
  (interactive "P")
  (set (make-local-variable 'bib-cite-minor-mode)
       (if arg
	   (> (prefix-numeric-value arg) 0)
	 (not bib-cite-minor-mode)))
  (cond
   (bib-cite-minor-mode                 ;Setup the minor-mode
    ;; Christoph Wedler's <wedler@fmi.uni-passau.de> suggestion for xemacs
    ;; Added for version 2.19
    (if (boundp 'tags-always-exact)
	(progn
	  (set (make-local-variable 'tags-always-exact) nil)))
    ;; mouse overlay
    (if bib-highlight-mouse-t
	(progn
	  (bib-cite-setup-highlight-mouse-keymap)
	  (bib-highlight-mouse)
	  (when bib-cite-is-XEmacs
	    (make-local-hook 'after-change-functions))
	  (add-hook 'after-change-functions
		    'bib-cite-setup-mouse-function nil t)))
    (if bib-cite-is-XEmacs
	(progn
	  (or (local-variable-p 'current-menubar (current-buffer))
	      (set-buffer-menubar current-menubar))
	  (add-submenu nil bib-cite-minor-mode-menu))))
   (t
   ;;;Undo the minor-mode
    ;; mouse overlay
    (cond
     (bib-cite-is-XEmacs
      (while bib-ext-list
	(delete-extent (car bib-ext-list))
	(setq bib-ext-list (cdr bib-ext-list))))
     (t
      (remove-hook 'after-change-functions 'bib-cite-setup-mouse-function t)
      (let ((before-change-functions) (after-change-functions))
	;; FIXME This detroys all mouse-faces and local-maps!
	;; FIXME Hope no other package is using them in this buffer!
	(remove-text-properties (point-min) (point-max)
				'(mouse-face t local-map t)))))
    (if bib-cite-is-XEmacs
	(delete-menu-item '("BCite"))))))

;;This must be eval'ed when the LaTeX mode is in use.
;; bib-highlight-mouse-keymap is a local variable so each buffer can have it's
;; own.
(defun bib-cite-setup-highlight-mouse-keymap ()
  "Set up the bib-cite text in the current buffer to be clickable."
  (set (make-local-variable 'bib-highlight-mouse-keymap)
   ;;; First, copy the local keymap so we don't have `disappearing' menus
   ;;; when the mouse is moved over a \ref, \label or \cite command.

   ;;; FIXME: Check out (mouse-major-mode-menu) to see how it grabs the local
   ;;;        menus to display.  Maybe on `highlighted' commands we could only
   ;;;        display the bib-cite stuff (or a subset of it).
	(let ((m (copy-keymap (current-local-map))))
	  (cond
	   (bib-cite-is-XEmacs
	    (set-keymap-name m 'bib-highlight-mouse-keymap)
	    (cond
	     ;;action-key stuff from Vladimir Alexiev <vladimir@cs.ualberta.ca>
	     ((commandp 'action-key)
	      ;; for hyperbole. The Right Way is to define implicit buttons
	      ;; (defib) bib-cite and label-ref instead of overriding
	      ;; action-key and assist key, so that eg smart key help can
	      ;; be obtained, but I'm lazy.
	      (substitute-key-definition 'action-key 'bib-find m global-map)
	      (substitute-key-definition 'assist-key 'bib-display m global-map)
	      (substitute-key-definition 'action-key-depress
					 'bib-find-mouse m global-map)
	      (substitute-key-definition 'assist-key-depress
					 'bib-display-mouse m global-map)
	      (substitute-key-definition 'action-mouse-key nil m global-map)
	      (substitute-key-definition 'assist-mouse-key nil m global-map))
	     (t                               ; xemacs, not hyperbole
	      (define-key m "\e\r" 'bib-find-mouse) ;   bug Fixed in V2.17
	      (define-key m "\e\n" 'bib-display-mouse) ;bug Fixed in V2.17
	      ;;(define-key m [(shift button1)] 'bib-display-mouse)
	      (define-key m [button3] 'bib-display-mouse)
	      (define-key m [button2] 'bib-find-mouse))))
	    (t                                 ; emacs 19
	     (cond
	      ((commandp 'action-key)
	       (substitute-key-definition 'action-key 'bib-find m global-map)
	       (substitute-key-definition 'assist-key 'bib-display m global-map)
	       (substitute-key-definition 'action-mouse-key-emacs19
					  'bib-find-mouse m global-map)
	       (substitute-key-definition 'assist-mouse-key-emacs19
					  'bib-display-mouse m global-map)
	       (substitute-key-definition 'action-key-depress-emacs19
					  nil m global-map)
	       (substitute-key-definition 'assist-key-depress-emacs19
					  nil m global-map))
	      (t                               ; emacs 19, not hyperbole
	       (define-key m [down-mouse-3] 'bib-display-mouse)
	       (define-key m [mouse-2] 'bib-find-mouse)))))
	  m)))

;;;###autoload
(defun turn-on-bib-cite ()
  "Unconditionally turn on Bib Cite mode."
  (bib-cite-minor-mode 1))

(defun bib-cite-setup-mouse-function (beg end old-len)
  (save-excursion
    (save-match-data
      (save-restriction
	(narrow-to-region
	 (progn (goto-char beg) (beginning-of-line) (point))
	 (progn (goto-char end) (forward-line 1) (point)))
	(bib-highlight-mouse)))))

(defvar bib-cite-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cba" 'bib-apropos)
    (define-key map "\C-cbb" 'bib-make-bibliography)
    (define-key map "\C-cbd" 'bib-display)
    (define-key map "\C-cbe" 'bib-etags)
    (define-key map "\C-cbf" 'bib-find)
    (define-key map "\C-cbn" 'bib-find-next)
    (define-key map "\C-cbh" 'bib-highlight-mouse)
    map)
  "Bib-cite minor-mode keymap.")

(easy-menu-define
 bib-cite-minor-mode-menu bib-cite-minor-mode-map "Menu keymap for bib-cite."
 '("BCite"
   ["Make BibTeX bibliography buffer" bib-make-bibliography t]
   ["Display citation or matching \\ref or \\label" bib-display t]
   ["Find BibTeX citation or matching \\ref or \\label" bib-find t]
   ["Search apropos BibTeX files" bib-apropos t]
   ["Build TAGS file for multi-file document" bib-etags (bib-master-file)]
   ["Refresh \\cite, \\ref and \\label mouse highlight"
    bib-highlight-mouse t]))

;; Install ourselves:
(or (assq 'bib-cite-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(bib-cite-minor-mode " BCite") minor-mode-alist)))
(or (assq 'bib-cite-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'bib-cite-minor-mode bib-cite-minor-mode-map)
		minor-mode-map-alist)))


;;; Add a menu entry to bibtex.el (Perhaps I should not do this).
(cond
 ((and (string-match "XEmacs\\|Lucid" emacs-version)
       (or window-system
	   (fboundp 'smart-menu)))      ;text menus by Bob Weiner
  ;;
  ;; xemacs under X with AUCTeX
  ;;

  ;; Add to bibtex.el's popup menu
  (defvar bib-cite-xemacs-bibtex-mode-menu
    '("---"
      "Bib-Cite"
      "---"
      ["Search apropos BibTeX files" bib-apropos t]
      ["Create AUCTeX auto parsing file" bib-create-auto-file t])
    "Submenu of bibtex-mode menu, used by bib-cite.")

  (if (boundp 'bibtex-menu)
      ;; Add menu now
      (setq bibtex-menu
	    (append
	     bibtex-menu
	     bib-cite-xemacs-bibtex-mode-menu))
    ;; Setup to add menu later
    (defun bib-cite-bibtex-mode-hook ()
      (if (boundp 'bibtex-menu)
	  (progn
	    (setq bibtex-menu
		  (append
		   bibtex-menu
		   bib-cite-xemacs-bibtex-mode-menu))
	    (remove-hook 'bibtex-mode-hook 'bib-cite-bibtex-mode-hook))))
    (add-hook 'bibtex-mode-hook 'bib-cite-bibtex-mode-hook))
  )

 ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
       (string-equal "19" (substring emacs-version 0 2))
       (or window-system
	   (fboundp 'tmm-menubar)))     ; 19.30 - Will autoload if necessary
  ;;
  ;; emacs-19 under X-windows (or non-X with tmm)
  ;;

  ;; This *almost* makes me want to switch over to XEmacs...

  ;; to AUCTeX auto file for a bibtex buffer
  (eval-after-load
   "bibtex"
   '(progn
      (add-hook 'bibtex-mode-hook 'TeX-bibtex-set-BibTeX-dialect)
      (cond
       ((lookup-key bibtex-mode-map [menu-bar move/edit])
	(define-key-after
	  (lookup-key bibtex-mode-map [menu-bar move/edit])
	  [bib-nil] '("---" . nil) '"--")
	(define-key-after
	  (lookup-key bibtex-mode-map [menu-bar move/edit])
	  [bib-apropos] '("Search Apropos" . bib-apropos) 'bib-nil)
	(define-key-after
	  (lookup-key bibtex-mode-map [menu-bar move/edit])
	  [auc-tex-parse]
	  '("Create AUCTeX auto parsing file" . bib-create-auto-file)
	  'bib-apropos))
       ((lookup-key bibtex-mode-map [menu-bar bibtex-edit])
	(define-key-after
	  (lookup-key bibtex-mode-map [menu-bar bibtex-edit])
	  [bib-nil] '("---" . nil) '"--")
	(define-key-after
	  (lookup-key bibtex-mode-map [menu-bar bibtex-edit])
	  [bib-apropos] '("Search Apropos" . bib-apropos) 'bib-nil)
	(define-key-after
	  (lookup-key bibtex-mode-map [menu-bar bibtex-edit])
	  [auc-tex-parse]
	  '("Create AUCTeX auto parsing file" . bib-create-auto-file)
	  'bib-apropos)))))))

;; Following from bibtex.el
(defvar
  bib-cite-bibtex-font-lock-keywords
  '(("^\\( \\|\t\\)*\\(@[A-Za-z]+\\)[ \t]*[({]\\([][A-Za-z0-9.:;?!`'()/*@_+=|<>-]+\\)?"
     (2 font-lock-function-name-face)
     (3 font-lock-reference-face nil t))
    ;; reference type and reference label
    ("^[ \t]*\\(OPT[^\"#%'(),={} \t\n0-9][^\"#%'(),={} \t\n]*\\)[ \t]*="
     1 font-lock-comment-face)
    ;; optional field names (treated as comments)
    ("^[ \t]*\\([^\"#%'(),={} \t\n0-9][^\"#%'(),={} \t\n]*\\)[ \t]*="
     1 font-lock-variable-name-face)
    ;; field names
    "Default expressions to fontify in BibTeX mode."))

(defvar bib-cite-bibtex-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; [alarson:19920214.1004CST] make double quote a string quote
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?$ "$$  " st)
    (modify-syntax-entry ?% "<   " st)
    (modify-syntax-entry ?'  "w   " st)
    (modify-syntax-entry ?@  "w   " st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?~ " " st)
    st))
;; Code from bibtex.el ends

;; @string starts with a letter and does not contain any of ""#%'(),={}
;; Here we do not check that the field contains only one string field and
;; nothing else.
(defvar bib-string-regexp
      "^[, \t]*[a-zA-Z]+[ \t]*=[ \t]*\\([a-zA-Z][^#%'(),={}\" \t\n]*\\)"
      "Regular expression for field containing a @string.")

(defun bib-display ()
  "Display BibTeX citation or matching \\ref or \\label command under point.

If text under cursor is a \\cite command, then display its BibTeX info from
\\bibliography input file.
   Example with cursor located over cite command or arguments:
     \cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
	^Display-all-citations          ^Display-this-citation

If text under cursor is a \\ref command, then display environment associated
with its matching \\label command.

If text under cursor is a \\label command, then display the text around
the first matching \\ref command.

The user is prompted for a \\label or \\ref is nothing suitable is found under
the cursor.  The first prompt is for a label.  If you answer with an empty
string, a second prompt for a ref will be given.

A TAGS file is created and used for multi-file documents under auctex."
  (interactive)
  (let ((cite)(ref)(label))
    (save-excursion
      (if (not (looking-at "\\\\"))
	  (search-backward "\\" nil t))
      (if (looking-at bib-ref-regexpc)
	  (setq ref t)
	(if (looking-at "\\\\label{")
	    (setq label t)
	  (setq cite t))))
    (cond
     ;; reftex doesn't handle label->ref
     ((and bib-cite-use-reftex-view-crossref
	   (or ref cite))
      ;;;FIXME: reftex doesn't want point on \ref or \cite part, but on keyword
      (require 'reftex)
      (reftex-view-crossref nil))
     (cite
      (bib-display-citation))
     (t
      (bib-display-label)))))

(defun bib-find ()
  "Edit BibTeX citation or find matching \\ref or \\label command under point.

For multi-entry cite commands, the cursor should be on the actual cite key
desired (otherwise a random entry will be selected).
e.g.: \cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
			^Display-this-citation

If text under cursor is a \\ref command, then point is moved to its matching
\\label command.

If text under cursor is a \\label command, then point is moved to the first
matching \\ref command.

The user is prompted for a \\label or \\ref is nothing suitable is found under
the cursor.  The first prompt is for a label.  If you answer with an empty
string, a second prompt for a ref will be given.

A TAGS file is created and used for multi-file documents under auctex."
  (interactive)
  (let ((cite)(ref)(label))
    (save-excursion
      (if (not (looking-at "\\\\"))
	  (search-backward "\\" nil t))
      (if (looking-at bib-ref-regexpc)
	  (setq ref t)
	(if (looking-at "\\\\label{")
	    (setq label t)
	  (setq cite t))))
    (cond
     ;; reftex doesn't handle label->ref
     ((and bib-cite-use-reftex-view-crossref
	   (or ref cite))
      (require 'reftex)
      (reftex-view-crossref t))
     (cite
      (bib-edit-citation))
     (t
      (bib-find-label)))))

(defvar bib-cite-search-ring nil
  "Bib-cite intenal variable to hold last \\ref or \\eqref find.")

(defun bib-find-next (&optional prev-p)
  "Find next occurrence of a \ref or \eqref.
This is made necessary because we now use a regexp to find tags in multi-file
documents, and the Emacs command `find-tag' doesn't allow to interactively
find the next occurrence of a regexp."
  (interactive "P")
  (if (bib-master-file)                 ;Multi-file document
      (if prev-p
	  (find-tag t '- t)
	(find-tag t t t))
    (if bib-cite-search-ring
	;;FIXME: Should first make sure I move off initial \ref{}.
	(let ((regexp (concat bib-ref-regexpc bib-cite-search-ring "}")))
	  (if prev-p
	      (if (not (re-search-backward regexp nil t))
		  (message "No previous occurrence of reference %s"
			   bib-cite-search-ring))
	    (if (not (re-search-forward regexp nil t))
		(message "No next occurrence of reference %s"
			 bib-cite-search-ring))))
      (message "Sorry, no previous reference to find.  Use bib-find?"))))

(defun bib-display-mouse (EVENT)
  "Display BibTeX citation or matching \\ref or \\label under mouse EVENT.
See bib-display."
  (interactive "e")
  (mouse-set-point EVENT)
  (bib-display))

(defun bib-find-mouse (EVENT)
  "Edit BibTeX citation or find matching \\ref or \\label under mouse EVENT.
See bib-find."
  (interactive "e")
  (mouse-set-point EVENT)
  (bib-find))

(defun bib-apropos ()
  "Display BibTeX entries containing a keyword from bibliography file.
The files specified in the \\bibliography command are searched unless
the current buffer is in `bibtex-mode' or is the Help buffer.  In those
cases, *it* is searched.  This allows you to trim down a search further
by using bib-apropos sequentially."
  ;;(interactive "sBibTeX apropos: ")
  (interactive)
  (let* ((keylist (and (boundp 'TeX-auto-update) ;Avoid error in FRAMEPOP
		       (fboundp 'LaTeX-bibitem-list) ;Use this if using auctex
		       (LaTeX-bibitem-list)))
	 (keyword (bib-apropos-keyword-at-point))
	 (keyword (completing-read "BiBTeX apropos: " keylist nil nil keyword))
	 (the-text)(key-point)(start-point)
	 (new-buffer-f (and (not (string-match "^bib" mode-name))
			    (not (string-equal "*Help*" (buffer-name)))))
	 (bib-buffer (or (and new-buffer-f (bib-get-bibliography nil))
			 (current-buffer))))
    (with-current-buffer bib-buffer
      (goto-char (point-min))
      (while (and (re-search-forward "^[ \t]*@" nil t)
		  (re-search-forward keyword nil t))
	(setq key-point (point))        ;To make sure this is within entry
	(re-search-backward "^[ \t]*@" nil t)
	(setq start-point (point))
	(forward-list 1)
	(if (< (point) key-point)       ;And this is that test...
	    (goto-char key-point)       ;Not within entry, skip it.
	  (setq the-text
		(cons (concat (buffer-substring start-point (point)) "\n")
		      the-text))))
      (if (not the-text)
	  (message "Sorry, no matches found.")
	(with-output-to-temp-buffer "*Help*"
	  (mapcar 'princ (nreverse the-text)))
	(bib-cite-fontify-help-as-bibtex)
	(if bib-novice
	    (message
	     (substitute-command-keys
	      (concat "Use \\[bib-apropos] again in the *help* buffer"
		      " to trim the search")))))
      (if new-buffer-f
	  (kill-buffer bib-buffer)))))

(defvar bib-document-citekeys-obarray-warnings nil
  "Bib-cite internal variable.")

(defun bib-make-bibliography ()
  "Extract citations used in the current document from \bibliography{} file(s).
Put them into a buffer named after the current buffer, with extension .bib.

In an AUCTeX multi-file document, parsing must be on and the citation keys
are extracted from the .aux files.

In a plain LaTeX buffer (not multi-file), the cite keys are extracted from
the text itself.  Therefore the text need not have been previously processed
by LaTeX.

This function is useful when you want to share a LaTeX file, and therefore want
to create a bibtex file containing only the references used in the document."
  (interactive)
  (let* ((the-keys-obarray (or (bib-document-citekeys-obarray)
			       (bib-buffer-citekeys-obarray)))
					;1st in case of error
	 (new-buffer
	  (create-file-buffer
	   (concat (substring (buffer-name) 0
			      (or (string-match "\\." (buffer-name))
				  (length (buffer-name))))
		   "-bib.bib")))
	 (bib-buffer (bib-get-bibliography nil))
	 (the-warnings (bib-get-citations the-keys-obarray
					  bib-buffer
					  new-buffer
					  nil)))
    (kill-buffer bib-buffer)
;;; (switch-to-buffer new-buffer)
    (funcall bib-switch-to-buffer-function new-buffer)
    (bibtex-mode)
    (if (or bib-document-citekeys-obarray-warnings
	    the-warnings)
	(progn
	  (cond
	   ((and bib-document-citekeys-obarray-warnings the-warnings)
	    (with-output-to-temp-buffer "*Help*"
	      (princ bib-document-citekeys-obarray-warnings the-warnings)))
	   (bib-document-citekeys-obarray-warnings
	    (with-output-to-temp-buffer "*Help*"
	      (princ bib-document-citekeys-obarray-warnings)))
	   (the-warnings
	    (with-output-to-temp-buffer "*Help*" (princ the-warnings))))
	  (setq bib-document-citekeys-obarray-warnings nil) ;Reset
	  (bib-cite-fontify-red)))
    (if bib-novice
	(message
	 (substitute-command-keys
	  "Use \\[save-buffer] to save this buffer to a file.")))))

(defun bib-cite-fontify-red (&optional limit)
  "Fontify *Help* buffer in red-bold up to optional LIMIT."
  (if (and window-system                ;Not exactly correct for XEmacs
	   (not (facep 'red-bold)))
      (progn
	(copy-face 'bold 'red-bold)
	(set-face-foreground 'red-bold "red")))
  (with-current-buffer "*Help*"
    (let ((before-change-functions) (after-change-functions))
      (put-text-property (point-min)(or limit (point-max))
			 'face 'red-bold))))

(defun bib-cite-fontify-help-xemacs (defaults)
  (if (fboundp 'font-lock-set-defaults-1) ; >= XEmcas 19.14
      (progn
	(set-buffer "*Help*")
	(setq font-lock-defaults-computed nil
	      font-lock-keywords nil)
	(font-lock-set-defaults-1
	 (and defaults (font-lock-find-font-lock-defaults defaults)))
	(font-lock-fontify-buffer)
	(setq font-lock-defaults-computed nil
	      font-lock-keywords nil)
	(font-lock-set-defaults-1))))

(defun bib-cite-fontify-help-as-bibtex ()
  (save-excursion
    (cond
     ((not (featurep 'font-lock))
      nil)                              ;No font-lock! Stop here.
     ;; font-lock under Emacs and XEmacs
     ((string-match "XEmacs\\|Lucid" emacs-version)
      ;; XEmacs
      (bib-cite-fontify-help-xemacs 'bibtex-mode))
     (t
      ;; Emacs
      (set-buffer "*Help*")
      (let ((font-lock-defaults
	     '(bib-cite-bibtex-font-lock-keywords
	       nil t ((?$ . "\"")(?\" . ".")))))
	(if font-lock-mode
	    (font-lock-mode)
	  (if (fboundp 'font-lock-unset-defaults) (font-lock-unset-defaults))
	  (font-lock-unfontify-buffer))
	(font-lock-fontify-buffer))))))

(defun bib-cite-fontify-help-as-latex ()
  (save-excursion
    (cond
     ((not (featurep 'font-lock))
      nil)                              ;No font-lock! Stop here.
     ;; font-lock under Emacs and XEmacs
     ((string-match "XEmacs\\|Lucid" emacs-version)
      ;; XEmacs, not necessary to do s.th. special for font-latex, we do *not*
      ;; want the buffer-local faces!
      (bib-cite-fontify-help-xemacs 'latex-mode))
     (t
      ;; Emacs
      (set-buffer "*Help*")
      ;; Actually, don't want to `permanently' affect *Help* buffer...
      ;;(if (featurep 'font-latex)
      ;; (font-latex-setup)
      ;; Rather I should deal with this in the `let' form:
      ;; (make-local-variable 'font-lock-string-face)
      ;; (setq font-lock-string-face font-latex-math-face
      ;;       font-latex-string-face (default-value 'font-lock-string-face))
      (let ((font-lock-defaults
	     (if (featurep 'font-latex)
		 '((font-latex-keywords font-latex-keywords-1
					font-latex-keywords-2)
		   nil nil ((?\( . ".") (?\) . ".") (?$ . "\"")) nil
		   (font-lock-comment-start-regexp . "%")
		   (font-lock-mark-block-function . mark-paragraph))
	       '(tex-font-lock-keywords nil nil ((?$ . "\""))))))
	(if font-lock-mode
	    (font-lock-mode)
	  (if (fboundp 'font-lock-unset-defaults) (font-lock-unset-defaults))
	  (font-lock-unfontify-buffer))
	(font-lock-fontify-buffer))))))

(defvar bib-document-TeX-files-warnings nil
  "Bib-cite internal variable.")

(defun bib-etags (&optional masterdir)
  "Invoke etags on all tex files of the document in directory MASTERDIR.
Store the TAGS file in the master-directory.
Expect errors if you use this outside of auctex or within a plain
single-file document.  Also makes sure that the TAGS buffer is updated.
See variables bib-etags-command and bib-etags-filename"
  (interactive)
  (require 'etags)
  (let* ((the-file-list (bib-document-TeX-files))
	 (the-file (car the-file-list))
	 (dir (or masterdir (bib-master-directory)))
	 (the-tags-file (expand-file-name bib-etags-filename dir))
	 (the-tags-buffer (get-file-buffer the-tags-file)))
    ;; Create TAGS file with first TeX file (master file)
    (shell-command (concat bib-etags-command the-tags-file " " the-file))
    (setq the-file-list (cdr the-file-list))
    ;; Append to TAGS file for all other TeX files.
    (while the-file-list
      (setq the-file (car the-file-list))
      (shell-command
       (concat bib-etags-append-command the-tags-file " " the-file))
      (setq the-file-list (cdr the-file-list)))
    (if the-tags-buffer                 ;buffer existed; we must refresh it.
	(with-current-buffer the-tags-buffer
	  (revert-buffer t t)))

    ;; Check value of tags-file-name against the-tags-file
    (or (equal the-tags-file  tags-file-name) ;make sure it's current
	(visit-tags-table the-tags-file))

    ;(set (make-local-variable 'tags-file-name) the-tags-file))
    ;; above should not be needed

    ;; Weird Bug:
    ;;  (visit-tags-table-buffer) seems to get called twice when called by
    ;;  find-tag on an undefined tag. The second time, it's in the TAGS
    ;;  buffer and returns an error because TAGS buffer does have
    ;;  tags-file-name set.
    ;;  To get around this.  I'm setting this variable in the TAGS buffer.
    ;; Skip this in XEmacs (Changed by Anders Stenman)
    (if (and (not (string-match "XEmacs\\|Lucid" emacs-version))
	     (get-file-buffer the-tags-file))
	(with-current-buffer (get-file-buffer the-tags-file)
	  (set (make-local-variable 'tags-file-name) the-tags-file))))


  (if bib-document-TeX-files-warnings   ;free variable loose in emacs!
      (progn
	(with-output-to-temp-buffer "*Help*"
	  (princ bib-document-TeX-files-warnings))
	(setq bib-document-TeX-files-warnings nil) ;Reset
	(bib-cite-fontify-red))))

(defun bib-Is-hidden ()
  "Return true is current point is hidden."
  (if (not selective-display)
      nil                               ;Not hidden if not using this...
    (save-excursion
      (if (not (re-search-backward "[\n\^M]" nil t))
	  nil                           ;Play safe
	(if (string-equal (match-string 0) "\n")
	    nil
	  t)))))

(defun bib-highlight-mouse ()
  "Make that nice green highlight when the mouse is over LaTeX commands."
  (interactive)
;;;Comment this out.  User should be able to use bib-highlight-mouse
;;;to try it out regardless of bib-highlight-mouse-t.
;;;Check bib-highlight-mouse-t only in automated cases.
;;;
;;;  (if (and bib-highlight-mouse-t
;;;           ;;window-system)        ;Do nothing unless under X
;;;           )
;;; *all of code was here*
;;;      )
  (save-excursion
    (let ((s)(e)(extent)(local-extent-list bib-ext-list)
	  (inhibit-read-only t)
	  (modified (buffer-modified-p))) ;put-text-property changing this?
      ;; * peta Wed Nov  8 16:27:29 1995 -- better remove the mouse face
      ;;   properties first.
      (setq bib-ext-list nil)		;Reconstructed below...
      (if (string-match "XEmacs\\|Lucid" emacs-version)
	  (while local-extent-list
	    (setq extent (car local-extent-list))
	    (if (or (extent-detached-p extent)
		    (and (<= (point-min)(extent-start-position extent))
			 (>= (point-max)(extent-end-position extent))))
		(delete-extent extent)
	      (setq bib-ext-list (cons extent bib-ext-list)))
	    (setq local-extent-list (cdr local-extent-list)))
	;; Remove properties for regular emacs
	;; FIXME This detroys all mouse-faces and local-maps!
	;; FIXME Hope no other package is using them in this buffer!
	(let ((before-change-functions) (after-change-functions))
	  (remove-text-properties (point-min) (point-max)
				  '(mouse-face t local-map t))))
      (goto-char (point-min))
      (while
	  (re-search-forward
	   (concat
	    "\\\\\\(" (substring bib-ref-regexp 2)
	    "\\|label\\|[A-Za-z]*cite[A-Za-z]*\\(\\[[^]]*\\]\\)*\\){[^}]*}")
	   nil t)
	(setq s (match-beginning 0))
	(setq e (match-end 0))
	(cond
	 ((string-match "XEmacs\\|Lucid" emacs-version)
	  (setq extent (make-extent s e))
	  (setq bib-ext-list (cons extent bib-ext-list))
	  (set-extent-property extent 'highlight t)
	  (set-extent-property extent 'start-open t)
	  (set-extent-property extent 'balloon-help 'bib-label-help)
	  (set-extent-property extent 'help-echo 'bib-label-help-echo)
	  (set-extent-property extent 'keymap bib-highlight-mouse-keymap))
	 (t
	  (let ((before-change-functions) (after-change-functions)
		;;(this-overlay (make-overlay s e))
		)
;;; Even using overlays doens't help here.  If bib-highlight-mouse-keymap
;;; does not include the AucTeX menus, then these disappear when we click
;;; onto a \cite command.  Perhaps using bib-cite as a minor mode will fix
;;; this?  For now, bib-cite must be loaded after these menus are built.
;;; It must therefore be loaded in a mode-hook.
	    (put-text-property s e 'local-map bib-highlight-mouse-keymap)
	    (put-text-property s e 'mouse-face 'highlight)
	  ;;(overlay-put this-overlay 'local-map bib-highlight-mouse-keymap)
	  ;;(overlay-put this-overlay 'mouse-face 'highlight)
	    ))))
      (set-buffer-modified-p modified))))

(defun bib-toggle-highlight ()
  "Toggle the enabling of bib-cite entries as clickable things."
;; FIXME: do something about after-change stuff?
  (interactive)
  (if (setq bib-highlight-mouse-t (not bib-highlight-mouse-t))
      (bib-highlight-mouse)
    (let ((modified (buffer-modified-p))
	  (inhibit-read-only t))
      (cond
       ((string-match "XEmacs\\|Lucid" emacs-version)
	(while bib-ext-list
	  (delete-extent (car bib-ext-list))
	  (setq bib-ext-list (cdr bib-ext-list))))
       (t
	(let ((before-change-functions) (after-change-functions))
	  (remove-text-properties (point-min) (point-max)
				  '(mouse-face local-map)))))
      (set-buffer-modified-p modified))))

(defun bib-label-help-echo (object)
  (if bib-label-help-echo-format
      (bib-label-help object bib-label-help-echo-format)))

;;; Balloon-help callback. Anders Stenman <stenman@isy.liu.se>
;;;             Patched by Bruce Ravel <bruce.ravel@nist.gov>
(defun bib-label-help (object &optional format)
  (or format (setq format "Use mouse button 2 to find the %s.
Use mouse button 3 to display the %s."))
  (save-match-data
    (let* ((string (extent-string object))
	   (type (cond ((string-match "^\\\\[A-Za-z]*cite[A-Za-z]*" string) "citation")
		       ((string-match
			 (concat "^" bib-ref-regexp) string) "\\label{}")
		       ((string-match "^\\\\label" string) "\\ref{}")
		       (t "this (unknown) reference"))))
      (format format type type))))

;;----------------------------------------------------------------------------
;; Routines to display or edit a citation's bibliography

(defun bib-display-citation ()
  "Do the displaying of cite info.  Return t if found cite key, nil otherwise.
Example with cursor located over cite command or arguments:
\cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
   ^Display-all-citations          ^Display-this-citation"
  (save-excursion
    (let* ((the-keys-obarray (bib-get-citekeys-obarray)) ;1st in case of error
	   (work-buffer (get-buffer-create "*bibtex-work*"))
	   (bib-buffer (bib-get-bibliography nil))
	   (the-warnings (bib-get-citations
			  the-keys-obarray
			  bib-buffer
			  work-buffer
			  bib-substitute-string-in-display))
	   (the-warn-point))
      (if the-warnings
	  (progn
	    (set-buffer work-buffer)
	    (goto-char 1)
	    (insert the-warnings)
	    (setq the-warn-point (point))))
      (with-output-to-temp-buffer
	  "*Help*"
	(set-buffer work-buffer)
	(princ (buffer-substring 1 (point-max))))
      (bib-cite-fontify-help-as-bibtex)
      (if the-warn-point
	  (bib-cite-fontify-red the-warn-point))
      (kill-buffer bib-buffer)
      (kill-buffer work-buffer))))

(defun bib-edit-citation ()
  "Do the edit of cite info.  Return t if found cite key, nil otherwise.
Find and and put edit point in bib file associated with a BibTeX citation
under cursor from \bibliography input file.
In a multi-entry cite command, the cursor should be on the actual cite key
desired (otherwise a random entry will be selected).
e.g.: \cite{Wadhams81,Bourke.et.al87,SchneiderBudeus94}
			^Display-this-citation"
  (let ((the-keys-obarray (bib-get-citekeys-obarray)) ;1st in case of error
	(bib-buffer (bib-get-bibliography t))
	(the-key)(the-file))
    (save-excursion
      (mapatoms                     ;Do this for each cite-key found...
       (lambda (cite-key)
	 (setq the-key (symbol-name cite-key)))
       the-keys-obarray)
      (set-buffer bib-buffer)
      (goto-char (point-min))
      (if (not (re-search-forward
		(concat "@[^{(]+[{(][\t ]*" (regexp-quote the-key) "[ ,\n]")
		nil t))
	  (progn
	    (kill-buffer bib-buffer)
	    (error "Sorry, could not find bib entry for %s" the-key))
	(re-search-backward "%%%Filename: \\([^\n]*\\)" nil t)
	(setq the-file (match-string 1))
	(kill-buffer bib-buffer)))
;;; (find-file the-file)
    (funcall bib-switch-to-buffer-function (find-file-noselect the-file))
    (goto-char (point-min))             ;V2.19 fix
    (re-search-forward (concat "@[^{(]+[{(][\t ]*"
			       (regexp-quote the-key)
			       "[ ,\n]") nil t)))

;;--------------------------------------------------------------------------
;; Function for bib-apropos

(defun bib-apropos-keyword-at-point ()
  "Return the keyword under point for initial input to bib-apropos prompt."
  (save-excursion
    (let ((here (point)))
      (cond
       ((and (re-search-backward "[\n{, ]" nil t)
	     (string-equal "{" (buffer-substring (match-beginning 0)
						 (match-end 0))))
	(if (fboundp 'buffer-substring-no-properties)
	    (buffer-substring-no-properties (1+ (point)) here)
	(buffer-substring (1+ (point)) here)))))))

;;--------------------------------------------------------------------------
;; Functions for Displaying or moving to matching \ref or \label command

(defun bib-display-label ()
"Display environment or first ref associated with a label.
The label or ref name is extracted from the text under the cursor, or the
user is prompted is nothing suitable is found.  The first prompt is for a
label.  If you answer with an empty string, a second prompt for a ref will
be given."
  (let ((the-regexp (bib-guess-or-prompt-for-label)))
    (if (not the-regexp)
	(message "No name given")
      (bib-display-or-find-label the-regexp t))))

(defun bib-find-label ()
  "Move to a label, or the first occurance of a ref.
The label or ref name is extracted from the text under the cursor.
If nothing suitable is found, the user is prompted. The first prompt is for a
label. If you answer with an empty string, a second prompt for a ref will be
given.

If within a single file document:
  You can move back with C-xC-x as the mark is set before moving.
  You can search for next occurrances of a ref command with C-sC-s.

If within a multi-file document (in auctex only)
  You can move back with C-xC-x if within the same buffer.  If not, just
  select your previous buffer.
  You can search for next occurrances of a ref command with tag commands:
     C-u M-.     Find next alternate definition of last tag specified.
     C-u - M-.   Go back to previous tag found."
  (let ((the-regexp (bib-guess-or-prompt-for-label)))
    (if (not the-regexp)
	(message "No name given")
      (bib-display-or-find-label the-regexp nil))))

;;--------------------------------------------------------------------------
;; Functions for Displaying or moving to matching \ref or \label command

(defun bib-display-or-find-label (the-regexp displayf)
;; work horse for bib-find-label and bib-display-label
  (let* ((masterfile (bib-master-file))
	 (masterdir (and masterfile
			 (file-name-directory masterfile)))
	 (new-point)(new-buffer))
    (save-excursion
      ;; Now we are either in a simple file, or with a multi-file document
      (cond
       (masterfile                   ;Multi-file document
	(cond
	 (displayf                  ;Display only
	  (set-buffer (bib-etags-find-noselect the-regexp masterdir))
	  (re-search-forward the-regexp nil t)
	  ;; ...because tags puts point on beginning of line
	  (if (string-match "^\\\\\\\\label" the-regexp)
	      (bib-display-this-environment) ;display the label's environment
	    (bib-display-this-ref)))    ;     display the ref's context
	 (t                         ;Move to it
	  (setq new-buffer (bib-etags-find-noselect the-regexp masterdir))
	  (if bib-novice
	      (message
	       (substitute-command-keys
		(concat "Use \\[bib-find-next] to find the next occurrence "
			"and C-u \\[bib-find-next] to find previous."))))
	  (if (equal new-buffer (current-buffer))
	      (setq new-point (point)))))) ;Moving with the same buffer
       (t                           ;Single-file document
	(goto-char (point-min))
	(cond
	 ((re-search-forward the-regexp nil t)
	  (if displayf
	      (if (string-match "^\\\\label" the-regexp)
		  (bib-display-this-environment) ;Display the environment
		(bib-display-this-ref)) ;         display the ref's context
	    (setq new-point (match-beginning 0))  ;or move there
	    (if (string-match "{\\(.*\\)}" the-regexp)
		(setq bib-cite-search-ring (match-string 1 the-regexp)))
	    (if bib-novice
		(message
		 (substitute-command-keys
		  (concat "Use \\[bib-find-next] to find the next occurrence "
			  "and C-u \\[bib-find-next] to find previous."))))))
	 (t
	  (message "Sorry, cannot find it (%s)" the-regexp))))))
    (if new-point
	(progn
	  (push-mark (point) t nil)   ;We've moving there... push mark
	  (goto-char new-point))
      (if new-buffer                    ;We've changing buffer
	  ;;(switch-to-buffer new-buffer)
	  (funcall bib-switch-to-buffer-function new-buffer)))
    (if (bib-Is-hidden)
	(save-excursion
	  (beginning-of-line)
	  (show-entry)))))

(defvar bib-label-prompt-map nil)
(if bib-label-prompt-map
    ()
  (setq bib-label-prompt-map (copy-keymap minibuffer-local-completion-map))
  (define-key bib-label-prompt-map " " 'self-insert-command))

(defun bib-guess-or-prompt-for-label ()
  "Guess from context, or prompt the user for a label command."
  (save-excursion
    (if (not (looking-at "\\\\"))        ;If not on beginning of a command
	     (re-search-backward "[\\]"
				 (save-excursion (beginning-of-line)(point))
				 t))
    (cond
     ((looking-at bib-ref-regexpc)   ;On \ref, looking for matching \label
      (let ((b (progn (search-forward "{" nil t)(forward-char -1)(point)))
	    (e (progn (forward-sexp 1)(point))))
	(concat "\\\\label" (regexp-quote (buffer-substring b e)))))
     ((looking-at "\\\\label{")         ;On \label, looking for matching \ref
      (let ((b (progn (search-forward "{" nil t)(forward-char -1)(point)))
	    (e (progn (forward-sexp 1)(point))))
	(concat  bib-ref-regexp (regexp-quote (buffer-substring b e)))))
     (t                                 ;Prompt the user
      (let* ((minibuffer-local-completion-map bib-label-prompt-map)
	     (the-alist (create-alist-from-list
			 (cdr (reverse LaTeX-label-list))))
	;;; LaTeX-label-list example:
	;;;  '(("label3" "label4")("label1" "label2") nil)
	;; so let's get rid of that nil part in embedded list.
	     (the-name
	      (if (string-equal "18" (substring emacs-version 0 2))
		  (completing-read "Label: " the-alist nil nil nil)
		(completing-read "Label: " the-alist nil nil nil
				 'LaTeX-find-label-hist-alist))))
	(if (not (equal the-name ""))
	    (concat "\\\\label{" (regexp-quote the-name) "}")
	  ;; else try to get a \ref
	  (if (string-equal "18" (substring emacs-version 0 2))
	      (setq the-name (completing-read "Ref: " the-alist nil nil nil))
	    (setq the-name (completing-read "Ref: " the-alist nil nil nil
					    'LaTeX-find-label-hist-alist)))
	  (if (not (equal the-name ""))
	      (concat bib-ref-regexpc (regexp-quote the-name) "}")
	    nil)))))))

(defun bib-display-this-ref ()
  "Display a few lines around current point."
  (cond
   ((bib-Is-hidden)
    (with-output-to-temp-buffer "*BiBTemp*"
      (princ
       (buffer-substring
	(save-excursion
	  (let ((i 3))
	    (while (and (> i 0)
			(re-search-backward "[\n\^M]" nil t)
			(setq i (1- i)))))
	  (point))
	(save-excursion
	  (let ((i 3))
	    (while (and (> i 0)
			(re-search-forward "[\n\^M]" nil t)
			(setq i (1- i)))))
	  (point)))))
    (set-buffer "*BiBTemp*")
    (while (search-forward "\^M" nil t)
      (replace-match "\n" nil t))
    (goto-char 1)
    (if (looking-at "\n")  ;Remove first empty line...
	(delete-char 1))
    (with-output-to-temp-buffer "*Help*"
      (princ (buffer-substring 1 (point-max))))
    (bib-cite-fontify-help-as-latex)
    (kill-buffer "*BiBTemp*"))
   (t
    (with-output-to-temp-buffer ;     display the ref's context
	"*Help*"
      (princ
       (buffer-substring (save-excursion (forward-line -2)(point))
			 (save-excursion (forward-line 3)(point)))))
    (bib-cite-fontify-help-as-latex))))

(defun bib-display-this-environment ()
  "Display the environment associated with a label, or its section name.
Assumes point is already on the label.
Does not save excursion."
;; Bugs:  The method used here to detect the environment is *not* foolproof.
;;        It will get confused, for example, between two figure environments,
;;        picking out both instead of the section label above them.  But since
;;        users typically puts their labels next to the section declaration,
;;        I'm satisfied with this... for now.
;; I could have used the following AUCTeX functions:
;;  LaTeX-current-environment
;;    Function: Return the name (a string) of the enclosing LaTeX environment.
;;  LaTeX-current-section
;;    Function: Return the level of the section that contain point.
;; but then this code would only work as part of AUCTeX...
  (let ((the-point (point))
	(end-point (point))
	(the-environment)(foundf))
    (while (and (not foundf)
		(goto-char end-point) ;Past end of last search
		(re-search-forward "\\(^\\|\^M\\)[ \t]*\\\\end{\\([^}]*\\)}"
				   nil t))
      (setq end-point (point))
      (setq the-environment (match-string 2))
      (and (not (string-match "document" the-environment))
	   (re-search-backward (concat "\\(^\\|\^M\\)[ \t]*\\\\begin{"
				       (regexp-quote the-environment) "}"))
	   (<= (point) the-point)
	   (setq foundf t)))
    (if foundf                          ;A good environment
	(progn
	  (cond ((bib-Is-hidden)        ;Better way is: replace-within-string
		 (with-output-to-temp-buffer "*BiBTemp*"
		   (princ (buffer-substring (point) end-point)))
		 (set-buffer "*BiBTemp*")
		 (while (search-forward "\^M" nil t)
		   (replace-match "\n" nil t))
		 (goto-char 1)
		 (if (looking-at "\n")  ;Remove first empty line...
		     (delete-char 1))
		 (with-output-to-temp-buffer "*Help*"
		   (princ (buffer-substring 1 (point-max))))
		 (kill-buffer "*BiBTemp*"))
		(t
		 (with-output-to-temp-buffer "*Help*"
		   (princ (buffer-substring (point) end-point)))))
	  (bib-cite-fontify-help-as-latex))
      ;; Just find the section declaration
      (goto-char the-point)
      (if (re-search-backward
;;;        "\\(^\\|\^M\\)[ \t]*\\\\\\(sub\\)*section{\\([^}]*\\)}" nil t)
;;; Michael Steiner <steiner@cs.uni-sb.de> patch
	   "\\(^\\|\^M\\)[ \t]*\\\\\\(\\(sub\\)*section\\|chapter\\|part\\)\\*?\
{\\([^}]*\\)}"
	   nil t)
	  (message (match-string 0))
	(error
	 "Sorry, could not find an environment or section declaration")))))

(defvar LaTeX-find-label-hist-alist nil "History list for LaTeX-find-label.")
(defvar LaTeX-label-list nil "Used by AUCTeX to store label names.")


(defun create-alist-from-list (the-list)
  "Return a single list from a THE-LIST that may contain either items or lists.
e.g. turns
'((\"label3\" \"label4\")(\"label1\" \"label2\") \"label\")
into
'((\"label3\") (\"label4\") (\"label1\") (\"label2\") (\"label\"))"
  (mapcar 'list (bib-cite-mh-list-to-string the-list)))

;;;
;;; Following two functions from mh-utils.el (part of GNU emacs)
;;; I have changed the names in case these functions change what they do.
;;;

(defun bib-cite-mh-list-to-string (l)
  "Flattens the list L and make every element of the new list into a string."
  (nreverse (bib-cite-mh-list-to-string-1 l)))

(defun bib-cite-mh-list-to-string-1 (l)
  (let ((new-list nil))
    (while l
      (cond ((null (car l)))
	    ((symbolp (car l))
	     (setq new-list (cons (symbol-name (car l)) new-list)))
	    ((numberp (car l))
	     (setq new-list (cons (int-to-string (car l)) new-list)))
	    ((equal (car l) ""))
	    ((stringp (car l)) (setq new-list (cons (car l) new-list)))
	    ((listp (car l))
	     (setq new-list (nconc (bib-cite-mh-list-to-string-1 (car l))
				   new-list)))
	    (t (error "Bad element in mh-list-to-string: %s" (car l))))
      (setq l (cdr l)))
    new-list))

;; -------------------------------------------------------------------------
;; Routines to extract cite keys from text

;;    ... is truly remarkable, as shown in \citeN{Thomson77,Test56}. Every
;; \cite[{\it e.g.}]{Thomson77,Test56}

(defun bib-get-citations (keys-obarray bib-buffer new-buffer substitute)
  "Put citations of KEYS-OBARRAY from BIB-BUFFER into NEW-BUFFER.
Substitute strings if SUBSTITUTE is t
Return the-warnings as text."
  (let ((the-warnings)                  ;The only variable to remember...
	(case-fold-search t))           ;All other results go into new-buffer
    ;; bibtex is not case-sensitive for keys.
    (save-excursion
      (let ((the-text))
	(set-buffer bib-buffer)
	(mapatoms                         ;Do this for each cite-key found...
	 (lambda (cite-key)
	   (goto-char (point-min))
	   (if (re-search-forward
		(concat "@[^{(]+[{(][\t ]*"
			(regexp-quote (symbol-name cite-key))
			"\\([, ]\\\|$\\)")
		;;           ^^     ^  comma, space or end-of-line
		nil t)
	       (setq the-text (concat the-text
				      (buffer-substring
				       (progn (beginning-of-line)(point))
				       (progn (forward-sexp 2)(point)))
				      "\n\n"))
	     (setq the-warnings (concat the-warnings
					"Cannot find entry for: "
					(symbol-name cite-key) "\n"))))
	 keys-obarray)
	(if (not the-text)
	    (error "Sorry, could not find any of the references"))
	;; Insert the citations in the new buffer
	(set-buffer new-buffer)
	(insert the-text)
	(goto-char 1))

      ;; We are at beginning of new-buffer.
      ;; Now handle crossrefs
      (let ((crossref-obarray (make-vector 201 0)))
	(while (re-search-forward
		"[, \t]*crossref[ \t]*=[ \t]*\\(\"\\|\{\\)" nil t)
	  ;;handle {text} or "text" cases
	  (if (string-equal "{" (match-string 1))
	      (re-search-forward "[^\}]+" nil t)
	    (re-search-forward "[^\"]+" nil t))
	  (intern (match-string 0) crossref-obarray))
	;; Now find the corresponding keys,
	;; but add them only if not already in `keys-obarray'
	(set-buffer bib-buffer)
	(goto-char 1)
	(let ((the-text))
	  (mapatoms                     ;Do this for each crossref key found...
	   (lambda (crossref-key)
	     (if (not (intern-soft (symbol-name crossref-key) keys-obarray))
		 (progn
		   ;; Not in keys-obarray, so not yet displayed.
		   (goto-char (point-min))
		   (if (re-search-forward
			(concat "@[^{(]+[{(][\t ]*"
				(regexp-quote (symbol-name crossref-key))
				"\\(,\\|$\\)")
			nil t)
		       (setq the-text
			     (concat the-text
				     (buffer-substring
				      (progn (beginning-of-line)(point))
				      (progn (forward-sexp 2)(point)))
				     "\n\n"))
		     (setq the-warnings
			   (concat the-warnings
				   "Cannot find crossref entry for: "
				   (symbol-name crossref-key) "\n"))))))
	   crossref-obarray)
	  ;; Insert the citations in the new buffer
	  (set-buffer new-buffer)
	  (goto-char (point-max))
	  (if the-text
	      (insert the-text)))
	(goto-char 1))

      ;; Now we have all citations in new-buffer, collect all used @String keys
      ;; Ex:  journal =      JPO,
      (let ((strings-obarray (make-vector 201 0)))
	(while (re-search-forward bib-string-regexp nil t)
	  (intern (match-string 1) strings-obarray))
	;; Now find the corresponding @String commands
	;; Collect either the @string commands, or the string to substitute
	(set-buffer bib-buffer)
	(goto-char 1)
	(let ((string-alist)
	      (the-text))
	  (mapatoms                     ;Do this for each string-key found...
	   (lambda (string-key)
	     (goto-char (point-min))
	     ;; search for @string{ key = {text}} or @string{ key = "text"}
	     (if (re-search-forward
		  (concat "^[ \t]*@string[{(]"
			  (regexp-quote (symbol-name string-key))
			  "[\t ]*=[\t ]*\\(\"\\|\{\\)")
		  nil t)
		 (let ((the-string-start (1- (match-end 1))) ;catch bracket
		       ;;handle {text} or "text" cases
		       (the-string-end
			(cond
			 ((string-equal "\"" (match-string 1))
			  (re-search-forward "[^\\]\"" nil t)
			  (point))
			 (t
			  (forward-char -1)
			  (forward-list 1)
			  (point)))))
		   (if substitute      ;Collect substitutions
		       (setq string-alist
			     (append
			      string-alist
			      (list
			       (cons (symbol-name string-key)
					;(regexp-quote
				     (buffer-substring the-string-start
						       the-string-end)))));)
		     ;;Collect the strings command themseves
		     (setq the-text
			   (concat the-text
				   (buffer-substring
				    (progn (forward-char 1)(point))
				    (re-search-backward "^[ \t]*@string[{(]"
							nil t))
				   "\n"))))
	       ;; @string entry not found
	       (if (not (member-cis (symbol-name string-key)
				    bib-string-ignored-warning))
		   (setq the-warnings
			 (concat the-warnings
				 "Cannot find @String entry for: "
				 (symbol-name string-key) "\n")))))
	   strings-obarray)
	  ;; Now we have `the-text' of @string commands,
	  ;; or the `string-alist' to substitute.
	  (set-buffer new-buffer)
	  (if substitute
	      (while string-alist
		(goto-char 1)
		(let* ((the-key (car (car string-alist)))
		       (the-string (cdr (car string-alist)))
		       (slashed-string  ; "J. of Geo.\" -> "J. of Geo.\\\\"
			(dired-replace-in-string
			 "\\\\" "\\\\" the-string)))

		  (while (re-search-forward
			  (concat "\\(^[, \t]*[a-zA-Z]+[ \t]*=[ \t]*\\)"
				  (regexp-quote the-key)
				  "\\([, \t\n]\\)")
			  nil t)
		    (replace-match (concat "\\1" slashed-string "\\2") t nil)))
		(setq string-alist (cdr string-alist)))
	    ;; substitute is nil; Simply insert text of @string commands
	    (goto-char 1)
	    (if the-text
		(insert the-text "\n")))
	  (goto-char 1))))

    ;; We are done!
    ;; Return the warnings...
    the-warnings))

;;; Following contributed by Michael Steiner <steiner@cs.uni-sb.de> The
;;  @string abbreviation are not case-sensitive, so we replaced the `member'
;;  test above with `member-cis' defined here:
(defun member-cis (ELT LIST)
  "Return non-nil if ELT is an element of LIST.
All elements should be strings.
Comparison is case-insensitive."
  ;; If list is exhausted,
  (if (null LIST)
      nil ;; if null then we haven't found the element ...
    ;; else split list and ...
    (let((listelt (car LIST))(listrest (cdr LIST)))
      ;; see if car is equal to ELT
      (if (string-equal (downcase ELT) (downcase listelt))
	  t ;; if so return true
	;; else recurse for rest of list
	(member-cis ELT listrest)))))

(defun bib-get-citekeys-obarray ()
  "Return obarray of citation key (within curly brackets) under cursor."
  (save-excursion
    ;; First find *only* a key *within a cite command
    (let ((the-point (point))
	  (keys-obarray (make-vector 201 0)))
      ;; First try to match a cite command
      (if (and (skip-chars-backward "a-zA-Z") ;Stops on \ or {
	       (looking-at "[a-zA-Z]*cite[a-zA-Z]*"))
	  (progn
	    ;;skip over any optional arguments to \cite[][]{key} command
	    (skip-chars-forward "a-zA-Z")
	    (while (looking-at "\\[")
	      (forward-list 1))
	    (re-search-forward "{[ \n]*\\([^,} \n]+\\)" nil t)
	    (intern (match-string 1) keys-obarray)
	    (while (and (skip-chars-forward " \n") ;no effect on while
			(looking-at ","))
	      (forward-char 1)
	      ;;The following re-search skips over leading spaces
	      (re-search-forward "\\([^,} \n]+\\)" nil t)
	      (intern (match-string 1) keys-obarray)))
	;; Assume we are on the keyword
	(goto-char the-point)
	(let ((the-start (re-search-backward "[\n{, ]" nil t))
	      (the-end (progn (goto-char the-point)
			      (re-search-forward "[\n}, ]" nil t))))
	  (if (and the-start the-end)
	      (intern (buffer-substring (1+ the-start) (1- the-end))
		      keys-obarray)
	    ;; Neither...
	    (error "Sorry, can't find a reference here"))))
      keys-obarray)))

(defun bib-buffer-citekeys-obarray ()
  "Extract citations keys used in the current buffer."
  (let ((keys-obarray (make-vector 201 0)))
    (save-excursion
      (goto-char (point-min))
      ;; Following must allow for \cite[e.g.][]{key} !!!
      ;; regexp for \cite{key1,key2} was "\\\\[a-Z]*cite[a-Z]*{\\([^,}]+\\)"
      (while (re-search-forward "\\\\[a-zA-Z]*cite[a-zA-Z]*\\(\\[\\|{\\)"
				nil t)
	(backward-char 1)
	(while (looking-at "\\[")       ; ...so skip all bracketted options
	  (forward-sexp 1))
	;; then lookup first key
	(if (looking-at "{[ \n]*\\([^,} \n]+\\)")
	    (progn
	      (intern (match-string 1) keys-obarray)
	      (goto-char (match-end 1))
	      (while (and (skip-chars-forward " \n")
			  (looking-at ","))
		(forward-char 1)
		(re-search-forward "\\([^,} \n]+\\)" nil t)
		(intern (match-string 1) keys-obarray)))))
      (if keys-obarray
	  keys-obarray
	(error "Sorry, could not find any citation keys in this buffer")))))

;;---------------------------------------------------------------------------
;; Multi-file document programming requirements:
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; bib-make-bibliography
;;    bib-document-citekeys-obarray needs the master .aux file to extract
;;   citation keys.
;;    Included .aux files (corresponding to \include'd LaTeX files) are
;;   then specified relative to the master-file-directory.
;;
;; bib-get-bibliography (used by interactive commands to extract bib sources)
;;
;;    bibtex source filenames are returned from (LaTeX-bibliography-list)
;;   unformatted.  Since only a single \bibliogragrphy command is allowed
;;   by BiBTeX in a document, it is safe to assume that their path is
;;   relative to the master file's directory (since the path is relative
;;   to where the BiBTeX program is actually ran).
;;

;; (See TeX-check-files, used in TeX-save-document.  All documents related
;;  files are returned by (TeX-style-list) and stored in TeX-active-styles.
;;  Original idea was to search TeX-check-path for files listed in
;;  TeX-active-styles (with possible relative or full paths) that end in .tex.)

(defun bib-master-directory ()
  "Return the directory associated with the master file.
If no master file, then return current default."
  (let ((masterfile (bib-master-file)))
    (if masterfile
	(file-name-directory (expand-file-name (TeX-master-file)))
      default-directory)))

(defun bib-master-file ()
  "Return master file full path, or nil if not a multi-file document."
;; I wish there were a better way to tell about non multi-file documents...
  (let ((master
	 (cond
	  ((not (boundp 'TeX-master))
	   ;; This buffer doesn't know what a master file is, so return now.
	   nil)
	  ((and TeX-master              ;Set, but not to t
		(not (symbolp TeX-master))) ; then we have an actual name
	   (expand-file-name TeX-master))
	  ((and (eq TeX-master 't)      ;Test if master file itself
		(progn                  ;But also require at least one \include
		  (save-excursion
		    (goto-char 1)       ;Too bad I have to do this search...
		    ;; Require that user uses \input{file}
		    ;; rather than            \input file
		    (re-search-forward "^[ \t]*\\\\\\(include\\|input\\){"
				       nil t))))
	   (buffer-file-name))
	  (t
	   nil))))
    (cond
     ((not master)
      nil)
     ((string-match ".\\(tex\\|ltx\\)$" master)
      master)
     ((file-readable-p (concat master ".ltx"))
      (concat master ".ltx"))
     (t
      (concat master ".tex")))))

;; I don't use this one because files are not returned in order...
;; (defun bib-document-TeX-files ()
;; ;; Return all tex input files associated with a known multi-file document.
;;   (let ((master-directory (bib-master-directory))
;;         (the-list (cons (file-name-nondirectory (TeX-master-file))
;;                         (TeX-style-list)))
;;      ;; TeX-style-list returns "../master" for the main file if TeX-master
;;      ;; was set like that.  "../master" would not be found relative
;;      ;; to the master-directory!  So let's add it to the list w/o directory.
;;         (the-result)
;;         (the-file))
;;     (while the-list
;;       (setq the-file (expand-file-name (car the-list) master-directory))
;;       (setq the-list (cdr the-list))
;;       (and (not (string-match ".tex$" the-file))
;;            (setq the-file (concat the-file ".tex")))
;;       (and (file-readable-p the-file)
;;            (not (member the-file the-result)) ;listed already?
;;            (setq the-result (cons the-file the-result))))
;;     the-result))

(defun bib-document-TeX-files ()
  "Return all tex input files associated with a *known* multi-file document.
For a multi-file document in auctex only.
No checking is done that this is a real multi-file document.
Sets global variable bib-document-TeX-files-warnings."
  (setq bib-document-TeX-files-warnings nil)
  (let* ((masterfile (bib-master-file))
	 (dir (and masterfile (file-name-directory masterfile)))
	 (tex-buffer (get-buffer-create "*tex-document*"))
	 (the-list (list masterfile))
	 (the-file))
    (if (not masterfile)
	(progn
	  (kill-buffer tex-buffer)
	  (error
	   "Sorry, but this is not a multi-file document (Try C-u C-c C-n if using auctex)")))
    (with-current-buffer tex-buffer
      ;; set its directory so relative includes work without expanding
      (setq default-directory dir)
      (insert-file-contents masterfile)
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\\\\\(input\\|include\\){\\(.*\\)}"
				nil t)
	(let ((the-file (match-string 2)))
	  (if (string-match ".sty$" the-file) ;Skip over style files!
	      nil
	    (if (and (not (file-readable-p (expand-file-name the-file dir)))
		     (not (string-match ".ltx$" the-file))
		     (file-readable-p
		      (expand-file-name (concat the-file ".ltx") dir)))
		(setq the-file (concat the-file ".ltx")))
	    (if (and (not (file-readable-p (expand-file-name the-file dir)))
		     (not (string-match ".tex$" the-file)))
		(setq the-file (concat the-file ".tex")))
	    (setq the-file (expand-file-name the-file dir))
	    (if (not (file-readable-p the-file))
		(setq bib-document-TeX-files-warnings
		      (concat
		       bib-document-TeX-files-warnings
		       (format "Warning: File not found: %s" the-file)))
	      (setq the-list (cons (expand-file-name the-file dir) the-list))
	      (end-of-line)(insert "\n")
	      (insert-file-contents the-file))))))
    (kill-buffer tex-buffer)
    (nreverse the-list)))

(defun bib-document-citekeys-obarray ()
  "Return cite keys obarray for multi-file document.
Return nil if not a multi-file document.
This is a AUCTeX supported feature only.
Also, see bib-buffer-citekeys-obarray.
Sets global variable bib-document-citekeys-obarray-warnings."
  (setq bib-document-citekeys-obarray-warnings nil)
  (let ((master-tex (bib-master-file))
	(master-aux))
    (if (not master-tex)
	nil                             ;Not a multifile document.  No need...
      (setq master-aux (bib-return-aux-file-from-tex master-tex "aux"))
      (or (file-readable-p master-aux)
	  (error "Sorry, cannot read file %s" master-aux))
      (and (file-newer-than-file-p master-tex master-aux)
	   (setq bib-document-citekeys-obarray-warnings
		 (format "Warning: %s is out of date relative to %s.\n"
			 master-aux master-tex)))
      (let ((work-buffer (get-buffer-create "*bib-cite-work*"))
	    (keys-obarray (make-vector 201 0)))
	(with-current-buffer work-buffer
	  (insert-file-contents master-aux)
	  ;; Because we will be looking for \input statements, we need to set
	  ;; the default directory to that of the master file.
	  (setq default-directory (file-name-directory master-tex))
	  ;; bib-make-bibliography will need this also to find .bib files
	  ;; look for \@input{chap1/part1.aux}
	  (while (re-search-forward "^\\\\@input{\\(.*\\)}$" nil t)
	    (let* ((auxfile (match-string 1))
		   (texfile (bib-return-aux-file-from-tex auxfile "tex")))
	      (if (not (file-readable-p auxfile))
		  (setq bib-document-citekeys-obarray-warnings
			(concat
			 bib-document-citekeys-obarray-warnings
			 (format "Warning: %s is not found or readable.\n"
				 auxfile)))
		(if (file-newer-than-file-p texfile auxfile)
		    (setq bib-document-citekeys-obarray-warnings
			  (concat
			   bib-document-citekeys-obarray-warnings
			   (format
			    "Warning: %s is out of date relative to %s.\n"
			    auxfile texfile))))
		(end-of-line)(insert "\n")
		(insert-file-contents auxfile))))
	  (goto-char 1)

;;; Patched by calvanes@dis.uniroma1.it (Diego Calvanese)
;;;      ;; look for \citation{gertsenshtein59}
;;;       (while (re-search-forward "^\\\\citation{\\(.*\\)}$" nil t)
;;;         (intern (buffer-substring (match-beginning 1)(match-end 1))
;;;                 keys-obarray))
	  ;; look for \citation{gertsenshtein59,vardi88,...,ullmann90}
	  ;; comma-separation generated by certain LaTeX styles.
	  (while (re-search-forward "^\\\\citation{\\(.*\\)}$" nil t)
	    (let ((string (match-string 1))
		  (start 0))
	      (while (string-match "\\([^,\n]+\\)" string start)
		(intern (substring string (match-beginning 1) (match-end 1))
			keys-obarray)
		(setq start (match-end 0))))))
	(kill-buffer work-buffer)
	keys-obarray))))

(defun bib-return-aux-file-from-tex (texname ext)
  "Given name.name.XXX in TEXNAME return name.name.EXT."
;; FIXME: Check if in ./, else search
  (let* ((filename (if (string-match "\\(.*\\)\\.[^\\.]+" texname)
		       (concat (match-string 1 texname) "." ext)
		     (concat texname "." ext)))
	 (sansdir (file-name-nondirectory filename)))
    (if (file-exists-p filename)
	filename
      ;; Search bib-cite-aux-inputs path
      (let ((filename (psg-checkfor-file-list sansdir bib-cite-aux-inputs)))
	(if (and filename (file-exists-p filename))
	    filename
	  (error "Could not find file %s" sansdir))))))

(defun bib-etags-find-noselect (tag &optional masterdir)
  "Returns a buffer with point on TAG.
Buffer is not selected.
Makes sure TAGS file exists, etc."
  (require 'etags)
  (let* ((master (or masterdir (bib-master-directory)))
	 (the-buffer (current-buffer))
	 (new-buffer)
	 (the-tags-file-name (expand-file-name bib-etags-filename master)))
    (or (file-exists-p the-tags-file-name) ;make sure TAGS exists
	(bib-etags master))
    (or (equal the-tags-file-name tags-file-name) ;make sure it's current
	(visit-tags-table the-tags-file-name))
    ;; find-tag-noselect should set the TAGS file for the new buffer
    ;; that's what C-h f visit-tags-table says...
    (cond
     ((string-match "XEmacs\\|Lucid" emacs-version)
      (find-tag tag)
      (setq new-buffer (current-buffer))
      (set-buffer the-buffer))
     (t
      (setq new-buffer (find-tag-noselect tag nil t))
					; -> Seems to set buffer to TAGS
      (set-buffer the-buffer)))
    new-buffer))

;; --------------------------------------------------------------------------
;; The following routines make a temporary bibliography buffer
;; holding all bibtex files found.

(defun bib-get-bibliography (include-filenames-f)
  "Returns a new bibliography buffer holding all bibtex files in the document.

If using AUCTeX, and either TeX-parse-self is set or C-c C-n is used to
parse the document, then the entire multifile document will be searched
for \bibliography commands.

If this fails, the current buffer is searched for the first \bibliography
command.

If include-filenames-f is true, include as a special header the filename
of each bib file.

Puts the buffer in text-mode such that forward-sexp works with german \"
accents embeded in bibtex entries."
  (let ((bib-list (or (and (fboundp 'LaTeX-bibliography-list)
			   (boundp 'TeX-auto-update)
			   (LaTeX-bibliography-list))
;; LaTeX-bibliography-list (if bound) returns an unformatted list of
;; bib files used in the document, but only if parsing is turned on
;; or C-c C-n was used.
		      (bib-bibliography-list)))
	(bib-buffer (get-buffer-create "*bibtex-bibliography*"))
	;; Path is relative to the master directory
	(default-directory (bib-master-directory))
	(the-name)(the-warnings)(the-file))
    (with-current-buffer bib-buffer
      ;; such that forward-sexp works with embeeded \" in german,
      ;; and unbalanced ()
      (erase-buffer)
      (set-syntax-table text-mode-syntax-table)
;;      (if (boundp 'bibtex-mode-syntax-table)
;;          (set-syntax-table bibtex-mode-syntax-table)
;;        (text-mode))
      )
    ;;We have a list of bib files
    ;;Search for them, include them, list those not readable
    (while bib-list
      (setq the-name (car (car bib-list))) ;Extract the string only
      (setq bib-list (cdr bib-list))
      (setq the-name
	    (substring the-name
		       (string-match "[^ ]+" the-name) ;remove leading spaces
		       (string-match "[ ]+$" the-name))) ;remove trailing space
      (if (not (string-match "\\.bib$" the-name))
	  (setq the-name (concat the-name ".bib")))
      (setq the-file
	    (or (and (file-readable-p the-name) the-name)
		(psg-checkfor-file-list
		 the-name (psg-list-env bib-bibtex-env-variable))
		;; Check for BIBINPUT env variable as well (by popular demand!)
		(psg-checkfor-file-list the-name (psg-list-env "BIBINPUT"))
		(and bib-cite-inputs
		     (psg-checkfor-file-list the-name bib-cite-inputs))
		(and (boundp 'TeX-check-path)
		     (psg-checkfor-file-list the-name TeX-check-path))))
      (if the-file
	  (with-current-buffer bib-buffer
	    (goto-char (point-max))
	    (if include-filenames-f
		(insert "%%%Filename: " the-file "\n"))
	    (insert-file-contents the-file nil)
	    (goto-char 1))
	(setq the-warnings
	      (concat the-warnings "Could not read file: " the-name "\n"))))
    (if the-warnings
	(progn
	  (with-output-to-temp-buffer "*Help*"
	    (princ the-warnings))
	  (kill-buffer bib-buffer)
	  (error
	   "Sorry, can't find all bibtex files in \\bibliography command"))
      bib-buffer)))

(defun bib-bibliography-list ()
  "Return list of bib files listed in first \\bibliography command in buffer.
Similar output to AUCTeX's LaTeX-bibliography-list
The first element may contain trailing whitespace (if there was any in input)
although BiBTeX doesn't allow it!"
  (save-excursion
    (goto-char 1)
    (if (not (re-search-forward "^[ \t]*\\\\bibliography{[ \t]*\\([^},]+\\)"
				nil t))
	(error "Sorry, can't find \\bibliography command anywhere")
      (let ((the-list (list (match-string 1)))
	    (doNext t))
	(while doNext
	  (if (looking-at ",")
	      (setq the-list
		    (append the-list
			    (list (buffer-substring
				   (progn (skip-chars-forward ", ")(point))
				   (progn (re-search-forward "[,}]" nil t)
					  (backward-char 1)
					  (skip-chars-backward ", ")
					  (point))))))
	    (setq doNext nil)))
	(mapcar 'list the-list)))))

;; BibTeX-mode key def to create AUCTeX's parsing file.
(defun bib-create-auto-file ()
  "Force the creation of the AUCTeX auto file for a bibtex buffer."
  (interactive)
  (if (not (require 'latex))
      (error "Sorry, This is only useful if you have AUCTeX"))
  (let ((TeX-auto-save t)
       (TeX-auto-update t)
       (TeX-auto-regexp-list BibTeX-auto-regexp-list))
    ;; TeX-auto-write
    ;; -> calls TeX-auto-store
    ;;    -> calls TeX-auto-parse
    ;;       clears LaTeX-auto-bibtem (temporary holding space for bibitems)
    ;;       searches buffer using regexp in TeX-auto-regexp-list
    ;;    -> if LaTeX-auto-bibtem (the temporary holding space for bibitems)
    ;;       holds stuffs like
    ;;         ("Zimmermann:1991" "Anger_et_al:1993")
    ;;       as determined by
    ;;         (member nil (mapcar 'TeX-auto-entry-clear-p TeX-auto-parser))
    ;;       then it creates the auto file.

    ;; TeX-auto-write may call TeX-master-file which may fail if
    ;; TeX-header-end is unset (by LaTeX-common-initialization in latex-mode)
    (if (not TeX-header-end)
	(setq TeX-header-end LaTeX-header-end))

    (TeX-auto-write)))

;; --------------------------------------------------------------------------
;; The following routines are also defined in other packages...

(defun psg-checkfor-file-list (filename list)
  "Check for presence of FILENAME in directory LIST.  Return 1st found path."
  ;;USAGE: (psg-checkfor-file-list "gri" (psg-list-env "PATH"))
  ;;USAGE: (psg-checkfor-file-list "gri-mode.el" load-path)
  ;;USAGE: (psg-checkfor-file-list "gri.cmd" (psg-translate-ff-list "gri.tmp"))
  (let ((the-list list)
	(filespec))
    (while the-list
      (if (not (car the-list))          ; it is nil
	  (setq filespec (expand-file-name filename))
	(setq filespec
	      (concat
	       (expand-file-name (file-name-as-directory (car the-list)))
	       filename)))
      (if (file-exists-p filespec)
	    (setq the-list nil)
	(setq filespec nil)
	(setq the-list (cdr the-list))))
    (if filespec
	filespec
      ;; If I have not found a file yet, then check if some directories
      ;; ended in // and recurse through them.
      (let ((the-list list))
	(while the-list
	  (if (not (string-match "//$" (car the-list))) nil
	    (setq filespec (car
			    (search-directory-tree
			     (substring (car the-list) 0 (match-beginning 0))
			     (concat "^" filename "$")
			     t
			     t)))
	    (if filespec                ;Success!
		(setq the-list nil)))
	  (setq the-list (cdr the-list)))
	filespec))))


(defun search-directory-tree (directories extension-regexp recurse first-file)
  "Return a list of all reachable files in DIRECTORIES ending with EXTENSION.
DIRECTORIES is a list or a single-directory string
EXTENSION-REGEXP is actually (any) regexp, usually \\\\.bib$
If RECURSE is t, then we will recurse into the directory tree,
	      nil, we will only search the list given.
If FIRST-FILE is t, stop after first file is found."
  (or (listp directories)
      (setq directories (list directories)))

  (let (match)
    (while directories
      (let* ((directory (file-name-as-directory  (car directories)))
	     (content (and directory
			   (file-readable-p directory)
			   (file-directory-p directory)
			   (directory-files directory))))
	(setq directories (cdr directories))
	(while content
	  (let ((file (expand-file-name (car content) directory)))
	    (cond ((string-match "[.]+$" (car content))) ;This or parent dir
		  ((not (file-readable-p file)))
		  ((and recurse
			(file-directory-p file))
		   (setq directories
			 (cons (file-name-as-directory file) directories)))
		  ((string-match extension-regexp
				 (file-name-nondirectory file))
		   (and first-file
			(setq content nil
			      directories nil))
		   (setq match (cons file match)))))
	  (setq content (cdr content)))))

    match))

;;; (defun psg-checkfor-file-list (filename list)
;;;   (let ((the-list list)
;;;         (filespec))
;;;     (while the-list
;;;       (if (not (car the-list))          ; it is nil
;;;           (setq filespec (concat "~/" filename))
;;;         (setq filespec
;;;               (concat (file-name-as-directory (car the-list)) filename)))
;;;       (if (file-exists-p filespec)
;;;             (setq the-list nil)
;;;         (setq filespec nil)
;;;         (setq the-list (cdr the-list))))
;;;     filespec))

(or (fboundp 'dired-replace-in-string)
    ;; This code is part of GNU emacs
    (defun dired-replace-in-string (regexp newtext string)
      ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
      ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
      (let ((result "") (start 0) mb me)
	(while (string-match regexp string start)
	  (setq mb (match-beginning 0)
		me (match-end 0)
		result (concat result (substring string start mb) newtext)
		start me))
	(concat result (substring string start)))))


;; Could use fset here to equal TeX-split-string to dired-split if only
;; dired-split is defined.  That would eliminate a check in psg-list-env.
(and (not (fboundp 'TeX-split-string))
     (not (fboundp 'dired-split))
     ;; This code is part of AUCTeX
     (defun TeX-split-string (char string)
       "Returns a list of strings. given REGEXP the STRING is split into
sections which in string was seperated by REGEXP.

Examples:

      (TeX-split-string \"\:\" \"abc:def:ghi\")
	  -> (\"abc\" \"def\" \"ghi\")

      (TeX-split-string \" *\" \"dvips -Plw -p3 -c4 testfile.dvi\")

	  -> (\"dvips\" \"-Plw\" \"-p3\" \"-c4\" \"testfile.dvi\")

If CHAR is nil, or \"\", an error will occur."

       (let ((regexp char)
	     (start 0)
	     (result '()))
	 (while (string-match regexp string start)
	   (let ((match (string-match regexp string start)))
	     (setq result (cons (substring string start match) result))
	     (setq start (match-end 0))))
	 (setq result (cons (substring string start nil) result))
	 (nreverse result))))

(defun bib-cite-file-directory-p (file)
  "Like default `file-directory-p' but allow FILE to end in // for ms-windows."
  (save-match-data
    (if (string-match "\\(.*\\)//$" file)
	(file-directory-p (match-string 1 file))
      (file-directory-p file))))

(defun psg-list-env (env)
  "Return a list of directory elements in ENV variable (w/o leading $)
argument may consist of environment variable plus a trailing directory, e.g.
HOME or HOME/bin (trailing directory not supported in dos or OS/2).

bib-dos-or-os2-variable affects:
  path separator used (: or ;)
  whether backslashes are converted to slashes"
  (if (not (getenv env))
      nil                               ;Because dired-replace-in-string fails
    (let* ((value (if bib-dos-or-os2-variable
		      (dired-replace-in-string "\\\\" "/" (getenv env))
		    (getenv env)))
	   (sep-char (or (and bib-dos-or-os2-variable ";") ":"))
	   (entries (and value
			 (or (and (fboundp 'TeX-split-string)
				  (TeX-split-string sep-char value))
			     (dired-split sep-char value)))))
      (loop for x in entries if (bib-cite-file-directory-p x) collect x))))

(provide 'bib-cite)
;;; bib-cite.el ends here
