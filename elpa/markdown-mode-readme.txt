markdown-mode is a major mode for editing [Markdown][]-formatted
text files in GNU Emacs.  markdown-mode is free software, licensed
under the GNU GPL.

 [Markdown]: http://daringfireball.net/projects/markdown/

The latest stable version is markdown-mode 2.0, released on March 24, 2013:

   * [markdown-mode.el][]
   * [Screenshot][][^theme]
   * [Release notes][]

 [markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el
 [screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20130131-002.png
 [release notes]: http://jblevins.org/projects/markdown-mode/rev-2-0

[^theme]: The theme used in the screenshot is
  [color-theme-twilight](https://github.com/crafterm/twilight-emacs).

markdown-mode is also available in several package managers, including:

   * Debian and Ubuntu Linux: [emacs-goodies-el][]
   * RedHat and Fedora Linux: [emacs-goodies][]
   * NetBSD: [textproc/markdown-mode][]
   * Arch Linux (AUR): [emacs-markdown-mode-git][]
   * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
   * FreeBSD: [textproc/markdown-mode.el][freebsd-port]

 [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
 [emacs-goodies]: https://admin.fedoraproject.org/pkgdb/acls/name/emacs-goodies
 [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
 [emacs-markdown-mode-git]: http://aur.archlinux.org/packages.php?ID=30389
 [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
 [macports-ticket]: http://trac.macports.org/ticket/35716
 [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el

The latest development version can be downloaded directly
([markdown-mode.el][devel.el]) or it can be obtained from the
(browsable and clonable) Git repository at
<http://jblevins.org/git/markdown-mode.git>.  The entire repository,
including the full project history, can be cloned via the Git protocol
by running

    git clone git://jblevins.org/git/markdown-mode.git

 [devel.el]: http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el

Installation:

Make sure to place `markdown-mode.el` somewhere in the load-path and add
the following lines to your `.emacs` file to associate markdown-mode
with `.text`, `.markdown`, and `.md` files:

    (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

There is no official Markdown file extension, nor is there even a
_de facto_ standard, so you can easily add, change, or remove any
of the file extensions above as needed.
