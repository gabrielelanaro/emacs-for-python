# Rules to generate the files that need to go into the ELPA package.

# Files we need to auto-generate:
#   dir
#   auctex.info
#   preview-latex.info
#   README
#   auctex.el (or auctex-pkg.el)?
#   tex-site.el
#   doc: preview-dtxdoc.texi
#   doc: version.texi
#   latex: prauctex.cfg
#   latex: prauctex.def
#   latex: prcounters.def
#   latex: preview.sty
#   latex: prfootnotes.def
#   latex: prlyx.def
#   latex: prshowbox.def
#   latex: prshowlabels.def
#   latex: prtightpage.def
#   latex: prtracingall.def


MAKEINFO=makeinfo
INSTALL_INFO=install-info
PERL=perl

MANUALS=auctex preview-latex
INFO_FILES=$(MANUALS:=.info)

TEXMFGEN:=$(shell sed -n 's/^%<installer>.*file[{]\([^}.]*\.[sdc][tef][yfg]\)[}].*/\1/p' latex/preview.dtx)
LATEX_FILES:=$(patsubst %, latex/%, $(shell echo $$(echo "$(TEXMFGEN)")))

GENERATED_FILES=dir			\
		$(INFO_FILES)		\
		README			\
		tex-site.el		\
		doc/version.texi	\
		doc/preview-dtxdoc.texi	\
		$(LATEX_FILES)

all: $(GENERATED_FILES)

clean:
	rm -f $(GENERATED_FILES)

# Copied&adapted from doc/Makefile.in.
MAKEINFO_PLAIN=$(MAKEINFO) -D rawfile --no-headers
README: doc/intro.texi doc/preview-readme.texi doc/macros.texi
	(cd doc; $(MAKEINFO_PLAIN) intro.texi --output -) >$@
	(cd doc; $(MAKEINFO_PLAIN) preview-readme.texi --output -) >> $@

# Commands copied&adapted from autogen.sh and doc/Makefile.in.
AUCTEXDATE:=$(shell LANG=C sed -n '1s/^\([-0-9][-0-9]*\).*/\1/p' ChangeLog)
THISVERSION:=$(shell sed -n '2,/^[0-9]/s/.*Version \(.*\) released\..*/\1/p' ChangeLog)
LASTVERSION:=$(shell sed -n '/.*Version .* released\./{s/.*Version \(.*\) released\..*/\1/p;q}' ChangeLog)
AUCTEXVERSION:=$(if $(THISVERSION),$(THISVERSION),$(LASTVERSION).$(AUCTEXDATE))

tex-site.el: tex-site.el.in
	sed -e 's|@lisppackagelispdir@|(file-name-directory load-file-name)|'\
	    -e 's|@lisppackagedatadir@|(file-name-directory load-file-name)|'\
	    -e 's|@lispautodir@|(if (file-writable-p "/usr/local/var/auctex") "/usr/local/var/auctex" "~/.emacs.d/auctex")|'\
	    -e 's|@AUCTEXVERSION@|$(AUCTEXVERSION)|'\
	    -e 's|@AUCTEXDATE@|$(AUCTEXDATE)|'\
	    $< >$@

doc/version.texi: ChangeLog
	echo @set VERSION $(AUCTEXVERSION) >$@
	echo @set UPDATED $(AUCTEXDATE) >>$@

# Copied&adapted from doc/Makefile.in.
doc/preview-dtxdoc.texi: latex/preview.dtx doc/preview-dtxdoc.pl
	$(PERL) doc/preview-dtxdoc.pl latex/preview.dtx $@

# Copied&adapted from doc/Makefile.in.
TEXI_SOURCES:=$(wildcard doc/*.texi) doc/version.texi doc/preview-dtxdoc.texi
$(INFO_FILES): %.info: $(TEXI_SOURCES)
	cd doc; $(MAKEINFO) --no-split $*.texi
	mv doc/$*.info $@

dir: $(INFO_FILES)
	for f in $(INFO_FILES); do $(INSTALL_INFO) --info-dir=. $$f; done

$(LATEX_FILES): latex/preview.dtx latex/bootstrap.ins
	cd latex; $(TEX) '\nonstopmode \input bootstrap.ins'
	cd latex; $(TEX) '\nonstopmode \input preview-mk.ins'
