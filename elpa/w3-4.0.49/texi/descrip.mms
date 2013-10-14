# where the w3 lisp files should go
prefix  = gnu_root
datadir = $(prefix):[lib]
infodir = $(prefix):[info]

ECHO       = write sys$output
MAKEINFO   = makeinfo
TEXI2HTML  = texi2html
TEXI2HTML_FLAGS = 
RM         = delete
MKDIR      = create/dir

############## no user servicable parts beyond this point ###################
.SUFFIXES: .txi .dvi .info .html

%.html: %.txi
	$(TEXI2HTML) $(TEXI2HTML_FLAGS) $(MMS$SOURCE)

%.dvi: %.txi
        tex $(MMS$SOURCE)
	texindex *.cp *.fn *.ky *.pg *.tp *.vr
	tex $(MMS$SOURCE)
	$(RM) 	*.cp  *.fn  *.ky  *.pg  *.tp  *.vr 	\
		*.cps *.fns *.kys *.pgs *.tps *.vrs	\
		*.log *.toc *.aux

%.info: %.txi
	$(MAKEINFO) $(MMS$SOURCE)

all: info

install:
	if f$parse("$(infodir)") .eqs. "" then $(MKDIR) $(infodir)
	copy/log *.info* $(infodir)
	- purge/log $(infodir)

distclean: clean
	$(RM) Makefile

clean:
	$(RM) *.dvi *.info* *.html

html: $(MANUALS:.txi=_toc.html)

dvi: $(MANUALS:.txi=.dvi)

info: $(MANUALS:.txi=.info)
