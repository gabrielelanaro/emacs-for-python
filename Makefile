# Byte compile emacs files under this directories

YASNIPPET = yasnippet/dropdown-list.elc			\
yasnippet/yasnippet-debug.elc				\
yasnippet/yasnippet.elc					

AUTOCOMPLETE = auto-complete/auto-complete-config.elc	\
auto-complete/auto-complete.elc				\
auto-complete/fuzzy.elc					\
auto-complete/popup.elc

PLUGINS = plugins/open-next-line.elc			\
plugins/pabbrev.elc				\
plugins/python-flymake.elc			\
plugins/python-outline.elc			\
plugins/python-testing.elc			\
plugins/utils.elc				\
plugins/virtualenv.elc


all : python-collection.elc yasnippet autocomplete plugins

yasnippet : $(YASNIPPET)

autocomplete : $(AUTOCOMPLETE)

plugins : $(PLUGINS)

%.elc : %.el
	emacs -batch -l python-collection.el -f batch-byte-compile $^

clean:
	find . -name \*.elc | xargs rm