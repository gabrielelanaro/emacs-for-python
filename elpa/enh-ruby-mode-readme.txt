This is a fork of http://http://github.com/jacott/Enhanced-Ruby-Mode
to provide further enhancements and bug fixes.

It has been renamed to enh-ruby-mode.el to avoid name conflicts
with ruby-mode that ships with emacs. All symbols that started with
'ruby now start with 'enh-ruby. This also makes it possible to
switch back and forth for testing purposes.

Provides fontification, indentation, syntax checking, and navigation for Ruby code.

If you're installing manually, you should add this to your .emacs
file after putting it on your load path:

   (add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode") ; must be added after any path containing old ruby-mode
   (setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby") ; so that still works if ruby points to ruby1.8


(require 'cl) ; for cdddr, caddr
