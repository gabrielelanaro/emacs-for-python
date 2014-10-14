Quickstart

    (require 'express)
    (express-install-aliases)

    (express "important message")

    (with-message-logonly
      (do-something-noisy))

Explanation

Express.el provides alternatives to Emacs' built-in `message'
function.

This library is generally only useful when programming in Emacs
Lisp.  However, some end-users may find it useful to control
messaging, especially for the case of quietening chatty libraries
in their ~/.emacs files (see below).

The principal `express' function by default works differently from
`message' in almost every respect, displaying with sound and visual
highlight, and not writing to the log.  See the `express' docstring
for details.  The variant function `express*' has identical
functionality, but accepts CL-style arguments.

The following functions provided by this library are drop-in
alternatives to `message' which may be useful in an `flet'
construct:

    `express-message-nolog'
    `express-message-logonly'
    `express-message-highlight'
    `express-message-insert'
    `express-message-notify'
    `express-message-popup'
    `express-message-temp'
    `express-message-string'

The following macros modify the behavior of `message' within
the enclosing expression:

    `express-with-message-nolog'
    `express-with-message-logonly'
    `express-with-message-highlight'
    `express-with-message-insert'
    `express-with-message-notify'
    `express-with-message-popup'
    `express-with-message-temp'
    `express-with-message-string'

For example, the following code would redirect messages from a very
chatty library to the log:

    (express-with-message-nolog
      (require 'very-chatty-library))

The same method may also be handy with `defadvice':

    (defadvice very-chatty-function (around very-chatty-redirect activate)
      (express-with-message-nolog
        ad-do-it))

Similarly, important messages may be redirected to a more visible
form:

    (defadvice an-important-function (around an-important-function activate)
      (express-with-message-notify
        ad-do-it))

To use `express', place the express.el library somewhere Emacs can find
it, and add the following to your ~/.emacs file:

    (require 'express)
    (express-install-aliases)     ; optionally, can also be set in customize

Running `express-install-aliases' or setting the corresponding
variable in customize will install convenience aliases outside
the "express-" namespace.  This is disabled by default.

See Also

    M-x customize-group RET express RET
    M-x customize-group RET notify RET
    M-x customize-group RET popup RET

Notes

    The function `express-message-noformat' is also available, but it
    is not quite a drop-in replacement for `message'.

    Some of the functions require the availability of notify.el,
    todochiku.el or popup.el.  In all cases, the function will
    degrade to an ordinary message if the external library is not
    present.

Compatibility and Requirements

    GNU Emacs version 24.4-devel     : yes, at the time of writing
    GNU Emacs version 24.3           : yes
    GNU Emacs version 23.3           : yes
    GNU Emacs version 22.2           : yes, with some limitations
    GNU Emacs version 21.x and lower : unknown

    Uses if present: string-utils.el, notify.el, todochiku.el,
                     popup.el

Bugs

    Soft dependency on unpublished popup-volatile.

    `message' is a subr.  Macros such as `express-with-message-logonly'
    will only affect calls to `message' from Lisp.

TODO

    Aliases are not turning on from customize setting alone.  The
    variable express-install-short-aliases does not seem to be
    set after loading `custom-file'.

    Truncation options based on string-utils.el

    Default icons and timeouts for notifications.

License

Simplified BSD License:

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

   1. Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials
      provided with the distribution.

This software is provided by Roland Walker "AS IS" and any express
or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular
purpose are disclaimed.  In no event shall Roland Walker or
contributors be liable for any direct, indirect, incidental,
special, exemplary, or consequential damages (including, but not
limited to, procurement of substitute goods or services; loss of
use, data, or profits; or business interruption) however caused
and on any theory of liability, whether in contract, strict
liability, or tort (including negligence or otherwise) arising in
any way out of the use of this software, even if advised of the
possibility of such damage.

The views and conclusions contained in the software and
documentation are those of the authors and should not be
interpreted as representing official policies, either expressed
or implied, of Roland Walker.
