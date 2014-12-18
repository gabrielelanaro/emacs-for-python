 This library provides three kinds of changes to the text cursor:

 1. When a buffer is read-only or is in overwrite mode, the cursor
    type changes to `curchg-overwrite/read-only-cursor-type'.  This
    is controlled by command `change-cursor-mode' and user option
    `curchg-change-cursor-on-overwrite/read-only-flag'.

 2. When an input method is in use, the cursor color changes to
    `curchg-input-method-cursor-color'.  This is controlled by
    command `change-cursor-mode' and user option
    `curchg-change-cursor-on-input-method-flag'.

 3. When Emacs is idle, the cursor type changes to
    `curchg-idle-cursor-type'.  This is controlled by command
    `toggle-cursor-type-when-idle'.

 To turn on all three types of cursor change by default, put the
 following in your Emacs init file (~/.emacs):

   (require 'cursor-chg)  ; Load this library
   (change-cursor-mode 1) ; On for overwrite/read-only/input mode
   (toggle-cursor-type-when-idle 1) ; On when idle

 Note: Library `oneonone.el' provides the same functionality as
 library `cursor-chg.el', and more.  If you use library
 `oneonone.el', then do NOT also use library `cursor-chg.el'.

 Note for Emacs 20: There is a bug in Emacs 20 which can lead to a
 fatal error (Emacs crash) when using `query-replace' with
 idle-cursor change enabled.  If you use Emacs 20, then consider
 using `toggle-cursor-type-when-idle' to disable idle-cursor change
 while you use `query-replace'.

 User options defined here:

   `curchg-change-cursor-on-input-method-flag',
   `curchg-change-cursor-on-overwrite/read-only-flag',
   `curchg-default-cursor-color', `curchg-default-cursor-type',
   `curchg-idle-cursor-type', `curchg-input-method-cursor-color',
   `curchg-overwrite/read-only-cursor-type'.

 Commands defined here:

   `change-cursor-mode', `curchg-change-cursor-when-idle-interval',
   `curchg-set-cursor-type', `curchg-toggle-cursor-type-when-idle',
   `set-cursor-type', `toggle-cursor-type-when-idle'.

 Internal variables defined here:

   `curchg-change-cursor-when-idle-p', `curchg-idle-interval',
   `curchg-idle-timer', `curchg-last-cursor-type'.

 Non-interactive functions defined here:

   `curchg-change-cursor-on-input-method',
   `curchg-change-cursor-on-overwrite/read-only',
   `curchg-change-cursor-to-idle-type',
   `curchg-change-cursor-to-idle-type-off'.

 Acknowledgements:

 The cursor-changing on input method and read-only was inspired by
 Juri Linkov <juri@jurta.org>.  Joe Casadonte <joc@netaxs.com>
 wrote a similar hook (`joc-cursor-type-set-hook'), which he got
 from Steve Kemp...
