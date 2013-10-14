;;; devices.el --- XEmacs device API emulation

;; Copyright (c) 1996-1999, 2013 Free Software Foundation, Inc.

;; Author: $Author: wmperry $
;; Created: $Date: 2002/02/01 17:42:48 $
;; Keywords: 

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a complete implementation of all the device-* functions found in
;; XEmacs 19.14.  A 'device' for Emacs 19 is just a frame, from which we can
;; determine the connection to an X display, etc.

;;; Code:

(eval-when-compile
  (require 'cl)
  (when (featurep 'xemacs)
    (set 'byte-optimize nil)))
    
(when (not (featurep 'xemacs))
  (defalias 'selected-device 'ignore)
  (defalias 'device-or-frame-p 'framep)
  (defalias 'device-console 'ignore)
  (defalias 'device-sound-enabled-p 'ignore)
  (defalias 'device-live-p 'frame-live-p)
  (defalias 'devicep 'framep)
  (defalias 'frame-device 'identity)
  (defalias 'redisplay-device 'redraw-frame)
  (defalias 'redraw-device 'redraw-frame)
  (defalias 'select-device 'select-frame)
  (defalias 'set-device-class 'ignore)

  (defun make-device (type connection &optional props)
    "Create a new device of type TYPE, attached to connection CONNECTION.

The valid values for CONNECTION are device-specific; however,
CONNECTION is generally a string. (Specifically, for X devices,
CONNECTION should be a display specification such as \"foo:0\", and
for TTY devices, CONNECTION should be the filename of a TTY device
file, such as \"/dev/ttyp4\", or nil to refer to XEmacs' standard
input/output.)

PROPS, if specified, should be a plist of properties controlling
device creation.

If CONNECTION specifies an already-existing device connection, that
device is simply returned; no new device is created, and PROPS
have no effect."
    (cond
     ((and (eq type 'x) connection)
      (make-frame-on-display connection props))
     ((eq type 'x)
      (make-frame props))
     ((eq type 'tty)
      nil)
     (t
      (error "Unsupported device-type: %s" type))))

  (defun make-frame-on-device (type connection &optional props)
    "Create a frame of type TYPE on CONNECTION.
TYPE should be a symbol naming the device type, i.e. one of

x	An X display.  CONNECTION should be a standard display string
	such as \"unix:0\", or nil for the display specified on the
	command line or in the DISPLAY environment variable.  Only if
	support for X was compiled into	XEmacs.
tty	A standard TTY connection or terminal.  CONNECTION should be
	a TTY device name such as \"/dev/ttyp2\" (as determined by
	the Unix command `tty') or nil for XEmacs' standard input
	and output (usually the TTY in which XEmacs started).  Only
	if support for TTY's was compiled into XEmacs.
ns	A connection to a machine running the NeXTstep windowing
	system.  Not currently implemented.
win32	A connection to a machine running Microsoft Windows NT or
	Windows 95.  Not currently implemented.
pc	A direct-write MS-DOS frame.  Not currently implemented.

PROPS should be an plist of properties, as in the call to `make-frame'.

If a connection to CONNECTION already exists, it is reused; otherwise,
a new connection is opened."
    (make-device type connection props))

  (defun make-tty-device (&optional tty terminal-type)
    "Create a new device on TTY.
  TTY should be the name of a tty device file (e.g. \"/dev/ttyp3\" under
SunOS et al.), as returned by the `tty' command.  A value of nil means
use the stdin and stdout as passed to XEmacs from the shell.
  If TERMINAL-TYPE is non-nil, it should be a string specifying the
type of the terminal attached to the specified tty.  If it is nil,
the terminal type will be inferred from the TERM environment variable."
    (make-device 'tty tty (list 'terminal-type terminal-type)))

  (defun make-x-device (&optional display)
    (make-device 'x display))

  (defun set-device-selected-frame (device frame)
    "Set the selected frame of device object DEVICE to FRAME.
If DEVICE is nil, the selected device is used.
If DEVICE is the selected device, this makes FRAME the selected frame."
    (select-frame frame))

  (defun set-device-baud-rate (device rate)
    "Set the output baud rate of DEVICE to RATE.
On most systems, changing this value will affect the amount of padding
and other strategic decisions made during redisplay."
    (setq baud-rate rate))

  (defun dfw-device (obj)
    "Given a device, frame, or window, return the associated device.
Return nil otherwise."
    (cond
     ((windowp obj)
      (window-frame obj))
     ((framep obj)
      obj)
     (t
      nil)))

  (defun event-device (event)
    "Return the device that EVENT occurred on.
This will be nil for some types of events (e.g. keyboard and eval events)."
    (dfw-device (posn-window (event-start event))))

  (defun device-connection (&optional device)
    "Return the connection of the specified device.
DEVICE defaults to the selected device if omitted"
    (or (cdr-safe (assq 'display (frame-parameters device))) "stdio"))

  (defun find-device (connection &optional type)
    "Look for an existing device attached to connection CONNECTION.
Return the device if found; otherwise, return nil.

If TYPE is specified, only return devices of that type; otherwise,
return devices of any type. (It is possible, although unlikely,
that two devices of different types could have the same connection
name; in such a case, the first device found is returned.)"
    (let ((devices (device-list))
	  (retval nil))
      (while (and devices (not nil))
	(if (equal connection (device-connection (car devices)))
	    (setq retval (car devices)))
	(setq devices (cdr devices)))
      retval))

  (defalias 'get-device 'find-device)

  (defmacro device-baud-rate (&optional device)
    "Return the output baud rate of DEVICE."
    'baud-rate)

  (defun device-on-window-system-p (&optional device)
    "Return non-nil if DEVICE is on a window system.
This generally means that there is support for the mouse, the menubar,
the toolbar, glyphs, etc."
    (and (cdr-safe (assq 'display (frame-parameters device))) t))

  (defun device-name (&optional device)
    "Return the name of the specified device."
    (or (cdr-safe (assq 'display (frame-parameters device))) "stdio"))

  (defun device-frame-list (&optional device)
    "Return a list of all frames on DEVICE.
If DEVICE is nil, the selected device will be used."
    (let ((desired (device-connection device)))
      (filtered-frame-list (function (lambda (x) (equal (device-connection x)
							desired))))))
  (defun device-list ()
    "Return a list of all devices"
    (let ((seen nil)
	  (cur nil)
	  (conn nil)
	  (retval nil)
	  (not-heard (frame-list)))
      (while not-heard
	(setq cur (car not-heard)
	      conn (device-connection cur)
	      not-heard (cdr not-heard))
	(if (member conn seen)
	    nil				; Already got it
	  (setq seen (cons conn seen)	; Whoo hoo, a new one!
		retval (cons cur retval))))
      retval))

  (defvar delete-device-hook nil
    "Function or functions to call when a device is deleted.
One argument, the to-be-deleted device.")

  (defun delete-device (device &optional force)
    "Delete DEVICE, permanently eliminating it from use.
Normally, you cannot delete the last non-minibuffer-only frame (you must
use `save-buffers-kill-emacs' or `kill-emacs').  However, if optional
second argument FORCE is non-nil, you can delete the last frame. (This
will automatically call `save-buffers-kill-emacs'.)"
    (let ((frames (device-frame-list device)))
      (run-hook-with-args 'delete-device-hook device)
      (while frames
	(delete-frame (car frames) force)
	(setq frames (cdr frames)))))

  (defun device-color-cells (&optional device)
    (case window-system
      ((x win32 w32 pm) (x-display-color-cells device))
      (otherwise 1)))

  (defun device-pixel-width (&optional device)
    (case window-system
      ((x win32 w32 pm) (x-display-pixel-width device))
      (otherwise (frame-width device))))

  (defun device-pixel-height (&optional device)
    (case window-system
      ((x win32 w32 pm) (x-display-pixel-height device))
      (otherwise (frame-height device))))

  (defun device-mm-width (&optional device)
    (case window-system
      ((x win32 w32 pm) (x-display-mm-width device))
      (otherwise nil)))

  (defun device-mm-height (&optional device)
    (case window-system
      ((x win32 w32 pm) (x-display-mm-height device))
      (otherwise nil)))

  (defun device-bitplanes (&optional device)
    (case window-system
      ((x win32 w32 pm) (x-display-planes device))
      (otherwise 2)))

  (defun device-class (&optional device)
    (if (fboundp 'display-color-p)
	(if (display-color-p device)
	    'color
	  (if (display-grayscale-p device)
	      'grayscale
	    'mono))
      (case window-system
	(x				; X11
	 (cond
	  ((fboundp 'x-display-visual-class)
	   (let ((val (symbol-name (x-display-visual-class device))))
	     (cond
	      ((string-match "color" val) 'color)
	      ((string-match "gray-scale" val) 'grayscale)
	      (t 'mono))))
	  ((fboundp 'x-display-color-p)
	   (if (x-display-color-p device)
	       'color
	     'mono))
	  (t 'color)))
	(pm				; OS/2 Presentation Manager
	 (cond
	  ((fboundp 'pm-display-visual-class)
	   (let ((val (symbol-name (pm-display-visual-class device))))
	     (cond
	      ((string-match "color" val) 'color)
	      ((string-match "gray-scale" val) 'grayscale)
	      (t 'mono))))
	  ((fboundp 'pm-display-color-p)
	   (if (pm-display-color-p device)
	       'color
	     'mono))
	  (t 'color)))
	(otherwise 'color))))

  (defun device-class-list ()
    "Returns a list of valid device classes."
    (list 'color 'grayscale 'mono))

  (defun valid-device-class-p (class)
    "Given a CLASS, return t if it is valid.
Valid classes are 'color, 'grayscale, and 'mono."
    (memq class (device-class-list)))

  (defun device-or-frame-type (device-or-frame)
    "Return the type (e.g. `x' or `tty') of DEVICE-OR-FRAME.
DEVICE-OR-FRAME should be a device or a frame object.  See `device-type'
for a description of the possible types."
    (or window-system 'tty))

  (defun device-type (&optional device)
    "Return the type of the specified device (e.g. `x' or `tty').
Value is `tty' for a tty device (a character-only terminal),
`x' for a device which is a connection to an X server,
'ns' for a device which is a connection to a NeXTStep dps server,
'win32' or 'w32' for a Windows-NT window,
'pm' for an OS/2 Presentation Manager window,
'intuition' for an Amiga screen"
    (device-or-frame-type device))

  (defun device-type-list ()
    "Return a list of valid console types."
    (if window-system
	(list window-system 'tty)
      (list 'tty)))

  (defun valid-device-type-p (type)
    "Given a TYPE, return t if it is valid."
    (memq type (device-type-list)))

  )   ; This closes the conditional on whether we are in XEmacs or not

(provide 'devices)

(eval-when-compile
  (when (featurep 'xemacs)
    (set 'byte-optimize t)))
