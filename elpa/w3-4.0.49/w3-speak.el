;;; w3-speak.el --- Emacs-W3 speech interface

;; Copyright (c) 1997, 1998, 2013 Free Software Foundation, Inc.

;; Author: wmperry
;; Original author: William Perry --<wmperry@cs.indiana.edu>
;; Cloned from emacspeak-w3.el
;; Created: 1996/10/16 20:56:40
;; Keywords: hypermedia, speech

;; This file is not part of GNU Emacs, but the same permissions apply.
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

;; A replacement module for emacspeak-w3 that uses all the new functionality
;; of Emacs/W3 3.0.
;;
;; This file would not be possible without the help of
;; T.V. Raman (raman@adobe.com) and his continued efforts to make Emacs/W3
;; even remotely useful. :)

;; This conforms to http://www4.inria.fr/speech2.html

;;; Code:

(require 'widget)
(require 'w3-forms)
(require 'w3-cus)
(require 'w3-keymap)
(require 'advice)
;; This condition-case needs to be here or it completely chokes
;; byte-compilation for people who do not have Emacspeak installed.
;; *sigh*
(condition-case ()
    (progn
      (require 'emacspeak)
      (require 'dtk-voices)
      (require 'emacspeak-speak)
      (require 'emacspeak-sounds)
      ;; (eval-when (compile)
      ;;   	 (require 'emacspeak-fix-interactive))
      )
  (error (message "Emacspeak not found - speech will not work.")))


;;{{{  speaking form fields 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now for the guts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-speak-summarize-form-field ()
  "Summarizes field under point if any."
  (let ((widget (widget-at (point))))
    (and widget (w3-form-summarize-field widget))))

;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement notification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice w3-widget-forward (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-widget-summarize (widget-at  (point )))))

(defadvice w3-widget-backward (after emacspeak pre act comp)
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-widget-summarize (widget-at  (point )))))

(defadvice w3-scroll-up (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
	(let ((start (point )))
	  (emacspeak-auditory-icon 'scroll)
	  (save-excursion
	    (forward-line (window-height))
	    (emacspeak-speak-region start (point ))))))

(defadvice w3-revert-form (after emacspeak pre act)
  "Announce that you cleared the form. "
  (dtk-speak "Cleared the form. "))

(defadvice w3-finish-text-entry (after emacspeak pre act )
  "Announce what the field was set to."
  (when (interactive-p)
    (w3-speak-summarize-form-field)))

(defadvice w3-start-of-document (after emacspeak pre act)
  "Produce an auditory icon.  Also speak the first line. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice w3-end-of-document (after emacspeak pre act)
  "Produce an auditory icon.  Also speak the first line."
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice w3-goto-last-buffer (after emacspeak pre act)
  "Speak the modeline so I know where I am."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice w3-quit (after emacspeak pre act)
  "Speak the mode line of the new buffer."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defvar dtk-punctuation-mode)
(defvar voice-lock-mode)

(defadvice w3-fetch (around  emacspeak  act comp )
  "First produce an auditory icon to indicate retrieval.
After retrieval, 
set `voice-lock-mode' to t after displaying the buffer,
and then speak the mode-line."
  (emacspeak-auditory-icon 'select-object)
  ad-do-it)

(defun w3-speak-mode-hook ()
  (set (make-local-variable 'voice-lock-mode) t)
  (setq dtk-punctuation-mode "some")
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

;; This is really the only function you should need to call unless
;; you are adding functionality.
(defun w3-speak-use-voice-locking (&optional arg) 
  "Tells w3 to start using voice locking.
This is done by setting the w3 variables so that anchors etc are not marked by
delimiters.  We then turn on `voice-lock-mode'.
Interactive prefix arg does the opposite."
  (interactive "P")
  (setq w3-echo-link 'text)
  (if arg
      (remove-hook 'w3-mode-hook 'w3-speak-mode-hook)
    (add-hook 'w3-mode-hook 'w3-speak-mode-hook)))

(defun w3-speak-browse-page ()
  "Browse a WWW page."
  (interactive)
  (emacspeak-audio-annotate-paragraphs)
  (emacspeak-execute-repeatedly 'forward-paragraph))

(define-key w3-mode-map "." 'w3-speak-browse-page)

(defvar w3-speak--last-progress-indication 0
  "Caches when we last produced a progress auditory icon.")

(defadvice url-lazy-message (around emacspeak pre act)
  "Provide pleasant auditory feedback about progress."
  (let ((now (nth 1 (current-time))))
    (when (> now
	     (+ 3 w3-speak--last-progress-indication))
	  (setq w3-speak--last-progress-indication now)
	  (apply 'message (ad-get-args 0))
	  (emacspeak-auditory-icon 'progress))))

(provide 'w3-speak)
