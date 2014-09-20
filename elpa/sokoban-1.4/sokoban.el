;;; sokoban.el --- Implementation of Sokoban for Emacs.

;; Copyright (C) 1998, 2013 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn.clements@xemacs.org>
;; Version: 1.4
;; Created: 1997-09-11
;; Keywords: games
;; Package-Type: multi

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; Modified: 1998-01-09, conditionalised use of locate-data-directory
;; Modified: 1998-01-27, added mouse interface code
;;   (provided by Sean MacLennan <bn932@freenet.carleton.ca>
;; Modified: 1998-02-06, fixed bug, where sokoban-done wasn't reset to
;;   zero in sokoban-restart-level
;; Modified: 1998-02-27, patches from Hrvoje Niksic
;;   added bounds check to sokoban-goto-level
;;   added popup menu
;;   display level and score in modeline
;; Modified: 1998-06-04, added `undo' feature
;;   added number of blocks done/total to score and modeline
;; Modified: 2003-06-14, update email address, remove URL

;; Tested with XEmacs 20.3/4/5 and Emacs 19.34

;; The game is based upon XSokoban, by
;; Michael Bischoff <mbi@mo.math.nat.tu-bs.de>

;; The levels and some of the pixmaps were
;; taken directly from XSokoban

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-use-glyphs t
  "Non-nil means use glyphs when available.")

(defvar sokoban-use-color t
  "Non-nil means use color when available.")

(defvar sokoban-font "-*-courier-medium-r-*-*-*-200-100-75-*-*-iso8859-*"
  "Name of the font used in X mode.")

(defvar sokoban-buffer-name "*Sokoban*")

(defvar sokoban-temp-buffer-name " Sokoban-tmp")

(defvar sokoban-level-file
  (if (fboundp 'locate-data-file)
      (locate-data-file "sokoban.levels")
    (or (locate-library "sokoban.levels")
            (let ((file (expand-file-name
                         "sokoban.levels"
                         (if load-file-name
                             (file-name-directory load-file-name)))))
              (and (file-exists-p file) file))
	(expand-file-name "sokoban.levels" data-directory))))

(defvar sokoban-width 20)
(defvar sokoban-height 16)

(defvar sokoban-buffer-width 20)
(defvar sokoban-buffer-height 20)

(defvar sokoban-score-x 0)
(defvar sokoban-score-y 17)

(defvar sokoban-level-data nil)

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sokoban-floor-xpm "\
/* XPM */
static char * floor_xpm[] = {
\"32 32 1 1\",
\"  c None\",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
};
")

(defconst sokoban-target-xpm "\
/* XPM */
static char * target_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\". c black\",
\"X c yellow\",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"          ............          \",
\"          .XXXXXXXXXX.          \",
\"           .XXXXXXXX.           \",
\"            .XXXXXX.            \",
\"      ..     .XXXX.     ..      \",
\"      .X.     .XX.     .X.      \",
\"      .XX.     ..     .XX.      \",
\"      .XXX.          .XXX.      \",
\"      .XXXX.        .XXXX.      \",
\"      .XXXXX.      .XXXXX.      \",
\"      .XXXXX.      .XXXXX.      \",
\"      .XXXX.        .XXXX.      \",
\"      .XXX.          .XXX.      \",
\"      .XX.     ..     .XX.      \",
\"      .X.     .XX.     .X.      \",
\"      ..     .XXXX.     ..      \",
\"            .XXXXXX.            \",
\"           .XXXXXXXX.           \",
\"          .XXXXXXXXXX.          \",
\"          ............          \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
};
")

(defconst sokoban-wall-xpm "\
/* XPM */
static char * wall_xpm[] = {
\"32 32 2 1\",
\"  c white\",
\". c SteelBlue\",
\" .............................. \",
\". ............................ .\",
\".. .......................... . \",
\"... ........................ . .\",
\"....                        . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\"....                         . .\",
\"... . . . . . . . . . . . .   . \",
\".. . . . . . . . . . . . . .   .\",
\". . . . . . . . . . . . . . .   \",
\" . . . . . . . . . . . . . . .  \",
};
")

(defconst sokoban-block-xpm "\
/* XPM */
static char * block_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\". c black\",
\"X c yellow\",
\".............................   \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.   \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX..  \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX..  \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.X. \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.X. \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".............................XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.X.\",
\" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.X.\",
\"  .XXXXXXXXXXXXXXXXXXXXXXXXXXX..\",
\"  .XXXXXXXXXXXXXXXXXXXXXXXXXXX..\",
\"   .XXXXXXXXXXXXXXXXXXXXXXXXXXX.\",
\"   .............................\",
};
")

(defconst sokoban-player-xpm "\
/* XPM */
static char * player_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\"o c white\",
\". c black\",
\"                                \",
\"                                \",
\"                                \",
\"            oooooooo            \",
\"            o......o            \",
\"           o.oooooo.o           \",
\"           o.oooooo.o           \",
\"          o.oooooooo.o          \",
\"          o.o..oo..o.o          \",
\"          o.oooooooo.o          \",
\"          oo.o....o.oo          \",
\"         oo..oo..oo..oo         \",
\"         o....o..o....o         \",
\"         o.o..o..o..o.o         \",
\"         o.o...oo...o.o         \",
\"        o.oo........oo.o        \",
\"        o.oo........oo.o        \",
\"       o.ooo........ooo.o       \",
\"       o.ooo........ooo.o       \",
\"       o.ooo........ooo.o       \",
\"        o.oo........oo.o        \",
\"        o.oo........oo.o        \",
\"        o.o..........o.o        \",
\"         o............o         \",
\"          o..........o          \",
\"           o........o           \",
\"          o.o.oooo.o.o          \",
\"         o.....oo.....o         \",
\"        o......oo......o        \",
\"       o.......oo.......o       \",
\"      o..o..o..oo.oo..o..o      \",
\"      oooooooooooooooooooo      \",
};
")

(defconst sokoban-floor ?\+)
;; note - space character in level file is also allowed to indicate floor
(defconst sokoban-target ?\.)
(defconst sokoban-wall ?\#)
(defconst sokoban-block ?\$)
(defconst sokoban-player ?\@)
(defconst sokoban-block-on-target ?\*)

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-floor-options
  `(((glyph
      [xpm :data ,sokoban-floor-xpm])
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar sokoban-target-options
  `(((glyph
      [xpm :data ,sokoban-target-xpm])
     ((mono-x mono-tty emacs-tty) ?\.)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 1 0.5])
     (color-tty "yellow"))))

(defvar sokoban-wall-options
  `(((glyph
      [xpm :data ,sokoban-wall-xpm])
     (emacs-tty ?\X)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [0 0 1])
     (color-tty "blue"))))

(defvar sokoban-block-options
  `(((glyph
      [xpm :data ,sokoban-block-xpm])
     ((mono-x mono-tty emacs-tty) ?\O)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar sokoban-player-options
  `(((glyph
      [xpm :data ,sokoban-player-xpm])
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 1 0])
     (color-tty "green"))))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-level 0)
(make-variable-buffer-local 'sokoban-level)
(defvar sokoban-level-map nil)
(make-variable-buffer-local 'sokoban-level-map)
(defvar sokoban-targets 0)
(make-variable-buffer-local 'sokoban-targets)
(defvar sokoban-x 0)
(make-variable-buffer-local 'sokoban-x)
(defvar sokoban-y 0)
(make-variable-buffer-local 'sokoban-y)
(defvar sokoban-moves 0)
(make-variable-buffer-local 'sokoban-moves)
(defvar sokoban-pushes 0)
(make-variable-buffer-local 'sokoban-pushes)
(defvar sokoban-done 0)
(make-variable-buffer-local 'sokoban-done)
(defvar sokoban-mouse-x 0)
(make-variable-buffer-local 'sokoban-mouse-x)
(defvar sokoban-mouse-y 0)
(make-variable-buffer-local 'sokoban-mouse-y)
(defvar sokoban-undo-list nil)
(make-variable-buffer-local 'sokoban-undo-list)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-mode-map
  (let ((map (make-sparse-keymap
	      (when (featurep 'xemacs) 'sokoban-mode-map))))
    (define-key map "n"	'sokoban-start-game)
    (define-key map "r"	'sokoban-restart-level)
    (define-key map "g"	'sokoban-goto-level)

    (define-key map [left]	'sokoban-move-left)
    (define-key map [right]	'sokoban-move-right)
    (define-key map [up]	'sokoban-move-up)
    (define-key map [down]	'sokoban-move-down)

    (when (featurep 'xemacs)
      (define-key map [button2]	  'sokoban-mouse-event-start)
      (define-key map [button2up] 'sokoban-mouse-event-end))

    (define-key map [down-mouse-2] 'sokoban-mouse-event-start)
    (define-key map [mouse-2]      'sokoban-mouse-event-end)
    ;; On some systems (OS X) middle mouse is difficult.
    ;; FIXME: Use follow-link?
    (define-key map [down-mouse-1] 'sokoban-mouse-event-start)
    (define-key map [mouse-1]      'sokoban-mouse-event-end)

    (define-key map [(control ?/)]	'sokoban-undo)
    map))

;; ;;;;;;;;;;;;;;;; level file parsing functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sokoban-level-regexp "^;LEVEL [0-9]+$")

(defconst sokoban-comment-regexp "^;")

(defun sokoban-init-level-data ()
  (setq sokoban-level-data nil)
  (with-current-buffer (find-file-noselect sokoban-level-file)
    (if (fboundp 'read-only-mode)
        (read-only-mode 1)
      (setq buffer-read-only t))
    (goto-char (point-min))
    (re-search-forward sokoban-level-regexp nil t)
    (forward-char)
    (while (not (eobp))
      (while (looking-at sokoban-comment-regexp)
	(forward-line))
      (let ((data (make-vector sokoban-height nil))
	    (fmt (format "%%-%ds" sokoban-width)))
	(dotimes (y sokoban-height)
	  (cond ((or (eobp)
		     (looking-at sokoban-comment-regexp))
		 (aset data y (format fmt "")))
		(t
		 (let ((start (point))
                       (end (line-end-position)))
                   (aset data
                         y
                         (format fmt (buffer-substring start end)))
                   (goto-char (1+ end))))))
	(push data sokoban-level-data)))
    (kill-buffer (current-buffer))
    (setq sokoban-level-data (nreverse sokoban-level-data))))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sokoban-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c sokoban-floor)
		   sokoban-floor-options)
                  ((= c sokoban-target)
		   sokoban-target-options)
                  ((= c sokoban-wall)
		   sokoban-wall-options)
                  ((= c sokoban-block)
		   sokoban-block-options)
                  ((= c sokoban-player)
		   sokoban-player-options)
                  (t
		   '(nil nil nil)))))
    options))

(defun sokoban-get-level-data ()
  (setq sokoban-level-map (nth (1- sokoban-level) sokoban-level-data)
	sokoban-targets 0)
  (dotimes (y sokoban-height)
    (dotimes (x sokoban-width)
      (let ((c (aref (aref sokoban-level-map y) x)))
	(cond
	 ((= c sokoban-target)
	  (incf sokoban-targets))
	 ((= c sokoban-block-on-target)
	  (incf sokoban-targets)
	  (incf sokoban-done))
	 ((= c ?\040) ;; treat space characters in level file as floor
	  (aset (aref sokoban-level-map y) x sokoban-floor)))))))

(defun sokoban-get-floor (x y)
  (let ((c (aref (aref sokoban-level-map y) x)))
    (if (or (= c sokoban-target)
	    (= c sokoban-block-on-target))
	sokoban-target
      sokoban-floor)))

(defun sokoban-init-buffer ()
  (gamegrid-init-buffer sokoban-buffer-width
			sokoban-buffer-height
			?\040)
  (dotimes (y sokoban-height)
    (dotimes (x sokoban-width)
      (let ((c (aref (aref sokoban-level-map y) x)))
	(if (= c sokoban-player)
	    (setq sokoban-x x
		  sokoban-y y))
	(if (= c sokoban-block-on-target)
	    (setq c sokoban-block))
	(gamegrid-set-cell x y c)))))

(defun sokoban-draw-score ()
  (let ((strings (vector (format "Moves:  %05d" sokoban-moves)
			 (format "Pushes: %05d" sokoban-pushes)
			 (format "Done:   %d/%d"
				 sokoban-done
				 sokoban-targets))))
    (dotimes (y 2)
      (let* ((string (aref strings y))
	     (len (length string)))
	(dotimes (x len)
	  (gamegrid-set-cell (+ sokoban-score-x x)
			     (+ sokoban-score-y y)
			     (aref string x))))))
  (setq mode-line-format
	(format "Sokoban:   Level: %3d   Moves: %05d   Pushes: %05d   Done: %d/%d"
		sokoban-level sokoban-moves sokoban-pushes
		sokoban-done sokoban-targets))
  (force-mode-line-update))

(defun sokoban-add-move (dx dy)
  (push (list 'move dx dy) sokoban-undo-list)
  (incf sokoban-moves)
  (sokoban-draw-score))

(defun sokoban-add-push (dx dy)
  (push (list 'push dx dy) sokoban-undo-list)
  (incf sokoban-moves)
  (incf sokoban-pushes)
  (sokoban-draw-score))

(defun sokoban-undo ()
  "Undo previous Sokoban change."
  (interactive)
  ;; FIXME: Use the normal undo (via `apply' undo entries).
  (if (null sokoban-undo-list)
      (message "Nothing to undo")
    (let* ((entry (pop sokoban-undo-list))
	   (type (car entry))
	   (dx (nth 1 entry))
	   (dy (nth 2 entry)))
      (cond ((eq type 'push)
	     (let* ((x (+ sokoban-x dx))
		    (y (+ sokoban-y dy))
		    (c (sokoban-get-floor x y)))
	       (gamegrid-set-cell x y c)
	       (if (eq c sokoban-target)
		   (decf sokoban-done))
	       (gamegrid-set-cell sokoban-x sokoban-y sokoban-block)
	       (setq c (sokoban-get-floor sokoban-x sokoban-y))
	       (if (eq c sokoban-target)
		   (incf sokoban-done)))
	     (setq sokoban-x (- sokoban-x dx))
	     (setq sokoban-y (- sokoban-y dy))
	     (gamegrid-set-cell sokoban-x sokoban-y sokoban-player)
	     (decf sokoban-pushes)
	     (decf sokoban-moves))
	    ((eq type 'move)
	     (let ((c (sokoban-get-floor sokoban-x sokoban-y)))
	       (gamegrid-set-cell sokoban-x sokoban-y c))
	     (setq sokoban-x (- sokoban-x dx))
	     (setq sokoban-y (- sokoban-y dy))
	     (gamegrid-set-cell sokoban-x sokoban-y sokoban-player)
	     (decf sokoban-moves))
	    (t
	     (message "Invalid entry in sokoban-undo-list")))
      (sokoban-draw-score))))

(defun sokoban-move (dx dy)
  (let* ((x (+ sokoban-x dx))
	 (y (+ sokoban-y dy))
	 (c (gamegrid-get-cell x y)))
    (cond ((or (eq c sokoban-floor)
	       (eq c sokoban-target))
	   (gamegrid-set-cell sokoban-x
			      sokoban-y
			      (sokoban-get-floor sokoban-x
						 sokoban-y))
	   (setq sokoban-x x
		 sokoban-y y)
	   (gamegrid-set-cell sokoban-x
			      sokoban-y
			      sokoban-player)
	   (sokoban-add-move dx dy))
	  ((eq c sokoban-block)
	   (let* ((xx (+ x dx))
		  (yy (+ y dy))
		  (cc (gamegrid-get-cell xx yy)))
	     (cond ((or (eq cc sokoban-floor)
			(eq cc sokoban-target))
		    (if (eq (sokoban-get-floor x y) sokoban-target)
			(decf sokoban-done))
		    (gamegrid-set-cell xx yy sokoban-block)
		    (gamegrid-set-cell x y sokoban-player)
		    (gamegrid-set-cell sokoban-x
				       sokoban-y
				       (sokoban-get-floor sokoban-x
							  sokoban-y))
		    (setq sokoban-x x
			  sokoban-y y)
		    (if (eq (sokoban-get-floor xx yy) sokoban-target)
			(incf sokoban-done))
		    (sokoban-add-push dx dy)
		    (cond ((= sokoban-done sokoban-targets)
			   (sit-for 3)
			   (sokoban-next-level))))))))))

(defun sokoban-event-x (event)
  (let ((x (gamegrid-event-x event)))
    (if (featurep 'xemacs)
        x
      ;; 32.0 is the pixel width of the xpm image
      (floor x (/ 32.0 (frame-char-width))))))

(defun sokoban-event-y (event)
  (let ((y (gamegrid-event-y event)))
    (if (featurep 'xemacs)
        y
      (floor y (/ 32.0 (frame-char-height))))))

(defun sokoban-mouse-event-start (event)
  "Record the beginning of a mouse click."
  (interactive "e")
  (setq sokoban-mouse-x (sokoban-event-x event))
  (setq sokoban-mouse-y (sokoban-event-y event)))

(defun sokoban-mouse-event-end (event)
  "Move according to the clicked position."
  (interactive "e")
  (let* ((x (sokoban-event-x event))
	 (y (sokoban-event-y event))
	 (dx (- x sokoban-x))
	 (dy (- y sokoban-y)))
    (cond
     ;; Ensure that press and release are in the same square
     ;; (which allows you to abort a move)
     ((not (and (eq sokoban-mouse-x x) (eq sokoban-mouse-y y)))
      nil)
     ;; Check that the move isn't diagonal
     ((not (or (eq dx 0) (eq dy 0)))
      nil)
     ((< dx 0)	;; Left
      (while (< dx 0)
	(sokoban-move -1 0)
	(setq dx (1+ dx))))
     ((> dx 0)	;; Right
      (while (> dx 0)
	(sokoban-move 1 0)
	(setq dx (1- dx))))
     ((> dy 0)	;; Up
      (while (> dy 0)
	(sokoban-move 0 1)
	(setq dy (1- dy))))
     ((< dy 0)	;; Down
      (while (< dy 0)
	(sokoban-move 0 -1)
	(setq dy (1+ dy)))))))

(defun sokoban-move-left ()
  "Move one square left."
  (interactive)
  (sokoban-move -1 0))

(defun sokoban-move-right ()
  "Move one square right."
  (interactive)
  (sokoban-move 1 0))

(defun sokoban-move-up ()
  "Move one square up."
  (interactive)
  (sokoban-move 0 -1))

(defun sokoban-move-down ()
  "Move one square down."
  (interactive)
  (sokoban-move 0 1))

(defun sokoban-restart-level ()
  "Restart the current level."
  (interactive)
  (setq sokoban-moves 0
	sokoban-pushes 0
	sokoban-done 0
	sokoban-undo-list nil)
  (sokoban-get-level-data)
  (sokoban-init-buffer)
  (sokoban-draw-score))

(defun sokoban-next-level ()
  (incf sokoban-level)
  (sokoban-restart-level))

(defun sokoban-goto-level (level)
  "Jump to a specified LEVEL."
  (interactive "nLevel: ")
  (when (or (< level 1)
            (> level (length sokoban-level-data)))
    (signal 'args-out-of-range
            (list "No such level number"
                  level 1 (> level (length sokoban-level-data)))))
  (setq sokoban-level level)
  (sokoban-restart-level))

(defun sokoban-start-game ()
  "Start a new game of Sokoban."
  (interactive)
  (setq sokoban-level 0)
  (sokoban-next-level))

(put 'sokoban-mode 'mode-class 'special)

(unless (featurep 'xemacs)
  (easy-menu-define sokoban-popup-menu nil "Popup menu for Sokoban mode."
    '("Sokoban Commands"
      ["Restart this level" sokoban-restart-level]
      ["Start new game" sokoban-start-game]
      ["Go to specific level" sokoban-goto-level]))
  (define-key sokoban-mode-map [down-mouse-3] sokoban-popup-menu))

(define-derived-mode sokoban-mode special-mode "Sokoban"
  "A mode for playing Sokoban.

sokoban-mode keybindings:
   \\{sokoban-mode-map}"

  (when (featurep 'xemacs)
    (setq mode-popup-menu
          '("Sokoban Commands"
            ["Restart this level" sokoban-restart-level]
            ["Start new game" sokoban-start-game]
            ["Go to specific level" sokoban-goto-level])))

  (set (make-local-variable 'gamegrid-use-glyphs) sokoban-use-glyphs)
  (set (make-local-variable 'gamegrid-use-color) sokoban-use-color)
  (set (make-local-variable 'gamegrid-font) sokoban-font)

  (gamegrid-init (sokoban-display-options))

  (if (null sokoban-level-data)
      (sokoban-init-level-data)))

;;;###autoload
(defun sokoban ()
  "Sokoban.

Push the blocks onto the target squares.

sokoban-mode keybindings:
   \\<sokoban-mode-map>
\\[sokoban-start-game]	Starts a new game of Sokoban
\\[sokoban-restart-level]	Restarts the current level
\\[sokoban-goto-level]	Jumps to a specified level
\\[sokoban-move-left]	Move one square to the left
\\[sokoban-move-right]	Move one square to the right
\\[sokoban-move-up]	Move one square up
\\[sokoban-move-down]	Move one square down"
  (interactive)

  (switch-to-buffer sokoban-buffer-name)
  (gamegrid-kill-timer)
  (sokoban-mode)
  (sokoban-start-game))

;;;###autoload
(unless (featurep 'xemacs)
  (define-key-after			; install a menu entry
    (lookup-key global-map [menu-bar tools games])
    [sokoban]
    '(menu-item "Sokoban" sokoban)
    'snake))

(provide 'sokoban)

;;; sokoban.el ends here

