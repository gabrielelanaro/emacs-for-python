;;; sokoban.el --- Play the Sokoban game in emacs
;; $Id: sokoban.el,v 1.23 2005/06/26 12:14:29 wilde Exp $

;; Copyright (C) 2005  Sascha Wilde

;; Version: 1.23
;; Author: Sascha Wilde <swilde@sha-bang.de>
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Play Sokoban in emacs.

;;; Code:

(require 'cl)

(defconst sokoban-version "0.14.1"
  "Version string for this version of GNU-Emacs Sokoban.")

(defconst sokoban-left  '(-1 .  0))
(defconst sokoban-right '( 1 .  0))
(defconst sokoban-down  '( 0 .  1))
(defconst sokoban-up    '( 0 . -1))

(defgroup sokoban nil
  "Sokoban game for GNU Emacs."
  :prefix "sokoban-"
  :group 'games)

(defcustom sokoban-playerfiles-dir "/tmp/"
  "*The directory holding the sokoban playerfiles.
Sokoban saves the information from `sokoban-player-stats' to a
playerfile in this directory.  If you don't want to use
playerfiles, set value to NIL."
  :group 'sokoban
  :type 'string)

(defvar sokoban-player-stats nil
  "Alist with player specific information as saved in the playerfiles.
This holds the best results for each finished level and the
players current level.")

(defconst sokoban-playerfile-prefix "sokoban-pl-"
  "The prefix used for sokoban playerfiles.")

(defvar sokoban-best-players-list nil
  "Alist with the best result for each level
generated from all available playerfiles, if `sokoban-playerfiles-dir'
is none nil.")

(defcustom sokoban-levels-dir "/usr/local/share/sokoban-levels"
  "*Directory holding the sokoban level files"
  :group 'sokoban
  :type 'string)

(defcustom sokoban-levels-basename "sokoban-lvl."
  "*Basename of the sokoban level files"
  :group 'sokoban
  :type 'string)

(defcustom sokoban-start-level 1
  "*Defines the level-numver to start with.
This might be overwritten by the last level played,
as saved in the playerfile."
  :group 'sokoban
  :type 'integer)

(defcustom sokoban-undo-penalty 10
  "*Defines the number of moves one undo costs."
  :group 'sokoban
  :type 'integer)

(defcustom sokoban-player-char ?@
  "*Defines the character used to diplay the player."
  :group 'sokoban
  :type 'character)

(defcustom sokoban-boulder-char ?o
  "*Defines the character used to diplay the boulders."
  :group 'sokoban
  :type 'character)

(defcustom sokoban-pit-char ?.
  "*Defines the character used to diplay the pits."
  :group 'sokoban
  :type 'character)

(defcustom sokoban-filled-pit-char ?*
  "*Defines the character used to diplay pits with boulders in level-files."
  :group 'sokoban
  :type 'character)

(defface sokoban-boulder-face
  '((((class color) (background dark))
     (:foreground "yellow"
      :weight  bold))
    (((class color) (background light))
     (:foreground "goldenrod4"
      :weight  bold)))
  "*Face used display boulders in sokoban game."
  :group 'sokoban)

(defface sokoban-pit-face
  '((t (:foreground "blue"
	:weight  bold)))
  "*Face used display pits in sokoban game."
  :group 'sokoban)

(defface sokoban-player-face
  '((t (:foreground "orange red")))
  "*Face used display player in sokoban game."
  :group 'sokoban)

(defvar sokoban-boulder-face 'sokoban-boulder-face)
(defvar sokoban-pit-face 'sokoban-pit-face)
(defvar sokoban-player-face 'sokoban-player-face)

(defconst sokoban-font-lock-keywords
  `((,(regexp-quote (char-to-string sokoban-boulder-char))
     . sokoban-boulder-face)
    (,(regexp-quote (char-to-string sokoban-pit-char))
     . sokoban-pit-face)
    (,(regexp-quote (char-to-string sokoban-player-char))
     . sokoban-player-face))
  "Stuff to highlight in sokoban.")

(defvar sokoban-mode-map nil
  "Keymap for sokoban.")
(setq sokoban-mode-map (make-sparse-keymap))
(define-key sokoban-mode-map [up]    'sokoban-move-up)
(define-key sokoban-mode-map [down]  'sokoban-move-down)
(define-key sokoban-mode-map [left]  'sokoban-move-left)
(define-key sokoban-mode-map [right] 'sokoban-move-right)
(define-key sokoban-mode-map "u" 'sokoban-undo)
(define-key sokoban-mode-map "b" 'sokoban-display-best-players-list)
(define-key sokoban-mode-map ">" 'sokoban-goto-next-level)
(define-key sokoban-mode-map "<" 'sokoban-goto-prev-level)

(defvar sokoban-pits-list nil
  "List of positions of all pits in buffer.  Buffer-local in sokoban-mode")
(defvar sokoban-filled-pits nil
  "Number of pits filled.  Buffer-local in sokoban-mode.")
(defvar sokoban-level nil
  "Number of current level.  Buffer-local in sokoban games.")
(defvar sokoban-moves nil
  "Number of moves made by player.  Buffer-local in sokoban-mode.")
(defvar sokoban-player-pos nil
  "Current position of player.  Buffer-local in sokoban-mode.")
(defvar sokoban-player-last-pos nil
  "Backup of last player position.  Buffer-local in sokoban-mode.")
(defvar sokoban-game-info nil
  "String with infos to the current game.  Buffer-local in sokoban-mode.")
(defvar sokoban-level-best-string nil
  "String holding the best result for the current level as displayed.")

(defun sokoban-forward-line (arg)
  "Like forward-line but preserve the current column.
The implementation is rather simple, as we can make certain
assumptions about the structure of a valid sokoban level buffer."
  (let ((goal-column (current-column)))
    (forward-line arg)
    (move-to-column goal-column)))

(defun sokoban-paint (char)
  "Insert char at point, overwriting the old char.
Extreme simple, but sufficient for our needs."
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert (char-to-string char)))
  t)

(defun sokoban-update-score (level moves)
  "Save number of moves for level to `sokoban-player-stats'."
  (let* ((level-name (concat sokoban-levels-basename
			     (number-to-string level)))
	 (entry (assoc level-name sokoban-player-stats)))
    (if entry
	(or (< (cdr entry) moves) (setcdr entry moves))
      (push (cons level-name moves) sokoban-player-stats))))

(defun sokoban-get-level-best (level &optional list)
  "Get best result for level from `sokoban-player-stats'."
  (if level
      (let* ((level-name (concat sokoban-levels-basename
				 (number-to-string level)))
	     (entry (assoc level-name
			   (or list sokoban-player-stats))))
	(if entry
	    (cdr entry)))))

(defun sokoban-update-current-level (level)
  "Save current level to `sokoban-player-stats'."
  (let ((entry (assoc :level sokoban-player-stats)))
    (if entry
	(setcdr entry level)
      (push (cons :level level) sokoban-player-stats))))

(defun sokoban-save-playerfile ()
  "Save `sokoban-player-stats' to playerfile."
  (if sokoban-playerfiles-dir
      (let ((filename (concat sokoban-playerfiles-dir "/"
			      sokoban-playerfile-prefix
			      (user-login-name))))
	(with-temp-file filename
	  (prin1 sokoban-player-stats (current-buffer)))
	(set-file-modes filename #o644))))

(defun sokoban-load-playerfile ()
  "Load `sokoban-player-stats' from playerfile."
  (if sokoban-playerfiles-dir
      (let ((filename (concat sokoban-playerfiles-dir "/"
			      sokoban-playerfile-prefix
			      (user-login-name))))
	(if (file-readable-p filename)
	    (with-temp-buffer
	      (insert-file-contents filename nil)
	      (setq sokoban-player-stats
		    (read (current-buffer))))))))

(defun sokoban-gen-best-players-list ()
  (if sokoban-playerfiles-dir
      (let ((files (directory-files sokoban-playerfiles-dir
				    t (concat "^" sokoban-playerfile-prefix)
				    t)))
	(dolist (filename files)
	  (if (file-readable-p filename)
	      (with-temp-buffer
		(insert-file-contents filename nil)
		(let ((stats (read (current-buffer)))
		      (player (substring (file-name-nondirectory filename)
					 (1- (length sokoban-levels-basename)))))
		  (dolist (entry stats)
		    (let* ((level-name  (car entry))
			   (best-entry (assoc level-name
					      sokoban-best-players-list)))
		      (if (and (stringp level-name)
			   (compare-strings level-name
					    0 (length sokoban-levels-basename)
					    sokoban-levels-basename
					    0 nil))
			  (cond ((and best-entry
				      (> (cadr best-entry) (cdr entry)))
				 (setcdr best-entry
					 (cons (cdr entry) player)))
				((or (not best-entry)
				     (= (cadr best-entry) (cdr entry)))
				 (push (cons level-name
					     (cons (cdr entry) player))
				       sokoban-best-players-list)))))))))))))


(defun sokoban-display-best-players-list ()
  (interactive)
  (if sokoban-best-players-list
      (progn
	(switch-to-buffer (get-buffer-create "*Sokoban Best Players*"))
	(erase-buffer)
	(dolist (entry sokoban-best-players-list)
	  (let ((level-name (car entry)))
	    (if (and (stringp level-name)
		     (compare-strings level-name
				      0 (length sokoban-levels-basename)
				      sokoban-levels-basename
				      0 nil))
		(insert (format "%4s: %5d - %s\n"
				(substring level-name
					   (length sokoban-levels-basename))
				(cadr entry)
				(cddr entry))))))
	(sort-columns nil (point-min) (point-max)))
    (error "No best players list available")))

(defun sokoban-refresh-pits ()
  "Refresh and evaluate pits."
  (save-excursion
    (setq sokoban-filled-pits 0)
    (dolist (pos sokoban-pits-list)
      (goto-char pos)
      (let ((char (char-after)))
	(cond ((= char 32)
	       (sokoban-paint sokoban-pit-char))
	      ((= char sokoban-boulder-char)
	       (setq sokoban-filled-pits
		     (1+ sokoban-filled-pits)))))))
  (if (= sokoban-filled-pits (length sokoban-pits-list))
      (sokoban-level-finished)))

(defun sokoban-load-next-level (&optional arg)
  "Load next level, with negative arg load previous level.
If requested level doesn't exist, load `sokoban-start-level'."
  (when (bound-and-true-p sokoban-level)
    (setq sokoban-level (if (and arg (< arg 0))
			    (1- sokoban-level)
			  (1+ sokoban-level)))
    (or (sokoban-load-level sokoban-level)
	(progn
	  (setq sokoban-level sokoban-start-level)
	  (sokoban-load-level sokoban-level)))
    (sokoban-init-level)
    t))

(defun sokoban-level-finished ()
  (message
   (format "You finished Level %d in %d moves.  Congratulations!"
	   (or (bound-and-true-p sokoban-level) 0)
	   sokoban-moves))
  (when (bound-and-true-p sokoban-level)
    (sokoban-update-score sokoban-level sokoban-moves))
  (when (sokoban-load-next-level)
    (sokoban-update-current-level sokoban-level)
    (sokoban-save-playerfile)))

(defun sokoban-find-player ()
  (goto-char (point-min))
  (search-forward (char-to-string sokoban-player-char))
  (forward-char -1)
  (setq sokoban-player-pos (point)))

(defun sokoban-init-pits ()
  "Reset and init `sokoban-pits-list'"
  (setq sokoban-pits-list nil)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (char-to-string sokoban-pit-char) nil t)
      (push (1- (point)) sokoban-pits-list))
    (goto-char (point-min))
    (while (search-forward (char-to-string sokoban-filled-pit-char) nil t)
      (forward-char -1)
      (push (point) sokoban-pits-list)
      (sokoban-paint sokoban-boulder-char))))


(defun sokoban-move-player-here ()
  "Move player to point.
Move player char to point, repaint pits and evaluate game status."
  (setq sokoban-player-pos (point))
  (sokoban-paint sokoban-player-char)
  (goto-char sokoban-player-last-pos)
  (sokoban-paint 32)
  (setq sokoban-moves (1+ sokoban-moves))
  (sokoban-refresh-pits)
  (sokoban-update-mode-line))

(defun sokoban-move-player (direction)
  (goto-char sokoban-player-pos)
  (setq sokoban-player-last-pos (point))
  (forward-char (car direction))
  (sokoban-forward-line (cdr direction))
  (cond ((or (= (char-after) 32) ; 32 is space
	     (= (char-after) sokoban-pit-char))
	 (sokoban-move-player-here))
	((= (char-after) sokoban-boulder-char)
	 (if (sokoban-move-boulder direction)
	     (sokoban-move-player-here)
	   (message "Can't move boulder!")))
	(t (message "Ouch!")))
  (goto-char sokoban-player-pos))

(defun sokoban-move-boulder (direction)
  "Move boulder at point into direction, if possible."
  (let ((last-pos (point)))
    (forward-char (car direction))
    (sokoban-forward-line (cdr direction))
    (prog1
	(if (or (= (char-after) 32) ; 32 is space
		(= (char-after) sokoban-pit-char))
	    (sokoban-paint sokoban-boulder-char)
	  nil)
      (goto-char last-pos))))

(defun sokoban-move-up ()
  "Move the player up if possible."
  (interactive)
  (sokoban-move-player sokoban-up))

(defun sokoban-move-down ()
  "Move the player down if possible."
  (interactive)
  (sokoban-move-player sokoban-down))

(defun sokoban-move-left ()
  "Move the player left if possible."
  (interactive)
  (sokoban-move-player sokoban-left))

(defun sokoban-move-right ()
  "Move the player right if possible."
  (interactive)
  (sokoban-move-player sokoban-right))

(defun sokoban-goto-next-level ()
  "Jump to next level."
  (interactive)
  (sokoban-load-next-level))

(defun sokoban-goto-prev-level ()
  "Jump to previous level."
  (interactive)
  (sokoban-load-next-level -1))

(defun sokoban-update-mode-line ()
  (setq sokoban-game-info (format "Level:%d [%d|%d] -- Moves:%d%s"
				  (or (bound-and-true-p sokoban-level)
				      0)
				  sokoban-filled-pits
				  (length sokoban-pits-list)
				  sokoban-moves
				  (or sokoban-level-best-string ""))))

(defun sokoban-undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (undo))
  (sokoban-find-player)
  (setq sokoban-moves (+ sokoban-moves sokoban-undo-penalty))
  (sokoban-refresh-pits)
  (sokoban-update-mode-line))

(defun sokoban-load-level (num)
  "Load sokoban level num."
  (let ((inhibit-read-only t)
	(level-file
	 (concat sokoban-levels-dir "/"
		 sokoban-levels-basename (number-to-string num))))
    (when (file-exists-p level-file)
      (insert-file-contents level-file nil nil nil t)
      t)))

(defun sokoban-init-level ()
  "Initialize level elements."
  (sokoban-init-pits)
  (sokoban-refresh-pits)
  (setq sokoban-moves 0)
  (setq sokoban-level-best-string
	(let ((best (sokoban-get-level-best sokoban-level))
	      (world-best (if sokoban-best-players-list
			      (sokoban-get-level-best 
			       sokoban-level
			       sokoban-best-players-list))))
	  (if (or best world-best)
	      (format " [Best:%s%s]"
		      (if best (number-to-string best) "")
		      (if (and world-best
			       (or (not best)
				   (< (car world-best) best)))
			  (format " (%s:%d)"
				  (cdr world-best) (car world-best))
		      "")))))
  (sokoban-update-mode-line)
  (sokoban-find-player)
  (buffer-disable-undo (current-buffer))
  (buffer-enable-undo))

;;;###autoload
(defun sokoban-mode ()
  "Major mode to play sokoban.

Commands:
\\{sokoban-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (toggle-read-only 1)
  (use-local-map sokoban-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sokoban-font-lock-keywords
			     t nil nil beginning-of-line))
  (setq cursor-type nil)
  (make-local-variable 'sokoban-level)
  (make-local-variable 'sokoban-player-pos)
  (make-local-variable 'sokoban-player-last-pos)
  (make-local-variable 'sokoban-moves)
  (make-local-variable 'sokoban-pits-list)
  (make-local-variable 'sokoban-filled-pits)
  (make-local-variable 'sokoban-game-info)
  (setq major-mode 'sokoban-mode)
  (setq mode-name "Sokoban")
  (setq header-line-format
	(list "Sokoban v" 'sokoban-version
	      " -- " 'sokoban-game-info))
  (sokoban-init-level)
  (run-hooks 'sokoban-mode-hook))

;;;###autoload
(defun sokoban ()
  "Play sokoban."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Sokoban*"))
  (sokoban-load-playerfile)
  (setq sokoban-best-players-list nil)
  (sokoban-gen-best-players-list)
  (let ((level (or (cdr (assoc :level sokoban-player-stats))
		   sokoban-start-level)))
    (sokoban-load-level level)
    (sokoban-mode)
    (setq sokoban-level level))
  (sokoban-init-level)
  (sokoban-update-mode-line))

(provide 'sokoban)
;;; sokoban.el ends here
