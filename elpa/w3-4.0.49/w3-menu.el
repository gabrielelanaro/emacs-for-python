;;; w3-menu.el --- Menu functions for emacs-w3

;; Copyright (c) 1996-2001, 2007, 2013 Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Keywords: menu, hypermedia

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

;;; Code:

(require 'w3-vars)
(require 'w3-mouse)
(eval-when-compile
  (require 'cl))
(defvar w3-html-bookmarks)
(autoload 'url-truncate-url-for-viewing "url-util")
(autoload 'w3-first-n-items "w3")
(autoload 'w3-only-links "w3")
(autoload 'w3-fix-spaces "w3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; InfoDock stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (fboundp 'id-menubar-set)
    (id-menubar-set 'w3-mode 'w3-menu-make-xemacs-menubar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spiffy new menus (for both Emacs and XEmacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar w3-menu-fsfemacs-bookmark-menu nil)
(defvar w3-menu-fsfemacs-debug-menu nil)
(defvar w3-menu-fsfemacs-edit-menu nil)
(defvar w3-menu-fsfemacs-file-menu nil)
(defvar w3-menu-fsfemacs-go-menu nil)
(defvar w3-menu-fsfemacs-help-menu nil)
(defvar w3-menu-fsfemacs-view-menu nil)
(defvar w3-menu-fsfemacs-options-menu nil)
(defvar w3-menu-fsfemacs-style-menu nil)
(defvar w3-menu-fsfemacs-search-menu nil)
(defvar w3-menu-w3-menubar nil)

(defcustom w3-use-menus '(file edit view go bookmark options buffers style
			       emacs nil help)
  "*Non-nil value causes W3 to provide a menu interface.
A value that is a list causes W3 to install its own menubar.
A value of 1 causes W3 to install a \"W3\" item in the Emacs menubar.

If the value of w3-use-menus is a list, it should be a list of symbols.
The symbols and the order that they are listed determine what menus
will be in the menubar and how they are ordered.  Valid symbol values
are:

file		-- A list of file related commands
edit		-- Various standard editing commands (copy/paste)
view		-- Controlling various things about the document view
go		-- Navigation control
bookmark	-- Bookmark / hotlist control
options		-- Various options
buffers		-- The standard buffers menu
emacs		-- A toggle button to switch back to normal emacs menus
style		-- Control style information and who gets to set what
search          -- Various search engines
help		-- The help menu
nil		-- ** special **

If nil appears in the list, it should appear exactly once.  All
menus after nil in the list will be displayed flushright in the
menubar.

NOTE!  The current port of Emacs to Windows NT/95 does not support
buttons in the menubar, so the 'emacs' keyword is currently ignored
on that platform."
  :group 'w3-menus
  :type '(set (const :tag "File related commands" :value file)
	      (const :tag "Standard editing commands" :value edit)
	      (const :tag "View document information" :value view)
	      (const :tag "Navigation" :value go)
	      (const :tag "Bookmarks" :value bookmark)
	      (const :tag "Options" :value options)
	      (const :tag "Buffer list" :value buffers)
	      (const :tag "Stylesheet information" :value style)
	      (const :tag "Search engines" :value search)
	      (const :tag "Toggle to default menus" :value emacs)
	      (const :tag "Separator" :value nil)
	      (const :tag "Help" :value help)))

(defvar w3-menu-hotlist-menu nil)
(defvar w3-menu-html-links-menu nil)
(defvar w3-menu-links-menu nil)
(make-variable-buffer-local 'w3-menu-links-menu)
(make-variable-buffer-local 'w3-menu-html-links-menu)
(make-variable-buffer-local 'w3-menu-hotlist-menu)

(defun w3-menu-breakup (menu-desc max-len)
  (if (> (length menu-desc) max-len)
      (append (w3-first-n-items menu-desc max-len)
	      (list (cons "More..."
			  (w3-menu-breakup (nthcdr max-len menu-desc) max-len))))
    menu-desc))

(defun w3-menu-truncate-item (string)
  (if (<= (length string) w3-max-menu-width)
      string
    (concat (substring string 0 w3-max-menu-width) "$")))

(defun w3-menu-dummy-menu (item)
  (if (featurep 'xemacs)
      (list (vector item nil nil))
    (list "Ignored" (vector item nil nil))))
      
(defun w3-menu-hotlist-constructor (_menu-items)
  (require 'w3-hot)
  (easy-menu-define
   w3-menu-hotlist-menu nil "Emacs/W3 Dynamic menu"
   (or (cdr w3-html-bookmarks)
       (let ((hot-menu nil)
	     (hot w3-hotlist))
	 (while hot
	   (setq hot-menu (cons (vector
				 (w3-menu-truncate-item (car (car hot)))
				 (list 'w3-fetch (car (cdr (car hot))))
				 t) hot-menu)
		 hot (cdr hot)))
	 (or hot-menu (w3-menu-dummy-menu "No Hotlist")))))
  w3-menu-hotlist-menu)

(defun w3-menu-html-links-constructor (_menu-items)
  (let ((links (mapcar 'cdr w3-current-links))
	(menu nil))
    (if links
	;; Fixme: delete*, reduce runtime cl dependency.
	(setq links (delete*
		     nil
		     (reduce 'append links)
		     :test-not (function
				(lambda (a b) ; arg order unknown
				  (member
				   (car (or a b))
				   w3-defined-link-types))))))
    (while links
      (let ((name (caar links))
	    (vals (cdar links))
	    (href nil)
	    (new nil))
	(if (= (length vals) 1)
	    (setq vals (car vals)
		  new (vector (or (plist-get vals 'title)
				  (capitalize name))
			      (list 'w3-fetch (plist-get vals 'href)) t))
	  (setq new (cons (capitalize name)
			  (mapcar (function
				   (lambda (x)
				     (setq href (plist-get x 'href))
				     (vector (or (plist-get x 'title) href)
					     (list 'w3-fetch href) t)))
				  vals))))
	(setq links (cdr links)
	      menu (cons new menu))))
    (easy-menu-define w3-menu-html-links-menu nil "Emacs/W3 dynamic menu"
		      (or menu
			  (w3-menu-dummy-menu "None")))
    w3-menu-html-links-menu))

(defun w3-menu-links-constructor (_menu-items)
  (let ((widgets (w3-only-links))
	widget href menu)
    (while widgets
      (setq widget (car widgets)
	    widgets (cdr widgets)
	    href (widget-get widget :href)
	    menu (cons
		  (vector (w3-menu-truncate-item
			   (or (widget-get widget :title)
			       (w3-fix-spaces
				(buffer-substring-no-properties
				 (widget-get widget :from)
				 (widget-get widget :to)))))
			  `(url-retrieve (url-expand-file-name ,href)) t)
		  menu)))
    (setq menu (w3-menu-breakup menu w3-max-menu-length))
    (easy-menu-define w3-menu-links-menu nil "Emacs/W3 dynamic menu"
		      (or menu (w3-menu-dummy-menu "No links")))
    w3-menu-links-menu))

(defun w3-toggle-minibuffer ()
  (interactive)
  (cond
   ((featurep 'xemacs)
    (if (equal (frame-property (selected-frame) 'minibuffer) t)
 
	;; frame has a minibuffer, so remove it
	;; unfortunately, we must delete and redraw the frame
	(let ((fp (frame-properties (selected-frame)))
	      (frame (selected-frame))
	      (buf (current-buffer)))
	  (select-frame
	   (make-frame (plist-put
			(plist-remprop
			 (plist-remprop fp 'window-id) 'minibuffer)
			'minibuffer nil)))
	  (delete-frame frame)
	  (switch-to-buffer buf))
      ;; no minibuffer so add one
      (set-frame-property (selected-frame) 'minibuffer t)))
   (t nil)))

(defun w3-toggle-location ()
  (interactive)
  (cond
   ((featurep 'xemacs)
    (let ((on (specifier-instance has-modeline-p (selected-window))))
      (set-specifier has-modeline-p (not on) (selected-window))))
   (t nil)))
   
(defun w3-toggle-menubar ()
  (interactive)
  (cond
   ;; XEmacs style
   ((featurep 'xemacs)
    (set-specifier menubar-visible-p (cons (current-buffer)
					   (not (specifier-instance
						 menubar-visible-p)))))
   ;; Emacs 19 style
   (t
    (menu-bar-mode (if (w3-menubar-active) -1 1)))))

(defun w3-location-active ()
  (if (featurep 'xemacs)
      (specifier-instance has-modeline-p (selected-window))
    t))

(defun w3-menubar-active ()
  (if (featurep 'xemacs)
      (and (featurep 'menubar) (specifier-instance menubar-visible-p))
    (and (boundp 'menu-bar-mode) menu-bar-mode)))

(defun w3-menu-global-menubar ()
  (if (featurep 'xemacs)
      (default-value 'default-menubar)
    (lookup-key (current-global-map) [menu-bar])))

(defconst w3-menu-file-menu
  (list
   "File"
   ["Open Location..." w3-fetch t]
   ["Open File..." w3-open-local t]
   ["Open in New Window..." w3-fetch-other-frame t]
   ["New Window" make-frame t]
   "---"
   ["Save" save-buffer t nil]
   (list
    "Save As..."
    ["HTML" (w3-save-as "HTML Source") t]
    ["Formatted Text" (w3-save-as "Formatted Text") t]
    ["PostScript" (w3-save-as "PostScript") t]
    ["Binary" (w3-save-as "Binary") t]
    )
   "---"
   (list
    "Print As..."
    ["PostScript" (w3-print-this-url nil "PostScript") t]
    ["Formatted Text" (w3-print-this-url nil "Formatted Text") t]
    ["HTML Source" (w3-print-this-url nil "HTML Source") t]
    )
   (list
    "Mail Document..."
    ["HTML" (w3-mail-current-document nil "HTML Source") t]
    ["Formatted Text" (w3-mail-current-document nil "Formatted Text") t]
    ["PostScript" (w3-mail-current-document nil "PostScript") t]
    )
   (if (featurep 'xemacs)
       ;; FIXME: These fancier separators were added to Emacs-21, but only
       ;; supported in the Lucid toolkit, whereas in most other toolkits they
       ;; are not even recognized as separators :-(
       "--:shadowDoubleEtchedIn"
     "---")
   ["Close" delete-frame (not (eq (next-frame) (selected-frame)))]
   ["Exit"  save-buffers-kill-emacs t]
   )
  "W3 file menu list.")

(defconst w3-menu-edit-menu
  (list
   "Edit"
   ["Undo"			advertised-undo		   nil]
   ["Cut"			kill-region		   nil]
   ["Copy"			copy-region-as-kill	   t]
   "----"
   ["Search..."			w3-search-forward	t]
   ["Search Again..."		w3-search-again		w3-last-search-item]
   "----"
   (list
    "Preferences"
    (if (fboundp 'custom-menu-create)
	(custom-menu-create 'w3)
      ["W3" ignore nil])
    (if (fboundp 'custom-menu-create)
	(custom-menu-create 'url)
      ["URL" ignore nil])
    )
   )
  "W3 edit menu list.")

(defconst w3-menu-view-menu
  (list
   "View"
   ["Document Information" w3-document-information t]
   ["Document Source" w3-source-document t]
   ["Document Errors" w3-display-errors w3-current-badhtml]
   ["Load Images" w3-load-delayed-images w3-delayed-images]
   "----"
   ["Refresh" w3-refresh-buffer w3-current-parse]
   ["Reload" w3-reload-document url-current-object]
   "----"
   ["Show URL" url-view-url t]
   ["Show URL At Point" w3-view-this-url t]
   "----"
   )
  "W3 menu view list.")

(defconst w3-menu-debug-menu
  (list
   "Debugging"
   ["View Parse Tree" (w3-display-parse-tree w3-current-parse)
    w3-current-parse]
   ["View Stylesheet" w3-display-stylesheet w3-current-stylesheet]
   ["Reload Stylesheets" w3-refresh-stylesheets t]
   )
  "W3 menu debug list.")

(defconst w3-menu-go-menu
  (list
   "Go"
   ["Forward" w3-history-forward
    (cdr (w3-history-find-url-internal (url-view-url t)))]
   ["Back" w3-history-backward
    (car (w3-history-find-url-internal (url-view-url t)))]
   ["Home" w3 w3-default-homepage]
   ["View History..." w3-show-history-list url-history-track]
   "----"
   '("Links" :filter w3-menu-links-constructor)
   '("Navigate" :filter w3-menu-html-links-constructor)
   )
  "W3 menu go list.")

(defconst w3-menu-bookmark-menu
  (list
   "Bookmark"
   ["View Bookmarks..." w3-hotlist-view w3-hotlist]
   ["Add Bookmark" w3-hotlist-add-document t]
   ["Delete Bookmark" w3-hotlist-delete t]
   ["Rename Bookmark" w3-hotlist-rename-entry t]
   ["Append Bookmark List" w3-hotlist-append t]
   "----"
   ["Add Keyword" w3-hotindex-add-key t]
   ["Remove Keyword" w3-hotindex-rm-key t]
   ["Query Keyword" w3-hotindex-query t]
   "----"
   '("Bookmarks" :filter w3-menu-hotlist-constructor)
   )
  "W3 menu bookmark list.")

(defconst w3-menu-options-menu
  (list "Options"
	["Edit Preferences" (customize-browse 'w3) t]
	"---"
	["Show Menubar" w3-toggle-menubar
	 :style toggle :selected (w3-menubar-active)]
	;; Fixme: should work in Emacs 21.
	(if (and (featurep 'xemacs) (featurep 'toolbar))
	    ["Show Toolbar" w3-toggle-toolbar
	     :style toggle :selected (w3-toolbar-active)]
	  ["Show Toolbar" w3-toggle-toolbar nil])
	(if (featurep 'xemacs)
	    ["Show Location" w3-toggle-location
	     :style toggle :selected (w3-location-active)]
	  ["Show Location" w3-toggle-location nil])
	(if (featurep 'xemacs)
	    ["Show Status Bar" w3-toggle-minibuffer
	     :style toggle
	     :selected (eq (frame-property (selected-frame) 'minibuffer) t)
	     ]
	  ["Show Status Bar" w3-toggle-minibuffer nil])
	["Incremental Display"
	 (setq w3-do-incremental-display (not w3-do-incremental-display))
	 :style toggle :selected w3-do-incremental-display]
	"----"
	["Auto Load Images"
	 (setq w3-delay-image-loads (not w3-delay-image-loads))
	 :style toggle :selected (not w3-delay-image-loads)]
	["Flush Image Cache" (setq w3-graphics-list nil) w3-graphics-list]
	"----"
;; 	["Download to disk" (setq w3-dump-to-disk (not w3-dump-to-disk))
;; 	 :style toggle :selected w3-dump-to-disk]
	["caching" (setq url-automatic-caching (not url-automatic-caching))
	 :style toggle :selected url-automatic-caching]
	["Use Cache Only"
	 (setq url-standalone-mode (not url-standalone-mode))
	 :style toggle :selected url-standalone-mode]
	"----"
	["Save Options" w3-menu-save-options t]
	)
  "W3 menu options list.")

(defconst w3-menu-style-menu
  (list
   "Style"
   ["Allow Document Stylesheets" (setq w3-honor-stylesheets
				       (not w3-honor-stylesheets))
    :style toggle :selected w3-honor-stylesheets]
   ["Honor Color Requests" (setq w3-user-colors-take-precedence
				 (not w3-user-colors-take-precedence))
    :style toggle :selected (not w3-user-colors-take-precedence)]
   ["Honor Font Requests" (setq w3-user-fonts-take-precedence
				 (not w3-user-fonts-take-precedence))
    :style toggle :selected (not w3-user-fonts-take-precedence)]
   "---"
   ["Reload Stylesheets" w3-refresh-stylesheets t]
   )
  "W3 menu style list.")

(defconst w3-menu-buffer-menu
  (if (featurep 'xemacs)
      '("Buffers"
	:filter buffers-menu-filter
	["List All Buffers" list-buffers t]
	"--!here")
    nil)
  "W3 menu buffer list.")

(defconst w3-menu-search-menu
  (list
   "Search"
   ["Yahoo!"    (w3-fetch "http://www.yahoo.com/") t]
   ["Excite"    (w3-fetch "http://www.excite.com/") t]
   ["AltaVista" (w3-fetch "http://www.altavista.com/") t]
   ["Google"    (w3-fetch "http://www.google.com/") t]
   ["FTP Search" (w3-fetch "http://ftpsearch.ntnu.no/home.html") t]
   "---"
   )
  "W3 search menu")

(defconst w3-menu-emacs-button
  (vector
   (if (featurep 'xemacs) "XEmacs" "Emacs") 'w3-menu-toggle-menubar t))

(defconst w3-menu-help-menu
  (list
   "Help"
   ["About Emacs-w3" (w3-fetch "about:") t]
   ["Manual" (w3-fetch (concat w3-documentation-root "docs/w3_toc.html")) t]
   ["On FAQ" (w3-fetch (concat w3-documentation-root "help/FAQ.html")) t]
   "---"
   ["Mail Developer(s)" w3-submit-bug t]
   )
  "W3 menu help list.")

(defvar w3-mode-menu-map nil)

(defun w3-menu-initialize-w3-mode-menu-map ()
  (if (null w3-mode-menu-map)
      (let ((map (make-sparse-keymap))
	    (dummy (make-sparse-keymap)))
	(require 'easymenu)
	;; initialize all the w3-menu-fsfemacs-*-menu variables
	;; with the menus.
	(easy-menu-define w3-menu-fsfemacs-bookmark-menu (list dummy) nil
			  w3-menu-bookmark-menu)
	(easy-menu-define w3-menu-fsfemacs-debug-menu (list dummy) nil
			  w3-menu-debug-menu)
	(easy-menu-define w3-menu-fsfemacs-edit-menu (list dummy) nil
			  w3-menu-edit-menu)
	(easy-menu-define w3-menu-fsfemacs-file-menu (list dummy) nil
			  w3-menu-file-menu)
	(easy-menu-define w3-menu-fsfemacs-go-menu (list dummy) nil
			  w3-menu-go-menu)
	(easy-menu-define w3-menu-fsfemacs-help-menu (list dummy) nil
			  w3-menu-help-menu)
	(easy-menu-define w3-menu-fsfemacs-view-menu (list dummy) nil
			  w3-menu-view-menu)
	(easy-menu-define w3-menu-fsfemacs-options-menu (list dummy) nil
			  w3-menu-options-menu)
	(easy-menu-define w3-menu-fsfemacs-style-menu (list dummy) nil
			  w3-menu-style-menu)
	(easy-menu-define w3-menu-fsfemacs-search-menu (list dummy) nil
			  w3-menu-search-menu)

	;; block the global menubar entries in the map so that W3
	;; can take over the menubar if necessary.
	(define-key map [rootmenu] (make-sparse-keymap))
	(define-key map [rootmenu w3] (cons "W3" (make-sparse-keymap "W3")))
	(define-key map [rootmenu w3 file] 'undefined)
	(define-key map [rootmenu w3 files] 'undefined)
	(define-key map [rootmenu w3 search] 'undefined)
	(define-key map [rootmenu w3 edit] 'undefined)
	(define-key map [rootmenu w3 options] 'undefined)
	(define-key map [rootmenu w3 buffer] 'undefined)
	(define-key map [rootmenu w3 mule] 'undefined)
	(define-key map [rootmenu w3 tools] 'undefined)
	(define-key map [rootmenu w3 help] 'undefined)
	(define-key map [rootmenu w3 help-menu] 'undefined)
	;; now build W3's menu tree.
	(let ((menu-alist
	       '(
		 (bookmark
		  (cons "Bookmark" w3-menu-fsfemacs-bookmark-menu))
		 (debug
		  (cons "Debug" w3-menu-fsfemacs-debug-menu))
		 (edit
		  (cons "Edit" w3-menu-fsfemacs-edit-menu))
		 (file
		  (cons "File" w3-menu-fsfemacs-file-menu))
		 (go
		  (cons "Go" w3-menu-fsfemacs-go-menu))
		 (help
		  (cons "Help" w3-menu-fsfemacs-help-menu))
;;;		 (buffers
;;;		  (cons "Buffers" (lookup-key global-map [menu-bar buffer])))
		 (options
		  (cons "Options" w3-menu-fsfemacs-options-menu))
		 (view
		  (cons "View" w3-menu-fsfemacs-view-menu))
		 (style
		  (cons "Style" w3-menu-fsfemacs-style-menu))
		 (search
		  (cons "Search" w3-menu-fsfemacs-search-menu))
		 (emacs
		  ;; FIXME!!! Currently, win32 doesn't support buttons
		  ;; in menubars, so we hack around it and ignore the
		  ;; 'emacs keyword on that platform.  REMOVE THIS CODE
		  ;; as soon as that is fixed.  19.35 timeframe?
		  (if (eq (device-type) 'win32)
		      nil
		    (cons "[Emacs]" 'w3-menu-toggle-menubar)))))
	      cons
	      (vec (vector 'rootmenu 'w3 nil))
	      ;; menus appear in the opposite order that we
	      ;; define-key them.
	      (menu-list 
	       (if (consp w3-use-menus)
		   (reverse w3-use-menus)
		 (list 'help nil 'emacs 'buffers 'options 'bookmark
		       'go 'view 'edit 'file))))
	  (while menu-list
	    (if (null (car menu-list))
		nil;; no flushright support in Emacs
	      (aset vec 2 (intern (concat "w3-menu-fsfemacs-"
					  (symbol-name
					   (car menu-list)) "-menu")))
	      (setq cons (assq (car menu-list) menu-alist))
	      (if cons
		  (define-key map vec (eval (car (cdr cons))))))
	    (setq menu-list (cdr menu-list))))
	(setq w3-mode-menu-map map)
	(run-hooks 'w3-menu-setup-hook))))

(defun w3-menu-make-xemacs-menubar ()
  (let ((menu-alist
	 '((bookmark . w3-menu-bookmark-menu)
	   (style    . w3-menu-style-menu)
	   (buffers  . w3-menu-buffer-menu)
	   (debug    . w3-menu-debug-menu)
	   (edit     . w3-menu-edit-menu)
	   (emacs    . w3-menu-emacs-button)
	   (file     . w3-menu-file-menu)
	   (go       . w3-menu-go-menu)
	   (help     . w3-menu-help-menu)
	   (options  . w3-menu-options-menu)
	   (search   . w3-menu-search-menu)
	   (view     . w3-menu-view-menu)
	   )
	 )
	cons
	(menubar nil)
	(menu-list w3-use-menus))
    (while menu-list
      (cond
       ((and (featurep 'infodock)
	     (memq (car menu-list) '(nil emacs))))
       ((null (car menu-list))
	(setq menubar (cons nil menubar)))
       (t (setq cons (assq (car menu-list) menu-alist))
	  (if cons
	      (setq menubar (cons (symbol-value (cdr cons)) menubar)))))
      (setq menu-list (cdr menu-list)))
    (nreverse menubar)))

(defun w3-menu-install-menubar ()
  (cond
   ((featurep 'xemacs)
    (cond
     ((not (featurep 'menubar)) nil)	; No menus available
     ((featurep 'infodock) nil)		; InfoDock does it automatically
     (t
      (setq w3-menu-w3-menubar (w3-menu-make-xemacs-menubar))
      (set-buffer-menubar w3-menu-w3-menubar))))
   ((not (fboundp 'w3-menu-fsfemacs-bookmark-menu))
    (w3-menu-initialize-w3-mode-menu-map)
    (define-key w3-mode-map [menu-bar]
      (lookup-key w3-mode-menu-map [rootmenu w3])))))

(defun w3-menu-install-menubar-item ()
  (cond
   ((featurep 'xemacs)
    (if (not (featurep 'menubar))
	nil				; No menus available
      (set-buffer-menubar (copy-sequence (w3-menu-global-menubar)))
      (add-menu nil "W3" (cdr w3-menu-w3-menubar))))
   ((not (fboundp 'w3-menu-fsfemacs-edit-menu))
    (w3-menu-initialize-w3-mode-menu-map)
    (define-key w3-mode-map [menu-bar]
      (lookup-key w3-mode-menu-map [rootmenu])))))

(defun w3-menu-install-menus ()
  (cond ((consp w3-use-menus)
	 (w3-menu-install-menubar))
	((eq w3-use-menus 1)
	 (w3-menu-install-menubar-item))
	(t nil)))

(defun w3-menu-set-menubar-dirty-flag ()
  (cond ((featurep 'xemacs)
	 (set-menubar-dirty-flag))
	(t
	 (force-mode-line-update))))

(defun w3-menu-toggle-menubar ()
  (interactive)
  (cond
   ;;((eq w3-use-menus 1)
   ;;nil)
   ((featurep 'xemacs)
    (if (null (car (find-menu-item current-menubar '("XEmacs"))))
	(set-buffer-menubar w3-menu-w3-menubar)
      (set-buffer-menubar (copy-sequence (w3-menu-global-menubar)))
      (condition-case ()
	  (add-menu-button nil ["W3" w3-menu-toggle-menubar t] nil)
	(void-function
	 (add-menu-item nil "W3" 'w3-menu-toggle-menubar t))))
    (w3-menu-set-menubar-dirty-flag))
   (t
    (if (not (eq (lookup-key w3-mode-map [menu-bar])
		 (lookup-key w3-mode-menu-map [rootmenu w3])))
	(define-key w3-mode-map [menu-bar]
	  (lookup-key w3-mode-menu-map [rootmenu w3]))
      (define-key w3-mode-map [menu-bar]
	(make-sparse-keymap))
      (define-key w3-mode-map [menu-bar w3]
	(cons "[W3]" 'w3-menu-toggle-menubar)))
    (w3-menu-set-menubar-dirty-flag))))

(defvar print-readably)

(defun w3-menu-save-options ()
  (interactive)
  (let ((output-buffer (find-file-noselect w3-default-configuration-file))
	output-marker)
    (with-current-buffer output-buffer
      ;;
      ;; Find and delete the previously saved data, and position to write.
      ;;
      (goto-char (point-min))
      (if (re-search-forward "^;; W3 Options Settings *\n" nil 'move)
	  (let ((p (match-beginning 0)))
	    (goto-char p)
	    (or (re-search-forward
		 "^;; End of W3 Options Settings *\\(\n\\|\\'\\)"
		 nil t)
		(error "can't find END of saved state in .emacs"))
	    (delete-region p (match-end 0)))
	(goto-char (point-max))
	(insert "\n"))
      (setq output-marker (point-marker))
      (let ((print-readably t)
	    (print-escape-newlines t)
	    (standard-output output-marker))
	(princ ";; W3 Options Settings\n")
	(princ ";; ===================\n")
	(dolist (var 
                 '(
                   ps-print-color-p
                   url-automatic-caching
                   url-honor-refresh-requests
                   url-privacy-level
                   url-cookie-confirmation
                   url-proxy-services
                   url-standalone-mode
                   url-use-hypertext-gopher
                   w3-default-homepage
                   w3-default-stylesheet
                   w3-delay-image-loads
                   w3-do-incremental-display
                   w3-dump-to-disk
                   w3-honor-stylesheets
                   w3-image-mappings
                   w3-load-hook
                   w3-mode-hook
                   w3-netscape-compatible-comments
                   w3-preferences-cancel-hook
                   w3-preferences-default-hook
                   w3-preferences-ok-hook
                   w3-preferences-setup-hook
                   w3-source-file-hook
                   w3-toolbar-orientation
                   w3-toolbar-type
                   w3-use-menus
                   w3-user-colors-take-precedence
                   ))
          (princ "  ")
          (if (and (symbolp var) (boundp var))
              (prin1 (list 'setq-default var
                           (let ((val (symbol-value var)))
                             (if (or (memq val '(t nil))
                                     (and (not (symbolp val))
                                          (not (listp val))))
                                 val
                               (list 'quote val))))))
          (if var (princ "\n")))
	(princ ";; ==========================\n")
	(princ ";; End of W3 Options Settings\n")))
    (set-marker output-marker nil)
    (with-current-buffer output-buffer
      (save-buffer))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for emacs variations that do not support the :filter
;;; keyword in menu items.  All versions of XEmacs that Emacs/W3 can
;;; run on support this, but only really recent (20.3 or later)
;;; versions of Emacs support this.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w3-menu-e19-bogus-filter-constructor (_name menu)
  (let ((x nil)
	(y nil))
    (setq x (x-popup-menu t menu)
	  y (and x (lookup-key menu (apply 'vector x))))
    (if (and x y)
	(funcall y))))
  






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Context-sensitive popup menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'w3-event-glyph
  (if (fboundp 'event-glyph) #'event-glyph #'ignore))

(defun w3-menu-popup-menu (menu)
  (if (fboundp 'popup-menu)
      (popup-menu menu)
    (let ((bogus-menu nil))
      (easy-menu-define bogus-menu nil nil menu)
    (w3-menu-e19-bogus-filter-constructor "Popup" bogus-menu))))

(defun w3-popup-menu (e)
  "Pop up a menu of common w3 commands"
  (interactive "e")
  (if (not w3-popup-menu-on-mouse-3)
      (call-interactively (lookup-key global-map (vector w3-mouse-button3)))
    (mouse-set-point e)
    (let* ((glyph (w3-event-glyph e))
	   (widget (or (and glyph (glyph-property glyph 'widget))
		       (widget-at (point))))
	   (parent (and widget (widget-get widget :parent)))
	   (href (or (and widget (widget-get widget :href))
		     (and parent (widget-get parent :href))))
	   (imag (or (and widget (widget-get widget :src))
		     (and parent (widget-get parent :src))))
	   (menu (copy-tree w3-popup-menu))
	   url trunc-url)
      (if href
	  (progn
	    (setq url href)
	    (if url (setq trunc-url (url-truncate-url-for-viewing
				     url
				     w3-max-menu-width)))
	    (setcdr menu (append (cdr menu)
				 '("---")
				 (mapcar
				  (function
				   (lambda (x)
				     (vector (format (car x) trunc-url)
					     (list (cdr x) url) t)))
				  w3-hyperlink-menu)))))
      (if imag
	  (progn
	    (setq url imag
		  trunc-url (url-truncate-url-for-viewing url
							  w3-max-menu-width))
	    (setcdr menu (append (cdr menu)
				 '("---")
				 (mapcar
				  (function
				   (lambda (x)
				     (vector (format (car x) trunc-url)
					     (list (cdr x) url) t)))
				  w3-graphlink-menu)))))
      (if (not (w3-menubar-active))
	  (setcdr menu (append (cdr menu)
			       '("---" ["Show Menubar" w3-toggle-menubar t]))))
      (w3-menu-popup-menu menu))))

(provide 'w3-menu)
