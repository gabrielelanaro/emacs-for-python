;;; toolbar-x.el --- fancy toolbar handling in Emacs and XEmacs

;; Copyright (C) 2004, 2005, 2008 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA 02110-1301 USA

;;; Author: Miguel Vinicius Santini Frasson

;;; Commentary:
;; This program implements a common interface to display toolbar
;; buttons in both Emacs and XEmacs.  A toolbar should be basicly
;; defined by a image and a command to run when the button is pressed,
;; and additional properties could be added.  This is the idea of this
;; program.  See the documentation of function
;; `toolbarx-install-toolbar' for a description of how to specify
;; toolbars.

;;; Features:

;; * Button properties are given in the toolbar definition (BUTTON
;; paramenter in `toolbarx-install-toolbar') and/or in an alist with
;; associates the symbol with properties (MEANING-ALIST paramenter in
;; `toolbarx-install-toolbar').

;; * Supported properties:
;; - All editors: `:insert', `:image', `:command', `:help', `:enable',
;;		  `:append-command' and `:prepend-command';
;; - Emacs only: `:visible' and `:button';
;; - XEmacs only: `:toolbar'.
;; For the precise value-type for each property, see documentation of
;; the function `toolbarx-install-toolbar'.
;; (ps: properties that are particular to an editor are just ignored
;; the other editor flavour.)

;; * Button properties may depend on the editor flavour, if the value
;; is a vector; the first element will be used for Emacs and the 2nd
;; for XEmacs. Example: `:image ["new" toolbar-file-icon]'

;; * Properties can have value specified by function (with no
;; argument) or variables that evaluate to an object of the correct
;; type for a particular property.  The evaluation is done when the
;; roolbar is refresh (a call of `toolbarx-refresh'.)
;; (ps: this is valid only for properties that *not* have \`form\' as
;; value type.)

;; * On `refresh time' (a call `toolbarx-refresh', necessary when the
;; toolbar should change), the `:insert' property (if present) is
;; evaluated to decide if button will be displayed.

;; Properties can be distributed to several buttons, using \`groups\'.
;; Example: (for (bar baz :toolbar (bottom . top) :insert foo-form)
;; means that `foo', `bar' and `baz' have `:insert foo-form' and `bar' and
;; `baz' have the property `:toolbar (bottom .	top)'.	(ps: this type
;; of value for the `:toolbar' property (XEmacs only) means that the
;; buttons will be in the bottom toolbar unless the default toolbar is
;; in the bottom, and in this case, this buttons go to the top
;; toolbar).

;; * (Part of) the toolbar definition can be stored in a variable,
;; evaluated in `installation time'.  See `:eval-group' on the
;; documentation of the function `toolbarx-install-toolbar'.

;; * It is possible to define sets of buttons that appear according to
;; an option selected in a dropdown menu.  See `:dropdown-group' on
;; the documentation of the function `toolbarx-install-toolbar'.

;;; Rough description of the implementation
;; There are 3 \`engines\' implemented:

;; == the 1st one (parsing) parses the toolbar definition
;; independently of editor flavour and store the parsed buttons with
;; their properties, in the same order that they appear in the
;; definitions, in a variable `toolbarx-internal-button-switches';

;; == the 2nd one (refresh for Emacs) inserts buttons in the Emacs
;; toolbar in the same order that they appear in the definitions;
;; buttons with a `:insert' property value that evaluates to nil are
;; ignored; if a (real) button does not have at least (valid) image
;; and command properties, they are silently ignored;

;; == the 3rd engine (refresh for XEmacs) is similar to the 2nd, but
;; inserts buttons in XEmacs.

;;; History:

;; This program was motivated by the intention of implementation of a
;; good toolbar for AUCTeX, that would work in both Emacs and XEmacs.
;; Since toolbars are very different in behaviour and implementation
;; (for instance, in Emacs one can display as many toolbar buttons as
;; wanted, because it becomes mult-line, and in XEmacs, there is one
;; line, but toolbars and all sides of a frame.)


;;; Code:

;; Note that this just gives a useful default.  Icons are expected to
;; be in subdirectory "images" or "toolbar" relative to the load-path.
;; Packages loading toolbarx are advised to explicitly add their own
;; searchpath with add-to-list here even when they fulfill that
;; criterion: another package might have loaded toolbar-x previously
;; when load-path was not yet correctly set.  The default setting
;; really caters only for toolbar-x' stock icons.

(defvar toolbarx-image-path
  (nconc
   (delq nil (mapcar #'(lambda(x)
			 (and x
			      (member
			       (file-name-nondirectory
				(directory-file-name x))
			       '("toolbar" "images"))
			      ;;(file-directory-p x)
			      x))
		     load-path))
   (list data-directory))
  "List of directories where toolbarx finds its images.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First engine: Parsing buttons

;; it obtains button information, process it and stores result in
;; `toolbarx-internal-button-switches', which is a list with 1st
;; element the symbol `:switches', the 2nd element as a list of
;; processed buttons, and the 3rd element is used for Emacs to store
;; the keys used in ``constant'' buttons.

;; The 2nd element of `toolbarx-internal-button-switches' is a list
;; where each element is either:
;;  * a button-list, that is, a list with elements to define a button.
;;  * a list where 1st elem is `:insert' and 2nd is a form, and the
;; following elements are in the same format of the 2nd element of
;; `toolbarx-internal-button-switches'.

(defun toolbarx-make-string-from-symbol (symbol)
  "Return a string from the name of a SYMBOL.
Upcase initials and replace dashes by spaces."
  (let* ((str (upcase-initials (symbol-name symbol)))
	 (str2))
    (dolist (i (append str nil))
      (if (eq i 45)			; if dash, push space
	  (push 32 str2)
	(push i str2)))			; else push identical
    (concat (nreverse str2))))

(defun toolbarx-make-symbol-from-string (string)
  "Return a (intern) symbol from STRING.
Downcase string and replace spaces by dashes."
  (let* ((str1 (append (downcase string) nil))
	 (str2))
    (dolist (i str1)
      (if (eq i 32)			; if dash, push space
	  (push 45 str2)
	(push i str2)))
    (intern (concat (nreverse str2)))))

(defun toolbarx-good-option-list-p (option-list valid-options)
  "Non-nil means the OPTION-LIST is of form (OPT FORM ... OPT FORM).
Each OPT is member of VALID-OPTIONS and OPT are pairwise
different.  OPTION-LIST equal to nil is a good option list."
  (let ((elt-in-valid t)
	(temp-opt-list option-list)
	(list-diff)
	(n (/ (length option-list) 2)))
    (dotimes (i n)
      (when (> i 0)
	(setq temp-opt-list (cddr temp-opt-list)))
      (add-to-list 'list-diff
		   (car temp-opt-list))
      (setq elt-in-valid (and elt-in-valid
			      (memq (car temp-opt-list)
				    valid-options))))
    (and elt-in-valid			; options are on VALID-OPTOPNS
	 ;; OPTION-LIST has all option different from each other
	 (eq (length list-diff) n)
	 ;; OPTION-LIST has even number of elements
	 (eq (% (length option-list) 2) 0))))

(defun toolbarx-separate-options (group-list valid-options &optional check)
  "Return a cons cell with non-options and options of GROUP-LIST.
The options-part is the largest tail of the list GROUP-LIST that
has an element of VALID-OPTIONS (the comparation is made with
`memq'.)  The non-options-part is the beginning of GROUP-LIST
less its tail.	Return a cons cell which `car' is the
non-options-part and the `cdr' is the options-part.

If CHECK is non-nil, the tail is the largest that yield non-nil
when applied to `toolbarx-good-option-list-p'."
  (let ((maximal)
	(temp))
    (dolist (i valid-options)
      (setq temp (memq i group-list))
      (when (and (> (length temp) (length maximal))
		 (if check
		     (toolbarx-good-option-list-p temp valid-options)
		   t))
	(setq maximal (memq i group-list))))
    (cons (butlast group-list (length maximal)) maximal)))


(defun toolbarx-merge-props (inner-props outer-props override add)
  "Merge property lists INNER-PROPS and OUTER-PROPS.
INNER-PROPS and OUTER-PROPS are two lists in the format
 (PROP VAL PROP VAL ... PROP VAL).
Returns a list with properties and values merged.

OVERRIDE and ADD are supposed to be lists of symbols.  The value
of a property in OVERRIDE is the one on OUTER-PROPS or
INNER-PROPS, but if the property is in both, the value in
INNER-PROPS is used.  The value of a property in ADD will be a
list with first element the symbol `:add-value-list' and the rest
are the properties, inner properties first."
  (let* ((merged)
	 (inner-prop)
	 (outer-prop))
    (dolist (prop override)
      (if (memq prop inner-props)
	  (setq merged (append merged
			       (list prop (cadr (memq prop inner-props)))))
	(when (memq prop outer-props)
	  (setq merged (append merged
			       (list prop (cadr (memq prop outer-props))))))))
    (dolist (prop add merged)
      (setq inner-prop (memq prop inner-props))
      (when inner-prop
	(if (and (listp (cadr inner-prop))
		 (eq (car (cadr inner-prop)) :add-value-list))
	    (setq inner-prop (cdr (cadr inner-prop)))
	  (setq inner-prop (list (cadr inner-prop)))))
      (setq outer-prop (memq prop outer-props))
      (when outer-prop
	(if (and (listp (cadr outer-prop))
		 (eq (car (cadr outer-prop)) :add-value-list))
	    (setq outer-prop (cdr (cadr outer-prop)))
	  (setq outer-prop (list (cadr outer-prop)))))
      (when (append inner-prop outer-prop)
	(setq merged (append merged
			     (list prop (cons :add-value-list
					      (append inner-prop
						      outer-prop)))))))))

(defun toolbarx-make-command (comm prep app)
  "Return a command made from COMM, PREP and APP.
COMM is a command or a form.  PREP and APP are forms.  If PREP or
APP are non-nil, they are added to the resulting command at the
beginning and end, respectively.  If both are nil and COMM is a
command, COMM is returned."
  (let ((comm-is-command (commandp comm)))
    (if (and (not prep)
	     (not app)
	     comm-is-command)
	comm
      (append '(lambda nil (interactive))
	      (when prep (list prep))
	      (when comm
		(if comm-is-command
		    `((call-interactively (function ,comm)))
		  (list comm)))
	      (when app (list app))))))

;; in Emacs, menus are made of keymaps (vectors are possible, but editors
;; handle `menu titles' differently) meanwhile in XEmacs, menus are lists of
;; vectors

(defun toolbarx-emacs-mount-popup-menu
  (strings var type &optional title save)
  "Return an interactive `lambda'-expression that shows a popup menu.
This function is the action of `toolbarx-mount-popup-menu' if
inside Emacs. See documentation of that function for more."
  ;; making the menu keymap by adding each menu-item definition
  ;; see (info "(elisp)Menu keymaps")
  (let* ((keymap (make-sparse-keymap title))
	 (count 1)
	 (used-symbols '(nil))
	 (key)
	 (real-type (if (eq type 'toggle) 'toggle 'radio))
	 (real-save (when save (if (eq save 'offer) 'offer 'always))))
    ;; warn if type is not `radio' ot `toggle'; use `radio' if incorrect.
    (unless (eq type real-type)
      (display-warning 'toolbarx
		       (format (concat "TYPE should be symbols `radio' or "
				       "`toggle', but %s found; using `radio'")
			       type)))
    ;; warn if save is not `nil', `offer' or `always'; use nil when incorrect
    (unless (eq save real-save)
      (setq real-save nil)
      (display-warning 'toolbarx
		       (format (concat "SAVE should be symbols `nil', "
				       "`offer' or `always', but %s found; "
				       "using `nil'")
			       save)))
    (dolist (i strings)
      ;; finding a new symbol
      (let* ((aux-count 0)
	    (i-symb (toolbarx-make-symbol-from-string i)))
	(setq key i-symb)
	(while (memq key used-symbols)
	  (setq aux-count (1+ aux-count))
	  (setq key (intern (format "%s-%d" i-symb aux-count))))
	(setq used-symbols (cons key used-symbols)))
      (define-key-after keymap (vector key)
	`(menu-item ,i
		    ,(append
		      `(lambda nil (interactive)
			 ,(if (eq real-type 'radio)
			      `(setq ,var ,count)
			    `(if (memq ,count ,var)
				(setq ,var (delete ,count ,var))
			       (setq ,var (sort (cons ,count ,var) '<))))
			 (toolbarx-refresh))
		      (when (eq real-save 'always)
			`((customize-save-variable
			   (quote ,var) ,var)))
		      `(,var))
		    :button ,(if (eq real-type 'radio)
				 `(:radio eq ,var ,count)
			       `(:toggle memq ,count ,var))))
      (setq count (1+ count)))
    (when (eq real-save 'offer)
      (define-key-after keymap [sep] '(menu-item "--shadow-etched-in-dash"))
      (let* ((aux-count 0)
	     (i-symb 'custom-save))
	(setq key i-symb)
	(while (memq key used-symbols)
	  (setq aux-count (1+ aux-count))
	  (setq key (intern (format "%s-%d" i-symb aux-count))))
	(setq used-symbols (cons key used-symbols)))
      (define-key-after keymap (vector key)
	`(menu-item "Save state of this menu"
		   (lambda nil (interactive)
		     (customize-save-variable (quote ,var) ,var)))))
    ;; returns a `lambda'-expression
    `(lambda nil (interactive) (popup-menu (quote ,keymap)))))

(defun toolbarx-xemacs-mount-popup-menu
  (strings var type &optional title save)
  "Return an interactive `lambda'-expression that shows a popup menu.
This function is the action of `toolbarx-mount-popup-menu' if
inside XEmacs. See documentation of that function for more."
  (let* ((menu (if (and title (stringp title))
		   (list title)
		 (setq title nil)
		 (list "Dropdown menu")))
	 (count 0)
	 (menu-item)
	 (menu-callback)
	 (real-type (if (eq type 'toggle) 'toggle 'radio))
	 (real-save (when save (if (eq save 'offer) 'offer 'always))))
    ;; warn if type is not `radio' ot `toggle'; use `radio' if incorrect.
    (unless (eq type real-type)
      (warn (concat "TYPE should be symbols `radio' or `toggle', "
		    "but %s found; using `radio'") type))
    ;; warn if save is not `nil', `offer' or `always'; use nil when incorrect
    (unless (eq save real-save)
      (setq real-save nil)
      (display-warning 'toolbarx
		       (format (concat "SAVE should be symbols `nil', "
				       "`offer' or `always', but %s found; "
				       "using `nil'")
			       save)))
    ;; making the menu list of vectors
    (dolist (str strings)
      (setq count (1+ count))
      (setq menu-callback (list 'progn
				(if (eq real-type 'radio)
				    `(setq ,var ,count)
				  `(if (memq ,count ,var)
				       (setq ,var (delete ,count ,var))
				     (setq ,var (sort (cons ,count ,var) '<))))
				'(toolbarx-refresh)))
      (when (eq real-save 'always)
	(setq menu-callback (append menu-callback
				    (list (list 'customize-save-variable
						(list 'quote var) var)))))
      (setq menu-item (vector str menu-callback
			      :style real-type
			      :selected (if (eq real-type 'radio)
					     `(eq ,var ,count)
					   `(memq ,count ,var))))
      (setq menu (append menu (list menu-item))))
    (when (eq real-save 'offer)
      (setq menu (append menu (list "--:shadowEtchedInDash")))
      (setq menu (append menu (list
			       (vector
				"Save state of this menu"
				`(customize-save-variable (quote ,var)
							  ,var))))))
    ;; returnung the lambda-expression
    `(lambda nil (interactive)
       (let ((popup-menu-titles ,(if title t nil)))
	 (popup-menu (quote ,menu))))))

(defun toolbarx-mount-popup-menu (strings var type &optional title save)
  "Return a command that show a popup menu.
The return is a `lambda'-expression with a interactive declaration.

STRINGS is a list of strings which will be the itens of the menu.

VAR is a symbol that is set when an item is clicked.  TYPE should
be one of the symbols `radio' or `toggle': `radio' means that the
nth item is selected if VAR is `n' and this item sets VAR to `n';
`toggle' means that VAR should be a list of integers and the nth
item is selected if `n' belongs to VAR.	 The item inserts or
deletes `n' from VAR.

TITLE is a string (the title of the popup menu) or nil for no
title.

SAVE is one of the symbols nil, `offer' or `always'.  If value
is nil, do not try to save anything.  If it is `offer', a menu
item is added offering the user the possibiity to save state of
that dropdown menu for future sesseions (using `custom').  If it
is `always', state is saved every time that a item is clicked."
  (if (featurep 'xemacs)
      (toolbarx-xemacs-mount-popup-menu strings var type title save)
    (toolbarx-emacs-mount-popup-menu strings var type title save)))

(defun toolbarx-option-value (opt)
  "Return option value according to Emacs flavour.
If OPT is a vector, return first element if in Emacs or
second if in XEmacs.  Otherwise, return OPT.
If OPT is vector and length is smaller than the necessary (like
if in XEmacs and vector has length 1), then nil is returned."
  (if (vectorp opt)
      (if (featurep 'xemacs)
	  (when (> (length opt) 1)
	    (aref opt 1))
	(when (> (length opt) 0)
	  (aref opt 0)))
    opt))

(defun toolbarx-eval-function-or-symbol (object type-test-func)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ non-nil means that VAL is a valid value, according to
the car of the result of TYPE-TEST-FUNCTION, that should return a
cons cell in the same format as the return of this function.

If OBJECT applied to TYPE-TEST-FUNC return (GOOD-OBJ . VAL), and
GOOD-OBJ is non-nil, return that.  Else, check if OBJECT is a
function.  If so, evaluate and test again with TYPE-TEST-FUNC.	If
not a function or if GOOD-OBJ is again nil, test if OBJECT is a
bound symbol, evaluate that and return the result of
TYPE-TEST-FUNC."
  (let* ((ret (funcall type-test-func object)))
    (unless (car ret)
      (if (functionp object)
	  (progn
	    (setq ret (funcall type-test-func (funcall object)))
	    (unless (car ret)
	      (when (and (symbolp object) (boundp object))
		(setq ret (funcall type-test-func (symbol-value object))))))
	;; ok, obj is not function; try symbol
	(when (and (symbolp object) (boundp object))
	  (setq ret (funcall type-test-func (symbol-value object))))))
    ret))

(defun toolbarx-test-image-type (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid image object VAL (see
documentation of function `toolbarx-process-symbol')."
  (let ((toolbarx-test-image-type-simple
	 (lambda (img)
	   (let* ((val (toolbarx-option-value img))
		  (all-obj-ok t)
		  (good-obj
		   (if (featurep 'xemacs)
		       ;; if XEmacs
		       (or (stringp val) ; a string
			   (glyphp val)  ; or a glyph
			   (and (symbolp val) ; or a symbol bound to a
				(boundp val)  ; glyph-list
				(check-toolbar-button-syntax
				 (vector val
					 (lambda nil (interactive))
					 nil nil) t))
			   (and (listp val) ; or a glyph-or-string list
				(> (length val) 0)
				(< (length val) 7)
				(dolist (i val all-obj-ok)
				  (setq all-obj-ok
					(and all-obj-ok
					     (or (not i)
						 (stringp i)
						 (glyphp i)))))))
		     ;; if Emacs
		     (or (stringp val)	  ; string
			 (and (consp val) ; or image descriptor
			      (eq (car val) 'image))
			 (and (symbolp val) ; or a symbol bound to a
			      (boundp val)  ; image descriptor
					    ; (defined with `defimage')
			      (consp (eval val))
			      (eq (car (eval val)) 'image))
			 (and (listp val) ; or list with 4 strings or
					  ; image descriptors
			      (= (length val) 4)
			      (dolist (i val all-obj-ok)
				(setq all-obj-ok
				      (and all-obj-ok
					   (or (stringp i)
					       (and (consp i)
						    (eq (car i)
							'image)))))))))))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-image-type-simple)))

(defun toolbarx-test-button-type (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid button object VAL (see
documentation of function `toolbarx-process-symbol')."
  (let ((toolbarx-test-button-type-simple
	 (lambda (but)
	   (let* ((val (toolbarx-option-value but))
		  (good-obj (if (featurep 'xemacs)
				;; if XEmacs
				t
			      ;; if Emacs
			      (and (consp val)
				   (memq (car val) '(:toggle :radio))))))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-button-type-simple)))

(defun toolbarx-test-any-type (obj)
  "Return a cons cell (t . VAL).
If OBJ is vector, return VAL according to editor.  Else, return
OBJ, because it is a form anyway."
  (cons t (toolbarx-option-value obj)))

(defun toolbarx-test-string-or-nil (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid help object VAL (see
documentation of function `toolbarx-process-symbol')."
  (let ((toolbarx-test-string-or-nil-simple
	 (lambda (obj)
	   (let* ((val (toolbarx-option-value obj))
		  (good-obj (or (stringp val)
				(not val))))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-string-or-nil-simple)))

(defun toolbarx-test-toolbar-type (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid toolbar property object
VAL (see documentation of function `toolbarx-process-symbol')."
  (let ((toolbarx-test-toolbar-type-simple
	 (lambda (obj)
	   (let* ((val (toolbarx-option-value obj))
		  (all-but-def-opts '(top bottom left right))
		  (all-opts '(default top bottom left right))
		  (good-obj
		   (if (featurep 'xemacs)
		       ;; if XEmacs
		       (if (symbolp val)
			   (memq val all-opts)
			 (and (consp val)
			      (memq (car val) all-but-def-opts)
			      (memq (cdr val) all-but-def-opts)))
		     ;; if Emacs
		     t)))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-toolbar-type-simple)))

(defun toolbarx-test-dropdown-type (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid `:type' property object
VAL of a dropdown group (see documentation of function
`toolbarx-process-dropdown-group'."
  (let ((toolbarx-test-dropdown-type-simple
	 (lambda (obj)
	   (let* ((val (toolbarx-option-value obj))
		  (good-obj (memq val '(radio toggle))))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-dropdown-type-simple)))

(defun toolbarx-test-symbol (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid `:variable' property
object VAL of a dropdown group (see documentation of function
`toolbarx-process-dropdown-group'."
  (let ((toolbarx-test-symbol-simple
	 (lambda (obj)
	   (let* ((val (toolbarx-option-value obj))
		  (good-obj (symbolp val)))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-symbol-simple)))

(defun toolbarx-test-dropdown-default (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid `:default' property
object VAL of a dropdown group (see documentation of function
`toolbarx-process-dropdown-group'."
  (let ((toolbarx-test-dropdown-default-simple
	 (lambda (obj)
	   (let* ((val (toolbarx-option-value obj))
		  (good-obj (or (integerp val)
				(and (listp val)
				     (let ((ok t))
				       (dolist (i val ok)
					 (setq ok (and ok (integerp i)))))))))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj
				      toolbarx-test-dropdown-default-simple)))

(defun toolbarx-test-dropdown-save (obj)
  "Return a cons cell (GOOD-OBJ . VAL).
GOOD-OBJ is non-nil if OBJ yields a valid `:save' property
object VAL of a dropdown group (see documentation of function
`toolbarx-process-dropdown-group'."
  (let ((toolbarx-test-dropdown-save-simple
	 (lambda (obj)
	   (let* ((val (toolbarx-option-value obj))
		  (good-obj (memq val '(nil offer always))))
	     (cons good-obj val)))))
    (toolbarx-eval-function-or-symbol obj toolbarx-test-dropdown-save-simple)))

(defconst toolbarx-button-props
  (let* ((props-types-alist
	  '((:image	      toolbarx-test-image-type)
	    (:command	      toolbarx-test-any-type)
	    (:enable	      toolbarx-test-any-type)
	    (:visible	      toolbarx-test-any-type)
	    (:help	      toolbarx-test-string-or-nil)
	    (:insert	      toolbarx-test-any-type	   . and)
	    (:toolbar	      toolbarx-test-toolbar-type)
	    (:button	      toolbarx-test-button-type)
	    (:append-command  toolbarx-test-any-type	   . progn)
	    (:prepend-command toolbarx-test-any-type	   . progn)))
	 (possible-props (nreverse (let* ((props ()))
				     (dolist (p props-types-alist props)
				       (setq props (cons (car p) props))))))
	 (props-override (nreverse (let* ((props ()))
				     (dolist (p props-types-alist props)
				       (unless (cddr p)
					 (setq props (cons (car p) props)))))))
	 (props-add (nreverse (let* ((props ()))
				(dolist (p props-types-alist props)
				  (when (cddr p)
				    (setq props (cons (car p) props))))))))
    (list props-types-alist possible-props props-override props-add))
  "List yielding all encarnations of properties of a button.
First element: alist, where each element is of form
 (PROP . (TYPE-TEST-FUNCTION . ADD-OR-NIL))
Second is a list with all properties.
Third, a list with properties that override when merging.
Fourth, a list of lists, each in the format (PROP ADD).")

(defconst toolbarx-dropdown-props
  ;; for naming dropdown properties see `Convention' in the doc string
  (let* ((props-types-alist
	  '((:type		       toolbarx-test-dropdown-type)
	    (:variable		       toolbarx-test-symbol)
	    (:default		       toolbarx-test-dropdown-default)
	    (:save		       toolbarx-test-dropdown-save)
	    (:title		       toolbarx-test-string-or-nil)
	    (:dropdown-image	       toolbarx-test-image-type)
	    (:dropdown-enable	       toolbarx-test-any-type)
	    (:dropdown-visible	       toolbarx-test-any-type)
	    (:dropdown-insert	       toolbarx-test-any-type	    . and)
	    (:dropdown-help	       toolbarx-test-string-or-nil)
	    (:dropdown-toolbar	       toolbarx-test-toolbar-type)
	    (:dropdown-append-command  toolbarx-test-any-type	    . progn)
	    (:dropdown-prepend-command toolbarx-test-any-type	    . progn)))
	 (possible-props (nreverse (let* ((props ()))
				     (dolist (p props-types-alist props)
				       (setq props (cons (car p) props))))))
	 (props-override (nreverse (let* ((props ()))
				     (dolist (p props-types-alist props)
				       (unless (cddr p)
					 (setq props (cons (car p) props)))))))
	 (props-add (nreverse (let* ((props ()))
				(dolist (p props-types-alist props)
				  (when (cddr p)
				    (setq props (cons (car p) props))))))))
    (list props-types-alist possible-props props-override props-add))
  "List yielding all encarnations of properties of a dropdown group.
First element: alist, where each element is of form
 (PROP . (TYPE-TEST-FUNCTION . ADD-OR-NIL))
Second is a list with all properties.
Third, a list with properties that override when merging.
Fourth, a list of lists, each in the format (PROP ADD).

Convention: properties for the dropdown button should be formed
with the strings \":dropdown-\" with the button property name
without `:'. This is used on the implementation.")

(defun toolbarx-process-group-without-insert (group-without-props
					      merged-props-without-insert
					      meaning-alist switches)
  "Return an updated version of SWITCHES.
GROUP-WITHOUT-PROPS and MERGED-PROPS-WITHOUT-INSERT are
preprocessed variables in `toolbarx-process-group'."
  (let ((current-switches switches))
    (dolist (i group-without-props current-switches)
      (setq i (toolbarx-option-value i))
      (if (symbolp i)
	  (setq current-switches
		(toolbarx-process-symbol i meaning-alist
					 merged-props-without-insert
					 current-switches))
	(when (listp i)
	  (setq current-switches
		(toolbarx-process-group i meaning-alist
					merged-props-without-insert
					current-switches)))))))

(defun toolbarx-process-group (group meaning-alist props switches)
  "Return an updated version of SWITCHES.
Append to already processed buttons (stored in SWITCHES) a
processed version of GROUP.  Groups are useful to distribute
properties.  External properties are given in PROPS, and merged
with the internal properties that are in the end of GROUP.  If
properties (after merge) contain a `:insert' property, return a
list where the first and second elements are `:insert' and its
value, and after that a list in the same format as SWITCHES."
  (cond
   ;; if DROPDOWN group
   ((eq (car group) :dropdown-group)
    (toolbarx-process-dropdown-group group meaning-alist props switches))
   ;; if EVAL group
   ((eq (car group) :eval-group)
    (let ((current-switches switches))
      (dolist (elt (cdr group) current-switches)
	(let ((eval-elt (eval elt)))
	  (setq current-switches
		(toolbarx-process-group (if (listp eval-elt)
					    eval-elt
					  (list eval-elt))
					meaning-alist props
					current-switches))))))
   ;; if normal group
   (t
    (let* ((splited-props
	    (toolbarx-separate-options
	     group (append (nth 1 toolbarx-button-props)
			   (nth 1 toolbarx-dropdown-props))))
	   (intern-props (cdr splited-props))
	   (group-without-props (car splited-props))
	   (merged-props
	    (toolbarx-merge-props intern-props props
				  (append (nth 2 toolbarx-button-props)
					  (nth 2 toolbarx-dropdown-props))
				  (append (nth 3 toolbarx-button-props)
					  (nth 3 toolbarx-dropdown-props)))))
      ;; check whether merged props have an `:insert'
      (if (memq :insert merged-props)
	  ;; if yes, prepend switches with a (:insert cond elements)
	  (let* ((memq-ins (memq :insert merged-props))
		 (ins-val (if (and (listp (cadr memq-ins))
				   (eq :add-value-list
				       (car (cadr memq-ins))))
			      ;; if property is add-value property
			      (let* ((p (assq
					 :insert
					 (nth 0 toolbarx-button-props)))
				     (add-list (list (cddr p)))
				     (prop-good-val))
				(dolist (val (cdr (cadr memq-ins)))
				  (setq prop-good-val (funcall (cadr p) val))
				  (when (car prop-good-val)
				    (setq add-list (cons (cdr prop-good-val)
							 add-list))))
				;; return: (nreverse add-list)
				(setq add-list (nreverse add-list))
				(if (eq 2 (length add-list))
				    (cadr add-list) ; just 1 value, no
				  add-list))	    ; add-function
			    ;; if property is not add-value
			    (cadr memq-ins)))
		 (merged-props-without-insert
		  (append (butlast merged-props (length memq-ins))
			  (cddr memq-ins)))
		 (group-switches
		  (toolbarx-process-group-without-insert
		   group-without-props merged-props-without-insert
		   meaning-alist nil)))
	    ;; return
	    (nreverse (cons (append (list :insert ins-val)
				    group-switches)
			    (nreverse switches))))
	;; if not, just append what is processed to switches
	(toolbarx-process-group-without-insert group-without-props
					       merged-props meaning-alist
					       switches))))))

(defun toolbarx-process-symbol (symbol meaning-alist props switches)
  "Process a button given by SYMBOL in MEANING-ALIST.
The processed button is appended in SWITCHES, which is returned.
Look for a association of SYMBOL in MEANING-ALIST for collecting
properties.  Such association is a list that represents either a
normal button (a description of the button) or an alias
group (the symbol is an alias for a group of buttons).	PROPS is
a externel list of properties that are merged and then applied to
the button.  Scope is given by GLOBAL-FLAG."
  ;; there are 3 situations: symbol is :new-line, there is an alias group
  ;; or a normal button
  (let ((button-assq (cdr (assq symbol meaning-alist))))
    (cond
     ((eq (car button-assq) :alias)
      ;; button association is ALIAS GROUP is passed to
      ;; `toolbarx-process-group' as is but without the car.
      ;; return: (toolbarx-process-group... returns updates switch
      (toolbarx-process-group (cdr button-assq) meaning-alist props switches))
     (t
      ;; NORMAL BUTTON (association is a list of properties)
      ;;
      ;; properties need to be processed, that is, merge internal
      ;; and external (given by PROPS) properties
      (let* (;; button properties defined in `toolbarx-button-props'
	     (props-override	(nth 2 toolbarx-button-props))
	     (props-add		(nth 3 toolbarx-button-props))
	     ;; split considering also dropdown-group properties
	     (button-assq-split
	      (toolbarx-separate-options
	       button-assq
	       (append (nth 1 toolbarx-button-props)
		       (nth 1 toolbarx-dropdown-props))))
	     (button-split-no-props (car button-assq-split))
	     (button-split-props (cdr button-assq-split))
	     ;; if there is no :image or :command in the props,
	     ;; try to get them from no-props part
	     (button-image-no-prop
	      (unless (memq :image button-split-props)
		(when (> (length button-split-no-props) 0)
		  (list :image (nth 0 button-split-no-props)))))
	     (button-command-no-prop
	      (unless (memq :command button-split-props)
		(when (> (length button-split-no-props) 1)
		  (list :command (nth 1 button-split-no-props)))))
	     (button-props (append button-split-props
				   button-image-no-prop
				   button-command-no-prop))
	     ;; merge props
	     (merged-props (toolbarx-merge-props button-props props
						 props-override
						 props-add)))
	;; return:
	(nreverse (cons (cons symbol merged-props) (nreverse switches))))))))

(defun toolbarx-process-dropdown-group (dropdown meaning-alist props switches)
  "Process buttons that appear according to dropdown menu.
Process a dropdown group DROPDOWN with meaning alist
MEANING-ALIST, external property list PROP and GLOBAL-FLAG
specifying scope. For a complete description, see documentation
of `toolbarx-install-toolbar'.	The processed buttons are stored
in the end of SWITCHES, which is returned."
  (let* ((dropdown-group (if (eq (car dropdown) :dropdown-group)
			     (cdr dropdown)
			   dropdown))
	 (dropdown-list-splited
	  (toolbarx-separate-options dropdown-group
				     (append
				      (nth 1 toolbarx-button-props)
				      (nth 1 toolbarx-dropdown-props))))
	 (dropdown-list	 (car dropdown-list-splited))
	 (dropdown-props (cdr dropdown-list-splited))
	 (merged-props
	  (toolbarx-merge-props dropdown-props props
				(append (nth 2 toolbarx-button-props)
					(nth 2 toolbarx-dropdown-props))
				(append (nth 3 toolbarx-button-props)
					(nth 3 toolbarx-dropdown-props))))
	 (merged-props-button-only
	  (let* ((props-button-only)
		 (prop))
	    (dolist (p (nth 1 toolbarx-button-props) props-button-only)
	      (setq prop (memq p merged-props))
	      (when prop
		(setq props-button-only
		      (append (list p (cadr prop))
			      props-button-only))))))
	 (merged-props-dropdown-only
	  (let* ((props-dropdown-only)
		 (prop))
	    (dolist (p (nth 1 toolbarx-dropdown-props) props-dropdown-only)
	      (setq prop (memq p merged-props))
	      (when prop
		(setq props-dropdown-only
		      (append (list p (cadr prop))
			      props-dropdown-only))))))
	 ;; get value for each property and check type ONLY for props that do
	 ;; not concern the dropdown button, like `:type', `:save', etc. The
	 ;; props that concern the button are going to be handled in refresh
	 ;; time.
	 (filtered-dropdown-group-props-only
	  (let* ((filtered-props-temp)
		 (prop-good-val)
		 (prop))
	    (save-match-data
	      (dolist (p (nth 0 toolbarx-dropdown-props) filtered-props-temp)
		(unless (string-match "^:dropdown-.*$"
				      (symbol-name (car p)))
		  ;;	property	   -> (car p)
		  ;;	test type function -> (cadr p)
		  (setq prop (memq (car p) merged-props-dropdown-only))
		  ;; if so, check if value is of correct type
		  (when prop
		    (setq prop-good-val (funcall (cadr p) (cadr prop)))
		    (if (car prop-good-val)
			(setq filtered-props-temp
			      (append filtered-props-temp
				      (list (car p) (cdr prop-good-val))))
		      (display-warning
		       'toolbarx
		       (format (concat "Wrong type for value in "
				       "property `%s' in dropdown group")
			       (car p))))))))))
	 ;; properties for the dropdown button from dropdown merged properties
	 (dropdown-button-props
	  (let* ((props))
	    (save-match-data
	      (dolist (pr (nth 1 toolbarx-dropdown-props))
		(when (and (memq pr merged-props-dropdown-only)
			   (string-match "^:dropdown-\\(.*\\)$"
					 (symbol-name pr)))
		  (let* ((new-pr (intern (concat ":"
						 (substring (symbol-name pr)
							    (match-beginning 1)
							    (match-end 1)))))
			 (val (cadr (memq pr merged-props-dropdown-only))))
		    (setq props (append (list new-pr val) props))))))
	    (unless (memq :image props)
	      (setq props (append (list :image "dropdown") props)))
	    props))
	 (dropdown-button-without-command
	  (cons 'dropdown dropdown-button-props))
	 ;; `:type' defaults to `radio'
	 (type (if (memq :type filtered-dropdown-group-props-only)
		   (cadr (memq :type filtered-dropdown-group-props-only))
		 'radio))
	 ;; `:default' defaults to 1 or nil depending on `type'
	 ;; if type is toggle and default is not a list, but a
	 ;; integer, set as the list with integer
	 (default
	   (let* ((memq-default (memq :default
				      filtered-dropdown-group-props-only))
		  (def-temp (cadr memq-default))
		  (default-temp (if memq-default
				    def-temp
				  (if (eq type 'radio) 1 (list 1)))))
	     default-temp))
	 ;; `:save' defaults to nil and require `:variable'
	 (save (let* ((save-temp
		       (when (memq :save filtered-dropdown-group-props-only)
			 (cadr (memq :save
				     filtered-dropdown-group-props-only)))))
		 (if (and save-temp
			  (not (memq :variable
				     filtered-dropdown-group-props-only)))
		     (progn
		       (display-warning
			'toolbarx
			(concat "`:save' property with non-nil value should "
				"be used only with the `:variable' property; "
				"using value nil for `:save'."))
		       nil)
		   save-temp)))
	 ;; `:title' defaults to nil
	 (title (when (memq :title filtered-dropdown-group-props-only)
		  (cadr (memq :title filtered-dropdown-group-props-only))))
	 ;; the menu variable is buildt from the `:variable' option or
	 ;; make a symbol not used
	 (variable (if (memq :variable filtered-dropdown-group-props-only)
		       (cadr (memq :variable
				   filtered-dropdown-group-props-only))
		     (let* ((count 0)
			    (symb (intern (format
					   "toolbarx-internal-menu-var-%d"
					   count))))
		       (while (boundp symb)
			 (setq count (1+ count))
			 (setq symb
			       (intern (format "toolbarx-internal-menu-var-%d"
					       count))))
		       symb)))
	 ;; auxiliary variables
	 (list-strings)
	 (list-buttons))
    ;; setting `variable'
    (if save
	(custom-declare-variable
	 variable default
	 "Used as variable of dropdown menu defined with `toolbarx'.")
      (when (not (boundp variable))
	(set variable default)))
    ;; now check `variable' content
    (set variable
	 (let ((val (eval variable)))
	   (if (eq type 'toggle)
	       (if (listp val)
		   val
		 (if (integerp val)
		     (list val)
		   (list 1)))
	     ;; then, type is radio
	     (if (integerp val)
		 val
	       (if (and val
			(listp val)
			(integerp (car val)))
		   (car val)
		 1)))))
    ;; === buiding `list-strings' and `list-buttons' ===
    ;; if only symbols, build `list-strings' and `list-buttons' from symbols
    (if (let ((only-symbols-flag t))
	  (dolist (i dropdown-list only-symbols-flag)
	    (setq only-symbols-flag (and only-symbols-flag (symbolp i)))))
	(let ((count 0))
	  (dolist (i dropdown-list)
	    ;; list-strings and list-buttons are buildt reversed
	    (setq list-strings (cons (toolbarx-make-string-from-symbol i)
				     list-strings))
	    (setq count (1+ count))
	    (setq list-buttons (cons (list i
					   :insert
					   (if (eq type 'radio)
					       (list 'eq count variable)
					     (list 'memq count variable)))
				     list-buttons))))
      ;; if not, the it must start with string
      (unless (stringp (car dropdown-list))
	(error "%s %s %s"
	       "If not all itens on dropdown are symbols, then a string"
	       "must come before each set of buttons; no string found"
	       "in first position."))
      (let ((count 0)
	    (elem)
	    (temp-list-buttons))
	(while dropdown-list
	  (setq elem (car dropdown-list))
	  (setq dropdown-list (cdr dropdown-list))
	  (if (stringp elem)
	      ;; if string, output `temp-list-buttons' and prepair it again
	      (progn
		;; list-strings and list-buttons are buildt reversed
		(setq list-strings (cons elem list-strings))
		(when temp-list-buttons
		  (setq list-buttons (cons (append (nreverse temp-list-buttons)
						   (list :insert
							 (if (eq type 'radio)
							     (list 'eq count
								   variable)
							   (list 'memq count
								 variable))))
					   list-buttons)))
		(setq temp-list-buttons nil)
		(setq count (1+ count)))
	    ;; else, if not string, just insert it to `temp-list-buttons'
	    ;; which is also buildt reversed
	    (setq temp-list-buttons (cons elem temp-list-buttons))))
	;; output last temp list, left behind
	(when temp-list-buttons
	  (setq list-buttons (cons (append (nreverse
					    temp-list-buttons)
					   (list
					    :insert (if (eq type 'radio)
							(list 'eq count
							      variable)
						      (list 'memq count
							    variable))))
				   list-buttons)))))
    ;; lists were made reversed (elements inserted at the beginning)
    (setq list-strings (nreverse list-strings))
    (setq list-buttons (nreverse list-buttons))
    ;; now, pass `list-buttons' as a group to `toolbarx-process-group'
    (let ((current-switches switches))
      (setq current-switches
	    (toolbarx-process-group list-buttons meaning-alist
				    merged-props ; pass non-processed props
				    current-switches))
      (setq current-switches
	    ;; outputing dropdown button
	    (toolbarx-process-group (append dropdown-button-without-command
					    (list :command
						  (toolbarx-mount-popup-menu
						   list-strings variable type
						   title save)))
				    meaning-alist merged-props-button-only
				    switches))
      current-switches)))



;; Still functions `toolbarx-install-toolbar' and `toolbarx-refresh'to
;; complete the parsing engine.	 Since they interface with other engines,
;; they must come in the end.

;;; How a image is made, giving a string as (part of) file name.

;; look at function `image-type-available-p' for Emacs !!!!

(defun toolbarx-find-image (image)
  "Return image descriptor or glyph for IMAGE.
In Emacs, return an image descriptor for IMAGE.  In XEmacs,
return a glyph.

IMAGE is string.  Usually IMAGE neither contains a directory nor
an extension.  If the extension is omitted, `xpm', `xbm' and
`pbm' are tried.  If the directory is omitted,
`toolbarx-image-path' is searched."
  ;; `find-image' in Emacs 21 looks in `load-path' and `data-directory'.  In
  ;; Emacs 22, we have `image-load-path' which includes `load-path' and
  ;; `data-directory'.
  ;;
  ;; If there's some API in XEmacs to find the images, we should use it
  ;; instead of locate-library.
  ;;
  ;; Emacs 22 has locate-file, but the other Emacsen don't.  The
  ;; following should hopefully get us to all images ultimately.

  (let ((file))
    (dolist (i '("" ".xpm" ".xbm" ".pbm"))
      (unless file
	(setq file (locate-library (concat image i) t toolbarx-image-path))))
    (if (featurep 'xemacs)
	(and file (make-glyph file))
      (if file
	  (create-image file)
	(find-image `((:type xpm :file ,(concat image ".xpm"))
		      (:type xbm :file ,(concat image ".xbm"))
		      (:type pbm :file ,(concat image ".pbm"))))))))

;; next variable interfaces between parsing and display engines
(defvar toolbarx-internal-button-switches nil
  "Store the list of processed buttons, used by `toolbarx-refresh'.
This variable can store different values for the different buffers.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Second engine: display parsed buttons in Emacs

(defun toolbarx-emacs-add-button (button used-keys keymap)
  "Insert a button where BUTTON is its description.
USED-KEYS should be a list of symbols, where the first element is
`:used-symbols'.  This list should store the symbols of the
buttons already inserted.  This list is changed by side effect.
KEYMAP is the keymap where the menu-item corresponding to the
tool-bal button is going to be inserted.  Insertion is made in
the end of KEYMAP.

BUTTON should be a list of form (SYMBOL . PROP-LIST).  SYMBOL is
a symbol that \"names\" this button.  PROP-LIST is a list in the
format (PROP VAL ... PROP VAL).	 The supported properties are
`:image', `:command', `:append-command', `:prepend-command',
`:help', `:enable', `:visible', `:button', `:insert' and
`:toolbar'. For a description of properties, see documentation of
function `toolbar-install-toolbar'."
  (let* ((symbol (nth 0 button))
	 (used-keys-list (when used-keys
			   (cdr used-keys)))
	 (filtered-props
	  (let* ((filtered-props-temp)
		 (prop-good-val)
		 (prop))
	    (dolist (p (nth 0 toolbarx-button-props) filtered-props-temp)
	      ;;    property	       -> (car p)
	      ;;    test type function -> (cadr p)
	      ;;    add-function       -> (cddr p)
	      (setq prop (memq (car p) button))
	      ;; if so, check if value is of correct type
	      (when prop
		;; if property is of add-type, them the value is a list
		;; (:add-value-list VAL VAL). Each VAL should be checked.
		(if (and (cddr p) (eq :add-value-list (car (cadr prop))))
		    (let* ((add-list (list (cddr p))))
		      (dolist (val (cdr (cadr prop)))
			(setq prop-good-val (funcall (cadr p) val))
			(when (car prop-good-val)
			  (setq add-list (cons (cdr prop-good-val) add-list))))
		      (setq add-list (nreverse add-list))
		      (when (eq 2 (length add-list)) ; just 1 value, no
						     ; add-function
			(setq add-list (cadr add-list)))
		      (setq filtered-props-temp (append
						 (list (car p) add-list)
						 filtered-props-temp)))
		  ;; if override-property
		  (setq prop-good-val (funcall (cadr p) (cadr prop)))
		  (when (car prop-good-val)
		    (setq filtered-props-temp (append
					       (list (car p)
						     (cdr prop-good-val))
					       filtered-props-temp))))))))
	 (insert (or (not (memq :insert filtered-props))
		     ;; (memq :insert filtered-props)
		     (eval (nth 1 (memq :insert filtered-props))))))
    (when insert
      (cond
       (t
	;; symbol is not :new-line, therefore a normal button
	(let* ((image (cadr (memq :image filtered-props)))
	       (image-descriptor
		(when (memq :image filtered-props)
		  (cond
		   ((stringp image) 	; string
		    (toolbarx-find-image image))
		   ((and (consp image)	; or image descriptor
			 (eq (car image) 'image))
		    image)
		   ((and (symbolp image) ; or a symbol bound to a
			 (boundp image)  ; image descriptor (defined
				       ; with `defimage')g
			 (consp (eval image))
			 (eq (car (eval image)) 'image))
		    (eval image))
		   (t			; otherwise, must be a list
					; with 4 strings or image
					; descriptors
		    (apply 'vector (mapcar (lambda (img)
					      (if (stringp img)
						  (toolbarx-find-image img)
						img))
					   image))))))
	       (command
		(let* ((com (nth 1 (memq :command filtered-props)))
		       (app (nth 1 (memq :append-command filtered-props)))
		       (prep (nth 1 (memq :prepend-command filtered-props))))
		  (when (or com app prep)
		    (toolbarx-make-command com prep app))))
	       (help (cons (memq :help filtered-props)
			   (cadr (memq :help filtered-props))))
	       (enable (cons (memq :enable filtered-props)
			     (cadr (memq :enable filtered-props))))
	       (visible (cons (memq :visible filtered-props)
			      (cadr (memq :visible filtered-props))))
	       (button (cons (memq :button filtered-props)
			     (cadr (memq :button filtered-props))))
	       (menuitem (append
			  (list 'menu-item
				(toolbarx-make-string-from-symbol symbol)
				command
				:image image-descriptor)
			  (when (car help)
			    (list :help (cdr help)))
			  (when (car enable)
			    (list :enable (cdr enable)))
			  (when (car visible)
			    (list :visible (cdr visible)))
			  (when (car button)
			    (list :button (cdr button)))))
	       (key-not-used
		(let* ((count 0)
		       (symb symbol))
		  (while (memq symb used-keys-list)
		    (setq count (1+ count))
		    (setq symb (intern (format "%s-%d" symbol count))))
		  symb)))
	  (when (and image-descriptor command)
	    (setq used-keys-list (cons key-not-used used-keys-list))
	    (define-key-after keymap
	      (vector key-not-used) menuitem))))))
    (when used-keys (setcdr used-keys used-keys-list))))


(defun toolbarx-emacs-refresh-process-button-or-insert-list (switches
							     used-keys
							     keymap)
  "Process SWITCHES, inserting buttons in `tool-bar-map'.
If a button is actually a `:insert' clause group (if `car' is
`:insert') and evaluation of `cdr' yields non-nil, process `cddr'
recursively as SWITCHES.  USED-KEYS is a list which `car' is
`:used-symbols' and which `cdr' is a list of symbols that have already
been used as keys in the keymap `tool-bar-map'."
  (dolist (button switches)
    (if (eq (car button) :insert)
	(when (eval (cadr button))
	  (toolbarx-emacs-refresh-process-button-or-insert-list (cddr button)
								used-keys
								keymap))
      (toolbarx-emacs-add-button button used-keys keymap))))



(defun toolbarx-emacs-refresh (&optional global-flag)
  "Refresh and redraw the toolbar in Emacs.
If GLOBAL-FLAG is non-nil, the default value of toolbar switches
is used and the default value of `toolbarx-map' is changed."
  (let* ((switches (if global-flag
		       (if (default-boundp 'toolbarx-internal-button-switches)
			   (default-value 'toolbarx-internal-button-switches)
			 toolbarx-internal-button-switches)
		     toolbarx-internal-button-switches))
	 (used-keys (list :used-symbols nil))
	 (tool-bar-map-temp (make-sparse-keymap)))
    (toolbarx-emacs-refresh-process-button-or-insert-list switches used-keys
							  tool-bar-map-temp)
    (if global-flag
	(setq-default tool-bar-map tool-bar-map-temp)
      (setq tool-bar-map tool-bar-map-temp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Third engine: display parsed buttons in XEmacs

(defun toolbarx-xemacs-image-properties (image)
  "Return a list of properties of IMAGE.
IMAGE should be a string or a list of one to six strings or
glyphs or nil, or a symbol bound to a list of one to six
glyphs (them must be a valid image list, like one created with
the function `toolbar-make-button-list').  Return a
list (GLYPH-LIST HEIGHT WIDTH) where HEIGHT (resp. WIDTH) is the
maximum of the heights (resp. widths) of all glyphs (or strings
converted to glyphs) in GLYPH-LIST.  If IMAGE is not a list, it
is treated as a list with IMAGE as only element.  Strings are
converted to glyphs with the function `toolbarx-find-image'.  If,
after possible string-to-glyph convertions, the list of glyphs
has nil as first element, GLYPH-LIST becomes nil."
  (let* ((glyph-list
	  (if (symbolp image)		; if symbol, them must be a
					; valid image list, like
					; created by function
					; `toolbar-make-button-list'
	      (eval image)
	    (let ((img-list (if (listp image)
				image
			      (list image)))
		  (glyph-list-temp))
	      ;; glyph-list-temp
	      (setq glyph-list-temp
		    (dolist (glyph img-list (nreverse glyph-list-temp))
		      (if (stringp glyph)
			  (setq glyph-list-temp
				(cons (toolbarx-find-image glyph)
				      glyph-list-temp))
			(setq glyph-list-temp (cons glyph glyph-list-temp)))))
	      (unless (car glyph-list-temp)
		(setq glyph-list-temp nil))
	      glyph-list-temp)))
	 (usable-buttons
	  ;; computing inheritage
	  (let* ((usable-temp))
	    (if toolbar-captioned-p	; problematic point :-(
		(progn
		  ;; CAP-UP:  cap-up -> up
		  (setq usable-temp (cons (cond
					   ((nth 3 glyph-list))
					   ((nth 0 glyph-list)))
					  usable-temp))
		  ;; CAP-DOWN:	cap-down -> cap-up -> down -> up
		  (setq usable-temp (cons (cond
					   ((nth 4 glyph-list))
					   ((nth 3 glyph-list))
					   ((nth 1 glyph-list))
					   ((nth 0 glyph-list)))
					  usable-temp))
		  ;; CAP-DISABLED:  cap-disabled -> cap-up -> disabled -> up
		  (setq usable-temp (cons (cond
					   ((nth 5 glyph-list))
					   ((nth 3 glyph-list))
					   ((nth 2 glyph-list))
					   ((nth 0 glyph-list)))
					  usable-temp)))
	      ;; UP:  up
	      (setq usable-temp (cons (nth 0 glyph-list) usable-temp))
	      ;; DOWN:	down -> up
	      (setq usable-temp (cons (cond
				       ((nth 1 glyph-list))
				       ((nth 0 glyph-list)))
				      usable-temp))
	      ;; DISABLED:  disabled -> up
	      (setq usable-temp (cons (cond
				       ((nth 2 glyph-list))
				       ((nth 0 glyph-list)))
				      usable-temp)))
	    usable-temp))
	 (height (apply 'max 0 (mapcar (lambda (glyph)
					 (if glyph
					     (glyph-height glyph)
					   0))
				       usable-buttons)))
	 (width (apply 'max 0 (mapcar (lambda (glyph)
					(if glyph
					    (glyph-width glyph)
					  0))
				      usable-buttons))))
    (list (if (symbolp image) image glyph-list) height width)))



(defun toolbarx-xemacs-button-properties (button)
  "Return a list of properties of BUTTON.
The result is either nil (if not to be inserted) or a list in the format
 (TOOLBAR HEIGHT WIDTH BUTTON-DESCRIPTION)
where

TOOLBAR is one of the symbols `default', `top', `right', `bottom'
  or `left'.

HEIGHT and WIDTH are the maximal dimentions of all the glyphs
  involved.

BUTTON-DESCRIPTION is button definition in XEmacs; see the
  documentation of variable `default-toolbar'."
  (let* ((filtered-props
	  (let* ((filtered-props-temp)
		 (prop-good-val)
		 (prop))
	    (dolist (p (nth 0 toolbarx-button-props) filtered-props-temp)
	      ;;    property	       -> (car p)
	      ;;    test type function -> (cadr p)
	      ;;    add-function       -> (cddr p)
	      (setq prop (memq (car p) button))
	      ;; if so, check if value is of correct type
	      (when prop
		;; if property is of add-type, them the value is a list
		;; (:add-value-list VAL VAL). Each VAL should be checked.
		(if (and (cddr p) (eq :add-value-list (car (cadr prop))))
		    (let* ((add-list (list (cddr p))))
		      (dolist (val (cdr (cadr prop)))
			(setq prop-good-val (funcall (cadr p) val))
			(when (car prop-good-val)
			  (setq add-list (cons (cdr prop-good-val) add-list))))
		      (setq add-list (nreverse add-list))
		      (when (eq 2 (length add-list)) ; just 1 value, no
						     ; add-function
			(setq add-list (cadr add-list)))
		      (setq filtered-props-temp (append
						 (list (car p) add-list)
						 filtered-props-temp)))
		  ;; if override-property
		  (setq prop-good-val (funcall (cadr p) (cadr prop)))
		  (when (car prop-good-val)
		    (setq filtered-props-temp (append
					       (list (car p)
						     (cdr prop-good-val))
					       filtered-props-temp))))))))
	 (insert (or (not (memq :insert filtered-props))
		     ;; (memq :insert filtered-props) holds
		     (eval (nth 1 (memq :insert filtered-props))))))
    (when insert
      (let* ((image-props (toolbarx-xemacs-image-properties
			   (cadr (memq :image filtered-props))))
	     (glyph-list (car image-props))
	     (image-height (nth 1 image-props))
	     (image-width (nth 2 image-props))
	     (command
	      (let* ((com (nth 1 (memq :command filtered-props)))
		     (app (nth 1 (memq :append-command filtered-props)))
		     (prep (nth 1 (memq :prepend-command filtered-props))))
		(when (or com app prep)
		  (toolbarx-make-command com prep app))))
	     ;; enable defaults to `t'
	     (enable (if (memq :enable filtered-props)
			 (cadr (memq :enable filtered-props))
		       t))
	    ;; help defaults to nil
	     (help (when (memq :help filtered-props)
		     (cadr (memq :help filtered-props))))
	     ;; toolbar defaults to `default'
	     (toolbar-prop (cons (memq :toolbar filtered-props)
				 (cadr (memq :toolbar filtered-props))))
	     (toolbar (if (car toolbar-prop)
			  (if (symbolp (cdr toolbar-prop))
			      (cdr toolbar-prop)
			    ;; (cdr toolbar-prop) is cons cell
			    (if (eq (cadr toolbar-prop)
					  (default-toolbar-position))
				      (cddr toolbar-prop)
				   (cadr toolbar-prop)))
			'default)))
	(when glyph-list
	  (list toolbar image-height image-width
		(vector glyph-list command enable help)))))))

(defun toolbarx-xemacs-refresh-process-button-or-insert-list (switches
							      toolbar-props)
  "Process SWITCHES, returning an updated version of TOOLBAR-PROPS.
TOOLBAR-PROPS should be a list with 12 elements, each one representing
properties (in this order) `locale', `default', `top', `right',
`bottom', `left', `default-height', `default-width', `top-height',
`right-width', `bottom-height' and `left-width'.  The return is a list
with the same properties updated.

NB: Buttons (vectors) are inserted in front of the lists
represented by `default', `top', `right', `bottom' and `left', so
the lists are built reversed."
  (let ((locale		 (nth 0	 toolbar-props))
	(default	 (nth 1	 toolbar-props))
	(top		 (nth 2	 toolbar-props))
	(right		 (nth 3	 toolbar-props))
	(bottom		 (nth 4	 toolbar-props))
	(left		 (nth 5	 toolbar-props))
	(default-height	 (nth 6	 toolbar-props))
	(default-width	 (nth 7	 toolbar-props))
	(top-height	 (nth 8	 toolbar-props))
	(right-width	 (nth 9	 toolbar-props))
	(bottom-height	 (nth 10 toolbar-props))
	(left-width	 (nth 11 toolbar-props))
	(toolbar-props-temp))
    (dolist (button switches)
      (if (eq (car button) :insert)
	  (when (eval (cadr button))
	    ;; if insert group, process `cddr'
	    (progn
	      (setq toolbar-props-temp
		    (toolbarx-xemacs-refresh-process-button-or-insert-list
		     (cddr button)
		     (list locale default top right bottom left
			   default-height default-width top-height
			   right-width bottom-height left-width)))
	      (setq default	   (nth 1  toolbar-props-temp))
	      (setq top		   (nth 2  toolbar-props-temp))
	      (setq right	   (nth 3  toolbar-props-temp))
	      (setq bottom	   (nth 4  toolbar-props-temp))
	      (setq left	   (nth 5  toolbar-props-temp))
	      (setq default-height (nth 6  toolbar-props-temp))
	      (setq default-width  (nth 7  toolbar-props-temp))
	      (setq top-height	   (nth 8  toolbar-props-temp))
	      (setq right-width	   (nth 9  toolbar-props-temp))
	      (setq bottom-height  (nth 10 toolbar-props-temp))
	      (setq left-width	   (nth 11 toolbar-props-temp))))
	;; else, if normal button
	(let* ((button-props (toolbarx-xemacs-button-properties button))
	       (toolbar (nth 0 button-props))
	       (height (nth 1 button-props))
	       (width (nth 2 button-props))
	       (button-description (nth 3 button-props)))
	  (when button-props
	    (cond
	     ;; default
	     ((eq toolbar 'default)
	      (setq default (cons button-description default))
	      (setq default-height (max default-height height))
	      (setq default-width (max default-width width)))
	     ;; top
	     ((eq toolbar 'top)
	      (setq top (cons button-description top))
	      (setq top-height (max top-height height)))
	     ;; right
	     ((eq toolbar 'right)
	      (setq right (cons button-description right))
	      (setq right-width (max right-width width)))
	     ;; bottom
	     ((eq toolbar 'bottom)
	      (setq bottom (cons button-description bottom))
	      (setq bottom-height (max bottom-height height)))
	     ;; left
	     ((eq toolbar 'left)
	      (setq left (cons button-description left))
	      (setq left-width (max left-width width))))))))
    ;; return a list similar to toolbar-props
    (list locale default top right bottom left default-height
	  default-width top-height right-width bottom-height left-width)))


(defun toolbarx-xemacs-refresh (&optional global-flag)
  "Refresh the toolbar in XEmacs."
  (let* ((switches (if global-flag
		       (if (default-boundp 'toolbarx-internal-button-switches)
			   (default-value 'toolbarx-internal-button-switches)
			 toolbarx-internal-button-switches)
		     toolbarx-internal-button-switches))
	 (locale  (if global-flag 'global (current-buffer)))
	 (toolbar-init (list locale	; locale
			     nil	; default
			     nil	; top
			     nil	; right
			     nil	; bottom
			     nil	; left
			     0		; default-height
			     0		; default-width
			     0		; top-height
			     0		; right-width
			     0		; bottom-height
			     0))	; left-width
	 (toolbar-props
	  (toolbarx-xemacs-refresh-process-button-or-insert-list switches
								 toolbar-init))
	 ;; NB: Buttons (vectors) are inserted in front of the lists
	 ;; represented by `default', `top', `right', `bottom' and
	 ;; `left', so the lists are built reversed.
	 (default	  (nreverse (nth 1  toolbar-props)))
	 (top		  (nreverse (nth 2  toolbar-props)))
	 (right		  (nreverse (nth 3  toolbar-props)))
	 (bottom	  (nreverse (nth 4  toolbar-props)))
	 (left		  (nreverse (nth 5  toolbar-props)))
	 (default-height  (nth 6  toolbar-props))
	 (default-width	  (nth 7  toolbar-props))
	 (top-height	  (nth 8  toolbar-props))
	 (right-width	  (nth 9  toolbar-props))
	 (bottom-height	  (nth 10 toolbar-props))
	 (left-width	  (nth 11 toolbar-props))
	 (button-raised-border 2)
	 (default-border (specifier-instance default-toolbar-border-width))
	 (top-border (specifier-instance top-toolbar-border-width))
	 (right-border (specifier-instance right-toolbar-border-width))
	 (bottom-border (specifier-instance bottom-toolbar-border-width))
	 (left-border (specifier-instance left-toolbar-border-width)))
    ;; adding borders
    (when default
      (setq default-height (+ (* 2 button-raised-border)
			      (* 2 default-border)
			      default-height))
      (setq default-width (+ (* 2 button-raised-border)
			     (* 2 default-border)
			     default-width)))
    (when top
      (setq top-height (+ (* 2 button-raised-border)
			  (* 2 top-border)
			  top-height)))
    (when right
      (setq right-width (+ (* 2 button-raised-border)
			   (* 2 right-border)
			   right-width)))
    (when bottom
      (setq bottom-height (+ (* 2 button-raised-border)
			     (* 2 bottom-border)
			     bottom-height)))
    (when left
      (setq left-width (+ (* 2 button-raised-border)
			  (* 2 left-border)
			  left-width)))
    ;; deal with specifiers
    ;; - remove all specifiers for toolbars witout buttons
    (if default
	(progn
	  ;; Only activate the tool bar if it is already visible.
	  (when toolbar-visible-p
	    (set-specifier default-toolbar-visible-p (not (not default)) locale)
	    (if (memq (default-toolbar-position) '(top bottom))
		(set-specifier default-toolbar-height default-height locale)
	      (set-specifier default-toolbar-width default-width locale)))
	  (set-specifier default-toolbar default locale))
      (remove-specifier default-toolbar locale)
      (remove-specifier default-toolbar-visible-p locale)
      (remove-specifier default-toolbar-height locale)
      (remove-specifier default-toolbar-width locale))
    (if top
	(progn
	  (set-specifier top-toolbar-visible-p (not (not top)) locale)
	  (set-specifier top-toolbar-height top-height locale)
	  (set-specifier top-toolbar top locale))
      (remove-specifier top-toolbar locale)
      (remove-specifier top-toolbar-visible-p locale)
      (remove-specifier top-toolbar-height locale))
    (if right
	(progn
	  (set-specifier right-toolbar-visible-p (not (not right))
			 locale)
	  (set-specifier right-toolbar-width right-width locale)
	  (set-specifier right-toolbar right locale))
      (remove-specifier right-toolbar locale)
      (remove-specifier right-toolbar-visible-p locale)
      (remove-specifier right-toolbar-width locale))
    (if bottom
	(progn
	  (set-specifier bottom-toolbar-visible-p (not (not bottom)) locale)
	  (set-specifier bottom-toolbar-height bottom-height locale)
	  (set-specifier bottom-toolbar bottom locale))
      (remove-specifier bottom-toolbar locale)
      (remove-specifier bottom-toolbar-visible-p locale)
      (remove-specifier bottom-toolbar-height locale))
    (if left
	(progn
	  (set-specifier left-toolbar-visible-p (not (not left)) locale)
	  (set-specifier left-toolbar-width left-width locale)
	  (set-specifier left-toolbar left locale))
      (remove-specifier left-toolbar locale)
      (remove-specifier left-toolbar-visible-p locale)
      (remove-specifier left-toolbar-width locale))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; finishing parsing engine

(defun toolbarx-refresh (&optional global-flag)
  "Redraw the toolbar, peviously installed with `toolbarx'.
Force global refresh if GLOBAL-FLAG is non-nil."
  (interactive "P")
  (if (featurep 'xemacs)
      (toolbarx-xemacs-refresh global-flag)
    (toolbarx-emacs-refresh global-flag)))

;;;###autoload (autoload 'toolbarx-install-toolbar "toolbar-x")

(defun toolbarx-install-toolbar (buttons &optional meaning-alist global-flag)
  "Install toolbar buttons given in BUTTONS.
Button properties are optionally given in MEANING-ALIST.  If
GLOBAL-FLAG is non-nil, toolbar is installed globally (on every
buffer that does not have a toolbar set locally).  BUTTONS is a
list of format
  (ELEM ... ELEM . PROPS),
where each ELEM is either

 - a list in the same format od BUTTONS, which is going to be
   refered as a *group*; groups are used to distribute properties
   recursively to its elements; there are groups with special
   format for special purpose: *dropdown groups* and also *eval
   groups*.

 - a symbol, which could be associated in MEANING-ALIST with a
   list of button properties (symbol + properties = a *button*)
   or associated to a special kind of group (an *alias group*).

 - a vector, which elements are on the previous formats (but not
   another vector); this is useful to specify different
   ingredients to the toolbar depending if editor is Emacs or
   XEmacs; the first element will be used in Emacs; the second
   element is going to be used in XEmacs.

Meaning alist
=============

MEANING-ALIST is a list where each element is in one of the
formats (SYMB . BUTTON-PROPS-LIST) or (SYMB .  ALIAS-GROUP).
BUTTON-PROPS-LIST is a list in one of the formats
  (IMAGE COMMAND PROP VAL PROP VAL ... PROP VAL)  or
  (PROP VAL PROP VAL ... PROP VAL).
The IMAGE is going to be used as the `:image' property of the
button (see button properties bellow), and COMMAND shall be used
as the `:command' property of the button.  Each PROP is one of
the button properties, and VAL is its respective value.
ALIAS-GROUP is a list which first element is the symbol `:alias'
and the cdr shall be processed as a group.

However, a symbol is not required to have an association in
MEANING-ALIST, which is only a way to specify properties to a
button.	 One can use groups to specify properties.  Nil is a good
MEANING-ALIST.

Buttons
=======

A toolbar button in `toolbarx' is the set with a symbol and
properties used to display the button, like a image and a command
to call when the button is pressed (which are the minimal
elements that a button should have.)  The supported properties
for buttons and their `basic types' (see note on how values of
properties are obtained!) are:

 :image -- in Emacs, either a string or image descriptor (see
   info for a definition), or a variable bound to a image
   descriptor (like those defined with `defimage') or a list of 4
   strings or image descriptors; in XEmacs, either a string or a
   glyph, or a symbol bount to a glyph, or a list of at least 1
   and at most 6 strings or glyphs or nil (not the first element
   though); defines the image file displayed by the button.  If
   it is a string, the image file found with that name (always
   using the function `toolbarx-find-image' to make the
   \`internal\' image descriptor) is used as button image.  For
   the other formats, the button image is handled in the same way
   as it is treated by the editors; see info nodes bellow for a
   description of the capabilities of each editor
      Emacs: info file \"elisp\", node \"Tool Bar\" (see `:image'
             property);
             PS: a *vector* of four strings is used in the Emacs
             Lisp documentation as the `more ellaborated' image
             property format, but here we reserve vectors to
             provide editor-dependent values; this motivates our
             choice for a list instead of vector (however,
             internally the list becomes a vector when displaying
             the button).
     XEmacs: info file \"lispref\", node \"Toolbar Descriptor
             Format\" (see GLYPH-LIST) or the documentation of
             the variable `default-toolbar'; check the inheritage
             in case of a ommited glyph or nil instead of glyph.

 :command -- a form; if the form happens to be a command, it will
   be called with `call-interactively'.

 :append-command -- a form added to the end of the value of
   `:command'.

 :prepend-command -- a form added at the beginning of the value
   of `:command'.

 :help -- either a string or nil; defined the help string of the
   button;

 :enable -- a form, evaluated constantly by both editors to
   determine if a button is active (enabled) or not.

 :visible -- in Emacs, a form that is evaluated constantly to
   determine if a button is visible; in XEmacs, this property is
   ignored.

 :button -- in Emacs, a cons cell (TYPE .  SELECTED) where the
   TYPE should be `:toggle' or `:radio' and the cdr should be a
   form.  SELECTED is evaluated to determine when the button is
   selected.  This property is ignored in XEmacs.

 :insert -- a form that is evaluated every time that the toolbar
   is refresh (a call of `toolbarx-refresh') to determine if the
   button is inserted or just ignored (until next refresh).

 :toolbar -- in XEmacs, either one of the symbols `default',
   `top', `bottom', `left', `right', or a cons cell
   (POS . POS-AVOID-DEFAULT) where POS and POS-AVOID-DEFAULT
   should be one of the symbols `top', `bottom', `left', `right';
   if a symbol, the button will be inserted in one of these
   toolbars; if a cons cell, button will be inserted in toolbar
   POS unless the position of the default toolbar is POS (then,
   the default toolbar would override the position-specific
   toolbar), and in this case, button will be inserted in toolbar
   POS-AVOID-DEFAULT; in Emacs, this property is meaningless, and
   therefore ignored.  Hint of use of this property: in a
   program, use or everything with `default' and the cons format
   to avoid the default toolbar, or use only the position
   specific buttons (symbols that are not `default'), because of
   the `overriding' system in XEmacs, when a position-specific
   toolbar overrides the default toolbar; for instance, if you
   put a button in the default toolbar and another in the top
   toolbar (and the default toolbar is in the top), then *only*
   the ones in the top toolbar will be visible!

How to specify a button
=======================

One can specify a button by its symbol or by a group to specify
properties.  For example,
  BUTTON =
    ( foo
      (bar :image [\"bar-Emacs\" \"bar-XEmacs\"]
           :command bar-function :help \"Bar help string\")
      :insert foo-bar )
  MEANING-ALIST = ( (foo :image \"foo\" :command foo-function) )
specifiy two buttons `foo' and `bar', each one with its necessary
:image and :command properties, and both use the :insert property
specified ate the end of BUTTONS (because groups distribute
properties to all its elements).  `foo' and `bar' will be
inserted only if `foo-bar' evaluation yields non-nil.  `bar' used
a different :image property depending if editor is Emacs or
XEmacs.

Note on how values of properties are obtained
=============================================

For each property PROP, its value should be either:
   i) a vector of 2 elements; then each element should be of the
      basic type of PROP.
  ii) an element on the basic type of PROP.
 iii) a function (that does not need arguments); it is evaluated
      and the return should be ot type i) or ii) above
  iv) a symbol bound to a element of type i) or ii).

The type is cheched in the order i), ii) iii) and iv).	This
evaluations are done every time that the oolbar is refresh.

Ps.: in order to specify a vector as value of a property (like
the :image in Emacs), it is necessary to provide the vector as
element of another vector.

Special groups
==============

Eval groups
-----------

If the first element of a group is the symbol `:eval-group', each
element is evaluated (with `eval'), put inside a list and
processed like a group.	 Eval groups are useful to store
definition of buttons in a variable.

Dropdown groups
---------------

The idea is to specify a set of buttons that appear when a
determined menu item of a dropdown menu is active.  The dropdown
menu appears when a button (by default with a triangle pointing
down) is clicked.  This button is called `dropdown button'.  The
dropdown button appears on the left of the currently visible
buttons of the dropdown group.

A dropdown group is a list which first element is the symbol
`:dropdown-group' and in one of the following formats
  (:dropdown-group SYMBOL-1 ... SYMBOL-n  PROP-1 VAL-1 ... PROP-k VAL-k)
or
  (:dropdown-group
     STRING-1 ITEM-11 ... ITEM-1n
     STRING-2 ITEM-21 ... ITEM-2m
	   . . .
     STRING-n ITEM-n1 ... ITEM-np
       PROP-1 VAL-1 ... PROP-j VAL-j)
where
 SYMBOL-* is a symbol that defines a button in MEANING-ALIST;
 STRING-* is a string that will appear in the dropdown menu;
 ITEM-* is any format that define buttons or groups.

\(a dropdown group of first format is internally converted to the
second by making strings from the symbols and each symbol is the
item)

The same rules for obtaining property values, described above,
apply here.  Properties are also distributed by groups.	 The
supported properties and their basic type are:

 :type -- one of the symbols `radio' (default) or `toggle'; if
   type is radio, only one of the itens may be active, and if
   type is toggle, any item number of itens can be active.

 :variable -- a symbol; it is the variable that govern the
   dropdown button; every time the value should be an integer
   starting from 1 (if type is radio) or a list of integers (if
   type is toggle).  The Nth set of buttons is :insert'ed.

 :default -- determines the default value when the menu is
   installed; it is ignored if a value was saved with custom; it
   defaults to 1 if type is radio or nil if type is toggle.  If
   value is a integer and type is `toggle', value used is a list
   with that integer.

 :save -- one of the symbols nil (default), `offer' or
   `always'; determined if it is possible for the user to save
   the which menu itens are active, for a next session.	 If value
   is `offer', a item (offering to save) is added to the
   popup menu.	If the value is `always', every time that a item
   is selected, the variable is saved.	If value is nil, variable
   shall not be saved.	If value is non-nil then `:variable' is
   mandatory.

 :title -- a string or nil; if a string, the popup menu will show
   is as menu title; if nil, no title is shown.

 :dropdown-help -- a string or nil; the help string of the
   dropdown button.

 :dropdown-image -- in Emacs, either a string or a vector of 4
   strings; in XEmacs, either a string or a glyph or a list of at
   least 1 and at most 6 strings or glyphs; defines the image
   file displayed by the dropdown button; by default, it is the
   string \"dropdown\".

 :dropdown-append-command,
 :dropdownprepend-command -- a form; append or prepend forms to
   the command that shows the dropdown menu, allowing extra code
   to run before or after the menu appears (remember that every
   menu item clicked refresh the toolbar.)

 :dropdown-enable -- a form; evaluated constantly by both editors
   to determine if the dropdown button is active (enabled) or
   not.

 :dropdown-visible -- a form; in Emacs, it is evaluated
   constantly to determine if the dropdown button is visible; in
   XEmacs, this property is ignored.

 :dropdown-toolbar -- in XEmacs, one of the symbols `default',
   `opposite', `top', `bottom', `left' or `right'; ignored in
   Emacs; in XEmacs, the toolbar where the dropdown button will
   appear.

Also, if the symbol `dropdown' is associted in MEANING-ALIST
with some properties, these properties override (or add) with
higher precedence.

Special buttons
===============

If the symbol of a button is `:new-line', it is inserted
a (faked) return, and the next button will be displayed a next
line of buttons.  The only property supported for this button is
`:insert'.  This feature is available only in Emacs.  In XEmacs,
this button is ignored."
  (let ((switches (toolbarx-process-group buttons meaning-alist nil nil)))
    (if global-flag
	(setq-default toolbarx-internal-button-switches
		      switches)
      (set (make-local-variable 'toolbarx-internal-button-switches)
	   switches)
      (unless (featurep 'xemacs)
	(make-local-variable 'tool-bar-map))))
  (toolbarx-refresh global-flag))


(defconst toolbarx-default-toolbar-meaning-alist
  `((separator :image "sep" :command t :enable nil :help "")

    (,(if (and (not (featurep 'xemacs)) (>= emacs-major-version 22))
	  'new-file
	'open-file)
     :image ["new" toolbar-file-icon]
     :command [find-file toolbar-open]
     :enable [(not (window-minibuffer-p
		    (frame-selected-window menu-updating-frame)))
	      t]
     :help ["Specify a new file's name, to edit the file" "Visit new file"])

    ,(when (and (not (featurep 'xemacs)) (>= emacs-major-version 22))
       '(open-file :image ["open" toolbar-file-icon]
		   :command [menu-find-file-existing toolbar-open]
		   :enable [(not (window-minibuffer-p
				  (frame-selected-window menu-updating-frame)))
			    t]
		   :help ["Read a file into an Emacs buffer" "Open a file"]))

    (dired :image [,(if (>= emacs-major-version 22)
			"diropen"
		      "open")
		   toolbar-folder-icon]
	   :command [dired toolbar-dired]
	   :help ["Read a directory, operate on its files" "Edit a directory"])

    (save-buffer :image ["save" toolbar-disk-icon]
		 :command [save-buffer toolbar-save]
		 :enable [(and
			   (buffer-modified-p)
			   (buffer-file-name)
			   (not (window-minibuffer-p
				 (frame-selected-window menu-updating-frame))))
			  t]
		 :help ["Save current buffer to its file"  "Save buffer"]
		 :visible (or buffer-file-name
			      (not (eq 'special
				       (get major-mode 'mode-class)))))

    ;; Emacs only
    (write-file :image "saveas"
		:command write-file
		:enable (not
			 (window-minibuffer-p
			  (frame-selected-window menu-updating-frame)))
		:insert [t nil]
		:help "Write current buffer to another file"
		:visible (or buffer-file-name
			     (not (eq 'special (get major-mode 'mode-class)))))

    (undo :image ["undo" toolbar-undo-icon]
	  :command [undo toolbar-undo]
	  :enable [(and (not buffer-read-only)
			(not (eq t buffer-undo-list))
			(if (eq last-command 'undo)
			    pending-undo-list
			  (consp buffer-undo-list)))
		   t]
	  :help ["Undo last operation" "Undo edit"]
	  :visible (not (eq 'special (get major-mode 'mode-class))))

    (cut :image ["cut" toolbar-cut-icon]
	 :help ["Delete text in region and copy it to the clipboard"
		"Kill region"]
	 :command [clipboard-kill-region toolbar-cut]
	 :visible (not (eq 'special (get major-mode 'mode-class))))

    (copy :image ["copy" toolbar-copy-icon]
	  :help ["Copy text in region to the clipboard" "Copy region"]
	  :command [clipboard-kill-ring-save toolbar-copy])

    (paste :image ["paste" toolbar-paste-icon]
	   :help ["Paste text from clipboard" "Paste from clipboard"]
	   :command [clipboard-yank toolbar-paste]
	   :visible (not (eq 'special (get major-mode 'mode-class))))

    ;; Emacs only
    (search-forward :command nonincremental-search-forward
		    :help "Search forward for a string"
		    :image "search"
		    :insert [t nil])

    (search-replace
     :image ["search-replace" toolbar-replace-icon]
     :command [query-replace toolbar-replace]
     :help ["Replace string interactively, ask about each occurrence"
	    "Search & Replace"])

    (print-buffer :image ["print" toolbar-printer-icon]
		  :command [print-buffer toolbar-print]
		  :help ["Print current buffer with page headings"
			 "Print buffer"])

    ;; Emacs only
    (customize :image "preferences"
	       :command customize
	       :help "Edit preferences (customize)"
	       :insert [t nil])

    ;; Emacs only
    (help :image "help"
	  :command (lambda () (interactive) (popup-menu menu-bar-help-menu))
	  :help "Pop up the Help menu"
	  :insert [t nil])

    ;; Emacs only
    (kill-buffer :command kill-this-buffer
		 :enable (kill-this-buffer-enabled-p)
		 :help "Discard current buffer"
		 :image "close"
		 :insert [t nil])

    ;; Emacs only
    (exit-emacs :image "exit"
		:command save-buffers-kill-emacs
		:help "Offer to save unsaved buffers, then exit Emacs"
		:insert [t nil])

    (spell-buffer :image ["spell" toolbar-spell-icon]
		  :command [ispell-buffer toolbar-ispell]
		  :help ["Check spelling of selected buffer" "Check spelling"])

    (info :image ["info" toolbar-info-icon]
	  :command [info toolbar-info]
	  :help ["Enter Info, the documentation browser" "Info documentation"])

    ;; XEmacs only
    (mail :image toolbar-mail-icon
	  :command toolbar-mail
	  :help "Read mail"
	  :insert [nil t])

    ;; XEmacs only
    (compile :image toolbar-compile-icon
	     :command toolbar-compile
	     :help "Start a compilation"
	     :insert [nil t])

    ;; XEmacs only
    (debug :image toolbar-debug-icon
	   :command toolbar-debug
	   :help "Start a debugger"
	   :insert [nil t])

    ;; XEmacs only
    (news :image toolbar-news-icon
	  :command toolbar-news
	  :help "Read news"
	  :insert [nil t]))
  "A meaning alist with definition of the default buttons.
The following buttons are available:

* Both Emacs and XEmacs: `open-file', `dired', `save-buffer',
  `undo', `cut', `copy', `paste', `search-replace', `print-buffer',
  `spell-buffer', `info'.

* Emacs only: `new-file' (Emacs 22+) `write-file', `search-forward',
  `customize', `help', `kill-buffer', `exit-emacs'.

* XEmacs only: `mail', `compile', `debug', `news'.

To reproduce the default toolbar in both editors with use as BUTTON
in `toolbarx-install-toolbar':

\(toolbarx-install-toolbar
 '([(open-file dired kill-buffer save-buffer write-file undo cut
               copy paste search-forward print-buffer customize help)
    (open-file dired save-buffer print-buffer cut copy paste undo
               spell-buffer search-replace mail info compile debug news)])
 toolbarx-default-toolbar-meaning-alist)

Ps.: there are more buttons available than suggested in the
expression above.")

(provide 'toolbar-x)

;;; toolbar-x.el ends here
