;;; tex-font.el --- Font-Lock support stolen from Emacs 21.
;;
;; Copyright (C) 1985, 86, 89, 92, 94, 95, 96, 97, 98, 1999
;;       Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex, faces

;; This file is part of AUC TeX.

;; AUC TeX is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUC TeX is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUC TeX; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Comments:

;; Please keep this file in sync with GNU Emacs 21.

;;; Code:

(defconst tex-font-lock-keywords-1
  (eval-when-compile
    (let* (;; Names of commands whose arg should be fontified as heading, etc.
	   (headings (regexp-opt
		      '("title"  "begin" "end" "chapter" "part"
			"section" "subsection" "subsubsection"
			"paragraph" "subparagraph" "subsubparagraph"
			"newcommand" "renewcommand" "newenvironment"
			"newtheorem")
		      t))
	   (variables (regexp-opt
		       '("newcounter" "newcounter*" "setcounter" "addtocounter"
			 "setlength" "addtolength" "settowidth")
		       t))
	   (includes (regexp-opt
		      '("input" "include" "includeonly" "bibliography"
			"epsfig" "psfig" "epsf" "nofiles" "usepackage"
			"documentstyle" "documentclass" "verbatiminput"
			"includegraphics" "includegraphics*")
		      t))
	   ;; Miscellany.
	   (slash "\\\\")
	   (opt " *\\(\\[[^]]*\\] *\\)*")
	   ;; This would allow highlighting \newcommand\CMD but requires
	   ;; adapting subgroup numbers below.
	   ;; (arg "\\(?:{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)\\|\\\\[a-z*]+\\)"))
	   (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)"))
      (list
       ;; Heading args.
       (list (concat slash headings "\\*?" opt arg)
	     ;; If ARG ends up matching too much (if the {} don't match, f.ex)
	     ;; jit-lock will do funny things: when updating the buffer
	     ;; the re-highlighting is only done locally so it will just
	     ;; match the local line, but defer-contextually will
	     ;; match more lines at a time, so ARG will end up matching
	     ;; a lot more, which might suddenly include a comment
	     ;; so you get things highlighted bold when you type them
	     ;; but they get turned back to normal a little while later
	     ;; because "there's already a face there".
	     ;; Using `keep' works around this un-intuitive behavior as well
	     ;; as improves the behavior in the very rare case where you do
	     ;; have a comment in ARG.
	     3 'font-lock-function-name-face 'keep)
       (list (concat slash "\\(re\\)?newcommand\\** *\\(\\\\[A-Za-z@]+\\)")
	     2 'font-lock-function-name-face 'keep)
       ;; Variable args.
       (list (concat slash variables " *" arg) 2 'font-lock-variable-name-face)
       ;; Include args.
       (list (concat slash includes opt arg) 3 'font-lock-builtin-face)
       ;; Definitions.  I think.
       '("^[ \t]*\\\\def *\\\\\\(\\(\\w\\|@\\)+\\)"
	 1 font-lock-function-name-face))))
  "Subdued expressions to highlight in TeX modes.")

(defconst tex-font-lock-keywords-2
  (append tex-font-lock-keywords-1
   (eval-when-compile
     (let* (;;
	    ;; Names of commands whose arg should be fontified with fonts.
	    (bold (regexp-opt '("textbf" "textsc" "textup"
				"boldsymbol" "pmb") t))
	    (italic (regexp-opt '("textit" "textsl" "emph") t))
	    (type (regexp-opt '("texttt" "textmd" "textrm" "textsf") t))
	    ;;
	    ;; Names of commands whose arg should be fontified as a citation.
	    (citations (regexp-opt
			'("label" "ref" "pageref" "vref" "eqref"
			  "cite" "nocite" "index" "glossary" "bibitem"
			  ;; These are text, rather than citations.
			  ;; "caption" "footnote" "footnotemark" "footnotetext"
			  )
			t))
	    ;;
	    ;; Names of commands that should be fontified.
	    (specials (regexp-opt
		       '("\\" "\\*" ;; "-"
			 "linebreak" "nolinebreak" "pagebreak" "nopagebreak"
			 "newline" "newpage" "clearpage" "cleardoublepage"
			 "displaybreak" "allowdisplaybreaks" "enlargethispage")
		       t))
	    (general "\\([a-zA-Z@]+\\**\\|[^ \t\n]\\)")
	    ;;
	    ;; Miscellany.
	    (slash "\\\\")
	    (opt " *\\(\\[[^]]*\\] *\\)*")
	    (arg "{\\(\\(?:[^{}\\]+\\|\\\\.\\|{[^}]*}\\)+\\)"))
       (list
	;;
	;; Citation args.
	(list (concat slash citations opt arg) 3 'font-lock-constant-face)
	;;
	;; Text between `` quotes ''.
	(cons (concat (regexp-opt `("``" "\"<" "\"`" "<<" "«") t)
		      "[^'\">»]+"	;a bit pessimistic
		      (regexp-opt `("''" "\">" "\"'" ">>" "»") t))
	      'font-lock-string-face)
	;;
	;; Command names, special and general.
	(cons (concat slash specials) 'font-lock-warning-face)
	(concat slash general)
	;;
	;; Font environments.  It seems a bit dubious to use `bold' etc. faces
	;; since we might not be able to display those fonts.
	(list (concat slash bold " *" arg) 2 '(quote bold) 'append)
	(list (concat slash italic " *" arg) 2 '(quote italic) 'append)
	;; (list (concat slash type arg) 2 '(quote bold-italic) 'append)
	;;
	;; Old-style bf/em/it/sl.  Stop at `\\' and un-escaped `&', for tables.
	(list (concat "\\\\\\(\\(bf\\)\\|em\\|it\\|sl\\)\\>"
		      "\\(\\([^}&\\]\\|\\\\[^\\]\\)+\\)")
	      3 '(if (match-beginning 2) 'bold 'italic) 'append)))))
   "Gaudy expressions to highlight in TeX modes.")

(defvar tex-font-lock-keywords tex-font-lock-keywords-1
  "Default expressions to highlight in TeX modes.")


(defface tex-math-face
  '((t :inherit font-lock-string-face))
  "Face used to highlight TeX math expressions.")
(defvar tex-math-face 'tex-math-face)

;; Use string syntax but math face for $...$.
(defun tex-font-lock-syntactic-face-function (state)
  (if (nth 3 state) tex-math-face font-lock-comment-face))

;;;###autoload
(defun tex-font-setup ()
  "Setup font lock support for TeX."
  (set (make-local-variable 'font-lock-defaults)
     '((tex-font-lock-keywords
	tex-font-lock-keywords-1 tex-font-lock-keywords-2)
       nil nil ((?$ . "\"")) nil
       ;; Who ever uses that anyway ???
       (font-lock-mark-block-function . mark-paragraph)
       (font-lock-syntactic-face-function
	. tex-font-lock-syntactic-face-function)))
    )

(provide 'tex-font)

;;; tex-font.el ends here