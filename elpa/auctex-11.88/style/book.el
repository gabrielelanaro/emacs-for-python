;;; book.el - Special code for book style.

;;; Code:

(defvar LaTeX-book-class-options
  '("a4paper" "a5paper" "b5paper" "letterpaper" "legalpaper" "executivepaper"
    "landscape" "10pt" "11pt" "12pt" "oneside" "twoside" "draft" "final"
    "titlepage" "notitlepage" "openright" "openany" "onecolumn" "twocolumn"
    "leqno" "fleqn" "openbib")
  "Package options for the book class.")

(TeX-add-style-hook
 "book"
 (lambda () 
   (LaTeX-largest-level-set "chapter")
   (LaTeX-add-counters "part" "chapter" "section" "subsection" "subsubsection"
		       "paragraph" "subparagraph" "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings"))
 LaTeX-dialect)

;;; book.el ends here
