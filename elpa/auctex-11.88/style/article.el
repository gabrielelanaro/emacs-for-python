;;; article.el - Special code for article style.

;;; Code:

(defvar LaTeX-article-class-options
  '("a4paper" "a5paper" "b5paper" "letterpaper" "legalpaper" "executivepaper"
    "landscape" "10pt" "11pt" "12pt" "oneside" "twoside" "draft" "final"
    "titlepage" "notitlepage" "onecolumn" "twocolumn" "leqno" "fleqn" "openbib")
  "Package options for the article class.")

(TeX-add-style-hook
 "article"
 (lambda ()
   (LaTeX-largest-level-set "section")
   (LaTeX-add-counters "part" "section" "subsection" "subsubsection" "paragraph"
		       "subparagraph" "figure" "table")
   (LaTeX-add-pagestyles "headings" "myheadings"))
 LaTeX-dialect)

;;; article.el ends here
