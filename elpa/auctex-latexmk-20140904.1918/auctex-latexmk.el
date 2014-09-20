;;; auctex-latexmk.el --- Add LatexMk support to AUCTeX
;; Version: 20140904.1918

;; Copyright (C) 2013, 2014 by Tomoya Tanjo

;; Author: Tomoya Tanjo <ttanjo@gmail.com>
;; URL: https://github.com/tom-tan/auctex-latexmk/
;; Package-Requires: ((auctex "11.87"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library adds LatexMk support to AUCTeX.
;;
;; Requirements:
;;   * AUCTeX
;;   * LatexMk
;;   * TeXLive (2011 or later if you write TeX source in Japanese)
;;
;; To use this package, add the following line to your .emacs file:
;;     (require 'auctex-latexmk)
;;     (auctex-latexmk-setup)
;; And add the following line to your .latexmkrc file:
;;     # .latexmkrc starts
;;     $pdf_mode = 1;
;;     # .latexmkrc ends
;; After that, by using M-x TeX-command-master (or C-c C-c), you can use
;; LatexMk command to compile TeX source.
;;
;; For Japanese users:
;;
;; LatexMk command automatically stores the encoding of a source file
;; and passes it to latexmk via an environment variable named "LATEXENC".
;; Here is the example of .latexmkrc to use "LATEXENC":
;;     # .latexmkrc starts
;;     $kanji    = "-kanji=$ENV{\"LATEXENC\"}" if defined $ENV{"LATEXENC"};
;;     $latex    = "platex -interaction=nonstopmode $kanji";
;;     $bibtex   = 'pbibtex $kanji';
;;     $dvipdf   = 'perl -e "exec(\'dvipdfmx\', \$ARGV[0])"';
;;     $pdf_mode = 3;
;;     # .latexmkrc ends

;;; Code:

(require 'tex-buf)
(require 'latex)

(defgroup auctex-latexmk nil
  "Add LatexMk support to AUCTeX."
  :group 'AUCTeX
  :prefix "auctex-latexmk")

(defcustom auctex-latexmk-encoding-alist
  '((japanese-iso-8bit      . "euc")
    (japanese-iso-8bit-unix . "euc")
    (euc-jp                 . "euc")
    (euc-jp-unix            . "euc")
    (utf-8                  . "utf8")
    (utf-8-unix             . "utf8")
    (japanese-shift-jis     . "sjis")
    (japanese-shift-jis-dos . "sjis"))
  "Encoding mapping for platex."
  :group 'auctex-latexmk)

(defun TeX-run-latexmk (name command file)
  (let ((TeX-sentinel-default-function 'Latexmk-sentinel)
        (pair (assq buffer-file-coding-system auctex-latexmk-encoding-alist)))
    (unless (null pair)
      (setenv "LATEXENC" (cdr pair)))
    (TeX-run-TeX name command file)
    (setenv "LATEXENC" nil)))

;;;###autoload
(defun auctex-latexmk-setup ()
  "Add LatexMk command to TeX-command-list."
  (setq-default TeX-command-list
                (cons
                 '("LatexMk" "latexmk %S%(mode) %t" TeX-run-latexmk nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk")
                 TeX-command-list)
                LaTeX-clean-intermediate-suffixes
                (append LaTeX-clean-intermediate-suffixes
                        '("\\.fdb_latexmk" "\\.aux.bak" "\\.fls"))))

(defun Latexmk-sentinel (process name)
  (save-excursion
    (goto-char (point-max))
    (cond
      ((re-search-backward (format "^%s finished at" mode-name) nil t)
       (if (re-search-backward "^Run number [0-9]+ of rule '\\(pdf\\|lua\\|xe\\)?latex'" nil t)
           (progn
             (forward-line 5)
             (let ((beg (point)))
               (when (string= (current-word) "Latexmk")
                 ;; Special treatment for MiKTeX
                 (forward-line))
               (re-search-forward "^Latexmk:" nil t)
               (beginning-of-line)
               (save-restriction
                 (narrow-to-region beg (point))
                 (goto-char (point-min))
                 (TeX-LaTeX-sentinel process name))))
         (message (format "%s: nothing to do" name))))
      ((re-search-backward (format "^%s exited abnormally with code" mode-name) nil t)
       (re-search-backward "^Collected error summary (may duplicate other messages):" nil t)
       (re-search-forward "^  \\([^:]+\\):" nil t)
       (let ((com (TeX-match-buffer 1)))
         (cond
           ((string-match "^\\(pdf\\|lua\\|xe\\)?latex" com)
            (goto-char (point-min))
            (TeX-LaTeX-sentinel process name)
            (when (string= TeX-command-next TeX-command-BibTeX)
              (setq TeX-command-default)))
           ((string-match "^bibtex " com)
            (forward-line -1)
            (re-search-backward com nil t)
            (forward-line 5)
            (let ((beg (point)))
              (re-search-forward "^Rule" nil t)
              (beginning-of-line)
              (save-restriction
                (narrow-to-region beg (point))
                (TeX-BibTeX-sentinel process name))))))))))

(defadvice TeX-recenter-output-buffer (around recenter-for-latexmk activate)
  (setq ad-return-value
        (let ((buffer (TeX-active-buffer)))
          (if buffer
              (if (with-current-buffer buffer
                    (goto-char (point-max))
                    (re-search-backward "^latexmk" nil t))
                  (let ((old-buffer (current-buffer)))
                    (TeX-pop-to-buffer buffer t t)
                    (bury-buffer buffer)
                    (goto-char (point-max))
                    (re-search-backward "^Run number [0-9]+ of rule 'bibtex .+'"
                                        nil t)
                    (re-search-forward "^Rule" nil t)
                    (forward-line -1)
                    (recenter (if line
                                  (prefix-numeric-value line)
                                  (/ (window-height) 2)))
                    (TeX-pop-to-buffer old-buffer nil t))
                  ad-do-it)
              (message "No process for this document.")))))

(provide 'auctex-latexmk)
;;; auctex-latexmk.el ends here
