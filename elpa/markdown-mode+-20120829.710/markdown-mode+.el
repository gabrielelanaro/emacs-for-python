;;; markdown-mode+.el --- extra functions for markdown-mode

;; Copyright (c) 2011 Donald Ephraim Curtis <dcurtis@milkbox.net>

;; Author: Donald Ephraim Curtis
;; URL: http://github.com/milkypostman/markdown-mode+.el
;; Version: 0.8
;; Keywords: markdown, latex, osx, rtf
;; Package-Requires: ((markdown-mode "20111229"))

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defcustom markdown-rtf-command "pandoc -s -t rtf"
  "Command to generate RTF from Markdown"
  :group 'markdown
  :type 'string)

(defcustom markdown-copy-command "pbcopy"
  "Command to copy directory to the clipboard and interpret MIME type."
  :group 'markdown
  :type 'string)

(defcustom markdown-latex-command "pandoc -s --mathjax -t latex"
  "Command to output LaTeX from Markdown."
  :group 'markdown
  :type 'string)

(defcustom markdown-pandoc-pdf-command "pandoc -s --mathjax"
  "Command to output LaTeX from Markdown."
  :group 'markdown
  :type 'string)

;;;###autoload
(defun markdown-export-latex ()
  "Output the Markdown file as LaTeX."
  (interactive)
  (let ((output-file (markdown-export-file-name ".tex")))
    (when output-file
      (let ((output-buffer-name (buffer-name
                                 (find-file-noselect output-file nil t)))
            (markdown-command markdown-latex-command))
        (markdown output-buffer-name)
        (with-current-buffer output-buffer-name
          (save-buffer)
          (kill-buffer output-buffer-name))
        output-file))))


;;;###autoload
(defun markdown-export-pdf ()
  "Output the Markdown file as LaTeX."
  (interactive)
  (save-window-excursion
    (markdown-export-latex)
    (let ((output-buffer-name (concat "*" (markdown-export-file-name "") "*")))
      (shell-command (concat "pdflatex" " --synctex=1 -interaction=nonstopmode "
                             (shell-quote-argument
                              (markdown-export-file-name ".tex")))
                     output-buffer-name))))


;;;###autoload
(defun markdown-export-pandoc-pdf ()
  "Output the Markdown file as LaTeX."
  (interactive)
  (let ((output-file (markdown-export-file-name ".pdf")))
    (when output-file
      (let ((output-buffer-name (buffer-name
                                 (find-file-noselect output-file nil t)))
            (markdown-command (concat markdown-pandoc-pdf-command
                                      " -o " output-file)))
        (markdown output-buffer-name)
        output-file))))


;;;###autoload
(defun markdown-code-copy (begin end)
  "Copy region from BEGIN to END to the clipboard with four spaces indenteded on each line.

Taken from
http://stackoverflow.com/questions/3519244/emacs-command-to-indent-code-by-4-spaces-to-format-for-paste-into-stackoverflow."
  (interactive "r")
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer begin end)
      (indent-rigidly (point-min) (point-max) 4)
      (clipboard-kill-ring-save (point-min) (point-max)))))

;;;###autoload
(defun markdown-copy-rtf ()
  "Render markdown and copy as RTF."
  (interactive)
  (save-window-excursion
    (let ((markdown-command markdown-rtf-command))
      (markdown)
      (with-current-buffer markdown-output-buffer-name
        (shell-command-on-region
         (point-min)
         (point-max)
         markdown-copy-command)))))

;;;###autoload
(defun markdown-copy-paste-html ()
  "Process file with multimarkdown, copy it to the clipboard, and paste in safari's selected textarea."
  (interactive)
  (markdown-copy-html)
  (do-applescript
   (concat
    (let ((metafn (concat (buffer-file-name) ".meta")))
      (cond
       ((and (buffer-file-name) (file-exists-p metafn))
        (save-buffer)
        (with-temp-buffer
          (insert-file-contents-literally metafn)
          (goto-char (point-min))
          (do-applescript
           (concat
            "tell application \""
            (buffer-substring-no-properties (point-at-bol) (point-at-eol))
            "\" to activate"))))
       (t
        "
tell application \"System Events\" to keystroke tab using {command down}
delay 0.2"
        )))
    "
tell application \"System Events\" to keystroke \"a\" using {command down}
tell application \"System Events\" to keystroke \"v\" using {command down}")))


(provide 'markdown-mode+)

;;; markdown-mode+.el ends here
