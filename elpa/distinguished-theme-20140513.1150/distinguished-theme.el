;;; distinguished-theme.el --- A dark and elegant theme for emacs.

;; Copyright © 2014 Kim Silkebækken

;; Author: Kim Silkebækken <kim.silkebaekken@gmail.com>
;; URL: https://github.com/Lokaltog/distinguished-theme
;; Version: 20140513.1150
;; X-Original-Version: 0.0.1

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; A modified port of the distinguished theme for vim.

;;; Code:
(deftheme distinguished "Distinguished color theme")

(let ((dst-fg          (if (display-graphic-p) "#ffffff" "color-231"))
      (dst-bg          (if (display-graphic-p) "#000000" "color-232"))
      (dst-bg+0        (if (display-graphic-p) "#151311" "color-234"))
      (dst-bg+1        (if (display-graphic-p) "#252321" "color-236"))
      (dst-bg+2        (if (display-graphic-p) "#474544" "color-238"))
      (dst-gray-2      (if (display-graphic-p) "#58574e" "color-240"))
      (dst-gray-1      (if (display-graphic-p) "#6c6a60" "color-242"))
      (dst-gray        (if (display-graphic-p) "#969385" "color-243"))
      (dst-gray+1      (if (display-graphic-p) "#b4b1a2" "color-250"))
      (dst-gray+2      (if (display-graphic-p) "#d0cbc0" "color-252"))
      (dst-steel       (if (display-graphic-p) "#8a9496" "color-245"))
      (dst-steel+1     (if (display-graphic-p) "#acb0b3" "color-247"))
      (dst-steel+2     (if (display-graphic-p) "#c0c7ca" "color-251"))
      (dst-blue        (if (display-graphic-p) "#67809c" "color-173"))
      (dst-blue+1      (if (display-graphic-p) "#b2c3cc" "color-66" ))
      (dst-blue+2      (if (display-graphic-p) "#d9e2ff" "color-69" ))
      (dst-green-2     (if (display-graphic-p) "#646d14" "color-22" ))
      (dst-green-1     (if (display-graphic-p) "#869038" "color-28" ))
      (dst-green       (if (display-graphic-p) "#a4ac64" "color-143"))
      (dst-green+1     (if (display-graphic-p) "#ccc768" "color-67" ))
      (dst-red-3       (if (display-graphic-p) "#3f1c0f" "color-124"))
      (dst-red-2       (if (display-graphic-p) "#7c2a09" "color-196"))
      (dst-red-1       (if (display-graphic-p) "#a7502d" "color-160"))
      (dst-red         (if (display-graphic-p) "#d47c59" "color-179"))
      (dst-red+1       (if (display-graphic-p) "#edb08f" "color-173"))
      (dst-red+2       (if (display-graphic-p) "#edbca2" "color-208"))
      (dst-yellow-2    (if (display-graphic-p) "#875f00" "color-94"))
      (dst-yellow-1    (if (display-graphic-p) "#ffd700" "color-220"))
      (dst-yellow      (if (display-graphic-p) "#d7af5f" "color-226"))
      (dst-yellow+1    (if (display-graphic-p) "#ffd75f" "color-137"))
      (dst-yellow+2    (if (display-graphic-p) "#f9ee98" "color-228"))
      (dst-intense-red (if (display-graphic-p) "#ff2a00" "color-202")))
  (custom-theme-set-faces
   'distinguished
   `(default ((t (:foreground ,dst-fg :background ,dst-bg))))
   `(cursor ((t (:foreground ,dst-bg :background ,dst-fg))))
   `(hl-line ((t (:background ,dst-bg+0))))
   `(minibuffer-prompt ((t (:foreground ,dst-green :weight bold))))
   `(region ((t (:background ,dst-bg+2))))
   `(secondary-selection ((t (:inherit hl-line))))
   `(fringe ((t (:foreground ,dst-gray :background ,dst-bg+0))))
   `(vertical-border ((t (:foreground ,dst-bg+2))))
   `(mode-line ((t (:foreground ,dst-gray+2 :background ,dst-bg+2 :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((t (:foreground ,dst-green :weight bold))))
   `(mode-line-inactive ((t (:foreground ,dst-gray :background ,dst-bg+1 :box (:line-width -1 :style released-button)))))

   ;; font lock
   `(font-lock-builtin-face ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,dst-gray))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,dst-bg+2))))
   `(font-lock-doc-face ((t (:foreground ,dst-gray))))
   `(font-lock-constant-face ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,dst-red :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,dst-blue :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,dst-steel+1 :weight bold :slant italic))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,dst-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,dst-red :weight bold))))
   `(font-lock-string-face ((t (:foreground ,dst-green))))
   `(font-lock-type-face ((t (:foreground ,dst-green+1 :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,dst-blue+1 :weight normal :slant italic))))
   `(font-lock-warning-face ((t (:foreground ,dst-intense-red :weight bold))))

   ;; basic whitespace-mode (tabs/newlines)
   `(whitespace-tab ((t (:foreground ,dst-bg+1 :background nil :weight normal))))
   `(whitespace-newline ((t (:foreground ,dst-red-3 :background nil :weight normal))))

   ;; show parens
   `(show-paren-mismatch ((t (:foreground ,dst-fg :background ,dst-red-1 :weight bold))))
   `(show-paren-match ((t (:foreground ,dst-fg :background ,dst-green-2 :weight bold))))

   ;; search highlight
   `(isearch ((t (:foreground ,dst-fg :background ,dst-green-2 :weight bold :slant normal :box (:line-width -1 :style released-button)))))
   `(isearch-fail ((t (:foreground ,dst-fg :background ,dst-red-1 :weight bold :slant normal :box (:line-width -1 :style released-button)))))
   `(lazy-highlight ((t (:foreground ,dst-bg :background ,dst-yellow+1 :weight bold :slant normal :box (:line-width -1 :style released-button)))))

   ;; rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dst-yellow+2))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dst-green))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dst-red+1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dst-blue+1))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dst-yellow+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dst-green))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dst-red+1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dst-blue+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dst-yellow+2))))

   ;; git-gutter
   `(git-gutter:added ((t (:foreground ,dst-green-1 :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,dst-red-1 :weight bold))))
   `(git-gutter:modified ((t (:foreground ,dst-blue :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,dst-fg :weight bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,dst-green-1  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,dst-red-1 :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,dst-blue :weight bold))))

   ;; flx
   `(flx-highlight-face ((t (:foreground ,dst-red :weight bold :underline ,dst-red-2))))

   ;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dst-red-1) :inherit unspecified))
      (t (:foreground ,dst-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dst-yellow) :inherit unspecified))
      (t (:foreground ,dst-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dst-blue) :inherit unspecified))
      (t (:foreground ,dst-blue :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,dst-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,dst-yellow+1 :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,dst-blue :weight bold))))

   ;; ace-jump
   `(ace-jump-face-background ((t (:foreground ,dst-gray-2 :background nil :weight normal :inverse-video nil))))
   `(ace-jump-face-foreground ((t (:foreground ,dst-bg :background ,dst-intense-red :weight bold :slant normal :inverse-video nil :box (:line-width -1 :style released-button)))))

   ;; auto-complete
   `(ac-candidate-face ((t (:foreground ,dst-gray :background ,dst-bg+1))))
   `(ac-completion-face ((t (:foreground ,dst-gray-1 :background nil :weight normal :slant normal :underline ,dst-bg+1))))
   `(ac-selection-face ((t (:foreground ,dst-fg :background ,dst-blue :weight bold))))
   `(popup-tip-face ((t (:foreground ,dst-bg :background ,dst-yellow))))
   `(popup-scroll-bar-foreground-face ((t (:background ,dst-gray+2))))
   `(popup-scroll-bar-background-face ((t (:background ,dst-bg+2))))
   `(popup-isearch-match ((t (:foreground ,dst-fg :background ,dst-bg))))

   ;; custom stuff from Lokaltog/emacsfiles
   `(font-lock-number-face ((t (:foreground ,dst-red :weight bold))))
   `(font-lock-pointer-face ((t (:foreground ,dst-yellow+1))))

   ;; diff
   `(diff-added ((t (:foreground ,dst-green :background ,dst-green-2))))
   `(diff-changed ((t (:foreground ,dst-yellow-1 :background ,dst-yellow-2))))
   `(diff-file-header ((t (:background ,dst-green+1 :weight bold))))
   `(diff-function ((t (:background ,dst-gray-1))))
   `(diff-header ((t (:background ,dst-gray-1))))
   `(diff-hunk-header ((t (:background ,dst-gray-1))))
   `(diff-index ((t (:background ,dst-blue+1))))
   `(diff-indicator-added ((t (:foreground ,dst-green-1 :weight bold))))
   `(diff-indicator-changed ((t (:foreground ,dst-yellow-1 :weight bold))))
   `(diff-indicator-removed ((t (:foreground ,dst-red-1 :weight bold))))
   `(diff-refine-added ((t (:foreground ,dst-green :background
                                        ,dst-green-1 :weight bold))))
   `(diff-refine-change ((t (:foreground ,dst-yellow-1 :background
                                         ,dst-red+2 :weight bold))))
   `(diff-refine-removed ((t (:foreground ,dst-red-1 :background ,dst-red+1))))
   `(diff-removed ((t (:foreground ,dst-red-1))))
  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'distinguished)
;;; distinguished-theme.el ends here
