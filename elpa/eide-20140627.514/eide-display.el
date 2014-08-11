;;; eide-display.el --- Emacs-IDE, display

;; Copyright (C) 2008-2014 Cédric Marie

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'eide-display)

(require 'eide-help)
(require 'eide-menu)
(require 'eide-project)

(defvar eide-display-background-color nil)
(defvar eide-display-foreground-color nil)

(defvar eide-display-color-theme nil)

;; Hidden text (for hide/show minor mode)
;; Does not work with Emacs 22.3: I comment it until I can test
;; and maybe fix the bug.
;;(make-face 'font-selective-display-face)
;;(set-face-foreground 'font-selective-display-face "blue")
;;(set-face-background 'font-selective-display-face "lavender")
;;(setq font-selective-display-face-id (face-id 'font-selective-display-face))

;;(setq selective-display-vector (vconcat "{ ... }\n"))
;;(setq selective-display-vector (vconcat "\n" (mapcar '(lambda (x) (+ (* font-selective-display-face-id 524288) x)) selective-display-vector)))
;;(set-display-table-slot standard-display-table 'selective-display selective-display-vector)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION VARIABLES
;; ----------------------------------------------------------------------------

(defcustom eide-custom-color-theme nil "Color theme for Emacs-IDE specific faces (menu, help, and list of projects). If 'auto', it will be set according to Emacs-IDE color theme for source code (light if none is enabled)."
  :tag "Color theme for Emacs-IDE specific faces (menu, help, and list of projects)"
  :type '(choice (const :tag "auto" nil) (const dark) (const light))
  :set '(lambda (param value) (set-default param value) (eide-display-apply-color-theme))
  :initialize 'custom-initialize-default
  :group 'eide-display)

(defcustom eide-custom-start-maximized t "Start with maximized frame."
  :tag "Start with maximized frame"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :group 'eide-display)

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-display-apply-color-theme ()
  "Apply color theme."
  (when eide-config-ready
    (if eide-custom-color-theme
      ;; Color theme for Emacs-IDE specific faces is forced
      (setq eide-display-color-theme eide-custom-color-theme)
      ;; Color theme for Emacs-IDE specific faces is not forced
      ;; and depends on which Emacs-IDE color theme is enabled
      (if (custom-theme-enabled-p 'eide-dark)
        ;; If eide-dark theme is enabled, use dark color theme for Emacs-IDE
        ;; specific faces
        (setq eide-display-color-theme 'dark)
        ;; If eide-light theme is enabled, or neither eide-dark nor eide-light
        ;; theme is enabled, use light color theme for Emacs-IDE specific faces
        (setq eide-display-color-theme 'light)))
    ;; Save current colors
    (setq eide-display-background-color (face-background 'default))
    (setq eide-display-foreground-color (face-foreground 'default))
    (eide-menu-apply-color-theme)
    (eide-project-apply-color-theme)
    (eide-help-apply-color-theme)))

(defun eide-display-set-colors-for-files ()
  "Set colors for edition mode."
  (set-background-color eide-display-background-color)
  (set-foreground-color eide-display-foreground-color)
  (set-face-background 'fringe eide-display-background-color))

;;; eide-display.el ends here
