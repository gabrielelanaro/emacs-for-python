;;; eide-config.el --- Emacs-IDE, config

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

(provide 'eide-config)

(require 'custom)

(require 'eide-display)
(require 'eide-menu)
(require 'eide-search)
(require 'eide-vc)

(defvar eide-config-ready nil)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION GROUPS AND VARIABLES
;; ----------------------------------------------------------------------------

(defgroup eide nil "Customization of Emacs-IDE."
  :tag "Emacs-IDE"
  :group 'emacs)
(defcustom eide-custom-override-emacs-settings t "Enable or disable \"Emacs settings\" group. If disabled, Emacs-IDE will not override any default or user setting. If enabled, Emacs-IDE will override some default or user settings, in order to provide a more user-friendly interface, and each setting can be enabled or disabled individually in \"Emacs settings\" group."
  :tag "Override Emacs settings"
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t))
  :set '(lambda (param value) (set-default param value) (eide-i-config-apply-emacs-settings))
  :initialize 'custom-initialize-default
  :group 'eide)

(defgroup eide-display nil "Display."
  :tag "Display"
  :group 'eide)

(defgroup eide-windows nil "Windows layout."
  :tag "Windows layout"
  :group 'eide)

(defgroup eide-menu nil "Menu colors and display."
  :tag "Menu colors and display"
  :group 'eide)

(defgroup eide-version-control nil "Version control facilities in menu."
  :tag "Version control"
  :group 'eide)

(defgroup eide-project nil "Projects management and default commands that are set in project configuration when a project is created."
  :tag "Projects"
  :group 'eide)

(defgroup eide-emacs-settings nil "Options that are not specific to Emacs-IDE, but can be set to override some default settings of Emacs, and provide a more user-friendly interface (requires 'Override Emacs settings' to be enabled)."
  :tag "Emacs settings"
  :group 'eide)

(defgroup eide-search nil "Tags and cscope options."
  :tag "Search"
  :group 'eide-emacs-settings)

;; ----------------------------------------------------------------------------
;; CUSTOMIZATION FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-i-config-apply-emacs-settings ()
  "Apply \"Emacs settings\" options."
  (when eide-config-ready
    (eide-search-apply-customization)
    (eide-keys-apply-emacs-settings)))

;; ----------------------------------------------------------------------------
;; FUNCTIONS
;; ----------------------------------------------------------------------------

(defun eide-config-init ()
  "Config initialization: save Emacs settings."
  ;; custom-theme-load-path requires Emacs 24
  (add-to-list 'custom-theme-load-path "/usr/share/emacs/site-lisp/")
  (add-to-list 'custom-theme-load-path "/usr/local/share/emacs/site-lisp/")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp")
  (eide-search-save-emacs-settings)
  (eide-keys-save-emacs-settings))

(defun eide-config-apply ()
  "Apply config."
  ;; Custom values are initialized (and set if customized) by
  ;; custom-set-variables in ~/.emacs, which may be done before or after
  ;; eide-start call.
  ;; There are dependencies between parameters: we cannot set them until they
  ;; have all been defined.
  ;; Moreover, in order to avoid to set different values successively, values
  ;; are not set until eide-config-ready is set (below).
  (setq eide-config-ready t)
  (eide-project-apply-customization)
  (eide-display-apply-color-theme)
  (eide-i-config-apply-emacs-settings)
  (eide-vc-apply-customization))

(defun eide-config-customize ()
  "Display customization of Emacs-IDE options (full frame)."
  (interactive)
  (eide-windows-hide-ide-windows)
  (eide-windows-save-and-unbuild-layout)
  (eide-keys-configure-for-special-buffer)
  (ad-deactivate 'switch-to-buffer)
  (customize-group 'eide))

(defun eide-config-customize-themes ()
  "Display customization of themes (full frame)."
  (interactive)
  (eide-windows-hide-ide-windows)
  (eide-windows-save-and-unbuild-layout)
  (eide-keys-configure-for-special-buffer)
  (ad-deactivate 'switch-to-buffer)
  (setq eide-windows-themes-edited-flag t)
  ;; customize-themes doesn't seem to be working properly
  ;; when selecting multiple themes.
  ;;(customize-themes))
  (customize-option 'custom-enabled-themes))

;;; eide-config.el ends here
