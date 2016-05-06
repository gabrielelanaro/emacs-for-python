(deftheme dark-theme eugeneai
  "Created 2014-03-25.")

(custom-theme-set-variables
 'dark-theme eugeneai
 '(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-safe-themes (quote ("5ea08b040c5515152d2bcae79a05ae5c98339b5729c359d02437bf4cc567cca5" "e200f481e31ceee929079ad8a6f629e45bdad45e5ac27101c89d55f4827071db" "e885ba299f1f0f0927bc8b10136a704c1ec05c7d04b7011559439dd9e56c56ab" "6252dbc43eefee0edc82ef000659ece4f77c4c989b4571d8d23485efc5e9ef4d" "d5af8f6ee92912a7bac8185ed447e1e825252728a1ebd1d41eb2da660d27ad62" default)))
 '(ispell-dictionary "english")
 '(minibuffer-auto-raise t)
 '(minibuffer-frame-alist (quote ((width . 80) (height . 1))))
 '(safe-local-variable-values (quote ((TeX-master . t) (ispell-dictionary . american) (py-indent-offset . 4) (TeX-master . "dis") (py-master-file . "/path/to/interactivetest.py") (whitespace-line-column . 80) (lexical-binding . t))))
 '(text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify)))
 '(which-function-mode nil)
 '(tool-bar-mode nil)
 '(show-paren-mode t)
 '(fringe-mode 0)
 '(column-number-mode t)
 '(blink-cursor-mode nil))

(custom-theme-set-faces
 'dark-theme eugeneai
 '(default ((t (:inherit nil :stipple nil :background "gray20" :foreground "wheat1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(minibuffer-prompt ((t (:foreground "CadetBlue1"))))
 '(mode-line ((t (:background "gray75" :foreground "black" :weight normal :height 0.5 :family "Droid Sans"))))
 '(mode-line-highlight ((t (:background "gold"))))
 '(tex-verbatim ((t (:family "Inconsolata LGC Medium")))))

(provide-theme 'dark-theme eugeneai)
