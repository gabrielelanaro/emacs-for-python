;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-master nil)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server (quote ask))
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (eugeneai-theme)))
 '(custom-safe-themes
   (quote
    ("5ea08b040c5515152d2bcae79a05ae5c98339b5729c359d02437bf4cc567cca5" "e200f481e31ceee929079ad8a6f629e45bdad45e5ac27101c89d55f4827071db" "e885ba299f1f0f0927bc8b10136a704c1ec05c7d04b7011559439dd9e56c56ab" "6252dbc43eefee0edc82ef000659ece4f77c4c989b4571d8d23485efc5e9ef4d" "d5af8f6ee92912a7bac8185ed447e1e825252728a1ebd1d41eb2da660d27ad62" default)))
 '(epy-load-yasnippet-p t)
 '(ispell-dictionary "ru-yeyo")
 '(load-prefer-newer t)
 '(minibuffer-auto-raise t)
 '(minibuffer-frame-alist (quote ((width . 80) (height . 1))))
 '(rw-hunspell-default-dictionary "russian")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/hunspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
 '(safe-local-variable-values
   (quote
    ((TeX-auto-save . t)
     (TeX-parse-self . t)
     (major-mode . rst-mode)
     (TeX-source-correlate-start-server)
     (TeX-source-correlate-mode . 1)
     (TeX-PDF-mode . 1)
     (eval ispell-change-dictionary "ru_RU_hunspell")
     (TeX-master . t)
     (ispell-dictionary . american)
     (py-indent-offset . 4)
     (TeX-master . "dis")
     (py-master-file . "/path/to/interactivetest.py")
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-flyspell text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(w3-honor-stylesheets t)
 '(which-function-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Mono" :foundry "unknown" :slant normal :weight normal :height 159 :width normal))))
 '(minibuffer-prompt ((t (:foreground "CadetBlue1"))))
 '(mode-line ((t (:background "gray75" :foreground "black" :weight normal :height 0.5 :family "Droid Sans"))))
 '(mode-line-highlight ((t (:background "gold")))))
