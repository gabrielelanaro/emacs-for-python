
;; how to implement ccheckers, ala windows
;; %f means the abs filename
;; %d may be the directory
;; Implement in this way: use replace-regexp-in-string and then
;; shlex-split, then pass to flymake
(funcall (flymake-command-setup "flymake %f"))
(epy-setup-checker "python mycustom.py %f")

(flymake-command-parse "flymake \"%f\"")
(flymake-command-parse "python C:\\Home\\mycustom1.py %f")
