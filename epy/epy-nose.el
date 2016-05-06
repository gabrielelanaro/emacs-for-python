(require 'nose)

;; Nose bindings
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key "\C-ca" 'nosetests-all)
            (local-set-key "\C-cF" 'nosetests-failed)
            (local-set-key "\C-cM" 'nosetests-module)  ;; C-c m conflicts w/ pylint
            (local-set-key "\C-c." 'nosetests-one)
            (local-set-key "\C-cx" 'nosetests-stop)
            (local-set-key "\C-cpa" 'nosetests-pdb-all)
            (local-set-key "\C-cpm" 'nosetests-pdb-module)
            (local-set-key "\C-cp." 'nosetests-pdb-one))
          )


(provide 'epy-nose)
