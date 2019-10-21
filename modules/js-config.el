(use-package rjsx-mode
  :after (lsp-mode)
  :mode "\\.js\\'"
  :config
  (setq exec-path (append '("/home/bpanthi/.node_modules/bin"
  			    "/home/bpanthi/.yarn/bin")
  			  exec-path))
  (setenv "PATH" (concat "/home/bpanthi/.yarn/bin:/home/bpanthi/.node_modules/bin:"
  			 (getenv "PATH")))
  (setq js2-strict-missing-semi-warning nil
  	js2-missing-semi-one-line-override t
  	js2-highlight-level 3)
  :hook (rjsx-mode . (lambda () 
  		       (js2-mode-idle-reparse (current-buffer))
  		       (company-mode-on)
  		       ;; (add-hook 'before-save-hook 'jsfmt-before-save))
  		       )))

(use-package flycheck
  :config 
  (setf flycheck-javascript-eslint-executable "/home/bpanthi/.node_modules/bin/eslint"))



;; (tern-mode t)
;; (add-to-list 'company-backends 'company-tern)
;; (flycheck-mode t)
;; (flycheck-select-checker 'javascript-eslint)
;; (add-to-list 'company-backends 'ac-js2-company)
;; (add-to-list 'company-backends 'company-yasnippet)
(use-package tern
  :config
  (add-to-list 'load-path "~/.emacs.d/extra/tern/emacs/")
  (add-to-list 'exec-path "/home/bpanthi/.emacs.d/extra/tern/bin/"))

  
;; (autoload 'tern-mode "tern.el" nil t)

