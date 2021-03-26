(use-package rjsx-mode
  :ensure t
  :defer t 
  :mode "\\.js\\'"
  :hook (rjsx-mode . (lambda () 
		       (js2-mode-idle-reparse (current-buffer))))

  :config
  (require 'lsp-mode)
  (setq exec-path (append '("/home/bpanthi/.node_modules/bin"
			    "/home/bpanthi/.yarn/bin")
			  exec-path))
  (setenv "PATH" (concat "/home/bpanthi/.yarn/bin:/home/bpanthi/.node_modules/bin:"
			 (getenv "PATH")))
  (setq js2-strict-missing-semi-warning nil
	js2-missing-semi-one-line-override t
	js2-highlight-level 3)
  
  (setf flycheck-javascript-eslint-executable "/home/bpanthi/.node_modules/bin/eslint"))

(use-package tern
  :ensure t
  :defer t
  :commands (tern-mode)
  :config
  (add-to-list 'load-path "~/.emacs.d/extra/tern/emacs/")
  (add-to-list 'exec-path "/home/bpanthi/.emacs.d/extra/tern/bin/"))


(use-package prettier-js
  :ensure t
  :hook ((js-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--trailing-comma" "all"
			   "--bracket-spacing" "false"
			   "--tab-width" "4")))
