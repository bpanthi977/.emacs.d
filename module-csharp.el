(use-package omnisharp
  :after (company flycheck)
  :config
  (setf omnisharp-server-executable-path "/home/bpanthi/.emacs.d/omnisharp/server/v1.26.3/run")
  :hook (csharp-mode .
		     (lambda ()
		       (omnisharp-mode)
		       (flycheck-mode)
		       (add-to-list 'company-backends #'company-omnisharp)
		       
		       (setq indent-tabs-mode nil)
		       (setq c-syntactic-indentation t)
		       (c-set-style "ellemtel")
		       (setq c-basic-offset 4)
		       (setq truncate-lines t)
		       (setq tab-width 4)
		       (setq evil-shift-width 4)
		       ;;csharp-mode README.md recommends this too
		       ;;(electric-pair-mode 1)       ;; Emacs 24
		       ;;(electric-pair-local-mode 1) ;; Emacs 25
		       ))
  :bind (:map omnisharp-mode-map
	      ("C-c r r" . omnisharp-run-code-action-refactoring)
	      ("C-c C-c" . recompile)))


