(use-package haskell-mode
  :ensure t
  :defer t
  :config
  
  (setq exec-path (append '("/home/bpanthi/.cabal/bin")
			  exec-path))
  (setq exec-path (append '("/home/bpanthi/.ghcup/bin/")
			  exec-path))
  (setenv "PATH" (concat "/home/bpanthi/.ghcup/bin/:"
			 (getenv "PATH")))

  (setq haskell-process-type 'ghci)
  
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))
  
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)

  (eval-after-load 'haskell-cabal '(progn
                                     (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))))

(use-package lsp-haskell
  :ensure t)




