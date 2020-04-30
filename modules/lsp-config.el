(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-mode)
  :bind (:map lsp-mode-map
			  ("M-." . lsp-find-definition)
			  ("M-?" . lsp-find-references)))

(use-package lsp-ui
  :ensure t
  :defer t 
  :after '(lsp-mode))
