(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
			  ("M-." . lsp-find-definition)
			  ("M-?" . lsp-find-references)))

(use-package lsp-ui
  :ensure t)
