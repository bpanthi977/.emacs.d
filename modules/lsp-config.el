(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-mode)
  :hook (lsp-mode . (lambda ()
					  (require 'company-lsp)
					  (add-to-list 'company-backends 'company-lsp)))
  :bind (:map lsp-mode-map
			  ("M-." . lsp-find-definition)
			  ("M-?" . lsp-find-references)))

(use-package lsp-ui
  :ensure t
  :defer t 
  :after '(lsp-mode))

(use-package company-lsp
  :ensure t
  :defer t
  :after (lsp-mode))
