(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode))

(use-package lsp-ui
  :ensure t
  :defer t 
  :after '(lsp-mode))

(use-package company-lsp
  :ensure t
  :defer t
  :after (lsp-mode))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :ensure t 
  :defer t 
  :commands (lsp lsp-mode)
  :bind (:map lsp-mode-map
	      ("M-." . lsp-find-definition)
	      ("M-?" . lsp-find-references)
	      ("M-," . pop-tag-mark))
  :init
  (setq lsp-keymap-prefix "M-m l")
  :config
  (setq lsp-auto-configure t)

  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable t)
  (setq lsp-headerline-breadcrumb-enable t)
  ;; Sideline code actions 
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-modeline-code-actions-enable nil)
  ;; * hide only errors
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t))
  
