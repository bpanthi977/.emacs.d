(setenv "PATH" (concat "/home/bpanthi/.local/bin:"
			 (getenv "PATH")))

(setq exec-path (append '("/home/bpanthi/.local/bin")
			exec-path))

(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode))

(use-package lsp-ui
  :ensure t
  :defer t 
  :after '(lsp-mode))


(defun bp/programming-mode-setup ()
  (add-hook 'before-save-hook #'whitespace-cleanup 0 t))

(add-hook 'prog-mode-hook #'bp/programming-mode-setup)


(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode -1))

(use-package dap-ui
  :config
  (setq dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . bottom) (slot . 1) (window-width . 0.20)))
    (,dap-ui--expressions-buffer . ((side . bottom) (slot . 2) (window-width . 0.20)))
    (,dap-ui--sessions-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
    (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
    (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
    (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.45))))))  

(use-package lsp-ivy
  :ensure t
  :defer t
  :after lsp)

(use-package iedit
  :ensure t
  :defer t
  :config
  (defun bp/lsp-iedit (&optional arg)
    (interactive "P")
    (print arg)
    (if (or arg (not lsp-mode))
        (iedit-mode)
      (lsp-iedit-highlights)))
  (global-set-key (kbd "C-;") #'bp/lsp-iedit))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :ensure t 
  :defer nil
  :commands (lsp lsp-mode)
  :init
  (bind-keys :map lsp-mode-map
	     ("M-." . lsp-find-definition)
             ("M-m ." . lsp-ivy-workspace-symbol)
	     ("M-?" . lsp-find-references)
	     ("M-," . pop-tag-mark)
             ("M-m d d" . lsp-ui-doc-glance)
             ("M-m d D" . lsp-ui-doc-show)
             ("M-m l r o" . bp/lsp-organize-imports))
  (define-key lsp-mode-map (kbd "M-m l") lsp-command-map)
  (defun bp/lsp-organize-imports ()
    (interactive)
    (lsp-remove-unused)
    (lsp-organize-imports))
  (setq lsp-keymap-prefix "M-m l")
  :config
  (setf lsp-disabled-clients '(semgrep-ls))

  (lsp-make-interactive-code-action fix-all "source.fixAll")
  (lsp-make-interactive-code-action remove-unused "source.removeUnused")
  (setf backup-by-copying t)
  (setq lsp-enable-file-watchers nil)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 3)
  (setq flycheck-check-syntax-automatically '(save idle-change)
	flycheck-idle-change-delay 3)

  (setq lsp-auto-configure t)

  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil)

  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-headerline-breadcrumb-enable t)
  ;; Sideline code actions 
  (setf lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-hover nil)
  (setq lsp-modeline-code-actions-enable nil)
  ;; * hide only errors
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t))
