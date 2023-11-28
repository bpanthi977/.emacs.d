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
  (setf show-trailing-whitespace t)
  (add-hook 'before-save-hook #'whitespace-cleanup 0 t))

(add-hook 'prog-mode-hook #'bp/programming-mode-setup)


(use-package dap-mode
  :ensure t
  :defer t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-mode -1))

(use-package dap-ui
  :defer t
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
  :demand t
  :commands (lsp lsp-mode)
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-m ." . lsp-ivy-workspace-symbol)
              ("M-?" . lsp-find-references)
              ("M-," . pop-tag-mark)
              ("M-m d d" . lsp-ui-doc-glance)
              ("M-m d D" . lsp-ui-doc-show)
              ("M-m l r o" . bp/lsp-organize-imports))
  :init
  (setq lsp-keymap-prefix "M-m l")
  :config
  (defun bp/lsp-organize-imports ()
    (interactive)
    (lsp-remove-unused)
    (lsp-organize-imports))

  (setf lsp-disabled-clients '(semgrep-ls))

  (lsp-make-interactive-code-action fix-all "source.fixAll")
  (lsp-make-interactive-code-action remove-unused "source.removeUnused")
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

;; Magit
(use-package magit
  :ensure t
  :defer t
  :init
  (setf transient-history-file (concat savefile-dir "/transient/history.el"))
  (setq smerge-command-prefix "\C-cm")
  (bind-keys :map bp/global-prefix-map
             ("g b" . magit-log-buffer-file))
  :config
  (global-set-key (kbd "C-x C-g") #'magit-dispatch)
  (defvar bp/version-control/valid-commit-title-prefixes
    '("🎁: feature (A new feature)"
      "🎨: design/ui (Design changes that don't add feature)"
      "⏪: revert (Revert back changes)"
      "🐛: bug fix (A bug fix)"
      "⚡️: performance (Improvement in performance without adding external feature)"
      "📚: docs (Changes to documentation)"
      "💄: style (Formatting, missing semi colons, etc; no code change)"
      "♻️: refactor (Refactoring production code)"
      "📋: tests (Adding tests, refactoring test; no production code change)"
      "🧹: chore (Updating build tasks, package manager configs, etc; no production code change)"
      "🚧: wip (Work in progress code)"
      "✂️: rebase (Rebase needed)")
    "Commit message guidelines.")

  (defun bp/magit-commit-emoji ()
    (interactive)
    (let ((splitter ":")
          (padding " ")
          (commit-type (completing-read "Commit title prefix: "
                                        bp/version-control/valid-commit-title-prefixes nil t)))
      (goto-char (point-min))
      (insert (car (s-split splitter commit-type)) padding)))

  (defun bp/git-commit-mode-hook ()
    "If the first line is empty, prompt for commit type and insert it.

Add PADDING between inserted commit type and start of title.  For
the `completing-read' show the whole message.  But use the
SPLITTER to determine the prefix to include."
    (when (and (string= (buffer-name) "COMMIT_EDITMSG")
               ;; Is the first line empty?
               (save-excursion
                 (goto-char (point-min))
                 (beginning-of-line-text)
                 (looking-at-p "^$")))
      (bp/magit-commit-emoji)))

  (add-hook 'find-file-hook 'bp/git-commit-mode-hook))

(use-package forge
  :ensure t
  :defer t
  :after magit)

;; orgit
;;; Save magit buffer/commit links to org mode
(use-package orgit
  :ensure t)


;;; git-link
;;; Copy url to github/bitbucket/... for current buffer and line number
(use-package git-link
  :ensure t
  :defer t
  :bind (("C-c l" . git-link))
  :config
  (setf git-link-use-commit t))
