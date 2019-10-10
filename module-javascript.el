(install-packages (list 'rjsx-mode
			'lsp-mode))

(setq exec-path (append '("/home/bpanthi/.yarn/bin") exec-path))
(setq exec-path (append '("/home/bpanthi/.node_modules/bin") exec-path))
(setenv "PATH" (concat "/home/bpanthi/.yarn/bin:/home/bpanthi/.node_modules/bin:" (getenv "PATH")))

(defun my-js-file-hook ()
  (interactive)
  (require 'rjsx-mode)
  ;; (require 'flycheck)
  (require 'lsp)

  (setq js2-strict-missing-semi-warning nil)
  (setq js2-missing-semi-one-line-override t)
  (setq js2-highlight-level 3)
  (rjsx-mode)
  (lsp-mode)
  ;; (tern-mode t)
  ;; (add-to-list 'company-backends 'company-tern)
  ;; (flycheck-mode t)
  ;; (flycheck-select-checker 'javascript-eslint)
  ;; (add-to-list 'company-backends 'ac-js2-company)
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (js2-mode-idle-reparse (current-buffer))
  (company-mode-on)
  ;; (setf js3-auto-indent-p nil         ; it's nice for commas to right themselves.
  ;; 	js3-enter-indents-newline nil ; don't need to push tab before typing
  ;; 	js3-indent-on-enter-key nil)   ; fix indenting before moving on
  ;; (add-hook 'before-save-hook 'jsfmt-before-save))
  )

(setf flycheck-javascript-eslint-executable "/home/bpanthi/.node_modules/bin/eslint")

;; (add-to-list 'load-path "~/.emacs.d/tern/emacs/")
;; (add-to-list 'exec-path "/home/bpanthi/.emacs.d/tern/bin/")
;; (autoload 'tern-mode "tern.el" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . my-js-file-hook))
