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

(use-package typescript-mode
  :defer t
  :config
  (defun bp/typescript-mode-hook ()
    (prettier-js-mode)
    (lsp)
    (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
    (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
    (local-set-key (kbd "C-c b") 'ts-send-buffer)
    (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
    (local-set-key (kbd "C-c l") 'ts-load-file-and-go))

  (add-hook 'typescript-mode-hook 'bp/typescript-mode-hook))

(defun lsp-js-ts-rename-file ()
  "Rename current file and all it's references in other files."
  (interactive)
  (let* ((name (buffer-name))
         (old (buffer-file-name))
         (basename (file-name-nondirectory old)))
    (unless (and old (file-exists-p old))
      (error "Buffer '%s' is not visiting a file." name))
    (let ((new (read-file-name "New name: " (file-name-directory old) basename nil basename)))
      (when (get-file-buffer new)
        (error "A buffer named '%s' already exists." new))
      (when (file-exists-p new)
        (error "A file named '%s' already exists." new))
      (lsp--send-execute-command
       "_typescript.applyRenameFile"
       (vector (list :sourceUri (lsp--buffer-uri)
                     :targetUri (lsp--path-to-uri new))))
      (mkdir (file-name-directory new) t)
      (rename-file old new)
      (rename-buffer new)
      (set-visited-file-name new)
      (set-buffer-modified-p nil)
      (lsp-disconnect)
      (setq-local lsp-buffer-uri nil)
      (lsp)
      (lsp--info "Renamed '%s' to '%s'." name (file-name-nondirectory new)))))

(defun bp/projectile-js-prettier ()
    (interactive)
    (dolist (b (projectile-project-buffers))
      (with-current-buffer b
        (ignore-errors (prettier-js)))))
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
