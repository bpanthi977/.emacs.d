(require 'cl)
(let ((extra-paths '("~/.node_modules/bin"
                     "~/.yarn/bin"
                     "~/.bun/bin"
                     "~/.local/bin"
                     "/opt/homebrew/bin")))
  (setq exec-path (append extra-paths exec-path))
  (setenv "PATH" (cl-reduce (lambda (path dir)
                           (concat dir ":" path))
                         extra-paths
                         :initial-value (getenv "PATH"))))
;; (setq exec-path (append exec-path '("~/.nvm/versions/node/v14.20.0/bin")))

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :hook (rjsx-mode . (lambda ()
                       (js2-mode-idle-reparse (current-buffer))))

  :config
  (require 'lsp-mode)
  (setq js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override t
        js2-highlight-level 3)

  (setf flycheck-javascript-eslint-executable "/home/bpanthi/.node_modules/bin/eslint"))

(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.tsx\\'"
  :config
  (defun bp/typescript-config-from-prettierrc ()
    (when (buffer-file-name)
      (let* ((prettierrc (locate-dominating-file (buffer-file-name)  ".prettierrc"))
             (config (if prettierrc
                         (with-temp-buffer
                           (insert-file-contents (concat prettierrc "/.prettierrc"))
                           (goto-char (point-min))
                           (json-parse-buffer)))))
        (when config
          (when-let ((indent-level (gethash "tabWidth" config)))
            (setq-local typescript-indent-level indent-level))))))


  (defun bp/typescript-mode-hook ()
    (bp/typescript-config-from-prettierrc)
    (lsp)
    (lambda ()
      (local-set-key (kbd "C-c C-c") #'(lambda ()
                                       (ts-send-region (region-beginning)(region-end))))
      (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
      (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
      (local-set-key (kbd "C-c b") 'ts-send-buffer)
      (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
      (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))
    (prettier-js-mode)

    (add-hook 'typescript-mode-hook 'bp/typescript-mode-hook))

(use-package json-mode
  :ensure t
  :defer t)

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
        (ignore-errors (prettier-prettify)))))

(use-package tern
  :ensure t
  :defer t
  :commands (tern-mode)
  :config
  (add-to-list 'load-path "~/.emacs.d/extra/tern/emacs/")
  (add-to-list 'exec-path "/home/bpanthi/.emacs.d/extra/tern/bin/"))

;; (use-package prettier
;;   :ensure t
;;   :defer t
;;   :hook ((js-mode . prettier-mode)))

(use-package ts-comint
  :ensure t
  :config
  (setf ts-comint-program-command "ts-node"))
