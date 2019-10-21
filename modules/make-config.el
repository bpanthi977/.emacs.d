(defun funcall-bound (var)
  "if var is bound call it"
  (if (boundp var) (funcall var)))

(setq make-compile-func
      (lambda ()
	(compile (format "make -k %s" (file-name-base (buffer-file-name))))))

(setq make-all-func
      (lambda ()
	(compile (format "make -k ALL"))))

(setq make-compile-link-run-func
      (lambda ()
	(funcall make-compile-func)
	(funcall make-link-func)
	(funcall make-run-func)))

(make-local-variable 'make-compile-func)
(make-local-variable 'make-run-func)
(make-local-variable 'make-compile-run-func)
(make-local-variable 'make-link-func)
(make-local-variable 'make-all-func)
(make-local-variable 'make-default-makefile)

(defun my-make-compile ()
  (interactive)
  (funcall make-compile-func))

(defun my-make-run ()
  (interactive)
  (funcall make-run-func))

(defun my-make-compile-run ()
  (interactive)
  (funcall make-compile-run-func))

(defun my-make-link ()
  (interactive)
  (funcall make-link-func))

(defun my-make-all ()
  (interactive)
  (funcall make-all-func))

(defun my-create-makefile ()
  (interactive)
  (when (and (boundp 'make-default-makefile) (file-exists-p make-default-makefile))
    (copy-file make-default-makefile (expand-file-name "Makefile" (file-name-directory (buffer-file-name))))))

(defun setup-make-keys ()
  (interactive)
  (local-set-key (kbd "C-c C-c") 'my-make-compile)
  (local-set-key (kbd "C-c C-l") 'my-make-link)
  (local-set-key (kbd "C-c C-r") 'my-make-run)
  (local-set-key (kbd "C-c C-f") 'my-make-compile-run)
  (local-set-key (kbd "C-c C-a") 'my-make-all)
  (local-set-key (kbd "C-c C-m") 'my-create-makefile))
  
