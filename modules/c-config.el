(install-packages (list ;;'irony
                        'helm-dash
                        'flycheck
                        'company
                        ;;'company-irony
                        ;;'company-irony-c-headers))
                        'rtags
                        'company-rtags
                        ))
(defun my-create-compilation-db-file ()
  (interactive)
  (async-shell-command "make clean ; bear make -k"))

(defun my-c-make-compile ()
  (compile (format "make -k %s.o" (file-name-base (buffer-file-name)))))

(defun my-c-make-link ()
  (compile (format "make -k link-%s" (file-name-base (buffer-file-name)))))

(defun my-c-make-run ()
  (let* ((name (file-name-base (buffer-file-name)))
         (args (read-string (format "bin/%s " name))))
        (let ((default-directory (concat default-directory "/bin")))
          (async-shell-command (format "%s %s" name args)
                                                   (get-buffer-create "output")))
    (set-window-buffer (car (last (window-list-1))) "output")
    (switch-to-buffer-other-window "output" )))

(defun my-make-compile-link-run-file ()
  (interactive)
  (compile (format "make -k %s" (file-name-base (buffer-file-name))))
  (my-c-make-run))


(defun setup-c-make ()
  (interactive)
  (setup-make-keys)
  (setq-local make-compile-func #'my-c-make-compile)
  (setq-local make-run-func #'my-c-make-run)
  (setq-local make-link-func #'my-c-make-link)
  (setq-local make-compile-run-func #'my-make-compile-link-run-file)
  (setq-local make-default-makefile "~/.emacs.d/modules/makefiles/default-c-makefile")
  (wolfe/pretty-symbol-push-default))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package dap-codelldb
  :ensure nil
  :defer t
  :after (:and (:or c-mode c++-mode)
               dap-mode))

;; (defun setup-irony-keys ()
;;   (interactive)
;;   (local-set-key (kbd "C-c C-f") 'my-make-compile-link-run-file)
;;   (local-set-key (kbd "C-.") 'helm-dash-at-point)
;;   (local-set-key (kbd "M-.") 'find-tag)
;;   (local-set-key (kbd "C-c C-d") 'my-create-compilation-db-file))

;; (defun my-irony-mode-hook ()
;;   (interactive)
;;   (require 'flycheck)
;;   (flycheck-irony-setup)
;;   (flycheck-select-checker 'irony)
;;   (require 'helm-dash)
;;   (helm-dash-activate-docset "C++")
;;    (setf company-irony-ignore-case t)
;;   (setup-c-make)
;;   (setup-irony-keys)

;;   (add-to-list 'company-backends 'company-irony)
;;   (add-to-list 'company-backends 'company-irony-c-headers)
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))



;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (add-hook 'fortran-mode-hook 'setup-c-make)
;; (add-hook 'fortran-mode-hook 'setup-irony-keys)
;; (add-hook 'f90-mode-hook 'setup-irony-keys)
;; (add-hook 'f90-mode-hook 'setup-c-make)


;; ;; Irony
;; (defun my-irony-mode-on ()
;;   ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
;;   (require 'irony)
;;   (when (member major-mode irony-supported-major-modes)
;;     (irony-mode 1)))

;; (add-hook 'c++-mode-hook 'my-irony-mode-on)
;; (add-hook 'c-mode-hook 'my-irony-mode-on)
;; (add-hook 'objc-mode-hook 'my-irony-mode-on)


;; (defun my-rtags-on ()
;;   (require 'rtags)

;;   (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
;;   (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

;;   (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;;   (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;;   (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
;;   (rtags-enable-standard-keybindings)

;;   ;;(setq rtags-use-helm t)
;;   (require 'company-rtags)
;;   (setq rtags-autostart-diagnostics t)
;;   (rtags-diagnostics)
;;   (setq rtags-completions-enabled t)
;;   (ad 'company-rtags company-backends)

;;   (rtags-start-process-unless-running))

;; ;; Shutdown rdm when leaving emacs.
;; (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
;; (add-hook 'c++-mode-hook 'my-rtags-on)
;; (add-hook 'c-mode-hook 'my-rtags-on)
(add-hook 'c-mode-hook 'setup-c-make)
;; (add-hook 'objc-mode-hook 'my-rtags-on)
