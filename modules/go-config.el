(install-packages (list 'go-mode
			'company-go
			'go-projectile))

;; pkg go installation
(setq exec-path (append '("/usr/local/go/bin") exec-path))
(setq exec-path (append '("/home/bpanthi/go/bin") exec-path))
(setenv "PATH" (concat "/home/bpanthi/go/bin:/usr/local/go/bin:" (getenv "PATH")))

(defun my-go-mode-hook ()
  (require 'godoc)
  (setq tab-width 2 indent-tabs-mode 1)
  ;; eldoc shows the signature of the function at point in the status bar.
  (go-eldoc-setup)
  (local-set-key (kbd "M-.") #'godef-jump)
  
  ;; Format file, add/remove imports before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (flycheck-mode)
  
  ;; Company
  (require 'company-go)
  (company-mode)
  ;; (set (make-local-variable 'company-backends) '(company-go))
  (add-to-list 'company-backends 'company-go)
  

  (require 'go-projectile)
  (setf go-projectile-tools-path "/home/bpanthi/go")
  (go-projectile-tools-add-path)
  (setq gofmt-command (concat go-projectile-tools-path "/bin/goimports"))
  ;; (eglot-ensure)
  ;; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
  (let ((map go-mode-map))
    (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
    (define-key map (kbd "C-c m") 'go-test-current-file)
    (define-key map (kbd "C-c .") 'go-test-current-test)
    (define-key map (kbd "C-c b") 'go-run)))
(add-hook 'go-mode-hook 'my-go-mode-hook)



;; Use projectile-test-project in place of 'compile'; assign whatever key you want.
;; (global-set-key [f9] 'projectile-test-project)

;; "projectile" recognizes git repos (etc) as "projects" and changes settings
;; as you switch between them.



;; gotest defines a better set of error regexps for go tests, but it only
;; enables them when using its own functions. Add them globally for use in
;; (require 'compile)
;; (require 'gotest)
;; (dolist (elt go-test-compilation-error-regexp-alist-alist)
;;   (add-to-list 'compilation-error-regexp-alist-alist elt))
;; (defun prepend-go-compilation-regexps ()
;;   (dolist (elt (reverse go-test-compilation-error-regexp-alist))
;;     (add-to-list 'compilation-error-regexp-alist elt t)))
;; (add-hook 'go-mode-hook 'prepend-go-compilation-regexps)


