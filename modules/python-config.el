;;(install-packages (list ;; 'flycheck


;; 'anaconda-mode
;; 'company-anaconda
;; 'elpy
;; 'company-jedi
;;		   'jedi-core
;;		   'helm-dash))

;; (require 'elpy)
;; (elpy-enable)

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; (add-hook 'elpy-mode-hook 'flycheck-mode))

(defun my-python-mode-hook ()
  ;; (require 'elpy)
  ;; (require 'flycheck)
  ;; (flycheck-mode)
  ;; (require 'anaconda-mode)
  ;; (require 'company-anaconda)
  ;; (elpy-modules-global-init)
  ;; (elpy-mode t)
  ;; (add-to-list 'company-backends 'company-elpy)
  (setf python-environment-directory (concat init-dir "extra/python-environment"))
  (require 'helm-dash)
  (require 'company-jedi)
  ;; (require 'company-jedi)
  ;; (require 'jedi-core)
  ;; (setq jedi:complete-on-dot t)
  (add-to-list 'company-backends 'company-jedi)
  ;; (add-to-list 'company-backends 'company-anaconda)
  ;; (anaconda-eldoc-mode t)
  ;; (anaconda-mode t)
  (helm-dash-activate-docset "Python_3")
  (helm-dash-activate-docset "OpenCV_C++")
  (local-set-key (kbd "C-.") 'helm-dash-at-point)
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)

  (wolfe/pretty-symbol-push-default)
  (push '("def"    . ?ƒ) prettify-symbols-alist)
  (push '("sum"    . ?Σ) prettify-symbols-alist)
  (push '("**2"    . ?²) prettify-symbols-alist)
  (push '("**3"    . ?³) prettify-symbols-alist)
  (push '("None"   . ?∅) prettify-symbols-alist)
  (push '("in"     . ?∈) prettify-symbols-alist)
  (push '("not in" . ?∉) prettify-symbols-alist)
  (push '("return" . ?➡) prettify-symbols-alist)
  (prettify-symbols-mode t)
  )

;; (add-hook 'python-mode-hook 'my-python-mode-hook)

(setq python-shell-interpreter "python")


(use-package jedi-core
  :ensure t
  :commands (jedi-mode)
  :defer t
  :hook (python-mode . my-python-mode-hook)
  :config
  (require 'python)
  (bind-keys :map python-mode-map
			 ("M-." . jedi:goto-definition)
			 ("M-," . jedi:goto-definition-pop-marker)
			 ("M-m d" . jedi:show-doc)))

(use-package company-jedi
  :ensure t
  :defer t 
  :after (company jedi-core))

(use-package lsp-python-ms
  :ensure t
  :defer t
  :after (python)
  :config
  (setf lsp-python-ms-executable "c:/Users/hp/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer.exe")
  ;; :hook (python-mode . (lambda ()
  ;; 						 (my-python-mode-hook)
  ;; 						 (lsp))))  ; or lsp-deferred
  )

(defmacro rm-nth (n var)
  `(setf ,var (remove (nth ,n ,var) ,var) ))

