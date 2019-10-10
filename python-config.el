(install-packages (list ;; 'flycheck
		   ;; 'anaconda-mode
		   ;; 'company-anaconda
		   ;; 'elpy
		   'company-jedi
		   'jedi-core
		   'helm-dash))

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
  (require 'helm-dash)
  (require 'company-jedi)
  (require 'jedi-core)
  (setq jedi:complete-on-dot t)
  (add-to-list 'company-backends 'company-jedi)
  ;; (add-to-list 'company-backends 'company-anaconda)
  ;; (anaconda-eldoc-mode t)
  ;; (anaconda-mode t)
  (helm-dash-activate-docset "Python_3")
  (helm-dash-activate-docset "OpenCV_C++")
  (local-set-key (kbd "C-.") 'helm-dash-at-point)
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))

;; (defun my-python-mode-hook ()

;;   )

(add-hook 'python-mode-hook 'my-python-mode-hook)

(setq python-shell-interpreter "python3")

;; WINDOWS CONFIG
(when (string-equal system-type "windows-nt")
  (setq python-shell-interpreter "bash -c python"))





