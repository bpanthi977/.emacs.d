(use-package company
  :ensure t
  :demand t
  :custom
  (company-auto-complete t)
  (company-auto-complete-chars "")
  ;; (company-frontends
  ;;   (quote
  ;; 	(company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-echo-frontend)))
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-quickhelp-color-background "#4F4F4F")
  (company-quickhelp-color-foreground "#DCDCCC")

  (company-require-match nil)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)

  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  :init
  (setf company-frontends
	'(company-echo-frontend company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-quickhelp-frontend))
  
  ;; Enable Company
  (global-company-mode 1)
  ;; (defun add-local-company-backend (backend)
  ;;   (add-to-list (make-local-variable 'company-backends) backend))
  (bind-keys :map company-active-map
	     ("M-n" . company-select-next)
	     ("M-p" . company-select-previous)
	     ("M-m d c" . company-show-doc-buffer))

  )

(use-package company-quickhelp
  :ensure t
  :after company
  :custom
  (company-quickhelp-color-background "#4F4F4F")
  (company-quickhelp-color-foreground "#DCDCCC")
  (company-quickhelp-delay 0.1)
  (company-quickhelp-mode t)
  (company-quickhelp-use-propertized-text t)
  :config 
  (add-to-list 'company-frontends 'company-quickhelp-frontend))


(use-package helm-dash
  :ensure t
  :defer t 
  :custom
  (helm-dash-docsets-path "/home/bpanthi/.local/share/Zeal/Zeal/docsets/"))
