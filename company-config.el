(use-package company
  :custom
  (company-auto-complete t)
  (company-auto-complete-chars nil)
  (company-frontends
   (quote
    (company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-quickhelp-frontend company-echo-frontend)))
  (company-idle-delay 0.1)
  (company-irony-ignore-case t)
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.5)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 2)
  (company-require-match nil)

  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (company-tooltip-flip-when-above t)
  :config
  ;; Enable Company
  (global-company-mode 1)
  (defun add-local-company-backend (backend)
    (add-to-list (make-local-variable 'company-backends) backend)))

(use-package company-quickhelp
  :after company
  :custom
  (company-quickhelp-color-background "#4F4F4F")
  (company-quickhelp-color-foreground "#DCDCCC")
  (company-quickhelp-delay 0.2)
  (company-quickhelp-mode t)
  (company-quickhelp-use-propertized-text t)
  :config 
  (add-to-list 'company-frontends 'company-quickhelp-frontend))

(use-package helm-dash
  :config
  (helm-dash-docsets-path "/home/bpanthi/.local/share/Zeal/Zeal/docsets/"))
