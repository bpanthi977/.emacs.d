(use-package company
  :config 
  (setq company-idle-delay 0.5
	company-tooltip-limit 10
	company-minimum-prefix-length 2)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  ;; Enable Company
  (global-company-mode 1)
  (defun add-local-company-backend (backend)
    (add-to-list (make-local-variable 'company-backends) backend)))

(use-package company-quickhelp
  :after company
  :config 
  (add-to-list 'company-frontends 'company-quickhelp-frontend)
  (setq company-quickhelp-delay 0.2))
