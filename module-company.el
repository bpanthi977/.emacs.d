(require-packages (list 'company
			'company-quickhelp))


(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

;; Company Quick-help
;;(require 'company-quickhelp)
;; (company-quickhelp--enable)
(add-to-list 'company-frontends 'company-quickhelp-frontend)
(setq company-quickhelp-delay 0.2)

;; Enable Company
(global-company-mode 1)

