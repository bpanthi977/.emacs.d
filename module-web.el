(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\'" . web-mode)
	 ("\\.php\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)))

(use-package ac-php
  :after (helm-dash)
  :mode "\\.php\\'"
  :commands (ac-php-find-symbol-at-point ac-php-location-stack-back helm-dash-at-point)
  :bind (:map php-mode-map
	      ("M-." . ac-php-find-symbol-at-point)
	      ("C-t" . ac-php-location-stack-back)
	      ("C-." . helm-dash-at-point))
  :hook (php-mode . (lambda () 
		      (add-local-company-backend 'company-ac-php-backend)
		      (ac-php-core-eldoc-setup ) ;; enable eldoc
		      (ac-php-remake-tags)
		      (helm-dash-activate-docset "PHP"))))

(use-package company-web 
  :hook (web-mode . (lambda ()
		      (add-local-company-backend 'company-web-html))))
