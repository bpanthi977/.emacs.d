(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\'" . web-mode)
	 ("\\.php\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)))

(use-package php-mode
  :ensure t
  :defer t
  :mode "\\.php\\'")

(use-package ac-php
  :ensure t
  :after (helm-dash php-mode)
  :mode "\\.php\\'"
  :commands (ac-php-find-symbol-at-point ac-php-location-stack-back helm-dash-at-point)
  :hook (php-mode . (lambda ()
		      (require 'ac-php)
		      (setq-local company-backends (cons 'company-ac-php-backend company-backends))
		      (ac-php-core-eldoc-setup ) ;; enable eldoc
		      (ac-php-remake-tags)
		      (helm-dash-activate-docset "PHP")))
  :config
  (bind-keys :map php-mode-map
	     ("M-." . ac-php-find-symbol-at-point)
	     ("C-t" . ac-php-location-stack-back)
	     ("C-." . helm-dash-at-point)))

(use-package company-web
  :ensure t
  :defer t
  :after (web-mode)
  :hook (web-mode . (lambda ()
		      (require 'company-web)
		      (setq-local company-backends (cons 'company-web-html company-backends)))))

