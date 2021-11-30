(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\'" . web-mode)
	 ("\\.[agj]sp\\'" . web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.erb\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)))

(use-package company-web
  :ensure t
  :defer t
  :after (web-mode)
  :hook (web-mode . (lambda ()
		      (require 'company-web)
		      (setq-local company-backends (cons 'company-web-html company-backends)))))

