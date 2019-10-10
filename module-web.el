(install-packages (list 'web-mode
			'ac-php
			'helm-dash
			'company
			'company-web))


(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" .web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; (defun my-web-mode-hook ()
;;   (interactive)
;;  
;;   )
;; (add-hook 'web-mode-hook 'my-web-mode-hook)


(defun my-php-file-hook ()
  (interactive)
  (require 'web-mode)
  (web-mode)
  ;; (require 'lsp-php)
  ;; (lsp-php-enable)
  (require 'ac-php)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ac-php-backend)
  (ac-php-core-eldoc-setup ) ;; enable eldoc
  (ac-php-remake-tags)
  (local-set-key  (kbd "M-.") 'ac-php-find-symbol-at-point)   ;goto define
  (local-set-key  (kbd "C-t") 'ac-php-location-stack-back) 
  (require 'helm-dash)
  (helm-dash-activate-docset "PHP")
  (local-set-key (kbd "C-.") 'helm-dash-at-point))

(defun my-html-file-hook ()
  (interactive)
  (require 'web-mode)
  (require 'company)
  (require 'company-web)
  (web-mode)
  (add-to-list 'company-backends 'company-web-html))

(add-to-list 'auto-mode-alist '("\\.php\\'" . my-php-file-hook))
(add-to-list 'auto-mode-alist '("\\.html\\'" . my-html-file-hook))


