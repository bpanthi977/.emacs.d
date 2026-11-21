(use-package ivy
  :ensure t
  :init
  (ivy-mode t)
  (keymap-unset ivy-minibuffer-map "S-SPC"))

(use-package swiper
  :ensure t
  :bind (:map swiper-map
	 ("M-c" . swiper-mc))
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)))

;; To use with multiple-cursors
(use-package phi-search
  :ensure t
  :bind (("C-c C-s" . phi-search)
	 ("C-c C-r" . phi-search-backward)))

;; Remembers history for M-x
(use-package smex
  :ensure t)

(use-package counsel
  :ensure t
  :bind (("C-c s" . counsel-rg))
  :init
  (counsel-mode t))
