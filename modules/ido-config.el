;; Ivy replaces ido
;; Counsel replaces smex


;; (use-package ido
;;   :bind (("C-x C-f" . ido-find-file))
;;   :config 
;;   (ido-vertical-mode)
;;   (setq ido-virtual-buffers t  
;; 	ido-enable-prefix nil
;; 	ido-enable-flex-matching t
;; 	ido-create-new-buffer 'always
;; 	ido-use-filename-at-point 'guess
;; 	ido-max-prospects 10
;; 	ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
;; 	ido-default-file-method 'selected-window
;; 	ido-auto-merge-work-directories-length -1))

;; (use-package ido-ubiquitous
;;   :after (ido)
;;   :init
;;   (ido-ubiquitous-mode 1))

;; ;;; smarter fuzzy matching for ido
;; (use-package flx-ido
;;   :after ido
;;   :config
;;   ;; disable ido faces to see flx highlights
;;   (setq ido-use-faces nil)
;;   :init
;;   (flx-ido-mode +1))

;; smex, remember recently and most frequently used commands
(use-package smex
  :ensure t
  :config 
  (setq smex-save-file (expand-file-name ".smex-items" savefile-dir)))


;;; Ivy
(use-package ivy
  :ensure t
  :bind (("C-x b" . ido-switch-buffer))
  :config 
  (require 'smex)
  (ivy-mode t)
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)
  :init
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-re-builders-alist '( (t . ivy--regex-ignore-order))))

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'full)
  :config
  (ivy-rich-mode 1))


(use-package counsel
  :ensure t
  :demand t
  :bind (("C-x C-f" . counsel-find-file)
		 ("C-x M-f" . counsel-recentf)
		 ("M-x" . counsel-M-x)				
		 ("C-c u" . counsel-unicode-char)
		 ("C-c s" . counsel-rg)
		 ("M-s" . counsel-imenu))
  :config
  ;;(setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) " ")
  ;; enable opening file as sudo
  (defadvice counsel-find-file (after find-file-sudo activate)
    "Find file as root if necessary."
    (when (and buffer-file-name
			   (file-exists-p buffer-file-name)
			   (not (file-writable-p buffer-file-name)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (ivy-configure 'counsel-M-x
    :initial-input ""))



(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))
