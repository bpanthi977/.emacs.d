;; Ivy replaces ido
;; Counsel replaces smex


;; (use-package ido
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
;; 	ido-auto-merge-work-directories-length -1)
;;   :init 
;;   (ido-mode +1))

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

;;; smex, remember recently and most frequently used commands
;; (use-package smex
;;   :bind (("M-x" . smex)
;; 	 ("M-X" . smex-major-mode-commands))
;;   :config 
;;   (setq smex-save-file (expand-file-name ".smex-items" savefile-dir))
;;   (smex-initialize))

;;; Ivy
(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  :init
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)				
	 ("C-c u" . counsel-unicode-char)
	 ("C-c s" . counsel-rg)))

(use-package swiper
  :bind ("C-s" . swiper))

(setf use-package-always-ensure t)
