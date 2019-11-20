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
  :config 
  (setq smex-save-file (expand-file-name ".smex-items" savefile-dir)))


;;; Ivy
(use-package ivy :demand
  :config
  (setq ivy-use-virtual-buffers t
		ivy-count-format "%d/%d "
		ivy-re-builders-alist '( (t . ivy--regex-ignore-order)))
  (ivy-mode 1))

(use-package counsel :demand
  :bind (("C-x C-f" . counsel-find-file)
		 ("M-x" . counsel-M-x)				
		 ("C-c u" . counsel-unicode-char)
		 ("C-c s" . counsel-rg))
  :config
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) " ")
  ;; enable opening file as sudo
  (defadvice counsel-find-file (after find-file-sudo activate)
    "Find file as root if necessary."
    (when (and buffer-file-name
			   (file-exists-p buffer-file-name)
			   (not (file-writable-p buffer-file-name)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))))

(use-package swiper :demand
  :bind ("C-s" . swiper))
