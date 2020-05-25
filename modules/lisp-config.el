;;Common Lisp
;; (use-package slime
;;   :ensure t
;;   :commands (slime)
;;   :hook (( slime-mode . (lambda ()
;; 						  (setf fill-column 100)
;; 						  (add-to-list 'company-backends 'company-slime))))
;;   :config 
;;   (setq inferior-lisp-program "sbcl --core C:/Users/hp/.cache/common-lisp/core" ;; --dynamic-space-size 2560"
;; 	slime-contribs '(slime-asdf slime-banner slime-fancy slime-hyperdoc slime-macrostep slime-mrepl slime-quicklisp slime-sbcl-exts slime-scratch slime-sprof slime-xref-browser slime-company)
;; 	common-lisp-hyperspec-root "~/Dev/lisp/HyperSpec-7-0/HyperSpec/")

;;   ;; (defun slime-ecl ()
;;   ;;   (interactive)
;;   ;;   (slime "/usr/bin/ecl"))
  
;;   (defun view-sdl-doc ()
;;     (interactive)
;;     (eww-open-file "~/Dev/lisp/quicklisp/dists/quicklisp/software/lispbuilder-20180831-git/lispbuilder-sdl/documentation/lispbuilder-sdl.html"))
;;   )
  
;;   ;; (when  (string-equal system-type "windows-nt")
;;   ;;   (setq inferior-lisp-program "sbcl")))

	

;; (use-package slime-company
;;   :ensure t)


(use-package sly
  :defer t 
  :ensure t
  ;; :requires (smartparens)
  :commands (sly)
  :bind (:map sly-prefix-map
			  ("M-p" . sly-mrepl-set-package))
  :config
 (setf inferior-lisp-program "sbcl --core C:/Users/hp/.cache/common-lisp/core" ;; --dynamic-space-size 2560
  		))
;;  (setf inferior-lisp-program "clisp"))
 ;; (setf inferior-lisp-program "sbcl --core c:/Users/hp/lack-core"))
  ;; :hook ((mrepl-mode . smartparens-mode)))

;; Emacs Lisp
(use-package elisp-slime-nav
  :ensure t
  :defer t 
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode)
  ;; Enable slime like M-. navigation in elisp source
  :bind (:map emacs-lisp-mode-map 
			  ("C-c C-c" . eval-defun)
			  ("C-c C-l" . eval-buffer)))
