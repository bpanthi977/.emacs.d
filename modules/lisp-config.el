;;Common Lisp


(use-package slime
  :ensure t
  :commands (slime)
  :hook (( slime-mode . (lambda ()
			  (wolfe/pretty-symbol-push-default)
			  (push '("defun"    . ?ƒ) prettify-symbols-alist)
			  (push '("defmacro" . ?μ) prettify-symbols-alist)
			  (push '("defvar"   . ?ν) prettify-symbols-alist)
			  (prettify-symbols-mode)
			  (setf fill-column 100)
			  (add-to-list 'company-backends 'company-slime))))
  :config
  (defun bp/log4cl () 
    (interactive)
    (load "/mnt/Data/Dev/lisp/quicklisp/log4slime-setup.el")
    (global-log4slime-mode 1))
    ;;; To save new core run the script at
  ;;; ~/Development/lisp/save-new-core.lisp
  (cond ((string-equal system-type "windows-nt")
	 (setf inferior-lisp-program "sbcl --core C:/Users/hp/.cache/common-lisp/core" ;; --dynamic-space-size 2560
	       ))
	(t
	 (setf inferior-lisp-program "sbcl --core /home/bpanthi/.cache/common-lisp/core"
	       common-lisp-hyperspec-root "file:///home/bpanthi/Dev/lisp/HyperSpec-7-0/HyperSpec/")))

  (setq	slime-contribs '(slime-asdf slime-banner slime-fancy slime-hyperdoc slime-macrostep slime-mrepl slime-quicklisp slime-sbcl-exts slime-scratch slime-sprof slime-xref-browser slime-company))

  ;; (defun view-sdl-doc ()
  ;;   (interactive)
  ;;   (eww-open-file "~/Dev/lisp/quicklisp/dists/quicklisp/software/lispbuilder-20180831-git/lispbuilder-sdl/documentation/lispbuilder-sdl.html"))
  ;; )

  ;; (when  (string-equal system-type "windows-nt")
  ;;   (setq inferior-lisp-program "sbcl")))

  (defun cl-services () 
    (interactive)
    (let ((port (string-to-number (with-temp-buffer 
				    (insert-file-contents "/tmp/cl-services-swank")
				    (buffer-string)))))
      (slime-connect "localhost" port)))
  )


;; (use-package slime-company
;;   :ensure t)


;; (use-package sly
;;   :defer t 
;;   :ensure t
;;   ;; :requires (smartparens)
;;   :commands (sly)
;;   :bind (:map sly-prefix-map
;; 	      ("M-p" . sly-mrepl-set-package)
;; 	      :map lisp-mode-map
;; 	      ("M-m d s" . sly-documentation))
;;   :hook (lisp-mode . (lambda ()
;; 		       (wolfe/pretty-symbol-push-default)
;; 		       (prettify-symbols-mode)))
;;   :config

;;   ;;; To save new core run the script at
;;   ;;; ~/Development/lisp/save-new-core.lisp
;;   (cond ((string-equal system-type "windows-nt")
;; 	 (setf inferior-lisp-program "sbcl --core C:/Users/hp/.cache/common-lisp/core" ;; --dynamic-space-size 2560
;; 	       ))
;; 	(t
;; 	 (setf inferior-lisp-program "sbcl --core /home/bpanthi/.cache/common-lisp/core"
;; 	       common-lisp-hyperspec-root "file:///home/bpanthi/Dev/lisp/HyperSpec-7-0/HyperSpec/")))
;;   :init 
;;   (defun sly-select-core ()
;;     (interactive)
;;     (let* ((dir (if windows-system?
;; 		    "C:/Users/hp/.cache/common-lisp/"
;; 		  "/home/bpanthi/.cache/common-lisp/"))
;; 	   (files (directory-files dir nil "core"))
;; 	   (core (completing-read "Core: " files)))
;;       (when core
;; 	(setf inferior-lisp-program (format "sbcl --core %s%s" dir core))
;; 	(sly))))
;;   )

;;  (setf inferior-lisp-program "clisp"))
;; (setf inferior-lisp-program "sbcl --core c:/Users/hp/lack-core"))
;; :hook ((mrepl-mode . smartparens-mode)))



;; Emacs Lisp
(use-package elisp-slime-nav
  :ensure t
  :defer t 
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode)
  :hook (emacs-lisp-mode . (lambda ()
			     (wolfe/pretty-symbol-push-default)
			     (push '("defun"    . ?ƒ) prettify-symbols-alist)
			     (push '("defmacro" . ?μ) prettify-symbols-alist)
			     (push '("defvar"   . ?ν) prettify-symbols-alist)
			     (prettify-symbols-mode)))
  ;; Enable slime like M-. navigation in elisp source
  :bind (:map emacs-lisp-mode-map 
	      ("C-c C-c" . eval-defun)
	      ("C-c C-l" . eval-buffer)))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
