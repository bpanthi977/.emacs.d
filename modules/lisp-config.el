;;Common Lisp
(use-package slime
  :ensure t
  :commands (slime cl-services)
  :hook (( slime-mode . (lambda ()
			  (wolfe/pretty-symbol-push-default)
			  (push '("defun"    . ?ƒ) prettify-symbols-alist)
			  (push '("defmacro" . ?μ) prettify-symbols-alist)
			  (push '("defvar"   . ?ν) prettify-symbols-alist)
			  (prettify-symbols-mode)
			  (setf fill-column 100)
			  (add-to-list 'company-backends 'company-slime))))
  :config
  (bind-keys :map lisp-mode-map
	     ("M-m d" . slime-documentation))
  
  (defun bp/log4cl () 
    (interactive)
    (load "/mnt/Data/Dev/lisp/quicklisp/log4slime-setup.el")
    (global-log4slime-mode 1))
  
  (cond ((string-equal system-type "windows-nt")
	 ;; --dynamic-space-size 2560
	 (setf inferior-lisp-program "sbcl --core C:/Users/hp/.cache/common-lisp/core"))
	(t
	 (setf inferior-lisp-program "sbcl --core /home/bpanthi/.cache/common-lisp/core"
	       common-lisp-hyperspec-root "file:///home/bpanthi/Dev/lisp/HyperSpec-7-0/HyperSpec/")))

  (setq	slime-contribs '(slime-asdf slime-banner slime-fancy slime-hyperdoc
				    slime-macrostep slime-mrepl slime-quicklisp
				    slime-sbcl-exts slime-scratch slime-sprof
				    slime-xref-browser slime-company))
  (defun cl-services () 
    (interactive)
    (let ((port (string-to-number (with-temp-buffer 
				    (insert-file-contents "/tmp/cl-services-swank")
				    (buffer-string)))))
      (slime-connect "localhost" port)))

  (defun slime-ccl ()
    (interactive)
    (let ((inferior-lisp-program "ccl"))
      (slime))))

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

(use-package elisp-mode
  :config 
  (define-key lisp-interaction-mode-map (kbd "C-j") #'electric-newline-and-maybe-indent))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
