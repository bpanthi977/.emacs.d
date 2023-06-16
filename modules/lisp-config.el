;;Common Lisp
(require 'cl)
;;; Use spaces insted of tabs for lisp family languages
(add-hook 'lisp-mode-hook (lambda ()
                            (setq-local indent-tabs-mode nil)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-local indent-tabs-mode nil)))

(defvar bp/lisp-ide 'sly) ;; 'sly or 'slime

(defun bp/lisp-mode-hook ()
  (wolfe/pretty-symbol-push-default)
  (push '("defun"    . ?ƒ) prettify-symbols-alist)
  (push '("defmacro" . ?μ) prettify-symbols-alist)
  (push '("defvar"   . ?ν) prettify-symbols-alist)
  (prettify-symbols-mode)
  (setf fill-column 100))

(defun bp/log4cl ()
    (interactive)
    (load "/mnt/Data/Dev/lisp/quicklisp/log4slime-setup.el")
    (global-log4slime-mode 1))

(defun cl-services ()
  (interactive)
  (let ((port (string-to-number (with-temp-buffer
                                  (insert-file-contents "/tmp/cl-services-swank")
                                  (buffer-string)))))
    (case bp/lisp-ide
      (sly (sly-connect "localhost" port))
      (slime (slime-connect "localhost" port)))))

(defun config-lisp ()
  (add-hook 'lisp-mode-hook #'bp/lisp-mode-hook)
  (cond ((string-equal system-type "windows-nt")
         (setf inferior-lisp-program "sbcl --core C:/Users/hp/.cache/common-lisp/core --dynamic-space-size 2560"))
        ((string-equal system-type "darwin")
         (setf inferior-lisp-program "sbcl --dynamic-space-size 6560"))
        (t
         (setf inferior-lisp-program "sbcl --core /home/bpanthi/.cache/common-lisp/core --dynamic-space-size 8560"
               common-lisp-hyperspec-root "file:///home/bpanthi/Dev/lisp/HyperSpec-7-0/HyperSpec/"))))

;; (use-package slime
;;   :ensure nil
;;   :defer t
;;   :commands (slime cl-services)
;;   :config
;;   (setf bp/lisp-ide 'slime)
;;   (define-key lisp-mode-map (kbd "M-m d d") #'slime-documentation)

;;   (setq slime-contribs '(slime-asdf slime-banner slime-fancy slime-hyperdoc
;;                                     ;;slime-repl-ansi-color
;;                                     slime-macrostep slime-mrepl slime-quicklisp
;;                                     slime-sbcl-exts slime-scratch slime-sprof
;;                                     slime-xref-browser slime-company))
;;   (defun slime-sbcl ()
;;     (interactive)
;;     (let ((inferior-lisp-program "sbcl"))
;;       (slime)))

;;   (defun slime-ccl ()
;;     (interactive)
;;     (let ((inferior-lisp-program "ccl"))
;;       (slime)))
;;   (config-lisp))

;; (use-package slime-company
;;   :ensure nil
;;   :defer t)

(use-package sly
  :ensure t
  :config
  (setf bp/lisp-ide 'sly)
  (define-key sly-mode-map (kbd "C-c M-p") #'sly-mrepl-set-package)
  (config-lisp))

(use-package lisp-markup
  :load-path "~/Dev/lisp/quicklisp/dists/quicklisp/software/markup-20210124-git/"
  :defer t)

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
  (define-key lisp-interaction-mode-map (kbd "C-j") #'newline))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima/")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(autoload 'imath-mode "imath" "Imath mode for math formula input" t)
