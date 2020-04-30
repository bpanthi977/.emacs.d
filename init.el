;; Always load newest byte code
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq load-prefer-newer t)
(setq debug-on-error t)
(defvar init-dir (file-name-directory load-file-name)
  "The root dir of Emacs config")
(defvar modules-dir (expand-file-name "modules" init-dir)
  "This directory stores all modules")
(defvar savefile-dir (expand-file-name "savefile" init-dir)
  "This stores savefiles like smex,recentf,...")

(add-to-list 'load-path modules-dir)
(add-to-list 'load-path (expand-file-name "my-packages" modules-dir))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;; Declare modules to load 
(defvar load-modules-list
  (list
   "packages-config"
   "keymaps-config"
   "company-config"
   "ido-config"
   "editor-config"
   "gui-config"
   "lisp-config"
  
   "make-config"
   "c-config"
   "csharp-config"
   "org-config"
   ;; "java-config"
   ;; "android-config"
   "python-config"
   ;; "go-config"
   "lsp-config"
   "web-config"
   "latex-config"
   "js-config"
   "tools"
   ))

;; Load the modules
(message "Loading modules")
(dolist (module load-modules-list)
   (message (format "loading %s" module))
   (load module))

;;Load custom file
;;(setf custom-file (expand-file-name "custom.el" init-dir))
;;(load-file (expand-file-name "custom.el" init-dir))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;Start Server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete t t)
 '(company-auto-complete-chars "." t)
 '(company-frontends
   (quote
	(company-pseudo-tooltip-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-echo-frontend)) t)
 '(company-idle-delay 0.1 t)
 '(company-minimum-prefix-length 2 t)
 '(company-quickhelp-color-background "#4F4F4F" t)
 '(company-quickhelp-color-foreground "#DCDCCC" t)
 '(company-require-match nil t)
 '(company-tooltip-flip-when-above t t)
 '(company-tooltip-limit 10 t)
 '(helm-dash-docsets-path "/home/bpanthi/.local/share/Zeal/Zeal/docsets/" t)
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
	(smartrep zenburn-theme which-key web-mode virtualenvwrapper use-package unicode-math-input unfill tern spacemacs-theme smex smartparens sly rjsx-mode pyenv-mode prettier-js powershell org-plus-contrib org-download omnisharp multiple-cursors magit macrostep lsp-ui lsp-python-ms ivy-rich helm-dash god-mode gnuplot-mode gnuplot expand-region evil esup elisp-slime-nav eglot edit-server counsel-projectile company-web company-rtags company-quickhelp company-jedi cmake-mode chyla-theme ccls auctex ace-window ac-php))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
