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
(add-to-list 'load-path (expand-file-name "git-clones/org-mode/lisp/" modules-dir))
(add-to-list 'load-path (expand-file-name "git-clones/org-mode/contrib/lisp/" modules-dir))


;;; taken from Doom emacs 
;; reduce the frequency of garbage collection by making it happen on
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

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


(setq gc-cons-threshold 16777216
	  gc-cons-percentage 0.1
	  file-name-handler-alist last-file-name-handler-alist)

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
 '(custom-safe-themes
   (quote
	("76c5b2592c62f6b48923c00f97f74bcb7ddb741618283bdb2be35f3c0e1030e3" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "983eb22dae24cab2ce86ac26700accbf615a3f41fef164085d829fe0bcd3c236" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "d5aec3a39364bc4c6c13f472b2d0cdaebd5cff7a6e4839749be2156fcc075006" default)))
 '(helm-dash-docsets-path "/home/bpanthi/.local/share/Zeal/Zeal/docsets/" t)
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
	(company-org-roam org-roam htmlize gnuplot org-edit-latex color-theme-sanityinc-tomorrow monokai-pro-theme solarized-theme smartrep zenburn-theme which-key web-mode virtualenvwrapper use-package unicode-math-input unfill tern spacemacs-theme smex smartparens sly rjsx-mode pyenv-mode prettier-js powershell org-plus-contrib org-download omnisharp multiple-cursors magit macrostep lsp-ui lsp-python-ms ivy-rich helm-dash god-mode expand-region evil esup elisp-slime-nav eglot edit-server counsel-projectile company-web company-rtags company-quickhelp company-jedi cmake-mode chyla-theme ccls auctex ace-window ac-php)))
 '(safe-local-variable-values (quote ((org-confirm-babel-evaluate))))
 '(transient-history-file "c:/Users/hp/.emacs.d/savefile/transient/history.el"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
