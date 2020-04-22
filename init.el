;; Always load newest byte code

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq load-prefer-newer t)

(defvar init-dir (file-name-directory load-file-name)
  "The root dir of Emacs config")
(defvar modules-dir (expand-file-name "modules" init-dir)
  "This directory stores all modules")
(defvar savefile-dir (expand-file-name "savefile" init-dir)
  "This stores savefiles like smex,recentf,...")

(add-to-list 'load-path modules-dir)
(add-to-list 'load-path (expand-file-name "packages" modules-dir))

;; reduce the frequency of garbage collection by making it happen on
;; each 20MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 20000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;; Declare modules to load 
(defvar load-modules-list
  (list
   "packages-config"
   "company-config"
   "ido-config"
   "editor-config"
   "gui-config"
   "lisp-config"
   "keymaps-config"
   "make-config"
   "c-config"
   "csharp-config"
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

;; Load custom file
(setf custom-file (expand-file-name "custom.el" init-dir))
(load-file (expand-file-name "custom.el" init-dir))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;Start Server
(require 'server)
(unless (server-running-p)
  (server-start))

