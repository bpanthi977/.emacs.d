;; Always load newest byte code
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(setq source-directory "/home/bpanthi/Dev/emacs")
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
(add-to-list 'load-path (expand-file-name "git-clones/org-roam/" modules-dir))


;;; taken from Doom emacs 
;; reduce the frequency of garbage collection by making it happen on
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshediold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defvar windows-system? (if (or (string-equal system-name "windows/nt")
			   (string-equal system-type "windows-nt"))
		       t nil))


;;Load custom file
(setf custom-file (expand-file-name "custom.el" init-dir))
(if (file-exists-p custom-file)
    (load-file custom-file))

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
   "programming-config"
   "make-config"
   "c-config"
   "csharp-config"
   "okular"
   "org-config"
   "elfeed-config"
   "org-mpv-notes"
   "org-web-notes"
   "org-calibre-notes"
   ;; "java-config"
   ;; "android-config"
   "python-config"
   ;; "go-config"

   "web-config"
   "latex-config"
   "js-config"
   "flutter-config"
   "tools"
   ))

;; Load the modules
(message "Loading modules")
(dolist (module load-modules-list)
   (message (format "loading %s" module))
   (load module))

(unless windows-system?
  (load "linux-config"))

;;Start Server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(setq gc-cons-threshold 16777216
	  gc-cons-percentage 0.1
	  file-name-handler-alist last-file-name-handler-alist)


(put 'downcase-region 'disabled nil)
