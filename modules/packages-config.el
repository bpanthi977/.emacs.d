;;
;; Packages
;;
(require 'package)
;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(if (eq system-type 'windows-nt)
    (progn (add-to-list 'package-archives
			'("melpa" . "http://melpa.org/packages/") t)		   
	   (add-to-list 'package-archives
			'("org" . "http://orgmode.org/elpa/") t))
  (progn (add-to-list 'package-archives
		      '("melpa" . "https://melpa.org/packages/") t)
	 (add-to-list 'package-archives
		      '("org" . "https://orgmode.org/elpa/") t)))
(package-initialize)

(defun install-packages (package-list)
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun require-packages (package-list)
  (dolist (package package-list)
    (unless (require package nil t)
      (package-install package)
      (require package))))

(unless (require 'use-package nil t)
  (package-refresh-contents nil)
  (require-packages '(use-package)))

(setq use-package-verbose t)

