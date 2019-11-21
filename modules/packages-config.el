;;
;; Packages
;;
(require 'package)
;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(if (eq system-type 'windows-nt)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

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

(when (string-equal (nth 0 command-line-args) "/usr/bin/emacs")
  (setf use-package-always-demand t))

(unless (require 'use-package nil t)
  (package-refresh-contents nil)
  (require-packages '(use-package)))


