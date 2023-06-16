;;
;; Packages
;;
(require 'package)
;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(cond ((eq system-type 'windows-nt)
       (add-to-list 'package-archives
                    '("melpa" . "http://melpa.org/packages/") t))
      (t
       (add-to-list 'package-archives
                    '("melpa" . "https://melpa.org/packages/") t)))
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

(use-package exec-path-from-shell
  :ensure t
  :defer nil
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))
;;(setf package-check-signature nil)
