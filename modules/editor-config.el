;; (require-packages (list 'smartparens
;; 			'smartparens-config
;; 			'magit
;; 			'recentf
;; 			'projectile
;; 			'uniquify
;; 			'ace-window
;; 			'saveplace))

;; turn on CUA-mode globally
(cua-mode t)
;; Newline at end of file
(setq require-final-newline t)
;; delete the selection with a keypress
(delete-selection-mode t)
;; Backup and autosave files
(setq backup-directory-alist
      `(("." . ,(concat savefile-dir "/backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix
      (concat savefile-dir "/auto-save-list/.saves-"))
;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; smart tab behavior - indent or complete
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; meaningful names for buffers with the same name
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; save point position in buffers
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
(setq-default save-place t)

;; save recent files
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)
(recentf-mode +1)
;; highlight the current line
(global-hl-line-mode +1)

;;smart pairing for all
(use-package smartparens
  :demand
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (show-smartparens-global-mode t)
  :bind (("C-M-f" . sp-forward-slurp-sexp)
	 ("C-M-b" . sp-backward-slurp-sexp))
  :hook ((prog-mode . smartparens-mode)))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Projectile
(use-package projectile
  :ensure t
  :config 
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-cache-file (concat init-dir "cache/projectile.cache")
	projectile-known-projects-file (concat savefile-dir "/projectil-bookmarks.eld"))
  :bind-keymap ("M-P" . projectile-command-map))

(use-package counsel-projectile
  :ensure t 
  :config
  (counsel-projectile-mode 1))

;; Ace Window
(use-package ace-window
  :ensure t
  :bind ("C-c o" . ace-window)
  :config
  (setf aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package yasnippet
  :ensure t
  :preface
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))

  :after (yasnippet-snippets company-yasnippet)
  :config
  (add-to-list 'company-backends 'company-yasnippet)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package markdown-mode
  :ensure t 
  :mode "\\.md\\'"
  :config 
  (setf markdown-command "/home/bpanthi/.local/bin/pandoc -f gfm"))

(global-set-key (kbd "M-.") 'find-function-at-point)

;; Tramp
(setq tramp-persistency-file-name (concat init-dir "cache/tramp"))
;; Abbrev 
(setq abbrev-file-name (concat savefile-dir "/abbrev_defs"))
;; Magit
(custom-set-variables `(transient-history-file ,(concat savefile-dir "/transient/history.el")))
;; Avy
(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
	 ("M-g C-c" . avy-goto-char)
	 ("M-s" . avy-goto-char-timer)
	 ("M-g C-g" . avy-goto-line))
  :config
  (setf avy-timeout-seconds 0.3))

(use-package god-mode
  :ensure t
  :bind (("C-SPC" . god-local-mode)
	 ("C-q" . god-local-mode)
	 ("M-q" . god-mode-all)))

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'bar
                      'box)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor )
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (haskell . nil)
   (latex . t)
   (ledger . t)         ;this is the important one for this tutorial
   (ocaml . nil)
   (octave . t)
   (python . t)
   (ruby . t)
   (screen . nil)
   (sql . nil)
   (sqlite . t)))

(use-package multiple-cursors
  :ensure t
  :demand t)
