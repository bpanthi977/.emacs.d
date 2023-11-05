(require 'cl)
(setq exec-path (append '("/home/bpanthi/.local/bin") exec-path))

(setq epa-pinentry-mode 'loopback) ;; See: [[id:C896DC59-2D11-4ADC-859F-54E4721C3B3A][Configuring GPG for Emacs in MacOS]]

(use-package emojify
  :ensure t
  :defer t)

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
(setq-default tab-always-indent 't)
(setq-default tab-width 8)
(setq tab-width 8)

;; Enable subword mode so that forward-word, kill-word, ... stop
;; at sub word boundaries like PascaCase or camelCase
(global-subword-mode 1)

(setq-default indent-tabs-mode t)
;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)
(show-paren-mode t)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; save point position in buffers
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
(save-place-mode t)


;; save recent files
(use-package recentf
  :ensure t
  :demand t
  :init
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; highlight the current line
;; (global-hl-line-mode t)
;; Don't save last deleted region or rectangle to register 0
(setf cua-delete-copy-to-register-0 nil)
;; Truncate lines
(setq-default truncate-lines t)

;; Tex Input
(setf default-input-method "TeX") ;; You can toggle this with C-\

;;smart pairing for all
(use-package smartparens
  :ensure t
  :defer nil
  :config
  :bind (;; Movement [see: https://ebzzry.io/en/emacs-pairs/]
         ( "C-M-f" . sp-forward-sexp)
         ( "C-M-b" . sp-backward-sexp)
         ( "C-M-a" . sp-beginning-of-sexp)
         ( "C-M-e" . sp-end-of-sexp)

         ;; transpose, kill, copy
         ( "C-M-t" . sp-transpose-sexp)
         ( "C-M-k" . sp-kill-sexp)
         ( "C-M-w" . sp-copy-sexp)

         ;; wrapping, unwraping
         ( "M-(" . sp-wrap-round)
         ( "M-[" . sp-wrap-square)
         ( "M-\"" . sp-wrap-quote)
         ( "M-<backspace>" . sp-backward-unwrap-sexp)

         ;; slurping and barfing
         ( "C-<right>" . sp-forward-slurp-sexp)
         ( "C-<left>" . sp-forward-barf-sexp)

         ;; ( "M-D" . sp-splice-sexp)
         ;; ( "C-M-<delete>" . sp-splice-sexp-killing-forward)
         ;; ( "C-M-<backspace>" . sp-splice-sexp-killing-backward)
         ;; ( "C-S-<backspace>" . sp-splice-sexp-killing-around)

         ;; ( "C-]" . sp-select-next-thing-exchange)
         ;; ( "C-<left_bracket>" . sp-select-previous-thing)
         ;; ( "C-M-]" . sp-select-next-thing)

         ;; ;;		 ( "M-F" . sp-forward-symbol)
         ;; ;;		 ( "M-B" . sp-backward-symbol)

         ;; ( "C-\"" . sp-change-inner)
         ( "M-i" . sp-change-enclosing))
  :hook ((prog-mode . smartparens-mode))
  :init
  (require 'smartparens-config)
  (smartparens-global-strict-mode -1)
  (defun sp-wrap-quote ()
    (interactive)
    (sp-wrap-with-pair "\""))
  (sp-with-modes '(org-mode) ;; Source: https://tasshin.com/blog/implementing-a-second-brain-in-emacs-and-org-mode/
    (sp-local-pair "*" "*"))
  (setq sp-autoskip-closing-pair nil)
  (setq sp-hybrid-kill-entire-symbol nil))

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
  :defer nil
  :config
  (defun bp/projectile-find-common-lisp-root (dir)
    (if (directory-files dir nil ".asd$")
        dir
      (let ((parent (file-name-directory (directory-file-name dir))))
        (unless (or (string-match locate-dominating-stop-dir-regexp dir)
                    (equal dir parent))
          (bp/projectile-find-common-lisp-root parent)))))

  (push #'bp/projectile-find-common-lisp-root projectile-project-root-functions)
  (pushnew "*\\node_modules" projectile-globally-ignored-directories)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-cache-file (concat init-dir ".cache/projectile.cache")
        projectile-known-projects-file (concat savefile-dir "/projectile-bookmarks.eld"))
  (projectile-load-known-projects)

  :init
  (defun bp/projectile-for-all (thunk)
    (interactive)
    (dolist (b (projectile-project-buffers))
       (with-current-buffer b
         (ignore-errors (funcall thunk)))))

  (defun bp/projectile-whitespace-cleanup ()
    (interactive)
    (dolist (b (projectile-project-buffers))
      (with-current-buffer b
        (ignore-errors (whitespace-cleanup)))))
  (projectile-mode 1)
  :bind-keymap ("M-P" . projectile-command-map))

(use-package helm-projectile
  :ensure t
  :init
  ;; (setq helm-projectile-fuzzy-match nil)
  (helm-projectile-on))

;; (use-package counsel-projectile
;;   :ensure t
;;   :defer t
;;   :config
;;   (counsel-projectile-mode 1))

;; Ace Window
(use-package ace-window
  :ensure t
  :init
  (require 'winner)
  (winner-mode t)
  (setf aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

  (setq treemacs-is-never-other-window t)

  (smartrep-define-key bp/global-prefix-map
      "w"
    '(("o" . other-window)
      ("u" . evil-window-up)
      ("d" . evil-window-down)
      ("w" . ace-window)
      ("D" . delete-window)
      ("k" . kill-buffer-and-window)
      ("d" . ace-delete-window)
      ("u" . winner-undo)
      ("r" . winner-redo)))


  (bind-keys :map bp/global-prefix-map
           ("w n" . make-frame)))

(use-package treemacs
  :ensure t
  :init
  (bind-keys :map bp/global-prefix-map
           ("w T" . treemacs)
           ("w t" . treemacs-select-window)))

(use-package yasnippet
  :ensure t
  :defer nil
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

(use-package yasnippet-snippets
  :ensure t
  :defer nil)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.md\\'"
  :config
  (setf markdown-command "/home/bpanthi/.local/bin/pandoc -f gfm"))

(global-set-key (kbd "M-.") 'find-function-at-point)

;; Tramp
(setq tramp-persistency-file-name (concat init-dir "cache/tramp"))
;; Abbrev
(setq abbrev-file-name (concat savefile-dir "/abbrev_defs"))

(use-package wgrep
  :ensure t
  :defer nil)

;; Avy
(use-package avy
  :ensure t
  :defer t
  :bind (:map bp/global-prefix-map
              ("g g" . bp/avy-goto-char-end-timer)
              ("g c" . avy-goto-char)
              ("g s" . avy-goto-char-timer)
              ("g l" . avy-goto-line)
              ("j" . bp/avy-goto-char-end))
  :config
  (defun bp/avy-goto-char-end-timer ()
    (interactive)
    (when (avy-goto-char-timer)
      (forward-char 1)))

  (defun bp/avy-goto-char-end (char &optional arg)
    (interactive (list (read-char "char: " t)
                       current-prefix-arg))
    (when (avy-goto-char char arg)
      (forward-char 1)))

  (global-set-key (kbd "M-SPC") #'bp/avy-goto-char-end)
  (global-set-key (kbd "M-S-SPC") #'avy-goto-char-timer)
  (setf avy-timeout-seconds 0.3))

(use-package god-mode
  :ensure t
  :defer t
  :bind (("M-q" . god-mode-all)
         :map bp/global-prefix-map
         ("q" . god-local-mode))
  :config
  (define-key god-local-mode-map (kbd ".") #'repeat)

  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'bar
                        'box)))

  (add-hook 'god-mode-enabled-hook 'my-update-cursor )
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

;; This interfered with company completion display
;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (fci-mode t))

;; (use-package unicode-math-input
;;   :ensure t
;;   :defer t)


;; spell checking
(use-package ispell
  :ensure t
  :defer t
  :commands (ispell ispell-buffer)
  :init
  (setq ispell-program-name "hunspell")
  (bind-keys :map bp/global-prefix-map
             ("e i i" . ispell)
             ("e i w" . ispell-word)
             ("e i c" . ispell-continue)))

(use-package flyspell
  :ensure t
  :demand nil
  :config
  (setq flyspell-issue-message-flag nil))

(use-package multiple-cursors
  :ensure t
  :defer t
  :commands (mc/mark-previous-like-this mc/mark-next-like-this)
  :init
  (smartrep-define-key bp/global-prefix-map
      "m"
    '(("p" . mc/mark-previous-like-this)
      ("n" . mc/mark-next-like-this)
      ("0" . mc/insert-numbers)
      ("a" . mc/insert-letters))))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C--" . er/contract-region)
         ("C-=" . er/expand-region)))

(use-package unfill
  :ensure t
  :bind (:map bp/global-prefix-map
              ("e u" . unfill-paragraph))
  :commands (unfill-paragraph unfill-toggle unfill-region))

(add-hook 'text-mode-hook (lambda ()
                            (visual-line-mode t)))


;; (use-package origami
;;   :ensure t
;;   :defer t
;;   :init
;;   (smartrep-define-key bp/global-prefix-map ";"
;;      '((";" . origami-recursively-toggle-node)
;;        ("a" . origami-show-only-node)
;;        ("o" . origami-open-all-nodes)
;;        ("c" . origami-close-all-nodes)
;;        ("u" . origami-undo)
;;        ("r" . origami-redo))))

;; (use-package discover
;;   :ensure t
;;   :demand t
;;   :init
;;   (global-discover-mode 1))

;; Dired
(use-package dired
  :config
  (setq dired-listing-switches "-alh")
  (setf dired-dwim-target t)
  (defun dired-open-file ()
    "In dired, open the file named on this line."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (case system-type
        (darwin (call-process "open" nil 0 nil file))
        (t (call-process "xdg-open" nil 0 nil file)))))

  (define-key dired-mode-map (kbd "C-c o") 'dired-open-file))

(use-package outshine
  :ensure t
  :defer t)

;; (use-package hyperbole
;;   :ensure t
;;   :defer nil
;;   :config
;;   (require 'hyperbole)
;;   (global-set-key (kbd "<S-down-mouse-1>") #'action-key-depress-emacs)
;;   (global-set-key (kbd "<S-mouse-1>") #'action-mouse-key-emacs)
;;   (global-set-key (kbd "<C-S-down-mouse-1>") #'mouse-appearance-menu) )


(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package eyebrowse
  :ensure t
  :demand t
  :config
  (eyebrowse-mode t)
  (smartrep-define-key bp/global-prefix-map
      "w"
    '(("." . eyebrowse-switch-to-window-config)
      ("0" . eyebrowse-switch-to-window-config-0)
      ("1" . eyebrowse-switch-to-window-config-1)
      ("2" . eyebrowse-switch-to-window-config-2)
      ("3" . eyebrowse-switch-to-window-config-3)
      ("4" . eyebrowse-switch-to-window-config-4)
      ("5" . eyebrowse-switch-to-window-config-5)
      ("6" . eyebrowse-switch-to-window-config-6)
      ("7" . eyebrowse-switch-to-window-config-7)
      ("8" . eyebrowse-switch-to-window-config-8)
      ("9" . eyebrowse-switch-to-window-config-9)
      ("c" . eyebrowse-create-named-window-config)
      ("," . eyebrowse-rename-window-config)
      (">" . eyebrowse-next-window-config)
      ("<" . eyebrowse-prev-window-config)
      ("p" . eyebrowse-last-window-config))))
