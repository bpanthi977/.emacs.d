;; Line-number of left margin
;;(global-linum-mode nil)
;;(add-hook 'text-mode-hook 'linum-mode)
;;(add-hook 'prog-mode-hook 'linum-mode)

;; Tool bar and menu bar are unnecessary
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; mode line settings
(line-number-mode nil)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " " (:eval (if (buffer-file-name)
                                          (abbreviate-file-name (buffer-file-name))
                                        "%b"))))

;; show available keybindings after you start typing
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(require 'calendar)
(defun load-todays-theme ()
  (let* ((theme-list '((spacemacs-theme spacemacs-dark)
                       (solarized-theme solarized-zenburn)
                       (monokai-pro-theme monokai-pro
                                          '(set-face-foreground font-lock-variable-name-face "light sea green"))
                       (solarized-theme solarized-dark)
                       (color-theme-sanityinc-tomorrow sanityinc-tomorrow-eighties)
                       (solarized-theme solarized-gruvbox-dark)
                       (color-theme-sanityinc-tomorrow sanityinc-tomorrow-night)
                       (zenburn-theme zenburn)))
         (n (length theme-list))
         (day (calendar-day-number (calendar-current-date)))
         theme)
    (setf day (mod day n))
    (setf theme (nth day theme-list))
    (load-theme (nth 1 theme) t)
    (if (third theme)
        (eval (third theme)))))

;;(load-todays-theme)
(unless (package-installed-p 'spacemacs)
  (package-install 'spacemacs-theme))

;;; when emacsclient is started by systemd graphics mode is set
;;; to terminal and thus spacemacs loads a non true-color theme
;;; which looks dull.
;;; Here I load spacemacs-common file where #'ture-color-p is
;;; defined, then temporarily set it to return t and load
;;; spacemacs-dark-theme
(let ((true-color-p* (symbol-function 'true-color-p)))
  (require 'spacemacs-common)
  (setf (symbol-function 'true-color-p) (lambda () t))
  (unwind-protect (progn
                    (message "loading theme")
                    (message "ture color p %s" (true-color-p))
                    (load-theme 'spacemacs-dark))
    (setf (symbol-function 'true-color-p) true-color-p*)))

(defun wolfe/pretty-symbol-push-default ()
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("=>" . ?⇒) prettify-symbols-alist))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (wolfe/pretty-symbol-push-default)
            (push '("defun"    . ?ƒ) prettify-symbols-alist)
            (push '("defmacro" . ?μ) prettify-symbols-alist)
            (push '("defvar"   . ?ν) prettify-symbols-alist)
            (prettify-symbols-mode t)))

;; (use-package zenburn-theme
;;   :ensure t
;;   :demand
;;   :config
;;   (load-theme 'zenburn t))

;; Symbola font does the job for emoji
;; t = for default fontset , nil = for character not found in other fonts
;; (font-sp.. ) = use Symbola font
;; (set-fontset-font t nil (font-spec :family "Noto Color Emoji"))
(set-fontset-font t 'symbol "Noto Color Emoji" nil)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil
                         ;;:background "#3F3F3F" :foreground "#DCDCCC"
                         :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal ;; :height 90
                         :width normal :foundry "ADBO" :family "Source Code Pro")))))
