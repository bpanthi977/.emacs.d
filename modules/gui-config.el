;; Line-number of left margin
(global-linum-mode 1)
(add-hook 'text-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)

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
(line-number-mode t)
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



;; Theme
(use-package zenburn-theme
  :ensure t
  :demand
  :config 
  (load-theme 'zenburn t))

;; Symbola font does the job for emoji
;; t = for default fontset , nil = for character not found in other fonts
;; (font-sp.. ) = use Symbola font
;; (set-fontset-font t nil (font-spec :family "Noto Color Emoji"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil
			 ;;:background "#3F3F3F" :foreground "#DCDCCC"
			 :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal ;; :height 90
			 :width normal :foundry "ADBO" :family "Source Code Pro")))))
