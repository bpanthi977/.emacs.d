(use-package spacemacs-theme
  :ensure t
  :init
  (load-theme 'spacemacs-dark))

(require 'ansi-color)

(custom-set-faces
 '(default ((t (:height 130
		:width normal
		:foundry "ADBO" 
		:family "Source Code Pro"))))
 '(ansi-color-bright-blue
   ((t (:foreground "#4f97d7"
        :background "#4f97d7")))))
