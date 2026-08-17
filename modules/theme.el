(use-package spacemacs-theme
  :ensure t
  :init
  (load-theme 'spacemacs-dark))

(require 'ansi-color)
(require 'magit-diff)

(custom-set-faces
 '(default ((t (:height 130
		:width normal
		:foundry "ADBO" 
		:family "Source Code Pro"))))
 '(ansi-color-bright-blue
   ((t (:foreground "#4f97d7"
        :background "#4f97d7")))))

(dolist (mapping
	 '((diff-added . magit-diff-added)
	   (diff-indicator-added . magit-diff-added)
	   (diff-refine-added . magit-diff-added-highlight)
	   (diff-removed . magit-diff-removed)
	   (diff-indicator-removed . magit-diff-removed)
	   (diff-refine-removed . magit-diff-removed-highlight)))
  (set-face-attribute (car mapping) nil
		      :inherit (cdr mapping)
		      :foreground 'unspecified
		      :background 'unspecified))
