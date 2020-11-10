(use-package pdf-tools 
  :pin manual  ;; because package updates may require rebuilding the executable  
  :defer t 
  :init
  ;; NOTE: line numbering mode may hang pdf-view mode

  ;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights (Highlight using {C-c C-a h} )
  ;; (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :config 
  
  (defun bp/pdf-slight-up ()
    (interactive)
    (pdf-view-scroll-up-or-next-page 12))

  (defun bp/pdf-slight-down ()
    (interactive)
    (pdf-view-scroll-down-or-previous-page 12))
  
  (defun bp/pdf-highlight-and-take-note () 
    (interactive)
    (let ((pdf-annot-activate-created-annotations nil)
	  (strings (pdf-view-active-region-text)))
      (kill-new (apply #'concatenate 'string strings))
      (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region nil)))
    (org-noter-insert-precise-note))
  
  (bind-keys :map pdf-view-mode-map 
	     ((kbd "x") . bp/pdf-slight-up)
	     ((kbd "z") . bp/pdf-slight-down)
	     ((kbd "C-c i") . bp/pdf-highlight-and-take-note)))

(use-package dictionary 
  :ensure t 
  :defer t 
  :config 
  (setf dictionary-server "localhost"))
