(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
	 ("C-x O" . bp/ace-window-global))
  :config
  (defun bp/ace-window-global (arg)
    (interactive "p")
    (let ((aw-scope 'global))
      (ace-window arg)))

  (setq aw-scope 'frame)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
