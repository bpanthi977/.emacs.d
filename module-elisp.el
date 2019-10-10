(use-package elisp-slime-nav
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode)
  ;; Enable slime like M-. navigation in elisp source
  :bind (:map emacs-lisp-mode-map 
	      ("C-c C-c" . eval-defun)))
