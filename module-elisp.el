(require-packages (list 'elisp-slime-nav))

;; Enable slime like M-. navigation in elisp source
(dolist (hook '(emacs-lisp-mode ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
			     (local-set-key (kbd "C-c C-c") #'eval-defun)))
