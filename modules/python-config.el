(defun bp/python-mode-hook ()
  ;; helm-dash
  (helm-dash-activate-docset "Python_3")
  ;(helm-dash-activate-docset "OpenCV_C++")

  ;; pretty symbols
  (wolfe/pretty-symbol-push-default)
  (push '("def"    . ?ƒ) prettify-symbols-alist)
  (push '("sum"    . ?Σ) prettify-symbols-alist)
  (push '("**2"    . ?²) prettify-symbols-alist)
  (push '("**3"    . ?³) prettify-symbols-alist)
  (push '("None"   . ?∅) prettify-symbols-alist)
  (push '("in"     . ?∈) prettify-symbols-alist)
  (push '("not in" . ?∉) prettify-symbols-alist)
  (push '("return" . ?➡) prettify-symbols-alist)
  (prettify-symbols-mode t))

(add-hook 'python-mode-hook #'bp/python-mode-hook)

(use-package lsp-pyright
  :ensure t
  :defer nil)

(use-package ein
  :ensure t
  :config
  (setf ein:output-area-inlined-images t))

;; for use in ein
;; use (elpy-enable) to enable it
(use-package elpy
  :ensure t)
