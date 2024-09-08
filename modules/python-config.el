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
(setf org-babel-python-command "python3.11")

;; To Solve
;; > Warning (python): Your ‘python-shell-interpreter’ doesn’t seem to support readline, yet ‘python-shell-completion-native-enable’ was t and "python3" is not part of the ‘python-shell-completion-native-disabled-interpreters’ list.
;; > Native completions have been disabled locally.
;; > Consider installing the python package "readline".
;;; disable python3 from native completion as per https://github.com/brittAnderson/psych363Practice/issues/124
(setq python-shell-completion-native-disabled-interpreters '("python3"))

(use-package lsp-pyright
  :ensure t
  :defer nil
  :config
  (setf lsp-pyright-multi-root nil))

(use-package ein
  :ensure t
  :config
  (setf ein:output-area-inlined-images t))

;; for use in ein
;; use (elpy-enable) to enable it
(use-package elpy
  :ensure t)
