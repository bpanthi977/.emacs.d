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

(use-package lsp-python-ms
  :ensure t
  :defer t
  :after (python)
  :hook (python-mode . #'bp/python-mode-hook)		     
  :config
  (if windows-system?
      (setf lsp-python-ms-executable "c:/Users/hp/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer.exe")
    (setf lsp-python-ms-executable "~/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer")))


