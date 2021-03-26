(use-package jedi-core
  :ensure t
  :commands (jedi-mode)
  :defer t
  :hook (python-mode .
		     (lambda ()
		       (add-to-list 'company-backends 'company-jedi)
		       
		       (helm-dash-activate-docset "Python_3")
		       (helm-dash-activate-docset "OpenCV_C++")
		       
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
		       (prettify-symbols-mode t)))
  :config
  (require 'helm-dash)
  (require 'company-jedi)
  (setf python-environment-directory (concat init-dir "extra/python-environment"))
  (setq python-shell-interpreter "python")
  
  (bind-keys :map python-mode-map
	     ("C-." . helm-dash-at-point)
	     ("M-." . jedi:goto-definition)
	     ("M-," . jedi:goto-definition-pop-marker)
	     ("M-m d" . jedi:show-doc)))

(use-package company-jedi
  :ensure t
  :defer t 
  :after (company jedi-core))

(use-package lsp-python-ms
  :ensure t
  :defer t
  :after (python)
  :config
  (if windows-system?
      (setf lsp-python-ms-executable "c:/Users/hp/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer.exe")
    (setf lsp-python-ms-executable "~/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer")))


