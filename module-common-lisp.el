(use-package slime
  :config 
  (setq inferior-lisp-program "/usr/bin/sbcl --core /home/bpanthi/.cache/common-lisp/core" ;; --dynamic-space-size 2560"
	slime-contribs '(slime-asdf slime-banner slime-fancy slime-hyperdoc slime-macrostep slime-mrepl slime-quicklisp slime-sbcl-exts slime-scratch slime-sprof slime-xref-browser slime-company)
	common-lisp-hyperspec-root "/home/bpanthi/HyperSpec-7-0/HyperSpec/")

  (defun view-sdl-doc ()
    (interactive)
    (browse-url "/home/bpanthi/software/lispbuilder-20160825-git/lispbuilder-sdl/documentation/lispbuilder-sdl.html"))
  
  :if (string-equal system-type "windows-nt")
  :config 
  (setq inferior-lisp-program "sbcl"
	common-lisp-hyperspec-root "d:/home/bpanthi/HyperSpec-7-0/HyperSpec/"))



