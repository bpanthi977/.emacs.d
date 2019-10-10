(use-package android-mode
  :config 
  (setf android-mode-sdk-dir "/home/bpanthi/Apps/android-sdk-linux"
	android-mode-sdk-tool-subdirs (list "tools")
	android-mode-built-command-alist '((ant . "ant -e")
					 (maven . "mvn")
					 (gradle . "gradle")))
  :bind (:map android-mode-map
	      ("C-c C-c" . android-gradle-test)
	      ("C-c C-f" . android-gradle-installDebug)
	      ("C-c C-r" . android-gradle-installDebug)))


 

