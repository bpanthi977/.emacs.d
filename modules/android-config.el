(use-package android-mode
  :custom
  (android-mode-build-command-alist
   (quote
    ((ant . "ant -e")
     (gradle . "gradle")
     (maven . "mvn"))))
  (android-mode-built-command-alist
   (quote
    ((ant . "ant -e")
     (maven . "mvn")
     (gradle . "gradle"))))
  (android-mode-sdk-dir "/home/bpanthi/Apps/android-sdk-linux")
  (android-mode-sdk-tool-subdirs (list "tools"))
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


 

