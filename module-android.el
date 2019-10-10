(install-packages (list 'android-mode))

(defun setup-android ()
  (interactive)
  (setf android-mode-sdk-dir "/home/bpanthi/Apps/android-sdk-linux"
      android-mode-sdk-tool-subdirs (list "tools")
      android-mode-built-command-alist '((ant . "ant -e")
					 (maven . "mvn")
					 (gradle . "gradle")))
  (local-set-key (kbd "C-c C-c") #'android-gradle-test)
  (local-set-key (kbd "C-c C-f") #'android-gradle-installDebug)
  (local-set-key (kbd "C-c C-r") #'android-gradle-installDebug))

(add-hook 'android-mode-hook #'setup-android)

