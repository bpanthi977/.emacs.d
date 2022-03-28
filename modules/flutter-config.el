(use-package lsp-dart
  :ensure t
  :defer t
  :config
  (require 'dart-mode)
  (define-key dart-mode-map (kbd "C-j") #'newline)
  (define-key dart-mode-map (kbd "M-m d d") #'lsp-ui-doc-glance)
  (define-key dart-mode-map (kbd "M-m d D") #'lsp-ui-doc-show)

  (setq lsp-dart-flutter-widget-guides nil
	lsp-dart-flutter-outline nil
	lsp-dart-outline nil
	lsp-dart-closing-labels nil
	lsp-dart-suggest-from-unimported-libraries t)

  (setq lsp-dart-flutter-sdk-dir "/home/bpanthi/Apps0/flutter/")
  (setq exec-path (append (list (concat lsp-dart-flutter-sdk-dir "bin")) exec-path))
  (setenv "PATH" (concat lsp-dart-flutter-sdk-dir "bin:" (getenv "PATH")))
  (setf lsp-dart-sdk-dir (concat lsp-dart-flutter-sdk-dir "bin/cache/dart-sdk/"))

  (require 'helm-dash)
  (add-hook 'dart-mode-hook (lambda ()
			      (electric-indent-mode)
			      (lsp)
			      (helm-dash-activate-docset "Dart")
			      (helm-dash-activate-docset "flutter")))

  (define-key dart-mode-map (kbd "M-l") lsp-command-map)
  (define-key dart-mode-map (kbd "M-l l") #'lsp-dart-dap-flutter-hot-reload)
  (define-key dart-mode-map (kbd "M-l M-l") #'lsp-dart-dap-flutter-hot-restart)
  
  ;;; ignore `lsp-dart-flutter-daemon-current-device' value and launch device anyway 
  ;;; the first time device is launched it doesn't work 
  ;;; it was necessary to do this or run `flutter run -d emulator-5554' in the terminal from the project directory 
  (lsp-defun lsp-dart-flutter-daemon-launch ((device &as &FlutterDaemonDevice :id :is-device?) callback)
    "Launch DEVICE and wait for connected state and call CALLBACK."
    ;; (if lsp-dart-flutter-daemon-current-device
    ;; 	(funcall callback lsp-dart-flutter-daemon-current-device)
    (progn
      (setq lsp-dart-flutter-daemon-current-device device)
      (if is-device?
          (funcall callback device)
        (-let* ((params (lsp-make-flutter-daemon-emulator-launch :emulator-id id)))
          (add-to-list 'lsp-dart-flutter-daemon-device-added-listeners
		       (cons id (list :callback callback)))
          (lsp-dart-flutter-daemon--send "emulator.launch" params callback))))))


;; Optional packages
(use-package yasnippet
  :ensure t
  :config (yas-global-mode))






