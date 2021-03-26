(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp)
  :config
  (setq exec-path (append '("/home/bpanthi/Apps/flutter/bin")
			exec-path))
  (setenv "PATH" (concat "/home/bpanthi/Apps/flutter/bin:"
			 (getenv "PATH")))
  (setf lsp-dart-sdk-dir "/home/bpanthi/Apps/flutter/bin/cache/dart-sdk/"
	lsp-dart-flutter-sdk-dir "/home/bpanthi/Apps/flutter/")
  (require 'helm-dash)
  (add-hook 'dart-mode-hook (lambda ()
			      (setq-local helm-dash-common-docsets '("Dart" "flutter"))
			      (dash-docs-reset-connections)))


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






