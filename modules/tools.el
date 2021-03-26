(use-package rclone-sync
  :defer t 
  :commands (rclone-copy-directory
			 rclone-copy-file
			 rclone-run-backup-tasks
			 rclone-sync-directory
			 rclone-reverse-copy-directory
			 rclone-run-file))

(bind-keys :map bp/global-prefix-map
			  ("s r f" . rclone-copy-file)
			  ("s r d" . rclone-copy-directory)
			  ("s r s" . rclone-sync-directory)
			  ("s r r" . rclone-reverse-copy-directory))

(use-package calculator
  :defer t)

(bind-keys :map bp/global-prefix-map
	   ("t c" . calc)
	   ("t d" . dictionary-search))

;;(setf which-key-enable-extended-define-key t)
;;(define-key bp/global-prefix-map "s" '("Save"))

(use-package anki-editor
  :ensure nil
  :defer t
  :config
  (setf anki-editor-create-decks t))

(defun bp/tesseract-on-file (file) 
  (save-window-excursion 
    (let ((buffer (generate-new-buffer "tesseract-ocr"))
	  (errbuffer (generate-new-buffer "tesseract-ocr-err")))
      (shell-command (format "tesseract \"%s\" -" (file-truename file) ) buffer errbuffer)
      (let ((string (with-current-buffer  buffer 
		      (buffer-string))))
	(kill-buffer buffer)
	(kill-buffer errbuffer)
	(remove ? string)))))

(defun bp/capture-screenshot () 
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "screenshot"))))
   (shell-command-to-string (format "xfce4-screenshooter -r -o cat > %s"
				    filename))
   (when (file-exists-p filename)
     (insert (bp/tesseract-on-file filename)))))

(bind-keys :map bp/global-prefix-map
	   ("e o" . bp/capture-screenshot))
