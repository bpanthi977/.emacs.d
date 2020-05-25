(use-package rclone-sync
  :defer t 
  :commands (rclone-copy-directory
			 rclone-copy-file
			 rclone-run-backup-tasks
			 rclone-synce-directory
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
		   ("t c" . calc))

;;(setf which-key-enable-extended-define-key t)
;;(define-key bp/global-prefix-map "s" '("Save"))

