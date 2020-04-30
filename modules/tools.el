(use-package rclone-sync
  :defer t 
  :commands (rclone-copy-directory rclone-copy-file rclone-run-backup-tasks rclone-run-file)
  :bind (:map bp/global-prefix-map
			  ("s r f" . rclone-copy-file)
			  ("s r d" . rclone-copy-directory))
  :bind (("C-c C-s f" . rclone-copy-file)
		 ("C-c C-s d" . rclone-copy-directory)))



