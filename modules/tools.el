(use-package rclone-sync
  :defer t
  :commands (rclone-copy-directory rclone-copy-file)
  :bind (("C-c C-s f" . rclone-copy-file)
		 ("C-c C-s d" . rclone-copy-directory)))
