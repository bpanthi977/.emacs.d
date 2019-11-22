(defun rclone--read-target-from-config (config-dir)
  "Read the target directory from config file"
  (with-temp-buffer
	(insert-file-contents (concat  config-dir
								   ".rclone"))
	(first (split-string (buffer-string) "\n" t))))

(defun rclone--find-config-or-error (root)
  "Starting at root file find the .rclone config file directory"
  (let ((config (locate-dominating-file root ".rclone")))
	(unless config
	  (error "No config found. Create .rclone file with target directory"))
	config))

(defun rclone--process-sentinel (process event)
  "Kill async shell buffer when copying finishes"
  (cond ((string-match-p "finished" event)
		 (progn
		   (kill-buffer (process-buffer process))
		   (message "rclone copy done")))))

(defun rclone--async-copy (source target)
  (let ((output-buffer (generate-new-buffer "*Async shell command*"))
		(command (concat "rclone -P copy "
						 source " " target)))
	  (async-shell-command command output-buffer)
	  (with-current-buffer output-buffer
		(insert command))
	  ;; kill buffer when process completes
	  (when (get-buffer-process output-buffer)
		(set-process-sentinel (get-buffer-process output-buffer)
							 'rclone--process-sentinel))))

(defun rclone--copy-file/directory (filepath directory-p)
  (let* ((config (rclone--find-config-or-error filepath))
		 (target-root (rclone--read-target-from-config config))
		 (target (concat target-root
						 (subseq (file-name-directory filepath)
								 (length config)))))
	(rclone--async-copy (if directory-p
							(file-name-directory filepath)
						  filepath)
						target)))

(defun rclone-copy-file ()
  (interactive)
  (rclone--copy-file/directory (buffer-file-name) nil))

(defun rclone-copy-directory ()
  (interactive)
  (rclone--copy-file/directory (buffer-file-name) t))

(provide 'rclone-sync)
