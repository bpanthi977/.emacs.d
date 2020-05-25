(defvar rclone--defer-run nil
  "Defer the commands to run in cmd for batch execution")

(defvar rclone--defered-commands nil)

(defun rclone--get-target-directory (config-dir)
  "Read the target directory from config file"
  (with-temp-buffer
	(insert-file-contents (concat  config-dir ".rclone"))
	(cl-first (split-string (buffer-string) "\n" t))))

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

(defun rclone--async (command source target &optional extra-commands process-sentinel)
  "Run rclone process to copy/sync source to target"
  (let ;; prepare command
	   ((command (concat "rclone -P " command " \""
						source "\" \"" target
						"\" -L --exclude-if-present .ignore "
						(or extra-commands "")))
		(process-sentinel (or process-sentinel 'rclone--process-sentinel)))
	(message command)
	(if rclone--defer-run
		(add-to-list 'rclone--defered-commands command)
	  (let ((output-buffer (generate-new-buffer "*Async shell command*")))
		;; run command p
		(async-shell-command command output-buffer)
		(with-current-buffer output-buffer
		  (insert command))
		;; kill buffer when process completes
		(when (get-buffer-process output-buffer)
		  (set-process-sentinel (get-buffer-process output-buffer)
								process-sentinel))))))

(defun rclone--async-defered ()
  "Run defered commands turn by turn with same output buffer"
  (message "Running defered commands")
  (let* ((output-buffer (generate-new-buffer "*Rclone Sync*"))
		 (perform (lambda ()
					(message "Performing")
					(when rclone--defered-commands
					  (let ((command (cl-first rclone--defered-commands)))
						(async-shell-command command output-buffer)
						(setf rclone--defered-commands (cl-rest rclone--defered-commands))
						(with-current-buffer output-buffer
						  (insert command))
						(message command)
						(when (get-buffer-process output-buffer)
						  (set-process-sentinel (get-buffer-process output-buffer)
												sentinel))))))
		 (count 0)
		 (total (length rclone--defered-commands))
		 (sentinel (lambda (process event)
					 (cond ((string-match-p "finished" event)
							(progn
							  (message "rclone sync done %d/" count total)
							  (if (= count total)
								  (kill-buffer (process-buffer process))
								(funcall perform))))))))
	(funcall perform)))


(defun rclone--create-target-path (source-path source-root target-root)
  "Create target path for given source-path"
  (concat target-root (cl-subseq (file-name-directory source-path)
								 (length source-root))))

(defun rclone--backup-file/directory (path directory-p backuptype)
  "Backup (2way, copy, sync) the path"
  (let* ((config (rclone--find-config-or-error path)) ;; get config for the path
		 (target-root (rclone--get-target-directory config)) ;; get root target directory
		 (target (rclone--create-target-path path config target-root))
		 (source (if directory-p (file-name-directory path) path)))
	(cond 
	 ((string-equal backuptype "copy")
	  (rclone--async "copy" source target))
	 ((string-equal backuptype "rcopy")
	  (rclone--async "copy" target source "--immutable"))
	 ((string-equal backuptype "2way")
	  (rclone--async "copy" source target)
	  (rclone--async "copy" target source))
	 ((string-equal backuptype "sync")
	  (rclone--async "sync" source target))
	 (t
	  (error "unknown backuptype %s" backuptype)))))

(defun rclone-copy-file ()
  (interactive)
  (rclone--backup-file/directory (buffer-file-name) nil "copy"))

(defun rclone-copy-directory ()
  (interactive)
  (rclone--backup-file/directory (buffer-file-name) t "copy"))

(defun rclone-sync-directory ()
  (interactive)
  (rclone--backup-file/directory (buffer-file-name) t "sync"))

(defun rclone-reverse-copy-directory ()
  (interactive)
  (rclone--backup-file/directory (buffer-file-name) t "rcopy"))

(defun rclone-run-backup-tasks ()
  "Run backup tasks from rclone-sync file at emacs.d
the file must contain lines with following format 
[copy|2way|sync] Source_dir_with_.rclone_file_in_its_tree 
For example 
2way E:/Documents/ 
"
  (interactive)
  (let ((config-file (expand-file-name "rclone-sync" init-dir)))
	(unless (file-exists-p config-file)
	  (error "rclone-sync file doesn't exist at emacs init directory"))
	(let ((rclone--defer-run t)
		  (rclone--defered-commands nil))
	  (with-temp-buffer
		(insert-file-contents config-file)
		(dolist (line (split-string (buffer-string) "\n"))
		  (when (> (length line) 4)
			(let ((type (substring line 0 4))
				  (config (substring line 5)))
			  (rclone--backup-file/directory config t type))))
		(message rclone--defered-commands)
		(setf rclone--defered-commands (reverse rclone--defered-commands))
		(rclone--async-defered)))))

(defun rclone-run-file ()
  "Run rclone command as per .rclone buffer"
  (interactive)
  (let* ((config (rclone--find-config-or-error (buffer-file-name)))
		 (backuptype (with-temp-buffer
						(insert-file-contents (concat config "/.rclone"))
						(cl-second (split-string (buffer-string))))))
	(rclone--backup-file/directory (buffer-file-name) t backuptype)));; get config for the path
		

(provide 'rclone-sync)
