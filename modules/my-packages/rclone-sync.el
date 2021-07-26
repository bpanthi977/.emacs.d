(defstruct rclone-config
  source
  target
  operation
  command-options)

(defun rclone--read-config (config-dir)
  (with-temp-buffer
    (insert-file-contents (concat  config-dir ".rclone"))
    (let ((c (split-string (buffer-string) "\n" t)))
      (make-rclone-config :source config-dir
			  :target (cl-first c)
			  :operation (cl-second c)
			  :command-options (cl-third c)))))

(defun rclone--get-config (root)
  "Starting at root file find the .rclone config file directory"
  (let ((config-file (locate-dominating-file root ".rclone")))
    (unless config-file
      (error "No config found. Create .rclone file with target directory"))
    (rclone--read-config (file-truename config-file))))

(defun rclone--process-sentinel (process event)
  "Kill async shell buffer when copying finishes"
  (cond ((string-match-p "finished" event)
	 (progn
	   (kill-buffer (process-buffer process))
	   (message "rclone operation done")))))

(defun rclone--async (command source target &optional extra-commands process-sentinel)
  "Run rclone process to copy/sync source to target"
  (let ;; prepare command
      ((command (concat "rclone -P " command " \""
			source "\" \"" target
			"\" -L --exclude-if-present .ignore "
			(or extra-commands "")))
       (process-sentinel (or process-sentinel 'rclone--process-sentinel)))
    (message command)
    (let ((output-buffer (generate-new-buffer "*Async shell command*")))
      ;; run command p
      (async-shell-command command output-buffer)
      (with-current-buffer output-buffer
	(insert command))
      ;; kill buffer when process completes
      (when (get-buffer-process output-buffer)
	(set-process-sentinel (get-buffer-process output-buffer)
			      process-sentinel)))))

(defun rclone--async* (command source target &optional args)
  (unless (member command '("2way" "copy" "sync" "rcopy"))
    (error "Unknown operation ~a" command))
  (let ((args (or args "")))
    (cond ((string-equal command "2way")
	   (rclone--async "copy" source target args)      
	   (rclone--async "copy" target source args))
	  ((string-equal command "rcopy")
	   (rclone--async "copy" target source (concat args " --immutable")))
	  (t
	   (rclone--async command source target args)))))

(defun rclone--target-dir (config &optional path)
  (let* ((path (file-truename (or path (buffer-file-name)))))
    (concat (rclone-config-target config)
	    (cl-subseq (file-name-directory path)
		       (length (rclone-config-source config))))))

(defun rclone-backup (path operation)
  "Backup (2way, copy, sync) the path"
  (let* ((config (rclone--get-config path))
	 (target (rclone--target-dir config path))
	 (source (file-truename path)))
    (rclone--async* operation source target (rclone-config-command-options config))))

(defun rclone-copy-file ()
  (interactive)
  (rclone-backup (buffer-file-name) "copy"))

(defun rclone-copy-directory ()
  (interactive)
  (rclone-backup (file-name-directory (buffer-file-name)) "copy"))

(defun rclone-sync-directory ()
  (interactive)
  (rclone-backup (file-name-directory (buffer-file-name)) "sync"))

(defun rclone-reverse-copy-directory ()
  (interactive)
  (rclone-backup (file-name-directory (buffer-file-name)) "rcopy"))

(defun rclone-run-file ()
  "Run rclone command as per .rclone buffer"
  (interactive)
  (let ((config (rclone--get-config (buffer-file-name))))
    (rclone--async* (rclone-config-operation config)
		    (file-truename (file-name-directory (buffer-file-name)))
		    (rclone--target-dir config (buffer-file-name)))))


(provide 'rclone-sync)

;; (defvar rclone--defer-run nil
;;   "Defer the commands to run in cmd for batch execution")

;; (defvar rclone--defered-commands nil)

;; (defun rclone--async-defered ()
;;   "Run defered commands turn by turn with same output buffer"
;;   (message "Running defered commands")
;;   (let* ((output-buffer (generate-new-buffer "*Rclone Sync*"))
;; 	 (perform (lambda ()
;; 		    (message "Performing")
;; 		    (when rclone--defered-commands
;; 		      (let ((command (cl-first rclone--defered-commands)))
;; 			(async-shell-command command output-buffer)
;; 			(setf rclone--defered-commands (cl-rest rclone--defered-commands))
;; 			(with-current-buffer output-buffer
;; 			  (insert command))
;; 			(message command)
;; 			(when (get-buffer-process output-buffer)
;; 			  (set-process-sentinel (get-buffer-process output-buffer)
;; 						sentinel))))))
;; 	 (count 0)
;; 	 (total (length rclone--defered-commands))
;; 	 (sentinel (lambda (process event)
;; 		     (cond ((string-match-p "finished" event)
;; 			    (progn
;; 			      (message "rclone sync done %d/" count total)
;; 			      (if (= count total)
;; 				  (kill-buffer (process-buffer process))
;; 				(funcall perform))))))))
;;     (funcall perform)))


;; (defun rclone-run-backup-tasks ()
;;   "Run backup tasks from rclone-sync file at emacs.d
;; the file must contain lines with following format 
;; [copy|2way|sync] Source_dir_with_.rclone_file_in_its_tree 
;; For example 
;; 2way E:/Documents/ 
;; "
;;   (interactive)
;;   (let ((config-file (expand-file-name "rclone-sync" init-dir)))
;;     (unless (file-exists-p config-file)
;;       (error "rclone-sync file doesn't exist at emacs init directory"))
;;     (let ((rclone--defer-run t)
;; 	  (rclone--defered-commands nil))
;;       (with-temp-buffer
;; 	(insert-file-contents config-file)
;; 	(dolist (line (split-string (buffer-string) "\n"))
;; 	  (when (> (length line) 4)
;; 	    (let ((type (substring line 0 4))
;; 		  (config (substring line 5)))
;; 	      (rclone--backup-file/directory config t type))))
;; 	(message rclone--defered-commands)
;; 	(setf rclone--defered-commands (reverse rclone--defered-commands))
;; 	(rclone--async-defered)))))
