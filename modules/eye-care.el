;; 20-20-20 for rule
(defvar bp/eye-care-timer nil)

(defcustom bp/eye-care-interval (* 20 60)
  "The time interval for eye rest. Usually 20 minutes.

You need to restart the time after changing this variable.")

(defcustom bp/eye-care-duration 20
  "The duration for which to take break. Usually 20 seconds.")

(defconst bp/eye-care-directory (file-name-directory (or load-file-name buffer-file-name)))

(defun bp/eye-care-beep ()
  ;; eye-care-sound1 was downloaded from https://20-20-20-timer.netlify.app/
  ;; Must use an absolute path: default-directory when the timer fires is
  ;; whatever buffer is current at that moment, not this file's directory.
  (start-process "afplay-process" nil "afplay"
		 (expand-file-name "eye-care-sound1.wav" bp/eye-care-directory)))

(defvar bp/eye-care-last-beep-time nil)

(defun bp/eye-care-routine ()
  (setf bp/eye-care-last-beep-time (current-time))
  (bp/eye-care-beep)
  (run-at-time bp/eye-care-duration nil #'bp/eye-care-beep))

(defun bp/eye-care-start ()
  (interactive)
  (if bp/eye-care-timer
      (error "Timer already running.")
    (setf bp/eye-care-timer
	  (run-at-time bp/eye-care-interval
		       bp/eye-care-interval
		       #'bp/eye-care-routine))))

(defun bp/eye-care-status ()
  "Show how long ago the last beep was and when the next one is due."
  (interactive)
  (if (not bp/eye-care-timer)
      (message "Eye care timer is not running.")
    (let* ((now (current-time))
	   (since (and bp/eye-care-last-beep-time
		       (float-time (time-subtract now bp/eye-care-last-beep-time))))
	   (until (float-time (time-subtract (timer--time bp/eye-care-timer) now))))
      (message "Last beep: %s.  Next beep: in %s."
	       (if since (format-seconds "%m min %s sec ago" since) "never yet")
	       (format-seconds "%m min %s sec" until)))))

(defun bp/eye-care-stop ()
  (interactive)
  (if (not bp/eye-care-timer)
      (error "Time not running")
    (cancel-timer bp/eye-care-timer)
    (setf bp/eye-care-timer nil)))
