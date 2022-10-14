(use-package mpv 
  :ensure t 
  :commands (org-mpv-notes)
  :defer t 
  :config 
  ;; from https://github.com/kljohann/mpv.el/wiki
  ;;  To create a mpv: link type that is completely analogous to file: links but opens using mpv-play instead, use:
  (org-add-link-type "mpv" #'mpv-play*)
  (defun mpv-play* (path)
    (print path)
    (mpv-play path))
  (defun org-mpv-complete-link (&optional arg)
    (replace-regexp-in-string
     "file:" "mpv:"
     (org-file-complete-link arg)
     t t))
  (org-link-set-parameters "mpv" :complete #'org-mpv-complete-link)

  ;; You can further add the following snippet to have mpv seek to the position of a timestamp when pressing /RET/ in an org buffer:
  (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)
  
  ;; Help: 
  ;; open mpv using [[mpv:...]] link 
  ;; use (mpv-insert-playback-position) to insert timestamp 
  ;; use C-c C-o to seek mpv to that time 
  

  ;; save screenshot as attachment 
  (defun org-mpv-notes-save-screenshot () 
    (interactive)
    (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
      ;; take screenshot 
      (mpv-run-command "screenshot-to-file" 
		       filename
		       "video")
      ;; attach it 
      (let ((org-attach-method 'mv))
	(org-attach-attach filename))
      ;; insert the link
      (insert "[[attachment:" (file-name-base filename) ".png]]")
      (org-display-inline-images)))

  (defun org-mpv-notes-screenshot-ocr () 
    (interactive)
    (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
      ;; take screenshot 
      (mpv-run-command "screenshot-to-file" 
		       filename
		       "video")
      (let ((string (bp/tesseract-on-file filename)))
	(insert "\n" 
		string
		"\n"))))

  (defun org-mpv-notes--video-arg (path)
    "video arg corresponding to path in [[mpv:...]] link"
    (if (or (string-prefix-p path "http:")
	    (string-prefix-p path "https:"))
	path
      (expand-file-name path
			(file-name-directory (file-truename (buffer-file-name))))))

  (defun org-mpv-notes-open () 
    (interactive)
    (save-excursion 
      ;; move to next heading 
      (unless (re-search-forward "^\\*+" nil t)
	;; if no heading goto end
	(goto-char (point-max)))
      ;; then search backwards for links 
      (let ((pos (re-search-backward "\\[\\[mpv:[^\n]*\\]\\]")))
	(when pos 
	  ;; when link is found play it
	  (forward-char)
	  (when (mpv-live-p)
	    (mpv-kill)
	    (sleep-for 0.05))
	  (mpv-start (org-element-property :path (org-element-context)))))))
  
  (defun org-mpv-notes-insert-note () 
    (interactive)
    (org-insert-heading)
    (save-excursion 
      (org-insert-property-drawer)
      (org-set-property "time" (org-timer-secs-to-hms (round (mpv-get-playback-position))))
      ))
    
  (defun org-mpv-notes-next-timestamp () 
    (interactive)
    (when (re-search-forward "[0-9]+:[0-9]+:[0-9]+" nil t)
      (mpv-seek-to-position-at-point)
      (org-show-entry)
      (recenter)))
  
  (defun org-mpv-notes-previous-timestamp () 
    (interactive)
    (when (re-search-backward "[0-9]+:[0-9]+:[0-9]+" nil t)
      (mpv-seek-to-position-at-point)
      (org-show-entry)
      (recenter)))

  (defun org-mpv-notes-this-timestamp () 
    "Seeks to the timestamp stored in the property drawer of the heading" 
    (interactive)
    (let ((timestamp (org-entry-get (point) "time" t)))
      (when timestamp
	(let ((time (org-timer-hms-to-secs timestamp)))
	  (when (> time 0)
	    (mpv-seek time)
	    (org-show-entry)
	    (recenter))))))

  (defun mpv-seek-double-step () 
    (interactive)
    (setf mpv-seek-step (* 2 mpv-seek-step))
    (message "%f" mpv-seek-step))

  (defun mpv-seek-halve-step ()
    (interactive)
    (setf mpv-seek-step (/ mpv-seek-step 2.0))
    (message "%f" mpv-seek-step))

  (defun mpv-toggle-fullscreen ()
    (interactive)
    ;; https://github.com/mpv-player/mpv/blob/master/DOCS/man/inpute.rst
    ;; https://github.com/mpv-player/mpv/blob/master/etc/input.conf
    (mpv--enqueue '("cycle" "fullscreen") #'ignore))

  (define-minor-mode org-mpv-notes
    "Org minor mode for Note taking alongside audio and video. 
Uses mpv.el to control mpv process" 
    nil 
    " mpv"
    :keymap `((,(kbd "M-n i") . mpv-insert-playback-position)
	      (,(kbd "M-n M-i") . org-mpv-notes-insert-note)
	      (,(kbd "M-n u") . mpv-revert-seek)
	      (,(kbd "M-n s") . org-mpv-notes-save-screenshot)
	      (,(kbd "M-n o") . org-mpv-notes-open)
	      (,(kbd "M-n k") . mpv-kill)
	      (,(kbd "M-n M-s") . org-mpv-notes-screenshot-ocr)
	      )
    (if org-mpv-notes
	(org-mpv-notes-open)
      (mpv-kill)))
  
  (smartrep-define-key org-mpv-notes-map "M-n"
    `(;; keys used for moving in the video
      ("b" . mpv-seek-backward)
      ("q" . keyboard-quit)
      ("f" . mpv-seek-forward)
      ("F" . mpv-toggle-fullscreen)
      ("<left>" . mpv-seek-backward)
      ("<right>" . mpv-seek-forward)
      ("<up>" . mpv-seek-double-step)
      ("<down>" . mpv-seek-halve-step)
      ;; keys used for moving in the notes
      ("n" . org-mpv-notes-next-timestamp)
      ("p" . org-mpv-notes-previous-timestamp)
      ("SPC" . mpv-pause)
      ("a" . mpv-seek-halve-step)
      ("d" . mpv-seek-double-step)
      ("." . org-mpv-notes-this-timestamp)))

)
 








