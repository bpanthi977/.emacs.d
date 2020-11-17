(defun org-web-notes/parse (file)
  (with-temp-buffer 
    (insert-file file)
    (libxml-parse-html-region (point-min) (point-max))))

(defun org-web-note/highlight? (node)
  (if (search "single-file-highlight" (dom-attr node 'class))
      (let ((id (dom-attr node 'data-singlefile-highlight-id)))
	(when (not (equal id org-web-note--heading-id))
	  (princ "\n\n"))
	(setf org-web-note--heading-id id)
	t)))

(defun org-web-note/heading? (node)
  (let ((tag (dom-tag node)))
    (setf org-web-note--heading-text 
	  (cond 
	   ((eql tag 'h1) 
	    (concat "\n* " (dom-texts node " ")))
	   ((eql tag 'h2) 
	    (concat "\n** " (dom-texts node " ")))
	   ((eql tag 'h3) 
	    (concat "\n*** " (dom-texts node " ")))
	   (t nil)))
    (when org-web-note--heading-text 
      (setf org-web-note--heading-id nil))
    org-web-note--heading-text))

(defun org-web-notes/walk-and-collect0 (node)
  (cond ((stringp node)
	 nil)
	((org-web-note/highlight? node)
	 (if (eql (dom-tag node) 'li)
	     (princ "\n+ "))
	 (princ (dom-texts node " ")))
	((org-web-note/heading? node)
	 (princ org-web-note--heading-text))
	(t 
	 (loop for n in (dom-children node) do
	       (org-web-notes/walk-and-collect0 n)))))

(defun org-web-notes/find-savefile-url (dom)
  (cond ((stringp dom)
	 nil)
	((eql (dom-tag dom) 'html)
	 (let ((comment (dom-text (dom-child-by-tag dom 'comment))))
	   (string-trim 
	    (substring comment 
		       (+ (search "url:" comment) 4)
		       (search "saved date: " comment)))))
	(t 
	 (loop for d in (dom-children dom) 
	       for url = (org-web-notes/find-savefile-url d) do 
	       (when url 
		 (return url))))))

(defun org-web-notes/select-html-file () 
  (let ((default-directory
         (if (eq major-mode 'dired-mode)
             (dired-current-directory)
           "~/Downloads/")))
    (let ((ivy-sort-functions-alist '((read-file-name-internal . file-newer-than-file-p))))
      (ivy-read "Html File: " #'read-file-name-internal
		:matcher #'counsel--find-file-matcher
		:preselect (counsel--preselect-file)
		:require-match 'confirm-after-completion
		:sort t
		:history 'file-name-history
		:keymap counsel-find-file-map
		:caller 'org-web-notes))))


(defun org-web-notes/select-save-file (file) 
  (let ((default-directory
         (if (eq major-mode 'dired-mode)
             (dired-current-directory)
           "~/org/")))
    (ivy-read "Save to: " #'read-file-name-internal
	      :initial-input (concat (file-name-base file) ".org")
	      :matcher #'counsel--find-file-matcher
	      :preselect (counsel--preselect-file)
	      :require-match 'confirm-after-completion
	      :history 'file-name-history
	      :keymap counsel-find-file-map
	      :caller 'org-web-notes)))

(defun org-web-notes/html-to-org (file)
  (insert "#+TITLE: " (file-name-base file) "\n"
	  "#+Date: " (format-time-string "%B %e, %Y") "\n")
  (let* ((dom (org-web-notes/parse file))
	 (url (org-web-notes/find-savefile-url dom))
	 (filename (concat (file-name-base file) "." (file-name-extension file))))
    (when url 
      (insert "#+ROAM_KEY: " url "\n"))
    (rename-file file (expand-file-name filename 
					"~/org/data/html/"))
    ;; insert the link
    (insert ":PROPERTIES: \n:FILE: [[file:./data/html/" filename 
	    "][Saved file]]" "\n:END:\n")
      
    (insert 
     (with-output-to-string
       (org-web-notes/walk-and-collect0 dom)))))

(defun org-web-notes-save ()
  "Parses source html file and saves it to savefile org file"
  (interactive)
  (let ((source (org-web-notes/select-html-file)))
    (when source 
      (let ((savefile (org-web-notes/select-save-file source)))
	(when savefile 
	  (find-file savefile)
	  (org-web-notes/html-to-org source))))))



