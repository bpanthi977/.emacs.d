;;; 
;;;; Emacs package to save notes for reading page in Okular
;;;
(require 'dbus)
(defun bp/okl-list-services () 
  (let ((result nil)
	(all-services (dbus-call-method :session "org.freedesktop.DBus" "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")))
    (loop for k in all-services do
	  (when (search "org.kde.okular-" k)
	    (push k result)))
    result))

(defun bp/okl-get-active-tab (service)
  (let (tab-title 
	(title (dbus-get-property :session service "/okular/okular__Shell_1" "org.qtproject.Qt.QWidget" "windowTitle")))
    (loop for node in (dbus-introspect-get-all-nodes :session service "/") do 
	  (when (string-match "^/okular[0-9]*$" node)
	    (ignore-errors (setf tab-title (dbus-call-method :session service node "org.kde.okular" "documentMetaData" "title")))
	    (when (and tab-title (string-equal tab-title ""))
	      (setf tab-title (file-name-base (bp/okl-get-document service node))))
	    (when (and tab-title (string-prefix-p tab-title title))
	      (return (list node tab-title)))))))

(defun bp/okl-get-document-tab (document)
  (block function 
    (loop for service in (bp/okl-list-services) do 
	  (let ((base (file-name-base document)))
	    (loop for node in (dbus-introspect-get-all-nodes :session service "/") do 
		  (when (string-match "^/okular[0-9]*$" node)
		    (when-let* ((doc (bp/okl-get-document service node))
				(tab-document (file-name-base doc)))
		      (when (string-equal tab-document base)
			(return-from function (values service node))))))))))

(defun bp/okl-get-page-number (service node)
  (dbus-call-method :session service node "org.kde.okular" "currentPage"))

(defun bp/okl-set-page-number (service node page-number)
  (dbus-call-method :session service node "org.kde.okular" "goToPage" page-number))

(defun bp/okl-raise (service)
  (dbus-call-method :session service "/okularshell" "org.kde.okular" "tryRaise"))

(defun bp/okl-reload (service node)
  (dbus-call-method :session service node "org.kde.okular" "reload"))

(defun bp/okl-get-document (service node) 
  (ignore-error dbus-error
    (dbus-call-method :session service node "org.kde.okular" "currentDocument")))

(defun bp/okl-save-note% (document page)
  (let ((prop-document (org-entry-get (point) "NOTER_DOCUMENT" t)))
    (if (or (not prop-document)
	    (not (string-suffix-p prop-document document)))
	(org-set-property "NOTER_DOCUMENT" (file-relative-name (file-truename document) (file-name-directory (file-truename (buffer-file-name))))))
    (org-set-property "NOTE_PAGE" (format "%d" page))))

(defvar bp/okl-ask-for-link-description nil)
(defun bp/okl-save-note%-link (document page)
  (if bp/okl-ask-for-link-description
      (let ((description (completing-read "Description:" (list (file-name-base (buffer-file-name))
							       (format "%s" page)
							       (current-kill 0)
							       (file-name-base document)))))
	(insert "[[./"
		(file-relative-name document (file-name-directory (buffer-file-name)))
		"::" (format "%s" page) 
		"][" description "]]"))
      (insert "[[./"
	      (file-relative-name (file-truename document) (file-name-directory (file-truename (buffer-file-name))))
	      "::" (format "%s" page) 
	      "][" (file-name-base document) " pg. " (format "%s" page) "]]")))

(defun bp/okular-document ()
  (let* ((okl-services (bp/okl-list-services))
	 (nodes-tabs (loop for s in okl-services collect (bp/okl-get-active-tab s))))
    (let* ((tabs (map 'list #'second nodes-tabs))
	   (selection (if (= (length tabs) 1) 
			  (first tabs)
			(ivy-completing-read "Document: " tabs nil t)))
	   (n (position selection tabs :test #'string-equal))
	   (service (nth n okl-services))
	   (node (first (nth n nodes-tabs)))
	   (document (bp/okl-get-document service node))
	   (page (bp/okl-get-page-number service node)))
      (values document page))))

(defun bp/okl-save-note (&optional link?)
  (interactive)
  (multiple-value-bind (document page) (bp/okular-document)
    (if link? 
	(bp/okl-save-note%-link document page)
      (bp/okl-save-note% document page))))

(defun bp/okular-note ()
  (interactive)
  (multiple-value-bind (document page) (bp/okular-document)
    (declare (ignore page))
    (when-let* ((orgfile (and document (concat (file-name-directory (file-truename document)) (file-name-base document) ".org"))))
      (if (file-exists-p orgfile)
          (find-file orgfile)
        (progn
          (find-file orgfile)
          (insert ":PROPERTIES:\n" ":NOTER_DOCUMENT: " (file-name-nondirectory document) "\n:END:\n")
          (insert "#+TITLE: " (file-name-base document) "\n"))))))

(defun bp/okl-save-note-as-link () 
  (interactive)
  (bp/okl-save-note t))

(defun bp/okl-save-note-as-link-ask-description ()
  (interactive)
  (let ((bp/okl-ask-for-link-description t))
    (bp/okl-save-note t)))

(defun bp/okl-note-link (file link)
  (multiple-value-bind (service node) (bp/okl-get-document-tab file)
    (let* ((match-location (string-match "::[0-9]+$" link))
	   (page (if match-location (subseq link (+ match-location 2)))))
      (if node 
	  (if page 
	      (bp/okl-set-page-number service node (string-to-number page))
	    (bp/okl-reload service node))
	(if page 
	    (start-process "okular" nil "/usr/bin/okular" "-p" page file)
	  (start-process "okular" nil "/usr/bin/okular" file))))))

(defun bp/okl-open-note-page () 
  (interactive)
  (let ((document (org-entry-get (point) "NOTER_DOCUMENT" t))
	(page (or (org-entry-get (point) "NOTE_PAGE" t)
                  (number-to-string (caar (read-from-string (org-entry-get (point) "NOTER_PAGE" t)))))))
    (multiple-value-bind (service node) (bp/okl-get-document-tab document)
      (if node 
	  (bp/okl-set-page-number service node (string-to-number page))
	 (start-process "okular" nil "/usr/bin/okular" "-p"  page document)))))

(defun bp/okl-insert-heading ()
  (interactive)
  (org-insert-heading-after-current)
  (bp/okl-save-note nil))

(define-prefix-command 'bp/okl-notes-prefix-map)
(define-key global-map (kbd "M-n") 'bp/okl-notes-prefix-map)

(bind-keys :map bp/okl-notes-prefix-map
	   ("n" . bp/okl-save-note)
           ("N" . bp/okular-note)
	   ("l" . bp/okl-save-note-as-link)
	   ("o" . bp/okl-open-note-page)
	   ("i" . bp/okl-insert-heading)
	   ("d" . bp/okl-save-note-as-link-ask-description))

(bind-keys :map bp/global-prefix-map
           ("o n" . bp/okular-note))
