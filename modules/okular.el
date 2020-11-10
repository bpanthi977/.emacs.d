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
	  (let (tab-document
		(base (file-name-base document)))  
	    (loop for node in (dbus-introspect-get-all-nodes :session service "/") do 
		  (when (string-match "^/okular[0-9]*$" node)
		    (setf tab-document (file-name-base (bp/okl-get-document service node)))
		    (when (string-equal tab-document base)
		      (return-from function (values service node)))))))))

(defun bp/okl-get-page-number (service node)
  (dbus-call-method :session service node "org.kde.okular" "currentPage"))

(defun bp/okl-set-page-number (service node page-number)
  (dbus-call-method :session service node "org.kde.okular" "goToPage" page-number))

(defun bp/okl-raise (service)
  (dbus-call-method :session service "/okularshell" "org.kde.okular" "tryRaise"))

(defun bp/okl-reload (service node)
  (dbus-call-method :session service node "org.kde.okular" "reload"))

(defun bp/okl-get-document (service node) 
  (dbus-call-method :session service node "org.kde.okular" "currentDocument"))

(defun bp/okl-save-note% (document page)
  (let ((prop-document (org-entry-get (point) "NOTER_DOCUMENT" t)))
    (if (or (not prop-document)
	    (not (string-suffix-p prop-document document)))
	(org-set-property "NOTER_DOCUMENT" (file-relative-name document (file-name-directory (buffer-file-name)))))
    (org-set-property "NOTE_PAGE" (format "%d" page))))

(defun bp/okl-save-note%-link (document page)
  (insert "[["
	  (file-relative-name document (file-name-directory (buffer-file-name)))
	  "::"
	  (format "%s" page) 
	  "]["
	  (file-name-base document) 
	  " pg. "
	  (format "%s" page) 
	  "]]"))

(defun bp/okl-save-note (&optional link?)
  (interactive)
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
      (if link? 
	  (bp/okl-save-note%-link document page)
        (bp/okl-save-note% document page)))))

(defun bp/okl-save-note-as-link () 
  (interactive)
  (bp/okl-save-note t))

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
	(page (org-entry-get (point) "NOTE_PAGE" t)))
    (multiple-value-bind (service node) (bp/okl-get-document-tab document)
      (if node 
	  (bp/okl-set-page-number service node (string-to-number page))
	 (start-process "okular" nil "/usr/bin/okular" "-p"  page document)))))

(bind-keys :map bp/global-prefix-map
	   ("n n" . bp/okl-save-note)
	   ("n l" . bp/okl-save-note-as-link)
	   ("n o" . bp/okl-open-note-page))
