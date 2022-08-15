(require 'dom)
(defun org-web-notes/parse (file)
  (with-temp-buffer
    (insert-file-contents file)
    (libxml-parse-html-region (point-min) (point-max))))

(defvar org-web-note--highlight-id nil
  "ID of the current highlight")

(defvar org-web-note--heading-text nil
  "Current heading text")

(defvar org-web-note--headings-count 0
  "Total headings found in the file")

(defun org-web-note/highlight? (node)
  (let ((class (dom-attr node 'class)))
    (when (and class
               (or (string-match "^single-file-highlight$" class)
                   (string-match " single-file-highlight$" class)
                   (string-match "^single-file-highlight " class)
                   (string-match " single-file-highlight " class)))
      (let ((id (dom-attr node 'data-singlefile-highlight-id)))
        (when (not (equal id org-web-note--highlight-id))
          (princ "\n\n"))
        (setf org-web-note--highlight-id id)
        t))))

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
      (setf org-web-note--highlight-id nil))
    org-web-note--heading-text))

(defun org-web-notes/walk-and-collect0 (node)
  (cond ((stringp node)
         nil)
        ((org-web-note/highlight? node)
         (if (eql (dom-tag node) 'li)
             (princ "\n+ "))
         (incf org-web-note--headings-count)
         (princ (dom-texts node " ")))
        (nil
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
                       (+ (string-match "url:" comment) 4)
                       (string-match "saved date: " comment)))))
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

(defun org-web-notes/html-date (file format)
  (let* ((name (file-name-base file))
         (match (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" name)))
    (if match
        (format-time-string
         format
         (encode-time (make-decoded-time
                       :year (string-to-number (subseq name match (+ match 4)))
                       :month (string-to-number (subseq name (+ match 5) (+ match 7)))
                       :day (string-to-number (subseq name (+ match 8) (+ match 10)))
                       :second 0 :minute 0 :hour 0)))
      (format-time-string format))))


(defun org-web-notes/html-to-org (file)
  (interactive "f")
  (setf org-web-note--highlight-id nil
        org-web-note--headings-count 0)

  (let* ((dom (org-web-notes/parse file))
         (url (org-web-notes/find-savefile-url dom))
         (filename (concat (file-name-base file) "." (file-name-extension file))))

    ;; insert the link
    (insert ":PROPERTIES: \n:FILE: [[file:./data/html/" filename
            "][Saved file]]\n"
            (if url
                (concat ":ROAM_REFS: " url "\n")
              "")
            ":END:\n")

    (insert "#+TITLE: " (file-name-base file) "\n"
            "#+Date: " (org-web-notes/html-date file "%B %e, %Y") "\n")

    (insert
     (with-output-to-string
       (org-web-notes/walk-and-collect0 dom)))
    org-web-note--headings-count))


(defun move-file-to (file folder)
  (rename-file file (expand-file-name
                       (concat (file-name-base file) "." (file-name-extension file))
                       folder)))

(defun org-web-notes-save ()
  "Parses source html file and saves it to savefile org file"
  (interactive)
  (let ((source (org-web-notes/select-html-file)))
    (when source
      (let ((savefile (org-web-notes/select-save-file source)))
        (when savefile
          (find-file savefile)
          (org-web-notes/html-to-org source)
          (goto-char (point-min))
          (org-id-get-create)
          (move-file-to source "~/org/data/html/"))))))

(defun org-web-notes-bulk (input-dir output-dir)
  "Parse source html files from `input-dir' and save to similarly named files in `output-dir'"
  (assert (string-suffix-p "/" output-dir))
  (unless (file-exists-p output-dir)
    (make-directory output-dir t))
  (let ((html-dir (expand-file-name "data/html/" output-dir)))
    (unless (file-exists-p html-dir)
      (make-directory html-dir t))
    (map 'nil
         (lambda (input)
           (let ((filename (file-name-base input))
                 (type (file-name-extension input)))
             (when (string-equal type "html")
               (let ((output-file (expand-file-name (concatenate 'string filename ".org")
                                                    output-dir)))
                 (find-file output-file)
                 (cond ((= (org-web-notes/html-to-org input) 0)
                        (set-buffer-modified-p nil)
                        (kill-buffer))
                       (t
                        (goto-char (point-min))
                        (org-id-get-create)
                        (save-buffer)
                        (kill-buffer)
                        (move-file-to input html-dir)))))))
         (directory-files input-dir t))))

;; (org-web-notes-bulk "~/Downloads/webpages/" "~/Downloads/webpages/org/")
