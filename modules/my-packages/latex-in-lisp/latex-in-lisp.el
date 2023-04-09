(defun latex-in-lisp/get-latex ()
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (forward-char)
      (forward-sexp 2)
      (backward-char)
      (let ((str-bounds (bounds-of-thing-at-point 'string)))
        (when str-bounds
          (list (buffer-substring (1+ (car str-bounds)) (1- (cdr str-bounds)))
                (car bounds) (cdr bounds)))))))

(defun latex-in-lisp/image-file (string)
  (setf (getf org-format-latex-options :foreground) (face-attribute 'default :foreground nil))
  (let* ((processing-type org-preview-latex-default-process)
         (processing-info (cdr (assq processing-type org-preview-latex-process-alist)))

         (prefix (concat org-preview-latex-image-directory "org-ltximg"))
         (absprefix (expand-file-name prefix))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))

         (options nil)
         (fg nil)
         (bg nil)
         (hash (sha1 (prin1-to-string
                      (list org-format-latex-header
                            org-latex-default-packages-alist
                            org-latex-packages-alist
                            org-format-latex-options
                            string fg bg))))

         (imagefile (format "%s_%s.%s" absprefix hash imagetype)))
    (make-directory org-preview-latex-image-directory t)
    imagefile))

(defun latex-in-lisp/create-image (string)
  "adopted from org-format-latex"
  (let ((file (latex-in-lisp/image-file string)))
    (unless (file-exists-p file)
      (org-create-formula-image string file org-format-latex-options (current-buffer)))
    file))

(defun latex-in-lisp/remove-image-at-point ()
  (let ((current-point (point)))
    (save-excursion
      (let ((prop (text-property-search-forward 'display)))
        (when (and prop
                   (<= (prop-match-beginning prop) current-point))
          (remove-text-properties (prop-match-beginning prop)
                                  (prop-match-end prop)
                                  '(display))
          t)))))

(defun latex-in-lisp/insert-image-at-point ()
  (let ((current-point (point)))
    (save-excursion
      (goto-char (min (point-max) (+ current-point 2)))
      (when (re-search-backward "(latex-doc" nil t)
        (when (<= (match-beginning 0) current-point)
          (let ((ret (latex-in-lisp/get-latex)))
            (print ret)
            (when (and ret (first ret))
              (cl-destructuring-bind (latex-string beg end) ret
                (let ((file (latex-in-lisp/create-image latex-string)))
                  (add-text-properties beg end (list 'display (create-image file)))
                  t)))))))))

(defun latex-in-lisp/toggle-image ()
  (interactive)
  (or (latex-in-lisp/remove-image-at-point)
      (latex-in-lisp/insert-image-at-point)))

(global-set-key (kbd "C-c C-x C-l") #'latex-in-lisp/toggle-image)

(provide 'latex-in-lisp)
