(cl-defun assort (seq &key (key #'identity) (test #'eql) (start 0) end)
  "Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

Groups are ordered as encountered. This property means you could, in
principle, use `assort' to implement `remove-duplicates' by taking the
first element of each group:

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)

However, if TEST is ambiguous (a partial order), and an element could
qualify as a member of more than one group, then it is not guaranteed
that it will end up in the leftmost group that it could be a member
of.

From Serapeum Library (Common Lisp)"
  (let ((groups nil)
        last-group)
    (map 'nil (lambda (item)
                (let* ((kitem (funcall key item))
                       (group (if (and last-group
                                       (funcall test kitem (car last-group)))
                                  last-group
                                (find-if
                                 (lambda (group)
                                   (funcall test kitem (car group)))
                                 groups))))
                  (cond (group
                         (setf last-group group)
                         (push item (cdr group)))
                        (t
                         (push (cons kitem (cons item nil)) groups)))))
         (subseq seq start end))
        (mapcar (lambda (group)
                  (nreverse (cdr group)))
                (nreverse groups))))


(defun note-headings (highlight)
  (gethash "toc_family_titles" highlight))

(defun org-calibre-notes/parse-and-insert (json-string file)
  (let ((json (ignore-errors (json-parse-string (current-kill 0)))))
    (if (null json)
        (message "invalid json string")
      (let ((notes-group (assort (gethash "highlights" json) :key #'note-headings
                                 :test #'equalp)))
        (cl-flet ((print-heading (note heading extra-level)
                              (when (> (length heading) 0)
                                (loop for title across heading
                                      for i from (1+ extra-level) do
                                      (insert (format "%s %s\n" (make-string i ?*) title))
                                      (when file
                                        (insert (format "[[nov:%s::%d:0][link]]\n"
                                                        file
                                                        (1+ (gethash "spine_index" note))))))))
               (print-note (note)
                           (insert (format "%s\n\n" (gethash "highlighted_text" note)))))
          (let ((prev-heading nil))
            (cl-loop for notes in notes-group
                  for heading = (note-headings (first notes)) do
                  (unless (equalp heading prev-heading)
                    (let ((max-match-length (loop for a across prev-heading
                                                  for b across heading
                                                  for i from 0
                                                  unless (equalp a b)
                                                  return i
                                                  finally (return i))))
                      (if (= max-match-length 0)
                          ;; completely new heading
                          (print-heading (elt notes 0) heading 0)
                        ;; subset of previous heading
                        (print-heading (elt notes 0) (subseq heading max-match-length) max-match-length))
                      (setf prev-heading heading)))
                  (loop for note in notes
                        do (print-note note)))))))))

(defun org-calibre-notes/select-ebook-file ()
  (let ((default-directory
          (if (eq major-mode 'dired-mode)
              (dired-current-directory)
            "~/Documents/")))
    (let ((ivy-sort-functions-alist '((read-file-name-internal . file-newer-than-file-p))))
      (ivy-read "EPub File: " #'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :preselect (counsel--preselect-file)
                :require-match 'confirm-after-completion
                :sort t
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'org-calibre-notes))))

(defun org-calibre-notes/select-save-file (file)
  (let ((default-directory
          (cond (file (file-name-directory file))
                ((eq major-mode 'dired-mode)
                 (dired-current-directory))
                (t "~/org/"))))
    (ivy-read "Save to: " #'read-file-name-internal
              :initial-input (concat (file-name-base file) ".org")
              :matcher #'counsel--find-file-matcher
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'org-calibre-notes)))

(defun org-calibre-notes-save ()
  (interactive)
  (let ((parsed-json (ignore-errors (json-parse-string (current-kill 0)))))
    (unless parsed-json
      (message "invalid json. Open ebook in calibre reader then: Export-> Format = calibre highlights -> Copy to Clipboard"))
    (when parsed-json
      (let* ((source (org-calibre-notes/select-ebook-file))
             (savefile (org-calibre-notes/select-save-file source)))
        (when savefile
          (find-file savefile)
          (org-calibre-notes/parse-and-insert parsed-json source))))))
