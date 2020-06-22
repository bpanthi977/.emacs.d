(defun sci-hub-pdf-url (doi)
  "Get url to the pdf from SCI-HUB"
  (setq *doi-utils-pdf-url* (concat "https://sci-hub.se/" doi) ;captcha
        *doi-utils-waiting* t
        )
  ;; try to find PDF url (if it exists)
  (url-retrieve (concat "https://sci-hub.se/" doi)
            (lambda (status)
              (goto-char (point-min))
              (while (search-forward-regexp "\\(https://\\|//sci-hub.se/downloads\\).+download=true'" nil t)
                (let ((foundurl (match-string 0)))
                  (message foundurl)
                  (if (string-match "https:" foundurl)
                  (setq *doi-utils-pdf-url* foundurl)
                (setq *doi-utils-pdf-url* (concat "https:" foundurl))))
                (setq *doi-utils-waiting* nil))))
  (while *doi-utils-waiting* (sleep-for 0.1))
  *doi-utils-pdf-url*)

(defun doi-utils-get-pdf-url (doi)
  (message "%s" doi)
  (sci-hub-pdf-url doi))

(provide 'doi-utils-scihub)
