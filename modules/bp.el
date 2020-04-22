(defun bp/update-env ()
  "updates env from given file 
file is dup of env
env > file (in windows) 
printenv -0 > file (in linux)"
  (interactive)
  (let ((str 
         (with-temp-buffer
           (insert-file-contents "C:/bp/envvar.temp")
           (buffer-string)))
		lst)
    (setq lst (split-string str "
"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

