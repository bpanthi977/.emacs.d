(defun bp/short-name (str)
  (interactive "sName:")
  (let ((terms (split-string str " ")))
    (setf terms (mapcar (lambda (term)
			  (if (> (length term) 5)
			      (subseq term 0 5)
			    term))
			terms))
    (setf str (string-join terms "_"))
    (with-temp-buffer
      (insert str)
      (mark-whole-buffer)
      (clipboard-kill-ring-save 0 0 t))
    (message str)))



