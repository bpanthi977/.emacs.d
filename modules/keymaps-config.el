(use-package smartrep
  :ensure t
  :demand t)

(use-package bind-key
  :ensure t
  :defer nil)

;;;;
;;;; Keyboard mappings
;;;;
(global-set-key (kbd "C-/") #'comment-or-uncomment-region)
(global-set-key (kbd "C->") #'enlarge-window-horizontally)
(global-set-key (kbd "C-<") #'shrink-window-horizontally)
(global-set-key (kbd "M->") #'enlarge-window)
(global-set-key (kbd "M-<") #'shrink-window)
;; Font size
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C-_") #'text-scale-decrease)
;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") #'menu-bar-mode)
(global-set-key (kbd "C-x g") #'magit-status)
(global-set-key (kbd "C-x C-K") #'kill-buffer-and-window)
;; (global-set-key (kbd "C-SPC") #'shift-modifier)

(defun shift-modifier ()
  "Capitalizes the next character pressed"
  (interactive)
  (let ((char (read-char)))
    (when char
      (setf char (cond ((= char 91) 123) ;; [ {
		       ((= char 93) 125) ;; ] }
		       ((= char 45) 95) ;; - _
		       ((= char 48) 41) ;; 9 (
		       ((= char 57) 40) ;; 0 )
		       ((= char 61) 43)
		       (t char)))
      (insert (capitalize (char-to-string char)))
      (sp-insert-pair))))

(define-prefix-command 'bp/global-prefix-map)
(define-key global-map (kbd "M-m") 'bp/global-prefix-map)

;; Keymaps
(bind-keys :map bp/global-prefix-map
	   ;; editing
	   ("e e" . hippie-expand)
	   ("e t" . toggle-truncate-lines)
	   ("e c" . upcase-initials-region)
	   ;; file
	   ("f r" . recover-this-file)
	   ("f d" . diff-buffer-with-file)
	   ("f f" . counsel-recentf))

(global-set-key (kbd "C-<tab>") 'outline-toggle-children)
(global-set-key (kbd "C-q") 'quoted-insert)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-v") 'view-mode)

(smartrep-define-key bp/global-prefix-map "e"
  '(("s" . (lambda () (cycle-spacing 0)))))

(smartrep-define-key bp/global-prefix-map "f"
  '(("k" . kill-this-buffer)
    ("s" . save-buffer)
    ("n" . next-buffer)
    ("p" . previous-buffer)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


