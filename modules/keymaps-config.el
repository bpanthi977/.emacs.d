(install-packages (list 'avy
			'magit
			'ace-window
			'flycheck))
;;;;
;;;; Keyboard mappings
;;;;
(defvar bpanthi-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-/") 'comment-or-uncomment-region)
    (define-key map (kbd "C->") 'enlarge-window-horizontally)
    (define-key map (kbd "C-<") 'shrink-window-horizontally)
    (define-key map (kbd "M->") 'enlarge-window)
    (define-key map (kbd "M-<") 'shrink-window)
    (define-key map (kbd "M-g M-g") 'avy-goto-line)
    (define-key map (kbd "M-g M-c") 'avy-goto-char)
    (define-key map (kbd "C-c C-e") 'flycheck-list-errors)
    ;; Font size
    (define-key map (kbd "C-+") 'text-scale-increase)
    (define-key map (kbd "C--") 'text-scale-decrease)
    ;; toggle menu-bar visibility
    (define-key map (kbd "<f12>") 'menu-bar-mode)
    (define-key map (kbd "C-x g") 'magit-status)
    (define-key map (kbd "C-SPC") 'shift-modifier)
  
    map))


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



(define-minor-mode bpanthi-keys-mode
  "A minor mode so that my key-bindings override annoying major-modes"
  :init-value t
  :lighter " my-keys"
  :keymap bpanthi-keys-minor-mode-map)

(define-globalized-minor-mode bpanthi-global-keys-mode bpanthi-keys-mode
  (lambda () (bpanthi-keys-mode 1)))

(bpanthi-global-keys-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
