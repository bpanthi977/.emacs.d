(use-package rclone-sync
  :defer t
  :commands (rclone-copy-directory
             rclone-copy-file
             rclone-run-backup-tasks
             rclone-sync-directory
             rclone-reverse-copy-directory
             rclone-run-file))

(defun rclone-generate-filter-file ()
  (interactive)
  (rclone--async-shell-command (concat "~/Development/rclone-filter.sh \"" (shell-quote-argument (buffer-file-name)) "\"")))

(bind-keys :map bp/global-prefix-map
           ("s r f" . rclone-copy-file)
           ("s r d" . rclone-copy-directory)
           ("s r s" . rclone-sync-directory)
           ("s r r" . rclone-reverse-copy-directory)
           ("s r g" . rclone-generate-filter-file))

(use-package calculator
  :defer t)

(bind-keys :map bp/global-prefix-map
           ("t c" . calc)
           ("t d" . dictionary-search))

;;(setf which-key-enable-extended-define-key t)
;;(define-key bp/global-prefix-map "s" '("Save"))

(use-package anki-editor
  :ensure nil
  :defer t
  :config
  (setf anki-editor-create-decks t))

(defun bp/tesseract-on-file (file)
  (save-window-excursion
    (let ((buffer (generate-new-buffer "tesseract-ocr"))
          (errbuffer (generate-new-buffer "tesseract-ocr-err")))
      (shell-command (format "tesseract \"%s\" -" (file-truename file) ) buffer errbuffer)
      (let ((string (with-current-buffer  buffer
                      (buffer-string))))
        (kill-buffer buffer)
        (kill-buffer errbuffer)
        (remove ? string)))))

(defun bp/capture-screenshot ()
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "screenshot"))))
    (shell-command-to-string (format "xfce4-screenshooter -r -o cat > %s"
                                     filename))
    (when (file-exists-p filename)
      (insert (bp/tesseract-on-file filename)))))

(bind-keys :map bp/global-prefix-map
           ("e o" . bp/capture-screenshot))

(use-package pdf-tools
  :pin manual  ;; because package updates may require rebuilding the executable
  :defer t
  :init
  ;; NOTE: line numbering mode may hang pdf-view mode

  ;; http://pragmaticemacs.com/emacs/view-and-annotate-pdfs-in-emacs-with-pdf-tools/
  ;; initialise
  ;;(pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights (Highlight using {C-c C-a h} )
  ;; (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch

  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (defun bp/pdf-slight-up ()
    (interactive)
    (pdf-view-scroll-up-or-next-page 12))

  (defun bp/pdf-slight-down ()
    (interactive)
    (pdf-view-scroll-down-or-previous-page 12))

  (defun bp/pdf-highlight-and-take-note ()
    (interactive)
    (let ((pdf-annot-activate-created-annotations nil)
          (strings (pdf-view-active-region-text)))
      (kill-new (apply #'concatenate 'string strings))
      (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region nil)))
    (org-noter-insert-precise-note))

  (bind-keys :map pdf-view-mode-map
             ((kbd "x") . bp/pdf-slight-up)
             ((kbd "z") . bp/pdf-slight-down)
             ((kbd "C-c i") . bp/pdf-highlight-and-take-note)))

(use-package nov
  :ensure t
  :config
  ;; https://emacs.stackexchange.com/a/70894
  (defun bp/org-nov-open-new-window (path)
    "Open nov.el link in a new window"
    (setq available-windows
          (delete (selected-window) (window-list)))
    (setq new-window
          (or (car available-windows)
              (split-window-sensibly)
              (split-window-right)))
    (select-window new-window)
    (nov-org-link-follow path))

  (defun bp/nov-find-buffer-visiting (file)
    (let ((truename (file-truename file)))
      (find-if (lambda (buffer)
                 (with-current-buffer buffer
                   (string-equal nov-file-name truename)))
               (buffer-list))))

  (defun bp/org-nov-jump (path)
    (if (string-match "^\\(.*\\)::\\([0-9]+\\):\\([0-9]+\\)$" path)
        (let* ((file (match-string 1 path))
               (index (string-to-number (match-string 2 path)))
               (point (string-to-number (match-string 3 path)))

               (buffer (bp/nov-find-buffer-visiting file)))
          (if (not buffer)
              (bp/org-nov-open-new-window path)
            (save-excursion
              (print buffer)
              (switch-to-buffer-other-frame buffer)
              (nov--find-file nil index point))))
      (error "Invalid nov.el link")))

  (org-link-set-parameters "nov" :follow #'bp/org-nov-jump))

(use-package dictionary
  :ensure t
  :commands (dictionary-search)
  :defer t
  :config
  (setf dictionary-server "localhost"))
