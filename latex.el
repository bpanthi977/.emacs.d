(install-packages (list 'auctex))
(require-packages (list 'tex 'latex))

(setq exec-path (append '("/usr/local/texlive/2019/bin/x86_64-linux/") exec-path))
(setenv "PATH" (concat "/usr/local/texlive/2019/bin/x86_64-linux:" (getenv "PATH")))
(setq org-image-actual-width nil)


;; copied from https://www.emacswiki.org/emacs/AUCTeX
(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))

(defun my-tex-mode ()
  (latex-math-mode)
  (latex-electric-env-pair-mode)
  (visual-line-mode)
  (turn-on-reftex)
  (tex-source-correlate-mode)
  (local-set-key (kbd "C-c e") 'LaTeX-environment)
  ;; (setq TeX-master (guess-TeX-master (buffer-file-name)))
  )
 
(add-hook 'LaTeX-mode-hook 'my-tex-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)
