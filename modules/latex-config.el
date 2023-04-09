;; copied from https://www.emacswiki.org/emacs/AUCTeX
(require 'cl)
(case system-type
  (darwin
   (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
   (push "/Library/TeX/texbin/" exec-path)))

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

(use-package latex
  :ensure auctex
  :defer t
  :mode "\\.tex\\'"
  :after (tex-mik)
  :hook ((LaTeX-mode .  (lambda () 
			  (latex-math-mode)
			  (latex-electric-env-pair-mode)
			  (visual-line-mode)
			  (turn-on-reftex)
			  (tex-source-correlate-mode)
			  (local-set-key (kbd "C-c e") 'LaTeX-environment)
			  ;; (setq TeX-master (guess-TeX-master (buffer-file-name)))
			  )))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)	
  ;; (load "preview-latex.el" nil t t)
  (when (string-equal system-type "windows-nt")
    
    (setq org-preview-latex-default-process 'dvisvgm))

  ;; Is this required? add these path to your system path
  ;;(setq exec-path (append '("/usr/local/texlive/2019/bin/x86_64-linux/") exec-path))
  ;;(setenv "PATH" (concat "/usr/local/texlive/2019/bin/x86_64-linux:" (getenv "PATH")))))
  (setq org-image-actual-width nil))
