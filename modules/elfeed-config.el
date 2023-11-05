(use-package elfeed
  :ensure t
  :demand nil
  :defer t
  :config
  ;; load feeds from org/elfeed.el file (which isn't in public repo)
  (let ((file (expand-file-name "elfeed.el" org-directory)))
    (when (file-exists-p file)
      (load file)))
  'elfeed-show-mode
  (add-hook 'elfeed-show-mode-hook #'visual-line-mode))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :demand t
  :defer t
  :config
  (setq elfeed-tube-auto-save-p nil)
  (setq elfeed-tube-auto-fetch-p t)
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t
  :defer t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))
