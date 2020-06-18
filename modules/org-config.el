(use-package org-tempo
  :ensure nil
  :defer t
  :after (org tempo)
  :hook (org-mode . (lambda () (require 'org-tempo)))
  :config 
  (add-to-list 'org-tempo-tags '("<f" . tempo-template-fact-with-source))
  (add-to-list 'org-tempo-tags '("<L" . tempo-template-latex-named-equation))
  (add-to-list 'org-tempo-tags '("!s" . tempo-template-source)))

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind (:map bp/global-prefix-map
			  ("o l" . org-store-link)
			  ("o e" . org-emphasize))
  :bind (:map org-mode-map
			  ("M-m o s". 'tempo-template-source)
			  ("M-m o c t". 'bp/org-capture-thought)
			  ("M-m o c n" . 'bp/org-capture-notes))
  :bind (:map org-src-mode-map
			  ("C-c C-c" . org-edit-src-exit))
  :hook (org-mode . (lambda ()
					  (setq ispell-parser 'tex)))
  :after (tempo)
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-hide-emphasis-markers t)
  (smartrep-define-key org-mode-map "M-m o"
	'(("t" . org-todo)))
  ;; Whenever a TODO entry is created, I want a timestamp
  ;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
  (defadvice org-insert-todo-heading (after bp/created-timestamp-advice activate)
	"Insert a CREATED property using org-expiry.el for TODO entries"
	(bp/insert-created-timestamp)
	)
  ;; Make it active
  (ad-activate 'org-insert-todo-heading)
  (defun bp/org-capture-thought ()
	(interactive)
	(org-capture nil "thoughts"))

  (defun bp/org-capture-notes ()
	(interactive)
	(org-capture nil "notes"))

  (add-hook 'org-mode-hook (lambda ()
							 (electric-indent-local-mode nil)
							 (modify-syntax-entry ?< ".")
							 (modify-syntax-entry ?> ".")))
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "e:/Programs/SumatraPDF/SumatraPDF.exe %s")
  (pushnew '("\\.pdf::\\([0-9]+\\)?\\'" .  "e:/Programs/SumatraPDF/SumatraPDF.exe %s -page %1")
		   org-file-apps)
  (require 'ox-latex)
  (setq org-preview-latex-image-directory (if windows-system? "E:/tmp/ltximg/" "/mnt/Data/tmp/ltximg/"))
  (setf org-startup-with-inline-images t
		org-image-actual-width 600
		org-startup-with-latex-preview nil)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
	 (ditaa . t)
	 (dot . t)
	 (shell . t)
	 (emacs-lisp . t)
	 (gnuplot . t)
	 (haskell . nil)
	 (latex . t)
	 (ledger . t)        
	 (ocaml . nil)
	 (octave . t)
	 (python . t)
	 (ruby . t)
	 (screen . nil)
	 (sql . nil)
	 (sqlite . t)
	 (lisp . t)
	 ))
  (setf org-babel-lisp-eval-fn 'sly-eval)
  (setq org-image-actual-width 300)
  (setq org-directory "~/Documents/synced/Notes/org/")
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (setq org-log-done t)

  (let ((last-input ""))
	(defvar-local bp/latex-inputs nil)
	(defun bp/org-insert-inline-latex (latex-fragment)
	  (interactive "sLatex:")
	  (insert-char ?$)
	  (insert latex-fragment)
	  (setf last-input latex-fragment)
	  (pushnew latex-fragment bp/latex-inputs :test #'string-equal)
	  (insert-char ?$)
	  (insert-char ? )
	  (org-latex-preview))

	(defun bp/org-populate-latex ()
	  (interactive)
	  (save-excursion
		(let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
			   (cnt 0)
			   (results nil))		
		  (goto-char (point-min))
		  (while (re-search-forward math-regexp (point-max) t)
			(let* ((context (org-element-context))
				   (type (org-element-type context)))
			  (when (memq type '(latex-environment latex-fragment))
				(let ((block-type (eq type 'latex-environment))
					  (value (org-element-property :value context))
					  (beg (org-element-property :begin context))
					  (end (save-excursion
							 (goto-char (org-element-property :end context))
							 (skip-chars-backward " \r\t\n")
							 (point))))
				  (if (eq type 'latex-fragment)
					  (setf value (subseq value 1 -1)))
				  (pushnew value results :test #'string-equal)))))
		  (setf bp/latex-inputs results))))

	(defun bp/org-insert-last-inline-latex ()
	  (interactive)
	  (let* ((result (ivy-completing-read "Latex: " bp/latex-inputs nil 'confirm nil nil nil))
			 (fragment? (not (string-prefix-p "\begin" result))))
		(pushnew result bp/latex-inputs :test #'string-equal)
		(when result
		  (if fragment? (insert "$"))
		  (insert result)
		  (if fragment? (insert "$"))
		  (org-latex-preview)
		  (insert " "))))	  
	
	(defun bp/org-insert-latex-equation (name latex)
	  "Insert a latex equation that can be referenced"
	  (interactive "sName:\nsLatex:")
	  (if (not (string= name ""))
		  (insert "#+NAME: eqn:"
				  name
				  "\n\\begin{equation}\n"
				  latex
				  "\n\\tag{"
				  name
				  "}\n\\end{equation}\n")
		(insert "\\begin{equation*}\n"
				latex
				"\n\\end{equation*}\n"))
		(org-latex-preview)))

  (bind-keys :map org-mode-map
			 ("M-m o i l" . bp/org-insert-inline-latex)
			 ("M-m o i i" . bp/org-insert-last-inline-latex )
			 ("M-m o i e" . bp/org-insert-latex-equation)
			 ("M-l" . bp/org-insert-last-inline-latex))
  )

;; TODO (require 'org-protocol)
(use-package org-capture
  :defer t
  :after (org)
  :commands (org-capture org-capture-goto-last-saved)
  :config
  (defun transform-square-brackets-to-round-ones(string-to-transform)
	"Transforms [ into ( and ] into ), other chars left unchanged."
	(concat 
	 (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
	)
  (defun capture-in-visited-org-file ()
	(let* ((buffer (current-buffer))
		   (current-point (point)))
	  (goto-char 0)
	  (if (search-forward-regexp "^\* Thoughts" nil t)
		  (forward-line)
		(progn (goto-char (point-max))
			   (insert "\n* Thoughts")))))

  (defun capture-note-in-current-heading ()
	(let* ((buffer (current-buffer))
		   (current-point (point)))
	  (if (search-forward-regexp "^\*+ Notes" nil t)
		  (forward-line)
		(progn (goto-char (point-max))
			   (insert "\n* Notes")))))

  (setq org-capture-templates `(
								("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
								 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")	
								("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
								 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
								("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
								 "* TODO %?\nCREATED: %U\n %i\n  %a")
								("j" "Journal" entry (file+datetree "~/org/journal.org")
								 "* %?\nEntered on %U\n  %i\n  %a")
								("n" "Note" entry (file "~/org/notes.org" )
								 "* %?")
								("k" "Quote" item (file+headline "~/org/notes.org" "Quotes")
								 "%? :: %x")
								("thoughts" "Capture Thoughts in a heading at bottom of file" item (function capture-in-visited-org-file)
								 "+ %?")
								("notes" "Capture notes below current heading" item (function capture-note-in-current-heading)
								 "+ %?")
								))
  :init
  (bind-keys :map bp/global-prefix-map
			 ("o c c" . org-capture)
			 ("o c l" . org-capture-goto-last-stored)))

(use-package tempo
  :config
  (setq tempo-interactive t)
  (tempo-define-template "fact-with-source"
						 '("+ " (p "Fact: ") "([[" (p "Source:") "][Source]])")
						 "<f"
						 "Inserts a fact with source")
  (tempo-define-template "source"
						 '("([[" (p "Source:") "][Source]])")
						 "!s"
						 "Insert a (Source)"))
 


;; (use-package org-download
;;   :ensure t
;;   :defer t)

(use-package gnuplot
  :ensure t
  :defer t
  :after (gnuplot-mode gnuplot-make-buffer)
  :mode "\\.gp\\'")

(use-package ox-latex
  :defer t  
  :config
  ;; For proper rendering of unicode symbols on latex
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  ;; You also need to bug fix the working of `Tranparent' in org.el.
  ;; See personal notes and also install librsvg-2-2.dll 
  (setq org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 1.5)

  (defadvice text-scale-increase (after bp/latex-preview-scaling-on-text-scaling activate)
	(plist-put org-format-latex-options :scale (* 1.5 (expt 1.2 text-scale-mode-amount))))
  
  (setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist)))

(use-package org-agenda
  :bind (:map bp/global-prefix-map
			  ("o a" . org-agenda))
  :config
  (setq org-agenda-files (list "~/org/notes.org"
							   "~/org/tasks.org"
							   "~/org/programming.org"
							   )))

;; Allow automatically handing of created/expired meta data.
;; in TODOs 

;; Configure it a bit to my liking
(use-package org-expiry
  :defer t
  :after (org)
  :commands (bp/insert-created-timestamp)
  :config 
  (setq
   org-expiry-created-property-name "CREATED" ; Name of property when an item is created
   org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
   )
  
  (defun bp/insert-created-timestamp()
	"Insert a CREATED property using org-expiry.el for TODO entries"
	(interactive)
	(org-expiry-insert-created)
	(org-back-to-heading)
	(org-end-of-line)
	(insert " ")
	))

(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory (if windows-system? "E:/Documents/synced/Notes/org/" "~/Documents/synced/Notes/org/"))
  (when windows-system?
	(setq org-roam-list-files-commands '((find . "C:/tools/msys64/usr/bin/find.exe") rg)))
  :bind (:map org-roam-mode-map
		 (("M-m r l" . org-roam)
		  ("M-m r f" . org-roam-find-file)
		  ("M-m r j" . org-roam-jump-to-index)
		  ("M-m r b" . org-roam-switch-to-buffer)
		  ("M-m r g" . org-roam-graph))
		 :map bp/global-prefix-map
		 (("r f" . org-roam-find-file))
		 :map org-mode-map
		 (("M-m o r" . org-roam-insert)
		  ("M-m r i" . org-roam-insert)
		  ("M-m r t" . bp/org-roam-tags)
		  ("M-m r a" . bp/org-roam-alias)
		  ("M-m r d" . org-roam-db-build-cache)
		  ("M-m r l" . org-roam)))
  :config
  (require 'ivy)
  (setq org-roam-db-location
		(cond ((string-equal system-type "gnu/linux")
			   (expand-file-name "dbs/linux/org-roam.db" org-roam-directory))
			  ((string-equal system-type "windows-nt")
			   (expand-file-name "dbs/windows/org-roam.db" org-roam-directory))))

  (add-hook 'org-mode-hook 'org-roam-mode)
  (defun bp/org-roam-headers (header)
	(interactive)
	(goto-char 0)
	(xref-push-marker-stack)
	(if (search-forward (concat "\n" header) nil t)
		(progn
		  (move-end-of-line 1)
		  (unless (eql (char-before) ?\ )
			(insert " ")))
	  (progn
		(goto-line 2)
		(insert header " \n")
		(move-end-of-line 0))))

  (defun bp/org-roam-tags ()
	(interactive)
	(bp/org-roam-headers "#+ROAM_TAGS:"))
  
  (defun bp/org-roam-alias ()
	(interactive)
	(bp/org-roam-headers "#+ROAM_ALIAS:")))

(defun  bp/org-company ()
  (interactive)
  (setf company-backends '(company-dabbrev)))
