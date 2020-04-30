(install-packages '(org-plus-contrib))
(use-package org-tempo
  :ensure nil
  :defer t
  :after (org tempo)
  :config 
  (add-to-list 'org-tempo-tags '("<f" . tempo-template-fact-with-source))
  (add-to-list 'org-tempo-tags '("!s" . tempo-template-source)))

(use-package org
  :mode "\\.org\\'"
  :hook (org-mode . auto-fill-mode)
  :bind (:map bp/global-prefix-map
			  ("o l" . org-store-link)
			  ("o e" . org-emphasize))
  :bind (:map org-mode-map
			  ("M-m o s". 'tempo-template-source)
			  ("M-m o t". 'bp/org-capture-thought))
  :config

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

  (add-hook 'org-mode-hook (lambda ()
							 (modify-syntax-entry ?< ".")
							 (modify-syntax-entry ?> ".")))
  (progn (setcdr (assoc "\\.pdf\\'" org-file-apps) "e:/Programs/SumatraPDF/SumatraPDF.exe %s"))

  (setq org-preview-latex-image-directory "E:/tmp/ltximg/")
  (setf org-startup-with-inline-images t
		org-startup-with-latex-preview t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
	 (ditaa . t)
	 (dot . t)
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
	 (lisp . t)))
  (setq org-image-actual-width 300)

  (setq org-directory "~/Documents/synced/Notes/org/")
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (setq org-log-done t)

  )

;; TODO (require 'org-protocol)
(use-package org-capture
  :defer t
  :after (org)
  :commands (org-capture)
  :bind (:map bp/global-prefix-map
			  ("o c" . org-capture))
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
								)))

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
