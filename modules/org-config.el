(defun bp/lecture-position () 
  (interactive)
  (set-frame-width (selected-frame) 80)
  (set-frame-height (selected-frame) 58))

;;; Org mode 
(use-package org
;;;; Bindings and hooks 
  :mode (("\\.org$" . org-mode))
  :bind (:map bp/global-prefix-map
	      ("o l" . org-store-link)
	      ("o e" . org-emphasize))
  :bind (:map org-mode-map
	      ("M-m o h" . 'bp/org-view-html-export)
	      ("M-m o s". 'bp/org-source-template)
	      ("M-m o c t". 'bp/org-capture-thought)
	      ("M-m o c n" . 'bp/org-capture-notes))
  :bind (:map org-src-mode-map
	      ("C-c C-c" . org-edit-src-exit))
  :hook (org-mode . (lambda ()
		      (org-content 2)
		      (electric-indent-mode -1)
		      (setq ispell-parser 'tex)))
  :config
;;;; requirements
  (require 'ox-latex)
  (require 'org-attach)
;;;; Exporting
;;;;; Exports to './output' directory
  (defvar org-export-output-directory-prefix "output" "prefix of directory used for org-mode export")
  (defadvice org-export-output-file-name (before org-add-export-dir activate)
    "Modifies org-export to place exported files in a different directory"
    (when (not pub-dir)
      (setq pub-dir org-export-output-directory-prefix)
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir))))

;;;;; Html Export Theming 
  ;; org html CSS theme
  ;; from https://gongzhitaao.org/orgcss/org.css
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-head-include-default-style nil)
  (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"/home/bpanthi/.emacs.d/modules/org.css\">")
  

  (defun bp/org-view-html-export () 
    (interactive)
    (let ((org-export-show-temporary-export-buffer nil))
      (org-html-export-as-html nil)
      (browse-url-of-buffer "*Org HTML Export*")))

;;;;; Comments in between paragraphs 
;; This allows comments in between a paragraph
  (defun delete-org-comments (backend)
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
				      'comment 'identity))
	  do
	  (setf (buffer-substring (org-element-property :begin comment)
				  (org-element-property :end comment))
		"")))
  (add-hook 'org-export-before-processing-hook 'delete-org-comments)
;;;; Better Org Outline bindings for show/hide while navigating 
  (defun bp/org-just-show-this ()
    (interactive)
    (org-content 2)
    (org-show-entry))

  (define-prefix-command 'bp/org-prefix-map)
  (define-key global-map (kbd "M-o") 'bp/org-prefix-map)
  
  (smartrep-define-key bp/org-prefix-map
      ""
    '(("	" . bp/org-just-show-this)
      ("f" . org-forward-heading-same-level)
      ("b" . org-backward-heading-same-level)
      ("n" . org-next-visible-heading)
      ("p" . org-previous-visible-heading)
      ("C-n" . next-line)
      ("C-p" . previous-line)
      ("s" . bp/org-just-show-this)
      ("N" . (lambda  ()
	       (org-next-visible-heading 1)
	       (bp/org-just-show-this)))
      ("P" . (lambda () 
	       (org-previous-visible-heading 1)
	       (bp/org-just-show-this)))
      ("F" . (lambda () 
	       (org-forward-heading-same-level)
	       (bp/org-just-show-this)))
      ("B" . (lambda () 
	       (org-backward-heading-same-level)
	       (bp/org-just-show-this)))))

;;;; Customizations
  (setq org-src-window-setup 'current-window)
  (setq org-hide-emphasis-markers t)
  (setf org-startup-with-inline-images t
	org-image-actual-width 500
	org-startup-with-latex-preview nil
	org-startup-folded 'content)

  (setq org-directory "~/Documents/synced/Notes/org/")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-log-done t)
  
  ;; Template for source of a fact
  (defun bp/org-source-template (link)
    (interactive "sSource:")
    (insert "([[" link "][Source]])"))

  

;;;;; PDF Viewers
  (if windows-system?
      (progn 
	(setcdr (assoc "\\.pdf\\'" org-file-apps) "e:/Programs/SumatraPDF/SumatraPDF.exe %s")
	(pushnew '("\\.pdf::\\([0-9]+\\)?\\'" .  "e:/Programs/SumatraPDF/SumatraPDF.exe %s -page %1")
		 org-file-apps))
    (progn
      (pushnew '("\\.pdf\\'" . bp/okl-note-link) org-file-apps)
      (pushnew '("\\.pdf::\\([0-9]+\\)?\\'" . bp/okl-note-link)
	       org-file-apps)))

;;;;; HTML Viewer 
  (defun bp/open-html-file (file link)
    (declare (ignore link))
    (browse-url-of-file file))

  (pushnew '("\\.html\\'" . bp/open-html-file)
	   org-file-apps)

;;;; Timestamp in TODO Heading
  ;; Switch between TODO, DONE and COMPLETED 
  ;; (smartrep-define-key org-mode-map "M-m o"
  ;;   '(("t" . org-todo)))

  ;; Whenever a TODO entry is created, I want a timestamp
  ;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
  (defadvice org-insert-todo-heading (after bp/created-timestamp-advice activate)
    "Insert a CREATED property using org-expiry.el for TODO entries"
    (bp/insert-created-timestamp))

  (ad-activate 'org-insert-todo-heading)
;;;; Image scaling with text 
  (defadvice text-scale-increase (after bp/image-scaling-on-text-scaling activate)
    (setq org-image-actual-width (list (truncate (* 500 (expt text-scale-mode-step text-scale-mode-amount)))))
    (org-redisplay-inline-images))

;;;; Org capture functions for thoughts and notes in org file 
  (defun bp/org-capture-thought ()
    (interactive)
    (org-capture nil "thoughts"))

  (defun bp/org-capture-notes ()
    (interactive)
    (org-capture nil "notes"))

  ;; (add-hook 'org-mode-hook (lambda ()
  ;; 			     (electric-indent-local-mode nil)
  ;; 			     (modify-syntax-entry ?< ".")
  ;; 			     (modify-syntax-entry ?> ".")))

;;;; org-babel  
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
     (maxima . t)
     (ruby . t)
     (screen . nil)
     (sql . nil)
     (sqlite . t)
     (lisp . t)
     (C . t)
     (shell . t)
     ))
  (setf org-babel-lisp-eval-fn 'slime-eval)

;;;; Linking org files across fs to ~/org/ 
  (defun bp/link-to-~org ()
    (interactive)
    (make-symbolic-link (buffer-file-name) "~/org/"))
  )

;;; Org Latex

(use-package ox-latex
  :defer t
  :commands (bp/org-insert-inline-latex bp/org-insert-last-inline-latex  bp/org-insert-latex-equation bp/org-insert-last-inline-latex)
  :config
   ;; For proper rendering of unicode symbols on latex
  (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
  (setq org-latex-default-packages-alist (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))

  ;; You also need to bug fix the working of `Tranparent' in org.el.
  ;; See personal notes and also install librsvg-2-2.dll 
  (setq org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :scale 1.5)

  ;; Don't clutter my directiory 
  (setq org-preview-latex-image-directory (if windows-system? "E:/tmp/ltximg/" "/mnt/Data/tmp/ltximg/"))

  ;; for latex in odt files 
  (setq org-latex-to-mathml-convert-command
	"latexmlmath \"%i\" --presentationmathml=%o"
	org-export-with-latex t)
;;;; Latex preview size scaling
  (defadvice text-scale-increase (after bp/latex-preview-scaling-on-text-scaling activate)
    (plist-put org-format-latex-options :scale (* 1.2 (/ (frame-char-height) 17) (expt text-scale-mode-step text-scale-mode-amount))))


  (defun bp/calculate-ascent-for-latex (text type)
    (cond ((eql type 'latex-environment) 'center)
	  ((eql type 'latex-fragment)
	   (cond ((find ?| text) 70)
		 ((find ?_ text) 80)
		 ((search "\\neq" text) 70)
		 (t 100)))
	  (t (error "Unknown latex type"))))
  
;;;; Latex inserting 
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

;;;; Document Classes 
;;;;; Elsevier Article

  (add-to-list 'org-latex-classes
 '("elsarticle"
     "
\\documentclass[final,5p,times,twocolumn]{elsarticle}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[EXTRA]

%% The graphicx package provides the includegraphics command.
\\usepackage{graphicx}
%% The amssymb package provides various useful mathematical symbols
\\usepackage{amssymb}
%% The amsthm package provides extended theorem environments
%% \\usepackage{amsthm}

%% natbib.sty is loaded by default. However, natbib options can be
%% provided with \\biboptions{...} command. Following options are
%% valid:

%%   round  -  round parentheses are used (default)
%%   square -  square brackets are used   [option]
%%   curly  -  curly braces are used      {option}
%%   angle  -  angle brackets are used    <option>
%%   semicolon  -  multiple citations separated by semi-colon
%%   colon  - same as semicolon, an earlier confusion
%%   comma  -  separated by comma
%%   numbers-  selects numerical citations
%%   super  -  numerical citations as superscripts
%%   sort   -  sorts multiple citations according to order in ref. list
%%   sort&compress   -  like sort, but also compresses numerical citations
%%   compress - compresses without sorting
%%
%% \\biboptions{comma,round}
% \\biboptions{}
\\usepackage{hyperref}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setf org-latex-title-command nil)


;;;;; Ethz (Good Theme) 
  (add-to-list 'org-latex-classes
	       '("ethz"
		 "\\documentclass[a4paper,11pt,titlepage]{memoir}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{fixltx2e}
  \\usepackage{graphicx}
  \\usepackage{longtable}
  \\usepackage{float}
  \\usepackage{wrapfig}
  \\usepackage{rotating}
  \\usepackage[normalem]{ulem}
  \\usepackage{amsmath}
  \\usepackage{textcomp}
  \\usepackage{marvosym}
  \\usepackage{wasysym}
  \\usepackage{amssymb}
  \\usepackage{hyperref}
  \\usepackage{mathpazo}
  \\usepackage{color}
  \\usepackage{enumerate}
  \\definecolor{bg}{rgb}{0.95,0.95,0.95}
  \\tolerance=1000
	[NO-DEFAULT-PACKAGES]
	[PACKAGES]
	[EXTRA]
  \\linespread{1.1}
  \\hypersetup{pdfborder=0 0 0}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;;;; Better defaults 
  (add-to-list 'org-latex-classes
	       '("article"
		 "\\documentclass[11pt,a4paper]{article}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{fixltx2e}
  \\usepackage{graphicx}
  \\usepackage{longtable}
  \\usepackage{float}
  \\usepackage{wrapfig}
  \\usepackage{rotating}
  \\usepackage[normalem]{ulem}
  \\usepackage{amsmath}
  \\usepackage{textcomp}
  \\usepackage{marvosym}
  \\usepackage{wasysym}
  \\usepackage{amssymb}
  \\usepackage{hyperref}
  \\usepackage{mathpazo}
  \\usepackage{color}
  \\usepackage{enumerate}
  \\definecolor{bg}{rgb}{0.95,0.95,0.95}
  \\tolerance=1000
	[NO-DEFAULT-PACKAGES]
	[PACKAGES]
	[EXTRA]
  \\linespread{1.1}
  \\hypersetup{pdfborder=0 0 0}"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))


  (add-to-list 'org-latex-classes '("ebook"
				    "\\documentclass[11pt, oneside]{memoir}
  \\setstocksize{9in}{6in}
  \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
  \\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
  \\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
  \\checkandfixthelayout
  % Much more laTeX code omitted
  "
				    ("\\chapter{%s}" . "\\chapter*{%s}")
				    ("\\section{%s}" . "\\section*{%s}")
				    ("\\subsection{%s}" . "\\subsection*{%s}")))
;;;; init key bindings
  :init 
  (with-eval-after-load "org"
    (bind-keys :map org-mode-map
	       ("M-m o i l" . bp/org-insert-inline-latex)
	       ("M-m o i i" . bp/org-insert-last-inline-latex )
	       ("M-m o i e" . bp/org-insert-latex-equation)
	       ("M-l" . bp/org-insert-last-inline-latex))))

;;; org-attach
(use-package org-attach 
  :defer t 
  :config 
  (setq org-attach-auto-tag nil
	org-attach-method "mv"
	org-attach-preferred-new-method 'dir
	org-attach-id-dir ".data/"
	org-attach-use-inheritance t)

  (defun bp/org-link--get-file ()
    "return path of attachment or normal file link file"
    (let ((context (org-element-context)))
      (if (eql (org-element-type context) 'link)
	  (let ((link-type (org-element-property :type context)))
	    (cond 
	     ((equal link-type "attachment") 
	      (file-truename (org-attach-expand (org-element-property :path context))))
	     ((equal link-type "file")
	      (file-truename (org-element-property :path context)))))
	(error "Point not in attachment or link"))))

  (defun bp/org-tesseract-at-point ()
    (interactive)
    (let ((file (bp/org-link--get-file)))
      (when file 
	(let ((string (bp/tesseract-on-file file)))
	  (insert "\n" string))))))

;;; Org Agenda 
(use-package org-agenda
  :defer t 
  :bind (:map bp/global-prefix-map
	      ("o a" . org-agenda))
  :config
  (setq org-agenda-files (list "~/org/notes.org"
			       "~/org/tasks.org"
			       "~/org/programming.org"
			       )))
;;; Org capture   
(use-package org-capture
  :defer t
  :commands (org-capture org-capture-goto-last-saved)
  :config
  (require 'org)
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

;;; Gnuplot 
(use-package gnuplot
  :ensure nil
  :defer t
  :after (gnuplot-mode gnuplot-make-buffer)
  :mode "\\.gp\\'"
  :init
  (setf gnuplot-program-version "5.2"
	gnuplot-program-major-version 5
	gnuplot-program-minor-version 2)
  )

;;; Comments

;; Allow automatically handing of created/expired meta data.
;; in TODOs 

;; Configure it a bit to my liking
;; (use-package org-expiry
;;   :defer t
;;   :after (org)
;;   :commands (bp/insert-created-timestamp)
;;   :config 
;;   (setq
;;    org-expiry-created-property-name "CREATED" ; Name of property when an item is created
;;    org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
;;    )

;;   (defun bp/insert-created-timestamp()
;;     "Insert a CREATED property using org-expiry.el for TODO entries"
;;     (interactive)
;;     (org-expiry-insert-created)
;;     (org-back-to-heading)
;;     (org-end-of-line)
;;     (insert " ")
;;     ))

;;; bp/org-company saner company setting for org mode 
(defun  bp/org-company ()
  (interactive)
  (setf company-backends '(company-dabbrev)))


;;; Research
;;;; Org Ref
(use-package org-ref
  :ensure t
  :defer t 
  :config
  (require 'bibtex)
  (bind-keys :map bibtex-mode-map
	     ("M-m o r p" . org-ref-bibtex-pdf)
	     ("M-m o r b" . org-ref-bibtex-hydra/body)
	     :map org-mode-map
	     ("M-m o i c" . org-ref-insert-link)
	     ("M-m o i r" . org-ref-insert-ref-link)
	     ("M-m o i k" . org-ref-helm-insert-label-link)
	     ("M-m o x" . org-ref-cite-hydra/body))

  ;; setup org-ref 
  (setq org-ref-bibliography-notes "~/org/bibliography/notes.org"
	org-ref-default-bibliography '("~/org/bibliography/references.bib")
	org-ref-pdf-directory "~/org/bibliography/papers/"
	bibtex-completion-pdf-field "pdf"
	org-ref-get-pdf-filename-function #'org-ref-get-pdf-filename-helm-bibtex)
  ;; may prevent slow down https://github.com/jkitchin/org-ref/issues/468
  (setq org-ref-show-broken-links nil)
  (setq org-ref-label-use-font-lock nil)


  ;;orhc-candidate-formats
  

  ;; this makes org-ref use same format as bibtex-completion-notes-path
  (setf org-ref-notes-function 'org-ref-notes-function-many-files)
  
  (setf doi-utils-open-pdf-after-download t)
  (require 'doi-utils-scihub)
  (setq dbus-debug nil))

;;;; Bibtex
(use-package bibtex
  :ensure t
  :defer t 
  :config
  (setq bibtex-completion-bibliography "~/org/bibliography/references.bib"
	bibtex-completion-library-path "~/org/bibliography/papers"
	bibtex-completion-notes-path "~/org/bibliography/notes.org"   ;; uses bibtex-completion-notes-template-one-file
	bibtex-completion-pdf-open-function 'org-open-file
	bibtex-completion-notes-template-one-file
	"
* ${author-abbrev} - ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :AUTHOR: ${AUTHOR}
  :YEAR: ${year}
  :JOURNAL: ${journal}
  :DOI: ${DOI}
  :URL: ${url}
  :END:

cite:${=key=}
"))

;;;; Varibles setup 
(defun bp/setup-research-dir-local-variables ()
  (interactive)
  (let ((dir (file-name-directory (buffer-file-name))))
    (add-dir-local-variable 'org-mode  'org-roam-directory dir)
    (add-dir-local-variable 'org-mode
			    'org-roam-db-location (expand-file-name (format "org-roam-%s.db" (if windows-system? "w" "l")) dir))
    (add-dir-local-variable 'org-mode
			    'bibtex-completion-bibliography (expand-file-name "references.bib" dir))
    (add-dir-local-variable 'org-mode
			    'org-ref-default-bibliography (list (expand-file-name "references.bib" dir)))
    (add-dir-local-variable 'org-mode
			    'bibtex-completion-notes-path (expand-file-name "notes.org" dir))
    (add-dir-local-variable 'org-mode
			    'org-ref-bibliography-notes (expand-file-name "notes.org" dir))
    (add-dir-local-variable 'org-mode
			    'bibtex-completion-library-path (expand-file-name "papers/" dir))    
    (add-dir-local-variable 'org-mode
			    'org-ref-pdf-directory (expand-file-name "papers/" dir))))

(defun bp/setup-research-folder-variables ()
  (interactive)
  (let ((dir (file-name-directory (buffer-file-name))))
    (setq org-roam-directory dir)
    (setq org-roam-db-location (expand-file-name (format "org-roam-%s.db" (if windows-system? "w" "l")) dir))
    (setq bibtex-completion-bibliography (expand-file-name "references.bib" dir))
    (setq org-ref-default-bibliography (list (expand-file-name "references.bib" dir)))
    (setq bibtex-completion-notes-path (expand-file-name "notes.org" dir))
    (setq org-ref-bibliography-notes (expand-file-name "notes.org" dir))
    (setq bibtex-completion-library-path (expand-file-name "papers/" dir))    
    (setq org-ref-pdf-directory (expand-file-name "papers/" dir))))


;;; Org-roam
(use-package org-roam
  :ensure t
  :defer t
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
  (setf org-roam-mode t)
  (setq org-roam-directory
	(if windows-system? "E:/Documents/synced/Notes/org/" "/mnt/Data/Documents/synced/Notes/org/"))
  (when windows-system?
    (setq org-roam-list-files-commands '((find . "C:/tools/msys64/usr/bin/find.exe") rg)))
  (require 'ivy)
  (setq org-roam-graph-viewer nil)
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-db-location
	(cond ((string-equal system-type "gnu/linux")
	       (expand-file-name "dbs/linux/org-roam.db" org-roam-directory))
	      ((string-equal system-type "windows-nt")
	       (expand-file-name "dbs/windows/org-roam.db" org-roam-directory))))

  ;;  (add-hook 'org-mode-hook 'org-roam-mode)
  
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

;;; Org roam server

(use-package org-roam-server
  :ensure t
  :defer t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  :init 
  (defun bp/org-roam-server ()
    (interactive)
    (org-roam-server-mode t)
    (browse-url "http://127.0.0.1:8080"))
  (bind-keys :map bp/global-prefix-map
	     ("r s" . bp/org-roam-server)))


;;; org-download
(use-package org-download 
  :ensure t 
  :defer t
  :commands (org-download-screenshot) 
  :config 
  (defvar bp/org-download-screenshot-title nil)
  (defun bp/org-download-file-formater (filename)
    "Asks the user for file name"
    (let* ((title (completing-read "Title:"  
				 (list  (current-kill 0))
				 nil nil))
	   (slug (bp/title-to-slug title))
	   (file (concatenate 'string (format-time-string "%Y%m%d%H%M%S-") slug ".png")))
      (setf bp/org-download-screenshot-title title)
      file))

  (defun bp/title-to-slug (title)
    "Copied from `org-roam--title-to-slug'
Convert TITLE to a filename-suitable slug."
    (cl-flet* ((nonspacing-mark-p (char)
                                  (eq 'Mn (get-char-code-property char 'general-category)))
               (strip-nonspacing-marks (s)
                                       (apply #'string (seq-remove #'nonspacing-mark-p
                                                                   (ucs-normalize-NFD-string s))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_")  ;; convert anything not alphanumeric
                      ("__*" . "_")  ;; remove sequential underscores
                      ("^_" . "")  ;; remove starting underscore
                      ("_$" . "")))  ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
	(downcase slug))))

  (defun bp/org-download-annotate-with-title (link)
    (prog1
	(when bp/org-download-screenshot-title
	  (concatenate 'string  "#+CAPTION: " bp/org-download-screenshot-title "\n"))
      (setf bp/org-download-screenshot-title nil)))

  (setq org-download-method 'attach 
	org-download-screenshot-method "xfce4-screenshooter -r -o cat > %s"
	org-download-file-format-function #'bp/org-download-file-formater
	org-download-annotate-function #'bp/org-download-annotate-with-title)
  
  :init 
  (bind-keys :map bp/global-prefix-map 
	     ("o d s" . org-download-screenshot))
  (global-set-key (kbd "M-s") 'org-download-screenshot))

;;; org-present
(use-package org-present
  :ensure t
  :defer t 
  :config 
  (add-hook 'org-present-after-navigate-functions
	    (lambda (buffer current-heading) 
	      (declare (ignore buffer current-heading))
	      (org-display-inline-images)
	      (org--latex-preview-region (point-min) (point-max))))
  )
