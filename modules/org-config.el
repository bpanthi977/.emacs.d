;; -*- eval: (outshine-mode); -*-
;; * Org mode
(require 'cl)

(use-package org
;; ** Bindings and hooks
  :defer nil
  :mode (("\\.org$" . org-mode))
  :bind (:map bp/global-prefix-map
              ("o l" . org-store-link)
              ("o L" . org-id-store-link)
              ("o e" . org-emphasize))
  :bind (:map org-mode-map
              ("M-m o h" . 'bp/org-view-html-export)
              ("M-m o s". 'bp/org-source-template)
              ("M-m o c t". 'bp/org-capture-thought)
              ("M-m o c n" . 'bp/org-capture-notes)
              ("M-m o c d" . 'bp/org-capture-definitions)
              ("M-m o v" . org-redisplay-inline-images))
  :bind (:map org-src-mode-map
              ("C-c C-c" . org-edit-src-exit))
  :hook (org-mode . (lambda () (bp/org-mode-hook)))
  :config

  (defun bp/org-mode-hook ()
    (org-content 2)
    (smartparens-mode 1)
    (setf org-pretty-entities t)
    (modify-syntax-entry ?$ "$$" org-mode-syntax-table)
    (setf org-pretty-entities-include-sub-superscripts nil)
    ;; a more noticable bold face for org mode in dark themes (spacemacs-dark)
    (face-remap-add-relative 'bold '(:weight bold :foreground "green3"))
    ;; auto-fill-mode
    (auto-fill-mode)
    (setq default-justification 'left)
    ;;(org-cdlatex-mode)
    (electric-indent-mode -1)
    (setq ispell-parser 'tex))

;; ** Indent
(setf org-startup-indented t)
;; ** requirements
  (require 'ox-latex)
;;  (require 'cdlatex)
  (require 'org-attach)
  ;;(require 'org-ref)
  (require 'org-id)
  (require 'ol-man)
  (require 'ol-info)
;; ** Exporting
;; *** Export to html with useful anchors
  ;; Adpated from https://github.com/alphapapa/unpackaged.el#export-to-html-with-useful-anchors
  (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)

  (defun unpackaged/org-export-get-reference (datum info)
    "Like `org-export-get-reference', except uses heading titles instead of random numbers."
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((crossrefs (plist-get info :crossrefs))
                 (cells (org-export-search-cells datum))
                 ;; Preserve any pre-existing association between
                 ;; a search cell and a reference, i.e., when some
                 ;; previously published document referenced a location
                 ;; within current file (see
                 ;; `org-publish-resolve-external-link').
                 ;;
                 ;; However, there is no guarantee that search cells are
                 ;; unique, e.g., there might be duplicate custom ID or
                 ;; two headings with the same title in the file.
                 ;;
                 ;; As a consequence, before re-using any reference to
                 ;; an element or object, we check that it doesn't refer
                 ;; to a previous element or object.
                 (new (or (cl-some
                           (lambda (cell)
                             (let ((stored (cdr (assoc cell crossrefs))))
                               (when stored
                                 (let ((old (org-export-format-reference stored)))
                                   (and (not (assoc old cache)) stored)))))
                           cells)
                          (when (org-element-property :raw-value datum)
                            ;; Heading with a title
                            (unpackaged/org-export-new-title-reference datum cache))
                          ;; NOTE: This probably breaks some Org Export
                          ;; feature, but if it does what I need, fine.
                          (org-export-format-reference
                           (org-export-new-reference cache))))
                 (reference-string new))
            ;; Cache contains both data already associated to
            ;; a reference and in-use internal references, so as to make
            ;; unique references.
            (dolist (cell cells) (push (cons cell new) cache))
            ;; Retain a direct association between reference string and
            ;; DATUM since (1) not every object or element can be given
            ;; a search cell (2) it permits quick lookup.
            (push (cons reference-string datum) cache)
            (plist-put info :internal-references cache)
            reference-string))))

  (defun unpackaged/org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
    (cl-macrolet ((inc-suffixf (place)
                    `(progn
                       (string-match (rx bos
                                         (minimal-match (group (1+ anything)))
                                         (optional "--" (group (1+ digit)))
                                         eos)
                                     ,place)
                       ;; HACK: `s1' instead of a gensym.
                       (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                  (match-string 2 ,place)))
                               (suffix (if suffix
                                           (string-to-number suffix)
                                         0)))
                         (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
      (let* ((title (org-element-property :raw-value datum))
             (ref (url-hexify-string (substring-no-properties title)))
             (parent (org-element-property :parent datum)))
        (while (--any (equal ref (car it))
                      cache)
          ;; Title not unique: make it so.
          (if parent
              ;; Append ancestor title.
              (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
                    ref (url-hexify-string (substring-no-properties title))
                    parent (org-element-property :parent parent))
            ;; No more ancestors: add and increment a number.
            (inc-suffixf ref)))
        ref)))

;; *** Exports to './output' directory
  (defvar org-export-output-directory-prefix "output" "prefix of directory used for org-mode export")
  (defadvice org-export-output-file-name (before org-add-export-dir activate )
    "Modifies org-export to place exported files in a different directory"
    (when (not pub-dir)
      (setq pub-dir org-export-output-directory-prefix)
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir))))


  (defvar bp/org-publishing-file nil)
  (defun org-publish-file--publishing-flag-around (f &rest args)
    (let ((bp/org-publishing-file t))
      (apply f args)))
  (advice-add 'org-publish-file :around #'org-publish-file--publishing-flag-around)

  (defun bp/org-html--format-image-relative (original-function source attribute info)
    "Modify the <img src=... /> link to point to path relative to html file instead of org file"
    (let ((org-file (buffer-file-name)))
      (cond ((and org-file
                  org-export-output-directory-prefix
                  (not (file-name-absolute-p source))
                  (not bp/org-publishing-file))
             (let* ((output-dir (format "%s/%s/"
                                        (file-name-directory org-file)
                                        org-export-output-directory-prefix))
                    (source (file-relative-name (file-truename source)
                                                output-dir)))
               (funcall original-function source attribute info)))
            (t
             (funcall original-function source attribute info)))))

  (advice-add 'org-html--format-image :around #'bp/org-html--format-image-relative)

;; *** Blog and Braindump
;; **** Blog and Braindump Common
  (require 'ox-publish)
  (setf org-babel-default-header-args '((:session . "none") (:results . "replace") (:exports . "both")
                                        (:eval . "never-export")
                                        (:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no")))
  (defun bp/org-html-preamble (export-options)
    (let ((date (org-export-get-date export-options)))
      (if date
          (concat "<p class=\"date\">Date: "
                  (org-export-data date export-options)
                  "</p>")
        "")))

  (defun bp/html-postamble (rss-url icon-url args)
    (let ((file (getf args :input-file))
          (rss (format "<a href=\"%s\"><img src=\"%s\" /></a>"
                       rss-url icon-url)))
      (cond ((or (string-suffix-p "index.org" file)
                 (string-suffix-p "sitemap.org" file))
             rss)
            ((or (string-suffix-p "meta.org" file)
                 (search  "errors/" file))
             nil)
            (t
             (let ((feedback-string
                    (format "<hr/>You can send your feedback, queries <a href=\"mailto:bpanthi977@gmail.com?subject=Feedback: %s\">here</a>"
                            (when-let ((title (car (getf args :title))))
                              (substring-no-properties title))))
                   (visits-claps "<span id=\"visits\"></span><span id=\"claps\"></span>")
                   (sendme-claps "<div id=\"claps-message\"></div>"))
               (concat feedback-string
                       visits-claps
                       sendme-claps
                       rss))))))

  ;; this hook is run in the temporary org buffer being exported
  ;; before any processing is done
  (defun bp/org-publish--add-setupfile (&rest args)
    (goto-char (point-min))
    (search-forward "#+title")
    (beginning-of-line)
    (insert "#+SETUPFILE: ./blog/templates/braindump.org\n"))

;; **** Braindump
  (defun bp/org-html-publish-to-html (&rest args)
    (let (;; add setupfile
          (org-export-before-processing-functions (cons #'bp/org-publish--add-setupfile
                                                        org-export-before-processing-functions))
          ;; exclude headings with :personal: tag
          (org-export-exclude-tags (cl-concatenate 'list '("PRESONAL" "personal" "private" "PRIVATE")
                                                   org-export-exclude-tags)))

      (apply #'org-html-publish-to-html args)))

;; **** Braindump Sitemap
  (defun bp/org-project-entry-unlistedp (entry project)
    (let ((tags (org-publish-find-property entry :filetags project)))
      (cl-member "unlisted" tags :test #'string-equal-ignore-case)))

  (defun bp/format-sitemap-entry (entry style project)
    "Format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
    (cond ((not (directory-name-p entry))
           (if (bp/org-project-entry-unlistedp entry project)
               ""
             (format "%s [[file:%s][%s]]"
                       (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
                       ;;(format-time-string "%Y-%m-%d" (file-attribute-modification-time (file-attributes (org-publish--expand-file-name entry project))))
                       entry
                       (org-publish-find-title entry project))))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry)))

;; **** Braindump RSS
  ;; Inspired from https://writepermission.com/org-blogging-rss-feed.html

  (defun rw/org-rss-publish-to-rss (plist filename pub-dir)
    "Publish RSS with PLIST, only when FILENAME is 'rss.org'.
PUB-DIR is when the output will be placed."
    (print filename)
    nil)

  (defvar bp/rfc-822-format-time "%a, %d %b %y %H:%M:%S %z")
  (defvar bp/rfc-3339-format-time "%Y-%m-%dT%H:%M:%S%:z")
  (defvar bp/braindump-base-url "https://bpanthi977.github.io/braindump/")
  (defvar bp/braindump-rss-url "https://bpanthi977.github.io/braindump/data/rss.xml")
  (defvar bp/braindump-rss-icon "https://bpanthi977.github.io/braindump/data/rss.png")

  (defvar bp/blog-base-url "https://bpanthi977.github.io/")
  (defvar bp/blog-rss-url "https://bpanthi977.github.io/rss.xml")
  (defvar bp/blog-rss-icon "https://bpanthi977.github.io/img/rss.png")


  (defun rw/format-rss-feed (title list)
    "Generate RSS feed, as a string.
TITLE is the title of the RSS feed.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
    (concat
     (format "<?xml version=\"1.0\" encoding=\"utf-8\"?>
   <feed xmlns=\"http://www.w3.org/2005/Atom\">
     <title type=\"text\">%s</title>
     <updated>%s</updated>
     <link rel=\"self\" type=\"application/atom+xml\" href=\"%sdata/rss.xml\"/>
     <id>%s</id>
     <generator uri=\"https://www.gnu.org/software/emacs/\" version=\"29.1\">Emacs</generator>
"
             title
             (format-time-string bp/rfc-3339-format-time (current-time))
             bp/braindump-base-url
             bp/braindump-base-url)

     (org-list-to-subtree list 1 '(:icount "" :istart ""))

     "
</feed>"))


  (defun rw/format-rss-feed-entry (baseurl entry style project)
    "Format ENTRY for the RSS feed.
ENTRY is a file name.  STYLE is either 'list' or 'tree'.
PROJECT is the current project."
    (cond ((and (not (directory-name-p entry))
                (bp/org-project-entry-unlistedp entry project))
           "")
          ((not (directory-name-p entry))
           (let* ((file (org-publish--expand-file-name entry project))
                  (title (org-publish-find-title entry project))
                  (link (url-encode-url (concat baseurl (file-name-sans-extension entry) ".html")))
                  (id (with-temp-buffer
                        (insert-file-contents file)
                        (let ((ids (org-property-values "id")))
                          (if ids
                              (concat "urn:uuid:"
                                      (first ids))
                            link)))))
             (format "
 <entry>
    <title type=\"text\">%s</title>
    <link href=\"%s\" />
    <id>%s</id>
    <published>%s</published>
    <updated>%s</updated>
    <author>
      <name>Bibek Panthi</name>
    </author>
    <content type=\"text/html\">
      <a href=\"%s\">%s</a>
    </content>
  </entry>"

                     (org-html-encode-plain-text title)
                     link
                     id
                     (format-time-string bp/rfc-3339-format-time (org-publish-find-date entry project))
                     (format-time-string bp/rfc-3339-format-time (bp/last-modified-time file))
                     link
                     link)))
          ((eq style 'tree)
           ;; Return only last subdir.
           (file-name-nondirectory (directory-file-name entry)))
          (t entry)))

  (defun bp/org-publish-braindump-rss ()
    (interactive)
    (let ((project (assoc "braindump-rss" org-publish-project-alist)))
      (org-publish-sitemap project "data/rss.xml")))

  (defun bp/org-publish-braindump-sitemap ()
    (interactive)
    (let ((project (assoc "braindump-rss" org-publish-project-alist)))
      (org-publish-sitemap (cl-concatenate 'list '(:sitemap t) project)
                           "sitemap.org")))

;; **** Publish List
  (setq org-publish-project-alist
        '(
          ("blog-org"
           :base-directory "~/Documents/synced/Notes/blog/"
           :exclude "templates/*"
           :base-extension "org"
           :publishing-directory "~/Development/Web/Blog/blog/"
           :recursive t
           :exclude "^meta.org\\|^rss.xml"
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           :html-preamble bp/org-html-preamble ;; org-html-preamble
           :html-postamble (lambda (args) (bp/html-postamble bp/blog-rss-url bp/blog-rss-icon args))

           :author "Bibek Panthi"
           :auto-sitemap t
           :sitemap-filename "rss.xml"
           :sitemap-title "Bibek's Blog"
           :sitemap-style list
           :sitemap-sort-files anti-chronologically
           :sitemap-function rw/format-rss-feed
           :sitemap-format-entry (lambda (&rest args)
                                   (apply #'rw/format-rss-feed-entry bp/blog-base-url args))

           )
          ("blog-static"
           :base-directory "~/org/blog/"
           :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg\\|php\\|ico\\|json"
           :publishing-directory "~/Development/Web/Blog/blog/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("blog" :components ("blog-org" "blog-static"))

          ("braindump-org"
           :base-directory "~/Documents/synced/Notes/"
           :base-extension "org"
           :publishing-directory "~/Development/Web/Blog/blog/braindump/"
           :exclude "^notes.org\\|^tasks.org\\|^rss.org"
           :recursive nil
           :publishing-function bp/org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t
           :html-preamble bp/org-html-preamble ;; org-html-preamble
           :html-postamble (lambda (args) (bp/html-postamble bp/braindump-rss-url bp/braindump-rss-icon args))

           :auto-sitemap nil
           :sitemap-filename "sitemap.org"
           :sitemap-ignore-case t
           :sitemap-format-entry bp/format-sitemap-entry
           :sitemap-title "Bibek's Digital Garden"
           )

          ("braindump-rss"
           :base-directory "~/Documents/synced/Notes/"
           :base-extension "org"
           :publishing-directory "~/Development/Web/Blog/blog/braindump/"
           :exclude "^notes.org\\|^tasks.org\\|^sitemap.org"
           :html-link-home "https://bpanthi977.github.com/braindump/"
           :html-link-use-abs-url t
           :section-number nil
           :table-of-contents nil
           :recursive nil
           :publishing-function rw/org-rss-publish-to-rss

           :author "Bibek Panthi"
           :auto-sitemap t
           :sitemap-filename "data/rss.xml"
           :sitemap-title "Bibek's Digital Garden"
           :sitemap-style list
           :sitemap-sort-files anti-chronologically
           :sitemap-function rw/format-rss-feed
           :sitemap-format-entry (lambda (&rest args)
                                   (apply #'rw/format-rss-feed-entry bp/braindump-base-url args))
           )

          ("braindump-static"
           :base-directory "~/Documents/synced/Notes/data/"
           :base-extension "html\\|xml\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|svg\\|php\\|ico\\|mkv\\|lisp"
           :publishing-directory "~/Development/Web/Blog/blog/braindump/data/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("braindump" :components ("braindump-org" "braindump-static"))
          ))

  (defun bp/org-upload-blog ()
    (interactive)
    (let ((default-directory "~/Development/Web/Blog/"))
      (async-shell-command "~/Development/Web/Blog/syncFtp.sh")))

;; *** Html Export Theming
  (defun bp/load-css-from-file (file)
    (message "Loading org html export css")
    (with-temp-buffer
      (insert "<style>\n")
      (insert-file-contents file)
      (goto-char (point-max))
      (insert "</style>\n")
      (buffer-string)))
  (defun bp/last-modified-time (file)
    (file-attribute-modification-time (file-attributes file)))

  (defvar bp/org-html-css nil)
  (defvar bp/org-html-loaded-timestamp nil)
  (defun bp/org-html-css ()
    (let* ((file (expand-file-name "modules/org.css" init-dir))
           (current-timestamp (bp/last-modified-time file)))
      (unless (equal bp/org-html-loaded-timestamp current-timestamp)
        (setf bp/org-html-css (bp/load-css-from-file file)
              bp/org-html-loaded-timestamp current-timestamp))
      bp/org-html-css))

  ;; Don't use inline styling, instead just export selector class
  (setf org-html-htmlize-output-type 'css)
;; *** Html Export
  (defadvice org-html-export-to-html (before html-export-load-css1 activate)
    (setq org-html-head-extra (bp/org-html-css)))

  (defadvice org-html-export-as-html (before html-export-load-css2 activate)
    (setq org-html-head-extra (bp/org-html-css)))

  (setq org-html-validation-link nil)

  (defun bp/org-html-src-block--wrap-with-details (f &rest args)
    (let ((html (apply f args)))
      (format "<details open><summary><span class='org-details-collapse'>&lt; Collapse code block</span><span class='org-details-expand'>&gt; Expand code block</span></summary>\n%s</details>" html)))

  (advice-add 'org-html-src-block :around #'bp/org-html-src-block--wrap-with-details)

  (defun bp/org-view-html-export ()
    (interactive)
    (let ((org-export-show-temporary-export-buffer nil))
      (org-html-export-as-html nil)
      (browse-url-of-buffer "*Org HTML Export*")))



;; *** Comments in between paragraphs
  ;; This allows comments in between a paragraph
  (defun delete-org-comments (backend)
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                                      'comment 'identity))
          do
          (delete-region (org-element-property :begin comment)
                         (org-element-property :end comment))))
  (add-hook 'org-export-before-processing-hook 'delete-org-comments)
;; ** Better Org Outline bindings for show/hide while navigating
  (defun bp/org-just-show-this ()
    (interactive)
    (org-content 2)
    (org-show-entry))

  (define-prefix-command 'bp/org-prefix-map)
  (define-key global-map (kbd "M-o") 'bp/org-prefix-map)

  (smartrep-define-key bp/org-prefix-map
      ""
    '(("	" . bp/org-just-show-this)
      ("e" . org-show-entry)
      ("t" . bp/org-just-show-this)
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

;; ** Customizations
  (setq org-src-window-setup 'current-window)
  (setq org-hide-emphasis-markers t)
  (setf org-startup-with-inline-images t
        org-image-actual-width '(500)
        org-startup-with-latex-preview nil
        org-startup-folded 'content)
  (setf org-id-link-to-org-use-id 'use-existing)
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-log-done t)

  ;; Template for source of a fact
  (defun bp/org-source-template (link)
    (interactive "sSource:")
    (insert "([[" link "][Source]])"))



;; *** PDF Viewers
  (if windows-system?
      (progn
        (setcdr (assoc "\\.pdf\\'" org-file-apps) "e:/Programs/SumatraPDF/SumatraPDF.exe %s")
        (pushnew '("\\.pdf::\\([0-9]+\\)?\\'" .  "e:/Programs/SumatraPDF/SumatraPDF.exe %s -page %1")
                 org-file-apps))
    (progn
      (pushnew '("\\.pdf\\'" . default) org-file-apps)
      (pushnew '("\\.pdf::\\([0-9]+\\)?\\'" . default)
               org-file-apps)))

;; *** Fixing Link Rot
  (defvar bp/org-replace-link-old nil)
  (defvar bp/org-replace-link-new nil)
  (defun bp/org-replace-link (old new)
    (interactive "sOld Link: \nfNew File: ")
    (let* ((old-escaped (regexp-quote old))
           (regexp (format "\\[\\[.*%s\\(.*\\)\\]\\]" old-escaped))
           (new (completing-read "Path to use:"
                                 (list (file-truename new)
                                       (concat "./"
                                               (file-relative-name (file-truename new)
                                                                   (file-name-directory (file-truename (buffer-file-name))))))))
           (new-escaped (string-replace "\\" "\\\\" new))
           (replacement (format "[[%s\\1]]" new-escaped)))
      (setf bp/org-replace-link-old regexp)
      (setf bp/org-replace-link-new replacement)
      (replace-regexp regexp replacement)))

  (defun bp/repeat-last-org-replace-link (dir)
    (interactive "DDirectory of org files: ")
    (dolist (file (directory-files-recursively dir ".*\\.org"))
      (with-current-buffer (find-file file)
        (replace-regexp bp/org-replace-link-old bp/org-replace-link-new))))

;; *** HTML Viewer
  (defun bp/open-html-file (file link)
    (declare (ignore link))
    (browse-url-of-file file))

  (pushnew '("\\.html\\'" . bp/open-html-file)
           org-file-apps)

;; ** Timestamp in TODO Heading
  ;; Switch between TODO, DONE and COMPLETED
  ;; (smartrep-define-key org-mode-map "M-m o"
  ;;   '(("t" . org-todo)))

  ;; Whenever a TODO entry is created, I want a timestamp
  ;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
  (defadvice org-insert-todo-heading (after bp/created-timestamp-advice activate)
    "Insert a CREATED property using org-expiry.el for TODO entries"
    (bp/insert-created-timestamp))

  (ad-activate 'org-insert-todo-heading)
;; ** Clocking
  ;; https://orgmode.org/manual/Clocking-commands.html
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

;; ** Image scaling with text
  (defadvice text-scale-increase (after bp/image-scaling-on-text-scaling activate)
    (when (eql major-mode 'org-mode)
      (setq org-image-actual-width (list (truncate (* 500 (expt text-scale-mode-step text-scale-mode-amount)))))
      (org-redisplay-inline-images)))

;; ** Org capture functions for thoughts and notes in org file
  (defun bp/org-capture-thought ()
    (interactive)
    (org-capture nil "thoughts"))

  (defun bp/org-capture-definitions ()
    (interactive)
    (org-capture nil "definitions"))

  (defun bp/org-capture-notes ()
    (interactive)
    (org-capture nil "notes"))

  ;; (add-hook 'org-mode-hook (lambda ()
  ;;                         (electric-indent-local-mode nil)
  ;;                         (modify-syntax-entry ?< ".")
  ;;                         (modify-syntax-entry ?> ".")))

;; ** org-babel
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

;; ** Linking org files across fs to ~/org/
  (defun bp/link-to-~org ()
    (interactive)
    (let ((name (completing-read "File Name: " (remove-if #'null
                                                          (list (file-name-base (buffer-file-name))
                                                                (pcase (org-collect-keywords '("TITLE"))
                                                                  (`(("TITLE" . ,val))
                                                                   val)))))))
      (make-symbolic-link (buffer-file-name) (concat "~/org/" name ".org"))))
;; ** Open email links (mid)
  (org-link-set-parameters "mid" :follow #'org-mid-follow)

  (defvar org-mid-email-program (case system-type
                             (darwin "/Applications/Thunderbird.app/Contents/MacOS/thunderbird")
                             (t "thunderbird")))

  (defun org-mid-follow (path &optional arg)
    "Open the email `PATH'"
    (make-process :name "thunderbird"
                  :command (list org-mid-email-program
                                 (concat "mid:" path))))

  (defun bp/insert-email-link (message-id)
    (interactive "sMessage Id:")
    (let ((stripped (string-trim message-id "<" ">")))
      (insert "[[mid:" stripped "]]")))

  ;; org config complete
  )

;; * org-id
(use-package org-id
  :defer nil
  :config
  (setf org-id-locations-file-relative t
        org-id-locations-file (expand-file-name ".org-id-locations" savefile-dir)))

;; * Org Latex

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

  ;; Don't clutter my directiory
  (setq org-preview-latex-image-directory (case system-type
                                            (windows-nt "E:/tmp/ltximg/")
                                            (darwin "/tmp/ltximg/")
                                            (t "/tmp/ltximg/")))

  ;; for latex in odt files
  (setq org-latex-to-mathml-convert-command
        "latexmlmath \"%i\" --presentationmathml=%o"
        org-export-with-latex t)
;; ** Latex preview size scaling
  ;; Adapted from https://karthinks.com/software/scaling-latex-previews-in-emacs/

  (defun bp/adjust-latex-previews-scale ()
    "Adjust the size of latex preview fragments when changing the
buffer's text scale."
    (pcase major-mode
      ('latex-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'category)
                 'preview-overlay)
             (bp/latex-preview--resize-fragment ov))))
      ('org-mode
       (dolist (ov (overlays-in (point-min) (point-max)))
         (if (eq (overlay-get ov 'org-overlay-type)
                 'org-latex-overlay)
             (bp/latex-preview--resize-fragment ov))))))

  (defun bp/latex-preview--resize-fragment (ov)
    (overlay-put
     ov 'display
     (cons 'image
           (plist-put
            (cdr (overlay-get ov 'display))
            :scale (* 2 (/ (frame-char-height) 12) (expt text-scale-mode-step text-scale-mode-amount))))))

  (add-hook 'text-scale-mode-hook #'bp/adjust-latex-previews-scale)
  (defadvice org-latex-preview (after bp/org-latex-preview--adjust-scale activate)
    (bp/adjust-latex-previews-scale))

;; ** Latex inserting
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

;; ** Document Classes
;; *** Elsevier Article

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

;; *** Better defaults
  (add-to-list 'org-latex-classes
               '("oldarticle"
                 "\\documentclass[11pt,a4paper]{article}
  \\usepackage[utf8]{inputenc}
  \\usepackage{fixltx2e}
  \\usepackage{float}
  \\usepackage[normalem]{ulem}
  \\usepackage{textcomp}
  \\usepackage{marvosym}
  \\usepackage{wasysym}
  \\usepackage{mathpazo}
  \\usepackage{enumerate}
\\usepackage{color}
  \\definecolor{bg}{rgb}{0.95,0.95,0.95}
  \\definecolor{commentsColor}{rgb}{0.497495, 0.497587, 0.497464}
  \\definecolor{keywordsColor}{rgb}{0.000000, 0.000000, 0.635294}
  \\definecolor{stringColor}{rgb}{0.558215, 0.000000, 0.135316}
\\usepackage{listings}
  \\tolerance=1000
        [PACKAGES]
        [EXTRA]
  \\linespread{1.2}"

                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}\"")))

  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt,a4paper]{article}
\\usepackage{lmodern}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{color}
\\definecolor{commentsColor}{rgb}{0.497495, 0.497587, 0.497464}
\\definecolor{keywordsColor}{rgb}{0.000000, 0.000000, 0.635294}
\\definecolor{stringColor}{rgb}{0.558215, 0.000000, 0.135316}
\\geometry{a4paper,left=2.5cm,top=2cm,right=2.5cm,bottom=2cm,marginparsep=7pt, marginparwidth=.6in}"

                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-listings 'listings)
  (setq org-latex-custom-lang-environments nil)

  ;; options passed to listing for each codeblock
  (setq org-latex-listings-options
        '(("frame" "lines")
          ("breaklines" "true")
          ("basicstyle" "\\footnotesize")
          ("showstringspaces" "false")
          ("numbers" "left")
          ("numberstyle" "\\tiny\\color{commentsColor}")
          ("commentstyle" "\\color{commentsColor}\\textit")
          ("keywordstyle" "\\color{keywordsColor}\\bfseries")
          ("stringstyle" "\\color{stringColor}")))


;; ** init key bindings
  :init
  (with-eval-after-load "org"
    (bind-keys :map org-mode-map
               ("M-m o i l" . bp/org-insert-inline-latex)
               ("M-m o i i" . bp/org-insert-last-inline-latex )
               ("M-m o i e" . bp/org-insert-latex-equation)
               ("M-l" . bp/org-insert-last-inline-latex))))

;;; Org-Attach
(use-package org-attach
  :defer t
  :config
  (setq org-attach-auto-tag nil
        org-attach-method "mv"
        org-attach-preferred-new-method 'dir
        org-attach-id-dir "data/"
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


  ;; adapted from [[elfeed:vxlabs.com#https://vxlabs.com/2020/07/25/emacs-lisp-function-convert-attachment-to-file/][An Emacs Lisp function to convert attachment: links to file: links for ox-hugo exports]]
  (defun bp/convert-attachment-to-file ()
    "Convert [[attachment:..]] to [[file:..][]file:..]]"
    (interactive)
    (let ((elem (org-element-context)))
      (if (eq (car elem) 'link)
          (let ((type (org-element-property :type elem)))
            ;; only translate attachment type links
            (when (string= type "attachment")
              ;; translate attachment path to relative filename using org-attach API
              ;; 2020-11-15: org-attach-export-link was removed, so had to rewrite
              (let* ((link-end (org-element-property :end elem))
                     (link-begin (org-element-property :begin elem))
                     ;; :path is everything after attachment:
                     (file (org-element-property :path elem))
                     ;; expand that to the full filename
                     (fullpath (org-attach-expand file))
                     ;; then make it relative to the directory of this org file
                     (current-dir (file-name-directory (or default-directory
                                                           buffer-file-name)))
                     (relpath (file-relative-name (file-truename fullpath) (file-truename current-dir))))
                ;; delete the existing link
                (delete-region link-begin link-end)
                ;; replace with file: link and file: description
                (insert (format "[[file:%s][file:%s]]" relpath relpath))))))))

  (defun bp/convert-all-attachment-to-file ()
    (interactive)
    (goto-char (point-min))
    (loop while (search-forward "[[attachment:" nil t) do
          (bp/convert-attachment-to-file)))

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
                               "~/org/tasks.org")))

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
  (defun capture-thoughts-in-visited-org-file ()
    (let* ((buffer (current-buffer))
           (current-point (point)))
      (goto-char 0)
      (if (search-forward-regexp "^\* Thoughts" nil t)
          (forward-line)
        (progn (goto-char (point-max))
               (insert "\n* Thoughts")))))

  (defun capture-definitions-in-visited-org-file ()
    (let* ((buffer (current-buffer))
           (current-point (point)))
      (goto-char 0)
      (if (search-forward-regexp "^\* Definitions" nil t)
          (forward-line)
        (progn (goto-char (point-max))
               (insert "\n* Definitions")))))

  (defun capture-note-in-current-heading ()
    (let* ((buffer (current-buffer))
           (current-point (point)))
      (if (search-forward-regexp "^\*+ Notes" nil t)
          (forward-line)
        (progn (goto-char (point-max))
               (insert "\n* Notes")))))

  ;; Emacs uses 32bit seconds since unix epoch. So, dates beyound 2038 and
  ;; before 1970 are not representable across some emacs code. calendar and org-mode
  ;; can support dates beyond this. but the default is restrictive.
  ;; let's not force to use only compatible dates in org-mode
  (setf org-read-date-force-compatible-dates nil)

  ;; file+datetree with :time-prompt t will ask for a date
  ;; using org-read-date. Add a keybinding to use the most recently used
  ;; timestamp in that prompt. Press "l" (l stands for last)
  (org-defkey org-read-date-minibuffer-local-map (kbd "l")
              (lambda () (interactive)
                (insert (or org-last-inserted-timestamp ""))))

  (setq org-capture-templates `(
                                ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
                                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
                                ("t" "Todo" entry (file+datetree "~/org/tasks.org")
                                 "* TODO %?\nCREATED: %U\n %i\n  %a")
                                ("j" "Journal" entry (file+datetree "~/org/private/journal.org.gpg")
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("e" "Event" entry (file+datetree "~/org/private/dates.org")
                                 "* %?\n %i \n %a \n"
                                 :time-prompt t)
                                ("w" "Words" entry (file "~/org/private/words.org")
                                 "* %?\nEntered on %U\n  %i\n  %a")
                                ("n" "Note" entry (file "~/org/notes.org" )
                                 "* %?\nCREATED: %U\n")
                                ("d" "Drill" entry (file "~/private/drill.org" )
                                 "* %? :drill:%^g\nCREATED : %U\n %i\n %a")
                                ("a" "Anki" entry (file "~/drill.org" )
                                 "* Card
:PROPERTIES:
:ANKI_NOTE_TYPE: Basic
:ANKI_DECK: %^{ANKI_DECK|GK|Maths|CS|CE|Emacs|ShareMarket}
:END:

# CREATED : %U
# %a

** Front
%?
** Back
%i")
                                ("c" "Anki Cloze" entry (file "~/drill.org" )
                                 "* Card
:PROPERTIES:
:ANKI_NOTE_TYPE: Cloze
:ANKI_DECK: %^{ANKI_DECK|GK|Maths|CS|CE|Emacs|ShareMarket}
:END:

# CREATED : %U
# %a

** Text
%?
** Extra
%i")
                                ("k" "Quote" item (file+headline "~/org/notes.org" "Quotes")
                                 "%? :: %x")
                                ("thoughts" "Capture Thoughts in a heading at bottom of file" item (function capture-thoughts-in-visited-org-file)
                                 "+ %?")
                                ("definitions" "Capture Definitions in a heading at bottom of file" item (function capture-definitions-in-visited-org-file)
                                 "+ %?")
                                ("notes" "Capture notes below current heading" item (function capture-note-in-current-heading)
                                 "+ %?\nCREATED: %U\n")
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

;; (defun bp/lecture-position ()
;;   (interactive)
;;   (set-frame-width (selected-frame) 80)
;;   (set-frame-height (selected-frame) 58))

;;; bp/org-company saner company setting for org mode
(defun  bp/org-company ()
  (interactive)
  (setf company-backends '(company-dabbrev)))
;;; Research
;; * Org Ref
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

  (setf doi-utils-open-pdf-after-download t
        doi-utils-download-pdf nil)
  (require 'doi-utils-scihub)
  (setq dbus-debug nil))

;; * Bibtex
;; (use-package bibtex
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq bibtex-completion-bibliography "~/org/bibliography/references.bib"
;;      bibtex-completion-library-path "~/org/bibliography/papers"
;;      bibtex-completion-notes-path "~/org/bibliography/notes.org"   ;; uses bibtex-completion-notes-template-one-file
;;      bibtex-completion-pdf-open-function 'org-open-file
;;      bibtex-completion-notes-template-one-file
;;      "
; * ${author-abbrev} - ${title}
;;   :PROPERTIES:
;;   :Custom_ID: ${=key=}
;;   :AUTHOR: ${AUTHOR}
;;   :YEAR: ${year}
;;   :JOURNAL: ${journal}
;;   :DOI: ${DOI}
;;   :URL: ${url}
;;   :END:

;; cite:${=key=}
;; "))

;; ** Varibles setup
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


;; * Org-roam
(use-package org-roam
  :ensure t
  :defer nil
  ;; :hook
  ;;   (after-init . org-roam-mode)
  :bind (:map bp/global-prefix-map
              (("r f" . org-roam-node-find)
               ("r F" . org-roam-ref-find)
               ("r c" . org-roam-capture)
               ("r g" . org-roam-graph)))
  :bind (:map org-mode-map
              (("M-m o r" . org-roam-node-insert)
               ("M-m r i" . org-roam-node-insert)
               ("M-m r r" . org-roam-buffer-toggle)
               ("M-m r R" . org-roam-ref-add)
               ("M-m r a" . org-roam-alias-add)
               ("M-m r t" . org-roam-tag-add)))
  :config
  (setf org-roam-mode nil)
  (setq org-roam-directory (file-truename "~/org")
        org-roam-capture-templates '(("d" "default" "plain" "%?"
                                      :target (file+head "${slug}.org"
                                                         "#+title: ${title}\n#+date:%t\n")
                                      :unnarrowed t)))
  (when windows-system?
    (setq org-roam-list-files-commands '((find . "C:/tools/msys64/usr/bin/find.exe") rg)))
  (setq org-roam-file-exclude-regexp '("data/" ".stversions/" ".stfolder/"))
  (setq org-roam-db-location
        (cond ((string-equal system-type "gnu/linux")
               (expand-file-name "dbs/linux/org-roam.db" org-roam-directory))
              ((string-equal system-type "windows-nt")
               (expand-file-name "dbs/windows/org-roam.db" org-roam-directory))
              ((string-equal system-type "darwin")
               (expand-file-name "dbs/darwin/org-roam.db" org-roam-directory))))

  (org-roam-db-autosync-mode 1)
  (defun bp/buffer-file-truename (args)
    (let ((file (first args)))
      (when-let (path (or file (buffer-file-name (buffer-base-buffer))))
        (list (file-truename path)))))
  (advice-remove 'org-roam-file-p #'bp/buffer-file-truename)
  (advice-add 'org-roam-file-p :filter-args #'bp/buffer-file-truename)

;; ** org-roam publish backreferences and refs
  (defun bp/org-roam-private-node? (node)
    (or (cl-find-if (lambda (s)
                      (or (string-equal-ignore-case "private" s)
                          (string-equal-ignore-case "personal" s)))
                (org-roam-node-tags node))
        (string-suffix-p "gpg" (org-roam-node-file node))))

  (defun bp/org-roam-backlink-nodes (node)
    (mapcar #'org-roam-backlink-source-node (org-roam-backlinks-get node)))

  (cl-defun bp/org-room-collect-backlinks-and-refs (backend)
    (when-let ((node (org-roam-node-at-point))
               (_ (not (bp/org-roam-private-node? node))))
      (let* ((source-node (org-roam-node-at-point))
             (source-file (org-roam-node-file source-node))
             (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                      (org-roam-node-list)))
             (refs (cl-remove-duplicates
                    (apply #'cl-concatenate 'list
                           (mapcar #'org-roam-node-refs nodes-in-file))
                    :test #'string-equal))
             ;; get backlinks from non-private notes
             (backlinks (cl-remove-duplicates
                         (--filter (and (not (bp/org-roam-private-node? it))
                                        (not (string-equal (org-roam-node-file it)
                                                           source-file)))
                                   (apply #'cl-concatenate 'list
                                          (mapcar #'bp/org-roam-backlink-nodes
                                                  nodes-in-file)))
                         :key #'org-roam-node-id
                         :test #'string-equal)))

        (when refs
          (let ((reflinks (org-roam-reflinks-get node)))
            (goto-char (point-max))
            (insert "
#+begin_export html
<hr />
<h3>References</h3>
#+end_export
")
            (dolist (ref refs)
              (insert (format "\n+ %s" ref))
              (let ((reflinks-filtered (--filter (string-suffix-p (org-roam-reflink-ref it) ref)
                                                 reflinks)))
                (dolist (reflink reflinks-filtered)
                  (let ((node (org-roam-reflink-source-node reflink)))
                    (insert (format " ([[id:%s][%s]]) "
                                    (org-roam-node-id node)
                                    (org-roam-node-title node)))))))))

        (when backlinks
          (goto-char (point-max))
          (insert "
#+begin_export html
<hr />
<h3>Backlinks</h3>
#+end_export
")
          (dolist (backlink backlinks)
            (insert (format "\n+ [[id:%s][%s]]"
                            (org-roam-node-id backlink)
                            (org-roam-node-title backlink))))))))

  (add-hook 'org-export-before-processing-hook 'bp/org-room-collect-backlinks-and-refs))

;; * org-roam-ui
;;; Supersedes org-roam-server
(use-package org-roam-ui
  :ensure t
  :defer t
  :bind (:map bp/global-prefix-map
              ("r s" . org-roam-ui-open)
              ("r o" . org-roam-ui-node-zoom))
  :config
  (setf org-roam-ui-follow t))

;; * org-transclusion
(use-package org-transclusion
  :ensure t
  :hook (org-mode . org-transclusion-mode)
  :init
  (bind-keys :map org-mode-map
             ("M-m o t t" . org-transclusion-add)
             ("M-m o t l" . bp/org-transclusion-insert-link)
             ("M-m o t a" . org-transclusion-add-all))

  (defun bp/org-transclusion-insert-link ()
    (interactive)
    (insert "#+transclude: ")
    (when (org-insert-link)
      (move-beginning-of-line 1)
      (insert "#+transclude: ")))

  :config
  (cl-pushnew 'keyword org-transclusion-exclude-elements)
  (setf org-transclusion-add-all-on-activate t)
  (pushnew #'org-transclusion-add-all org-export-before-processing-functions))

;; * org-download
(use-package org-download
  :ensure t
  :demand t
  :commands (org-download-screenshot)
  :config
  (defvar bp/org-download-screenshot-title nil)
  (defun bp/org-download-file-formater (filename)
    "Asks the user for file name"
    (let* ((title (completing-read "Title:"
                                   (list  (current-kill 0)
                                          (pcase (org-collect-keywords '("TITLE"))
                                                                  (`(("TITLE" . ,val))
                                                                   val)))
                                   nil nil))
           (slug (bp/title-to-slug title))
           (file (concat slug (format-time-string "-%Y%m%d%H%M%S") ".png")))
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
        (concat (when bp/org-download-screenshot-title
                  (concat  "#+CAPTION: " bp/org-download-screenshot-title "\n"))
                "#+ATTR_ORG: :width 400 \n")
      (setf bp/org-download-screenshot-title nil)))

  ;; modifies org-download.el org-download--dir-2 function
  (defun org-download--dir-2 ()
    (if org-download-heading-lvl
        (org-download-get-heading
         org-download-heading-lvl)
      (file-name-base (buffer-file-name))))

  (set-default 'org-download-image-dir "./data/")
  (set-default 'org-download-heading-lvl nil)
  (setf org-mpv-notes-save-image-function #'org-download-image)

  (setq org-download-method 'directory
        org-download-screenshot-method "xfce4-screenshooter -r -o cat > %s"
        org-download-file-format-function #'bp/org-download-file-formater
        org-download-annotate-function #'bp/org-download-annotate-with-title)

  :init
  (bind-keys :map bp/global-prefix-map
             ("o d s" . org-download-screenshot)))


;;; Presentation
;; (use-package org-present
;;   :ensure t
;;   :defer t
;;   :config
;;   (add-hook 'org-present-after-navigate-functions
;;          (lambda (buffer current-heading)
;;            (declare (ignore buffer current-heading))
;;            (org-display-inline-images)
;;            (org--latex-preview-region (point-min) (point-max))))
;;   )

;; * Presentation
(use-package org-tree-slide
  :ensure t)
;; * org noter
(use-package org-noter
  :ensure t
  :defer t
  :config
  (setf org-noter-notes-search-path '("~/org/" "~/Documents/synced/BE/")
        org-noter-doc-split-fraction '(0.8 0.2)))

;; * orglink
(use-package orglink
  :ensure t
  :defer nil
  :config
  (global-orglink-mode))
;; * org-mpv-notes
(use-package org-mpv-notes
  :commands (org-mpv-notes)
  :defer t
  :config
  (setf org-mpv-notes-save-image-function #'org-download-image
        org-mpv-notes-ocr-command "tesseract"
        org-mpv-notes-ocr-command-args "-")

  (smartrep-define-key org-mpv-notes-mode-map "M-n"
    `(;; keys used for moving in the video
      ("b" . mpv-seek-backward)
      ("q" . keyboard-quit)
      ("f" . mpv-seek-forward)
      ("F" . mpv-toggle-fullscreen)
      ("<left>" . mpv-seek-backward)
      ("<right>" . mpv-seek-forward)
      ("<up>" . mpv-seek-double-step)
      ("<down>" . mpv-seek-halve-step)
      ("]" . org-mpv-speed-up)
      ("[" . org-mpv-speed-down)
      ;; keys used for moving in the notes
      ("n" . org-mpv-notes-next-timestamp)
      ("p" . org-mpv-notes-previous-timestamp)
      ("SPC" . mpv-pause)
      ("a" . mpv-seek-halve-step)
      ("d" . mpv-seek-double-step)
      ("." . org-mpv-notes-this-timestamp))))
