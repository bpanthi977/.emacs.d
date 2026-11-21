;;; agent-sessions-folder-test.el --- Folder tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-sessions)

(defmacro agent-sessions-test-folder-test-with-db (&rest body)
  "Run BODY with an isolated database."
  (declare (indent 0))
  `(let* ((agent-sessions-db-file (make-temp-file "agent-folders-"))
          (agent-sessions--db nil)
          (agent-sessions--folders-loaded-p nil)
          (agent-sessions--folders-cache-version nil)
          (agent-sessions--folders nil)
          (agent-sessions--folder-assignments nil)
          (agent-sessions--order nil))
     (unwind-protect (progn ,@body)
       (when (sqlitep agent-sessions--db)
         (sqlite-close agent-sessions--db))
       (dolist (path (list agent-sessions-db-file
                          (concat agent-sessions-db-file "-wal")
                          (concat agent-sessions-db-file "-shm")))
         (when (file-exists-p path) (delete-file path))))))

(ert-deftest agent-sessions-test-folders-repo-scoping-and-persistence ()
  (agent-sessions-test-folder-test-with-db
    (let* ((a-root (make-temp-file "repo-a-" t))
           (b-root (make-temp-file "repo-b-" t))
           (a-buf (generate-new-buffer " *a*"))
           (b-buf (generate-new-buffer " *b*")))
      (unwind-protect
          (let* ((a-repo (agent-sessions--canonical-path a-root))
                 (b-repo (agent-sessions--canonical-path b-root))
                 (a-folder (agent-sessions--folder-create "Work" a-repo))
                 (b-folder (agent-sessions--folder-create "Work" b-repo))
                 (a (list :buffer a-buf :agent-session-id "a" :repo-root a-root))
                 (b (list :buffer b-buf :agent-session-id "b" :repo-root b-root)))
            (should-not (equal (plist-get a-folder :id) (plist-get b-folder :id)))
            (agent-sessions--folder-set a (plist-get a-folder :id))
            (agent-sessions--folder-set b (plist-get b-folder :id))
            (should-error (agent-sessions--folder-set a (plist-get b-folder :id)))
            (agent-sessions--folder-rename a-folder "Renamed")
            (sqlite-close agent-sessions--db)
            (setq agent-sessions--db nil
                  agent-sessions--folders-loaded-p nil)
            (should (equal "Renamed" (plist-get
                                       (agent-sessions--folder
                                        (plist-get a-folder :id)) :name)))
            (should (equal (plist-get b-folder :id)
                           (agent-sessions--folder-for-session b)))
            (agent-sessions--folder-delete
             (agent-sessions--folder (plist-get a-folder :id)))
            (should-not (agent-sessions--folder-for-session a))
            (should (agent-sessions--folder-for-session b)))
        (mapc #'kill-buffer (list a-buf b-buf))
        (delete-directory a-root t)
        (delete-directory b-root t)))))

(ert-deftest agent-sessions-test-folders-create-recovers-stale-cache ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-stale-cache-" t))
           (repo (agent-sessions--canonical-path root)))
      (unwind-protect
          (let* ((existing (agent-sessions--folder-create "Work" repo))
                 (id (plist-get existing :id)))
            ;; A module reload can leave the old global-folder cache in Emacs.
            (setq agent-sessions--folders (list (list :id id :name "Work"))
                  agent-sessions--folders-loaded-p t)
            (should (equal id (plist-get
                               (agent-sessions--folder-create "work" repo)
                               :id)))
            (should (equal repo (plist-get (car agent-sessions--folders)
                                           :repo)))
            ;; A second Emacs may create a folder after this cache was loaded.
            (sqlite-execute (agent-sessions--db)
                            "INSERT INTO folders (repo, name) VALUES (?, ?)"
                            (list repo "Other"))
            (let ((other (agent-sessions--folder-create "other" repo)))
              (should (equal "Other" (plist-get other :name)))
              (should (equal 2 (length agent-sessions--folders))))
            (setq agent-sessions--folders nil
                  agent-sessions--folders-loaded-p t
                  agent-sessions--folders-cache-version nil)
            (should (equal 2 (length (agent-sessions--folders-for-repo repo)))))
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-render-repo-folder-and-worktree-column ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-render-" t))
           (terminal (generate-new-buffer " *render-terminal*"))
           (dashboard (generate-new-buffer " *render-dashboard*"))
           (agent-sessions--states nil)
           (agent-sessions--notes nil)
           (agent-sessions--shared-files nil)
           (agent-sessions--shared-links nil))
      (unwind-protect
          (let* ((folder (agent-sessions--folder-create
                          "Tasks" (agent-sessions--canonical-path root)))
                 (session (list :buffer terminal :agent-session-id "render"
                                :agent-type "codex" :status 'stopped
                                :repo "sample-repo" :repo-root root
                                :worktree "sample-worktree (feature/test)"
                                :worktree-name "sample-worktree"
                                :branch "feature/test" :worktree-path root))
                 (entry (list :id "render" :session session :title "Task")))
            (agent-sessions--folder-set session (plist-get folder :id))
            (with-current-buffer dashboard
              (agent-sessions-dashboard-mode)
              (cl-letf (((symbol-function 'agent-sessions--live-entries)
                         (lambda () (list entry))))
                (let ((agent-sessions-group-by-folder t))
                  (agent-sessions--refresh)
                  (should (string-match-p "sample-repo\n  Tasks (1)"
                                          (buffer-string)))
                  (should (string-match-p "No folder (0)" (buffer-string)))
                  (should (string-match-p "sample-worktree.*Task  (feature/test)"
                                          (buffer-string))))
                (let ((agent-sessions-group-by-folder nil))
                  (agent-sessions--refresh)
                  (should (string-match-p
                           (concat (regexp-quote
                                    "  sample-worktree (feature/test)\n")
                                   "    .*Task.*\\[Tasks\\]")
                           (buffer-string)))
                  (let ((row (seq-find
                              (lambda (line) (string-match-p "Task" line))
                              (split-string (buffer-string) "\n" t))))
                    (should-not (string-match-p "sample-worktree" row))
                    (should-not (string-match-p "feature/test" row)))
                  (setq agent-sessions--overview-level 1)
                  (agent-sessions--show-overview-level 1)
                  (agent-sessions--refresh)
                  (let* ((repo-section
                          (car (oref magit-root-section children)))
                         (worktree-section
                          (car (oref repo-section children))))
                    (should-not (oref repo-section hidden))
                    (should (oref worktree-section hidden)))
                  (setq agent-sessions--overview-level 3)
                  (agent-sessions--show-overview-level 3)
                  (agent-sessions--refresh)
                  (should (= agent-sessions--overview-level 2))))))
        (mapc #'kill-buffer (list terminal dashboard))
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-kill-deletes-only-empty-folders ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-kill-folder-" t))
           (repo (agent-sessions--canonical-path root))
           (terminal (generate-new-buffer " *kill-folder-terminal*"))
           (dashboard (generate-new-buffer " *kill-folder-dashboard*"))
           (agent-sessions--states nil)
           (agent-sessions--notes nil)
           (agent-sessions--shared-files nil)
           (agent-sessions--shared-links nil))
      (unwind-protect
          (let* ((full (agent-sessions--folder-create "Full" repo))
                 (empty (agent-sessions--folder-create "Empty" repo))
                 (session (list :buffer terminal :agent-session-id "kill-folder"
                                :agent-type "codex" :status 'stopped
                                :repo "sample-repo" :repo-root root
                                :worktree "wt (main)" :worktree-name "wt"
                                :branch "main" :worktree-path root))
                 (entry (list :id "kill-folder" :session session :title "Task")))
            (agent-sessions--folder-set session (plist-get full :id))
            (with-current-buffer dashboard
              (agent-sessions-dashboard-mode)
              (cl-letf (((symbol-function 'agent-sessions--live-entries)
                         (lambda () (list entry))))
                (let ((agent-sessions-group-by-folder t))
                  (agent-sessions--refresh)
                  (cl-flet ((heading (name)
                              (goto-char (point-min))
                              (search-forward (format "  %s (" name))
                              (beginning-of-line)))
                    (heading "Full")
                    (agent-sessions-kill)
                    (should (agent-sessions--folder (plist-get full :id)))
                    (should (buffer-live-p terminal))
                    (heading "No folder")
                    (agent-sessions-kill)
                    (heading "Empty")
                    (agent-sessions-kill)
                    (should-not (agent-sessions--folder (plist-get empty :id)))
                    (should-not (string-match-p "Empty (" (buffer-string)))
                    (should (string-match-p "Full (1)" (buffer-string)))
                    (should (string-match-p "No folder (0)"
                                            (buffer-string))))))))
        (mapc #'kill-buffer (list terminal dashboard))
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-worktree-column-follows-longest-name ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-width-" t))
           (a (generate-new-buffer " *width-a*"))
           (b (generate-new-buffer " *width-b*"))
           (dashboard (generate-new-buffer " *width-dashboard*"))
           (entries (list
                     (list :id "a" :title "First task"
                           :session (list :buffer a :repo "repo" :repo-root root
                                          :worktree-name "short" :worktree-path root))
                     (list :id "b" :title "Second task"
                           :session (list :buffer b :repo "repo" :repo-root root
                                          :worktree-name "long-worktree-name"
                                          :worktree-path root)))))
      (unwind-protect
          (with-current-buffer dashboard
            (agent-sessions-dashboard-mode)
            (let ((agent-sessions-group-by-folder t))
              (cl-letf (((symbol-function 'agent-sessions--live-entries)
                         (lambda () entries)))
                (agent-sessions--refresh)
                (should (= agent-sessions--worktree-column-width 19))
                (let ((lines (split-string (buffer-string) "\n" t)))
                  (should (seq-some
                           (lambda (line)
                             (string-match-p
                              "^    short +  .*First task$" line))
                           lines))
                  (should (seq-some
                           (lambda (line)
                             (string-match-p
                              "^    long-worktree-name +  .*Second task$" line))
                           lines))))))
        (mapc #'kill-buffer (list a b dashboard))
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-manual-order-is-persisted-per-folder ()
  (agent-sessions-test-folder-test-with-db
    (let* ((repo (agent-sessions--canonical-path
                  (make-temp-file "folder-order-repo-" t)))
           (a (generate-new-buffer " *folder-order-a*"))
           (b (generate-new-buffer " *folder-order-b*"))
           (dashboard (generate-new-buffer " *folder-order-dashboard*"))
           (one (agent-sessions--folder-create "One" repo))
           (two (agent-sessions--folder-create "Two" repo))
           (a-session (list :buffer a :repo "repo" :repo-root repo
                            :worktree-path repo :agent-session-id "a"))
           (b-session (list :buffer b :repo "repo" :repo-root repo
                            :worktree-path repo :agent-session-id "b"))
           (agent-sessions--registry (make-hash-table :test 'equal))
           (entries (list (list :id "a" :title "First" :session a-session)
                          (list :id "b" :title "Second" :session b-session))))
      (unwind-protect
          (progn
            (puthash "a" a-session agent-sessions--registry)
            (puthash "b" b-session agent-sessions--registry)
            (agent-sessions--folder-set a-session (plist-get one :id))
            (agent-sessions--folder-set b-session (plist-get one :id))
            (with-current-buffer dashboard
              (agent-sessions-dashboard-mode)
              (let ((agent-sessions-group-by-folder t))
                (cl-letf (((symbol-function 'agent-sessions--live-entries)
                           (lambda () entries)))
                  (agent-sessions--refresh)
                  (should (< (save-excursion (goto-char (point-min))
                                             (search-forward "One (2)"))
                             (save-excursion (goto-char (point-min))
                                             (search-forward "Two (0)"))))
                  (goto-char (point-min))
                  (search-forward "Two (0)")
                  (beginning-of-line)
                  (agent-sessions-move-up)
                  (should (< (save-excursion (goto-char (point-min))
                                             (search-forward "Two (0)"))
                             (save-excursion (goto-char (point-min))
                                             (search-forward "One (2)"))))
                  (should (< (save-excursion (goto-char (point-min))
                                             (search-forward "First"))
                             (save-excursion (goto-char (point-min))
                                             (search-forward "Second"))))
                  (goto-char (point-min))
                  (search-forward "First")
                  (beginning-of-line)
                  (agent-sessions-move-down)
                  (should (< (save-excursion (goto-char (point-min))
                                             (search-forward "Second"))
                             (save-excursion (goto-char (point-min))
                                             (search-forward "First"))))
                  (setq agent-sessions--order nil)
                  (agent-sessions--refresh)
                  (should (< (save-excursion (goto-char (point-min))
                                             (search-forward "Second"))
                             (save-excursion (goto-char (point-min))
                                             (search-forward "First"))))))))
        (mapc #'kill-buffer (list a b dashboard))
        (delete-directory repo t)))))

(ert-deftest agent-sessions-test-folders-shift-tab-cycles-hierarchy-depth ()
  (with-temp-buffer
    (agent-sessions-dashboard-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (agent-sessions-root)
        (magit-insert-section (agent-sessions-repo '(:name "repo"))
          (magit-insert-heading "repo")
          (magit-insert-section (agent-sessions-folder 1)
            (magit-insert-heading "folder")
            (magit-insert-section (agent-sessions-row "s")
              (magit-insert-heading "session")
              (magit-insert-section (agent-sessions-file "/tmp/file" t)
                (magit-insert-heading "file"))
              (magit-insert-section (agent-sessions-link "https://example.com" t)
                (magit-insert-heading "link")))))))
    (let* ((repo (car (oref magit-root-section children)))
           (folder (car (oref repo children)))
           (session (car (oref folder children)))
           (attachments (oref session children)))
      (cl-labels ((visible (heading)
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward heading)
                      (not (invisible-p (1- (point)))))))
        (should (eq (lookup-key agent-sessions-dashboard-mode-map (kbd "<backtab>"))
                    #'agent-sessions-toggle-overview))
        (should (eq (lookup-key agent-sessions-dashboard-mode-map (kbd "<S-tab>"))
                    #'agent-sessions-toggle-overview))
        (agent-sessions--show-overview-level 0)
        (should (oref repo hidden))
        (should (visible "repo"))
        (should-not (visible "folder"))
        (agent-sessions-toggle-overview)
        (should (= agent-sessions--overview-level 1))
        (should-not (oref repo hidden))
        (should (oref folder hidden))
        (should (visible "folder"))
        (should-not (visible "session"))
        (agent-sessions-toggle-overview)
        (should (= agent-sessions--overview-level 2))
        (should-not (oref folder hidden))
        (should (oref session hidden))
        (should (visible "session"))
        (should-not (visible "file"))
        (agent-sessions-toggle-overview)
        (should (= agent-sessions--overview-level 3))
        (should-not (oref session hidden))
        (should (cl-every (lambda (section) (not (oref section hidden)))
                          attachments))
        (should (visible "file"))
        (should (visible "link"))
        (agent-sessions-toggle-overview)
        (should (= agent-sessions--overview-level 0))
        (should (oref repo hidden))))))

(ert-deftest agent-sessions-test-folders-shift-tab-skips-absent-attachment-level ()
  (with-temp-buffer
    (agent-sessions-dashboard-mode)
    (let ((inhibit-read-only t))
      (magit-insert-section (agent-sessions-root)
        (magit-insert-section (agent-sessions-repo '(:name "repo"))
          (magit-insert-heading "repo")
          (magit-insert-section
              (agent-sessions-worktree '(:label "worktree"))
            (magit-insert-heading "worktree")
            (magit-insert-section (agent-sessions-row "s")
              (magit-insert-heading "session"))))))
    (let* ((repo (car (oref magit-root-section children)))
           (worktree (car (oref repo children)))
           (session (car (oref worktree children))))
      (should (= (agent-sessions--overview-max-level) 2))
      (agent-sessions--show-overview-level 0)
      (dotimes (level 2)
        (agent-sessions-toggle-overview)
        (should (= agent-sessions--overview-level (1+ level))))
      (should-not (oref repo hidden))
      (should-not (oref worktree hidden))
      (should-not (oref session hidden))
      (agent-sessions-toggle-overview)
      (should (= agent-sessions--overview-level 0))
      (should (oref repo hidden)))))

(ert-deftest agent-sessions-test-folders-manually-opened-session-survives-refresh ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-visible-" t))
           (path (make-temp-file "shared-visible-"))
           (terminal (generate-new-buffer " *visible-terminal*"))
           (dashboard (generate-new-buffer " *visible-dashboard*"))
           (session (list :buffer terminal :agent-session-id "visible"
                          :repo "repo" :repo-root root :worktree "worktree"
                          :worktree-path root))
           (entry (list :id "visible" :title "Task" :session session)))
      (unwind-protect
          (with-current-buffer dashboard
            (agent-sessions-dashboard-mode)
            (let ((agent-sessions-group-by-folder nil))
              (cl-letf (((symbol-function 'agent-sessions--live-entries)
                         (lambda () (list entry)))
                        ((symbol-function 'agent-sessions--session-files)
                         (lambda (_) (list (list :path path :visited-at 1)))))
                (agent-sessions--refresh)
                (setq agent-sessions--overview-level 2)
                (agent-sessions--show-overview-level 2)
                (let* ((repo (car (oref magit-root-section children)))
                       (worktree (car (oref repo children)))
                       (row (car (oref worktree children))))
                  (should (oref row hidden))
                  ;; Opening one row with TAB must survive an unrelated hook
                  ;; refresh while the global Shift+TAB level stays at two.
                  (magit-section-show row))
                (agent-sessions--refresh)
                (let* ((repo (car (oref magit-root-section children)))
                       (worktree (car (oref repo children)))
                       (row (car (oref worktree children))))
                  (should-not (oref row hidden))
                  (goto-char (point-min))
                  (search-forward (file-name-nondirectory path))
                  (should-not (invisible-p (1- (point))))))))
        (mapc #'kill-buffer (list terminal dashboard))
        (delete-file path)
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-action-creates-in-session-repo ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-action-" t))
           (terminal (generate-new-buffer " *action-terminal*"))
           (dashboard (generate-new-buffer " *action-dashboard*"))
           (agent-sessions--registry (make-hash-table :test 'equal))
           (session (list :buffer terminal :agent-session-id "action"
                          :repo-root root :worktree-path root)))
      (unwind-protect
          (progn
            (puthash "action" session agent-sessions--registry)
            (with-current-buffer dashboard
              (agent-sessions-dashboard-mode)
              (let ((inhibit-read-only t))
                (magit-insert-section (agent-sessions-root)
                  (magit-insert-section (agent-sessions-row "action")
                    (magit-insert-heading "session\n"))))
              (goto-char (point-min))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) "New task"))
                        ((symbol-function 'agent-sessions--refresh) #'ignore))
                (agent-sessions-folder-action))
              (should (equal (agent-sessions--canonical-path root)
                             (plist-get (car agent-sessions--folders) :repo)))
              (should (agent-sessions--folder-for-session session))))
        (mapc #'kill-buffer (list terminal dashboard))
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-migrate-global-assignments-by-repo ()
  (agent-sessions-test-folder-test-with-db
    (let ((db (sqlite-open agent-sessions-db-file)))
      (unwind-protect
          (progn
            (sqlite-execute db "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)")
            (sqlite-execute db "INSERT INTO meta VALUES ('schema_version', '2')")
            (sqlite-execute db "CREATE TABLE folders (
              id INTEGER PRIMARY KEY, name TEXT NOT NULL UNIQUE COLLATE NOCASE)")
            (sqlite-execute db "CREATE TABLE session_folders (
              agent_sid TEXT PRIMARY KEY, folder_id INTEGER NOT NULL)")
            (sqlite-execute db "CREATE TABLE sessions (
              id INTEGER PRIMARY KEY, agent_sid TEXT, repo TEXT)")
            (sqlite-execute db "INSERT INTO folders VALUES (1, 'Work')")
            (sqlite-execute db "INSERT INTO session_folders VALUES ('a', 1), ('b', 1)")
            (sqlite-execute db "INSERT INTO sessions VALUES
              (1, 'a', '/repo-a/'), (2, 'b', '/repo-b/')")
            (agent-sessions--db-migrate db)
            (should (equal '(3) (car (sqlite-select db
               "SELECT COUNT(*) FROM folders"))))
            (should (equal '(("/repo-a/") ("/repo-b/"))
                           (sqlite-select db "SELECT f.repo FROM session_folders sf
                             JOIN folders f ON f.id = sf.folder_id
                             ORDER BY sf.agent_sid"))))
        (sqlite-close db)))))

(ert-deftest agent-sessions-test-folders-carry-terminal-choice-into-agent-id ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-hydrate-" t))
           (buf (generate-new-buffer " *hydrate*"))
           (agent-sessions--registry (make-hash-table :test 'equal))
           (terminal-id "terminal"))
      (unwind-protect
          (let* ((folder (agent-sessions--folder-create
                          "Work" (agent-sessions--canonical-path root)))
                 (session (list :buffer buf :repo-root root :worktree-path root)))
            (with-current-buffer buf (setq-local agent-sessions-id terminal-id))
            (puthash terminal-id session agent-sessions--registry)
            (agent-sessions--folder-set session (plist-get folder :id))
            (agent-sessions--folder-hydrate buf "agent-id")
            (should (equal (plist-get folder :id)
                           (agent-sessions--folder-for-session
                            (plist-put (copy-sequence session)
                                       :agent-session-id "agent-id")))))
        (kill-buffer buf)
        (delete-directory root t)))))

(ert-deftest agent-sessions-test-folders-fork-inherits-parent-folder ()
  (agent-sessions-test-folder-test-with-db
    (let* ((root (make-temp-file "repo-fork-" t))
           (other-root (make-temp-file "repo-fork-other-" t))
           (parent-buf (generate-new-buffer " *parent*"))
           (child-buf (generate-new-buffer " *child*"))
           (chosen-buf (generate-new-buffer " *chosen*"))
           (agent-sessions--registry (make-hash-table :test 'equal)))
      (unwind-protect
          (let* ((repo (agent-sessions--canonical-path root))
                 (work (agent-sessions--folder-create "Work" repo))
                 (mine (agent-sessions--folder-create "Mine" repo))
                 (parent (list :buffer parent-buf :agent-session-id "parent"
                               :repo-root root))
                 (child (list :buffer child-buf :repo-root root
                              :worktree-path root))
                 (from-parent '("parent" . "parent label")))
            (agent-sessions--folder-set parent (plist-get work :id))
            (with-current-buffer child-buf
              (setq-local agent-sessions-id "child-terminal"))
            (puthash "child-terminal" child agent-sessions--registry)
            ;; A fork lands in its parent's folder, persisted under its own id.
            (agent-sessions--folder-inherit child-buf from-parent repo)
            (agent-sessions--folder-hydrate child-buf "child")
            (should (equal (plist-get work :id)
                           (agent-sessions--folder-for-session
                            (list :agent-session-id "child" :repo-root root))))
            ;; A folder already chosen for the terminal wins over the parent's.
            (with-current-buffer chosen-buf
              (setq-local agent-sessions--folder-id (plist-get mine :id)))
            (agent-sessions--folder-inherit chosen-buf from-parent repo)
            (should (equal (plist-get mine :id)
                           (buffer-local-value 'agent-sessions--folder-id
                                               chosen-buf)))
            ;; A parent's folder never crosses into another repo.
            (with-current-buffer chosen-buf
              (setq-local agent-sessions--folder-id nil))
            (agent-sessions--folder-inherit
             chosen-buf from-parent
             (agent-sessions--canonical-path other-root))
            (should-not (buffer-local-value 'agent-sessions--folder-id
                                            chosen-buf)))
        (mapc #'kill-buffer (list parent-buf child-buf chosen-buf))
        (delete-directory root t)
        (delete-directory other-root t)))))

;;; agent-sessions-folder-test.el ends here
