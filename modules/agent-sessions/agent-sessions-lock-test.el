;;; agent-sessions-lock-test.el --- Tests for emacs-lock  -*- lexical-binding: t; -*-

;; These drive the real `bin/emacs-lock' against a database the real schema
;; code created, because the CLI and Emacs meet only at the table layout: a
;; test of either side alone would not notice the two drifting apart.

(require 'ert)
(require 'agent-sessions)

(defvar bp/lock-test--db nil "The temp database file of the running test.")

(defmacro bp/lock-test-with-db (&rest body)
  "Run BODY against a fresh database created by the schema code."
  (declare (indent 0) (debug t))
  `(let* ((db-file (make-temp-file "agent-locks-test-" nil ".db"))
          (bp/lock-test--db db-file)
          (bp/agent-sessions-db-file db-file)
          (bp/agent-sessions--db nil)
          (bp/agent-locks--held nil)
          (bp/agent-locks--waiters nil)
          (bp/agent-locks--loaded-p nil))
     (unwind-protect
         (progn (bp/agent-sessions--db) ,@body)
       (when (sqlitep bp/agent-sessions--db)
         (sqlite-close bp/agent-sessions--db))
       (dolist (path (list db-file (concat db-file "-wal") (concat db-file "-shm")))
         (when (file-exists-p path) (delete-file path))))))

(defun bp/lock-test--env (who &optional extra)
  "An environment for running as holder WHO (nil for no identity) plus EXTRA.
Every inherited agent identity is dropped, and the dashboard poke goes to
`true', so a test can never reach the user's Emacs."
  (append extra
          (and who (list (concat "CLAUDE_CODE_SESSION_ID=" who)))
          (list (concat "EMACS_AGENT_SESSIONS_DB=" bp/lock-test--db)
                "EMACS_LOCK_EMACSCLIENT=true")
          (seq-remove (lambda (entry)
                        (string-match-p
                         "\\`\\(CLAUDE_CODE_SESSION_ID\\|CLAUDE_PID\\|CODEX_THREAD_ID\\|CODEX_SESSION_ID\\|EMACS_AGENT_SESSION_ID\\|EMACS_AGENT_SESSIONS_DB\\)="
                         entry))
                      process-environment)))

(defun bp/lock-test--cli ()
  (expand-file-name "emacs-lock" bp/agent-sessions--bin-dir))

(defun bp/lock-test--run (who &rest args)
  "Run emacs-lock ARGS as WHO; return (EXIT-CODE . OUTPUT).
WHO is a session id, nil, or (ID . EXTRA-ENVIRONMENT)."
  (let ((process-environment
         (if (consp who) (bp/lock-test--env (car who) (cdr who))
           (bp/lock-test--env who))))
    (with-temp-buffer
      (let ((code (apply #'call-process (bp/lock-test--cli) nil t nil args)))
        (cons code (buffer-string))))))

(defun bp/lock-test--start (who &rest args)
  "Start emacs-lock ARGS as WHO in the background; return the process."
  (let ((process-environment (bp/lock-test--env who)))
    (make-process :name "emacs-lock-test" :buffer (generate-new-buffer " *lock-test*")
                  :command (cons (bp/lock-test--cli) args)
                  :connection-type 'pipe :noquery t)))

(defun bp/lock-test--wait-exit (proc &optional seconds)
  "Wait up to SECONDS for PROC to exit; return its exit code or nil."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and (process-live-p proc) (< (float-time) deadline))
      (accept-process-output proc 0.1))
    (unless (process-live-p proc) (process-exit-status proc))))

(defun bp/lock-test--rows (sql &rest args)
  (sqlite-select (bp/agent-sessions--db) sql args))

(defun bp/lock-test--new (who name)
  "Have WHO be shown the known names, then create NAME, holding it.
The refusal of an unknown name is what shows them."
  (bp/lock-test--run who "acquire" name)
  (bp/lock-test--run who "acquire" name "--new" (concat name " resource")))

;;; Acquire and release

(ert-deftest bp/lock-acquire-release-round-trip ()
  (bp/lock-test-with-db
    (should (equal 0 (car (bp/lock-test--new "A" "stack"))))
    (should (string-match-p "already held" (cdr (bp/lock-test--run "A" "acquire" "stack"))))
    (let ((held (bp/lock-test--run "B" "acquire" "stack")))
      (should (equal 1 (car held)))
      (should (string-match-p "HELD stack by:\n  stack  held by A" (cdr held))))
    (should (equal 1 (car (bp/lock-test--run "B" "release" "stack"))))
    (should (equal 0 (car (bp/lock-test--run "A" "release" "stack"))))
    (should (equal 1 (car (bp/lock-test--run "A" "release" "stack"))))
    (should (equal 0 (car (bp/lock-test--run "B" "acquire" "stack"))))))

(ert-deftest bp/lock-names-are-normalised ()
  (bp/lock-test-with-db
    (bp/lock-test--new "A" "Local  Dev\tStack")
    (should (equal '(("local-dev-stack")) (bp/lock-test--rows "SELECT name FROM locks")))
    (should (string-match-p "already held"
                            (cdr (bp/lock-test--run "A" "acquire" " LOCAL dev stack "))))))

(ert-deftest bp/lock-refuses-unknown-name-and-lists-recent-ones ()
  (bp/lock-test-with-db
    (bp/lock-test--new "A" "stack")
    (let ((out (bp/lock-test--run "B" "acquire" "db")))
      (should (equal 1 (car out)))
      (should (string-match-p "unknown lock name: db" (cdr out)))
      (should (string-match-p "  stack  stack resource" (cdr out))))
    (should (equal '(("stack")) (bp/lock-test--rows "SELECT name FROM lock_names")))))

(ert-deftest bp/lock-new-name-requires-having-seen-the-newest ()
  "A creates a name; B's creation is refused until B has been shown A's
name, and then succeeds."
  (bp/lock-test-with-db
    (should (equal 0 (car (bp/lock-test--run "A" "acquire" "stack" "--new" "the stack"))))
    (let ((out (bp/lock-test--run "B" "acquire" "db" "--new" "db")))
      (should (equal 1 (car out)))
      (should (string-match-p "names were added since you last looked" (cdr out)))
      (should (string-match-p "  stack  the stack" (cdr out))))
    (should (equal 0 (car (bp/lock-test--run "B" "acquire" "db" "--new" "db"))))
    (should (equal '(("stack") ("db"))
                   (bp/lock-test--rows "SELECT name FROM lock_names ORDER BY id")))))

(ert-deftest bp/lock-directories-conflict-by-containment ()
  (bp/lock-test-with-db
    (let* ((root (file-truename (make-temp-file "lock-dirs-" t)))
           (parent (expand-file-name "a_b" root))
           (child (expand-file-name "c d" parent))
           (sibling (expand-file-name "axb" root))
           (quoted (expand-file-name "x'y" root)))
      (unwind-protect
          (progn
            (dolist (d (list child sibling quoted)) (make-directory d t))
            (should (equal 0 (car (bp/lock-test--run "A" "acquire" parent))))
            ;; Inside, and around, a held directory: both blocked.
            (should (equal 1 (car (bp/lock-test--run "B" "acquire" child))))
            (should (equal 1 (car (bp/lock-test--run "B" "acquire" root))))
            ;; `_' is not a wildcard, and quotes survive.
            (should (equal 0 (car (bp/lock-test--run "B" "acquire" sibling))))
            (should (equal 0 (car (bp/lock-test--run "B" "acquire" quoted))))
            (should (equal (list (list (concat quoted "/")))
                           (bp/lock-test--rows "SELECT name FROM locks WHERE name LIKE '%y/'")))
            ;; Directories never enter the name registry.
            (should (null (bp/lock-test--rows "SELECT * FROM lock_names"))))
        (delete-directory root t)))))

(ert-deftest bp/lock-concurrent-acquires-have-one-winner ()
  (bp/lock-test-with-db
    (bp/lock-test--new "seed" "stack")
    (bp/lock-test--run "seed" "release" "stack")
    (let* ((procs (mapcar (lambda (i) (bp/lock-test--start (format "W%d" i) "acquire" "stack"))
                          (number-sequence 1 8)))
           (codes (mapcar #'bp/lock-test--wait-exit procs)))
      (should (equal 1 (seq-count (lambda (c) (eql c 0)) codes)))
      (should (equal 7 (seq-count (lambda (c) (eql c 1)) codes)))
      (should (equal 1 (caar (bp/lock-test--rows "SELECT count(*) FROM locks")))))))

;;; Listing

(ert-deftest bp/lock-list-shows-held-names-and-queue-length ()
  (bp/lock-test-with-db
    (should (equal "HELD: none\n" (cdr (bp/lock-test--run "A" "list"))))
    (let ((dir (file-truename (make-temp-file "lock-list-" t))))
      (unwind-protect
          (progn
            (bp/lock-test--run "A" "acquire" dir)
            (bp/lock-test--new "A" "stack")
            (sqlite-execute (bp/agent-sessions--db)
                            "INSERT INTO lock_waiters (name, is_dir, waiter_kind, waiter_key, waiter_label, expires_at)
                             VALUES ('stack', 0, 'session', 'B', 'B', ?), ('stack', 0, 'session', 'C', 'C', ?)"
                            (list (+ (bp/agent-sessions--now) 60) (+ (bp/agent-sessions--now) 60)))
            (should (equal (format "HELD:\n%s\nstack (2 waiting)\n" dir)
                           (cdr (bp/lock-test--run "B" "list"))))
            (let ((verbose (cdr (bp/lock-test--run "B" "list" "--verbose"))))
              (should (string-match-p "held by A" verbose))
              (should (string-match-p "^WAITING:\n  stack  queued: B" verbose))))
        (delete-directory dir t)))))

(ert-deftest bp/lock-list-does-not-count-as-seeing-names ()
  "`list' shows no unheld names, so it must not let a later --new through."
  (bp/lock-test-with-db
    (bp/lock-test--new "A" "stack")
    (bp/lock-test--run "A" "release" "stack")
    (bp/lock-test--run "B" "list")
    (let ((out (bp/lock-test--run "B" "acquire" "db" "--new" "a database")))
      (should (equal 1 (car out)))
      (should (string-match-p "  stack  stack resource" (cdr out))))))

;;; Waiting

(ert-deftest bp/lock-wait-is-first-come-first-served ()
  (bp/lock-test-with-db
    (bp/lock-test--new "H" "stack")
    (let ((q1 (bp/lock-test--start "Q1" "wait" "stack" "--interval" "1")))
      (sleep-for 1.5)
      (let ((q2 (bp/lock-test--start "Q2" "wait" "stack" "--interval" "1")))
        (sleep-for 1.5)
        ;; A plain acquire does not jump the queue, even once the lock is free.
        (should (equal 1 (car (bp/lock-test--run "J" "acquire" "stack"))))
        (bp/lock-test--run "H" "release" "stack")
        (should (string-match-p "QUEUED" (cdr (bp/lock-test--run "J" "acquire" "stack"))))
        (should (eql 0 (bp/lock-test--wait-exit q1 5)))
        (should (process-live-p q2))
        (bp/lock-test--run "Q1" "release" "stack")
        (should (eql 0 (bp/lock-test--wait-exit q2 5)))
        (should (equal '(("Q2")) (bp/lock-test--rows "SELECT holder_key FROM locks")))))))

(ert-deftest bp/lock-expired-waiter-does-not-block ()
  (bp/lock-test-with-db
    (bp/lock-test--new "seed" "stack")
    (bp/lock-test--run "seed" "release" "stack")
    (sqlite-execute (bp/agent-sessions--db)
                    "INSERT INTO lock_waiters (name, is_dir, waiter_kind, waiter_key, expires_at)
                     VALUES ('stack', 0, 'session', 'gone', 1)")
    (should (equal 0 (car (bp/lock-test--run "A" "acquire" "stack"))))
    (should (null (bp/lock-test--rows "SELECT * FROM lock_waiters")))))

(ert-deftest bp/lock-wait-timeout-leaves-the-queue ()
  (bp/lock-test-with-db
    (bp/lock-test--new "H" "stack")
    (let ((out (bp/lock-test--run "T" "wait" "stack" "--timeout" "1" "--interval" "1")))
      (should (equal 1 (car out)))
      (should (string-match-p "no longer queued" (cdr out))))
    (should (null (bp/lock-test--rows "SELECT * FROM lock_waiters")))))

(ert-deftest bp/lock-wait-on-unknown-name-fails-at-once ()
  (bp/lock-test-with-db
    (should (equal 1 (car (bp/lock-test--run "A" "wait" "nothing" "--timeout" "5"))))
    (should (null (bp/lock-test--rows "SELECT * FROM lock_waiters")))))

;;; Liveness and environment

(ert-deftest bp/lock-liveness-follows-pid-and-start-time ()
  (bp/lock-test-with-db
    (bp/lock-test--run (list "A" (format "CLAUDE_PID=%d" (emacs-pid)))
                       "acquire" "stack" "--new" "the stack")
    (should (string-match-p "held by A (running)" (cdr (bp/lock-test--run "B" "list" "--verbose"))))
    (bp/agent-locks--reload)
    (should (eq 'running (plist-get (car bp/agent-locks--held) :liveness)))
    ;; Same pid, different start time: a reused pid is not the holder.
    (sqlite-execute (bp/agent-sessions--db) "UPDATE locks SET holder_pid_start = 'then'")
    (should (string-match-p "held by A (not running)" (cdr (bp/lock-test--run "B" "list" "--verbose"))))
    (bp/agent-locks--reload)
    (should (eq 'not-running (plist-get (car bp/agent-locks--held) :liveness)))))

(ert-deftest bp/lock-identity-falls-back-to-the-session-log ()
  (bp/lock-test-with-db
    (sqlite-execute (bp/agent-sessions--db)
                    "INSERT INTO sessions (emacs_sid, agent_type, agent_sid, worktree, title, opened_at)
                     VALUES ('9-1', 'pi', 'pi-sid', '/w', 'Pi worker', 0)")
    (should (equal 0 (car (bp/lock-test--run (list nil "EMACS_AGENT_SESSION_ID=9-1")
                                             "acquire" "stack" "--new" "the stack"))))
    (should (equal '(("session" "pi-sid" "Pi worker"))
                   (bp/lock-test--rows "SELECT holder_kind, holder_key, holder_label FROM locks")))))

(ert-deftest bp/lock-cannot-run-fails-loudly-and-writes-nothing ()
  (bp/lock-test-with-db
    (should (equal 3 (car (bp/lock-test--run nil "acquire" "stack"))))
    (let ((owner (bp/lock-test--run nil "list")))
      ;; Listing needs no identity.
      (should (equal 0 (car owner))))
    (let* ((missing (concat bp/lock-test--db ".missing"))
           (bp/lock-test--db missing))
      (should (equal 3 (car (bp/lock-test--run "A" "list"))))
      (should-not (file-exists-p missing)))
    (let* ((bare (make-temp-file "agent-locks-bare-" nil ".db"))
           (bp/lock-test--db bare))
      (unwind-protect
          (progn
            (call-process "sqlite3" nil nil nil bare "CREATE TABLE meta (key TEXT)")
            (let ((out (bp/lock-test--run "A" "list")))
              (should (equal 3 (car out)))
              (should (string-match-p "no lock tables" (cdr out)))))
        (delete-file bare)))))

(ert-deftest bp/lock-terminal-environment-names-the-database ()
  (let ((bp/agent-sessions-db-file "/tmp/some/agent-sessions.db"))
    (should (member "EMACS_AGENT_SESSIONS_DB=/tmp/some/agent-sessions.db"
                    (bp/agent-sessions--terminal-environment "1-1")))))

;;; Emacs side

(ert-deftest bp/lock-dashboard-shows-held-locks-and-queue ()
  (bp/lock-test-with-db
    (bp/lock-test--new "A" "stack")
    (sqlite-execute (bp/agent-sessions--db)
                    "INSERT INTO lock_waiters (name, is_dir, waiter_kind, waiter_key, waiter_label, expires_at)
                     VALUES ('stack', 0, 'session', 'B', 'Bee', ?)"
                    (list (+ (bp/agent-sessions--now) 60)))
    (should (bp/agent-locks--reload))
    (should-not (bp/agent-locks--reload))
    (with-temp-buffer
      (let ((bp/agent-sessions-buffer-name (buffer-name)))
        (bp/agent-sessions-dashboard-mode)
        (bp/agent-sessions--refresh)
        ;; Neither A nor B has a session row here, so both show in the
        ;; section, each naming who it is.
        (should (string-match-p
                 (concat "^Locks held outside the sessions below\n"
                         "    🔒 stack  held by A (liveness unknown) for [0-9]+s  ⏳ Bee\n"
                         "    ⏳ stack  Bee waiting\n")
                 (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest bp/lock-session-marker-shows-locks-held-and-queue-places ()
  (let ((bp/agent-locks--held
         (list (list :name "stack" :holder '("session" "H"))
               (list :name "a/" :holder '("session" "M"))
               (list :name "b/" :holder '("session" "M"))))
        (bp/agent-locks--waiters
         (list (list :name "stack" :waiter '("session" "W1"))
               (list :name "stack" :waiter '("session" "W2"))
               (list :name "db" :waiter '("session" "X"))
               (list :name "db" :waiter '("session" "W2"))
               (list :name "db" :waiter '("session" "M")))))
    ;; The count is locks held, not who waits behind them.
    (should (equal "🔒" (bp/agent-locks--session-marker "1" '(:agent-session-id "H"))))
    (should (equal "🔒 (2) ⏳ (3)"
                   (bp/agent-locks--session-marker "2" '(:agent-session-id "M"))))
    (should (equal "⏳ (1)" (bp/agent-locks--session-marker "3" '(:agent-session-id "W1"))))
    ;; One place per queue, in queue order.
    (should (equal "⏳ (2, 2)" (bp/agent-locks--session-marker "4" '(:agent-session-id "W2"))))
    (should (equal "" (bp/agent-locks--session-marker "5" '(:agent-session-id "Y"))))
    ;; Padding is by display width, so rows with and without 🔒 line up.
    (should (= (string-width (bp/agent-locks--pad "🔒" 7))
               (string-width (bp/agent-locks--pad "⏳ (1)" 7))
               (string-width (bp/agent-locks--pad "" 7))))))

(ert-deftest bp/lock-rows-hang-under-their-session ()
  (bp/lock-test-with-db
    (bp/lock-test--run "A" "acquire" "stack" "--new" "the stack" "--why" "e2e run")
    (sqlite-execute (bp/agent-sessions--db)
                    "INSERT INTO lock_waiters (name, is_dir, waiter_kind, waiter_key, waiter_label, why, expires_at)
                     VALUES ('db', 0, 'session', 'A', 'A', 'migrate', ?)"
                    (list (+ (bp/agent-sessions--now) 60)))
    (bp/agent-locks--reload)
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (bp/agent-sessions-root)
          (bp/agent-locks--insert-session-locks "1-1" '(:agent-session-id "A") "        ")
          (bp/agent-locks--insert-session-locks "1-2" '(:agent-session-id "B") "        ")))
      (should (string-match-p
               "\\`        🔒 stack  [0-9]+s — e2e run\n        ⏳ db  waiting — migrate\n\\'"
               (buffer-substring-no-properties (point-min) (point-max))))
      (goto-char (point-min))
      (should (equal "stack" (plist-get (bp/agent-locks--at-point) :name)))
      (forward-line 1)
      (should-not (bp/agent-locks--at-point)))))

(ert-deftest bp/lock-force-release-files-notice-without-reviving ()
  (bp/lock-test-with-db
    (bp/lock-test--new "A" "stack")
    (bp/agent-locks--reload)
    (let ((lock (car bp/agent-locks--held))
          (revived nil))
      (cl-letf (((symbol-function 'bp/agent-orchestration-send)
                 (lambda (&rest _) (setq revived t)))
                ((symbol-function 'bp/agent-orchestration--revive-recipient)
                 (lambda (&rest _) (setq revived t))))
        (should (bp/agent-locks--force-release lock))
        (should-not (bp/agent-locks--force-release lock)))
      (should-not revived)
      (should (null bp/agent-locks--held))
      (should (equal '(("user" "session" "A" "Lock released by the user: stack" nil))
                     (bp/lock-test--rows
                      "SELECT from_kind, to_kind, to_key, subject, read_at FROM mail"))))))

(ert-deftest bp/lock-force-release-skips-a-lock-that-changed-hands ()
  (bp/lock-test-with-db
    (bp/lock-test--new "A" "stack")
    (bp/agent-locks--reload)
    (let ((seen (car bp/agent-locks--held)))
      (sqlite-execute (bp/agent-sessions--db) "UPDATE locks SET acquired_at = acquired_at + 5")
      (should-not (bp/agent-locks--force-release seen))
      (should (equal 1 (caar (bp/lock-test--rows "SELECT count(*) FROM locks"))))
      (should (null (bp/lock-test--rows "SELECT * FROM mail"))))))

(ert-deftest bp/lock-notice-from-the-dashboard-reads-as-such ()
  (let ((text (bp/agent-orchestration--format-message
               (list :from nil :from-key '("user" "dashboard") :from-label "the user"
                     :type "status" :subject "Lock released by the user: stack"
                     :body "b" :files "" :report ""))))
    (should (string-match-p "from the user (via the agent-sessions dashboard)" text))
    (should (string-match-p "Reply: none needed" text))))

(ert-deftest bp/lock-adopt-points-a-restored-holder-at-its-terminal ()
  (bp/lock-test-with-db
    (bp/lock-test--new "S" "stack")
    (sqlite-execute (bp/agent-sessions--db)
                    "UPDATE locks SET holder_pid = 999999, holder_pid_start = 'long ago'")
    (bp/agent-locks--reload)
    (let* ((buf (generate-new-buffer " *lock-adopt*"))
           (proc (make-process :name "lock-adopt" :buffer buf
                               :command '("sleep" "30") :noquery t))
           (bp/agent-session-id->buffer (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (puthash "1-1" buf bp/agent-session-id->buffer)
            (bp/agent-locks--adopt "1-1" "someone-else")
            (should (eq 'not-running (plist-get (car bp/agent-locks--held) :liveness)))
            (bp/agent-locks--adopt "1-1" "S")
            (should (equal (list (list (process-id proc)))
                           (bp/lock-test--rows "SELECT holder_pid FROM locks")))
            (should (eq 'running (plist-get (car bp/agent-locks--held) :liveness))))
        (delete-process proc)
        (kill-buffer buf)))))

(ert-deftest bp/lock-adopt-on-session-start-does-not-trust-a-stale-copy ()
  "A restore resumes seconds after the old terminal died; adopt anyway.
The working copy still says `running' until the next heartbeat, which used to
skip the adoption until the conversation's next hook event."
  (bp/lock-test-with-db
    (bp/lock-test--new "S" "stack")
    (let* ((old (make-process :name "lock-old-terminal" :command '("sleep" "30")
                              :noquery t))
           (buf (generate-new-buffer " *lock-adopt-fresh*"))
           (new (make-process :name "lock-new-terminal" :buffer buf
                              :command '("sleep" "30") :noquery t))
           (bp/agent-session-id->buffer (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (sqlite-execute (bp/agent-sessions--db)
                            "UPDATE locks SET holder_pid = ?, holder_pid_start = ?"
                            (list (process-id old)
                                  (bp/agent-locks--pid-start (process-id old))))
            (bp/agent-locks--reload)
            (should (eq 'running (plist-get (car bp/agent-locks--held) :liveness)))
            (delete-process old)
            (while (bp/agent-locks--pid-start (process-id old)) (sleep-for 0.05))
            (puthash "1-2" buf bp/agent-session-id->buffer)
            ;; An ordinary event trusts the working copy, which is still stale.
            (bp/agent-locks--adopt "1-2" "S")
            (should (equal (list (list (process-id old)))
                           (bp/lock-test--rows "SELECT holder_pid FROM locks")))
            (bp/agent-locks--adopt "1-2" "S" t)
            (should (equal (list (list (process-id new)))
                           (bp/lock-test--rows "SELECT holder_pid FROM locks")))
            (should (eq 'running (plist-get (car bp/agent-locks--held) :liveness))))
        (when (process-live-p old) (delete-process old))
        (delete-process new)
        (kill-buffer buf)))))

(defun bp/lock-test--cli-ok (who &rest args)
  "Run emacs-lock ARGS as WHO and return just its exit code."
  (car (apply #'bp/lock-test--run who args)))

;;; Mailboxes

(defvar bp/mailbox-test--dir nil "The mailbox directory of the running test.")

(defmacro bp/mailbox-test-with (&rest body)
  "Run BODY with a fresh database, a mailbox directory and two sessions.
Terminal T-A runs conversation A, T-B runs B; neither has a buffer, so
nothing is pasted anywhere."
  (declare (indent 0) (debug t))
  `(bp/lock-test-with-db
     (let* ((bp/mailbox-test--dir
             (file-name-as-directory
              (file-truename (make-temp-file "agent-mailbox-test-" t))))
            (bp/agent-sessions (make-hash-table :test 'equal))
            (bp/agent-session-id->buffer (make-hash-table :test 'equal))
            (bp/agent-orchestration--mail (make-hash-table :test 'equal))
            (bp/agent-mailboxes--unread nil)
            (buffers nil))
       ;; Registry entries need a live buffer, or `--live-entries' prunes
       ;; them; with no process in it, nothing is ever pasted there.
       (dolist (spec '(("T-A" "A" "wt-a") ("T-B" "B" "wt-b")))
         (let ((buf (generate-new-buffer (concat " *mailbox-" (car spec) "*"))))
           (push buf buffers)
           (with-current-buffer buf
             (setq-local bp/agent-session-id (car spec)))
           (puthash (car spec) buf bp/agent-session-id->buffer)
           (puthash (car spec) (list :buffer buf :agent-session-id (nth 1 spec)
                                     :agent-type "claude" :worktree (nth 2 spec))
                    bp/agent-sessions)))
       (unwind-protect (progn ,@body)
         (mapc #'kill-buffer buffers)
         (delete-directory bp/mailbox-test--dir t)))))

(defun bp/mailbox-test--archive (direction)
  "The archive files in the test mailbox's mail/DIRECTION, as their contents."
  (let ((folder (expand-file-name (concat "mail/" direction)
                                  bp/mailbox-test--dir)))
    (and (file-directory-p folder)
         (mapcar (lambda (file)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
                 (directory-files folder t "\\.md\\'")))))

(ert-deftest bp/mailbox-mail-waits-for-the-lock-holder ()
  (bp/mailbox-test-with
    (should (string-prefix-p
             "Sent question to mailbox "
             (bp/agent-orchestration-send "T-B" bp/mailbox-test--dir
                                          "question" "Review this" "body")))
    ;; Nobody holds it yet: the mail is only recorded.
    (should (string-match-p "do not hold the lock"
                            (bp/agent-orchestration-check
                             "T-A" t bp/mailbox-test--dir)))
    (should (= 0 (bp/lock-test--cli-ok "A" "acquire" bp/mailbox-test--dir)))
    (let ((out (bp/agent-orchestration-check "T-A" t bp/mailbox-test--dir)))
      (should (string-prefix-p "messages: 1\n" out))
      (should (string-match-p "Subject: Review this" out))
      ;; Answered as the role: the reply carries --from the mailbox.
      (should (string-match-p
               (regexp-quote (format "Reply: emacs-agent send --to B --from %s --type answer"
                                     (shell-quote-argument bp/mailbox-test--dir)))
               out)))
    (should (string-prefix-p "messages: 0"
                             (bp/agent-orchestration-check
                              "T-A" t bp/mailbox-test--dir)))
    (should (equal '(("mailbox" 1))
                   (bp/lock-test--rows
                    "SELECT to_kind, read_at IS NOT NULL FROM mail")))))

(ert-deftest bp/mailbox-only-its-holder-reads-or-speaks-for-it ()
  (bp/mailbox-test-with
    (should (= 0 (bp/lock-test--cli-ok "A" "acquire" bp/mailbox-test--dir)))
    (bp/agent-orchestration-send "T-A" bp/mailbox-test--dir "status" "note" "")
    (should (string-match-p "do not hold the lock"
                            (bp/agent-orchestration-check
                             "T-B" nil bp/mailbox-test--dir)))
    (should (string-match-p "do not hold the lock"
                            (bp/agent-orchestration-send
                             "T-B" "T-A" "status" "spoof" "" bp/mailbox-test--dir)))
    (should (equal '((1)) (bp/lock-test--rows "SELECT count(*) FROM mail")))))

(ert-deftest bp/mailbox-must-be-an-existing-directory ()
  (bp/mailbox-test-with
    (should (string-match-p
             "No such mailbox directory"
             (bp/agent-orchestration-send "T-A" "/no/such/mailbox/" "status"
                                          "s" "")))
    (should (null (bp/lock-test--rows "SELECT * FROM mail")))))

(ert-deftest bp/mailbox-sending-as-a-mailbox-files-replies-there ()
  (bp/mailbox-test-with
    (should (= 0 (bp/lock-test--cli-ok "A" "acquire" bp/mailbox-test--dir)))
    (cl-letf (((symbol-function 'bp/agent-orchestration-deliver) #'ignore))
      (should (string-prefix-p
               "Sent question to T-B"
               (bp/agent-orchestration-send "T-A" "T-B" "question" "Which?" "x"
                                            bp/mailbox-test--dir))))
    (should (equal `(("mailbox" ,bp/mailbox-test--dir "session" "B"))
                   (bp/lock-test--rows
                    "SELECT from_kind, from_key, to_kind, to_key FROM mail")))
    (let ((out (bp/agent-orchestration-check "T-B" t)))
      (should (string-match-p
               (regexp-quote (concat "message from mailbox " bp/mailbox-test--dir
                                     " (sent by wt-a)"))
               out))
      (should (string-match-p
               (regexp-quote (concat "Reply: emacs-agent send --to "
                                     (shell-quote-argument bp/mailbox-test--dir)
                                     " --type answer"))
               out)))))

(ert-deftest bp/mailbox-archives-incoming-and-outgoing-mail ()
  (bp/mailbox-test-with
    (should (= 0 (bp/lock-test--cli-ok "A" "acquire" bp/mailbox-test--dir)))
    (bp/agent-orchestration-send "T-B" bp/mailbox-test--dir "status" "In" "hello")
    (cl-letf (((symbol-function 'bp/agent-orchestration-deliver) #'ignore))
      (bp/agent-orchestration-send "T-A" "T-B" "answer" "Out" "bye"
                                   bp/mailbox-test--dir))
    (let ((in (bp/mailbox-test--archive "in"))
          (out (bp/mailbox-test--archive "out")))
      (should (= 1 (length in)))
      (should (string-match-p "^From: session B (terminal T-B)$" (car in)))
      (should (string-match-p (concat "^To: mailbox "
                                      (regexp-quote bp/mailbox-test--dir) "$")
                              (car in)))
      (should (string-match-p "^Subject: In$" (car in)))
      (should (string-match-p "\n\nhello\n\\'" (car in)))
      (should (= 1 (length out)))
      (should (string-match-p
               (concat "^From: mailbox " (regexp-quote bp/mailbox-test--dir)
                       ", session A (terminal T-A)$")
               (car out)))
      (should (string-match-p "^To: session B (terminal T-B)$" (car out)))
      (should (string-match-p "^Type: answer$" (car out))))))

(ert-deftest bp/mailbox-reply-line-says-whether-a-reply-is-expected ()
  (let ((msg (lambda (type)
               (bp/agent-orchestration--format-message
                (list :from nil :from-key '("session" "S") :from-label "s"
                      :type type :subject "x" :body "" :files "" :report "")))))
    (should (string-match-p "^Reply: emacs-agent send --to S --type answer"
                            (funcall msg "question")))
    (should (string-match-p "^Reply (optional): emacs-agent send --to S --subject"
                            (funcall msg "status")))
    (should (string-match-p "^Reply (optional): " (funcall msg "answer")))))

(ert-deftest bp/mailbox-pending-lists-only-mailboxes-without-a-running-listener ()
  (bp/mailbox-test-with
    (bp/agent-orchestration-send "T-B" bp/mailbox-test--dir "status" "First" "")
    (bp/agent-locks--reload)
    (should (equal (list bp/mailbox-test--dir)
                   (mapcar (lambda (m) (plist-get m :dir))
                           (bp/agent-mailboxes--pending))))
    (should (equal "First" (plist-get (car bp/agent-mailboxes--unread)
                                      :oldest-subject)))
    ;; A holder that is not running still leaves it pending, and says so.
    (should (= 0 (bp/lock-test--cli-ok "A" "acquire" bp/mailbox-test--dir)))
    (sqlite-execute (bp/agent-sessions--db)
                    "UPDATE locks SET holder_pid = 999999, holder_pid_start = 'long ago'")
    (bp/agent-locks--reload)
    (with-temp-buffer
      (bp/agent-mailboxes--insert-section)
      (should (string-match-p "✉1 .*First  — locked by .* (not running)"
                              (buffer-string))))
    ;; A running holder collects its own mail: not listed.
    (sqlite-execute (bp/agent-sessions--db)
                    "UPDATE locks SET holder_pid = ?, holder_pid_start = ?"
                    (list (emacs-pid) (bp/agent-locks--pid-start (emacs-pid))))
    (bp/agent-locks--reload)
    (should-not (bp/agent-mailboxes--pending))
    (with-temp-buffer
      (bp/agent-mailboxes--insert-section)
      (should (string-empty-p (buffer-string))))))

(defconst bp/mailbox-test--stub-reply-file
  "reply=$(printf '%s' \"$2\" | sed -n '1s/^(let ((f \"\\([^\"]*\\)\").*/\\1/p')\n"
  "Shell line for a stub emacsclient: set $reply to the file emacs-agent
reads Emacs's answer from, as named at the head of the elisp it sends.")

(ert-deftest bp/agent-cli-reads-the-reply-from-its-file-not-emacsclient-output ()
  ;; Real emacsclient can garble a long reply on stdout; the stub prints that
  ;; garbage and writes the true reply to the file, which must win, byte-exact.
  (let* ((stub-dir (make-temp-file "agent-reply-stub-" t))
         (stub (expand-file-name "emacsclient" stub-dir))
         (fixture (expand-file-name "fixture" stub-dir))
         (text (concat
                (mapconcat (lambda (i)
                             (format "%d. a \"quoted\" \\path & more\n- दash line" i))
                           (number-sequence 1 400) "\n")
                "\n\nlast line &_ ends here")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region text nil fixture nil 'silent))
          (with-temp-file stub
            (insert "#!/bin/sh\n" bp/mailbox-test--stub-reply-file
                    "cat " (shell-quote-argument fixture) " > \"$reply\"\n"
                    "printf '\"partial\\n*ERROR*: Unknown message: -\\n"
                    "*ERROR*: Unknown message: print-nonl &_x\\n'\n"))
          (set-file-modes stub #o755)
          (let ((process-environment
                 (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                       (bp/lock-test--env nil '("EMACS_AGENT_SESSION_ID=T-1"))))
                (coding-system-for-read 'utf-8-unix))
            (with-temp-buffer
              (should (= 0 (call-process
                            (expand-file-name "emacs-agent" bp/agent-sessions--bin-dir)
                            nil t nil "list")))
              (should (equal text (buffer-string))))))
      (delete-directory stub-dir t))))

(ert-deftest bp/agent-cli-retries-a-refused-connection ()
  ;; A busy Emacs refuses connections in bursts; a refusal never reached it,
  ;; so the CLI retries.  Any other failure still ends the command at once.
  (let* ((stub-dir (make-temp-file "agent-refused-stub-" t))
         (stub (expand-file-name "emacsclient" stub-dir))
         (count (expand-file-name "count" stub-dir)))
    (unwind-protect
        (cl-flet ((run (tries failure)
                    (with-temp-file stub
                      (insert "#!/bin/sh\n" bp/mailbox-test--stub-reply-file
                              "n=$(cat " (shell-quote-argument count) " 2>/dev/null || echo 0)\n"
                              "echo $((n + 1)) > " (shell-quote-argument count) "\n"
                              "if [ \"$n\" -lt 2 ]; then echo " (shell-quote-argument failure) " >&2; exit 1; fi\n"
                              "printf 'workers: 0' > \"$reply\"\n"))
                    (set-file-modes stub #o755)
                    (when (file-exists-p count) (delete-file count))
                    (let ((process-environment
                           (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                                 (bp/lock-test--env
                                  nil (list "EMACS_AGENT_SESSION_ID=T-1"
                                            (format "EMACS_AGENT_CONNECT_TRIES=%d" tries))))))
                      (with-temp-buffer
                        (list (call-process (expand-file-name "emacs-agent"
                                                              bp/agent-sessions--bin-dir)
                                            nil t nil "list")
                              (buffer-string))))))
          (let ((refused "emacsclient: can't connect to /tmp/server: Connection refused"))
            (should (equal '(0 "workers: 0") (run 3 refused)))
            ;; Out of tries: the refusal is reported.
            (pcase-let ((`(,code ,output) (run 2 refused)))
              (should (= 1 code))
              (should (string-match-p "Connection refused" output))))
          (pcase-let ((`(,code ,output) (run 3 "*ERROR*: void-function x")))
            (should (= 1 code))
            (should (string-match-p "void-function" output))
            (should (equal "1\n" (with-temp-buffer
                                   (insert-file-contents count)
                                   (buffer-string))))))
      (delete-directory stub-dir t))))

(ert-deftest bp/agent-cli-wait-rides-out-an-unreachable-emacs ()
  ;; A watcher outlives Emacs restarts: unreachable is waited out like silence.
  (let* ((stub-dir (make-temp-file "agent-unreachable-stub-" t))
         (stub (expand-file-name "emacsclient" stub-dir))
         (count (expand-file-name "count" stub-dir))
         (errors (expand-file-name "stderr" stub-dir)))
    (unwind-protect
        (cl-flet ((run (down-for &rest args)
                    (with-temp-file stub
                      (insert "#!/bin/sh\n" bp/mailbox-test--stub-reply-file
                              "n=$(cat " (shell-quote-argument count) " 2>/dev/null || echo 0)\n"
                              "echo $((n + 1)) > " (shell-quote-argument count) "\n"
                              (format "if [ \"$n\" -lt %d ]; then " down-for)
                              "echo \"emacsclient: can't find socket\" >&2; exit 1; fi\n"
                              "printf 'messages: 1\\n--- mail' > \"$reply\"\n"))
                    (set-file-modes stub #o755)
                    (when (file-exists-p count) (delete-file count))
                    (let ((process-environment
                           (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                                 (bp/lock-test--env
                                  nil '("EMACS_AGENT_SESSION_ID=T-1"
                                        "EMACS_AGENT_CONNECT_TRIES=1")))))
                      (with-temp-buffer
                        (list (apply #'call-process
                                     (expand-file-name "emacs-agent"
                                                       bp/agent-sessions--bin-dir)
                                     nil (list t errors) nil "wait" args)
                              (buffer-string)
                              (with-temp-buffer
                                (insert-file-contents errors)
                                (buffer-string)))))))
          ;; Down for two polls, then the mail is delivered.
          (pcase-let ((`(,code ,out ,err)
                       (run 2 "--timeout" "30" "--interval" "0")))
            (should (= 0 code))
            (should (equal "messages: 1\n--- mail\n" out))
            (should (equal "emacs-agent: cannot reach Emacs; still waiting\n" err)))
          ;; Down for the whole wait: it ends as a plain timeout.
          (pcase-let ((`(,code ,out ,_)
                       (run 1000 "--timeout" "1" "--interval" "1")))
            (should (= 0 code))
            (should (string-prefix-p "messages: 0 (no mail after" out))))
      (delete-directory stub-dir t))))

(defvar bp/mailbox-test--stdin nil
  "File `bp/mailbox-test--agent' feeds emacs-agent as stdin; nil for /dev/null.")

(ert-deftest bp/agent-cli-send-reads-the-body-from-stdin-without-body ()
  (bp/mailbox-test-with
    (let ((stdin (make-temp-file "agent-send-stdin-" nil nil
                                 "piped \"body\"\n- second line")))
      (unwind-protect
          (progn
            ;; A heredoc or pipe stands in for --body.
            (pcase-let ((`(,code ,_ ,elisp)
                         (let ((bp/mailbox-test--stdin stdin))
                           (bp/mailbox-test--agent "A" "send" "--to" "T-2"
                                                   "--subject" "s"))))
              (should (= 0 code))
              (should (string-match-p
                       (regexp-quote "\"s\" \"piped \\\"body\\\"\n- second line\"")
                       elisp)))
            ;; An explicit --body wins, and stdin is not read.
            (pcase-let ((`(,code ,_ ,elisp)
                         (let ((bp/mailbox-test--stdin stdin))
                           (bp/mailbox-test--agent "A" "send" "--to" "T-2"
                                                   "--subject" "s" "--body" "given"))))
              (should (= 0 code))
              (should (string-match-p (regexp-quote "\"s\" \"given\"") elisp)))
            ;; Neither pipe nor file (here /dev/null): an empty body, at once.
            (pcase-let ((`(,code ,_ ,elisp)
                         (bp/mailbox-test--agent "A" "send" "--to" "T-2"
                                                 "--subject" "s")))
              (should (= 0 code))
              (should (string-match-p (regexp-quote "\"s\" \"\"") elisp))))
        (delete-file stdin)))))

(defun bp/mailbox-test--agent (who &rest args)
  "Run emacs-agent ARGS as conversation WHO against a stub emacsclient.
The stub records the elisp it was asked to evaluate and answers as Emacs
would on success, so this tests what the shell itself decides.  Returns
\(EXIT-CODE OUTPUT ELISP)."
  (let* ((stub-dir (make-temp-file "agent-mailbox-stub-" t))
         (record (expand-file-name "called" stub-dir))
         (stub (expand-file-name "emacsclient" stub-dir)))
    (unwind-protect
        (progn
          (with-temp-file stub
            (insert "#!/bin/sh\nprintf '%s' \"$2\" > " (shell-quote-argument record) "\n"
                    bp/mailbox-test--stub-reply-file
                    "case \"$2\" in\n"
                    "  *orchestration-check*) printf 'messages: 0' > \"$reply\" ;;\n"
                    "  *) printf 'Sent ok' > \"$reply\" ;;\n"
                    "esac\n"))
          (set-file-modes stub #o755)
          (let ((process-environment
                 (cons (concat "PATH=" stub-dir ":" (getenv "PATH"))
                       (bp/lock-test--env who '("EMACS_AGENT_SESSION_ID=T-1"))))
                (default-directory (file-name-directory
                                    (directory-file-name bp/mailbox-test--dir))))
            (with-temp-buffer
              (let ((code (apply #'call-process
                                 (expand-file-name "emacs-agent"
                                                   bp/agent-sessions--bin-dir)
                                 bp/mailbox-test--stdin t nil args)))
                (list code (buffer-string)
                      (and (file-exists-p record)
                           (with-temp-buffer
                             (insert-file-contents record)
                             (buffer-string))))))))
      (delete-directory stub-dir t))))

(ert-deftest bp/mailbox-cli-resolves-paths-and-claims-the-lock ()
  (bp/mailbox-test-with
    (let ((relative (concat "./" (file-name-nondirectory
                                  (directory-file-name bp/mailbox-test--dir)))))
      ;; A relative --to is resolved in the caller's directory.
      (pcase-let ((`(,code ,_ ,elisp)
                   (bp/mailbox-test--agent "A" "send" "--to" relative
                                           "--subject" "s")))
        (should (= 0 code))
        (should (string-match-p (regexp-quote
                                 (format "\"T-1\" \"%s\"" bp/mailbox-test--dir))
                                elisp)))
      ;; --from takes the lock, then names the mailbox to Emacs.
      (pcase-let ((`(,code ,_ ,elisp)
                   (bp/mailbox-test--agent "A" "send" "--to" "T-2" "--from"
                                           relative "--subject" "s")))
        (should (= 0 code))
        (should (string-match-p (regexp-quote
                                 (format "\"%s\")" bp/mailbox-test--dir))
                                elisp)))
      (should (equal '(("session" "A"))
                     (bp/lock-test--rows "SELECT holder_kind, holder_key FROM locks")))
      ;; Someone else holding it: refused before Emacs is asked anything.
      (pcase-let ((`(,code ,output ,elisp)
                   (bp/mailbox-test--agent "B" "wait" "--mailbox" relative
                                           "--timeout" "0")))
        (should (= 1 code))
        (should (string-match-p "held by" output))
        (should-not elisp))
      (pcase-let ((`(,code ,_ ,elisp)
                   (bp/mailbox-test--agent "A" "check" "--mailbox" relative)))
        (should (= 0 code))
        (should (string-match-p (regexp-quote
                                 (format "t \"%s\")" bp/mailbox-test--dir))
                                elisp)))
      (pcase-let ((`(,code ,output ,_)
                   (bp/mailbox-test--agent "A" "send-and-wait" "--to" "T-2"
                                           "--from" relative "--subject" "s")))
        (should (= 1 code))
        (should (string-match-p "does not take --from" output)))
      (pcase-let ((`(,code ,output ,_)
                   (bp/mailbox-test--agent "A" "send" "--to" "./no-such-mailbox"
                                           "--subject" "s")))
        (should (= 1 code))
        (should (string-match-p "no such mailbox directory" output))))))

(ert-deftest bp/mailbox-start-prompt-names-the-agent-and-its-files ()
  (let ((bp/agent-mailboxes-agents-directory "~/dev/agents/"))
    (should (equal "You are luffa. Read ~/dev/agents/PLAYBOOK.md and then ~/dev/agents/luffa/context.md"
                   (bp/agent-mailboxes--start-prompt
                    (expand-file-name "~/dev/agents/luffa/"))))))

(defun bp/mailbox-test--spawn (from)
  "Spawn a worker from T-A with FROM as its mailbox, launching nothing.
Return (OUTPUT ENVIRONMENT BRIEF-TEXT) or (OUTPUT nil nil) on refusal."
  (let* ((bp/agent-orchestration--brief-dir (make-temp-file "agent-briefs-" t))
         (worktree (make-temp-file "agent-worktree-" t))
         (worker (generate-new-buffer " *mailbox-worker*"))
         env)
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-orchestration--ensure-trusted) #'ignore)
                  ((symbol-function 'bp/agent-sessions--terminal-send) #'ignore)
                  ((symbol-function 'bp/agent-sessions--set-title-override) #'ignore)
                  ((symbol-function 'bp/agent-sessions--create-terminal)
                   (lambda (&rest _)
                     (setq env bp/agent-sessions--extra-terminal-env)
                     (with-current-buffer worker
                       (setq-local bp/agent-session-id "T-W"))
                     worker)))
          (let ((out (bp/agent-orchestration-spawn "T-A" "claude" worktree
                                                   "Do the thing" "" "" from)))
            (list out env
                  (let ((brief (car (directory-files
                                     bp/agent-orchestration--brief-dir t "\\.md\\'"))))
                    (and brief (with-temp-buffer
                                 (insert-file-contents brief)
                                 (buffer-string)))))))
      (kill-buffer worker)
      (delete-directory worktree t)
      (delete-directory bp/agent-orchestration--brief-dir t))))

(ert-deftest bp/mailbox-spawned-worker-reports-to-the-mailbox ()
  (bp/mailbox-test-with
    ;; Without the lock, nothing is spawned.
    (pcase-let ((`(,out ,env ,_) (bp/mailbox-test--spawn bp/mailbox-test--dir)))
      (should (string-match-p "do not hold the lock" out))
      (should-not env))
    (should (= 0 (bp/lock-test--cli-ok "A" "acquire" bp/mailbox-test--dir)))
    (pcase-let ((`(,out ,env ,brief) (bp/mailbox-test--spawn bp/mailbox-test--dir)))
      (should (string-match-p (concat "Reports to: mailbox "
                                      (regexp-quote bp/mailbox-test--dir))
                              out))
      (should (member (concat "EMACS_AGENT_COORDINATOR=" bp/mailbox-test--dir) env))
      (should (string-match-p (concat "Your coordinator   "
                                      (regexp-quote bp/mailbox-test--dir))
                              brief)))
    ;; Without --from the terminal stays the coordinator.
    (pcase-let ((`(,out ,env ,_) (bp/mailbox-test--spawn "")))
      (should-not (string-match-p "Reports to" out))
      (should (member "EMACS_AGENT_COORDINATOR=T-A" env)))))

;;; agent-sessions-lock-test.el ends here
