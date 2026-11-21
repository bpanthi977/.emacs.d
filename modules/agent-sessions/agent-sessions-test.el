;;; agent-sessions-test.el --- Tests for agent-sessions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-sessions)

(defun bp/agent-orchestration-test--message (type subject)
  (list :id (concat "test-" subject) :from "sender" :from-label "sender"
        :type type :subject subject :body "" :files "" :report "" :read nil))

(defmacro bp/agent-sessions-test--with-temp-db (&rest body)
  "Run BODY with an isolated agent-sessions database."
  (declare (indent 0) (debug t))
  `(let* ((db-file (make-temp-file "agent-sessions-test-"))
          (snapshot-directory
           (make-temp-file "agent-sessions-snapshots-test-" t))
          (bp/agent-sessions-db-file db-file)
          (bp/agent-sessions-file-snapshot-directory snapshot-directory)
          (bp/agent-sessions--db nil)
          (bp/agent-sessions--shared-files nil)
          (bp/agent-sessions--shared-paths nil)
          (bp/agent-sessions--shared-links nil))
     (unwind-protect
         (progn ,@body)
       (when (sqlitep bp/agent-sessions--db)
         (sqlite-close bp/agent-sessions--db))
       (dolist (path (list db-file (concat db-file "-wal")
                           (concat db-file "-shm")))
         (when (file-exists-p path)
           (delete-file path)))
       (delete-directory snapshot-directory t))))

(ert-deftest bp/agent-hook-routes-stale-daemon-id-to-unresolved-terminal ()
  (bp/agent-sessions-test--with-temp-db
   (let* ((directory (make-temp-file "agent-hook-worktree-" t))
          (target (generate-new-buffer " *agent-hook-target*"))
          (other (generate-new-buffer " *agent-hook-other*"))
          (payload-file (make-temp-file "agent-hook-payload-"))
          (bp/agent-session-id->buffer (make-hash-table :test 'equal))
          (bp/agent-sessions (make-hash-table :test 'equal))
          (bp/agent-session-notify-on-attention nil)
          logged)
     (unwind-protect
         (progn
           (with-current-buffer target
             (setq-local bp/agent-session-id "actual-terminal")
             (setq default-directory (file-name-as-directory directory)))
           (with-current-buffer other
             (setq-local bp/agent-session-id "other-terminal")
             (setq default-directory (file-name-as-directory directory)))
           (puthash "actual-terminal" target bp/agent-session-id->buffer)
           (puthash "other-terminal" other bp/agent-session-id->buffer)
           ;; A terminal in the same worktree already owned by another thread
           ;; must not make routing ambiguous.
           (puthash "other-terminal"
                    (list :buffer other :agent-type "codex"
                          :agent-session-id "other-thread")
                    bp/agent-sessions)
           (with-temp-file payload-file
             (insert
              (json-serialize
               (let ((payload (make-hash-table :test 'equal)))
                 (puthash "hook_event_name" "Stop" payload)
                 (puthash "session_id" "actual-thread" payload)
                 (puthash "cwd" directory payload)
                 payload))))
           (cl-letf (((symbol-function 'bp/agent-sessions--foreground-command)
                      (lambda (buf)
			(and (eq buf target) "codex")))
                     ((symbol-function 'bp/agent-session--repo-info)
                      (lambda (_)
			(list :repo "repo" :repo-root directory
                              :worktree "worktree"
                              :worktree-path directory)))
                     ((symbol-function 'bp/agent-sessions--log-update)
                      (lambda (buf type sid)
			(setq logged (list buf type sid))))
                     ((symbol-function 'bp/agent-orchestration--hydrate) #'ignore)
                     ((symbol-function 'bp/agent-orchestration-deliver) #'ignore)
                     ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                      #'ignore))
             ;; The inherited daemon id no longer names any live terminal.
             (bp/agent-hook-notify "stale-terminal" "codex" payload-file))
           (let ((session (gethash "actual-terminal" bp/agent-sessions)))
             (should (equal "actual-thread"
                            (plist-get session :agent-session-id)))
             (should (eq target (plist-get session :buffer)))
             (should (eq 'needs-attention (plist-get session :status))))
           (should-not (gethash "stale-terminal" bp/agent-sessions))
           (should (equal (list target "codex" "actual-thread") logged)))
       (when (file-exists-p payload-file)
         (delete-file payload-file))
       (kill-buffer target)
       (kill-buffer other)
       (delete-directory directory t)))))

(ert-deftest bp/agent-hook-follows-claude-clear-to-new-conversation ()
  (bp/agent-sessions-test--with-temp-db
   (let* ((directory (make-temp-file "agent-hook-clear-" t))
          (term (generate-new-buffer " *agent-hook-clear*"))
          (bp/agent-session-id->buffer (make-hash-table :test 'equal))
          (bp/agent-sessions (make-hash-table :test 'equal))
          (bp/agent-session-notify-on-attention nil))
     (cl-flet ((deliver (event sid)
                 (let ((file (make-temp-file "agent-hook-payload-")))
                   (with-temp-file file
                     (insert (json-serialize
                              (let ((p (make-hash-table :test 'equal)))
                                (puthash "hook_event_name" event p)
                                (puthash "session_id" sid p)
                                (puthash "cwd" directory p)
                                p))))
                   (bp/agent-hook-notify "1-1" "claude" file)))
               (rows ()
                 (sqlite-select (bp/agent-sessions--db)
                                "SELECT agent_sid, close_reason FROM sessions
                                  ORDER BY id")))
       (unwind-protect
           (cl-letf (((symbol-function 'bp/agent-session--repo-info)
                      (lambda (_) (list :repo "repo" :repo-root directory
                                        :worktree "wt"
                                        :worktree-path directory)))
                     ((symbol-function 'bp/agent-sessions--detect-parent)
                      #'ignore)
                     ((symbol-function 'bp/agent-orchestration--hydrate) #'ignore)
                     ((symbol-function 'bp/agent-orchestration-deliver) #'ignore)
                     ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                      #'ignore))
             (with-current-buffer term
               (setq-local bp/agent-session-id "1-1")
               (setq default-directory (file-name-as-directory directory)))
             (puthash "1-1" term bp/agent-session-id->buffer)
             (bp/agent-sessions--log-open term)
             (deliver "SessionStart" "old")
             ;; `/clear', with the old conversation's end delivered last.
             (deliver "SessionStart" "new")
             (deliver "SessionEnd" "old")
             (should (equal "new" (plist-get (gethash "1-1" bp/agent-sessions)
                                             :agent-session-id)))
             (should (equal '(("old" "superseded") ("new" nil)) (rows))))
         (kill-buffer term)
         (delete-directory directory t))))))

(ert-deftest bp/agent-sessions-share-file-prefers-durable-agent-id ()
  (let* ((source (make-temp-file "agent-share-routing-"))
         (target-buffer (generate-new-buffer " *agent-share-target*"))
         (wrong-buffer (generate-new-buffer " *agent-share-wrong*"))
         (target-session (list :buffer target-buffer
                               :agent-session-id "target-thread"))
         (wrong-session (list :buffer wrong-buffer
                              :agent-session-id "wrong-thread"))
         (bp/agent-sessions (make-hash-table :test 'equal))
         shared-with)
    (unwind-protect
        (progn
          ;; The daemon's inherited terminal id is live, but belongs to a
          ;; different session.  The durable agent id must win.
          (puthash "actual-terminal" target-session bp/agent-sessions)
          (puthash "stale-terminal" wrong-session bp/agent-sessions)
          (cl-letf (((symbol-function 'bp/agent-sessions--share-file)
                     (lambda (session _path _label)
                       (setq shared-with session)))
                    ((symbol-function
                      'bp/agent-sessions--revert-shared-file-buffer)
                     #'ignore)
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should
             (string-prefix-p
              "Shared "
              (bp/agent-sessions-share-file
               "stale-terminal" source "handoff" "target-thread")))
            (should (eq shared-with target-session))
            (setq shared-with nil)
            (should
             (equal
              "No Emacs agent session registered for agent id missing-thread"
              (bp/agent-sessions-share-file
               "stale-terminal" source "handoff" "missing-thread")))
            (should-not shared-with)))
      (delete-file source)
      (kill-buffer target-buffer)
      (kill-buffer wrong-buffer))))

(ert-deftest bp/agent-sessions-emacs-share-help-lists-every-state ()
  "`emacs-share --help' names each accepted state, and only those.
The help writes the states out with their meanings, so this keeps it in step
with `bp/agent-sessions-states'.  `state --help' is help, not a state."
  (let ((script (expand-file-name "bin/emacs-share"
                                  (file-name-directory
                                   (locate-library "agent-sessions"))))
        (process-environment (copy-sequence process-environment)))
    ;; Help must not need Emacs: no session, no emacsclient call.
    (setenv "EMACS_AGENT_SESSION_ID" nil)
    (dolist (args '(("--help") ("state" "--help")))
      (with-temp-buffer
        (should (= 0 (apply #'call-process script nil t nil args)))
        (let ((listed nil))
          (goto-char (point-min))
          (while (re-search-forward "^        \\([A-Z][A-Z-]+\\)  " nil t)
            (push (match-string 1) listed))
          (should (equal (nreverse listed) bp/agent-sessions-states)))))
    (with-temp-buffer
      (should (= 2 (call-process script nil t nil "state")))
      (should (string-match-p "--help" (buffer-string))))))

(ert-deftest bp/agent-sessions-emacs-share-passes-codex-thread-id ()
  (let* ((directory (make-temp-file "agent-share-cli-" t))
         (source (expand-file-name "handoff.md" directory))
         (capture (expand-file-name "expression" directory))
         (fake-client (expand-file-name "emacsclient" directory))
         (script (expand-file-name "bin/emacs-share"
                                   (file-name-directory
                                    (locate-library "agent-sessions"))))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (with-temp-file source (insert "handoff\n"))
          (with-temp-file fake-client
            (insert "#!/bin/sh\n"
                    "printf '%s' \"$2\" > \"$EMACS_SHARE_TEST_CAPTURE\"\n"
                    "printf '\"Shared test\"\\n'\n"))
          (set-file-modes fake-client #o755)
          (setenv "PATH" (concat directory path-separator (getenv "PATH")))
          (setenv "EMACS_AGENT_SESSION_ID" "stale-terminal")
          (setenv "CODEX_SESSION_ID" "older-codex-id")
          (setenv "CODEX_THREAD_ID" "current-thread")
          (setenv "EMACS_SHARE_TEST_CAPTURE" capture)
          (should (= 0 (call-process script nil nil nil "file" source)))
          (let ((expression (with-temp-buffer
                              (insert-file-contents capture)
                              (buffer-string))))
            (should (string-match-p
                     (concat (regexp-quote
                              "handoff.md\" \"\" \"current-thread\")")
                             "\\'")
                     expression))
            (should-not (string-match-p "older-codex-id" expression))))
      (delete-directory directory t))))

(ert-deftest bp/agent-sessions-session-row-omits-raw-hook-event ()
  (with-temp-buffer
    (bp/agent-sessions-dashboard-mode)
    (let ((inhibit-read-only t)
          (bp/agent-sessions--row-status nil)
          (entry (list :id "terminal"
                       :title "Investigate the issue"
                       :session (list :agent-type "codex"
                                      :status 'running
                                      :last-event "PreToolUse"))))
      (cl-letf (((symbol-function 'bp/agent-sessions--session-state)
                 (lambda (_) nil))
                ((symbol-function 'bp/agent-orchestration--row-badge)
                 (lambda (_) ""))
                ((symbol-function 'bp/agent-sessions--insert-note) #'ignore)
                ((symbol-function 'bp/agent-sessions--insert-links) #'ignore)
                ((symbol-function 'bp/agent-sessions--insert-files) #'ignore))
        (bp/agent-sessions--insert-session entry)
        (should (string-match-p "running" (buffer-string)))
        (should (string-match-p "Investigate the issue" (buffer-string)))
        (should-not (string-match-p "PreToolUse" (buffer-string)))))))

(ert-deftest bp/agent-orchestration-status-alone-does-not-steer-running-codex ()
  (let* ((id "terminal")
         (message (bp/agent-orchestration-test--message "status" "progress"))
         (session (list :status 'running :agent-type "codex"
                        :agent-session-id "thread"))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (puthash id (list message) bp/agent-orchestration--mail)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-orchestration--codex-queue)
               (lambda (_ messages) (setq queued messages) t))
              ((symbol-function 'bp/agent-orchestration--consume)
               (lambda (messages) (setq consumed messages))))
      (should-not (bp/agent-orchestration-deliver id))
      (should-not queued)
      (should-not consumed)
      (should-not (plist-get message :read)))))

(ert-deftest bp/agent-orchestration-actionable-mail-steers-with-earlier-status ()
  (let* ((id "terminal")
         (status (bp/agent-orchestration-test--message "status" "progress"))
         (question (bp/agent-orchestration-test--message "question" "decision"))
         (pending (list status question))
         (session (list :status 'running :agent-type 'codex
                        :agent-session-id "thread"))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed refreshed)
    (puthash id pending bp/agent-orchestration--mail)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-orchestration--codex-queue)
               (lambda (_ messages) (setq queued messages) t))
              ((symbol-function 'bp/agent-orchestration--consume)
               (lambda (messages) (setq consumed messages)))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               (lambda () (setq refreshed t))))
      (should (eq 'queued (bp/agent-orchestration-deliver id)))
      (should (equal pending queued))
      (should (equal pending consumed))
      (should refreshed))))

(ert-deftest bp/agent-orchestration-failed-native-queue-leaves-mail-unread ()
  (let* ((id "terminal")
         (question (bp/agent-orchestration-test--message "question" "decision"))
         (session (list :status 'running :agent-type "codex"
                        :agent-session-id "thread"))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         consumed)
    (puthash id (list question) bp/agent-orchestration--mail)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-orchestration--codex-queue)
               (lambda (&rest _) nil))
              ((symbol-function 'bp/agent-orchestration--consume)
               (lambda (messages) (setq consumed messages))))
      (should-not (bp/agent-orchestration-deliver id))
      (should-not consumed)
      (should-not (plist-get question :read)))))

(ert-deftest bp/agent-orchestration-generic-idle-delivery-still-pastes-all-mail ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *agent-delivery-test*"))
         (message (bp/agent-orchestration-test--message "status" "progress"))
         (session (list :status 'stopped :agent-type "other" :buffer buffer))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         pasted consumed)
    (unwind-protect
        (progn
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-orchestration--receptive-p)
                     (lambda (_) t))
                    ((symbol-function 'bp/agent-orchestration--paste)
                     (lambda (_ text) (setq pasted text)))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'delivered (bp/agent-orchestration-deliver id)))
            (should (equal (list message) consumed))
            (should (string-match-p "while you were idle" pasted))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-status-mail-queues-at-empty-running-claude ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-delivery-test*"))
         (message (bp/agent-orchestration-test--message "status" "progress"))
         (session (list :status 'running :agent-type "claude" :buffer buffer))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0\n────────────────\n  Opus"))
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'bp/agent-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (bp/agent-orchestration-deliver id)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-actionable-mail-queues-after-retained-claude-prompt ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-delivery-test*"))
         (message (bp/agent-orchestration-test--message "answer" "decision"))
         (session (list :status 'running :agent-type 'claude :buffer buffer
                        :submitted-prompt "previous request"))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0previous request\n"
                    "────────────────\n  Opus"))
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'bp/agent-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (bp/agent-orchestration-deliver id)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-claude-draft-withholds-actionable-mail ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-draft-delivery-test*"))
         (message (bp/agent-orchestration-test--message "answer" "decision"))
         (session (list :status 'stopped :agent-type 'claude :buffer buffer))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0my unfinished message\n"
                    "────────────────\n  Opus"))
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'bp/agent-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages))))
            (should-not (bp/agent-orchestration-deliver id))
            (should-not queued)
            (should-not consumed)
            (should-not (plist-get message :read))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-claude-submit-releases-withheld-mail ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-submit-delivery-test*"))
         (message (bp/agent-orchestration-test--message "status" "progress"))
         (session (list :status 'running :agent-type 'claude :buffer buffer))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          ;; Keep the old draft visible to prove the hook, rather than a
          ;; screen redraw race, is the authority that it was submitted.
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0my submitted message\n"
                    "────────────────\n  Opus"))
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'bp/agent-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (bp/agent-orchestration-deliver id t)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-claude-composer-detection-is-conservative ()
  (let* ((buffer (generate-new-buffer " *claude-composer-test*"))
         (session (list :agent-type "claude" :buffer buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "────────────────\n❯\u00a0\n────────────────\n  Opus")
          (should (bp/agent-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0first line\n  wrapped line\n"
                  "────────────────\n  Opus")
          (should-not
           (bp/agent-orchestration--claude-receptive-composer-p session))
          ;; Claude can retain the submitted prompt in the composer after the
          ;; hook fires.  Visual hard wrapping must not turn that into a draft.
          (setq session
                (plist-put session :submitted-prompt "first line wrapped line"))
          (should (bp/agent-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0a genuinely new draft\n"
                  "────────────────\n  Opus")
          (should-not
           (bp/agent-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0")
          (let ((start (point)))
            (insert "suggested completion")
            (add-text-properties
             start (point)
             '(font-lock-face (:inherit (eat-term-faint eat-term-font-0)))))
          (insert "\n────────────────\n  Opus")
          (should (bp/agent-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0typed prefix ")
          (let ((start (point)))
            (insert "suggested remainder")
            (add-text-properties
             start (point)
             '(font-lock-face (:inherit (eat-term-faint eat-term-font-0)))))
          (insert "\n────────────────\n  Opus")
          (should-not
           (bp/agent-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "Claude output without a recognizable composer")
          (should-not
           (bp/agent-orchestration--claude-receptive-composer-p session)))
      (kill-buffer buffer))))

(defun bp/agent-orchestration-test--claude-screen (composer &optional below)
  "A Claude screen whose composer box holds COMPOSER, with BELOW after it."
  (concat "● earlier output\n\n"
          "────────────────\n❯ " composer "\n────────────────\n"
          (or below "  Opus 5.5 · think:medium\n  ⏵⏵ auto mode on")))

(defmacro bp/agent-orchestration-test--with-hidden-ghostel (spec &rest body)
  "Run BODY with SPEC = (SESSION RENDERED LIVE) bound to a hidden ghostel session.
RENDERED is the stale buffer text; LIVE is what the terminal holds now
\(the form is evaluated on each read, so BODY may `setq' it)."
  (declare (indent 1))
  (let ((session (nth 0 spec)) (rendered (nth 1 spec)) (live (nth 2 spec))
        (buffer (make-symbol "buffer")))
    `(let* ((,buffer (generate-new-buffer " *claude-hidden-ghostel*"))
            (,session (list :agent-type "claude" :buffer ,buffer)))
       (unwind-protect
           (cl-letf (((symbol-function 'bp/agent-sessions--buffer-terminal-type)
                      (lambda (_) 'ghostel))
                     ((symbol-function 'ghostel--copy-all-text)
                      (lambda (_) ,live)))
             (with-current-buffer ,buffer
               (setq-local ghostel--term 'fake-term)
               (setq-local ghostel--pending-redraw t)
               (insert ,rendered))
             ,@body)
         (kill-buffer ,buffer)))))

(ert-deftest bp/agent-orchestration-ghostel-hidden-buffer-is-stale ()
  "Only a ghostel buffer with a pending redraw lags its terminal."
  (bp/agent-orchestration-test--with-hidden-ghostel (session "" "")
    (should (bp/agent-orchestration--ghostel-stale-p (plist-get session :buffer)))
    (with-current-buffer (plist-get session :buffer)
      (setq-local ghostel--pending-redraw nil))
    (should-not
     (bp/agent-orchestration--ghostel-stale-p (plist-get session :buffer)))))

(ert-deftest bp/agent-orchestration-claude-hidden-ghostel-reads-the-terminal ()
  "A hidden ghostel buffer is judged by what its terminal holds now.
ghostel does not draw hidden terminals, so the buffer can be empty or show an
old screen.  Mail used to wait there until the human submitted a prompt."
  (let (live)
    (bp/agent-orchestration-test--with-hidden-ghostel (session "" live)
      ;; Never shown since it started: the buffer is empty.
      (setq live (bp/agent-orchestration-test--claude-screen ""))
      (should (bp/agent-orchestration--claude-receptive-composer-p session))
      ;; A suggestion appeared while hidden: nobody could have typed it.
      (setq live (bp/agent-orchestration-test--claude-screen
                  "what's the status of all the agents?"))
      (should (bp/agent-orchestration--claude-receptive-composer-p session))
      ;; No composer recognized, or no text at all: wait.
      (setq live "Claude output without a recognizable composer")
      (should-not (bp/agent-orchestration--claude-receptive-composer-p session))
      (setq live nil)
      (should-not (bp/agent-orchestration--claude-receptive-composer-p session)))))

(ert-deftest bp/agent-orchestration-claude-hidden-ghostel-keeps-drafts ()
  "A draft typed before the terminal was hidden is still in the buffer; keep it."
  (let ((live (bp/agent-orchestration-test--claude-screen
               "a long human draft that the terminal wraps differently")))
    (bp/agent-orchestration-test--with-hidden-ghostel
        (session (bp/agent-orchestration-test--claude-screen
                  "a long human draft that\n  the terminal wraps differently")
                 live)
      (should-not (bp/agent-orchestration--claude-receptive-composer-p session))
      ;; Once submitted it is no longer a draft.
      (setq session (plist-put session :submitted-prompt
                               "a long human draft that the terminal wraps differently"))
      (should (bp/agent-orchestration--claude-receptive-composer-p session)))))

(ert-deftest bp/agent-orchestration-claude-hidden-ghostel-faint-suggestion ()
  "A suggestion shown faint when last seen is still a suggestion while hidden."
  (bp/agent-orchestration-test--with-hidden-ghostel
      (session "" (bp/agent-orchestration-test--claude-screen "open the PR"))
    (with-current-buffer (plist-get session :buffer)
      (insert "────────────────\n❯ ")
      (let ((start (point)))
        (insert "open the PR")
        (add-text-properties
         start (point) '(font-lock-face (:inherit (eat-term-faint)))))
      (insert "\n────────────────\n  Opus"))
    (should (bp/agent-orchestration--claude-receptive-composer-p session))))

(ert-deftest bp/agent-orchestration-claude-hidden-ghostel-ignores-dialogs ()
  "A dialog below the transcript's last prompt is not a composer."
  (bp/agent-orchestration-test--with-hidden-ghostel
      (session ""
               (concat "❯ fix the bug\n\n● Running a command\n"
                       "────────────────\n Bash command\n   ls\n"
                       " Do you want to proceed?\n ❯ 1. Yes\n   2. No\n"))
    (should-not (bp/agent-orchestration--claude-receptive-composer-p session))))

(ert-deftest bp/agent-orchestration-claude-queue-only-pastes-and-submits ()
  (let* ((buffer (generate-new-buffer " *claude-queue-test*"))
         (session (list :agent-type "claude" :buffer buffer))
         (message (bp/agent-orchestration-test--message "answer" "result"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-orchestration--terminal-input)
                   (lambda (_ input) (push (list 'input input) events)))
                  ((symbol-function 'bp/agent-sessions--terminal-paste)
                   (lambda (_ text) (push (list 'paste text) events)))
                  ((symbol-function 'run-at-time)
                   (lambda (_ _ function &rest args) (apply function args))))
          (should (bp/agent-orchestration--claude-queue session (list message)))
          (setq events (nreverse events))
          (should (equal (mapcar #'car events) '(paste input)))
          (should (equal (cadr (nth 1 events))
                         bp/agent-orchestration-claude-queue-submit-key))
          (should (string-match-p "Subject: result" (cadr (nth 0 events)))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-idle-codex-uses-native-queue-not-composer ()
  (let* ((id "terminal")
         (message (bp/agent-orchestration-test--message "status" "progress"))
         (session (list :status 'stopped :agent-type "codex"
                        :agent-session-id "thread"))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed pasted)
    (puthash id (list message) bp/agent-orchestration--mail)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-orchestration--codex-queue)
               (lambda (_ messages) (setq queued messages) t))
              ((symbol-function 'bp/agent-orchestration--consume)
               (lambda (messages) (setq consumed messages)))
              ((symbol-function 'bp/agent-orchestration--paste)
               (lambda (&rest _) (setq pasted t)))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               #'ignore))
      (should (eq 'queued (bp/agent-orchestration-deliver id)))
      (should (equal (list message) queued))
      (should (equal (list message) consumed))
      (should-not pasted))))

(ert-deftest bp/agent-orchestration-interrupted-codex-empty-prompt-delivers-directly ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *codex-interrupted-delivery-test*"))
         (message (bp/agent-orchestration-test--message "answer" "decision"))
         ;; Codex does not fire Stop for an interrupted turn, so this stale
         ;; status is the live failure mode this regression covers.
         (session (list :status 'running :agent-type "codex"
                        :agent-session-id "thread" :buffer buffer))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed pasted)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "■ Conversation interrupted - tell the model what to do differently.\n"
                    "\n› Ask Codex to do anything\n\n"
                    "  gpt-5.6-sol medium · Context 31% used"))
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-orchestration--codex-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'bp/agent-orchestration--paste)
                     (lambda (_ text) (setq pasted text)))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'delivered (bp/agent-orchestration-deliver id)))
            (should (equal (list message) consumed))
            (should (string-match-p "Subject: decision" pasted))
            (should-not queued)))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-codex-draft-still-uses-native-queue ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *codex-draft-delivery-test*"))
         (message (bp/agent-orchestration-test--message "answer" "decision"))
         (session (list :status 'stopped :agent-type "codex"
                        :agent-session-id "thread" :buffer buffer))
         (bp/agent-orchestration--mail (make-hash-table :test 'equal))
         queued consumed pasted)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "› Keep this human draft intact\n\n"
                    "  gpt-5.6-sol medium · Context 31% used"))
          (puthash id (list message) bp/agent-orchestration--mail)
          (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'bp/agent-orchestration--codex-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'bp/agent-orchestration--paste)
                     (lambda (&rest _) (setq pasted t)))
                    ((symbol-function 'bp/agent-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (bp/agent-orchestration-deliver id)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))
            (should-not pasted)))
      (kill-buffer buffer))))

(ert-deftest bp/agent-orchestration-native-codex-queue-uses-durable-thread-id ()
  (let ((session (list :agent-session-id "thread-uuid"))
        (message (bp/agent-orchestration-test--message "answer" "result"))
        invocation)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/codex"))
              ((symbol-function 'bp/agent-orchestration--call-with-timeout)
               (lambda (program args timeout)
                 (setq invocation (list program args timeout))
                 0)))
      (should (bp/agent-orchestration--codex-queue session (list message)))
      (should (equal (car invocation) "/bin/codex"))
      (should (equal (seq-take (cadr invocation) 4)
                     '("queue" "--thread" "thread-uuid" "--message")))
      (should (string-match-p "Subject: result" (nth 4 (cadr invocation))))
      (should (= (nth 2 invocation)
                 bp/agent-orchestration--native-queue-timeout)))))

(ert-deftest bp/agent-sessions-show-file-diff-opens-latest-commit-then-reads ()
  (let ((path "/repo/docs/plan.md")
        events)
    (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
               (lambda () (list 'session path (list :path path))))
              ((symbol-function 'magit-toplevel)
               (lambda (_) "/repo/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest args)
                 (push (list 'git default-directory args) events)
                 "abc123"))
              ((symbol-function 'magit-show-commit--arguments)
               (lambda () '(("--stat") nil)))
              ((symbol-function 'magit-show-commit)
               (lambda (commit args files &optional _module)
                 (push (list 'show default-directory commit args files) events)))
              ((symbol-function 'bp/agent-sessions--set-visited)
               (lambda (match-fn visitedp)
                 (push (list 'visited
                             (funcall match-fn (list :path path))
                             visitedp)
                       events)
                 t))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               (lambda () (push '(refresh) events))))
      (bp/agent-sessions-show-file-diff)
      (should
       (equal (nreverse events)
              '((git "/repo/"
                     ("log" "-1" "--format=%H" "--follow" "--"
                      "docs/plan.md"))
                (show "/repo/" "abc123" ("--stat") ("docs/plan.md"))
                (visited t t)
                (refresh)))))))

(ert-deftest bp/agent-sessions-show-file-diff-keeps-uncommitted-file-unread ()
  (let ((path "/repo/docs/plan.md")
        shown visited)
    (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
               (lambda () (list 'session path (list :path path))))
              ((symbol-function 'magit-toplevel)
               (lambda (_) "/repo/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _) nil))
              ((symbol-function 'magit-show-commit)
               (lambda (&rest _) (setq shown t)))
              ((symbol-function 'bp/agent-sessions--set-visited)
               (lambda (&rest _) (setq visited t))))
      (should-error (bp/agent-sessions-show-file-diff) :type 'user-error)
      (should-not shown)
      (should-not visited))))

(defun bp/agent-sessions-test--write-file (path contents)
  "Replace PATH with CONTENTS for a shared-file test."
  (with-temp-file path
    (insert contents)))

(ert-deftest bp/agent-sessions-share-file-keeps-latest-two-submissions ()
  (bp/agent-sessions-test--with-temp-db
    (let* ((source (make-temp-file "agent-shared-file-" nil ".md"))
           (session (list :agent-session-id "thread-1"))
           (key '("session" "thread-1")))
      (unwind-protect
          (progn
            (bp/agent-sessions-test--write-file source "first\n")
            (let ((first (bp/agent-sessions--share-file session source nil)))
              (should (equal source (plist-get first :path))))
            (let ((snapshot (bp/agent-sessions--file-snapshot-row
                             (list key) source)))
              (should-not (plist-get snapshot :previous-path))
              (should (equal "first\n"
                             (with-temp-buffer
                               (insert-file-contents
                                (plist-get snapshot :current-path))
                               (buffer-string)))))
            (bp/agent-sessions-test--write-file source "second\n")
            (bp/agent-sessions--share-file session source nil)
            (bp/agent-sessions-test--write-file source "third\n")
            (bp/agent-sessions--share-file session source nil)
            (let ((snapshot (bp/agent-sessions--file-snapshot-row
                             (list key) source)))
              (should (equal "second\n"
                             (with-temp-buffer
                               (insert-file-contents
                                (plist-get snapshot :previous-path))
                               (buffer-string))))
              (should (equal "third\n"
                             (with-temp-buffer
                               (insert-file-contents
                                (plist-get snapshot :current-path))
                               (buffer-string)))))
            (should
             (equal '((1))
                    (sqlite-select
                     (bp/agent-sessions--db)
                     "SELECT COUNT(*) FROM shared_file_snapshots"))))
        (delete-file source)))))

(ert-deftest bp/agent-sessions-file-snapshots-follow-durable-session-id ()
  (bp/agent-sessions-test--with-temp-db
    (let ((source (make-temp-file "agent-shared-file-"))
          (buffer (generate-new-buffer "agent-snapshot-terminal")))
      (unwind-protect
          (let ((terminal-session (list :buffer buffer)))
            (bp/agent-sessions-test--write-file source "before id\n")
            (bp/agent-sessions--share-file terminal-session source nil)
            (bp/agent-sessions-test--write-file source "after id\n")
            (let* ((durable-session
                    (list :buffer buffer :agent-session-id "thread-2"))
                   (durable-key '("session" "thread-2")))
              (bp/agent-sessions--share-file durable-session source nil)
              (let ((snapshot (bp/agent-sessions--file-snapshot-row
                               (list durable-key) source)))
                (should (equal "before id\n"
                               (with-temp-buffer
                                 (insert-file-contents
                                  (plist-get snapshot :previous-path))
                                 (buffer-string))))
                (should (equal "after id\n"
                               (with-temp-buffer
                                 (insert-file-contents
                                  (plist-get snapshot :current-path))
                                 (buffer-string)))))
              (should
               (equal '((0))
                      (sqlite-select
                       (bp/agent-sessions--db)
                       "SELECT COUNT(*) FROM shared_file_snapshots
                         WHERE kind = 'session' AND key = ?"
                       (list (buffer-name buffer)))))))
        (kill-buffer buffer)
        (delete-file source)))))

(ert-deftest bp/agent-sessions-show-submission-diff-compares-cache-then-reads ()
  (let* ((path "/outside-git/plan.md")
         (previous (make-temp-file "previous--plan-"))
         (current (make-temp-file "current--plan-"))
         (session (list :agent-session-id "thread-1"))
         (buffer (get-buffer-create "*Agent submission diff: plan.md*"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
                   (lambda () (list session path (list :path path))))
                  ((symbol-function 'bp/agent-sessions--file-snapshot-row)
                   (lambda (keys candidate)
                     (push (list 'snapshot keys candidate) events)
                     (list :previous-path previous :current-path current)))
                  ((symbol-function 'diff-no-select)
                   (lambda (old new switches no-async destination)
                     (push (list 'diff old new switches no-async destination)
                           events)
                     buffer))
                  ((symbol-function 'bp/agent-sessions--set-visited)
                   (lambda (match-fn visitedp)
                     (push (list 'visited
                                 (funcall match-fn (list :path path))
                                 visitedp)
                           events)
                     t))
                  ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                   (lambda () (push '(refresh) events)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (shown) (push (list 'pop shown) events))))
          (bp/agent-sessions-show-file-submission-diff)
          (should
           (equal
            (nreverse events)
            `((snapshot (("session" "thread-1")) ,path)
              (diff ,previous ,current "-u" t ,buffer)
              (visited t t)
              (refresh)
              (pop ,buffer)))))
      (kill-buffer buffer)
      (delete-file previous)
      (delete-file current))))

(ert-deftest bp/agent-sessions-show-submission-diff-needs-two-versions ()
  (let ((path "/outside-git/plan.md")
        visited)
    (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
               (lambda ()
                 (list (list :agent-session-id "thread-1") path
                       (list :path path))))
              ((symbol-function 'bp/agent-sessions--file-snapshot-row)
               (lambda (&rest _)
                 (list :current-path "/cache/current--plan.md")))
              ((symbol-function 'bp/agent-sessions--set-visited)
               (lambda (&rest _) (setq visited t))))
      (should-error (bp/agent-sessions-show-file-submission-diff)
                    :type 'user-error)
      (should-not visited))))

(ert-deftest bp/agent-sessions-capital-d-initializes-missing-snapshot ()
  (bp/agent-sessions-test--with-temp-db
    (let* ((path (make-temp-file "agent-shared-file-"))
           (session (list :agent-session-id "thread-1"))
           (key '("session" "thread-1"))
           visited message-text)
      (unwind-protect
          (progn
            (bp/agent-sessions-test--write-file path "current contents\n")
            (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
                       (lambda () (list session path (list :path path))))
                      ((symbol-function 'bp/agent-sessions--set-visited)
                       (lambda (&rest _) (setq visited t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq message-text
                               (apply #'format format-string args)))))
              (bp/agent-sessions-show-file-submission-diff))
            (let ((snapshot (bp/agent-sessions--file-snapshot-row
                             (list key) path)))
              (should snapshot)
              (should-not (plist-get snapshot :previous-path))
              (should
               (equal "current contents\n"
                      (with-temp-buffer
                        (insert-file-contents
                         (plist-get snapshot :current-path))
                        (buffer-string)))))
            (should (equal message-text
                           "Saved current file as its first submitted snapshot"))
            (should-not visited))
        (delete-file path)))))

(ert-deftest bp/agent-sessions-capital-d-is-contextual ()
  (let (action file-at-point)
    (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
               (lambda () file-at-point))
              ((symbol-function 'bp/agent-sessions-show-file-submission-diff)
               (lambda () (setq action 'diff)))
              ((symbol-function 'bp/agent-sessions-detach-parent)
               (lambda () (setq action 'detach))))
      (setq file-at-point '(session path file))
      (bp/agent-sessions-submission-diff-or-detach-parent)
      (should (eq action 'diff))
      (setq action nil file-at-point nil)
      (bp/agent-sessions-submission-diff-or-detach-parent)
      (should (eq action 'detach)))))

(ert-deftest bp/agent-sessions-shared-links-allow-multiple-per-kind ()
  (bp/agent-sessions-test--with-temp-db
    (let* ((session (list :agent-session-id "thread-1"))
           (first (bp/agent-sessions--share-link
                   session "linear" "https://linear.app/issue/ABC-1" "ABC-1"))
           (shared-at (plist-get first :shared-at)))
      (should-not (plist-get first :visited-at))
      (should (bp/agent-sessions--set-link-visited
               (lambda (link) (equal (plist-get link :url)
                                     "https://linear.app/issue/ABC-1")) t))
      (let ((same (bp/agent-sessions--share-link
                   session "linear" "https://linear.app/issue/ABC-1" "ABC-1")))
        (should (plist-get same :visited-at))
        (should (= shared-at (plist-get same :shared-at))))
      (let ((second (bp/agent-sessions--share-link
                     session "linear" "https://linear.app/issue/ABC-2" "ABC-2")))
        (should-not (plist-get second :visited-at)))
      (bp/agent-sessions--share-link
       session "pr" "https://github.com/org/repo/pull/1" "PR 1")
      (bp/agent-sessions--share-link
       session "pr" "https://github.com/org/repo/pull/2" "PR 2")
      (should (= 4 (length (bp/agent-sessions--session-links session))))
      (with-temp-buffer
        (bp/agent-sessions-dashboard-mode)
        (let ((inhibit-read-only t)
              (bp/agent-sessions--row-status nil))
          (bp/agent-sessions--insert-links session "  ")
          (dolist (label '("ABC-1" "ABC-2" "PR 1" "PR 2"))
            (should (string-match-p (regexp-quote label) (buffer-string))))))
      (should (equal '((2))
                     (sqlite-select
                      (bp/agent-sessions--db)
                      "SELECT COUNT(*) FROM shared_links
                        WHERE owner_kind = 'session'
                          AND owner_key = 'thread-1'
                          AND link_kind = 'linear'")))
      (should (bp/agent-sessions--detach-link
               session "https://linear.app/issue/ABC-2"))
      (should (= 3 (length (bp/agent-sessions--session-links session))))
      (should (plist-get (car (bp/agent-sessions--session-links session))
                         :visited-at)))))

(ert-deftest bp/agent-sessions-shared-links-migrate-old-key ()
  (bp/agent-sessions-test--with-temp-db
    (let ((db (bp/agent-sessions--db)))
      (sqlite-execute db "DROP TABLE shared_links")
      (sqlite-execute db "CREATE TABLE shared_links (
        owner_kind TEXT NOT NULL, owner_key TEXT NOT NULL,
        link_kind TEXT NOT NULL, url TEXT NOT NULL, label TEXT,
        shared_at INTEGER NOT NULL, updated_at INTEGER NOT NULL,
        visited_at INTEGER, PRIMARY KEY (owner_kind, owner_key, link_kind))")
      (sqlite-execute db "INSERT INTO shared_links VALUES
        ('session', 'thread-1', 'linear', 'https://linear.app/issue/ABC-1',
         'ABC-1', 100, 100, 101)")
      (sqlite-execute db "UPDATE meta SET value = '1' WHERE key = 'schema_version'")
      (bp/agent-sessions--db-migrate db)
      (bp/agent-sessions--share-link
       (list :agent-session-id "thread-1") "linear"
       "https://linear.app/issue/ABC-2" "ABC-2")
      (should (equal '((2))
                     (sqlite-select db "SELECT COUNT(*) FROM shared_links")))
      (should (equal '((101))
                     (sqlite-select db "SELECT visited_at FROM shared_links
                       WHERE url = 'https://linear.app/issue/ABC-1'"))))))

(ert-deftest bp/agent-sessions-reap-spares-live-emacs ()
  (bp/agent-sessions-test--with-temp-db
    (let ((db (bp/agent-sessions--db))
          (live (format "%d-1" (emacs-pid)))
          ;; Above any pid the kernel hands out, so never a running process.
          (dead "999999999-1"))
      (dolist (sid (list live dead))
        (sqlite-execute db "INSERT INTO sessions (emacs_sid, worktree, opened_at)
                            VALUES (?, '/tmp', 100)"
                        (list sid)))
      (bp/agent-sessions--meta-set db "last_seen" 200)
      ;; What a second Emacs opening the file does: the live one's row must
      ;; stay open, and only the dead one's is closed as a crash.
      (bp/agent-sessions--db-reap db)
      (should (equal '((nil nil))
                     (sqlite-select db "SELECT closed_at, close_reason
                                          FROM sessions WHERE emacs_sid = ?"
                                    (list live))))
      (should (equal '((200 "crash"))
                     (sqlite-select db "SELECT closed_at, close_reason
                                          FROM sessions WHERE emacs_sid = ?"
                                    (list dead)))))))

(ert-deftest bp/agent-sessions-exit-recloses-rows-wrongly-reaped ()
  (bp/agent-sessions-test--with-temp-db
    (let* ((db (bp/agent-sessions--db))
           (term (generate-new-buffer " *agent-log-term*"))
           (bp/agent-session-id->buffer (make-hash-table :test 'equal))
           held other)
      (unwind-protect
          (progn
            ;; Both rows closed as a crash by another Emacs; only the first
            ;; is still the current row of a terminal running here.
            (dolist (_ '(1 2))
              (sqlite-execute db "INSERT INTO sessions
                                    (emacs_sid, worktree, opened_at,
                                     closed_at, close_reason)
                                  VALUES ('1-1', '/tmp', 100, 150, 'crash')"))
            (setq held 1 other 2)
            (with-current-buffer term
              (setq-local bp/agent-session--log-row held))
            (puthash "1-1" term bp/agent-session-id->buffer)
            (bp/agent-sessions--log-close-all)
            (should (equal "emacs-exit"
                           (caar (sqlite-select db "SELECT close_reason
                                                     FROM sessions WHERE id = ?"
                                                (list held)))))
            (should (equal '((150 "crash"))
                           (sqlite-select db "SELECT closed_at, close_reason
                                               FROM sessions WHERE id = ?"
                                          (list other)))))
        (kill-buffer term)))))

(ert-deftest bp/agent-session-start-logs-resumed-identity ()
  (bp/agent-sessions-test--with-temp-db
    (let ((bp/agent-session-id->buffer (make-hash-table :test 'equal))
          term)
      (unwind-protect
          (cl-letf (((symbol-function 'bp/agent-session--repo-info)
                     (lambda (_) (list :worktree "wt" :worktree-path "/tmp")))
                    ((symbol-function 'bp/agent-session--spawn)
                     (lambda (&rest _)
                       (setq term (generate-new-buffer " *agent-log-resume*"))
                       (with-current-buffer term
                         (setq-local bp/agent-session-id "1-1"))
                       (bp/agent-sessions--log-open term)
                       term)))
            ;; No hook fires: an idle resumed Codex must still be restorable.
            (bp/agent-session-start "/tmp" 'codex "thread-1")
            (should (equal '(("codex" "thread-1"))
                           (sqlite-select (bp/agent-sessions--db)
                                          "SELECT agent_type, agent_sid
                                             FROM sessions")))
            (should (equal "thread-1"
                           (plist-get (car (bp/agent-sessions--log-rows
                                            "closed_at IS NULL"))
                                      :agent-sid))))
        (when (buffer-live-p term)
          (kill-buffer term))))))

(ert-deftest bp/agent-sessions-never-prompted-claude-is-not-resumable ()
  (let* ((config (make-temp-file "agent-claude-config-" t))
         (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config)
                                    process-environment)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "projects/-wt" config) t)
          (write-region "" nil (expand-file-name "projects/-wt/used.jsonl"
                                                 config))
          (should (bp/agent-sessions--log-resumable-p
                   (list :agent-type "claude" :agent-sid "used")))
          ;; Logged at `SessionStart', but no message was ever sent.
          (should-not (bp/agent-sessions--log-resumable-p
                       (list :agent-type "claude" :agent-sid "unused")))
          ;; Only Claude defers its transcript; others are taken at their word.
          (should (bp/agent-sessions--log-resumable-p
                   (list :agent-type "codex" :agent-sid "unused"))))
      (delete-directory config t))))

(ert-deftest bp/agent-sessions-mark-unread-moves-to-the-next-row ()
  (with-temp-buffer
    (bp/agent-sessions-dashboard-mode)
    (let ((inhibit-read-only t)
          (file (list :path "/tmp/agent-file-1" :visited-at nil)))
      (magit-insert-section (bp/agent-sessions-root)
        (magit-insert-section (bp/agent-session-row "session-1")
          (magit-insert-heading "session 1\n")
          (magit-insert-section (bp/agent-session-file "/tmp/agent-file-1")
            (magit-insert-heading "file 1\n"))
          (magit-insert-section (bp/agent-session-file "/tmp/agent-file-2")
            (magit-insert-heading "file 2\n")))
        (magit-insert-section (bp/agent-session-row "session-2")
          (magit-insert-heading "session 2\n")
          (magit-insert-section (bp/agent-session-file "/tmp/agent-file-3")
            (magit-insert-heading "file 3\n"))))
      (goto-char (point-min))
      (search-forward "file 1")
      (cl-letf (((symbol-function 'bp/agent-sessions--file-at-point)
                 (lambda () (list 'session "/tmp/agent-file-1" file)))
                ((symbol-function 'bp/agent-sessions--set-visited)
                 (lambda (_match visited)
                   (plist-put file :visited-at (and visited 1))
                   t))
                ((symbol-function 'bp/agent-sessions--refresh) #'ignore))
        (bp/agent-sessions-mark-unread)
        (should (equal "/tmp/agent-file-2"
                       (oref (magit-current-section) value)))
        (should (plist-get file :visited-at))
        ;; Past a session's last file, on to the next session's row.
        (bp/agent-sessions-mark-unread)
        (should (equal "session-2" (oref (magit-current-section) value)))
        ;; From a session row to its first file; the last row stays put.
        (bp/agent-sessions-mark-unread)
        (should (equal "/tmp/agent-file-3"
                       (oref (magit-current-section) value)))
        (bp/agent-sessions-mark-unread)
        (should (equal "/tmp/agent-file-3"
                       (oref (magit-current-section) value))))
      (should (eq (lookup-key bp/agent-sessions-dashboard-mode-map (kbd "m"))
                  (lookup-key bp/agent-sessions-dashboard-mode-map (kbd "u")))))))

(ert-deftest bp/agent-sessions-mark-unread-toggles-a-session-row ()
  (let ((buf (generate-new-buffer " *agent-session-toggle*"))
        (bp/agent-sessions (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-sessions--link-at-point) #'ignore)
                  ((symbol-function 'bp/agent-sessions--file-at-point) #'ignore)
                  ((symbol-function 'bp/agent-sessions--session-at-point)
                   (lambda () (list :buffer buf)))
                  ((symbol-function 'bp/agent-sessions--refresh) #'ignore))
          (with-current-buffer buf (setq-local bp/agent-session-id "s-1"))
          (puthash "s-1" (list :status 'stopped) bp/agent-sessions)
          (cl-flet ((marked ()
                      (buffer-local-value 'bp/agent-session--marked-unread buf)))
            ;; Read -> unread -> read.
            (bp/agent-sessions-mark-unread)
            (should (marked))
            (bp/agent-sessions-mark-unread)
            (should-not (marked))
            ;; An agent-reported needs-attention reads as unread: `u' marks it
            ;; read the way focusing would, without setting the manual mark.
            (puthash "s-1" (list :status 'needs-attention) bp/agent-sessions)
            (bp/agent-sessions-mark-unread)
            (should-not (marked))
            (should (eq 'stopped (plist-get (gethash "s-1" bp/agent-sessions)
                                            :status)))
            ;; The prefix argument always marks read.
            (bp/agent-sessions-mark-unread)
            (should (marked))
            (bp/agent-sessions-mark-unread t)
            (should-not (marked))
            (bp/agent-sessions-mark-unread t)
            (should-not (marked))))
      (kill-buffer buf))))

(ert-deftest bp/agent-sessions-share-link-validates-kind-and-url ()
  (let ((session (list :agent-session-id "thread-1"))
        refreshed)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               (lambda () (setq refreshed t))))
      (should (string-prefix-p
               "Unknown link kind"
               (bp/agent-sessions-share-link
                "terminal" "ticket" "https://example.com/1")))
      (should (string-prefix-p
               "Invalid link URL"
               (bp/agent-sessions-share-link
                "terminal" "linear" "file:///tmp/issue")))
      (should-not refreshed))))

(ert-deftest bp/agent-sessions-open-link-marks-read-before-browsing ()
  (let* ((link (list :kind "pr" :url "https://github.com/org/repo/pull/1"))
         events)
    (cl-letf (((symbol-function 'bp/agent-sessions--link-at-point)
               (lambda () (list 'session "pr" link)))
              ((symbol-function 'bp/agent-sessions--set-link-visited)
               (lambda (match-fn visitedp)
                 (push (list 'visited (funcall match-fn link) visitedp) events)
                 t))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               (lambda () (push '(refresh) events)))
              ((symbol-function 'browse-url)
               (lambda (url) (push (list 'browse url) events))))
      (bp/agent-sessions--open-link-at-point)
      (should
       (equal (nreverse events)
              '((visited t t)
                (refresh)
                (browse "https://github.com/org/repo/pull/1")))))))

(ert-deftest bp/agent-sessions-w-copies-attachment-value ()
  (let ((kill-ring nil)
        (link nil)
        (file nil))
    (cl-letf (((symbol-function 'bp/agent-sessions--link-at-point)
               (lambda () link))
              ((symbol-function 'bp/agent-sessions--file-at-point)
               (lambda () file)))
      (setq file '(session "/tmp/shared plan.md" nil))
      (bp/agent-sessions-copy-attachment)
      (should (equal "/tmp/shared plan.md" (car kill-ring)))
      (setq file nil
            link '(session "https://example.com/pr/1"
                           (:url "https://example.com/pr/1")))
      (bp/agent-sessions-copy-attachment)
      (should (equal "https://example.com/pr/1" (car kill-ring)))
      (setq link nil)
      (bp/agent-sessions-copy-attachment)
      (should (equal "https://example.com/pr/1" (car kill-ring))))
    (should (eq (lookup-key bp/agent-sessions-dashboard-mode-map (kbd "w"))
                #'bp/agent-sessions-copy-attachment))))

(ert-deftest bp/agent-sessions-share-state-sets-and-clears ()
  (let ((session (list :agent-session-id "thread-1"))
        writes refreshes)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-sessions--state-set)
               (lambda (keys state) (push (list keys state) writes)))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               (lambda () (setq refreshes (1+ (or refreshes 0))))))
      (should (equal "State: PLAN-REVIEW"
                     (bp/agent-sessions-share-state "terminal" "PLAN-REVIEW")))
      (should (equal "State cleared"
                     (bp/agent-sessions-share-state "terminal" "")))
      (should (equal (nreverse writes)
                     (list (list (bp/agent-sessions--session-note-keys session)
                                 "PLAN-REVIEW")
                           (list (bp/agent-sessions--session-note-keys session)
                                 nil))))
      (should (= refreshes 2)))))

(ert-deftest bp/agent-sessions-share-state-refuses-unknown-state ()
  (let ((session (list :agent-session-id "thread-1"))
        written)
    (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'bp/agent-sessions--state-set)
               (lambda (&rest _) (setq written t)))
              ((symbol-function 'bp/agent-sessions--refresh-if-visible)
               #'ignore))
      (let ((reply (bp/agent-sessions-share-state "terminal" "MERGED")))
        (should (string-prefix-p "Unknown state `MERGED'" reply))
        (should (string-match-p "PLAN-REVIEW" reply)))
      (should-not written))))

(ert-deftest bp/agent-sessions-share-state-without-session ()
  (cl-letf (((symbol-function 'bp/agent-sessions--session-for-share)
             (lambda (&rest _) nil)))
    (should (string-prefix-p
             "No Emacs terminal registered"
             (bp/agent-sessions-share-state "terminal" "DONE")))
    (should (string-prefix-p
             "No Emacs agent session registered for agent id thread-1"
             (bp/agent-sessions-share-state "terminal" "DONE" "thread-1")))))

(ert-deftest bp/agent-sessions-states-cycle-in-workflow-order ()
  (let ((seen nil) (state nil))
    (dotimes (_ (1+ (length bp/agent-sessions-states)))
      (setq state (bp/agent-sessions--next-state state))
      (push state seen))
    (should (equal (nreverse seen)
                   '("TODO" "PLANNING" "PLAN-REVIEW" "IMPLEMENTING"
                     "IMPL-REVIEW" "PR-OPEN" "AUTOFIX" "REVIEW" "DONE"
                     "BLOCKED" "LEARN" nil)))))

(ert-deftest bp/agent-sessions--pi-session-start-is-not-running ()
  ;; Pi fires session_start when its TUI opens at an idle prompt, so a fresh
  ;; session must land on `stopped', not fall through to the `running' default
  ;; in `bp/agent-hook-notify' — that is the frozen-running-at-nothing bug.
  (should (eq 'stopped (bp/agent-session--status-for "pi" "SessionStart")))
  (should (eq 'stopped (bp/agent-session--status-for "claude" "SessionStart")))
  ;; Codex fires its SessionStart lazily on the first prompt, the same instant
  ;; as UserPromptSubmit — an unordered SessionStart must be able to clobber
  ;; `running', so Codex gets no mapping at all.
  (should (null (bp/agent-session--status-for "codex" "SessionStart"))))

(ert-deftest bp/agent-sessions--reconcile-status-demotes-only-provably-idle ()
  (let* ((buffer (generate-new-buffer " *pi-reconcile-test*"))
       (session (list :status 'running :agent-type "pi" :buffer buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-sessions--tpgid-readable-p)
                   (lambda (_) t)))
          ;; Nothing in the foreground: the shell owns the pty, so pi cannot be
          ;; mid-turn — the killed-mid-turn freeze must read as stopped.
          (cl-letf (((symbol-function 'bp/agent-sessions--foreground-command)
                     (lambda (_) nil)))
            (let ((reconciled (bp/agent-sessions--reconcile-status session buffer)))
              (should (eq 'stopped (plist-get reconciled :status)))
              ;; Display-time only: the registry plist must survive untouched,
              ;; the way `bp/agent-sessions--unread-status' works.
              (should (eq 'running (plist-get session :status)))))
          ;; pi idling at its own prompt still holds the foreground with its
          ;; TUI — no demotion; the SessionStart mapping covers that case.
          (cl-letf (((symbol-function 'bp/agent-sessions--foreground-command)
                     (lambda (_) "pi")))
            (should (eq 'running
                        (plist-get (bp/agent-sessions--reconcile-status
                                    session buffer)
                                   :status)))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-sessions--reconcile-status-needs-tpgid-to-demote ()
  ;; On a platform whose process-attributes omits tpgid, a nil foreground says
  ;; "unknown", not "shell in front" — running must survive untouched.
  (let* ((buffer (generate-new-buffer " *pi-reconcile-tpgid-test*"))
       (session (list :status 'running :agent-type "pi" :buffer buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-sessions--tpgid-readable-p)
                   (lambda (_) nil))
                  ((symbol-function 'bp/agent-sessions--foreground-command)
                   (lambda (_) nil)))
          (should (eq 'running
                      (plist-get (bp/agent-sessions--reconcile-status
                                  session buffer)
                                 :status))))
      (kill-buffer buffer))))

(ert-deftest bp/agent-sessions-copy-tree-as-org-list ()
  (bp/agent-sessions-test--with-temp-db
    (let* ((bp/agent-sessions--notes nil)
           (bp/agent-sessions--order nil)
           (worktree (make-temp-file "agent-sessions-worktree-" t))
           (plan (expand-file-name "plan.md" worktree))
           (session (list :agent-session-id "thread-1" :agent-type "claude"
                          :worktree-path worktree))
           (row (magit-section))
           (kill-ring nil)
           (kill-ring-yank-pointer nil)
           (interprogram-cut-function nil))
      (unwind-protect
          (progn
            (oset row type 'bp/agent-session-row)
            (oset row value "terminal")
            (write-region "" nil plan)
            (bp/agent-sessions--note-set
             (bp/agent-sessions--session-note-keys session) "Done\nShipped")
            (bp/agent-sessions--share-link
             session "pr" "https://github.com/org/repo/pull/1" "PR #1")
            (bp/agent-sessions--share-file session plan "")
            (bp/agent-sessions--note-set
             (list (bp/agent-sessions--file-note-key plan)) "Read first")
            (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                       (lambda (_) session))
                      ((symbol-function 'magit-current-section)
                       (lambda () row)))
              (bp/agent-sessions-copy-tree))
            (should (equal
                     (concat
                      "- [[agent-session:claude::thread-1::"
                      (file-name-as-directory worktree)
                      "][claude thread-1]]\n"
                      "  Done\n"
                      "  Shipped\n"
                      "  - [[https://github.com/org/repo/pull/1][PR #1]]\n"
                      "  - [[file:" (abbreviate-file-name plan) "][plan.md]]\n"
                      "    Read first\n")
                     (current-kill 0))))
        (delete-directory worktree t)))))

(ert-deftest bp/agent-sessions-copy-tree-nests-forks-under-parent ()
  (bp/agent-sessions-test--with-temp-db
    (let* ((bp/agent-sessions--notes nil)
           (bp/agent-sessions--order nil)
           (sessions
            `(("parent" :agent-session-id "p" :agent-type "claude")
              ("child" :agent-session-id "c" :agent-type "claude"
               :branched-from ("p" . "parent"))
              ("other" :agent-session-id "o" :agent-type "claude")
              ;; A cycle has no root, and must still be copied once each.
              ("loop-a" :agent-session-id "a" :agent-type "claude"
               :branched-from ("b" . "loop-b"))
              ("loop-b" :agent-session-id "b" :agent-type "claude"
               :branched-from ("a" . "loop-a"))))
           (worktree (magit-section))
           (rows (mapcar (lambda (entry)
                           (let ((row (magit-section)))
                             (oset row type 'bp/agent-session-row)
                             (oset row value (car entry))
                             (oset row parent worktree)
                             row))
                         sessions)))
      (oset worktree type 'bp/agent-session-worktree)
      (oset worktree value (list :label "wt" :path "/nonexistent/wt"))
      (oset worktree children rows)
      (cl-letf (((symbol-function 'bp/agent-sessions--session-for-id)
                 (lambda (id) (cdr (assoc id sessions))))
                ((symbol-function 'bp/agent-sessions--session-org-link)
                 (lambda (session)
                   (cons (plist-get session :agent-session-id)
                         (plist-get session :agent-session-id)))))
        (should (equal (concat "- [[p][p]]\n"
                               "  - [[c][c]]\n")
                       (bp/agent-sessions--org-tree (car rows) "")))
        (should (equal (concat "- wt\n"
                               "  - [[p][p]]\n"
                               "    - [[c][c]]\n"
                               "  - [[o][o]]\n"
                               "  - [[a][a]]\n"
                               "    - [[b][b]]\n")
                       (bp/agent-sessions--org-tree worktree "")))))))

(ert-deftest bp/agent-sessions-manual-order-ignores-buffer-name ()
  (bp/agent-sessions-test--with-temp-db
   (let* ((agent (generate-new-buffer "*agent*"))
          (bare (generate-new-buffer "*bare*"))
          (entries
           (list (list :session (list :buffer agent :agent-session-id "uuid-1"))
                 (list :session (list :buffer bare))))
          (scope (bp/agent-sessions--sessions-scope "/repo" "/repo/wt"))
          (bp/agent-sessions--order nil))
     (unwind-protect
         (progn
           (with-current-buffer bare (setq-local bp/agent-session-id "123-4"))
           ;; An agent is keyed by its own session id, a bare terminal by its
           ;; terminal id.
           (should (equal (mapcar #'bp/agent-sessions--entry-order-key entries)
                          '("uuid-1" "123-4")))
           (bp/agent-sessions--order-write-group scope nil '("123-4" "uuid-1"))
           (with-current-buffer agent (rename-buffer "wt - renamed agent"))
           (with-current-buffer bare (rename-buffer "wt - renamed bare"))
           (should (equal (bp/agent-sessions--apply-manual-order
                           scope #'bp/agent-sessions--entry-order-key entries)
                          (reverse entries))))
       (mapc #'kill-buffer (list agent bare))))))

(defmacro bp/agent-sessions-test--with-titled-terminal (buf &rest body)
  "Run BODY with BUF bound to a terminal buffer in worktree `project_3'."
  (declare (indent 1))
  `(let ((,buf (generate-new-buffer "*eat*")))
     (unwind-protect
         (cl-letf (((symbol-function 'bp/agent-sessions--buffer-repo-info)
                    (lambda (_) (list :worktree-name "project_3"))))
           ,@body)
       (kill-buffer ,buf))))

(ert-deftest bp/agent-sessions-edit-title-renames-buffer ()
  (bp/agent-sessions-test--with-titled-terminal buf
    (cl-letf (((symbol-function 'bp/agent-sessions--session-at-point)
               (lambda () (list :buffer buf)))
              ((symbol-function 'bp/agent-sessions--refresh) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "Fix login")))
      (bp/agent-sessions-edit-title))
    (should (equal (buffer-name buf) "project_3 - Fix login"))
    (should (equal (buffer-local-value 'bp/agent-session-title-override buf)
                   "Fix login"))
    ;; Clearing the title keeps the name it last had.
    (cl-letf (((symbol-function 'bp/agent-sessions--session-at-point)
               (lambda () (list :buffer buf)))
              ((symbol-function 'bp/agent-sessions--refresh) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (bp/agent-sessions-edit-title))
    (should-not (buffer-local-value 'bp/agent-session-title-override buf))
    (should (equal (buffer-name buf) "project_3 - Fix login"))))

(ert-deftest bp/agent-sessions-restored-title-renames-buffer ()
  (bp/agent-sessions-test--with-titled-terminal buf
    (cl-letf (((symbol-function 'bp/agent-sessions--refresh-if-visible) #'ignore))
      (bp/agent-sessions--restore-title buf (list :title "Old task")))
    (should (equal (buffer-name buf) "project_3 - Old task"))))

(ert-deftest bp/agent-sessions-agent-title-renames-buffer ()
  "An agent's OSC title renames its buffer until the user sets one."
  (bp/agent-sessions-test--with-titled-terminal buf
    (let ((bp/agent-sessions (make-hash-table :test 'equal)))
      (with-current-buffer buf
        (setq-local bp/agent-session-id "agent-title")
        ;; A plain shell's title (no agent registered) leaves the name alone.
        (bp/agent-sessions--set-agent-title "user@host:~")
        (should (equal (buffer-name buf) "*eat*"))
        (puthash "agent-title" (list :buffer buf) bp/agent-sessions)
        (bp/agent-sessions--set-agent-title "Fix login")
        (should (equal (buffer-name buf) "project_3 - Fix login"))
        ;; Animated status glyphs don't reach the name.
        (bp/agent-sessions--set-agent-title "⠏ Fix login")
        (should (equal (buffer-name buf) "project_3 - Fix login"))
        (bp/agent-sessions--set-agent-title "✳ Fix login")
        (should (equal (buffer-name buf) "project_3 - Fix login"))
        ;; A user title wins over later agent titles.
        (bp/agent-sessions--set-title-override buf "Mine")
        (bp/agent-sessions--set-agent-title "Agent again")
        (should (equal (buffer-name buf) "project_3 - Mine"))
        (should (equal bp/agent-session-title "Agent again"))))))

(ert-deftest bp/agent-sessions-terminal-id-for-agent ()
  "A Codex thread id finds its own terminal, whatever id the daemon carries."
  (let ((bp/agent-sessions (make-hash-table :test 'equal))
        (live (generate-new-buffer "*live*"))
        (dead (generate-new-buffer "*dead*")))
    (unwind-protect
        (progn
          (puthash "1-73" (list :agent-session-id "thread-a" :buffer live)
                   bp/agent-sessions)
          (puthash "1-23" (list :agent-session-id "thread-b" :buffer dead)
                   bp/agent-sessions)
          (kill-buffer dead)
          (should (equal (bp/agent-sessions-terminal-id-for-agent "thread-a")
                         "1-73"))
          (should (equal (bp/agent-sessions-terminal-id-for-agent "thread-b") ""))
          (should (equal (bp/agent-sessions-terminal-id-for-agent "nope") "")))
      (kill-buffer live))))

(ert-deftest bp/agent-sessions-ghostel-pre-spawn-registers-plain-terminals ()
  "A plain ghostel terminal gets an id; its compile and eshell buffers don't."
  (let ((bp/agent-session-id->buffer (make-hash-table :test 'equal))
        (term (generate-new-buffer "*ghostel*"))
        (compile (generate-new-buffer "*ghostel-compile*")))
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-sessions--log-open) #'ignore)
                  ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                   #'ignore))
          (with-current-buffer term
            (setq-local ghostel-identity '((kind . term) (name . "x")))
            (let ((process-environment '("PATH=/usr/bin")))
              (bp/agent-sessions--ghostel-pre-spawn)
              (should bp/agent-session-id)
              (should (equal (getenv "EMACS_AGENT_SESSION_ID")
                             bp/agent-session-id))
              (should (eq (gethash bp/agent-session-id
                                   bp/agent-session-id->buffer)
                          term))))
          (with-current-buffer compile
            (setq-local ghostel-identity '((kind . compile)))
            (let ((process-environment '("PATH=/usr/bin")))
              (bp/agent-sessions--ghostel-pre-spawn)
              (should-not bp/agent-session-id)
              (should-not (getenv "EMACS_AGENT_SESSION_ID")))))
      (mapc #'kill-buffer (list term compile)))))

(ert-deftest bp/agent-sessions-ghostel-terminal-pid ()
  "A ghostel terminal's pid is its native shell's, not its event pipe's."
  (let ((buf (generate-new-buffer "*ghostel*")))
    (unwind-protect
        (let ((pipe (make-pipe-process :name "test-ghostel-pipe" :buffer buf
                                       :noquery t)))
          (with-current-buffer buf
            (setq major-mode 'ghostel-mode)
            (setq-local ghostel--pid 4242))
          (should (eq (bp/agent-sessions--buffer-terminal-type buf) 'ghostel))
          (should (eql (bp/agent-sessions--terminal-pid buf) 4242))
          (delete-process pipe)
          (should-not (bp/agent-sessions--terminal-pid buf)))
      (kill-buffer buf))))

(ert-deftest bp/agent-orchestration-ghostel-faint-is-ghost-text ()
  "ghostel's dimmed default color reads as Claude ghost text; other text doesn't.
The colors are what a live ghostel rendered for `ESC[2m' on this theme."
  (cl-letf (((symbol-function 'ghostel--face-hex-color)
             (lambda (_face attr)
               (if (eq attr :foreground) "#b2b2b2" "#292b2e"))))
    (let ((faint (propertize "fix the tests" 'face '(:foreground "#818283")))
          (faint-on-red (propertize "x" 'face '(:foreground "#c8807e"
                                                :background "#f2241f")))
          (green-faint (propertize "x" 'face '(:foreground "#518122")))
          (typed "fix "))
      (should (bp/agent-orchestration--claude-ghost-completion-p faint))
      (should (bp/agent-orchestration--claude-ghost-completion-p faint-on-red))
      (should-not (bp/agent-orchestration--claude-ghost-completion-p green-faint))
      (should-not (bp/agent-orchestration--claude-ghost-completion-p
                   (concat typed faint))))))

(ert-deftest bp/agent-orchestration-worker-title-renames-buffer ()
  (bp/agent-sessions-test--with-titled-terminal buf
    (bp/agent-orchestration--mark-worker buf "coord" (list) "Write tests")
    (should (equal (buffer-name buf) "project_3 - Write tests"))))

;;; Global mode and package defaults

(defmacro bp/agent-sessions-test-with-mode-state (&rest body)
  "Run BODY with the mode's global side effects isolated and undone."
  (declare (indent 0))
  `(let ((project-switch-commands (copy-tree project-switch-commands))
         (project-prefix-map (copy-keymap project-prefix-map))
         (bp/agent-sessions--heartbeat-timer nil)
         (bp/agent-sessions--passthrough-added nil)
         (after-load-functions after-load-functions)
         (buffer-list-update-hook buffer-list-update-hook)
         (kill-emacs-hook kill-emacs-hook))
     (cl-letf (((symbol-function 'server-start) #'ignore)
               ((symbol-function 'server-running-p) (lambda (&rest _) t)))
       (unwind-protect (progn ,@body)
         (bp/agent-sessions-mode -1)))))

(ert-deftest bp/agent-sessions-mode-installs-and-removes-its-wiring ()
  (bp/agent-sessions-test-with-mode-state
    (bp/agent-sessions-mode 1)
    (should (memq #'bp/agent-sessions--after-load after-load-functions))
    (should (memq #'bp/agent-session--clear-attention-on-focus
                  buffer-list-update-hook))
    (should (memq #'bp/agent-sessions--log-close-all kill-emacs-hook))
    (should (timerp bp/agent-sessions--heartbeat-timer))
    (should (eq (lookup-key project-prefix-map (kbd "a"))
                #'bp/agent-session-switch-or-new))
    (should (member bp/agent-sessions--project-switch-command
                    project-switch-commands))
    (bp/agent-sessions-mode -1)
    (should-not (memq #'bp/agent-sessions--after-load after-load-functions))
    (should-not (memq #'bp/agent-session--clear-attention-on-focus
                      buffer-list-update-hook))
    (should-not (memq #'bp/agent-sessions--log-close-all kill-emacs-hook))
    (should-not bp/agent-sessions--heartbeat-timer)
    (should-not (lookup-key project-prefix-map (kbd "a")))
    (should-not (member bp/agent-sessions--project-switch-command
                        project-switch-commands))))

(ert-deftest bp/agent-sessions-mode-leaves-a-function-switch-menu-alone ()
  (bp/agent-sessions-test-with-mode-state
    (setq project-switch-commands #'project-prefix-or-any-command)
    (bp/agent-sessions-mode 1)
    (should (eq project-switch-commands #'project-prefix-or-any-command))
    (bp/agent-sessions-mode -1)
    (should (eq project-switch-commands #'project-prefix-or-any-command))))

(ert-deftest bp/agent-sessions-eat-key-spells-meta-as-escape ()
  (should (equal (bp/agent-sessions--eat-key "M-m") [?\e ?m]))
  (should (equal (bp/agent-sessions--eat-key "C-c") [?\C-c]))
  (should (equal (bp/agent-sessions--eat-key "M-m a") [?\e ?m ?a])))

(defvar bp/agent-sessions-test--exceptions nil
  "Stand-in for a terminal package's passthrough list.")

(ert-deftest bp/agent-sessions-passthrough-adds-and-removes-only-its-keys ()
  (let ((bp/agent-sessions--passthrough-lists
         '((fake bp/agent-sessions-test--exceptions . identity)))
        (bp/agent-sessions--passthrough-added nil)
        (bp/agent-sessions-test--exceptions '("C-c" "M-x"))
        (bp/agent-sessions-terminal-passthrough-keys '("M-m" "M-x")))
    (bp/agent-sessions--apply-passthrough 'fake)
    (should (equal bp/agent-sessions-test--exceptions '("M-m" "C-c" "M-x")))
    ;; Applying again (a package reloaded) adds nothing twice.
    (bp/agent-sessions--apply-passthrough 'fake)
    (should (equal bp/agent-sessions-test--exceptions '("M-m" "C-c" "M-x")))
    ;; "M-x" was already there, so it is the user's and stays.
    (bp/agent-sessions--remove-passthrough)
    (should (equal bp/agent-sessions-test--exceptions '("C-c" "M-x")))))

(ert-deftest bp/agent-sessions-passthrough-reaches-eat ()
  (require 'eat)
  (let ((saved eat-semi-char-non-bound-keys)
        (bp/agent-sessions--passthrough-added nil)
        (bp/agent-sessions-terminal-passthrough-keys '("M-m")))
    (unwind-protect
        (progn
          (bp/agent-sessions--apply-passthrough 'eat)
          (should (member [?\e ?m] eat-semi-char-non-bound-keys))
          (bp/agent-sessions--remove-passthrough)
          (should (equal eat-semi-char-non-bound-keys saved)))
      (customize-set-variable 'eat-semi-char-non-bound-keys saved))))

(ert-deftest bp/agent-sessions-terminal-auto-picks-the-first-installed ()
  (let ((bp/agent-sessions-terminal 'auto))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (lib &rest _) (member lib '("vterm" "eat")))))
      (should (eq (bp/agent-sessions--terminal-backend) 'vterm)))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
      (should-error (bp/agent-sessions--terminal-backend) :type 'user-error)))
  (let ((bp/agent-sessions-terminal 'eat))
    (should (eq (bp/agent-sessions--terminal-backend) 'eat))))

(ert-deftest bp/agent-sessions-terminal-mode-binds-org-link-insertion ()
  (with-temp-buffer
    (bp/agent-sessions-terminal-mode 1)
    (should (eq (key-binding (kbd "C-c C-l"))
                #'bp/agent-session-insert-org-link))))

(provide 'agent-sessions-test)
;;; agent-sessions-test.el ends here
