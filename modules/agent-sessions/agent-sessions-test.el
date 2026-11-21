;;; agent-sessions-test.el --- Tests for agent-sessions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-sessions)

(defun agent-sessions-orchestration-test--message (type subject)
  (list :id (concat "test-" subject) :from "sender" :from-label "sender"
        :type type :subject subject :body "" :files "" :report "" :read nil))

(defmacro agent-sessions-test--with-temp-db (&rest body)
  "Run BODY with an isolated agent-sessions database."
  (declare (indent 0) (debug t))
  `(let* ((db-file (make-temp-file "agent-sessions-test-"))
          (snapshot-directory
           (make-temp-file "agent-sessions-snapshots-test-" t))
          (agent-sessions-db-file db-file)
          (agent-sessions-file-snapshot-directory snapshot-directory)
          (agent-sessions--db nil)
          (agent-sessions--shared-files nil)
          (agent-sessions--shared-paths nil)
          (agent-sessions--shared-links nil))
     (unwind-protect
         (progn ,@body)
       (when (sqlitep agent-sessions--db)
         (sqlite-close agent-sessions--db))
       (dolist (path (list db-file (concat db-file "-wal")
                           (concat db-file "-shm")))
         (when (file-exists-p path)
           (delete-file path)))
       (delete-directory snapshot-directory t))))

(ert-deftest agent-sessions-hook-routes-stale-daemon-id-to-unresolved-terminal ()
  (agent-sessions-test--with-temp-db
   (let* ((directory (make-temp-file "agent-hook-worktree-" t))
          (target (generate-new-buffer " *agent-hook-target*"))
          (other (generate-new-buffer " *agent-hook-other*"))
          (payload-file (make-temp-file "agent-hook-payload-"))
          (agent-sessions-id->buffer (make-hash-table :test 'equal))
          (agent-sessions--registry (make-hash-table :test 'equal))
          (agent-sessions-notify-on-attention nil)
          logged)
     (unwind-protect
         (progn
           (with-current-buffer target
             (setq-local agent-sessions-id "actual-terminal")
             (setq default-directory (file-name-as-directory directory)))
           (with-current-buffer other
             (setq-local agent-sessions-id "other-terminal")
             (setq default-directory (file-name-as-directory directory)))
           (puthash "actual-terminal" target agent-sessions-id->buffer)
           (puthash "other-terminal" other agent-sessions-id->buffer)
           ;; A terminal in the same worktree already owned by another thread
           ;; must not make routing ambiguous.
           (puthash "other-terminal"
                    (list :buffer other :agent-type "codex"
                          :agent-session-id "other-thread")
                    agent-sessions--registry)
           (with-temp-file payload-file
             (insert
              (json-serialize
               (let ((payload (make-hash-table :test 'equal)))
                 (puthash "hook_event_name" "Stop" payload)
                 (puthash "session_id" "actual-thread" payload)
                 (puthash "cwd" directory payload)
                 payload))))
           (cl-letf (((symbol-function 'agent-sessions--foreground-command)
                      (lambda (buf)
			(and (eq buf target) "codex")))
                     ((symbol-function 'agent-sessions--repo-info)
                      (lambda (_)
			(list :repo "repo" :repo-root directory
                              :worktree "worktree"
                              :worktree-path directory)))
                     ((symbol-function 'agent-sessions--log-update)
                      (lambda (buf type sid)
			(setq logged (list buf type sid))))
                     ((symbol-function 'agent-sessions-orchestration--hydrate) #'ignore)
                     ((symbol-function 'agent-sessions-orchestration-deliver) #'ignore)
                     ((symbol-function 'agent-sessions--refresh-if-visible)
                      #'ignore))
             ;; The inherited daemon id no longer names any live terminal.
             (agent-sessions-hook-notify "stale-terminal" "codex" payload-file))
           (let ((session (gethash "actual-terminal" agent-sessions--registry)))
             (should (equal "actual-thread"
                            (plist-get session :agent-session-id)))
             (should (eq target (plist-get session :buffer)))
             (should (eq 'needs-attention (plist-get session :status))))
           (should-not (gethash "stale-terminal" agent-sessions--registry))
           (should (equal (list target "codex" "actual-thread") logged)))
       (when (file-exists-p payload-file)
         (delete-file payload-file))
       (kill-buffer target)
       (kill-buffer other)
       (delete-directory directory t)))))

(ert-deftest agent-sessions-hook-follows-claude-clear-to-new-conversation ()
  (agent-sessions-test--with-temp-db
   (let* ((directory (make-temp-file "agent-hook-clear-" t))
          (term (generate-new-buffer " *agent-hook-clear*"))
          (agent-sessions-id->buffer (make-hash-table :test 'equal))
          (agent-sessions--registry (make-hash-table :test 'equal))
          (agent-sessions-notify-on-attention nil))
     (cl-flet ((deliver (event sid)
                 (let ((file (make-temp-file "agent-hook-payload-")))
                   (with-temp-file file
                     (insert (json-serialize
                              (let ((p (make-hash-table :test 'equal)))
                                (puthash "hook_event_name" event p)
                                (puthash "session_id" sid p)
                                (puthash "cwd" directory p)
                                p))))
                   (agent-sessions-hook-notify "1-1" "claude" file)))
               (rows ()
                 (sqlite-select (agent-sessions--db)
                                "SELECT agent_sid, close_reason FROM sessions
                                  ORDER BY id")))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-sessions--repo-info)
                      (lambda (_) (list :repo "repo" :repo-root directory
                                        :worktree "wt"
                                        :worktree-path directory)))
                     ((symbol-function 'agent-sessions--detect-parent)
                      #'ignore)
                     ((symbol-function 'agent-sessions-orchestration--hydrate) #'ignore)
                     ((symbol-function 'agent-sessions-orchestration-deliver) #'ignore)
                     ((symbol-function 'agent-sessions--refresh-if-visible)
                      #'ignore))
             (with-current-buffer term
               (setq-local agent-sessions-id "1-1")
               (setq default-directory (file-name-as-directory directory)))
             (puthash "1-1" term agent-sessions-id->buffer)
             (agent-sessions--log-open term)
             (deliver "SessionStart" "old")
             ;; `/clear', with the old conversation's end delivered last.
             (deliver "SessionStart" "new")
             (deliver "SessionEnd" "old")
             (should (equal "new" (plist-get (gethash "1-1" agent-sessions--registry)
                                             :agent-session-id)))
             (should (equal '(("old" "superseded") ("new" nil)) (rows))))
         (kill-buffer term)
         (delete-directory directory t))))))

(ert-deftest agent-sessions-share-file-prefers-durable-agent-id ()
  (let* ((source (make-temp-file "agent-share-routing-"))
         (target-buffer (generate-new-buffer " *agent-share-target*"))
         (wrong-buffer (generate-new-buffer " *agent-share-wrong*"))
         (target-session (list :buffer target-buffer
                               :agent-session-id "target-thread"))
         (wrong-session (list :buffer wrong-buffer
                              :agent-session-id "wrong-thread"))
         (agent-sessions--registry (make-hash-table :test 'equal))
         shared-with)
    (unwind-protect
        (progn
          ;; The daemon's inherited terminal id is live, but belongs to a
          ;; different session.  The durable agent id must win.
          (puthash "actual-terminal" target-session agent-sessions--registry)
          (puthash "stale-terminal" wrong-session agent-sessions--registry)
          (cl-letf (((symbol-function 'agent-sessions--share-file)
                     (lambda (session _path _label)
                       (setq shared-with session)))
                    ((symbol-function
                      'agent-sessions--revert-shared-file-buffer)
                     #'ignore)
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should
             (string-prefix-p
              "Shared "
              (agent-sessions-share-file
               "stale-terminal" source "handoff" "target-thread")))
            (should (eq shared-with target-session))
            (setq shared-with nil)
            (should
             (equal
              "No Emacs agent session registered for agent id missing-thread"
              (agent-sessions-share-file
               "stale-terminal" source "handoff" "missing-thread")))
            (should-not shared-with)))
      (delete-file source)
      (kill-buffer target-buffer)
      (kill-buffer wrong-buffer))))

(ert-deftest agent-sessions-emacs-share-help-lists-every-state ()
  "`emacs-share --help' names each accepted state, and only those.
The help writes the states out with their meanings, so this keeps it in step
with `agent-sessions-states'.  `state --help' is help, not a state."
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
          (should (equal (nreverse listed) agent-sessions-states)))))
    (with-temp-buffer
      (should (= 2 (call-process script nil t nil "state")))
      (should (string-match-p "--help" (buffer-string))))))

(ert-deftest agent-sessions-emacs-share-passes-codex-thread-id ()
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

(ert-deftest agent-sessions-session-row-omits-raw-hook-event ()
  (with-temp-buffer
    (agent-sessions-dashboard-mode)
    (let ((inhibit-read-only t)
          (agent-sessions--row-status nil)
          (entry (list :id "terminal"
                       :title "Investigate the issue"
                       :session (list :agent-type "codex"
                                      :status 'running
                                      :last-event "PreToolUse"))))
      (cl-letf (((symbol-function 'agent-sessions--session-state)
                 (lambda (_) nil))
                ((symbol-function 'agent-sessions-orchestration--row-badge)
                 (lambda (_) ""))
                ((symbol-function 'agent-sessions--insert-note) #'ignore)
                ((symbol-function 'agent-sessions--insert-links) #'ignore)
                ((symbol-function 'agent-sessions--insert-files) #'ignore))
        (agent-sessions--insert-session entry)
        (should (string-match-p "running" (buffer-string)))
        (should (string-match-p "Investigate the issue" (buffer-string)))
        (should-not (string-match-p "PreToolUse" (buffer-string)))))))

(ert-deftest agent-sessions-orchestration-status-alone-does-not-steer-running-codex ()
  (let* ((id "terminal")
         (message (agent-sessions-orchestration-test--message "status" "progress"))
         (session (list :status 'running :agent-type "codex"
                        :agent-session-id "thread"))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (puthash id (list message) agent-sessions-orchestration--mail)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions-orchestration--codex-queue)
               (lambda (_ messages) (setq queued messages) t))
              ((symbol-function 'agent-sessions-orchestration--consume)
               (lambda (messages) (setq consumed messages))))
      (should-not (agent-sessions-orchestration-deliver id))
      (should-not queued)
      (should-not consumed)
      (should-not (plist-get message :read)))))

(ert-deftest agent-sessions-orchestration-actionable-mail-steers-with-earlier-status ()
  (let* ((id "terminal")
         (status (agent-sessions-orchestration-test--message "status" "progress"))
         (question (agent-sessions-orchestration-test--message "question" "decision"))
         (pending (list status question))
         (session (list :status 'running :agent-type 'codex
                        :agent-session-id "thread"))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed refreshed)
    (puthash id pending agent-sessions-orchestration--mail)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions-orchestration--codex-queue)
               (lambda (_ messages) (setq queued messages) t))
              ((symbol-function 'agent-sessions-orchestration--consume)
               (lambda (messages) (setq consumed messages)))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               (lambda () (setq refreshed t))))
      (should (eq 'queued (agent-sessions-orchestration-deliver id)))
      (should (equal pending queued))
      (should (equal pending consumed))
      (should refreshed))))

(ert-deftest agent-sessions-orchestration-failed-native-queue-leaves-mail-unread ()
  (let* ((id "terminal")
         (question (agent-sessions-orchestration-test--message "question" "decision"))
         (session (list :status 'running :agent-type "codex"
                        :agent-session-id "thread"))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         consumed)
    (puthash id (list question) agent-sessions-orchestration--mail)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions-orchestration--codex-queue)
               (lambda (&rest _) nil))
              ((symbol-function 'agent-sessions-orchestration--consume)
               (lambda (messages) (setq consumed messages))))
      (should-not (agent-sessions-orchestration-deliver id))
      (should-not consumed)
      (should-not (plist-get question :read)))))

(ert-deftest agent-sessions-orchestration-generic-idle-delivery-still-pastes-all-mail ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *agent-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "status" "progress"))
         (session (list :status 'stopped :agent-type "other" :buffer buffer))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         pasted consumed)
    (unwind-protect
        (progn
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions-orchestration--receptive-p)
                     (lambda (_) t))
                    ((symbol-function 'agent-sessions-orchestration--paste)
                     (lambda (_ text) (setq pasted text)))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'delivered (agent-sessions-orchestration-deliver id)))
            (should (equal (list message) consumed))
            (should (string-match-p "while you were idle" pasted))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-status-mail-queues-at-empty-running-claude ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "status" "progress"))
         (session (list :status 'running :agent-type "claude" :buffer buffer))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0\n────────────────\n  Opus"))
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'agent-sessions-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (agent-sessions-orchestration-deliver id)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-actionable-mail-queues-after-retained-claude-prompt ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "answer" "decision"))
         (session (list :status 'running :agent-type 'claude :buffer buffer
                        :submitted-prompt "previous request"))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0previous request\n"
                    "────────────────\n  Opus"))
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'agent-sessions-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (agent-sessions-orchestration-deliver id)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-claude-draft-withholds-actionable-mail ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-draft-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "answer" "decision"))
         (session (list :status 'stopped :agent-type 'claude :buffer buffer))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0my unfinished message\n"
                    "────────────────\n  Opus"))
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'agent-sessions-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages))))
            (should-not (agent-sessions-orchestration-deliver id))
            (should-not queued)
            (should-not consumed)
            (should-not (plist-get message :read))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-claude-submit-releases-withheld-mail ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *claude-submit-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "status" "progress"))
         (session (list :status 'running :agent-type 'claude :buffer buffer))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed)
    (unwind-protect
        (progn
          ;; Keep the old draft visible to prove the hook, rather than a
          ;; screen redraw race, is the authority that it was submitted.
          (with-current-buffer buffer
            (insert "────────────────\n❯\u00a0my submitted message\n"
                    "────────────────\n  Opus"))
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions--foreground-command)
                     (lambda (_) "claude"))
                    ((symbol-function 'agent-sessions-orchestration--claude-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (agent-sessions-orchestration-deliver id t)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-claude-composer-detection-is-conservative ()
  (let* ((buffer (generate-new-buffer " *claude-composer-test*"))
         (session (list :agent-type "claude" :buffer buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (insert "────────────────\n❯\u00a0\n────────────────\n  Opus")
          (should (agent-sessions-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0first line\n  wrapped line\n"
                  "────────────────\n  Opus")
          (should-not
           (agent-sessions-orchestration--claude-receptive-composer-p session))
          ;; Claude can retain the submitted prompt in the composer after the
          ;; hook fires.  Visual hard wrapping must not turn that into a draft.
          (setq session
                (plist-put session :submitted-prompt "first line wrapped line"))
          (should (agent-sessions-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0a genuinely new draft\n"
                  "────────────────\n  Opus")
          (should-not
           (agent-sessions-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0")
          (let ((start (point)))
            (insert "suggested completion")
            (add-text-properties
             start (point)
             '(font-lock-face (:inherit (eat-term-faint eat-term-font-0)))))
          (insert "\n────────────────\n  Opus")
          (should (agent-sessions-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "────────────────\n❯\u00a0typed prefix ")
          (let ((start (point)))
            (insert "suggested remainder")
            (add-text-properties
             start (point)
             '(font-lock-face (:inherit (eat-term-faint eat-term-font-0)))))
          (insert "\n────────────────\n  Opus")
          (should-not
           (agent-sessions-orchestration--claude-receptive-composer-p session))
          (erase-buffer)
          (insert "Claude output without a recognizable composer")
          (should-not
           (agent-sessions-orchestration--claude-receptive-composer-p session)))
      (kill-buffer buffer))))

(defun agent-sessions-orchestration-test--claude-screen (composer &optional below)
  "A Claude screen whose composer box holds COMPOSER, with BELOW after it."
  (concat "● earlier output\n\n"
          "────────────────\n❯ " composer "\n────────────────\n"
          (or below "  Opus 5.5 · think:medium\n  ⏵⏵ auto mode on")))

(defmacro agent-sessions-orchestration-test--with-hidden-ghostel (spec &rest body)
  "Run BODY with SPEC = (SESSION RENDERED LIVE) bound to a hidden ghostel session.
RENDERED is the stale buffer text; LIVE is what the terminal holds now
\(the form is evaluated on each read, so BODY may `setq' it)."
  (declare (indent 1))
  (let ((session (nth 0 spec)) (rendered (nth 1 spec)) (live (nth 2 spec))
        (buffer (make-symbol "buffer")))
    `(let* ((,buffer (generate-new-buffer " *claude-hidden-ghostel*"))
            (,session (list :agent-type "claude" :buffer ,buffer)))
       (unwind-protect
           (cl-letf (((symbol-function 'agent-sessions--buffer-terminal-type)
                      (lambda (_) 'ghostel))
                     ((symbol-function 'ghostel--copy-all-text)
                      (lambda (_) ,live)))
             (with-current-buffer ,buffer
               (setq-local ghostel--term 'fake-term)
               (setq-local ghostel--pending-redraw t)
               (insert ,rendered))
             ,@body)
         (kill-buffer ,buffer)))))

(ert-deftest agent-sessions-orchestration-ghostel-hidden-buffer-is-stale ()
  "Only a ghostel buffer with a pending redraw lags its terminal."
  (agent-sessions-orchestration-test--with-hidden-ghostel (session "" "")
    (should (agent-sessions-orchestration--ghostel-stale-p (plist-get session :buffer)))
    (with-current-buffer (plist-get session :buffer)
      (setq-local ghostel--pending-redraw nil))
    (should-not
     (agent-sessions-orchestration--ghostel-stale-p (plist-get session :buffer)))))

(ert-deftest agent-sessions-orchestration-claude-hidden-ghostel-reads-the-terminal ()
  "A hidden ghostel buffer is judged by what its terminal holds now.
ghostel does not draw hidden terminals, so the buffer can be empty or show an
old screen.  Mail used to wait there until the human submitted a prompt."
  (let (live)
    (agent-sessions-orchestration-test--with-hidden-ghostel (session "" live)
      ;; Never shown since it started: the buffer is empty.
      (setq live (agent-sessions-orchestration-test--claude-screen ""))
      (should (agent-sessions-orchestration--claude-receptive-composer-p session))
      ;; A suggestion appeared while hidden: nobody could have typed it.
      (setq live (agent-sessions-orchestration-test--claude-screen
                  "what's the status of all the agents?"))
      (should (agent-sessions-orchestration--claude-receptive-composer-p session))
      ;; No composer recognized, or no text at all: wait.
      (setq live "Claude output without a recognizable composer")
      (should-not (agent-sessions-orchestration--claude-receptive-composer-p session))
      (setq live nil)
      (should-not (agent-sessions-orchestration--claude-receptive-composer-p session)))))

(ert-deftest agent-sessions-orchestration-claude-hidden-ghostel-keeps-drafts ()
  "A draft typed before the terminal was hidden is still in the buffer; keep it."
  (let ((live (agent-sessions-orchestration-test--claude-screen
               "a long human draft that the terminal wraps differently")))
    (agent-sessions-orchestration-test--with-hidden-ghostel
        (session (agent-sessions-orchestration-test--claude-screen
                  "a long human draft that\n  the terminal wraps differently")
                 live)
      (should-not (agent-sessions-orchestration--claude-receptive-composer-p session))
      ;; Once submitted it is no longer a draft.
      (setq session (plist-put session :submitted-prompt
                               "a long human draft that the terminal wraps differently"))
      (should (agent-sessions-orchestration--claude-receptive-composer-p session)))))

(ert-deftest agent-sessions-orchestration-claude-hidden-ghostel-faint-suggestion ()
  "A suggestion shown faint when last seen is still a suggestion while hidden."
  (agent-sessions-orchestration-test--with-hidden-ghostel
      (session "" (agent-sessions-orchestration-test--claude-screen "open the PR"))
    (with-current-buffer (plist-get session :buffer)
      (insert "────────────────\n❯ ")
      (let ((start (point)))
        (insert "open the PR")
        (add-text-properties
         start (point) '(font-lock-face (:inherit (eat-term-faint)))))
      (insert "\n────────────────\n  Opus"))
    (should (agent-sessions-orchestration--claude-receptive-composer-p session))))

(ert-deftest agent-sessions-orchestration-claude-hidden-ghostel-ignores-dialogs ()
  "A dialog below the transcript's last prompt is not a composer."
  (agent-sessions-orchestration-test--with-hidden-ghostel
      (session ""
               (concat "❯ fix the bug\n\n● Running a command\n"
                       "────────────────\n Bash command\n   ls\n"
                       " Do you want to proceed?\n ❯ 1. Yes\n   2. No\n"))
    (should-not (agent-sessions-orchestration--claude-receptive-composer-p session))))

(ert-deftest agent-sessions-orchestration-claude-queue-only-pastes-and-submits ()
  (let* ((buffer (generate-new-buffer " *claude-queue-test*"))
         (session (list :agent-type "claude" :buffer buffer))
         (message (agent-sessions-orchestration-test--message "answer" "result"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-sessions-orchestration--terminal-input)
                   (lambda (_ input) (push (list 'input input) events)))
                  ((symbol-function 'agent-sessions--terminal-paste)
                   (lambda (_ text) (push (list 'paste text) events)))
                  ((symbol-function 'run-at-time)
                   (lambda (_ _ function &rest args) (apply function args))))
          (should (agent-sessions-orchestration--claude-queue session (list message)))
          (setq events (nreverse events))
          (should (equal (mapcar #'car events) '(paste input)))
          (should (equal (cadr (nth 1 events))
                         agent-sessions-orchestration-claude-queue-submit-key))
          (should (string-match-p "Subject: result" (cadr (nth 0 events)))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-idle-codex-uses-native-queue-not-composer ()
  (let* ((id "terminal")
         (message (agent-sessions-orchestration-test--message "status" "progress"))
         (session (list :status 'stopped :agent-type "codex"
                        :agent-session-id "thread"))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed pasted)
    (puthash id (list message) agent-sessions-orchestration--mail)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions-orchestration--codex-queue)
               (lambda (_ messages) (setq queued messages) t))
              ((symbol-function 'agent-sessions-orchestration--consume)
               (lambda (messages) (setq consumed messages)))
              ((symbol-function 'agent-sessions-orchestration--paste)
               (lambda (&rest _) (setq pasted t)))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               #'ignore))
      (should (eq 'queued (agent-sessions-orchestration-deliver id)))
      (should (equal (list message) queued))
      (should (equal (list message) consumed))
      (should-not pasted))))

(ert-deftest agent-sessions-orchestration-interrupted-codex-empty-prompt-delivers-directly ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *codex-interrupted-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "answer" "decision"))
         ;; Codex does not fire Stop for an interrupted turn, so this stale
         ;; status is the live failure mode this regression covers.
         (session (list :status 'running :agent-type "codex"
                        :agent-session-id "thread" :buffer buffer))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed pasted)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "■ Conversation interrupted - tell the model what to do differently.\n"
                    "\n› Ask Codex to do anything\n\n"
                    "  gpt-5.6-sol medium · Context 31% used"))
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions-orchestration--codex-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'agent-sessions-orchestration--paste)
                     (lambda (_ text) (setq pasted text)))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'delivered (agent-sessions-orchestration-deliver id)))
            (should (equal (list message) consumed))
            (should (string-match-p "Subject: decision" pasted))
            (should-not queued)))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-codex-draft-still-uses-native-queue ()
  (let* ((id "terminal")
         (buffer (generate-new-buffer " *codex-draft-delivery-test*"))
         (message (agent-sessions-orchestration-test--message "answer" "decision"))
         (session (list :status 'stopped :agent-type "codex"
                        :agent-session-id "thread" :buffer buffer))
         (agent-sessions-orchestration--mail (make-hash-table :test 'equal))
         queued consumed pasted)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "› Keep this human draft intact\n\n"
                    "  gpt-5.6-sol medium · Context 31% used"))
          (puthash id (list message) agent-sessions-orchestration--mail)
          (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                     (lambda (_) session))
                    ((symbol-function 'agent-sessions-orchestration--codex-queue)
                     (lambda (_ messages) (setq queued messages) t))
                    ((symbol-function 'agent-sessions-orchestration--paste)
                     (lambda (&rest _) (setq pasted t)))
                    ((symbol-function 'agent-sessions-orchestration--consume)
                     (lambda (messages) (setq consumed messages)))
                    ((symbol-function 'agent-sessions--refresh-if-visible)
                     #'ignore))
            (should (eq 'queued (agent-sessions-orchestration-deliver id)))
            (should (equal (list message) queued))
            (should (equal (list message) consumed))
            (should-not pasted)))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-orchestration-native-codex-queue-uses-durable-thread-id ()
  (let ((session (list :agent-session-id "thread-uuid"))
        (message (agent-sessions-orchestration-test--message "answer" "result"))
        invocation)
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/codex"))
              ((symbol-function 'agent-sessions-orchestration--call-with-timeout)
               (lambda (program args timeout)
                 (setq invocation (list program args timeout))
                 0)))
      (should (agent-sessions-orchestration--codex-queue session (list message)))
      (should (equal (car invocation) "/bin/codex"))
      (should (equal (seq-take (cadr invocation) 4)
                     '("queue" "--thread" "thread-uuid" "--message")))
      (should (string-match-p "Subject: result" (nth 4 (cadr invocation))))
      (should (= (nth 2 invocation)
                 agent-sessions-orchestration--native-queue-timeout)))))

(ert-deftest agent-sessions-show-file-diff-opens-latest-commit-then-reads ()
  (let ((path "/repo/docs/plan.md")
        events)
    (cl-letf (((symbol-function 'agent-sessions--file-at-point)
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
              ((symbol-function 'agent-sessions--set-visited)
               (lambda (match-fn visitedp)
                 (push (list 'visited
                             (funcall match-fn (list :path path))
                             visitedp)
                       events)
                 t))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               (lambda () (push '(refresh) events))))
      (agent-sessions-show-file-diff)
      (should
       (equal (nreverse events)
              '((git "/repo/"
                     ("log" "-1" "--format=%H" "--follow" "--"
                      "docs/plan.md"))
                (show "/repo/" "abc123" ("--stat") ("docs/plan.md"))
                (visited t t)
                (refresh)))))))

(ert-deftest agent-sessions-show-file-diff-keeps-uncommitted-file-unread ()
  (let ((path "/repo/docs/plan.md")
        shown visited)
    (cl-letf (((symbol-function 'agent-sessions--file-at-point)
               (lambda () (list 'session path (list :path path))))
              ((symbol-function 'magit-toplevel)
               (lambda (_) "/repo/"))
              ((symbol-function 'magit-git-string)
               (lambda (&rest _) nil))
              ((symbol-function 'magit-show-commit)
               (lambda (&rest _) (setq shown t)))
              ((symbol-function 'agent-sessions--set-visited)
               (lambda (&rest _) (setq visited t))))
      (should-error (agent-sessions-show-file-diff) :type 'user-error)
      (should-not shown)
      (should-not visited))))

(defun agent-sessions-test--write-file (path contents)
  "Replace PATH with CONTENTS for a shared-file test."
  (with-temp-file path
    (insert contents)))

(ert-deftest agent-sessions-share-file-keeps-latest-two-submissions ()
  (agent-sessions-test--with-temp-db
    (let* ((source (make-temp-file "agent-shared-file-" nil ".md"))
           (session (list :agent-session-id "thread-1"))
           (key '("session" "thread-1")))
      (unwind-protect
          (progn
            (agent-sessions-test--write-file source "first\n")
            (let ((first (agent-sessions--share-file session source nil)))
              (should (equal source (plist-get first :path))))
            (let ((snapshot (agent-sessions--file-snapshot-row
                             (list key) source)))
              (should-not (plist-get snapshot :previous-path))
              (should (equal "first\n"
                             (with-temp-buffer
                               (insert-file-contents
                                (plist-get snapshot :current-path))
                               (buffer-string)))))
            (agent-sessions-test--write-file source "second\n")
            (agent-sessions--share-file session source nil)
            (agent-sessions-test--write-file source "third\n")
            (agent-sessions--share-file session source nil)
            (let ((snapshot (agent-sessions--file-snapshot-row
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
                     (agent-sessions--db)
                     "SELECT COUNT(*) FROM shared_file_snapshots"))))
        (delete-file source)))))

(ert-deftest agent-sessions-file-snapshots-follow-durable-session-id ()
  (agent-sessions-test--with-temp-db
    (let ((source (make-temp-file "agent-shared-file-"))
          (buffer (generate-new-buffer "agent-snapshot-terminal")))
      (unwind-protect
          (let ((terminal-session (list :buffer buffer)))
            (agent-sessions-test--write-file source "before id\n")
            (agent-sessions--share-file terminal-session source nil)
            (agent-sessions-test--write-file source "after id\n")
            (let* ((durable-session
                    (list :buffer buffer :agent-session-id "thread-2"))
                   (durable-key '("session" "thread-2")))
              (agent-sessions--share-file durable-session source nil)
              (let ((snapshot (agent-sessions--file-snapshot-row
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
                       (agent-sessions--db)
                       "SELECT COUNT(*) FROM shared_file_snapshots
                         WHERE kind = 'session' AND key = ?"
                       (list (buffer-name buffer)))))))
        (kill-buffer buffer)
        (delete-file source)))))

(ert-deftest agent-sessions-show-submission-diff-compares-cache-then-reads ()
  (let* ((path "/outside-git/plan.md")
         (previous (make-temp-file "previous--plan-"))
         (current (make-temp-file "current--plan-"))
         (session (list :agent-session-id "thread-1"))
         (buffer (get-buffer-create "*Agent submission diff: plan.md*"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-sessions--file-at-point)
                   (lambda () (list session path (list :path path))))
                  ((symbol-function 'agent-sessions--file-snapshot-row)
                   (lambda (keys candidate)
                     (push (list 'snapshot keys candidate) events)
                     (list :previous-path previous :current-path current)))
                  ((symbol-function 'diff-no-select)
                   (lambda (old new switches no-async destination)
                     (push (list 'diff old new switches no-async destination)
                           events)
                     buffer))
                  ((symbol-function 'agent-sessions--set-visited)
                   (lambda (match-fn visitedp)
                     (push (list 'visited
                                 (funcall match-fn (list :path path))
                                 visitedp)
                           events)
                     t))
                  ((symbol-function 'agent-sessions--refresh-if-visible)
                   (lambda () (push '(refresh) events)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (shown) (push (list 'pop shown) events))))
          (agent-sessions-show-file-submission-diff)
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

(ert-deftest agent-sessions-show-submission-diff-needs-two-versions ()
  (let ((path "/outside-git/plan.md")
        visited)
    (cl-letf (((symbol-function 'agent-sessions--file-at-point)
               (lambda ()
                 (list (list :agent-session-id "thread-1") path
                       (list :path path))))
              ((symbol-function 'agent-sessions--file-snapshot-row)
               (lambda (&rest _)
                 (list :current-path "/cache/current--plan.md")))
              ((symbol-function 'agent-sessions--set-visited)
               (lambda (&rest _) (setq visited t))))
      (should-error (agent-sessions-show-file-submission-diff)
                    :type 'user-error)
      (should-not visited))))

(ert-deftest agent-sessions-capital-d-initializes-missing-snapshot ()
  (agent-sessions-test--with-temp-db
    (let* ((path (make-temp-file "agent-shared-file-"))
           (session (list :agent-session-id "thread-1"))
           (key '("session" "thread-1"))
           visited message-text)
      (unwind-protect
          (progn
            (agent-sessions-test--write-file path "current contents\n")
            (cl-letf (((symbol-function 'agent-sessions--file-at-point)
                       (lambda () (list session path (list :path path))))
                      ((symbol-function 'agent-sessions--set-visited)
                       (lambda (&rest _) (setq visited t)))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (setq message-text
                               (apply #'format format-string args)))))
              (agent-sessions-show-file-submission-diff))
            (let ((snapshot (agent-sessions--file-snapshot-row
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

(ert-deftest agent-sessions-capital-d-is-contextual ()
  (let (action file-at-point)
    (cl-letf (((symbol-function 'agent-sessions--file-at-point)
               (lambda () file-at-point))
              ((symbol-function 'agent-sessions-show-file-submission-diff)
               (lambda () (setq action 'diff)))
              ((symbol-function 'agent-sessions-detach-parent)
               (lambda () (setq action 'detach))))
      (setq file-at-point '(session path file))
      (agent-sessions-submission-diff-or-detach-parent)
      (should (eq action 'diff))
      (setq action nil file-at-point nil)
      (agent-sessions-submission-diff-or-detach-parent)
      (should (eq action 'detach)))))

(ert-deftest agent-sessions-shared-links-allow-multiple-per-kind ()
  (agent-sessions-test--with-temp-db
    (let* ((session (list :agent-session-id "thread-1"))
           (first (agent-sessions--share-link
                   session "linear" "https://linear.app/issue/ABC-1" "ABC-1"))
           (shared-at (plist-get first :shared-at)))
      (should-not (plist-get first :visited-at))
      (should (agent-sessions--set-link-visited
               (lambda (link) (equal (plist-get link :url)
                                     "https://linear.app/issue/ABC-1")) t))
      (let ((same (agent-sessions--share-link
                   session "linear" "https://linear.app/issue/ABC-1" "ABC-1")))
        (should (plist-get same :visited-at))
        (should (= shared-at (plist-get same :shared-at))))
      (let ((second (agent-sessions--share-link
                     session "linear" "https://linear.app/issue/ABC-2" "ABC-2")))
        (should-not (plist-get second :visited-at)))
      (agent-sessions--share-link
       session "pr" "https://github.com/org/repo/pull/1" "PR 1")
      (agent-sessions--share-link
       session "pr" "https://github.com/org/repo/pull/2" "PR 2")
      (should (= 4 (length (agent-sessions--session-links session))))
      (with-temp-buffer
        (agent-sessions-dashboard-mode)
        (let ((inhibit-read-only t)
              (agent-sessions--row-status nil))
          (agent-sessions--insert-links session "  ")
          (dolist (label '("ABC-1" "ABC-2" "PR 1" "PR 2"))
            (should (string-match-p (regexp-quote label) (buffer-string))))))
      (should (equal '((2))
                     (sqlite-select
                      (agent-sessions--db)
                      "SELECT COUNT(*) FROM shared_links
                        WHERE owner_kind = 'session'
                          AND owner_key = 'thread-1'
                          AND link_kind = 'linear'")))
      (should (agent-sessions--detach-link
               session "https://linear.app/issue/ABC-2"))
      (should (= 3 (length (agent-sessions--session-links session))))
      (should (plist-get (car (agent-sessions--session-links session))
                         :visited-at)))))

(ert-deftest agent-sessions-shared-links-migrate-old-key ()
  (agent-sessions-test--with-temp-db
    (let ((db (agent-sessions--db)))
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
      (agent-sessions--db-migrate db)
      (agent-sessions--share-link
       (list :agent-session-id "thread-1") "linear"
       "https://linear.app/issue/ABC-2" "ABC-2")
      (should (equal '((2))
                     (sqlite-select db "SELECT COUNT(*) FROM shared_links")))
      (should (equal '((101))
                     (sqlite-select db "SELECT visited_at FROM shared_links
                       WHERE url = 'https://linear.app/issue/ABC-1'"))))))

(ert-deftest agent-sessions-reap-spares-live-emacs ()
  (agent-sessions-test--with-temp-db
    (let ((db (agent-sessions--db))
          (live (format "%d-1" (emacs-pid)))
          ;; Above any pid the kernel hands out, so never a running process.
          (dead "999999999-1"))
      (dolist (sid (list live dead))
        (sqlite-execute db "INSERT INTO sessions (emacs_sid, worktree, opened_at)
                            VALUES (?, '/tmp', 100)"
                        (list sid)))
      (agent-sessions--meta-set db "last_seen" 200)
      ;; What a second Emacs opening the file does: the live one's row must
      ;; stay open, and only the dead one's is closed as a crash.
      (agent-sessions--db-reap db)
      (should (equal '((nil nil))
                     (sqlite-select db "SELECT closed_at, close_reason
                                          FROM sessions WHERE emacs_sid = ?"
                                    (list live))))
      (should (equal '((200 "crash"))
                     (sqlite-select db "SELECT closed_at, close_reason
                                          FROM sessions WHERE emacs_sid = ?"
                                    (list dead)))))))

(ert-deftest agent-sessions-exit-recloses-rows-wrongly-reaped ()
  (agent-sessions-test--with-temp-db
    (let* ((db (agent-sessions--db))
           (term (generate-new-buffer " *agent-log-term*"))
           (agent-sessions-id->buffer (make-hash-table :test 'equal))
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
              (setq-local agent-sessions--log-row held))
            (puthash "1-1" term agent-sessions-id->buffer)
            (agent-sessions--log-close-all)
            (should (equal "emacs-exit"
                           (caar (sqlite-select db "SELECT close_reason
                                                     FROM sessions WHERE id = ?"
                                                (list held)))))
            (should (equal '((150 "crash"))
                           (sqlite-select db "SELECT closed_at, close_reason
                                               FROM sessions WHERE id = ?"
                                          (list other)))))
        (kill-buffer term)))))

(ert-deftest agent-sessions-start-logs-resumed-identity ()
  (agent-sessions-test--with-temp-db
    (let ((agent-sessions-id->buffer (make-hash-table :test 'equal))
          term)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-sessions--repo-info)
                     (lambda (_) (list :worktree "wt" :worktree-path "/tmp")))
                    ((symbol-function 'agent-sessions--spawn)
                     (lambda (&rest _)
                       (setq term (generate-new-buffer " *agent-log-resume*"))
                       (with-current-buffer term
                         (setq-local agent-sessions-id "1-1"))
                       (agent-sessions--log-open term)
                       term)))
            ;; No hook fires: an idle resumed Codex must still be restorable.
            (agent-sessions-start "/tmp" 'codex "thread-1")
            (should (equal '(("codex" "thread-1"))
                           (sqlite-select (agent-sessions--db)
                                          "SELECT agent_type, agent_sid
                                             FROM sessions")))
            (should (equal "thread-1"
                           (plist-get (car (agent-sessions--log-rows
                                            "closed_at IS NULL"))
                                      :agent-sid))))
        (when (buffer-live-p term)
          (kill-buffer term))))))

(ert-deftest agent-sessions-never-prompted-claude-is-not-resumable ()
  (let* ((config (make-temp-file "agent-claude-config-" t))
         (process-environment (cons (concat "CLAUDE_CONFIG_DIR=" config)
                                    process-environment)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "projects/-wt" config) t)
          (write-region "" nil (expand-file-name "projects/-wt/used.jsonl"
                                                 config))
          (should (agent-sessions--log-resumable-p
                   (list :agent-type "claude" :agent-sid "used")))
          ;; Logged at `SessionStart', but no message was ever sent.
          (should-not (agent-sessions--log-resumable-p
                       (list :agent-type "claude" :agent-sid "unused")))
          ;; Only Claude defers its transcript; others are taken at their word.
          (should (agent-sessions--log-resumable-p
                   (list :agent-type "codex" :agent-sid "unused"))))
      (delete-directory config t))))

(ert-deftest agent-sessions-mark-unread-moves-to-the-next-row ()
  (with-temp-buffer
    (agent-sessions-dashboard-mode)
    (let ((inhibit-read-only t)
          (file (list :path "/tmp/agent-file-1" :visited-at nil)))
      (magit-insert-section (agent-sessions-root)
        (magit-insert-section (agent-sessions-row "session-1")
          (magit-insert-heading "session 1\n")
          (magit-insert-section (agent-sessions-file "/tmp/agent-file-1")
            (magit-insert-heading "file 1\n"))
          (magit-insert-section (agent-sessions-file "/tmp/agent-file-2")
            (magit-insert-heading "file 2\n")))
        (magit-insert-section (agent-sessions-row "session-2")
          (magit-insert-heading "session 2\n")
          (magit-insert-section (agent-sessions-file "/tmp/agent-file-3")
            (magit-insert-heading "file 3\n"))))
      (goto-char (point-min))
      (search-forward "file 1")
      (cl-letf (((symbol-function 'agent-sessions--file-at-point)
                 (lambda () (list 'session "/tmp/agent-file-1" file)))
                ((symbol-function 'agent-sessions--set-visited)
                 (lambda (_match visited)
                   (plist-put file :visited-at (and visited 1))
                   t))
                ((symbol-function 'agent-sessions--refresh) #'ignore))
        (agent-sessions-mark-unread)
        (should (equal "/tmp/agent-file-2"
                       (oref (magit-current-section) value)))
        (should (plist-get file :visited-at))
        ;; Past a session's last file, on to the next session's row.
        (agent-sessions-mark-unread)
        (should (equal "session-2" (oref (magit-current-section) value)))
        ;; From a session row to its first file; the last row stays put.
        (agent-sessions-mark-unread)
        (should (equal "/tmp/agent-file-3"
                       (oref (magit-current-section) value)))
        (agent-sessions-mark-unread)
        (should (equal "/tmp/agent-file-3"
                       (oref (magit-current-section) value))))
      (should (eq (lookup-key agent-sessions-dashboard-mode-map (kbd "m"))
                  (lookup-key agent-sessions-dashboard-mode-map (kbd "u")))))))

(ert-deftest agent-sessions-mark-unread-toggles-a-session-row ()
  (let ((buf (generate-new-buffer " *agent-session-toggle*"))
        (agent-sessions--registry (make-hash-table :test #'equal)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-sessions--link-at-point) #'ignore)
                  ((symbol-function 'agent-sessions--file-at-point) #'ignore)
                  ((symbol-function 'agent-sessions--session-at-point)
                   (lambda () (list :buffer buf)))
                  ((symbol-function 'agent-sessions--refresh) #'ignore))
          (with-current-buffer buf (setq-local agent-sessions-id "s-1"))
          (puthash "s-1" (list :status 'stopped) agent-sessions--registry)
          (cl-flet ((marked ()
                      (buffer-local-value 'agent-sessions--marked-unread buf)))
            ;; Read -> unread -> read.
            (agent-sessions-mark-unread)
            (should (marked))
            (agent-sessions-mark-unread)
            (should-not (marked))
            ;; An agent-reported needs-attention reads as unread: `u' marks it
            ;; read the way focusing would, without setting the manual mark.
            (puthash "s-1" (list :status 'needs-attention) agent-sessions--registry)
            (agent-sessions-mark-unread)
            (should-not (marked))
            (should (eq 'stopped (plist-get (gethash "s-1" agent-sessions--registry)
                                            :status)))
            ;; The prefix argument always marks read.
            (agent-sessions-mark-unread)
            (should (marked))
            (agent-sessions-mark-unread t)
            (should-not (marked))
            (agent-sessions-mark-unread t)
            (should-not (marked))))
      (kill-buffer buf))))

(ert-deftest agent-sessions-share-link-validates-kind-and-url ()
  (let ((session (list :agent-session-id "thread-1"))
        refreshed)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               (lambda () (setq refreshed t))))
      (should (string-prefix-p
               "Unknown link kind"
               (agent-sessions-share-link
                "terminal" "ticket" "https://example.com/1")))
      (should (string-prefix-p
               "Invalid link URL"
               (agent-sessions-share-link
                "terminal" "linear" "file:///tmp/issue")))
      (should-not refreshed))))

(ert-deftest agent-sessions-open-link-marks-read-before-browsing ()
  (let* ((link (list :kind "pr" :url "https://github.com/org/repo/pull/1"))
         events)
    (cl-letf (((symbol-function 'agent-sessions--link-at-point)
               (lambda () (list 'session "pr" link)))
              ((symbol-function 'agent-sessions--set-link-visited)
               (lambda (match-fn visitedp)
                 (push (list 'visited (funcall match-fn link) visitedp) events)
                 t))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               (lambda () (push '(refresh) events)))
              ((symbol-function 'browse-url)
               (lambda (url) (push (list 'browse url) events))))
      (agent-sessions--open-link-at-point)
      (should
       (equal (nreverse events)
              '((visited t t)
                (refresh)
                (browse "https://github.com/org/repo/pull/1")))))))

(ert-deftest agent-sessions-w-copies-attachment-value ()
  (let ((kill-ring nil)
        (link nil)
        (file nil))
    (cl-letf (((symbol-function 'agent-sessions--link-at-point)
               (lambda () link))
              ((symbol-function 'agent-sessions--file-at-point)
               (lambda () file)))
      (setq file '(session "/tmp/shared plan.md" nil))
      (agent-sessions-copy-attachment)
      (should (equal "/tmp/shared plan.md" (car kill-ring)))
      (setq file nil
            link '(session "https://example.com/pr/1"
                           (:url "https://example.com/pr/1")))
      (agent-sessions-copy-attachment)
      (should (equal "https://example.com/pr/1" (car kill-ring)))
      (setq link nil)
      (agent-sessions-copy-attachment)
      (should (equal "https://example.com/pr/1" (car kill-ring))))
    (should (eq (lookup-key agent-sessions-dashboard-mode-map (kbd "w"))
                #'agent-sessions-copy-attachment))))

(ert-deftest agent-sessions-share-state-sets-and-clears ()
  (let ((session (list :agent-session-id "thread-1"))
        writes refreshes)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions--state-set)
               (lambda (keys state) (push (list keys state) writes)))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               (lambda () (setq refreshes (1+ (or refreshes 0))))))
      (should (equal "State: PLAN-REVIEW"
                     (agent-sessions-share-state "terminal" "PLAN-REVIEW")))
      (should (equal "State cleared"
                     (agent-sessions-share-state "terminal" "")))
      (should (equal (nreverse writes)
                     (list (list (agent-sessions--session-note-keys session)
                                 "PLAN-REVIEW")
                           (list (agent-sessions--session-note-keys session)
                                 nil))))
      (should (= refreshes 2)))))

(ert-deftest agent-sessions-share-state-refuses-unknown-state ()
  (let ((session (list :agent-session-id "thread-1"))
        written)
    (cl-letf (((symbol-function 'agent-sessions--session-for-id)
               (lambda (_) session))
              ((symbol-function 'agent-sessions--state-set)
               (lambda (&rest _) (setq written t)))
              ((symbol-function 'agent-sessions--refresh-if-visible)
               #'ignore))
      (let ((reply (agent-sessions-share-state "terminal" "MERGED")))
        (should (string-prefix-p "Unknown state `MERGED'" reply))
        (should (string-match-p "PLAN-REVIEW" reply)))
      (should-not written))))

(ert-deftest agent-sessions-share-state-without-session ()
  (cl-letf (((symbol-function 'agent-sessions--session-for-share)
             (lambda (&rest _) nil)))
    (should (string-prefix-p
             "No Emacs terminal registered"
             (agent-sessions-share-state "terminal" "DONE")))
    (should (string-prefix-p
             "No Emacs agent session registered for agent id thread-1"
             (agent-sessions-share-state "terminal" "DONE" "thread-1")))))

(ert-deftest agent-sessions-states-cycle-in-workflow-order ()
  (let ((seen nil) (state nil))
    (dotimes (_ (1+ (length agent-sessions-states)))
      (setq state (agent-sessions--next-state state))
      (push state seen))
    (should (equal (nreverse seen)
                   '("TODO" "PLANNING" "PLAN-REVIEW" "IMPLEMENTING"
                     "IMPL-REVIEW" "PR-OPEN" "AUTOFIX" "REVIEW" "DONE"
                     "BLOCKED" "LEARN" nil)))))

(ert-deftest agent-sessions--pi-session-start-is-not-running ()
  ;; Pi fires session_start when its TUI opens at an idle prompt, so a fresh
  ;; session must land on `stopped', not fall through to the `running' default
  ;; in `agent-sessions-hook-notify' — that is the frozen-running-at-nothing bug.
  (should (eq 'stopped (agent-sessions--status-for "pi" "SessionStart")))
  (should (eq 'stopped (agent-sessions--status-for "claude" "SessionStart")))
  ;; Codex fires its SessionStart lazily on the first prompt, the same instant
  ;; as UserPromptSubmit — an unordered SessionStart must be able to clobber
  ;; `running', so Codex gets no mapping at all.
  (should (null (agent-sessions--status-for "codex" "SessionStart"))))

(ert-deftest agent-sessions--reconcile-status-demotes-only-provably-idle ()
  (let* ((buffer (generate-new-buffer " *pi-reconcile-test*"))
       (session (list :status 'running :agent-type "pi" :buffer buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-sessions--tpgid-readable-p)
                   (lambda (_) t)))
          ;; Nothing in the foreground: the shell owns the pty, so pi cannot be
          ;; mid-turn — the killed-mid-turn freeze must read as stopped.
          (cl-letf (((symbol-function 'agent-sessions--foreground-command)
                     (lambda (_) nil)))
            (let ((reconciled (agent-sessions--reconcile-status session buffer)))
              (should (eq 'stopped (plist-get reconciled :status)))
              ;; Display-time only: the registry plist must survive untouched,
              ;; the way `agent-sessions--unread-status' works.
              (should (eq 'running (plist-get session :status)))))
          ;; pi idling at its own prompt still holds the foreground with its
          ;; TUI — no demotion; the SessionStart mapping covers that case.
          (cl-letf (((symbol-function 'agent-sessions--foreground-command)
                     (lambda (_) "pi")))
            (should (eq 'running
                        (plist-get (agent-sessions--reconcile-status
                                    session buffer)
                                   :status)))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions--reconcile-status-needs-tpgid-to-demote ()
  ;; On a platform whose process-attributes omits tpgid, a nil foreground says
  ;; "unknown", not "shell in front" — running must survive untouched.
  (let* ((buffer (generate-new-buffer " *pi-reconcile-tpgid-test*"))
       (session (list :status 'running :agent-type "pi" :buffer buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-sessions--tpgid-readable-p)
                   (lambda (_) nil))
                  ((symbol-function 'agent-sessions--foreground-command)
                   (lambda (_) nil)))
          (should (eq 'running
                      (plist-get (agent-sessions--reconcile-status
                                  session buffer)
                                 :status))))
      (kill-buffer buffer))))

(ert-deftest agent-sessions-copy-tree-as-org-list ()
  (agent-sessions-test--with-temp-db
    (let* ((agent-sessions--notes nil)
           (agent-sessions--order nil)
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
            (oset row type 'agent-sessions-row)
            (oset row value "terminal")
            (write-region "" nil plan)
            (agent-sessions--note-set
             (agent-sessions--session-note-keys session) "Done\nShipped")
            (agent-sessions--share-link
             session "pr" "https://github.com/org/repo/pull/1" "PR #1")
            (agent-sessions--share-file session plan "")
            (agent-sessions--note-set
             (list (agent-sessions--file-note-key plan)) "Read first")
            (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                       (lambda (_) session))
                      ((symbol-function 'magit-current-section)
                       (lambda () row)))
              (agent-sessions-copy-tree))
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

(ert-deftest agent-sessions-copy-tree-nests-forks-under-parent ()
  (agent-sessions-test--with-temp-db
    (let* ((agent-sessions--notes nil)
           (agent-sessions--order nil)
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
                             (oset row type 'agent-sessions-row)
                             (oset row value (car entry))
                             (oset row parent worktree)
                             row))
                         sessions)))
      (oset worktree type 'agent-sessions-worktree)
      (oset worktree value (list :label "wt" :path "/nonexistent/wt"))
      (oset worktree children rows)
      (cl-letf (((symbol-function 'agent-sessions--session-for-id)
                 (lambda (id) (cdr (assoc id sessions))))
                ((symbol-function 'agent-sessions--session-org-link)
                 (lambda (session)
                   (cons (plist-get session :agent-session-id)
                         (plist-get session :agent-session-id)))))
        (should (equal (concat "- [[p][p]]\n"
                               "  - [[c][c]]\n")
                       (agent-sessions--org-tree (car rows) "")))
        (should (equal (concat "- wt\n"
                               "  - [[p][p]]\n"
                               "    - [[c][c]]\n"
                               "  - [[o][o]]\n"
                               "  - [[a][a]]\n"
                               "    - [[b][b]]\n")
                       (agent-sessions--org-tree worktree "")))))))

(ert-deftest agent-sessions-manual-order-ignores-buffer-name ()
  (agent-sessions-test--with-temp-db
   (let* ((agent (generate-new-buffer "*agent*"))
          (bare (generate-new-buffer "*bare*"))
          (entries
           (list (list :session (list :buffer agent :agent-session-id "uuid-1"))
                 (list :session (list :buffer bare))))
          (scope (agent-sessions--sessions-scope "/repo" "/repo/wt"))
          (agent-sessions--order nil))
     (unwind-protect
         (progn
           (with-current-buffer bare (setq-local agent-sessions-id "123-4"))
           ;; An agent is keyed by its own session id, a bare terminal by its
           ;; terminal id.
           (should (equal (mapcar #'agent-sessions--entry-order-key entries)
                          '("uuid-1" "123-4")))
           (agent-sessions--order-write-group scope nil '("123-4" "uuid-1"))
           (with-current-buffer agent (rename-buffer "wt - renamed agent"))
           (with-current-buffer bare (rename-buffer "wt - renamed bare"))
           (should (equal (agent-sessions--apply-manual-order
                           scope #'agent-sessions--entry-order-key entries)
                          (reverse entries))))
       (mapc #'kill-buffer (list agent bare))))))

(defmacro agent-sessions-test--with-titled-terminal (buf &rest body)
  "Run BODY with BUF bound to a terminal buffer in worktree `project_3'."
  (declare (indent 1))
  `(let ((,buf (generate-new-buffer "*eat*")))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-sessions--buffer-repo-info)
                    (lambda (_) (list :worktree-name "project_3"))))
           ,@body)
       (kill-buffer ,buf))))

(ert-deftest agent-sessions-edit-title-renames-buffer ()
  (agent-sessions-test--with-titled-terminal buf
    (cl-letf (((symbol-function 'agent-sessions--session-at-point)
               (lambda () (list :buffer buf)))
              ((symbol-function 'agent-sessions--refresh) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "Fix login")))
      (agent-sessions-edit-title))
    (should (equal (buffer-name buf) "project_3 - Fix login"))
    (should (equal (buffer-local-value 'agent-sessions-title-override buf)
                   "Fix login"))
    ;; Clearing the title keeps the name it last had.
    (cl-letf (((symbol-function 'agent-sessions--session-at-point)
               (lambda () (list :buffer buf)))
              ((symbol-function 'agent-sessions--refresh) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (agent-sessions-edit-title))
    (should-not (buffer-local-value 'agent-sessions-title-override buf))
    (should (equal (buffer-name buf) "project_3 - Fix login"))))

(ert-deftest agent-sessions-restored-title-renames-buffer ()
  (agent-sessions-test--with-titled-terminal buf
    (cl-letf (((symbol-function 'agent-sessions--refresh-if-visible) #'ignore))
      (agent-sessions--restore-title buf (list :title "Old task")))
    (should (equal (buffer-name buf) "project_3 - Old task"))))

(ert-deftest agent-sessions-agent-title-renames-buffer ()
  "An agent's OSC title renames its buffer until the user sets one."
  (agent-sessions-test--with-titled-terminal buf
    (let ((agent-sessions--registry (make-hash-table :test 'equal)))
      (with-current-buffer buf
        (setq-local agent-sessions-id "agent-title")
        ;; A plain shell's title (no agent registered) leaves the name alone.
        (agent-sessions--set-agent-title "user@host:~")
        (should (equal (buffer-name buf) "*eat*"))
        (puthash "agent-title" (list :buffer buf) agent-sessions--registry)
        (agent-sessions--set-agent-title "Fix login")
        (should (equal (buffer-name buf) "project_3 - Fix login"))
        ;; Animated status glyphs don't reach the name.
        (agent-sessions--set-agent-title "⠏ Fix login")
        (should (equal (buffer-name buf) "project_3 - Fix login"))
        (agent-sessions--set-agent-title "✳ Fix login")
        (should (equal (buffer-name buf) "project_3 - Fix login"))
        ;; A user title wins over later agent titles.
        (agent-sessions--set-title-override buf "Mine")
        (agent-sessions--set-agent-title "Agent again")
        (should (equal (buffer-name buf) "project_3 - Mine"))
        (should (equal agent-sessions-title "Agent again"))))))

(ert-deftest agent-sessions-terminal-id-for-agent ()
  "A Codex thread id finds its own terminal, whatever id the daemon carries."
  (let ((agent-sessions--registry (make-hash-table :test 'equal))
        (live (generate-new-buffer "*live*"))
        (dead (generate-new-buffer "*dead*")))
    (unwind-protect
        (progn
          (puthash "1-73" (list :agent-session-id "thread-a" :buffer live)
                   agent-sessions--registry)
          (puthash "1-23" (list :agent-session-id "thread-b" :buffer dead)
                   agent-sessions--registry)
          (kill-buffer dead)
          (should (equal (agent-sessions-terminal-id-for-agent "thread-a")
                         "1-73"))
          (should (equal (agent-sessions-terminal-id-for-agent "thread-b") ""))
          (should (equal (agent-sessions-terminal-id-for-agent "nope") "")))
      (kill-buffer live))))

(ert-deftest agent-sessions-ghostel-pre-spawn-registers-plain-terminals ()
  "A plain ghostel terminal gets an id; its compile and eshell buffers don't."
  (let ((agent-sessions-id->buffer (make-hash-table :test 'equal))
        (term (generate-new-buffer "*ghostel*"))
        (compile (generate-new-buffer "*ghostel-compile*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-sessions--log-open) #'ignore)
                  ((symbol-function 'agent-sessions--refresh-if-visible)
                   #'ignore))
          (with-current-buffer term
            (setq-local ghostel-identity '((kind . term) (name . "x")))
            (let ((process-environment '("PATH=/usr/bin")))
              (agent-sessions--ghostel-pre-spawn)
              (should agent-sessions-id)
              (should (equal (getenv "EMACS_AGENT_SESSION_ID")
                             agent-sessions-id))
              (should (eq (gethash agent-sessions-id
                                   agent-sessions-id->buffer)
                          term))))
          (with-current-buffer compile
            (setq-local ghostel-identity '((kind . compile)))
            (let ((process-environment '("PATH=/usr/bin")))
              (agent-sessions--ghostel-pre-spawn)
              (should-not agent-sessions-id)
              (should-not (getenv "EMACS_AGENT_SESSION_ID")))))
      (mapc #'kill-buffer (list term compile)))))

(ert-deftest agent-sessions-ghostel-terminal-pid ()
  "A ghostel terminal's pid is its native shell's, not its event pipe's."
  (let ((buf (generate-new-buffer "*ghostel*")))
    (unwind-protect
        (let ((pipe (make-pipe-process :name "test-ghostel-pipe" :buffer buf
                                       :noquery t)))
          (with-current-buffer buf
            (setq major-mode 'ghostel-mode)
            (setq-local ghostel--pid 4242))
          (should (eq (agent-sessions--buffer-terminal-type buf) 'ghostel))
          (should (eql (agent-sessions--terminal-pid buf) 4242))
          (delete-process pipe)
          (should-not (agent-sessions--terminal-pid buf)))
      (kill-buffer buf))))

(ert-deftest agent-sessions-orchestration-ghostel-faint-is-ghost-text ()
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
      (should (agent-sessions-orchestration--claude-ghost-completion-p faint))
      (should (agent-sessions-orchestration--claude-ghost-completion-p faint-on-red))
      (should-not (agent-sessions-orchestration--claude-ghost-completion-p green-faint))
      (should-not (agent-sessions-orchestration--claude-ghost-completion-p
                   (concat typed faint))))))

(ert-deftest agent-sessions-orchestration-worker-title-renames-buffer ()
  (agent-sessions-test--with-titled-terminal buf
    (agent-sessions-orchestration--mark-worker buf "coord" (list) "Write tests")
    (should (equal (buffer-name buf) "project_3 - Write tests"))))

;;; Global mode and package defaults

(defmacro agent-sessions-test-with-mode-state (&rest body)
  "Run BODY with the mode's global side effects isolated and undone."
  (declare (indent 0))
  `(let ((project-switch-commands (copy-tree project-switch-commands))
         (project-prefix-map (copy-keymap project-prefix-map))
         (agent-sessions--heartbeat-timer nil)
         (agent-sessions--passthrough-added nil)
         (after-load-functions after-load-functions)
         (buffer-list-update-hook buffer-list-update-hook)
         (kill-emacs-hook kill-emacs-hook))
     (cl-letf (((symbol-function 'server-start) #'ignore)
               ((symbol-function 'server-running-p) (lambda (&rest _) t)))
       (unwind-protect (progn ,@body)
         (agent-sessions-mode -1)))))

(ert-deftest agent-sessions-mode-installs-and-removes-its-wiring ()
  (agent-sessions-test-with-mode-state
    (agent-sessions-mode 1)
    (should (memq #'agent-sessions--after-load after-load-functions))
    (should (memq #'agent-sessions--clear-attention-on-focus
                  buffer-list-update-hook))
    (should (memq #'agent-sessions--log-close-all kill-emacs-hook))
    (should (timerp agent-sessions--heartbeat-timer))
    (should (eq (lookup-key project-prefix-map (kbd "a"))
                #'agent-sessions-switch-or-new))
    (should (member agent-sessions--project-switch-command
                    project-switch-commands))
    (agent-sessions-mode -1)
    (should-not (memq #'agent-sessions--after-load after-load-functions))
    (should-not (memq #'agent-sessions--clear-attention-on-focus
                      buffer-list-update-hook))
    (should-not (memq #'agent-sessions--log-close-all kill-emacs-hook))
    (should-not agent-sessions--heartbeat-timer)
    (should-not (lookup-key project-prefix-map (kbd "a")))
    (should-not (member agent-sessions--project-switch-command
                        project-switch-commands))))

(ert-deftest agent-sessions-mode-leaves-a-function-switch-menu-alone ()
  (agent-sessions-test-with-mode-state
    (setq project-switch-commands #'project-prefix-or-any-command)
    (agent-sessions-mode 1)
    (should (eq project-switch-commands #'project-prefix-or-any-command))
    (agent-sessions-mode -1)
    (should (eq project-switch-commands #'project-prefix-or-any-command))))

(ert-deftest agent-sessions-eat-key-spells-meta-as-escape ()
  (should (equal (agent-sessions--eat-key "M-m") [?\e ?m]))
  (should (equal (agent-sessions--eat-key "C-c") [?\C-c]))
  (should (equal (agent-sessions--eat-key "M-m a") [?\e ?m ?a])))

(defvar agent-sessions-test--exceptions nil
  "Stand-in for a terminal package's passthrough list.")

(ert-deftest agent-sessions-passthrough-adds-and-removes-only-its-keys ()
  (let ((agent-sessions--passthrough-lists
         '((fake agent-sessions-test--exceptions . identity)))
        (agent-sessions--passthrough-added nil)
        (agent-sessions-test--exceptions '("C-c" "M-x"))
        (agent-sessions-terminal-passthrough-keys '("M-m" "M-x")))
    (agent-sessions--apply-passthrough 'fake)
    (should (equal agent-sessions-test--exceptions '("M-m" "C-c" "M-x")))
    ;; Applying again (a package reloaded) adds nothing twice.
    (agent-sessions--apply-passthrough 'fake)
    (should (equal agent-sessions-test--exceptions '("M-m" "C-c" "M-x")))
    ;; "M-x" was already there, so it is the user's and stays.
    (agent-sessions--remove-passthrough)
    (should (equal agent-sessions-test--exceptions '("C-c" "M-x")))))

(ert-deftest agent-sessions-passthrough-reaches-eat ()
  (require 'eat)
  (let ((saved eat-semi-char-non-bound-keys)
        (agent-sessions--passthrough-added nil)
        (agent-sessions-terminal-passthrough-keys '("M-m")))
    (unwind-protect
        (progn
          (agent-sessions--apply-passthrough 'eat)
          (should (member [?\e ?m] eat-semi-char-non-bound-keys))
          (agent-sessions--remove-passthrough)
          (should (equal eat-semi-char-non-bound-keys saved)))
      (customize-set-variable 'eat-semi-char-non-bound-keys saved))))

(ert-deftest agent-sessions-terminal-auto-picks-the-first-installed ()
  (let ((agent-sessions-terminal 'auto))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (lib &rest _) (member lib '("vterm" "eat")))))
      (should (eq (agent-sessions--terminal-backend) 'vterm)))
    (cl-letf (((symbol-function 'locate-library) (lambda (&rest _) nil)))
      (should-error (agent-sessions--terminal-backend) :type 'user-error)))
  (let ((agent-sessions-terminal 'eat))
    (should (eq (agent-sessions--terminal-backend) 'eat))))

(ert-deftest agent-sessions-terminal-mode-binds-org-link-insertion ()
  (with-temp-buffer
    (agent-sessions-terminal-mode 1)
    (should (eq (key-binding (kbd "C-c C-l"))
                #'agent-sessions-insert-org-link))))

(provide 'agent-sessions-test)
;;; agent-sessions-test.el ends here
