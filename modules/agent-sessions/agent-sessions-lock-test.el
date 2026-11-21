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

