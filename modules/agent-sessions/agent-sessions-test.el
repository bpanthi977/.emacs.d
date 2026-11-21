;;; agent-sessions-test.el --- Tests for agent-sessions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-sessions)

(defun bp/agent-orchestration-test--message (type subject)
  (list :id (concat "test-" subject) :from "sender" :from-label "sender"
        :type type :subject subject :body "" :files "" :report "" :read nil))

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

(provide 'agent-sessions-test)
;;; agent-sessions-test.el ends here
