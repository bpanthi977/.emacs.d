;; Dashboard for Claude/Codex sessions running in vterm buffers.
;;
;; Every vterm buffer gets a unique id injected into its shell environment
;; (EMACS_AGENT_SESSION_ID) at creation time, with no special launcher command
;; needed. Hook scripts for Claude Code / Codex forward their event payloads
;; to `bp/agent-hook-notify' via emacsclient, keyed by that id, so sessions
;; sharing a worktree still resolve to the right buffer.

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'server)
;; Not autoloaded: `with-sqlite-transaction' is a macro in sqlite.el, so
;; without this the persistent store's writes fail at *call* time, long after
;; the file has loaded cleanly.
(require 'sqlite)
(require 'magit)
(require 'magit-section)
(require 'project)

(defconst bp/agent-sessions--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "agent-sessions") default-directory))
  "Directory this package lives in; its hook forwarder scripts sit beside it.")

(defconst bp/agent-sessions--bin-dir
  (expand-file-name "bin" bp/agent-sessions--dir)
  "Directory of the commands an agent may run from inside a session terminal.
Placed on the PATH of every session terminal, so `emacs-share-file' is simply
there — nothing for the user to install and nothing for the agent to locate.")

(defvar bp/agent-sessions--extra-terminal-env nil
  "Extra `VAR=VALUE' strings to inject into the next terminal created.
Bound around a terminal-creating call.  It exists because the environment is
assembled by the vterm/eat advice, deep inside those packages' own internals,
so there is no argument to thread through: see `bp/agent-orchestration-spawn',
the only binder.  A worker learns its role and its coordinator from the
environment rather than from its launch command because the id it is addressed
by is minted by that same advice, and so is not known to the caller yet.")

(defun bp/agent-sessions--terminal-environment (id)
  "Environment entries to inject into a session terminal identified by ID.
The PATH entry is built from the PATH the terminal was going to inherit anyway
\(`process-environment' is what the subprocess gets), so this is strictly
additive.  Assembling one from anywhere else could silently *shrink* the
shell's PATH, which is the failure mode worth designing away here."
  (append (list (format "EMACS_AGENT_SESSION_ID=%s" id)
                (format "PATH=%s%s%s" bp/agent-sessions--bin-dir path-separator
                        (or (getenv "PATH") "")))
          bp/agent-sessions--extra-terminal-env))

(defvar bp/agent-session-id->buffer (make-hash-table :test 'equal)
  "Session id -> vterm buffer, populated for every vterm buffer at creation.")

(defvar bp/agent-sessions (make-hash-table :test 'equal)
  "Session id -> plist (:buffer :agent-type :status :last-event :updated-at :repo :worktree).
Populated lazily: an entry only exists once a hook event has fired for it.")

(defvar bp/agent-session-counter 0)

(defconst bp/agent-orchestration-message-types
  '("status" "done" "blocked" "question" "answer")
  "The types an inter-agent message may carry.
Deliberately short: `done' is the one an orchestrator waits on, `blocked' and
`question' are the two ways a worker stops short of it, `answer' closes a
question, and `status' is everything else.  A larger vocabulary would have to
earn itself by changing what some command *does*, not just what a row says.")

(defvar bp/agent-orchestration--mail (make-hash-table :test 'equal)
  "Recipient terminal id -> list of message plists, oldest first.
A message is (:id :from :from-key :from-label :to :to-key :type :subject :body
:files :report :sent-at :read).

This is the working copy, keyed the way mail is *addressed* at runtime; the
record is the `mail' table, keyed by durable identity.  Both exist because they
answer different questions: the render path reads this one (it must never touch
the database), and a session that outlives this Emacs is found again through the
other.  Filled from the database by `bp/agent-orchestration--hydrate' when a
session's identity becomes known, and dropped for a terminal when it is killed
\(`bp/agent-session--cleanup') — dropping the working copy, never the record.")

(defvar bp/agent-orchestration--polls (make-hash-table :test 'equal)
  "Terminal id -> `float-time' of its last inbox read.
`emacs-agent wait' polls, so a recent read means that agent is coming back for
its own mail; see `bp/agent-orchestration--polling-p'.")

(defvar bp/agent-orchestration--counter 0
  "Source of message and brief ids within this Emacs.")

(defvar-local bp/agent-orchestration-coordinator nil
  "Terminal id of the agent that spawned this one, when it was spawned by one.
Set on the worker's buffer at spawn, which is what `emacs-agent list' reads to
answer \"whose workers are these\" without consulting the message log.")

(defvar-local bp/agent-session-id nil
  "Unique id injected into this vterm buffer's shell environment, if any.")

(defvar-local bp/agent-session--created-at nil
  "When this terminal buffer was created; the dashboard's stable sort key.
Lives on the buffer rather than in the session plist because the plist is
rebuilt from scratch on every hook event, and because plain terminals that
have never fired a hook need the same key.  See
`bp/agent-sessions--sort-tree-stable'.")

(defconst bp/agent-sessions-buffer-name "*Agent Sessions*")

(defcustom bp/agent-sessions-terminal 'vterm
  "Terminal emulator backend used for agent sessions.
Either `vterm' or `eat'.  Only the selected package needs to be installed;
it is loaded lazily the first time a session terminal is created."
  :type '(choice (const :tag "vterm" vterm)
                 (const :tag "eat" eat)))

(defcustom bp/agent-sessions-show-all-worktrees nil
  "When non-nil, list every worktree of a repo, even those with no session.
This makes it easy to spin up a new session in an idle worktree with the
`+' key (`bp/agent-sessions-new-vterm').  Only repos that already have at
least one session are shown; the option controls whether their empty
worktrees are listed too."
  :type 'boolean)

(defcustom bp/agent-session-notify-on-attention t
  "When non-nil, show a desktop notification when a session needs attention.
The notification fires only on the transition into `needs-attention'/`error',
not on every hook event, so a session that stays waiting is announced once."
  :type 'boolean)

(defvar bp/agent-sessions-sort-by-activity nil
  "When non-nil, sort repos, worktrees, and sessions by most recent activity.
Off by default because `:updated-at' changes on every hook event, so two
sessions running at once continually swap places in the dashboard.  The
default is the stable order of `bp/agent-sessions--sort-tree-stable'.
Toggle with `s' \(`bp/agent-sessions-toggle-sort').")

(defconst bp/agent-session-status-alist
  '((claude . ((Stop . needs-attention)
               (StopFailure . error)
               (PermissionRequest . needs-attention)
               (TeammateIdle . needs-attention)
               (UserPromptSubmit . running)))
    (codex . ((Stop . needs-attention)
              (PermissionRequest . needs-attention)
              (UserPromptSubmit . running)
              (agent-turn-complete . needs-attention))))
  "Per-agent hook/notify event name -> status.
Events not listed here (PreToolUse, PostToolUse, SubagentStart/Stop, etc.)
leave a session's existing status unchanged rather than resetting it — hook
invocations are independent async processes with no delivery ordering
guarantee, so an informational event can arrive after Stop/PermissionRequest
and must not clobber the needs-attention state those set.")

(defun bp/agent-session--status-for (agent-type event-name)
  "Return the status EVENT-NAME maps to, or nil if it shouldn't change status."
  (cdr (assq (intern event-name)
             (alist-get (intern agent-type) bp/agent-session-status-alist))))

(defun bp/agent-sessions--canonical-path (path)
  "Return PATH canonicalized to a directory name for use as a hash-table key."
  (and path (file-name-as-directory (file-truename path))))

(defun bp/agent-sessions--worktree-label (path branch)
  "Return the display label for the worktree at PATH on BRANCH.
Shows the worktree directory name and, when known, its branch as
`<dir> (<branch>)' so sessions sharing a repo but on different worktrees are
easy to tell apart.  Falls back to just the directory name when BRANCH is nil
(detached HEAD)."
  (let ((dir (file-name-nondirectory (directory-file-name path))))
    (if branch
        (format "%s (%s)" dir branch)
      dir)))

(defun bp/agent-session--repo-info (dir)
  "Return session repo/worktree info for the git repo containing DIR.
The plist has :repo NAME, :repo-root ROOT (the main worktree's directory),
:worktree LABEL, and :worktree-path DIR."
  (let* ((default-directory dir)
         (toplevel (ignore-errors (magit-toplevel))))
    (if (not toplevel)
        (list :repo nil :repo-root nil :worktree nil :worktree-path nil)
      (let* ((common-dir (ignore-errors
                            (let ((default-directory toplevel))
                              (magit-git-string "rev-parse" "--path-format=absolute"
                                                 "--git-common-dir"))))
             (main-root (if common-dir
                            (file-name-as-directory (expand-file-name ".." common-dir))
                          toplevel))
             (repo-name (file-name-nondirectory (directory-file-name main-root)))
             (branch (ignore-errors (let ((default-directory toplevel))
                                       (magit-get-current-branch))))
             (worktree-label (bp/agent-sessions--worktree-label toplevel branch)))
        (list :repo repo-name
              :repo-root (file-name-as-directory main-root)
              :worktree worktree-label
              :worktree-path (file-name-as-directory toplevel))))))

(defun bp/agent-sessions--compute-worktrees (root)
  "Return (:label L :path P) for every non-bare worktree of the repo at ROOT.
Runs git; use `bp/agent-sessions--all-worktrees' for the cached version."
  (ignore-errors
    (let ((default-directory root))
      (delq nil
            (mapcar
             (lambda (wt)
               ;; (PATH COMMIT BRANCH BARE DETACHED LOCKED PRUNABLE)
               (unless (nth 3 wt)
                 (let* ((path (car wt))
                        (branch (nth 2 wt))
                        (label (bp/agent-sessions--worktree-label path branch)))
                   (list :label label :path (file-name-as-directory path)))))
             (magit-list-worktrees))))))

(defvar bp/agent-sessions--worktrees-cache (make-hash-table :test 'equal)
  "Cache of repo ROOT -> git worktree list, to keep refreshes cheap.
`magit-list-worktrees' spawns a git subprocess per repo, which made
re-rendering (e.g. toggling the sort with `s') slow.  Invalidated by
`bp/agent-sessions-refresh' (\\`g'), which re-reads git.")

(defun bp/agent-sessions--all-worktrees (root)
  "Return the cached git worktree list for the repo at ROOT.
Populates the cache on first use; `bp/agent-sessions-refresh' clears it."
  (let ((cached (gethash root bp/agent-sessions--worktrees-cache 'miss)))
    (if (eq cached 'miss)
        (puthash root (bp/agent-sessions--compute-worktrees root)
                 bp/agent-sessions--worktrees-cache)
      cached)))

(defun bp/agent-session--cleanup ()
  (when bp/agent-session-id
    (bp/agent-sessions--log-close-buffer)
    (remhash bp/agent-session-id bp/agent-session-id->buffer)
    (remhash bp/agent-session-id bp/agent-sessions)
    (remhash bp/agent-session-id bp/agent-orchestration--mail)
    (remhash bp/agent-session-id bp/agent-orchestration--polls)
    (bp/agent-sessions--refresh-if-visible)))

(defun bp/agent-sessions--vterm-advice (orig-fun pop-to-buf-fun &optional arg)
  (let* ((id (format "%d-%d" (emacs-pid) (cl-incf bp/agent-session-counter)))
         (vterm-environment (append (bp/agent-sessions--terminal-environment id)
                                    vterm-environment))
         (buf (funcall orig-fun pop-to-buf-fun arg)))
    (when (buffer-live-p buf)
      (puthash id buf bp/agent-session-id->buffer)
      (with-current-buffer buf
        (setq-local bp/agent-session-id id)
        (setq-local bp/agent-session--created-at (current-time))
        (add-hook 'kill-buffer-hook #'bp/agent-session--cleanup nil t))
      (bp/agent-sessions--log-open buf)
      (bp/agent-sessions--refresh-if-visible))
    buf))

(defvar-local bp/agent-session-title nil
  "Terminal title most recently set by this vterm's process (OSC escape), if any.
Agents typically set this to something like the current task/session summary.")

(defvar-local bp/agent-session-title-override nil
  "A user-chosen title for this session, set via the dashboard's `t' command.
When non-nil it takes precedence over `bp/agent-session-title' everywhere the
session is labelled, so the agent's OSC title updates don't clobber it.")

(defvar-local bp/agent-session--branched-from nil
  "When non-nil, a cons (PARENT-SID . LABEL) recording the session this one was
branched/forked from.  Set on a freshly branched terminal (by the `b' command
or `bp/agent-sessions-mark-parent') so its agent session records the parentage
once its first hook fires, and preserved across subsequent hook events.")

(defvar-local bp/agent-session--marked-unread nil
  "When non-nil, the user marked this session as needing attention (`u').
Kept on the buffer rather than in `bp/agent-sessions' so it works for a plain
terminal that has no registry entry yet, and so a later hook event's status
computation (which honours the *previous* status) can't be skewed by it.  The
flag only overrides the status at display time, and is cleared the same way a
real needs-attention is: by looking at the session.")

(defun bp/agent-sessions--capture-title (title)
  (setq-local bp/agent-session-title title))

(defun bp/agent-sessions--eat-advice (orig-fun program arg display-fn)
  "Give eat sessions the same id injection/registration as vterm.
Injects EMACS_AGENT_SESSION_ID and the shared-file command's directory via
`process-environment' (which `eat-exec' inherits when it spawns the shell) and
registers the resulting buffer."
  (let* ((id (format "%d-%d" (emacs-pid) (cl-incf bp/agent-session-counter)))
         (process-environment
          (append (bp/agent-sessions--terminal-environment id)
                  process-environment))
         (buf (funcall orig-fun program arg display-fn)))
    (when (buffer-live-p buf)
      (puthash id buf bp/agent-session-id->buffer)
      (with-current-buffer buf
        (setq-local bp/agent-session-id id)
        (setq-local bp/agent-session--created-at (current-time))
        (add-hook 'kill-buffer-hook #'bp/agent-session--cleanup nil t))
      (bp/agent-sessions--log-open buf)
      (bp/agent-sessions--refresh-if-visible))
    buf))

;; eat's default title handler is `ignore', so instead capture the title by
;; advising the emulator's title setter, which runs with the eat buffer
;; current (same idea as the `vterm--set-title' advice above).
(defun bp/agent-sessions--eat-capture-title (title &rest _)
  (setq-local bp/agent-session-title title))

(defun bp/agent-session--clear-attention-on-focus ()
  "Downgrade needs-attention to `stopped' for the session in the selected window.
A needs-attention session has stopped and is waiting for the user; once the
user looks at it the highlight should clear, but the agent is still idle, so
the status becomes `stopped' rather than `running' (which would wrongly imply
it is thinking/outputting).

Deliberately does not rely on `current-buffer' here: vterm's process filter
briefly makes its buffer current while inserting output even when it isn't
the buffer the user is actually looking at, which would clear the flag
spuriously."
  (let* ((buf (window-buffer (selected-window)))
         (id (and (buffer-live-p buf) (buffer-local-value 'bp/agent-session-id buf)))
         (changed nil))
    (when id
      (let ((session (gethash id bp/agent-sessions)))
        ;; A user-set unread mark clears the same way, and for the same reason:
        ;; the user has now looked at the session.
        (when (buffer-local-value 'bp/agent-session--marked-unread buf)
          (with-current-buffer buf (setq-local bp/agent-session--marked-unread nil))
          (setq changed t))
        (when (and session (eq (plist-get session :status) 'needs-attention))
          (puthash id (plist-put session :status 'stopped) bp/agent-sessions)
          (setq changed t))))
    ;; A shared file stops nagging once it has been read, however it was
    ;; opened — `RET' in the dashboard is the common way, but a file the agent
    ;; also mentioned by path and you jumped to yourself has still been read.
    ;; Gated on the database being open, which it is whenever a terminal has
    ;; ever been created, so an Emacs with no sessions pays nothing for this
    ;; on a hook that runs at every buffer switch.
    (when (and (buffer-live-p buf) (sqlitep bp/agent-sessions--db))
      (let ((truename (buffer-local-value 'buffer-file-truename buf)))
        (when (and truename
                   (bp/agent-sessions--shared-path-p truename)
                   (bp/agent-sessions--set-visited
                    (lambda (f) (equal (file-truename (plist-get f :path))
                                       truename))
                    t))
          (setq changed t))))
    (when changed
      (bp/agent-sessions--refresh-if-visible))))

(defun bp/agent-session--desktop-notify (title body)
  "Show a desktop notification with TITLE and BODY.
Uses `osascript' on macOS, D-Bus `notifications-notify' where available, and
falls back to the echo area otherwise.  Always non-blocking."
  (cond
   ((eq system-type 'darwin)
    (call-process "osascript" nil 0 nil
                  "-e" (format "display notification %S with title %S"
                               body title)))
   ((fboundp 'notifications-notify)
    (notifications-notify :title title :body body))
   (t (message "%s: %s" title body))))

(defun bp/agent-session--notify-attention (buf agent-type info status)
  "Notify that BUF's AGENT-TYPE session needs attention.
INFO is the repo plist from `bp/agent-session--repo-info'; STATUS is the new
\(attention-worthy) status.  The notification title is `<repo> - <worktree>';
the body names the agent type, state, and its terminal title (task summary)."
  (let* ((repo (plist-get info :repo))
         (wt (plist-get info :worktree))
         (where (cond ((and repo wt) (format "%s - %s" repo wt))
                      (wt wt)
                      (repo repo)
                      (t (buffer-name buf))))
         (title (or (buffer-local-value 'bp/agent-session-title buf)
                    (buffer-name buf))))
    (bp/agent-session--desktop-notify
     where
     (format "%s %s: %s"
             agent-type
             (if (eq status 'error) "error" "needs attention")
             title))))

(defun bp/agent-hook-notify (session-id agent-type payload-file)
  "Entry point called via emacsclient from the Claude/Codex hook scripts."
  (let* ((buf (gethash session-id bp/agent-session-id->buffer))
         (payload (unwind-protect
                      (ignore-errors
                        (json-parse-string (with-temp-buffer
                                              (insert-file-contents payload-file)
                                              (buffer-string))))
                    (ignore-errors (delete-file payload-file))))
         (event-name (or (and payload (gethash "hook_event_name" payload))
                         (and payload (gethash "type" payload))
                         "unknown"))
         (existing (gethash session-id bp/agent-sessions))
         (status (or (bp/agent-session--status-for agent-type event-name)
                     (and existing (plist-get existing :status))
                     'running))
         ;; The agent's *own* session id (a UUID), as opposed to our
         ;; per-buffer EMACS_AGENT_SESSION_ID. This is what `claude --resume'
         ;; / `codex resume' need. Not every event carries it, so fall back
         ;; to the previously captured value.
         (agent-session-id (or (and payload (gethash "session_id" payload))
                               (and payload (gethash "conversation_id" payload))
                               (and payload (gethash "thread_id" payload))
                               (and existing (plist-get existing :agent-session-id))))
         ;; Path to this session's transcript (Claude includes it on every
         ;; hook, notably SessionStart). Used to auto-detect fork parentage.
         (transcript-path (or (and payload (gethash "transcript_path" payload))
                              (and existing (plist-get existing :transcript-path)))))
    (when (buffer-live-p buf)
      (let* ((info (bp/agent-session--repo-info (buffer-local-value 'default-directory buf)))
             ;; Parentage, in priority order: what a previous event already
             ;; recorded; the marker a fresh `b'-branch left on the buffer; or,
             ;; the first time we see this session, an auto-detected fork parent
             ;; (a manual `claude … --fork-session' shares the parent's history).
             (branched-from (or (and existing (plist-get existing :branched-from))
                                (buffer-local-value 'bp/agent-session--branched-from buf)
                                (and (not existing)
                                     (equal agent-type "claude")
                                     transcript-path
                                     (bp/agent-sessions--detect-parent
                                      agent-session-id transcript-path session-id)))))
        (puthash session-id
                 (list :buffer buf
                       :agent-type agent-type
                       :agent-session-id agent-session-id
                       :transcript-path transcript-path
                       :status status
                       :last-event event-name
                       :updated-at (current-time)
                       :repo (plist-get info :repo)
                       :repo-root (plist-get info :repo-root)
                       :worktree (plist-get info :worktree)
                       :worktree-path (plist-get info :worktree-path)
                       :branched-from branched-from)
                 bp/agent-sessions)
        ;; Keep the restore log in step.  `SessionEnd' is the agent telling us
        ;; it is going away while its terminal stays; every other event just
        ;; describes a session that is still running.  Note this deliberately
        ;; does not appear in `bp/agent-session-status-alist': ending is not a
        ;; status a straggler event should be able to undo.
        (if (equal event-name "SessionEnd")
            (bp/agent-sessions--log-end buf)
          (bp/agent-sessions--log-update buf agent-type agent-session-id))
        ;; Notify only on the transition *into* an attention state, so a
        ;; session that stays waiting isn't re-announced on every later event.
        (when (and bp/agent-session-notify-on-attention
                   (bp/agent-sessions--attention-p status)
                   (not (bp/agent-sessions--attention-p
                         (and existing (plist-get existing :status)))))
          (bp/agent-session--notify-attention buf agent-type info status))
        ;; This event is where a session's durable identity becomes known, so
        ;; it is also where mail stored for it under that identity — by a
        ;; previous Emacs, or before this terminal had an agent id — is picked
        ;; up.  Then deliver: mail that arrived while the session was mid-turn
        ;; had nowhere to go, because pasting into a running TUI would land in
        ;; the middle of its turn, and this event is the moment that stops
        ;; being true.
        (bp/agent-orchestration--hydrate session-id)
        (bp/agent-orchestration-deliver session-id)))
    (bp/agent-sessions--refresh-if-visible))
  nil)

(defvar-local bp/agent-sessions--repo-info-cache nil
  "Cons of (DIRECTORY . REPO-INFO-PLIST) cached per terminal buffer.
Computing repo info shells out to git, so cache it per buffer and only
recompute when the buffer's `default-directory' changes (e.g. after a `cd').")

(defun bp/agent-sessions--buffer-repo-info (buf)
  "Return `bp/agent-session--repo-info' for BUF, cached on its directory."
  (with-current-buffer buf
    (let ((dir default-directory))
      (if (and bp/agent-sessions--repo-info-cache
               (equal (car bp/agent-sessions--repo-info-cache) dir))
          (cdr bp/agent-sessions--repo-info-cache)
        (cdr (setq bp/agent-sessions--repo-info-cache
                   (cons dir (bp/agent-session--repo-info dir))))))))

(defun bp/agent-sessions--buffer-terminal-type (buf)
  "Return a symbol naming BUF's terminal backend (`vterm', `eat', or `term')."
  (pcase (buffer-local-value 'major-mode buf)
    ('vterm-mode 'vterm)
    ('eat-mode 'eat)
    (_ 'term)))

(defvar-local bp/agent-sessions--foreground-cache nil
  "Cache for `bp/agent-sessions--foreground-command': (TPGID . NAME).")

(defun bp/agent-sessions--foreground-command (buf)
  "Name of the command running in the foreground of BUF's terminal, or nil.
Nil means BUF's own shell is in the foreground — nothing is running — so the
caller should fall back to naming the terminal backend.

Reads the pty's foreground process *group* (`tpgid') straight out of Emacs's
native process table: two `process-attributes' calls, no subprocess and no
walk of the process list.  When a command is in the foreground the kernel
points `tpgid' at that job's leader; when it exits, `tpgid' returns to the
shell's own `pgrp', so a row tracks start and finish with no bookkeeping and
nothing to keep in sync.

The name comes from `args', not `comm': Claude's executable lives in a
version-numbered directory, so its `comm' reads \"2.1.231\".  Platforms whose
`process-attributes' omits `tpgid' just get nil, and rows read as before.

The foreground group is re-read on every call, and only the *name* is cached,
reused while that group is unchanged.  That test is exact rather than a
heuristic — the same foreground group is the same job, so its name cannot have
changed — which is why there is no time-based reuse here.  A timed cache was
tried and removed: reading the group for 12 terminals costs ~3.6ms against a
~22ms refresh, and skipping that check bought back only those 3.6ms in
exchange for a label that could be up to a third of a second out of date.  For
a display whose whole purpose is showing what is running now, that is the
wrong side of the trade; `g' already costs ~1.4s, so this is not the expensive
thing on the path.  What the cache does save is real: reading a *command line*
is the costly half, and a job that sits there never pays it twice."
  (with-current-buffer buf
    (let* ((proc (get-buffer-process buf))
           (attrs (and proc (process-live-p proc)
                       (ignore-errors (process-attributes (process-id proc)))))
           (tpgid (cdr (assq 'tpgid attrs)))
           (pgrp (cdr (assq 'pgrp attrs)))
           ;; Non-nil only when some job other than the shell holds the
           ;; terminal, i.e. a command is actually running.
           (fg (and (integerp tpgid) (> tpgid 0) (not (eql tpgid pgrp)) tpgid))
           (cache bp/agent-sessions--foreground-cache))
      (cond
       ((null fg)
        (setq-local bp/agent-sessions--foreground-cache nil)
        nil)
       ((and cache (eql fg (car cache)))
        (cdr cache))
       (t
        (let* ((args (cdr (assq 'args (ignore-errors (process-attributes fg)))))
               (name (and (stringp args) (not (string-empty-p args))
                          (file-name-nondirectory
                           (car (split-string args nil t))))))
          (setq-local bp/agent-sessions--foreground-cache (cons fg name))
          name))))))

(defun bp/agent-sessions--terminal-session (buf)
  "Synthesize a session plist for a plain terminal BUF with no agent session.
These fill in for vterm/eat buffers that have not (yet) fired an agent hook,
so idle terminals still appear in the dashboard tree."
  (let ((info (bp/agent-sessions--buffer-repo-info buf)))
    (list :buffer buf
          ;; Whatever is running right now, agent or not; the backend name only
          ;; when the terminal is sitting at its shell prompt.
          :agent-type (or (bp/agent-sessions--foreground-command buf)
                          (bp/agent-sessions--buffer-terminal-type buf))
          :status 'idle
          :last-event nil
          :updated-at nil
          ;; What the buffer already knows about its parentage, so a terminal
          ;; that was just branched or spawned nests under the session that
          ;; created it straight away rather than only once its first hook
          ;; event promotes the marker into a registry entry.
          :branched-from (buffer-local-value 'bp/agent-session--branched-from buf)
          :repo (plist-get info :repo)
          :repo-root (plist-get info :repo-root)
          :worktree (plist-get info :worktree)
          :worktree-path (plist-get info :worktree-path))))

(defun bp/agent-sessions--session-for-id (id)
  "Return the session plist for ID.
Prefers a real agent session (`bp/agent-sessions'); falls back to a
synthesized terminal session for a plain terminal buffer that has no
agent hook yet, or nil if no live buffer is registered for ID."
  (or (gethash id bp/agent-sessions)
      (let ((buf (gethash id bp/agent-session-id->buffer)))
        (and (buffer-live-p buf)
             (bp/agent-sessions--terminal-session buf)))))

(defun bp/agent-sessions--session-title (session)
  "The title for SESSION's buffer: the user override if any, else the OSC title."
  (let ((buf (plist-get session :buffer)))
    (and (buffer-live-p buf)
         (or (buffer-local-value 'bp/agent-session-title-override buf)
             (buffer-local-value 'bp/agent-session-title buf)))))

(defun bp/agent-sessions--session-short-label (session)
  "A concise human label for SESSION: its terminal title, else worktree, else id."
  (or (bp/agent-sessions--session-title session)
      (plist-get session :worktree)
      (plist-get session :agent-session-id)
      "?"))

(defun bp/agent-sessions--session-label (session)
  "A disambiguating label for SESSION: `<worktree> [<agent>] <title>'.
Used when prompting the user to pick a session."
  (format "%s [%s] %s"
          (or (plist-get session :worktree) "?")
          (plist-get session :agent-type)
          (or (bp/agent-sessions--session-title session)
              (plist-get session :agent-session-id)
              "")))

(defcustom bp/agent-session-auto-detect-branches t
  "When non-nil, auto-detect that a Claude session was forked from another.
A fork (`claude --resume … --fork-session', or Claude's own fork feature)
copies the parent's transcript verbatim, so the child's message uuids include
the parent's, sharing the same root uuid.  On the child's first hook we scan
tracked sessions and record the best-matching parent as `:branched-from', the
same field the `b' command and `bp/agent-sessions-mark-parent' set by hand."
  :type 'boolean)

(defvar bp/agent-sessions--uuid-cache (make-hash-table :test 'equal)
  "Transcript PATH -> (MTIME ROOT-UUID . UUID-HASHSET).
Caches the message uuids of a JSONL transcript, keyed on file mtime so a
grown transcript is re-read but an unchanged one is not.")

(defun bp/agent-sessions--transcript-uuids (path)
  "Return (ROOT-UUID . SET) of message uuids in the JSONL transcript at PATH.
ROOT-UUID is the first message uuid (the conversation root, shared by every
fork of a lineage); SET is a hash-set of all message uuids.  Cached by mtime.
Returns nil when PATH is unreadable or has no message uuids."
  (when (and path (file-readable-p path))
    (let* ((mtime (file-attribute-modification-time (file-attributes path)))
           (cached (gethash path bp/agent-sessions--uuid-cache)))
      (if (and cached (equal (car cached) mtime))
          (cdr cached)
        (let ((set (make-hash-table :test 'equal))
              (root nil))
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (while (re-search-forward
                    "\"uuid\"[[:space:]]*:[[:space:]]*\"\\([0-9a-fA-F-]+\\)\"" nil t)
              (let ((u (match-string 1)))
                (unless root (setq root u))
                (puthash u t set))))
          (if (zerop (hash-table-count set))
              (progn (puthash path (cons mtime nil) bp/agent-sessions--uuid-cache) nil)
            (cdr (puthash path (cons mtime (cons root set))
                          bp/agent-sessions--uuid-cache))))))))

(defun bp/agent-sessions--detect-parent (child-sid child-transcript self-id)
  "Return (PARENT-SID . LABEL) if CHILD-TRANSCRIPT looks like a fork, else nil.
CHILD-SID is the child's own agent session id and SELF-ID its EMACS session id
\(both used to avoid matching the child against itself).  A fork copies its
parent's messages verbatim, so among tracked Claude sessions sharing the
child's root uuid, the parent is the one whose uuids are (almost) fully
contained in the child's; the largest such match is the immediate parent."
  (when bp/agent-session-auto-detect-branches
    (let ((child (bp/agent-sessions--transcript-uuids child-transcript)))
      (when (and child (cdr child) (> (hash-table-count (cdr child)) 1))
        (let ((child-root (car child))
              (child-set (cdr child))
              (best nil) (best-overlap 0))
          (maphash
           (lambda (id session)
             (let ((p-path (plist-get session :transcript-path))
                   (p-sid (plist-get session :agent-session-id)))
               (when (and p-path
                          (equal (plist-get session :agent-type) "claude")
                          (not (equal id self-id))
                          (not (and child-sid p-sid (equal p-sid child-sid)))
                          (not (equal p-path child-transcript)))
                 (let ((p (bp/agent-sessions--transcript-uuids p-path)))
                   (when (and p (cdr p) (equal (car p) child-root))
                     (let ((p-set (cdr p)) (p-size 0) (overlap 0))
                       (maphash (lambda (u _)
                                  (cl-incf p-size)
                                  (when (gethash u child-set) (cl-incf overlap)))
                                p-set)
                       ;; Parent must be (nearly) wholly contained in the child
                       ;; and smaller than it; pick the most-contained match.
                       (when (and (> p-size 0)
                                  (>= overlap 2)
                                  (< p-size (hash-table-count child-set))
                                  (>= (/ (float overlap) p-size) 0.85)
                                  (> overlap best-overlap))
                         (setq best session best-overlap overlap))))))))
           bp/agent-sessions)
          (when best
            (cons (plist-get best :agent-session-id)
                  (bp/agent-sessions--session-short-label best))))))))

(defun bp/agent-sessions--unread-status (session buf)
  "SESSION with its status forced to `needs-attention' if BUF is marked unread.
Returns a *copy* — the registry plist must not be mutated, or the mark would
leak into the status a later hook event inherits.  An `error' status outranks
the mark and is left alone."
  (if (and (buffer-live-p buf)
           (buffer-local-value 'bp/agent-session--marked-unread buf)
           (not (memq (plist-get session :status) '(needs-attention error))))
      (plist-put (copy-sequence session) :status 'needs-attention)
    session))

(defun bp/agent-sessions--live-entries ()
  "Return (:id ID :session PLIST :title TITLE) for each live terminal buffer.
Includes both real agent sessions (those a hook event has fired for) and
plain vterm/eat buffers with no agent session yet, so idle terminals still
show up in the dashboard.  Opportunistically drops registry entries whose
buffer has been killed, since `kill-buffer-hook' cleanup can be missed (e.g.
buffer killed without running local hooks)."
  (let (entries)
    ;; Real agent sessions: a hook event has fired for these.
    (maphash
     (lambda (id session)
       (let ((buf (plist-get session :buffer)))
         (if (buffer-live-p buf)
             (push (list :id id
                         :session (bp/agent-sessions--unread-status session buf)
                         :title (or (buffer-local-value 'bp/agent-session-title-override buf)
                                    (buffer-local-value 'bp/agent-session-title buf)))
                   entries)
           (remhash id bp/agent-sessions)
           (remhash id bp/agent-session-id->buffer))))
     bp/agent-sessions)
    ;; Plain terminal buffers with no agent session yet.
    (maphash
     (lambda (id buf)
       (cond ((not (buffer-live-p buf))
              (remhash id bp/agent-session-id->buffer))
             ((not (gethash id bp/agent-sessions))
              (push (list :id id
                          :session (bp/agent-sessions--unread-status
                                    (bp/agent-sessions--terminal-session buf) buf)
                          :title (or (buffer-local-value 'bp/agent-session-title-override buf)
                                    (buffer-local-value 'bp/agent-session-title buf)))
                    entries))))
     bp/agent-session-id->buffer)
    (nreverse entries)))

(defface bp/agent-session-needs-attention
  '((t :inherit warning :weight bold :extend t
       :background "#4a3b00" :foreground "#ffd75f"))
  "Face for a session row that needs attention (spans the full row).")

(defface bp/agent-session-error
  '((t :inherit error :weight bold :extend t
       :background "#4a1010" :foreground "#ff8f8f"))
  "Face for a session row in the error state (spans the full row).")

(defun bp/agent-sessions--face-for (status)
  (pcase status
    ('needs-attention 'bp/agent-session-needs-attention)
    ('error 'bp/agent-session-error)
    (_ 'default)))

(defvar-local bp/agent-sessions--row-status nil
  "Alist of (LINE-POS . STATUS) for the rows as last rendered, in buffer order.
Filled by the render path and consumed by `N'/`P'.  It exists because the
status a row *shows* is not the one in the registry — `bp/agent-sessions--
unread-status' overlays the `u' mark onto a copy at display time — so a
navigation command that re-derives status from the registry silently skips
every manually marked row.  Record what was drawn; don't derive it twice.")

(defun bp/agent-orchestration--messages (id)
  (gethash id bp/agent-orchestration--mail))

(defun bp/agent-orchestration--pending (id &optional types)
  "Unread messages for ID, optionally filtered to TYPES (a list of strings)."
  (seq-filter (lambda (m)
                (and (not (plist-get m :read))
                     (or (null types) (member (plist-get m :type) types))))
              (bp/agent-orchestration--messages id)))

(defun bp/agent-orchestration--pending-count (id)
  (length (bp/agent-orchestration--pending id)))

(defun bp/agent-orchestration--consume (messages)
  "Mark MESSAGES read, in memory and in the database.
Each plist carries `:read' from birth so this mutates the stored plist rather
than a copy.  The database write is what stops a message being delivered twice
across a restart; a failure to record it is reported rather than raised, since
an agent reading its inbox must not fail because the database is busy."
  (let ((now (bp/agent-sessions--now)))
    (dolist (m messages)
      (plist-put m :read t)
      (condition-case err
          (sqlite-execute (bp/agent-sessions--db)
                          "UPDATE mail SET read_at = ? WHERE id = ?"
                          (list now (plist-get m :id)))
        (error (message "agent-sessions: could not mark mail read (%s)"
                        (error-message-string err)))))))

(defun bp/agent-orchestration--row-badge (id)
  "The dashboard suffix announcing unread mail for ID, or an empty string."
  (let ((n (bp/agent-orchestration--pending-count id)))
    (if (> n 0) (format "  ✉%d" n) "")))

(defun bp/agent-sessions--insert-session (entry &optional depth suppress-parent-note)
  "Insert a session row for ENTRY.
DEPTH indents the row (children of a branched-from parent are rendered one
level deeper).  When SUPPRESS-PARENT-NOTE is non-nil the `↳ from …' suffix is
omitted — used when the row is already nested under its parent, where the
indentation conveys the relationship."
  (let* ((id (plist-get entry :id))
         (session (plist-get entry :session))
         (status (plist-get session :status))
         (face (bp/agent-sessions--face-for status))
         (marker (pcase status
                   ('needs-attention "● ")
                   ('error "✖ ")
                   (_ "  ")))
         (title (or (plist-get entry :title) ""))
         (last-event (plist-get session :last-event))
         (branched-from (plist-get session :branched-from))
         (indent (make-string (* 2 (or depth 0)) ?\s)))
    ;; Record what this row *shows*, for `N'/`P' (see the variable's docstring).
    (push (cons (line-beginning-position) status) bp/agent-sessions--row-status)
    (magit-insert-section (bp/agent-session-row id)
      ;; A heading rather than a plain insert so the row's note becomes its
      ;; collapsible body; with no note the body is empty and magit shows no
      ;; fold indicator.
      (magit-insert-heading
        (propertize
         (concat "    " indent marker
                 (format "%-7s %-16s %s%s%s%s\n"
                         (plist-get session :agent-type)
                         status
                         title
                         (if last-event (format " (%s)" last-event) "")
                         (bp/agent-orchestration--row-badge id)
                         (if (and branched-from (not suppress-parent-note))
                             (format "  ↳ from %s" (cdr branched-from))
                           "")))
         'font-lock-face face))
      (bp/agent-sessions--insert-note
       (bp/agent-sessions--session-note-keys session)
       (concat "    " indent "    "))
      (bp/agent-sessions--insert-files session (concat "    " indent "    ")))))

(defun bp/agent-sessions--insert-entries (entries)
  "Insert ENTRIES, nesting any branched session under its parent when present.
A session whose `:branched-from' parent id matches another entry in ENTRIES is
rendered indented directly beneath that parent (one extra level per hop);
entries whose parent isn't in this list render at the top level.

The parent/child graph is built and walked by unique entry `:id' (never by
the shared agent session id) and a session is never made its own parent, so
duplicate session ids and reference cycles cannot cause infinite recursion; a
visited set and a final safety-net pass guarantee each entry renders exactly
once."
  (let ((by-sid (make-hash-table :test 'equal))    ; agent-session-id -> entries
        (children (make-hash-table :test 'equal))  ; parent entry :id -> child entries
        (parent-of (make-hash-table :test 'equal)) ; entry :id -> t when it has a parent
        (roots nil))
    ;; Index sid -> list of entries (a session id can appear in more than one
    ;; terminal — notably a just-forked child briefly shares its parent's id
    ;; until the fork mints a new one).
    (dolist (e entries)
      (let ((sid (plist-get (plist-get e :session) :agent-session-id)))
        (when sid (push e (gethash sid by-sid)))))
    (maphash (lambda (sid es) (puthash sid (nreverse es) by-sid)) by-sid)
    ;; Resolve each entry's parent from its recorded parent session id.  Match a
    ;; *different* entry carrying that id: a fork can transiently share its
    ;; parent's session id, so `sid = psid' is fine as long as it's another
    ;; terminal.  The visited set below is what actually prevents cycles.
    (dolist (e entries)
      (let* ((psid (car (plist-get (plist-get e :session) :branched-from)))
             (parent (and psid
                          (seq-find (lambda (c) (not (eq c e)))
                                    (gethash psid by-sid)))))
        (when parent
          (puthash (plist-get e :id) t parent-of)
          (push e (gethash (plist-get parent :id) children)))))
    (dolist (e entries)
      (unless (gethash (plist-get e :id) parent-of)
        (push e roots)))
    (setq roots (nreverse roots))
    (let ((visited (make-hash-table :test 'equal)))
      (cl-labels ((emit (e depth nested)
                    (unless (gethash (plist-get e :id) visited)
                      (puthash (plist-get e :id) t visited)
                      (bp/agent-sessions--insert-session e depth nested)
                      (dolist (c (reverse (gethash (plist-get e :id) children)))
                        (emit c (1+ depth) t)))))
        (dolist (r roots) (emit r 0 nil))
        ;; Safety net: any entry not reached from a root (e.g. a mutual cycle)
        ;; still renders, at top level.
        (dolist (e entries) (emit e 0 nil))))))

(defun bp/agent-sessions--worktrees-for (root session-groups)
  "Return the ordered worktree plists to render for a repo.
SESSION-GROUPS is an alist of (CANONICAL-PATH . ENTRIES).  When
`bp/agent-sessions-show-all-worktrees' is set and ROOT is known, every
git worktree is listed (in git order), merging in any sessions; otherwise
only the worktrees that actually have sessions are returned.
Each element is (:label L :path P :entries ENTRIES)."
  (let (result seen)
    (when (and bp/agent-sessions-show-all-worktrees root)
      (dolist (wt (bp/agent-sessions--all-worktrees root))
        (let* ((path (plist-get wt :path))
               (canon (bp/agent-sessions--canonical-path path)))
          (push canon seen)
          (push (list :label (plist-get wt :label)
                      :path path
                      :entries (cdr (assoc canon session-groups)))
                result))))
    (dolist (sg session-groups)
      (unless (member (car sg) seen)
        (let ((session (plist-get (car (cdr sg)) :session)))
          (push (list :label (or (plist-get session :worktree) "?")
                      ;; Fall back to the live buffer's directory so `+' works
                      ;; even for sessions registered before :worktree-path
                      ;; was tracked (stale registry entries).
                      :path (or (plist-get session :worktree-path)
                                (let ((buf (plist-get session :buffer)))
                                  (and (buffer-live-p buf)
                                       (buffer-local-value 'default-directory buf))))
                      :entries (cdr sg))
                result))))
    (nreverse result)))

(defun bp/agent-sessions--entry-time (entry)
  "The :updated-at time of ENTRY's session, or nil."
  (plist-get (plist-get entry :session) :updated-at))

(defun bp/agent-sessions--entries-time (entries)
  "The most recent :updated-at among ENTRIES, or nil when none have one."
  (let (best)
    (dolist (e entries best)
      (let ((tm (bp/agent-sessions--entry-time e)))
        (when (and tm (or (null best) (time-less-p best tm)))
          (setq best tm))))))

(defun bp/agent-sessions--by-recency (time-fn items)
  "Return ITEMS sorted most-recent-first by (TIME-FN item).
Items whose TIME-FN returns nil (no activity, e.g. empty worktrees) sort last."
  (sort (copy-sequence items)
        (lambda (a b)
          (let ((ta (funcall time-fn a))
                (tb (funcall time-fn b)))
            (cond ((and ta tb) (time-less-p tb ta))
                  (ta t)
                  (t nil))))))

(defun bp/agent-sessions--sort-tree (repos)
  "Sort REPOS, each repo's worktrees, and each worktree's sessions by recency."
  (bp/agent-sessions--by-recency
   (lambda (repo)
     (bp/agent-sessions--entries-time
      (mapcan (lambda (wt) (copy-sequence (plist-get wt :entries)))
              (plist-get repo :worktrees))))
   (mapcar
    (lambda (repo)
      (plist-put
       repo :worktrees
       (bp/agent-sessions--by-recency
        (lambda (wt) (bp/agent-sessions--entries-time (plist-get wt :entries)))
        (mapcar
         (lambda (wt)
           (plist-put wt :entries
                      (bp/agent-sessions--by-recency
                       #'bp/agent-sessions--entry-time
                       (plist-get wt :entries))))
         (plist-get repo :worktrees)))))
    repos)))

(defconst bp/agent-sessions--status-rank
  '((error . 0) (needs-attention . 1) (running . 2))
  "Sort rank per status; anything unlisted (stopped, idle, nil) ranks last.
Only these coarse buckets participate in ordering: a status change is a rare,
meaningful event (a session started waiting on you), so a row moving between
buckets carries information, whereas a row moving because a tool ran does not.")

(defun bp/agent-sessions--entry-rank (entry)
  (or (alist-get (plist-get (plist-get entry :session) :status)
                 bp/agent-sessions--status-rank)
      (length bp/agent-sessions--status-rank)))

(defun bp/agent-sessions--entry-created (entry)
  "ENTRY's buffer creation time, or nil for a buffer registered before the
`bp/agent-session--created-at' stamp existed."
  (let ((buf (plist-get (plist-get entry :session) :buffer)))
    (and (buffer-live-p buf)
         (buffer-local-value 'bp/agent-session--created-at buf))))

(defun bp/agent-sessions--session-buffer-name (session)
  (let ((buf (plist-get session :buffer)))
    (or (and (buffer-live-p buf) (buffer-name buf)) "")))

(defun bp/agent-sessions--entry-buffer-name (entry)
  (bp/agent-sessions--session-buffer-name (plist-get entry :session)))

(defun bp/agent-sessions--sort-entries (entries)
  "Sort ENTRIES by attention bucket, then oldest-first, then buffer name.
Every key is stable under ordinary activity: unlike `:updated-at', none of
them changes when a session merely runs a tool, so two busy sessions keep
their positions instead of trading places on each hook event.  The buffer name
is the final tie-break so the order never depends on hash-table iteration
order (e.g. for buffers with no creation stamp)."
  (sort (copy-sequence entries)
        (lambda (a b)
          (let ((ra (bp/agent-sessions--entry-rank a))
                (rb (bp/agent-sessions--entry-rank b)))
            (if (/= ra rb)
                (< ra rb)
              (let ((ca (bp/agent-sessions--entry-created a))
                    (cb (bp/agent-sessions--entry-created b)))
                (cond ((and ca cb (not (time-equal-p ca cb))) (time-less-p ca cb))
                      ;; Unstamped buffers sort after stamped ones.
                      ((and ca (not cb)) t)
                      ((and cb (not ca)) nil)
                      (t (string< (bp/agent-sessions--entry-buffer-name a)
                                  (bp/agent-sessions--entry-buffer-name b))))))))))

;;; Persisted user intent ---------------------------------------------------
;;
;; Everything else in this file is rebuilt from live buffers, but the manual
;; order (below) and the notes (further down) record decisions the *user* made,
;; not observed session state, so they are written to disk (see CLAUDE.md).
;; All three live in one SQLite file: the notes, the manual order, and the
;; session log that `R' and `bp/agent-sessions-restore-previous' read.  The log
;; is what forced a database — it is append-heavy, queried by recency, and
;; grows without bound, so the old rewrite-the-whole-file-per-change approach
;; was never going to fit it.  Notes and order moved in alongside so there is
;; exactly one persistent thing to back up, inspect, or delete.
;;
;; Both of those are still cached in a hash table and read from it: the render
;; path must never touch the database.  The database is read once on first use
;; and written only when something actually changes.

(defcustom bp/agent-sessions-db-file
  (expand-file-name "agent-sessions.db" user-emacs-directory)
  "SQLite file holding the notes, the manual order, and the session log."
  :type 'file)

(defconst bp/agent-sessions--schema-version 1)

(defvar bp/agent-sessions--db nil
  "Open SQLite handle, or nil before the database is first used.")

(defvar bp/agent-sessions--heartbeat-timer nil
  "Timer stamping `meta.last_seen'; see `bp/agent-sessions--db-heartbeat'.")

(defconst bp/agent-sessions--db-schema
  '("CREATE TABLE IF NOT EXISTS meta (
       key   TEXT PRIMARY KEY,
       value TEXT NOT NULL)"

    ;; One row per agent session — not per terminal.  A terminal you run
    ;; `claude' in twice produces two rows, so either can be restored.
    "CREATE TABLE IF NOT EXISTS sessions (
       id           INTEGER PRIMARY KEY,
       emacs_sid    TEXT    NOT NULL,
       agent_type   TEXT,
       agent_sid    TEXT,
       worktree     TEXT    NOT NULL,
       repo         TEXT,
       label        TEXT,
       title        TEXT,
       opened_at    INTEGER NOT NULL,
       closed_at    INTEGER,
       close_reason TEXT)"
    "CREATE INDEX IF NOT EXISTS sessions_recent ON sessions (closed_at DESC)"
    "CREATE INDEX IF NOT EXISTS sessions_open
       ON sessions (emacs_sid) WHERE closed_at IS NULL"
    "CREATE INDEX IF NOT EXISTS sessions_agent ON sessions (agent_sid)"

    "CREATE TABLE IF NOT EXISTS notes (
       kind       TEXT NOT NULL,
       key        TEXT NOT NULL,
       note       TEXT NOT NULL,
       updated_at INTEGER NOT NULL,
       PRIMARY KEY (kind, key))"

    ;; The scope columns default to '' rather than NULL on purpose: SQLite
    ;; treats NULLs as distinct from each other in a primary key, so a NULL
    ;; scope would make every write insert a duplicate instead of replacing.
    "CREATE TABLE IF NOT EXISTS order_entries (
       kind           TEXT    NOT NULL,
       scope_repo     TEXT    NOT NULL DEFAULT '',
       scope_worktree TEXT    NOT NULL DEFAULT '',
       member         TEXT    NOT NULL,
       position       INTEGER NOT NULL,
       PRIMARY KEY (kind, scope_repo, scope_worktree, member))"
    "CREATE INDEX IF NOT EXISTS order_group
       ON order_entries (kind, scope_repo, scope_worktree, position)"

    ;; Files an agent handed the user to look at, keyed by the same identity a
    ;; session's note uses.  Two timestamps, not one: `shared_at' is the first
    ;; time a path was shared and is what orders the list, while `updated_at'
    ;; is the latest, so an agent re-sharing a changed file can light its row
    ;; up again without the row moving out from under the cursor.
    ;; Mail between agents.  Filed under the same durable identity a note or a
    ;; shared file uses, never under the terminal id it was addressed to at
    ;; runtime: terminal ids carry this Emacs's pid and die with it, while the
    ;; agent session id is what `R' resumes, so mail filed under it comes back
    ;; attached to the conversation it belongs to.  Read messages are kept
    ;; rather than deleted, the way `sessions' keeps closed rows: what the
    ;; workers reported is the record of the run.
    "CREATE TABLE IF NOT EXISTS mail (
       id         TEXT PRIMARY KEY,
       from_kind  TEXT NOT NULL,
       from_key   TEXT NOT NULL,
       from_label TEXT,
       to_kind    TEXT NOT NULL,
       to_key     TEXT NOT NULL,
       type       TEXT NOT NULL,
       subject    TEXT NOT NULL,
       body       TEXT,
       files      TEXT,
       report     TEXT,
       sent_at    INTEGER NOT NULL,
       read_at    INTEGER)"
    "CREATE INDEX IF NOT EXISTS mail_inbox ON mail (to_kind, to_key, read_at)"

    "CREATE TABLE IF NOT EXISTS shared_files (
       kind       TEXT    NOT NULL,
       key        TEXT    NOT NULL,
       path       TEXT    NOT NULL,
       label      TEXT,
       shared_at  INTEGER NOT NULL,
       updated_at INTEGER NOT NULL,
       visited_at INTEGER,
       PRIMARY KEY (kind, key, path))")
  "Statements that bring an empty or existing database up to date.
All are `IF NOT EXISTS', so running them on every open is the whole migration
story for as long as the schema only ever grows.  `bp/agent-sessions--schema-version'
is recorded in `meta' so a future change that *rewrites* data can tell how old
a file is.")

(defun bp/agent-sessions--now () (truncate (float-time)))

(defun bp/agent-sessions--db ()
  "Return the open database handle, creating the file on first use.
Opening also reaps (see `bp/agent-sessions--db-reap'), which is why every
caller goes through here rather than touching `bp/agent-sessions--db'."
  (if (sqlitep bp/agent-sessions--db)
      bp/agent-sessions--db
    (let ((db (sqlite-open bp/agent-sessions-db-file)))
      ;; WAL so a second Emacs reading the file can't block this one; the
      ;; timeout so that if one does write, we wait rather than signalling.
      (sqlite-execute db "PRAGMA journal_mode=WAL")
      (sqlite-execute db "PRAGMA busy_timeout=3000")
      (with-sqlite-transaction db
        (dolist (stmt bp/agent-sessions--db-schema)
          (sqlite-execute db stmt))
        (sqlite-execute db "INSERT OR IGNORE INTO meta VALUES ('schema_version', ?)"
                        (list (number-to-string bp/agent-sessions--schema-version))))
      (setq bp/agent-sessions--db db)
      (bp/agent-sessions--db-reap db)
      db)))

;; Re-run the schema against an already-open handle, so *loading* this file
;; picks up a schema addition.  `--db' migrates only when it opens the file,
;; and the handle is cached for the life of the Emacs — so without this the
;; documented way to develop this package (reload into the running server)
;; leaves the new code querying a table the old connection never created,
;; which is a flood of "no such table" rather than a clean failure.  Every
;; statement is `IF NOT EXISTS', so re-running them costs nothing.
(when (sqlitep bp/agent-sessions--db)
  (with-sqlite-transaction bp/agent-sessions--db
    (dolist (stmt bp/agent-sessions--db-schema)
      (sqlite-execute bp/agent-sessions--db stmt))))

(defun bp/agent-sessions--meta-get (db key)
  (caar (sqlite-select db "SELECT value FROM meta WHERE key = ?" (list key))))

(defun bp/agent-sessions--meta-set (db key value)
  (sqlite-execute db "INSERT INTO meta VALUES (?, ?)
                      ON CONFLICT(key) DO UPDATE SET value = excluded.value"
                  (list key (format "%s" value))))

(defun bp/agent-sessions--db-reap (db)
  "Close session rows that a previous Emacs left open by dying.
This is the whole crash-detection scheme, and it works because there is only
ever one Emacs: a freshly started one has no sessions of its own yet, so any
row still marked open must belong to an Emacs that never ran its
`kill-emacs-hook'.  The heartbeat in `meta' dates the death; without one we
fall back to when the session opened."
  (let ((seen (bp/agent-sessions--meta-get db "last_seen")))
    (sqlite-execute db
                    "UPDATE sessions
                        SET closed_at = COALESCE(?, opened_at),
                            close_reason = 'crash'
                      WHERE closed_at IS NULL"
                    (list (and seen (string-to-number seen))))))

(defun bp/agent-sessions--db-heartbeat ()
  "Record that Emacs was alive just now, so a crash can be dated.
Deliberately a no-op until the database has been opened for some other
reason — an Emacs with no agent sessions has nothing to date."
  (when (sqlitep bp/agent-sessions--db)
    (bp/agent-sessions--meta-set bp/agent-sessions--db
                                 "last_seen" (bp/agent-sessions--now))))

;;; Session log -------------------------------------------------------------
;;
;; A history of every agent session that has run, so one can be brought back
;; after it is gone: `R' for one you ended yourself, and
;; `bp/agent-sessions-restore-previous' for the lot that went down with Emacs.
;;
;; This is the one thing here that records *observed session state* rather than
;; something the user chose, which the ephemerality rule in CLAUDE.md otherwise
;; forbids.  It earns the exception by never feeding the dashboard: rows still
;; come only from live buffers, so nothing here can make a dead session look
;; alive.  It is a record of what happened, including the dying.
;;
;; How a session's ending is classified, and why it can be:
;;
;;   killed      `kill-buffer-hook' fired — `k' in the dashboard, `C-x k' on
;;               the terminal, or the shell exiting under it.
;;   agent-exit  the agent reported `SessionEnd' but the terminal lives on.
;;   superseded  a different agent session id appeared in the same terminal,
;;               so the previous one had ended without us hearing.
;;   emacs-exit  `kill-emacs-hook' fired: Emacs quit with this still running.
;;   crash       no hook fired at all; found still-open at the next startup.
;;
;; The first three mean "you ended it" and are what `R' offers; the last two
;; mean "it was taken from you" and are what restore-previous offers.  Emacs
;; runs `kill-emacs-hook' but *not* `kill-buffer-hook' when it exits, and that
;; asymmetry is the only reason the two can be told apart at all.

(defvar-local bp/agent-session--log-row nil
  "Row id in the `sessions' table for this terminal's current agent session.
On the buffer rather than looked up, because a terminal can run several agent
sessions in turn and each gets its own row.")

(defvar-local bp/agent-session--log-state nil
  "Last (AGENT-SID AGENT-TYPE TITLE) written for `bp/agent-session--log-row'.
Compared before every write so a busy session touches the database once per
agent session instead of once per hook event.  The dashboard re-renders on
every event; the database must stay off that path.")

(defvar-local bp/agent-session--log-ended-sid nil
  "Agent session id this terminal has already logged the end of.
Hook events have no delivery-order guarantee, so an informational event can
land after `SessionEnd'.  Without this, such a straggler would open a fresh
row and resurrect a session that has already finished.")

(defun bp/agent-sessions--log-open (buf)
  "Insert a session-log row for BUF and remember its id on the buffer."
  (with-current-buffer buf
    (let* ((info (bp/agent-sessions--buffer-repo-info buf))
           (db (bp/agent-sessions--db)))
      (sqlite-execute
       db "INSERT INTO sessions (emacs_sid, worktree, repo, label, opened_at)
           VALUES (?, ?, ?, ?, ?)"
       (list bp/agent-session-id
             (or (plist-get info :worktree-path)
                 (bp/agent-sessions--canonical-path default-directory))
             (plist-get info :repo-root)
             (plist-get info :worktree)
             (bp/agent-sessions--now)))
      (setq-local bp/agent-session--log-state nil)
      (setq-local bp/agent-session--log-row
                  (caar (sqlite-select db "SELECT last_insert_rowid()"))))))

(defun bp/agent-sessions--log-close-row (row reason)
  "Close session-log ROW with REASON, if it is still open."
  (sqlite-execute (bp/agent-sessions--db)
                  "UPDATE sessions SET closed_at = ?, close_reason = ?
                    WHERE id = ? AND closed_at IS NULL"
                  (list (bp/agent-sessions--now) reason row)))

(defun bp/agent-sessions--log-supersede (row)
  "Retire ROW, whose terminal has started a *different* agent session.
Usually that means the previous agent exited without us hearing, and the row
is closed.  The exception is the fork transient: at `SessionStart' a
`--fork-session' child reports its parent's id before minting its own, so ROW
may be a copy of a session still open in another terminal.  Such a row is an
artifact of the fork rather than a session that ever ran here, so it is
deleted instead of being offered by `R' as a resumable duplicate.

Note this reads the *log*, never the fork-detection state — see CLAUDE.md on
why a `sid == parent-sid' test must not go anywhere near that logic."
  (let ((db (bp/agent-sessions--db)))
    (if (sqlite-select db
                       "SELECT 1 FROM sessions a
                         WHERE a.id = ?
                           AND EXISTS (SELECT 1 FROM sessions b
                                        WHERE b.agent_sid = a.agent_sid
                                          AND b.id <> a.id
                                          AND b.closed_at IS NULL)"
                       (list row))
        (sqlite-execute db "DELETE FROM sessions WHERE id = ?" (list row))
      (bp/agent-sessions--log-close-row row "superseded"))))

(defun bp/agent-sessions--log-update (buf agent-type agent-sid)
  "Bring BUF's session-log row in line with the hook event just processed."
  (with-current-buffer buf
    (let* ((title (or bp/agent-session-title-override bp/agent-session-title))
           (state (list agent-sid agent-type title)))
      (unless (or (equal state bp/agent-session--log-state)
                  (and agent-sid
                       (equal agent-sid bp/agent-session--log-ended-sid)))
        (let ((prev-sid (car bp/agent-session--log-state)))
          (when (and bp/agent-session--log-row prev-sid agent-sid
                     (not (equal prev-sid agent-sid)))
            (bp/agent-sessions--log-supersede bp/agent-session--log-row)
            (setq-local bp/agent-session--log-row nil))
          (unless bp/agent-session--log-row
            (bp/agent-sessions--log-open buf))
          (sqlite-execute (bp/agent-sessions--db)
                          "UPDATE sessions
                              SET agent_type = ?, agent_sid = ?, title = ?
                            WHERE id = ?"
                          (list agent-type agent-sid title
                                bp/agent-session--log-row))
          (setq-local bp/agent-session--log-state state))))))

(defun bp/agent-sessions--log-end (buf)
  "Close BUF's current log row: its agent reported that it is exiting.
The terminal itself lives on, and running an agent in it again opens a fresh
row rather than reviving this one."
  (with-current-buffer buf
    (when bp/agent-session--log-row
      (bp/agent-sessions--log-close-row bp/agent-session--log-row "agent-exit")
      (setq-local bp/agent-session--log-ended-sid
                  (car bp/agent-session--log-state))
      (setq-local bp/agent-session--log-row nil)
      (setq-local bp/agent-session--log-state nil))))

(defun bp/agent-sessions--log-close-buffer ()
  "Close the log rows of the terminal being killed, from `kill-buffer-hook'.
Covers both `k' in the dashboard (which is a plain `kill-buffer') and `C-x k'
on the terminal.  Does nothing when the database was never opened, which means
this Emacs logged no sessions and so has none to close."
  (when (and bp/agent-session-id (sqlitep bp/agent-sessions--db))
    (sqlite-execute bp/agent-sessions--db
                    "UPDATE sessions SET closed_at = ?, close_reason = 'killed'
                      WHERE emacs_sid = ? AND closed_at IS NULL"
                    (list (bp/agent-sessions--now) bp/agent-session-id))))

(defun bp/agent-sessions--log-close-all ()
  "Mark every still-open session as ended by Emacs exiting.
Runs from `kill-emacs-hook'.  Also stamps the heartbeat, so that if this Emacs
is *killed* mid-exit the reap still has a recent time to date the crash from."
  (when (sqlitep bp/agent-sessions--db)
    (bp/agent-sessions--db-heartbeat)
    (sqlite-execute bp/agent-sessions--db
                    "UPDATE sessions
                        SET closed_at = ?, close_reason = 'emacs-exit'
                      WHERE closed_at IS NULL"
                    (list (bp/agent-sessions--now)))))

;;; Manual order ------------------------------------------------------------
;;
;; `M-n' / `M-p' nudge the thing at point within its group, overriding the
;; automatic order of `--sort-tree-stable' for that group only.

(defvar bp/agent-sessions--order nil
  "Hash table SCOPE -> ordered list of item keys, or nil before first load.
One scope per group that can be reordered: the repo list, one per repo's
worktrees, one per worktree's sessions.  A scope is a list — (\"worktrees\"
REPO-KEY) and friends — rather than a joined string, so no separator character
can ever appear inside a path component and blur two scopes together.

Item keys are stable identities rather than display labels, which change with
a branch checkout: a canonical repo root, a canonical worktree path, and — for
a session — its buffer name, the only session-level identity that can outlive
a restart.  Backed by the `order_entries' table.")

(defconst bp/agent-sessions--repos-scope '("repos"))

(defun bp/agent-sessions--worktrees-scope (repo-key)
  (list "worktrees" repo-key))

(defun bp/agent-sessions--sessions-scope (repo-key worktree-key)
  (list "sessions" repo-key worktree-key))

(defun bp/agent-sessions--repo-key (repo)
  "Manual-order key for REPO: its canonical root, else its display name."
  (or (bp/agent-sessions--canonical-path (plist-get repo :root))
      (plist-get repo :name)
      "?"))

(defun bp/agent-sessions--worktree-key (wt)
  "Manual-order key for worktree WT: its canonical path, else its label.
Only ever compared within one repo's scope, so the label fallback can't
collide with a like-named worktree of another repo."
  (or (bp/agent-sessions--canonical-path (plist-get wt :path))
      (plist-get wt :label)
      "?"))

(defun bp/agent-sessions--order-scope-row (scope)
  "Return (KIND SCOPE-REPO SCOPE-WORKTREE) for SCOPE, for use as query values.
Absent scope components become the empty string, never NULL — see the
`order_entries' schema for why."
  (list (car scope) (or (nth 1 scope) "") (or (nth 2 scope) "")))

(defun bp/agent-sessions--order-table ()
  "Return the manual-order table, loading it from the database once.
An empty table just means the user hasn't reordered anything."
  (or bp/agent-sessions--order
      (setq bp/agent-sessions--order
            (let ((table (make-hash-table :test 'equal)))
              (pcase-dolist (`(,kind ,repo ,wt ,member)
                             (sqlite-select
                              (bp/agent-sessions--db)
                              "SELECT kind, scope_repo, scope_worktree, member
                                 FROM order_entries ORDER BY position"))
                (let ((scope (cond ((string-empty-p repo) (list kind))
                                   ((string-empty-p wt) (list kind repo))
                                   (t (list kind repo wt)))))
                  (puthash scope
                           (append (gethash scope table) (list member))
                           table)))
              table))))

(defun bp/agent-sessions--order-write-group (scope old new)
  "Persist NEW as SCOPE's member order, writing only the rows that moved.
`M-n' / `M-p' transpose one item with its neighbour, and
`bp/agent-sessions--merge-order' turns that into a NEW list differing from OLD
in exactly two slots — so this normally issues exactly two UPDATEs no matter
how large the group is, and touches no other group at all.

A member NEW has but OLD doesn't is one being placed for the first time (it
had been keeping the automatic order), and is inserted at its new slot; one
OLD has but NEW doesn't is gone and is deleted."
  (let ((row (bp/agent-sessions--order-scope-row scope))
        (db (bp/agent-sessions--db))
        (i 0))
    (with-sqlite-transaction db
      (dolist (member new)
        (unless (eql (seq-position old member) i)
          (sqlite-execute
           db "INSERT INTO order_entries
                 (kind, scope_repo, scope_worktree, member, position)
               VALUES (?, ?, ?, ?, ?)
               ON CONFLICT(kind, scope_repo, scope_worktree, member)
               DO UPDATE SET position = excluded.position"
           (append row (list member i))))
        (setq i (1+ i)))
      (dolist (member old)
        (unless (member member new)
          (sqlite-execute
           db "DELETE FROM order_entries
                WHERE kind = ? AND scope_repo = ? AND scope_worktree = ?
                  AND member = ?"
           (append row (list member))))))))

(defun bp/agent-sessions--order-delete-group (scope)
  "Forget SCOPE's manual order entirely."
  (sqlite-execute (bp/agent-sessions--db)
                  "DELETE FROM order_entries
                    WHERE kind = ? AND scope_repo = ? AND scope_worktree = ?"
                  (bp/agent-sessions--order-scope-row scope)))

(defun bp/agent-sessions--order-delete-all ()
  "Forget every group's manual order."
  (sqlite-execute (bp/agent-sessions--db) "DELETE FROM order_entries"))

(defun bp/agent-sessions--apply-manual-order (scope key-fn items)
  "Return ITEMS reordered to match the manual order recorded for SCOPE.
KEY-FN maps an item to its manual-order key.  Items with no recorded key keep
their incoming (automatic) order and follow the ones that have, so a session
or worktree that appears after a reorder lands at the end of its group rather
than displacing anything the user placed deliberately."
  (let ((order (gethash scope (bp/agent-sessions--order-table))))
    (if (null order)
        items
      (let (ranked rest)
        (dolist (item items)
          (let ((pos (seq-position order (funcall key-fn item))))
            (if pos (push (cons pos item) ranked) (push item rest))))
        (append (mapcar #'cdr (sort (nreverse ranked)
                                    (lambda (a b) (< (car a) (car b)))))
                (nreverse rest))))))

(defun bp/agent-sessions--sort-tree-stable (repos)
  "Order REPOS alphabetically, their worktrees alphabetically, rows by attention,
then apply any manual order (`M-n' / `M-p') on top at each level.
The automatic tree skeleton is deliberately *not* ordered by activity or
attention: a fixed repo/worktree layout is what makes the dashboard navigable
from memory.  Urgency surfaces within a worktree instead, via `--sort-entries'.
A manually placed group overrides that for the items the user positioned; the
rest keep the automatic order after them."
  (bp/agent-sessions--apply-manual-order
   bp/agent-sessions--repos-scope
   #'bp/agent-sessions--repo-key
   (sort (mapcar
          (lambda (repo)
            (let ((rkey (bp/agent-sessions--repo-key repo)))
              (plist-put
               repo :worktrees
               (bp/agent-sessions--apply-manual-order
                (bp/agent-sessions--worktrees-scope rkey)
                #'bp/agent-sessions--worktree-key
                (sort (mapcar
                       (lambda (wt)
                         (plist-put
                          wt :entries
                          (bp/agent-sessions--apply-manual-order
                           (bp/agent-sessions--sessions-scope
                            rkey (bp/agent-sessions--worktree-key wt))
                           #'bp/agent-sessions--entry-buffer-name
                           (bp/agent-sessions--sort-entries
                            (plist-get wt :entries)))))
                       (plist-get repo :worktrees))
                      (lambda (a b) (string< (or (plist-get a :label) "")
                                             (or (plist-get b :label) ""))))))))
          repos)
         (lambda (a b) (string< (or (plist-get a :name) "")
                                (or (plist-get b :name) ""))))))

;;; Notes -------------------------------------------------------------------
;;
;; `e' attaches a free-form, multi-line note to the repo, worktree, or session
;; at point; it renders under that thing's heading.  Persisted for the same
;; reason as the manual order: it is something the user wrote, not session
;; state, so it has to outlive both the session and Emacs.

(defvar bp/agent-sessions--notes nil
  "Hash table KEY -> note string, or nil before the notes are first read.
A KEY is (\"repo\" REPO-KEY), (\"worktree\" WORKTREE-KEY) or (\"session\" ID),
reusing the stable identities of the manual order.  The type tag is not
decoration: a repo's *main* worktree has the same canonical path as the repo
itself, so an untagged key would make those two share one note.

For a session, ID is the agent's own session id when it has one — that
survives renaming the terminal buffer — and the buffer name otherwise, which is
all a plain terminal has to be identified by.  See
`bp/agent-sessions--session-note-keys'.")

(defun bp/agent-sessions--notes-table ()
  "Return the notes table, loading it from the database once."
  (or bp/agent-sessions--notes
      (setq bp/agent-sessions--notes
            (let ((table (make-hash-table :test 'equal)))
              (pcase-dolist (`(,kind ,key ,note)
                             (sqlite-select (bp/agent-sessions--db)
                                            "SELECT kind, key, note FROM notes"))
                (puthash (list kind key) note table))
              table))))

(defun bp/agent-sessions--repo-note-key (repo)
  "Note key for REPO, a plist with :root/:name (a repo section's value)."
  (list "repo" (bp/agent-sessions--repo-key repo)))

(defun bp/agent-sessions--worktree-note-key (wt)
  "Note key for worktree WT, a plist with :path/:label."
  (list "worktree" (bp/agent-sessions--worktree-key wt)))

(defun bp/agent-sessions--session-note-keys (session)
  "Note keys to try for SESSION, most durable identity first.
An agent session is keyed by the agent's own session id, so renaming its
terminal buffer keeps the note; a plain terminal has no such id and falls back
to its buffer name.  Both keys are consulted when reading, so a note written on
a bare terminal is still found once `claude'/`codex' starts in it and the
session gains an id — at which point `bp/agent-sessions--note-set' moves it to
the id key."
  (let ((sid (plist-get session :agent-session-id))
        (name (bp/agent-sessions--session-buffer-name session)))
    (delq nil (list (and sid (list "session" sid))
                    (and (not (string-empty-p name)) (list "session" name))))))

(defun bp/agent-sessions--note-find (keys)
  "Return (KEY . NOTE) for the first of KEYS carrying a note, or nil."
  (let ((table (bp/agent-sessions--notes-table)))
    (seq-some (lambda (key)
                (let ((note (gethash key table)))
                  (and (stringp note) (not (string-empty-p note))
                       (cons key note))))
              keys)))

(defun bp/agent-sessions--note-set (keys note)
  "Store NOTE under the first of KEYS and save; an empty NOTE removes it.
Notes under the remaining (less durable) KEYS are dropped, so a session that
gains an agent session id ends up with its note under that id alone rather than
one note per identity it has had."
  (let ((table (bp/agent-sessions--notes-table))
        (db (bp/agent-sessions--db)))
    (with-sqlite-transaction db
      (dolist (key (cdr keys))
        (remhash key table)
        (sqlite-execute db "DELETE FROM notes WHERE kind = ? AND key = ?" key))
      (if (and note (not (string-empty-p note)))
          (progn
            (puthash (car keys) note table)
            (sqlite-execute db "INSERT INTO notes VALUES (?, ?, ?, ?)
                                ON CONFLICT(kind, key) DO UPDATE
                                  SET note = excluded.note,
                                      updated_at = excluded.updated_at"
                            (append (car keys)
                                    (list note (bp/agent-sessions--now)))))
        (remhash (car keys) table)
        (sqlite-execute db "DELETE FROM notes WHERE kind = ? AND key = ?"
                        (car keys))))))

(defface bp/agent-session-note
  '((t :inherit font-lock-comment-face))
  "Face for a note attached to a repo, worktree, or session row.")

(defun bp/agent-sessions--insert-note (keys indent)
  "Insert the note for KEYS, if any, as a section indented by INDENT.
The note's first line is the section heading and the rest its body, so TAB
collapses a long note to one line and expands it again.  Fold state survives
the dashboard's re-render because magit caches it per section identity, and a
note's identity is its key — not its position or text."
  (when-let ((found (bp/agent-sessions--note-find keys)))
    (let ((lines (split-string (cdr found) "\n")))
      (magit-insert-section (bp/agent-session-note (car found))
        (magit-insert-heading
          (propertize (concat indent "· " (car lines))
                      'font-lock-face 'bp/agent-session-note))
        (dolist (line (cdr lines))
          (insert (propertize (concat indent "  " line "\n")
                              'font-lock-face 'bp/agent-session-note)))))))

(defun bp/agent-sessions--note-target-at-point ()
  "Return (KEYS . LABEL) for the note-bearing thing at point, or nil.
Point may be on a repo heading, a worktree heading or a session row — or inside
a note already rendered under one of those, which resolves to its parent so `e'
re-edits the note point is sitting in."
  (let ((section (magit-current-section)))
    (when (and section (eq (oref section type) 'bp/agent-session-note))
      (setq section (oref section parent)))
    (pcase (and section (oref section type))
      ('bp/agent-session-repo
       (let ((value (oref section value)))
         (cons (list (bp/agent-sessions--repo-note-key value))
               (or (plist-get value :name) "repo"))))
      ('bp/agent-session-worktree
       (let ((value (oref section value)))
         (cons (list (bp/agent-sessions--worktree-note-key value))
               (or (plist-get value :label) "worktree"))))
      ('bp/agent-session-file
       (let ((path (oref section value)))
         (cons (list (bp/agent-sessions--file-note-key path))
               (file-name-nondirectory (directory-file-name path)))))
      ('bp/agent-session-row
       (when-let* ((session (bp/agent-sessions--session-for-id
                             (oref section value)))
                   ;; No id and no buffer name means nothing durable to hang a
                   ;; note on (a registry entry whose buffer is already gone).
                   (keys (bp/agent-sessions--session-note-keys session)))
         (cons keys (bp/agent-sessions--session-short-label session)))))))

(defun bp/agent-sessions-note-newline ()
  "Insert a line break into the note being read in the minibuffer."
  (interactive)
  (insert "\n"))

(defvar bp/agent-sessions-note-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-j") #'bp/agent-sessions-note-newline)
    map)
  "Keymap used while reading a note with `bp/agent-sessions-edit-note'.
Notes are multi-line, but `minibuffer-local-map' binds \\`C-j' to
`exit-minibuffer' just like \\`RET', so it has to be rebound here; \\`RET'
still accepts the note.")

(defun bp/agent-sessions-edit-note (&optional remove)
  "Attach or edit a note on the repo, worktree, or session at point.
The note is free-form text shown under that thing's heading, foldable with TAB,
and persisted in `bp/agent-sessions-db-file'.  Insert a line break with
\\<bp/agent-sessions-note-minibuffer-map>\\[bp/agent-sessions-note-newline] and
accept with \\[exit-minibuffer]; an empty note removes it.  With a prefix
argument (REMOVE), delete the note without prompting — emptying a many-line
note by hand is tedious, since \\[kill-line] only kills the line point is on."
  (interactive "P")
  (pcase (bp/agent-sessions--note-target-at-point)
    (`(,keys . ,label)
     (let* ((existing (cdr (bp/agent-sessions--note-find keys)))
            (note (if remove
                      ""
                    (string-trim
                     (read-from-minibuffer
                      (format "Note for %s (C-j for newline): " label)
                      existing bp/agent-sessions-note-minibuffer-map)))))
       (bp/agent-sessions--note-set keys note)
       (bp/agent-sessions--refresh)
       (message (if (string-empty-p note)
                    "Note cleared for %s."
                  "Note saved for %s.")
                label)))
    (_ (message "Point is not on a repo, worktree, or session."))))

;;; Shared files ------------------------------------------------------------
;;
;; An agent that wants the user to read something runs `emacs-share-file FILE
;; ["why"]' (bin/emacs-share-file, on the PATH of every session terminal); the
;; file then hangs under that agent's row in the dashboard, highlighted until
;; the user opens it.  It exists because a terminal is a bad inbox: "look at
;; /tmp/plan.md" scrolls away, and is only seen at all if you are already in
;; that buffer.
;;
;; This is persisted for the same reason notes and the manual order are — it is
;; something someone *chose* to put there, not observed session state — and it
;; obeys the same rules: loaded into a hash table once, written on change, and
;; never queried from the render path.
;;
;; Two design points worth keeping:
;;
;; - **A file is attached to the session's most durable identity, not to its
;;   terminal.**  It reuses `bp/agent-sessions--session-note-keys' verbatim: the
;;   agent's own session id when it has one, the buffer name otherwise, reading
;;   both and collapsing onto the first on write.  So renaming a terminal keeps
;;   its files, a file shared into a bare shell survives `claude' starting in
;;   it, and a session brought back with `R' comes back with its files.  The
;;   caller passes the *terminal* id (that is what the shell env has), and this
;;   layer resolves it — the same split the hook path makes.
;;
;; - **Re-sharing a path means "I changed it, look again", not "add it twice".**
;;   The row's `visited_at' is cleared so it lights up exactly as it did the
;;   first time, but its `shared_at' — and therefore its position — is left
;;   alone.  That follows the ordering rule the dashboard already lives by: a
;;   row may move when something meaningful changes, and a re-share is not that;
;;   a row jumping to the bottom of the list is the thing the user is trying to
;;   aim at moving out from under them.

(defvar bp/agent-sessions--shared-files nil
  "Hash table KEY -> list of shared-file plists, or nil before first load.
A KEY is (\"session\" ID), exactly as in `bp/agent-sessions--notes'.  Each
plist is (:path P :label L :shared-at TS :updated-at TS :visited-at TS-or-nil);
`:visited-at' nil is what makes a row render highlighted.  Backed by the
`shared_files' table.")

(defvar bp/agent-sessions--shared-paths nil
  "Hash set of `file-truename's of every shared file, or nil when it is stale.
Consulted by the focus hook on every buffer switch, so it has to be a set
lookup rather than a scan; `bp/agent-sessions--shared-paths-invalidate' drops
it whenever the file table changes.")

(defun bp/agent-sessions--shared-files-table ()
  "Return the shared-files table, loading it from the database once."
  (or bp/agent-sessions--shared-files
      (setq bp/agent-sessions--shared-files
            (let ((table (make-hash-table :test 'equal)))
              (pcase-dolist (`(,kind ,key ,path ,label ,shared ,updated ,visited)
                             (sqlite-select
                              (bp/agent-sessions--db)
                              "SELECT kind, key, path, label, shared_at,
                                      updated_at, visited_at
                                 FROM shared_files ORDER BY shared_at, path"))
                (let ((k (list kind key)))
                  (puthash k
                           (append (gethash k table)
                                   (list (list :path path :label label
                                               :shared-at shared
                                               :updated-at updated
                                               :visited-at visited)))
                           table)))
              table))))

(defun bp/agent-sessions--shared-paths-invalidate ()
  "Drop the cached truename set; it is rebuilt on the next lookup."
  (setq bp/agent-sessions--shared-paths nil))

(defun bp/agent-sessions--shared-path-p (truename)
  "Non-nil when TRUENAME is the true name of some shared file."
  (and truename
       (gethash truename
                (or bp/agent-sessions--shared-paths
                    (setq bp/agent-sessions--shared-paths
                          (let ((set (make-hash-table :test 'equal)))
                            (maphash
                             (lambda (_key files)
                               (dolist (f files)
                                 (puthash (file-truename (plist-get f :path))
                                          t set)))
                             (bp/agent-sessions--shared-files-table))
                            set))))))

(defun bp/agent-sessions--file-note-key (path)
  "Note key for the shared file at PATH.
Keyed on the path alone, not on path-plus-session: the note is about the file,
and the same file handed to two sessions is still one thing to say something
about."
  (list "file" path))

(defun bp/agent-sessions--files-scope (session)
  "Manual-order scope for SESSION's shared files.
Packed into the existing two scope columns as (\"files\" SESSION-KEY) rather
than needing a third: `kind' already separates it from every other scope, and
adding a column would mean a real migration, which `CREATE TABLE IF NOT
EXISTS' cannot do.  SESSION-KEY is the same durable identity the files
themselves are stored under, so an arrangement survives a terminal rename."
  (list "files" (or (cadr (car (bp/agent-sessions--session-note-keys session)))
                    "?")))

(defun bp/agent-sessions--session-files (session)
  "Files shared with SESSION, in display order.
First-shared first, with any manual order (`M-n' / `M-p') applied on top.
Every identity the session has had is read, not just the current one, so a
file attached before `claude' started in the terminal is not invisible in the
window between that and the next write (which collapses them)."
  (let ((table (bp/agent-sessions--shared-files-table))
        files)
    (dolist (key (bp/agent-sessions--session-note-keys session))
      ;; `append' shares the tail of its last argument, and `sort' is
      ;; destructive — without the copy this would reorder the stored list.
      (setq files (append files (copy-sequence (gethash key table)))))
    (bp/agent-sessions--apply-manual-order
     (bp/agent-sessions--files-scope session)
     (lambda (f) (plist-get f :path))
     (sort files (lambda (a b) (< (plist-get a :shared-at)
                                  (plist-get b :shared-at)))))))

(defun bp/agent-sessions--shared-file-write (key file)
  "Upsert FILE under KEY in the database."
  (sqlite-execute
   (bp/agent-sessions--db)
   "INSERT INTO shared_files
      (kind, key, path, label, shared_at, updated_at, visited_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(kind, key, path) DO UPDATE
      SET label      = excluded.label,
          updated_at = excluded.updated_at,
          visited_at = excluded.visited_at"
   (append key (list (plist-get file :path)
                     (plist-get file :label)
                     (plist-get file :shared-at)
                     (plist-get file :updated-at)
                     (plist-get file :visited-at)))))

(defun bp/agent-sessions--files-collapse (keys)
  "Move file rows stored under the less durable KEYS onto (car KEYS).
The same tidying `bp/agent-sessions--note-set' does, and for the same reason: a
session that gains an agent session id should end up with one file list under
that id rather than one per identity it has held.  A path already present under
the primary key wins, so this can never undo a newer share."
  (let* ((table (bp/agent-sessions--shared-files-table))
         (primary (car keys))
         (db (bp/agent-sessions--db))
         (kept (gethash primary table))
         (moved nil))
    (dolist (key (cdr keys))
      (when-let ((files (gethash key table)))
        (dolist (f files)
          (unless (seq-find (lambda (k) (equal (plist-get k :path)
                                               (plist-get f :path)))
                            kept)
            (push f moved)))
        (remhash key table)
        (sqlite-execute db "DELETE FROM shared_files WHERE kind = ? AND key = ?"
                        key)))
    (when moved
      (setq moved (nreverse moved))
      (puthash primary (append kept moved) table)
      (dolist (f moved)
        (bp/agent-sessions--shared-file-write primary f)))))

(defun bp/agent-sessions--share-file (session path label)
  "Attach PATH to SESSION with LABEL, unvisited.  Return the file plist.
Re-sharing a path already attached updates it in place rather than adding a
second row: the label is refreshed (an empty LABEL keeps the old one, since an
agent re-sharing may have nothing to add beyond \"changed\"), `:visited-at' is
cleared so the row highlights again, and `:shared-at' — the sort key — is left
at the original share so the row does not move."
  (let ((keys (bp/agent-sessions--session-note-keys session))
        (path (expand-file-name path))
        (now (bp/agent-sessions--now)))
    (unless keys
      (error "Session has no durable identity to attach a file to"))
    (bp/agent-sessions--files-collapse keys)
    (let* ((table (bp/agent-sessions--shared-files-table))
           (primary (car keys))
           (files (gethash primary table))
           (existing (seq-find (lambda (f) (equal (plist-get f :path) path))
                               files))
           (file (list :path path
                       :label (or (and label (not (string-empty-p label)) label)
                                  (and existing (plist-get existing :label)))
                       :shared-at (or (and existing (plist-get existing :shared-at))
                                      now)
                       :updated-at now
                       :visited-at nil)))
      (puthash primary
               (if existing
                   (mapcar (lambda (f) (if (eq f existing) file f)) files)
                 (append files (list file)))
               table)
      (bp/agent-sessions--shared-file-write primary file)
      (bp/agent-sessions--shared-paths-invalidate)
      file)))

(defun bp/agent-sessions--detach-file (session path)
  "Remove PATH from SESSION's shared files; return non-nil if it was attached.
The file on disk is never touched — this drops the pointer to it, nothing more."
  (let ((table (bp/agent-sessions--shared-files-table))
        (db (bp/agent-sessions--db))
        (removed nil))
    (dolist (key (bp/agent-sessions--session-note-keys session))
      (let ((files (gethash key table)))
        (when (seq-find (lambda (f) (equal (plist-get f :path) path)) files)
          (puthash key
                   (seq-remove (lambda (f) (equal (plist-get f :path) path))
                               files)
                   table)
          (sqlite-execute db "DELETE FROM shared_files
                               WHERE kind = ? AND key = ? AND path = ?"
                          (append key (list path)))
          (setq removed t))))
    (when removed (bp/agent-sessions--shared-paths-invalidate))
    removed))

(defun bp/agent-sessions--set-visited (match-fn visitedp)
  "Set the visited state of every shared file MATCH-FN accepts.
Returns non-nil when something actually changed, so callers can skip a render.
Matching is by predicate rather than by (session, path) because the two callers
want different tests — a dashboard command knows the exact stored path, while
the focus hook only knows the true name of a buffer's file — and because a file
shared with two sessions should stop nagging from both once it has been read."
  (let ((db (bp/agent-sessions--db))
        (stamp (and visitedp (bp/agent-sessions--now)))
        (changed nil))
    (maphash
     (lambda (key files)
       (dolist (f files)
         (when (and (funcall match-fn f)
                    (not (eq (null (plist-get f :visited-at)) (null stamp))))
           (plist-put f :visited-at stamp)
           (sqlite-execute db "UPDATE shared_files SET visited_at = ?
                                WHERE kind = ? AND key = ? AND path = ?"
                           (list stamp (car key) (cadr key)
                                 (plist-get f :path)))
           (setq changed t))))
     (bp/agent-sessions--shared-files-table))
    changed))

(defun bp/agent-sessions--revert-shared-file-buffer (path)
  "Refresh an unmodified buffer already visiting PATH from disk.
Do not create a buffer for PATH, and never replace unsaved buffer changes."
  (when-let ((buffer (find-buffer-visiting path)))
    (with-current-buffer buffer
      (unless (buffer-modified-p)
        (revert-buffer t t)))))

;;;###autoload
(defun bp/agent-sessions-share-file (session-id path &optional label)
  "Attach PATH to the agent session running in terminal SESSION-ID.
The entry point `bin/emacs-share-file' calls over emacsclient, so SESSION-ID is
the terminal's EMACS_AGENT_SESSION_ID — the id the shell has — and resolving it
to whatever session is running there happens here, the same split the hook path
makes.  If an unmodified buffer already visits PATH, refresh it from disk;
modified buffers are left alone so sharing can never discard unsaved work.
Returns a string for the caller to print: emacsclient is the agent's only
channel back, so a failure has to be reported in the value, not signalled."
  (let ((session (bp/agent-sessions--session-for-id session-id)))
    (cond
     ((null session)
      (format "No Emacs terminal registered for session id %s" session-id))
     ((not (file-exists-p path))
      (format "No such file: %s" path))
     (t
      (bp/agent-sessions--revert-shared-file-buffer path)
      (bp/agent-sessions--share-file session path label)
      (bp/agent-sessions--refresh-if-visible)
      (format "Shared %s" (abbreviate-file-name (expand-file-name path)))))))

(defun bp/agent-sessions--file-at-point ()
  "Return (SESSION PATH FILE) for the shared-file row at point, or nil.
Point may be on the row itself or inside a note rendered under it, which
resolves to the file the note is about — the same courtesy
`bp/agent-sessions--note-target-at-point' extends."
  (let ((section (magit-current-section)))
    (when (and section (eq (oref section type) 'bp/agent-session-note))
      (setq section (oref section parent)))
    (when (and section (eq (oref section type) 'bp/agent-session-file))
      (let* ((path (oref section value))
             (row (bp/agent-sessions--enclosing-section 'bp/agent-session-row))
             (session (and row (bp/agent-sessions--session-for-id
                                (oref row value)))))
        (when session
          (list session path
                (seq-find (lambda (f) (equal (plist-get f :path) path))
                          (bp/agent-sessions--session-files session))))))))

(defun bp/agent-sessions--open-file-at-point (display-fn)
  "Open the shared file at point with DISPLAY-FN, clearing its highlight.
The refresh happens before DISPLAY-FN runs so the dashboard's point
restoration is not competing with whatever DISPLAY-FN does to the windows."
  (pcase (bp/agent-sessions--file-at-point)
    (`(,_session ,path ,_file)
     (if (not (file-exists-p path))
         (message "File no longer exists: %s" (abbreviate-file-name path))
       (when (bp/agent-sessions--set-visited
              (lambda (f) (equal (plist-get f :path) path)) t)
         (bp/agent-sessions--refresh-if-visible))
       (funcall display-fn (find-file-noselect path))))))

(defface bp/agent-session-file-unvisited
  '((t :inherit bp/agent-session-needs-attention))
  "Face for a shared file not opened since the agent last shared it.
Inherits the needs-attention face deliberately: an unread file and a waiting
session are the same claim on the user's attention, and the dashboard should
not make them learn two highlights for it.")

(defface bp/agent-session-file
  '((t :inherit default))
  "Face for a shared file the user has already opened.")

(defun bp/agent-sessions--insert-file (file indent)
  "Insert a row for shared FILE, indented by INDENT.
INDENT puts the row at the same depth as the session's note — one level in
from the session row — so it reads as hanging off that session rather than as
another row beside it.  Its section value is the path:
stable across renders, unique within a session, and disambiguated from the
same path under another session by magit's parent chain."
  (let* ((path (plist-get file :path))
         (unvisited (null (plist-get file :visited-at)))
         ;; One stat per file per render.  Affordable at the scale this holds
         ;; (a handful of paths, all local) and worth it: a file an agent
         ;; wrote to /tmp and the reaper has since removed should say so
         ;; rather than fail on RET.
         (missing (not (file-exists-p path))))
    ;; Record what this row *shows*, for `N'/`P' — see the docstring of
    ;; `bp/agent-sessions--row-status'.  An unopened file is reachable by the
    ;; attention keys for the same reason it is highlighted like one.
    (push (cons (line-beginning-position)
                (if unvisited 'needs-attention 'idle))
          bp/agent-sessions--row-status)
    (magit-insert-section (bp/agent-session-file path)
      (magit-insert-heading
       (propertize
        (concat indent (if unvisited "● " "  ")
                (format "%-28s %s"
                        (file-name-nondirectory (directory-file-name path))
                        (or (plist-get file :label)
                            (abbreviate-file-name path)))
                (if missing "  (missing)" "")
                "\n")
        'font-lock-face (if unvisited
                            'bp/agent-session-file-unvisited
                          'bp/agent-session-file)))
      (bp/agent-sessions--insert-note
       (list (bp/agent-sessions--file-note-key path))
       (concat indent "    ")))))

(defun bp/agent-sessions--insert-files (session indent)
  "Insert a row for every file shared with SESSION, indented by INDENT."
  (dolist (file (bp/agent-sessions--session-files session))
    (bp/agent-sessions--insert-file file indent)))

(defun bp/agent-sessions--build-tree (entries)
  "Group live ENTRIES into a list of repo plists for rendering.
Each element is (:name NAME :root ROOT :worktrees (WORKTREE ...)).
When `bp/agent-sessions-sort-by-activity' is set, every level is ordered
most-recently-active first; otherwise the stable order of
`bp/agent-sessions--sort-tree-stable' is used."
  (let ((repos
         (mapcar
          (lambda (rg)
            (let* ((rentries (cdr rg))
                   (session (plist-get (car rentries) :session))
                   (root (plist-get session :repo-root))
                   (session-groups
                    (seq-group-by
                     (lambda (e) (bp/agent-sessions--canonical-path
                                  (plist-get (plist-get e :session) :worktree-path)))
                     rentries)))
              (list :name (or (plist-get session :repo) "?")
                    :root root
                    :worktrees (bp/agent-sessions--worktrees-for root session-groups))))
          (seq-group-by
           (lambda (e) (bp/agent-sessions--canonical-path
                        (plist-get (plist-get e :session) :repo-root)))
           entries))))
    (if bp/agent-sessions-sort-by-activity
        (bp/agent-sessions--sort-tree repos)
      (bp/agent-sessions--sort-tree-stable repos))))

(defun bp/agent-sessions--insert-worktree (wt)
  (let ((entries (plist-get wt :entries)))
    (magit-insert-section (bp/agent-session-worktree
                           (list :label (plist-get wt :label)
                                 :path (plist-get wt :path)))
      (magit-insert-heading (format "  %s" (plist-get wt :label)))
      (bp/agent-sessions--insert-note
       (list (bp/agent-sessions--worktree-note-key wt)) "    ")
      (bp/agent-sessions--insert-entries entries))))

(defun bp/agent-sessions--insert-repo (repo)
  (magit-insert-section (bp/agent-session-repo
                         (list :name (plist-get repo :name)
                               :root (plist-get repo :root)))
    (magit-insert-heading (propertize (plist-get repo :name) 'face 'bold))
    (bp/agent-sessions--insert-note
     (list (bp/agent-sessions--repo-note-key repo)) "  ")
    (dolist (wt (plist-get repo :worktrees))
      (bp/agent-sessions--insert-worktree wt))
    (insert "\n")))

(defun bp/agent-sessions--refresh ()
  (interactive)
  (let* ((inhibit-read-only t)
         (win (get-buffer-window (current-buffer)))
         (saved-point (if win (window-point win) (point)))
         (section (save-excursion
                    (goto-char saved-point)
                    (magit-current-section)))
         ;; Remember the section at point by its stable identity, not by
         ;; absolute position: erase-buffer + rebuild (and sorting) shift and
         ;; reorder everything, so a char offset is meaningless.
         (ident (and section (magit-section-ident section)))
         ;; Keep point's position within that section as well.  Restoring only
         ;; IDENT puts point at the section's first column on every hook-driven
         ;; refresh, undoing ordinary horizontal motion such as `C-f'/`C-b'.
         (line-offset
          (and section
               (save-excursion
                 (goto-char saved-point)
                 (count-lines (oref section start) (line-beginning-position)))))
         (column (save-excursion
                   (goto-char saved-point)
                   (current-column)))
         (wstart (and win (window-start win))))
    (erase-buffer)
    (setq bp/agent-sessions--row-status nil)
    (magit-insert-section (bp/agent-sessions-root)
      (let ((repos (bp/agent-sessions--build-tree (bp/agent-sessions--live-entries))))
        (if (null repos)
            (insert "No active sessions.\n")
          (dolist (repo repos)
            (bp/agent-sessions--insert-repo repo)))))
    (setq bp/agent-sessions--row-status (nreverse bp/agent-sessions--row-status))
    ;; Freshly inserted sections carry the visibility magit cached for them
    ;; (e.g. a note folded with TAB) in their `hidden' slot, but nothing has
    ;; applied it to the new text yet; this pass does, exactly as
    ;; `magit-refresh-buffer' does after its own inserters run.  Rebind the
    ;; cache off so re-showing doesn't record the state it is restoring.
    (let ((magit-section-cache-visibility nil))
      (magit-section-show magit-root-section))
    (let ((target (and ident (magit-get-section ident))))
      (if (not target)
          (goto-char (point-min))
        (let ((start (oref target start))
              (end (oref target end)))
          (goto-char start)
          (forward-line line-offset)
          ;; The section may have become shorter during the refresh.  In that
          ;; case use its last line instead of wandering into the next section.
          (when (>= (point) end)
            (goto-char (max start (1- end)))
            (beginning-of-line))
          (move-to-column column))))
    (when win
      (set-window-point win (point))
      (when wstart
        (set-window-start win (min wstart (point-max)) t)))))

(defun bp/agent-sessions--reread-repo-info ()
  "Re-read repo/branch info from disk for every live session buffer.
The per-buffer `bp/agent-sessions--repo-info-cache' is keyed only on the
buffer's directory, so it never notices a branch checkout in the same
directory; and a real agent session's `:worktree' label is frozen at its first
hook.  Both go stale when the branch changes.  Clear the per-buffer caches and
refresh each registered session's repo/worktree fields so `g' reflects the
current branch."
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'bp/agent-sessions--repo-info-cache buf)
      (with-current-buffer buf
        (setq bp/agent-sessions--repo-info-cache nil))))
  (maphash
   (lambda (id session)
     (let ((buf (plist-get session :buffer)))
       (when (buffer-live-p buf)
         (let ((info (bp/agent-sessions--buffer-repo-info buf)))
           ;; Only overwrite when the buffer is still in a repo; a session that
           ;; has cd'd out keeps its last-known labels rather than going blank.
           (when (plist-get info :repo-root)
             (puthash id
                      (plist-put
                       (plist-put
                        (plist-put
                         (plist-put session :repo (plist-get info :repo))
                         :repo-root (plist-get info :repo-root))
                        :worktree (plist-get info :worktree))
                       :worktree-path (plist-get info :worktree-path))
                      bp/agent-sessions))))))
   bp/agent-sessions))

(defun bp/agent-sessions-refresh ()
  "Refresh the dashboard, re-reading git worktrees and branches from disk.
Unlike the internal re-render used by hooks and the sort toggle, this
invalidates the worktree cache and re-reads each session's repo/branch info."
  (interactive)
  (clrhash bp/agent-sessions--worktrees-cache)
  (bp/agent-sessions--reread-repo-info)
  (bp/agent-sessions--refresh))

(defun bp/agent-sessions--refresh-if-visible ()
  (let ((buf (get-buffer bp/agent-sessions-buffer-name)))
    (when buf
      (with-current-buffer buf
        (bp/agent-sessions--refresh)))))

(defun bp/agent-sessions--session-at-point ()
  (let ((section (magit-current-section)))
    (and section (eq (oref section type) 'bp/agent-session-row)
         (bp/agent-sessions--session-for-id (oref section value)))))

(defun bp/agent-sessions--switch-project-read-command ()
  "Read one key from the `project-switch-commands' menu and return its command.
Returns `ignore' for anything unrecognised, so the menu closes instead of
re-prompting.  Built from project.el's own pieces (`project-prefix-map',
`project-switch-commands', `project--menu-prompt') so a user's customisation
of the dispatch menu still applies; the deprecated (KEY COMMAND LABEL) row
format is normalised the same way `project--switch-project-command' does."
  (let* ((menu (mapcar (lambda (row) (if (characterp (car row)) (reverse row) row))
                       project-switch-commands))
         (map (let ((temp (make-sparse-keymap)))
                (set-keymap-parent temp project-prefix-map)
                (dolist (row menu temp)
                  (when-let ((cmd (nth 0 row))
                             (keychar (nth 2 row)))
                    (define-key temp (vector keychar) cmd)))))
         (choice (let ((overriding-local-map map))
                   (read-key-sequence (concat "Choose: " (project--menu-prompt)))))
         (command (lookup-key map choice)))
    (message nil)
    (if (and (commandp command)
             (or project-switch-use-entire-map (assq command menu)))
        command
      #'ignore)))

(defun bp/agent-sessions--switch-project (dir)
  "`project-switch-project' in DIR, but dismissed by any unrecognised key.
`project--switch-project-command' loops until it reads a key it knows, so the
dispatch menu behaves like a mode you have to escape from.  Here RET on a
heading is a glance at what's available, so a stray key should just close the
menu rather than trap you in it.  The override is scoped to this call — the
looping behaviour everywhere else in Emacs is left alone."
  (cl-letf (((symbol-function 'project--switch-project-command)
             #'bp/agent-sessions--switch-project-read-command))
    (project-switch-project dir)))

(defun bp/agent-sessions-jump ()
  "Act on the thing at point.
On a session row, pop to its vterm buffer.  On a shared-file row, open the
file — which is also what clears its highlight.  On a worktree or repo
heading, open project.el's dispatch menu in that directory (see
`bp/agent-sessions--switch-project')."
  (interactive)
  (if (bp/agent-sessions--file-at-point)
      (bp/agent-sessions--open-file-at-point #'pop-to-buffer)
    (let ((section (magit-current-section)))
      (pcase (and section (oref section type))
        ('bp/agent-session-row
         (let* ((session (bp/agent-sessions--session-for-id (oref section value)))
                (buf (and session (plist-get session :buffer))))
           (if (buffer-live-p buf)
               (pop-to-buffer buf)
             (message "No session at point."))))
        ((or 'bp/agent-session-worktree 'bp/agent-session-repo)
         (let* ((value (oref section value))
                (dir (or (plist-get value :path) (plist-get value :root))))
         (if (and dir (file-directory-p dir))
             (bp/agent-sessions--switch-project dir)
           (message "No worktree directory at point."))))
        (_ (message "Nothing to do here."))))))

(defun bp/agent-sessions-display ()
  "Display the session or shared file at point in another window.
Point stays in the dashboard either way."
  (interactive)
  ;; inhibit-same-window: never reuse/replace the dashboard's own window.
  (if (bp/agent-sessions--file-at-point)
      (bp/agent-sessions--open-file-at-point
       (lambda (buf) (display-buffer buf '(nil (inhibit-same-window . t)))))
    (let* ((session (bp/agent-sessions--session-at-point))
           (buf (and session (plist-get session :buffer))))
      (if (buffer-live-p buf)
          (display-buffer buf '(nil (inhibit-same-window . t)))
        (message "No session at point.")))))

(defun bp/agent-sessions--row-locations ()
  "Return (POSITION . ID) for each session row, in buffer order."
  (let (rows last-start)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((section (magit-current-section)))
          (when (and section
                     (eq (oref section type) 'bp/agent-session-row)
                     (/= (oref section start) (or last-start -1)))
            (setq last-start (oref section start))
            (push (cons last-start (oref section value)) rows)))
        (forward-line 1)))
    (nreverse rows)))

(defun bp/agent-sessions--nearby-session-id ()
  "Return the id of a session row next to the row at point.
Prefer the following row; when point is on the final row, return the previous
one.  Return nil when point is not on a session row or no other row exists."
  (let ((section (magit-current-section)))
    (when (and section (eq (oref section type) 'bp/agent-session-row))
      (let* ((start (oref section start))
             (rows (bp/agent-sessions--row-locations))
             (next (seq-find (lambda (row) (> (car row) start)) rows))
             (previous (seq-find (lambda (row) (< (car row) start))
                                 (reverse rows))))
        (cdr (or next previous))))))

(defun bp/agent-sessions--goto-session-id (id)
  "Move point to the dashboard row for session ID and return non-nil.
Return nil without moving point when ID is no longer displayed."
  (let ((row (seq-find (lambda (candidate) (equal (cdr candidate) id))
                       (bp/agent-sessions--row-locations))))
    (when row
      (goto-char (car row))
      t)))

(defun bp/agent-sessions--neighbour-ident (section)
  "Return the ident of the section to put point on once SECTION is gone.
The next sibling of the same type, else the previous one, else the parent — so
detaching a file lands on the next file, the last remaining file, or the
session they hang off, never back at the top of the buffer.

An *ident* rather than a position or a section object because the buffer is
erased and rebuilt in between, which is the same reason
`bp/agent-sessions--refresh' restores point by ident; and the reason this is
needed at all is that the refresh's own restoration cannot help here — the
section point was on is precisely the one that no longer exists."
  (when-let* ((parent (and section (oref section parent)))
              (type (oref section type))
              (siblings (seq-filter (lambda (c) (eq (oref c type) type))
                                    (oref parent children)))
              (pos (seq-position siblings section)))
    (magit-section-ident (or (nth (1+ pos) siblings)
                             (and (> pos 0) (nth (1- pos) siblings))
                             parent))))

(defun bp/agent-sessions--goto-ident (ident)
  "Move point to the section identified by IDENT, if it is still displayed."
  (when-let ((target (and ident (magit-get-section ident))))
    (goto-char (oref target start))
    t))

(defun bp/agent-sessions-kill ()
  "Kill the session at point, or detach the shared file at point.
On a session row this kills its terminal buffer and moves to a neighbouring
row.  On a shared-file row it drops the file from that session's list and
leaves the file on disk completely alone: this list is an inbox, and clearing
an item out of it must never be able to destroy the thing it points at."
  (interactive)
  (pcase (bp/agent-sessions--file-at-point)
    (`(,session ,path ,_file)
     ;; Resolve the neighbour *before* detaching, while the row is still on
     ;; screen to have neighbours at all.
     (let ((ident (bp/agent-sessions--neighbour-ident
                   (bp/agent-sessions--enclosing-section
                    'bp/agent-session-file))))
       (if (bp/agent-sessions--detach-file session path)
           (progn
             (bp/agent-sessions--refresh)
             (bp/agent-sessions--goto-ident ident)
             (message "Detached %s (the file on disk is untouched)."
                      (file-name-nondirectory (directory-file-name path))))
         (message "Not attached to this session."))))
    (_ (bp/agent-sessions--kill-session))))

(defun bp/agent-sessions--kill-session ()
  "Kill the session at point and move to a neighboring session row."
  (let* ((session (bp/agent-sessions--session-at-point))
         (buf (and session (plist-get session :buffer)))
         (nearby-id (and (buffer-live-p buf)
                         (bp/agent-sessions--nearby-session-id))))
    (if (not (buffer-live-p buf))
        (message "No session at point.")
      (when (kill-buffer buf)
        ;; The terminal's kill-buffer hook refreshes the dashboard once, but
        ;; the deleted section cannot be restored and that refresh lands at
        ;; point-min.  Re-render defensively (for terminals without the hook),
        ;; then select the adjacent surviving row by its stable session id.
        (bp/agent-sessions--refresh)
        (when nearby-id
          (bp/agent-sessions--goto-session-id nearby-id))))))

(defun bp/agent-sessions-mark-unread (&optional unmark)
  "Mark the session at point as unread — i.e. needing attention.
Gives the row the same highlight, `●' marker, sort priority and `N'/`P'
reachability a hook-reported needs-attention has, for sessions you want to
come back to yourself.  With a prefix argument (UNMARK), clear the mark.
The mark also clears on its own once you focus the session's buffer.

On a shared-file row this toggles the unopened highlight instead: `u' on a
file you have opened lights it up again, and `u' on one you have not clears it
without opening it — which is the whole point, for a file you have decided you
do not need to read.  UNMARK always clears."
  (interactive "P")
  (pcase (bp/agent-sessions--file-at-point)
    (`(,_session ,path ,file)
     (let ((visited (or (and unmark t) (null (plist-get file :visited-at)))))
       (bp/agent-sessions--set-visited
        (lambda (f) (equal (plist-get f :path) path)) visited)
       (bp/agent-sessions--refresh)
       (message (if visited "Marked as read." "Marked as unread."))))
    (_ (bp/agent-sessions--mark-session-unread unmark))))

(defun bp/agent-sessions--mark-session-unread (unmark)
  "Set or clear the unread mark on the session row at point."
  (let* ((session (bp/agent-sessions--session-at-point))
         (buf (and session (plist-get session :buffer))))
    (if (not (buffer-live-p buf))
        (message "No session at point.")
      (with-current-buffer buf
        (setq-local bp/agent-session--marked-unread (not unmark)))
      (bp/agent-sessions--refresh)
      (message (if unmark "Unread mark cleared." "Marked as needing attention.")))))

(defun bp/agent-sessions-branch ()
  "Branch (fork) the agent session at point into a new session.
Opens a new terminal in the same worktree and forks the conversation, so the
new session shares its parent's history but gets its own session id
\(`claude --fork-session' / `codex fork').  The new session is tagged as
branched from the one at point, shown as `↳ from …' in the dashboard."
  (interactive)
  (let* ((session (bp/agent-sessions--session-at-point))
         (agent (and session (plist-get session :agent-type)))
         (sid (and session (plist-get session :agent-session-id)))
         (path (and session
                    (or (plist-get session :worktree-path)
                        (let ((buf (plist-get session :buffer)))
                          (and (buffer-live-p buf)
                               (buffer-local-value 'default-directory buf)))))))
    (cond
     ((null session) (message "No session at point."))
     ((null sid) (message "Session at point has no resumable session id yet."))
     ((null path) (message "Session at point has no worktree to branch in."))
     (t (bp/agent-session--spawn
         path agent sid bp/agent-session-branch-commands
         (cons sid (bp/agent-sessions--session-short-label session)))))))

(defun bp/agent-sessions-mark-parent ()
  "Record that the session at point was branched from another session.
Prompts for the parent among the live sessions.  Use this to track a fork you
made outside the `b' command (e.g. running `claude --resume … --fork-session'
or `codex fork' by hand)."
  (interactive)
  (let* ((section (magit-current-section))
         (id (and section (eq (oref section type) 'bp/agent-session-row)
                  (oref section value)))
         (session (and id (bp/agent-sessions--session-for-id id))))
    (if (null session)
        (message "Point is not on a session row.")
      (let* ((self-sid (plist-get session :agent-session-id))
             (cands
              (delq nil
                    (mapcar (lambda (e)
                              (let ((es (plist-get e :session)))
                                ;; Exclude the target itself and any terminal
                                ;; running the *same* session (same conversation
                                ;; can't be its own parent — that would cycle).
                                (unless (or (equal (plist-get e :id) id)
                                            (and self-sid
                                                 (equal (plist-get es :agent-session-id)
                                                        self-sid)))
                                  (cons (bp/agent-sessions--session-label es) es))))
                            (bp/agent-sessions--live-entries))))
             (choice (and cands
                          (completing-read "Branched from: "
                                           (mapcar #'car cands) nil t)))
             (parent (and choice (cdr (assoc choice cands)))))
        (cond
         ((null cands) (message "No other sessions to branch from."))
         ((null parent) (message "No parent selected."))
         (t
          (let ((bf (cons (plist-get parent :agent-session-id)
                          (bp/agent-sessions--session-short-label parent)))
                (buf (plist-get session :buffer))
                (real (gethash id bp/agent-sessions)))
            ;; Set on the buffer so it survives future hook rebuilds, and on the
            ;; live registry entry (if any) so it shows immediately.
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local bp/agent-session--branched-from bf)))
            (when real
              (puthash id (plist-put real :branched-from bf) bp/agent-sessions))
            (bp/agent-sessions--refresh)
            (message "Marked as branched from %s" (cdr bf)))))))))

(defun bp/agent-sessions-edit-title ()
  "Edit the title of the session at point.
The chosen title is stored as a user override on the session's buffer, so it
takes precedence over — and survives — the agent's own OSC title updates.
Clearing it (entering an empty string) restores the agent's live title."
  (interactive)
  (let* ((session (bp/agent-sessions--session-at-point))
         (buf (and session (plist-get session :buffer))))
    (if (not (buffer-live-p buf))
        (message "No session at point.")
      (let* ((initial (with-current-buffer buf
                        (or bp/agent-session-title-override
                            bp/agent-session-title)))
             (new (string-trim (read-string "Session title: " initial))))
        (with-current-buffer buf
          (setq-local bp/agent-session-title-override
                      (unless (string-empty-p new) new)))
        (bp/agent-sessions--refresh)
        (message (if (string-empty-p new)
                     "Title override cleared."
                   (format "Title set to %s" new)))))))

(defun bp/agent-sessions--context-at-point ()
  "Return (:path DIR :name NAME) for the worktree/repo enclosing point, or nil.
Walks up from the section at point so it works whether point is on a
worktree heading, a repo heading, or an individual session row."
  (let ((section (magit-current-section)))
    (catch 'found
      (while section
        (let ((value (oref section value)))
          (pcase (oref section type)
            ('bp/agent-session-worktree
             (throw 'found (list :path (plist-get value :path)
                                 :name (plist-get value :label))))
            ('bp/agent-session-repo
             (throw 'found (list :path (plist-get value :root)
                                 :name (plist-get value :name))))))
        (setq section (oref section parent)))
      nil)))

(defun bp/agent-sessions--terminal-open (dir bufname)
  "Open the configured terminal in DIR in another window; return the buffer.
BUFNAME is the desired buffer name.  Loads the backend package lazily."
  (let ((default-directory (file-name-as-directory dir)))
    (pcase bp/agent-sessions-terminal
      ('vterm (require 'vterm) (vterm-other-window bufname))
      ('eat (require 'eat)
            (let ((eat-buffer-name bufname))
              ;; arg non-nil, non-number => eat--1 uses `generate-new-buffer'.
              (eat-other-window nil t)))
      (_ (error "Unknown `bp/agent-sessions-terminal': %S"
                bp/agent-sessions-terminal)))))

(defun bp/agent-sessions--terminal-send (buf string)
  "Send STRING followed by Enter to the terminal in BUF."
  (with-current-buffer buf
    (pcase bp/agent-sessions-terminal
      ('vterm (vterm-send-string string) (vterm-send-return))
      ('eat (eat-term-send-string eat-terminal string)
            (eat-term-send-string eat-terminal "\r")))))

(defun bp/agent-sessions--create-terminal (dir &optional name)
  "Open a new terminal in DIR named <NAME>-<backend>.
NAME defaults to DIR's basename (typically the worktree name).  The backend
is `bp/agent-sessions-terminal'."
  (let* ((base (or name (file-name-nondirectory (directory-file-name dir))))
         (bufname (format "%s-%s" base bp/agent-sessions-terminal)))
    (bp/agent-sessions--terminal-open dir bufname)))

(defun bp/agent-sessions--goto-terminal-row (buf)
  "Move the dashboard's point to the row for terminal BUF, if it has one.
Creating a terminal re-renders the dashboard, and that render restores point
to the section it was on — the worktree or repo heading `+' was pressed on —
so a row that did not exist before it has to be sought out afterwards.  The
window points are set explicitly because the dashboard is no longer the
selected buffer by then (the new terminal is), and an unselected window
displays its own point, not the buffer's."
  (when-let* ((id (and (buffer-live-p buf)
                       (buffer-local-value 'bp/agent-session-id buf)))
              (dashboard (get-buffer bp/agent-sessions-buffer-name)))
    (with-current-buffer dashboard
      (when (bp/agent-sessions--goto-session-id id)
        (dolist (win (get-buffer-window-list dashboard nil t))
          (set-window-point win (point)))))))

(defun bp/agent-sessions-new-vterm ()
  "Create a new vterm in the worktree at point, named after that worktree.
Leaves the dashboard's point on the new terminal's row, so the thing just
created is what `k', `t', `e' and `n'/`p' act from.  A second `+' still opens
a terminal in the same worktree, since `bp/agent-sessions--context-at-point'
walks up from a session row to the worktree containing it."
  (interactive)
  (let* ((ctx (bp/agent-sessions--context-at-point))
         (dir (plist-get ctx :path)))
    (if (or (null dir) (not (file-directory-p dir)))
        (message "No worktree at point.")
      (bp/agent-sessions--goto-terminal-row
       (bp/agent-sessions--create-terminal dir (plist-get ctx :name))))))

;;;###autoload
(defun bp/agent-session-switch-or-new ()
  "Switch to an agent session of the current project, or start a new one.
Prompts to pick among the sessions running in the current project's
worktree; choosing nothing (empty input) opens a new vterm in the project
root, like `+' in the dashboard.  With no sessions at all, goes straight to
creating one."
  (interactive)
  (let* ((root (or (ignore-errors (project-root (project-current)))
                   default-directory))
         (sessions
          (seq-filter
           (lambda (entry)
             (let* ((s (plist-get entry :session))
                    (wp (or (plist-get s :worktree-path)
                            (let ((b (plist-get s :buffer)))
                              (and (buffer-live-p b)
                                   (buffer-local-value 'default-directory b))))))
               (and wp (file-equal-p wp root))))
           (bp/agent-sessions--live-entries)))
         (cands
          (mapcar (lambda (entry)
                    (let* ((s (plist-get entry :session))
                           (buf (plist-get s :buffer)))
                      (cons (format "%s [%s %s]"
                                    (buffer-name buf)
                                    (plist-get s :agent-type)
                                    (plist-get s :status))
                            buf)))
                  sessions))
         (choice (and cands
                      (completing-read
                       (format "Agent session in %s (empty for new): "
                               (file-name-nondirectory (directory-file-name root)))
                       (mapcar #'car cands) nil nil)))
         (buf (and choice (cdr (assoc choice cands)))))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (bp/agent-sessions--create-terminal
       root (plist-get (bp/agent-session--repo-info root) :worktree)))))

(defun bp/agent-sessions--rows ()
  "Return (LINE-POS . STATUS) for each session and shared-file row, in order.
This is what the last render drew (`bp/agent-sessions--row-status'), never a
fresh lookup: only the render path knows the displayed status."
  bp/agent-sessions--row-status)

(defun bp/agent-sessions--goto-row (direction predicate)
  "Move point to the next/previous row (DIRECTION is `next' or `prev').
When PREDICATE is non-nil, only rows whose status satisfies it are considered."
  (let* ((cur (line-beginning-position))
         (positions (mapcar #'car
                            (if predicate
                                (seq-filter (lambda (r) (funcall predicate (cdr r)))
                                            (bp/agent-sessions--rows))
                              (bp/agent-sessions--rows))))
         (target (pcase direction
                   ('next (seq-find (lambda (p) (> p cur)) positions))
                   ('prev (seq-find (lambda (p) (< p cur)) (reverse positions))))))
    (cond (target (goto-char target))
          ((null positions) (message "No matching row."))
          (t (message "No %s row." (if (eq direction 'next) "next" "previous"))))))

(defun bp/agent-sessions-next ()
  "Move to the next session row."
  (interactive)
  (bp/agent-sessions--goto-row 'next nil))

(defun bp/agent-sessions-prev ()
  "Move to the previous session row."
  (interactive)
  (bp/agent-sessions--goto-row 'prev nil))

(defun bp/agent-sessions-toggle-sort ()
  "Toggle the dashboard between activity order and the stable order.
Activity order reorders on every hook event, so concurrently busy sessions
trade places constantly; the stable order is the default for that reason."
  (interactive)
  (setq bp/agent-sessions-sort-by-activity
        (not bp/agent-sessions-sort-by-activity))
  (bp/agent-sessions--refresh)
  (message "Sorting by %s"
           (if bp/agent-sessions-sort-by-activity
               "most recent activity"
             "attention, then session age")))

(defun bp/agent-sessions--attention-p (status)
  (memq status '(needs-attention error)))

(defun bp/agent-sessions-next-attention ()
  "Move to the next session row that needs attention."
  (interactive)
  (bp/agent-sessions--goto-row 'next #'bp/agent-sessions--attention-p))

(defun bp/agent-sessions-prev-attention ()
  "Move to the previous session row that needs attention."
  (interactive)
  (bp/agent-sessions--goto-row 'prev #'bp/agent-sessions--attention-p))

(defun bp/agent-sessions--enclosing-section (type)
  "Return the innermost section of TYPE at or above point, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) type)))
      (setq section (oref section parent)))
    section))

(defun bp/agent-sessions--group-at-point ()
  "Return (SCOPE KEYS KEY) for the reorderable thing at point, or nil.
SCOPE is its manual-order scope, KEYS the manual-order keys of every sibling
in its group in current display order, and KEY its own key.  The group is
re-derived from the live tree rather than scraped from the buffer, so the keys
are exactly what the next render will order."
  (let* ((section (magit-current-section))
         (repos (bp/agent-sessions--build-tree (bp/agent-sessions--live-entries)))
         (repo-sec (bp/agent-sessions--enclosing-section 'bp/agent-session-repo))
         (rkey (and repo-sec (bp/agent-sessions--repo-key (oref repo-sec value))))
         (repo (and rkey (seq-find (lambda (r)
                                     (equal rkey (bp/agent-sessions--repo-key r)))
                                   repos))))
    (pcase (and section (oref section type))
      ('bp/agent-session-repo
       (list bp/agent-sessions--repos-scope
             (mapcar #'bp/agent-sessions--repo-key repos)
             rkey))
      ('bp/agent-session-worktree
       (when repo
         (list (bp/agent-sessions--worktrees-scope rkey)
               (mapcar #'bp/agent-sessions--worktree-key
                       (plist-get repo :worktrees))
               (bp/agent-sessions--worktree-key (oref section value)))))
      ('bp/agent-session-row
       (let* ((wt-sec (bp/agent-sessions--enclosing-section
                       'bp/agent-session-worktree))
              (wkey (and wt-sec repo
                         (bp/agent-sessions--worktree-key (oref wt-sec value))))
              (wt (and wkey
                       (seq-find (lambda (w)
                                   (equal wkey
                                          (bp/agent-sessions--worktree-key w)))
                                 (plist-get repo :worktrees))))
              (session (bp/agent-sessions--session-for-id (oref section value))))
         (when (and wt session)
           (list (bp/agent-sessions--sessions-scope rkey wkey)
                 (mapcar #'bp/agent-sessions--entry-buffer-name
                         (plist-get wt :entries))
                 (bp/agent-sessions--session-buffer-name session)))))
      ('bp/agent-session-file
       (let* ((row (bp/agent-sessions--enclosing-section 'bp/agent-session-row))
              (session (and row (bp/agent-sessions--session-for-id
                                 (oref row value)))))
         (when session
           (list (bp/agent-sessions--files-scope session)
                 (mapcar (lambda (f) (plist-get f :path))
                         (bp/agent-sessions--session-files session))
                 (oref section value))))))))

(defun bp/agent-sessions--merge-order (saved visible)
  "Splice the VISIBLE key order into SAVED, keeping absent keys in place.
Each slot in SAVED held by a currently visible key is refilled from VISIBLE in
order, and visible keys SAVED doesn't know about are appended.  Rewriting a
group therefore never discards the remembered slot of a repo, worktree, or
session that simply isn't on screen right now (all its terminals closed, say)."
  (let ((queue visible)
        (vis (let ((h (make-hash-table :test 'equal)))
               (dolist (k visible) (puthash k t h))
               h))
        result)
    (dolist (k saved)
      (if (gethash k vis)
          (when queue (push (pop queue) result))
        (push k result)))
    (append (nreverse result) queue)))

(defun bp/agent-sessions--persist-move (scope keys key delta)
  "Record KEY shifted by DELTA among KEYS within SCOPE, and save.
Returns non-nil when the order changed; messages and returns nil when KEY is
already at the end it is being moved towards."
  (let* ((pos (seq-position keys key))
         (dest (and pos (+ pos delta))))
    (cond
     ((null pos) (message "Nothing to reorder at point.") nil)
     ((or (< dest 0) (>= dest (length keys)))
      (message "Already %s in its group." (if (< delta 0) "first" "last"))
      nil)
     (t
      (let* ((swapped (copy-sequence keys))
             (table (bp/agent-sessions--order-table))
             (old (gethash scope table))
             new)
        (setf (nth pos swapped) (nth dest keys)
              (nth dest swapped) key)
        (setq new (bp/agent-sessions--merge-order old swapped))
        (puthash scope new table)
        ;; Hand both lists over so only the rows that actually moved are
        ;; written — for a transposition that is two of them.
        (bp/agent-sessions--order-write-group scope old new)
        t)))))

(defun bp/agent-sessions--move (delta)
  "Move the repo, worktree, or session row at point by DELTA within its group."
  (if bp/agent-sessions-sort-by-activity
      (message "Manual order is ignored while sorting by activity (press `s').")
    (pcase (bp/agent-sessions--group-at-point)
      (`(,scope ,keys ,key)
       (when (bp/agent-sessions--persist-move scope keys key delta)
         (bp/agent-sessions--refresh)))
      (_ (message "Nothing to reorder at point.")))))

(defun bp/agent-sessions-move-down ()
  "Move the repo, worktree, or session at point one place down its group.
Only the enclosing group is affected: a session moves among the sessions of
its worktree, a worktree among its repo's worktrees, a repo among the repos.
The chosen order is written to `bp/agent-sessions-db-file', so it survives
both the session and Emacs itself; a session is remembered by its buffer name,
so renaming one forgets its place.  Unavailable while sorting by activity
\(`s'), which reorders by recency on every event.  `C-c C-o' clears it again."
  (interactive)
  (bp/agent-sessions--move 1))

(defun bp/agent-sessions-move-up ()
  "Move the repo, worktree, or session at point one place up its group.
See `bp/agent-sessions-move-down'."
  (interactive)
  (bp/agent-sessions--move -1))

(defun bp/agent-sessions-clear-manual-order ()
  "Forget the manual order (`M-n' / `M-p') and return to the automatic one.
With point inside a repo, worktree, or on a session row, clears just that
item's group; with a prefix argument, clears every group."
  (interactive)
  (let ((table (bp/agent-sessions--order-table)))
    (if current-prefix-arg
        (progn (clrhash table)
               (bp/agent-sessions--order-delete-all)
               (bp/agent-sessions--refresh)
               (message "Manual order cleared everywhere."))
      (pcase (bp/agent-sessions--group-at-point)
        (`(,scope ,_ ,_)
         (remhash scope table)
         (bp/agent-sessions--order-delete-group scope)
         (bp/agent-sessions--refresh)
         (message "Manual order cleared for this group."))
        (_ (message "Point is not in a reorderable group."))))))

;;;###autoload
(defun bp/agent-session-rename-to-title ()
  "Rename the current vterm buffer to its agent session title.
The title captured from the terminal (`bp/agent-session-title') is offered
as the editable default so it can be tweaked before confirming."
  (interactive)
  (let* ((initial (or bp/agent-session-title (buffer-name)))
         (new (string-trim (read-string "Rename buffer to: " initial))))
    (unless (string-empty-p new)
      (rename-buffer new t)
      (message "Renamed to %s" new))))

(defcustom bp/agent-session-resume-commands
  '((claude . "claude --resume %s")
    (codex  . "codex resume %s"))
  "Alist mapping AGENT-TYPE symbol to a shell command that resumes a session.
The %s is replaced with the agent's own session id."
  :type '(alist :key-type symbol :value-type string))

(defcustom bp/agent-session-branch-commands
  '((claude . "claude --resume %s --fork-session")
    (codex  . "codex fork %s"))
  "Alist mapping AGENT-TYPE symbol to a shell command that branches a session.
Branching forks the conversation into a new session that shares the parent's
history but gets its own session id (`claude --fork-session' / `codex fork').
The %s is replaced with the parent's own session id."
  :type '(alist :key-type symbol :value-type string))

(defun bp/agent-session--spawn (worktree-path agent-type session-id commands
                                              &optional branched-from)
  "Open a terminal in WORKTREE-PATH and run COMMANDS's template for AGENT-TYPE.
COMMANDS is an alist like `bp/agent-session-resume-commands'; its template's
%s is filled with SESSION-ID.  When BRANCHED-FROM is non-nil (a cons
\(PARENT-SID . LABEL)), tag the new terminal so the session it starts records
that it was branched from that parent.  Returns the new terminal buffer."
  (let* ((agent (if (symbolp agent-type) agent-type (intern agent-type)))
         (template (alist-get agent commands))
         (default-directory (file-name-as-directory worktree-path)))
    (unless template
      (error "No command configured for agent type `%s'" agent))
    (unless (file-directory-p default-directory)
      (error "Worktree no longer exists: %s" worktree-path))
    (let ((buf (bp/agent-sessions--create-terminal
                default-directory
                (plist-get (bp/agent-session--repo-info default-directory) :worktree))))
      (when (and branched-from (buffer-live-p buf))
        (with-current-buffer buf
          (setq-local bp/agent-session--branched-from branched-from)))
      (bp/agent-sessions--terminal-send buf (format template session-id))
      buf)))

;;;###autoload
(defun bp/agent-session-start (worktree-path agent-type session-id)
  "Open a vterm in WORKTREE-PATH and resume AGENT-TYPE's SESSION-ID.
AGENT-TYPE is a symbol such as `claude' or `codex'.  This is the target of
the `elisp:' links produced by `org-store-link' on a session row."
  (bp/agent-session--spawn worktree-path agent-type session-id
                           bp/agent-session-resume-commands))

(defun bp/agent-sessions--buffer-for-agent-session (session-id)
  "Return the live buffer running the agent session SESSION-ID, or nil.
SESSION-ID is the agent's own session id (a UUID) as recorded in
`bp/agent-sessions' under :agent-session-id."
  (catch 'found
    (maphash
     (lambda (_id session)
       (when (equal (plist-get session :agent-session-id) session-id)
         (let ((buf (plist-get session :buffer)))
           (when (buffer-live-p buf)
             (throw 'found buf)))))
     bp/agent-sessions)
    nil))

;;;###autoload
(defun bp/agent-session-open (worktree-path agent-type session-id)
  "Jump to the running AGENT-TYPE session SESSION-ID, or resume it if not open.
If a live terminal is already running SESSION-ID (matched by the agent's own
session id), switch to it; otherwise open a new terminal in WORKTREE-PATH and
resume it via `bp/agent-session-start'.  This is the target of the
`agent-session' `elisp:' links produced by `org-store-link'."
  (let ((buf (bp/agent-sessions--buffer-for-agent-session session-id)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (bp/agent-session-start worktree-path agent-type session-id))))

;;; Restoring sessions from the log -----------------------------------------

(defun bp/agent-sessions--log-rows (where &optional args)
  "Return session-log rows matching WHERE as plists, newest ending first.
Deduplicated by agent session id, keeping each id's most recent row: the fork
transient can leave a second row carrying a parent's id, and one entry per
resumable session is what the caller wants either way.  Rows with no agent
session id are skipped — a bare terminal has nothing to resume, and `+' opens
one anyway."
  (mapcar
   (lambda (r)
     (pcase-let ((`(,sid ,type ,worktree ,label ,title ,closed ,reason) r))
       (list :agent-sid sid :agent-type type :worktree worktree
             :label label :title title :closed-at closed :reason reason)))
   (sqlite-select
    (bp/agent-sessions--db)
    (concat "SELECT agent_sid, agent_type, worktree, label, title,
                    MAX(closed_at) AS closed_at, close_reason
               FROM sessions
              WHERE agent_sid IS NOT NULL AND agent_type IS NOT NULL AND "
            where
            " GROUP BY agent_sid ORDER BY closed_at DESC")
    args)))

(defun bp/agent-sessions--log-describe (row)
  "A one-line label for session-log ROW, for completion or confirmation."
  (format "%-28s %-7s %s"
          (or (plist-get row :label) "?")
          (or (plist-get row :agent-type) "?")
          (or (plist-get row :title)
              (substring (or (plist-get row :agent-sid) "?") 0 8))))

(defun bp/agent-sessions--log-annotate (row)
  "The trailing annotation for session-log ROW: when it ended, and how."
  (let ((at (plist-get row :closed-at)))
    (format "  %s, %s"
            (if at
                (format-time-string "%b %e %H:%M" (seconds-to-time at))
              "?")
            (pcase (plist-get row :reason)
              ("killed" "you closed it")
              ("agent-exit" "agent exited")
              ("superseded" "replaced in its terminal")
              ("emacs-exit" "Emacs quit")
              ("crash" "Emacs died")
              (other (or other "?"))))))

(defun bp/agent-sessions--restore-row (row)
  "Resume the session described by ROW, or jump to it if it is already open."
  (let ((wt (plist-get row :worktree))
        (sid (plist-get row :agent-sid))
        (type (intern (plist-get row :agent-type))))
    (cond
     ((bp/agent-sessions--buffer-for-agent-session sid)
      (pop-to-buffer (bp/agent-sessions--buffer-for-agent-session sid))
      (message "That session is already open."))
     ((not (file-directory-p wt))
      (message "Worktree is gone: %s" wt))
     (t (bp/agent-session-start wt type sid)))))

;;;###autoload
(defun bp/agent-sessions-restore ()
  "Resume a session that has ended, picked from the log.
Lists every session the log knows the end of, most recently ended first,
annotated with when and how it ended.  A session whose terminal is somehow
still open is switched to rather than resumed twice, and one whose worktree
has since been deleted says so instead of failing in a shell."
  (interactive)
  (let ((rows (bp/agent-sessions--log-rows "closed_at IS NOT NULL")))
    (if (null rows)
        (message "No ended sessions in the log yet.")
      (let* ((table (mapcar (lambda (r)
                              (cons (bp/agent-sessions--log-describe r) r))
                            rows))
             (completion-extra-properties
              (list :annotation-function
                    (lambda (k)
                      (bp/agent-sessions--log-annotate (cdr (assoc k table))))))
             (choice (completing-read "Restore session: " table nil t)))
        (when-let ((row (cdr (assoc choice table))))
          (bp/agent-sessions--restore-row row))))))

;;;###autoload
(defun bp/agent-sessions-restore-previous ()
  "Reopen the sessions that were running when Emacs last stopped.
Covers both an Emacs that crashed and one that was quit while sessions were
still going — from here those are the same thing, and each closes its batch of
rows with a single timestamp, which is how the last batch is identified.

Deliberately not bound to a key: it is a recovery command, wanted rarely and
never by accident.  Sessions already open are skipped, as are worktrees that
no longer exist, and what is left is shown for confirmation before anything
is spawned."
  (interactive)
  (let* ((rows (bp/agent-sessions--log-rows
                "close_reason IN ('crash', 'emacs-exit')
                 AND closed_at = (SELECT MAX(closed_at) FROM sessions
                                   WHERE close_reason IN ('crash', 'emacs-exit'))"))
         (live (seq-filter (lambda (r)
                             (bp/agent-sessions--buffer-for-agent-session
                              (plist-get r :agent-sid)))
                           rows))
         (gone (seq-filter (lambda (r)
                             (not (file-directory-p (plist-get r :worktree))))
                           rows))
         (todo (seq-difference rows (append live gone))))
    (cond
     ((null rows) (message "No sessions were lost — nothing to restore."))
     ((null todo)
      (message "Nothing to restore: %d already open, %d worktree(s) gone."
               (length live) (length gone)))
     (t
      (with-output-to-temp-buffer "*Agent Sessions Restore*"
        (princ (format "Sessions from the last Emacs (%s):\n\n"
                       (bp/agent-sessions--log-annotate (car todo))))
        (dolist (r todo)
          (princ (concat "  " (bp/agent-sessions--log-describe r) "\n")))
        (when live
          (princ (format "\nAlready open, will be skipped: %d\n" (length live))))
        (dolist (r gone)
          (princ (format "\nWorktree gone, cannot restore: %s\n"
                         (plist-get r :worktree)))))
      (when (yes-or-no-p (format "Restore %d session(s)? " (length todo)))
        ;; Staggered: each spawns a terminal and types into it, and firing them
        ;; all in one go leaves the shells racing their own startup.
        (let ((n 0))
          (dolist (row todo)
            (run-at-time (* n 1.0) nil
                         (lambda (r)
                           (condition-case err
                               (save-window-excursion
                                 (bp/agent-sessions--restore-row r))
                             (error (message "agent-sessions: %s"
                                             (error-message-string err)))))
                         row)
            (setq n (1+ n))))
        (message "Restoring %d session(s)…" (length todo)))))))

(defun bp/agent-sessions--store-link-for (session)
  "Store an `elisp:' resume link for SESSION via `org-link-store-props'.
Return non-nil on success, nil when SESSION lacks the info to build a link."
  (when session
    (let* ((path (or (plist-get session :worktree-path)
                     (let ((buf (plist-get session :buffer)))
                       (and (buffer-live-p buf)
                            (buffer-local-value 'default-directory buf)))))
           (agent (plist-get session :agent-type))
           (sid (plist-get session :agent-session-id))
           (branch (and path
                        (ignore-errors
                          (let ((default-directory path))
                            (magit-get-current-branch)))))
           (title (bp/agent-sessions--session-title session)))
      (when (and path sid)
        (org-link-store-props
         :type "agent-session"
         :link (format "agent-session:%s::%s::%s"
                       agent sid (file-name-as-directory path))
         :description (if branch
                          (format "%s %s %s" branch agent (or title sid))
                        (format "%s %s" agent (or title sid))))
        t))))

(defun bp/agent-sessions--org-follow-link (link &optional _arg)
  "Follow an `agent-session:' LINK, jumping to or resuming the session.
LINK is AGENT::SESSION-ID::WORKTREE-PATH, as produced by the `:store'
handler.  Using a dedicated link type (rather than an `elisp:' link) avoids
the confirmation prompt Org shows before running arbitrary elisp."
  (if (string-match "\\`\\([^:]+\\)::\\([^:]+\\)::\\(.*\\)\\'" link)
      (bp/agent-session-open (match-string 3 link)
                             (match-string 1 link)
                             (match-string 2 link))
    (error "Malformed agent-session link: %s" link)))

(defun bp/agent-sessions--org-store-link (&optional _interactive)
  "Store an `elisp:' link that resumes a Claude/Codex session.
Works both on a session row in `bp/agent-sessions-mode' and inside a vterm
buffer that has an active session (keyed by the buffer-local
`bp/agent-session-id').  Registered as the `:store' handler for
`org-store-link'."
  (bp/agent-sessions--store-link-for
   (cond ((derived-mode-p 'bp/agent-sessions-mode)
          (bp/agent-sessions--session-at-point))
         (bp/agent-session-id
          (gethash bp/agent-session-id bp/agent-sessions)))))

(defun bp/agent-sessions--sync-default-directory ()
  "Track the worktree/repo at point in `default-directory'.
This makes `C-x p' project commands (and anything else keyed off
`default-directory') operate on the worktree under the cursor."
  (let* ((ctx (bp/agent-sessions--context-at-point))
         (dir (and ctx (plist-get ctx :path))))
    (when (and dir (file-directory-p dir))
      (setq default-directory (file-name-as-directory dir)))))

(define-derived-mode bp/agent-sessions-mode magit-section-mode "Agent-Sessions"
  "Major mode listing Claude/Codex sessions running in vterm, as a
repo > worktree > session tree with foldable sections (TAB to fold/unfold)."
  (add-hook 'post-command-hook
            #'bp/agent-sessions--sync-default-directory nil t))

;; `define-derived-mode' sets the parent keymap only when the map has none, and
;; the map itself is a `defvar' that survives reloading this file.  An Emacs
;; that loaded a version of this mode deriving from something else therefore
;; keeps that old parent forever — which cost us TAB: `tabulated-list-mode-map'
;; inherits `button-buffer-map', whose `forward-button' shadowed
;; `magit-section-toggle' ("No button").  Re-point it on every load.
(set-keymap-parent bp/agent-sessions-mode-map magit-section-mode-map)


;;; Orchestration ------------------------------------------------------------
;;
;; An agent spawning other agents and talking to them.  Everything here is
;; reached from `bin/emacs-agent' over emacsclient, so every entry point
;; *returns* its failures as a string: emacsclient's printed value is the
;; agent's only channel back, exactly as for `bp/agent-sessions-share-file'.
;;
;; What this deliberately is not: there are no task rows, no dependency graph
;; and no scheduler.  The orchestrator is an agent with a context window; that
;; context *is* the plan, and duplicating it in a table would mean keeping two
;; plans honest.  What Emacs owns is the part an agent cannot do for itself —
;; starting a terminal, addressing another live one, and blocking until it
;; answers.

(defcustom bp/agent-orchestration-launch-commands
  '((claude . "claude \"$(cat %s)\"")
    (codex  . "codex \"$(cat %s)\""))
  "Shell command per agent type that starts a worker on a task brief.
%s is the path of the brief file `bp/agent-orchestration-spawn' wrote.

The brief is read by the shell instead of being passed as a literal argument
because it is a two-kilobyte multi-line prompt, and typing that into a terminal
as one quoted argument is exactly the sort of line a TUI mangles.  `cat' keeps
the typed line short whichever agent is launched, and passing the task at
launch avoids the race that sending it afterwards would have: a TUI that is
still starting silently drops input."
  :type '(alist :key-type symbol :value-type string))

(defcustom bp/agent-orchestration-push-when-idle t
  "When non-nil, mail for an agent that is resting is typed into its terminal.
An orchestrator that is waiting collects its own mail through `emacs-agent
wait', so this covers the other case: mail arriving for an agent that has
finished its turn and would otherwise sit idle until the user prompted it.
Delivery is skipped while the recipient is polling, so a waiting orchestrator
cannot be handed the same message twice — see `bp/agent-orchestration--polling-p'."
  :type 'boolean)

(defcustom bp/agent-orchestration-poll-grace 30
  "Seconds after an inbox read during which mail is not pushed to a terminal.
`emacs-agent wait' polls every couple of seconds, so anything shorter than its
interval would race it; anything much longer would leave a coordinator that has
stopped waiting waiting again."
  :type 'integer)

(defconst bp/agent-orchestration--submit-delay 0.5
  "Seconds between pasting a message into a TUI and pressing Enter.
The pasted block only becomes the TUI's composed input after it has processed
it; submitting in the same breath submits an empty prompt.")

(defvar bp/agent-orchestration--brief-dir
  (expand-file-name "emacs-agent-briefs/" temporary-file-directory)
  "Where task briefs are written.  Kept after the worker starts, on purpose:
it is the only record of what a worker was actually told, and it is what makes
a launch reproducible by hand (`claude \"$(cat …)\"').")

;;;; Ids and labels

(defun bp/agent-orchestration--next-id (prefix)
  "An id unique across Emacs restarts, not just within one.
The pid is in it for the same reason it is in a terminal id: stored mail
outlives the Emacs that minted its ids, and a fresh counter would otherwise
hand a new message the id of a stored one."
  (format "%s-%d-%d" prefix (emacs-pid) (cl-incf bp/agent-orchestration--counter)))

(defun bp/agent-orchestration--task-title (task)
  "A short label for TASK: its first line, truncated."
  (let* ((line (car (split-string (string-trim task) "\n" t)))
         (line (or line "task")))
    (if (> (length line) 48) (concat (substring line 0 45) "…") line)))

(defun bp/agent-orchestration--mail-key (id session)
  "The durable identity mail for SESSION in terminal ID is filed under.
The agent's own session id when it has one: that is what `R' resumes, so mail
filed under it comes back attached to the conversation it belongs to.  Failing
that the terminal id, which carries this Emacs's pid — unique for the life of
the terminal and never minted again, so a bare terminal's mail can never be
inherited by an unrelated one that happens to reuse its buffer name.  (Which is
also why mail under a terminal key is not restorable: nothing can resume a bare
shell, so there would be nothing to restore it to.)"
  (let ((sid (plist-get session :agent-session-id)))
    (if sid (list "session" sid) (list "terminal" id))))

(defun bp/agent-orchestration--mail-insert (msg)
  "Record MSG in the database.  Reported, not raised, on failure: a message
already queued in memory should still be delivered when the record fails."
  (condition-case err
      (sqlite-execute
       (bp/agent-sessions--db)
       "INSERT OR REPLACE INTO mail
          (id, from_kind, from_key, from_label, to_kind, to_key,
           type, subject, body, files, report, sent_at, read_at)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL)"
       (append (list (plist-get msg :id))
               (plist-get msg :from-key)
               (list (plist-get msg :from-label))
               (plist-get msg :to-key)
               (list (plist-get msg :type)
                     (plist-get msg :subject)
                     (plist-get msg :body)
                     (plist-get msg :files)
                     (plist-get msg :report)
                     (plist-get msg :sent-at))))
    (error (message "agent-sessions: could not record mail (%s)"
                    (error-message-string err)))))

(defun bp/agent-orchestration--hydrate (id)
  "Load ID's unread mail from the database into its in-memory inbox.
Called when a session's identity becomes known (a hook event) and when an agent
reads its inbox — never from the render path, which must not query the
database.  Safe to call repeatedly: messages already present are skipped by id,
and ids carry the pid of the Emacs that minted them, so one restored from a
previous run cannot collide with one from this one."
  (let* ((session (bp/agent-sessions--session-for-id id))
         (key (and session (bp/agent-orchestration--mail-key id session))))
    (when key
      (condition-case err
          (let* ((existing (bp/agent-orchestration--messages id))
                 (known (mapcar (lambda (m) (plist-get m :id)) existing))
                 (rows (sqlite-select
                        (bp/agent-sessions--db)
                        "SELECT id, from_kind, from_key, from_label, type, subject,
                                body, files, report, sent_at
                           FROM mail
                          WHERE to_kind = ? AND to_key = ? AND read_at IS NULL
                          ORDER BY sent_at, rowid"
                        key))
                 (fresh nil))
            (pcase-dolist (`(,mid ,fkind ,fkey ,flabel ,type ,subject
                                  ,body ,files ,report ,sent)
                           rows)
              (unless (member mid known)
                (push (list :id mid
                            ;; No `:from': the terminal that sent this belonged
                            ;; to a previous Emacs.  `:from-key' is how a live
                            ;; one is found again, if it was restored too.
                            :from nil :from-key (list fkind fkey) :from-label flabel
                            :to id :to-key key :type type :subject subject
                            :body body :files files :report report
                            :sent-at sent :read nil)
                      fresh)))
            (when fresh
              (puthash id (append existing (nreverse fresh))
                       bp/agent-orchestration--mail)))
        (error (message "agent-sessions: could not read stored mail (%s)"
                        (error-message-string err)))))))

(defun bp/agent-orchestration--live-id-for-key (key)
  "Terminal id of the live session filed under durable KEY, or nil.
How a restored conversation is found again: the id it had when it sent a
message is gone, but the identity it was filed under is not."
  (seq-some (lambda (entry)
              (let ((eid (plist-get entry :id)))
                (and (equal key (bp/agent-orchestration--mail-key
                                 eid (plist-get entry :session)))
                     eid)))
            (bp/agent-sessions--live-entries)))

(defun bp/agent-orchestration--sender-terminal (msg)
  "The live terminal id MSG can be answered at, or nil when its sender is gone."
  (let ((from (plist-get msg :from))
        (key (plist-get msg :from-key)))
    (cond ((and from (bp/agent-sessions--session-for-id from)) from)
          (key (bp/agent-orchestration--live-id-for-key key)))))

(defun bp/agent-orchestration--record-poll (id)
  (puthash id (float-time) bp/agent-orchestration--polls))

(defun bp/agent-orchestration--polling-p (id)
  (let ((last (gethash id bp/agent-orchestration--polls)))
    (and last (< (- (float-time) last) bp/agent-orchestration-poll-grace))))

;;;; Message rendering

(defun bp/agent-orchestration--describe-sender (id)
  "How a message from ID is introduced: its agent type and worktree, if live."
  (let ((session (bp/agent-sessions--session-for-id id)))
    (if (null session)
        " (terminal gone)"
      (format " (%s%s)"
              (plist-get session :agent-type)
              (let ((wt (plist-get session :worktree)))
                (if wt (concat ", " wt) ""))))))

(defun bp/agent-orchestration--format-message (msg)
  "Render MSG for an agent to read, ending with the command that answers it.
A message restored from the database may have outlived the terminal that sent
it, so the sender is resolved through its durable key: answerable when that
conversation is live again, named but not addressable when it is not."
  (let* ((live (bp/agent-orchestration--sender-terminal msg))
         (label (or (plist-get msg :from-label) (plist-get msg :from) "unknown")))
    (concat (format "--- [%s] from %s%s\n" (plist-get msg :type)
                    (or live label)
                    (if live
                        (bp/agent-orchestration--describe-sender live)
                      (format " (%s — that terminal is gone)" label)))
            (format "Subject: %s\n" (plist-get msg :subject))
            (let ((body (plist-get msg :body)))
              (if (and body (not (string-empty-p body))) (concat body "\n") ""))
            (let ((files (plist-get msg :files)))
              (if (and files (not (string-empty-p files)))
                  (format "Files: %s\n" files) ""))
            (let ((report (plist-get msg :report)))
              (if (and report (not (string-empty-p report)))
                  (format "Report: %s\n" report) ""))
            (if live
                (format "Reply: emacs-agent send --to %s --type answer --subject \"...\"\n"
                        live)
              "Reply: the sender's terminal is not live here; nothing to reply to.\n"))))

(defun bp/agent-orchestration--format-messages (messages)
  (concat (format "messages: %d\n" (length messages))
          (mapconcat #'bp/agent-orchestration--format-message messages "\n")))

;;;; Delivery into a resting terminal

(defun bp/agent-orchestration--receptive-p (session)
  "Non-nil when SESSION can be handed a message right now.

Two conditions, and both are stated as what they exclude, because the
alternative — enumerating the states that are fine — has already been wrong
once: `needs-attention' becomes `stopped' as soon as the user looks at the
terminal (`bp/agent-session--clear-attention-on-focus'), which silently ended
delivery to exactly the session an orchestrator is most likely to be watching.

- Not mid-turn.  `running' is the only status that means a turn is in progress;
  anything else is an agent sitting at its prompt, however it got there.
- Something holds the terminal's foreground.  Nil there means the shell's own
  prompt is in front, and a paste plus Enter would not reach a TUI at all — the
  *shell* would run the message as a command line.  That is not merely untidy:
  a message body is written by another agent, and executing it is a hazard the
  queue does not have.  Mail for a bare shell therefore stays queued for
  `emacs-agent check', which is the right way to read it anyway."
  (let ((buf (plist-get session :buffer)))
    (and (not (eq (plist-get session :status) 'running))
         (buffer-live-p buf)
         (bp/agent-sessions--foreground-command buf)
         t)))

(defun bp/agent-orchestration--paste (buf text)
  "Paste TEXT into BUF's terminal as one bracketed paste, then submit it.
Bracketed paste rather than plain input because a TUI submits on every newline
it is *typed*, which would fire the agent's turn on the message's first line and
feed it the rest as separate prompts."
  (with-current-buffer buf
    (pcase bp/agent-sessions-terminal
      ('vterm (vterm-send-string text t))
      ('eat (eat-term-send-string-as-yank eat-terminal text))))
  ;; The buffer travels as a timer argument, not in a closure: this file is
  ;; loaded with dynamic binding, so a lambda over `buf' would find it void
  ;; when the timer fires (the same reason `bp/agent-sessions-restore' passes
  ;; its row this way).
  (run-at-time
   bp/agent-orchestration--submit-delay nil
   (lambda (target)
     (when (buffer-live-p target)
       (with-current-buffer target
         (pcase bp/agent-sessions-terminal
           ('vterm (vterm-send-return))
           ('eat (eat-term-send-string eat-terminal "\r"))))))
   buf))

(defun bp/agent-orchestration-deliver (id)
  "Type ID's pending mail into its terminal, when that is the right thing to do.
Called both when a message arrives and from the hook path when a session comes
to rest, since either can be the moment the other condition becomes true.
Consuming before pasting is what keeps this idempotent across both callers."
  (when bp/agent-orchestration-push-when-idle
    (let* ((session (bp/agent-sessions--session-for-id id))
           (buf (and session (plist-get session :buffer))))
      (when (and session (buffer-live-p buf)
                 (bp/agent-orchestration--receptive-p session)
                 (bp/agent-orchestration--pending id))
        (if (bp/agent-orchestration--polling-p id)
            ;; The recipient read its inbox a moment ago, so it is mid-wait and
            ;; will collect this itself.  Look again once that stops being
            ;; true: without the retry, mail that lands just after a wait gives
            ;; up would sit unread until the next hook event happened along.
            (run-at-time (1+ bp/agent-orchestration-poll-grace) nil
                         #'bp/agent-orchestration-deliver id)
          (let ((pending (bp/agent-orchestration--pending id)))
            (bp/agent-orchestration--consume pending)
            (bp/agent-orchestration--paste
             buf (concat "Orchestration mail arrived while you were idle.\n\n"
                         (bp/agent-orchestration--format-messages pending)))
            (bp/agent-sessions--refresh-if-visible)))))))

;;;; The worker brief

(defun bp/agent-orchestration--worker-preamble (coordinator-id task)
  "The protocol a spawned worker is launched with, followed by TASK."
  (format "You are a worker agent in an Emacs-managed terminal, dispatched by another
agent — your coordinator.  You were started with this file as your prompt.

  Your terminal id   $EMACS_AGENT_SESSION_ID   (`emacs-agent' reads it for you)
  Your coordinator   %s                        (also $EMACS_AGENT_COORDINATOR)
  Your worktree      the directory this shell started in

=== HOW YOU REPORT ===

`emacs-agent' is on your PATH; `emacs-agent guide' prints the full reference.
It is your only channel to the coordinator — do not try to reach a human
through Slack, a PR comment, or anything else during the run.

  # REQUIRED when you finish, including when you fail or give up.  Send it
  # exactly once.  --summary is three sentences: what you did, what you found,
  # what is left.
  emacs-agent done --summary \"...\" [--files a.el,b.el] [--report path/to/notes.md]

  # Blocked on a decision only the coordinator can make.  Blocks until it
  # answers and prints the answer.
  emacs-agent ask --question \"...\" [--timeout 600]

  # A progress note, when there is something worth knowing mid-task.  Optional.
  emacs-agent send --type status --subject \"...\" --body \"...\"

RULES

- Never ask a question through an interactive prompt (no AskUserQuestion, no
  reading stdin).  Your coordinator is another agent and cannot see a TUI
  prompt; your turn would hang until a human noticed.  Use `emacs-agent ask'.
- Failure is still a `done', with a subject that says so.  Never exit silently.
- After `done', end your turn and sit at your prompt.  Do not poll for more
  work and do not start something unrelated; further work arrives as new input.
- You may be sharing this worktree with other workers.  Touch only the files
  your task names, and leave git state alone — no `git add', `commit',
  `checkout', `rebase' or `stash' — unless the task says this worktree is
  yours.  The index and HEAD are shared by everyone in a worktree, so staging
  anything would clobber a co-tenant's work in progress.
- `emacs-share-file FILE \"why\"' hands a file to the human in the dashboard.
  That is for the human, and is not a substitute for reporting `done'.

=== YOUR TASK ===

%s"
          coordinator-id (string-trim task)))

(defun bp/agent-orchestration--write-brief (coordinator-id task)
  "Write the brief for TASK to a file and return its path."
  (make-directory bp/agent-orchestration--brief-dir t)
  (let ((path (expand-file-name
               (format "%s-%s.md" coordinator-id
                       (bp/agent-orchestration--next-id "brief"))
               bp/agent-orchestration--brief-dir)))
    (with-temp-file path
      (insert (bp/agent-orchestration--worker-preamble coordinator-id task)))
    path))

;;;; Entry points (called from `bin/emacs-agent')

(defun bp/agent-orchestration-spawn (coordinator-id type worktree task &optional title)
  "Start a TYPE agent in WORKTREE on TASK, dispatched by COORDINATOR-ID.
Returns a string to print.  WORKTREE must already exist: allocating checkouts
is the orchestrator's job, not this package's, so that nothing here can leave a
stray worktree or branch behind."
  (let* ((coordinator (bp/agent-sessions--session-for-id coordinator-id))
         (agent (and type (not (string-empty-p type)) (intern type)))
         (template (and agent (alist-get agent bp/agent-orchestration-launch-commands)))
         (dir (and worktree (not (string-empty-p worktree))
                   (file-name-as-directory (expand-file-name worktree)))))
    (cond
     ((null coordinator)
      (format "No Emacs terminal registered for session id %s" coordinator-id))
     ((null template)
      (format "Unknown agent type `%s'; known types: %s" type
              (mapconcat (lambda (cell) (symbol-name (car cell)))
                         bp/agent-orchestration-launch-commands ", ")))
     ((or (null dir) (not (file-directory-p dir)))
      (format "No such worktree directory: %s (create it first, or pass one from `emacs-agent worktrees')"
              (or worktree "")))
     ((string-empty-p (string-trim (or task "")))
      "Refusing to spawn a worker with an empty --task")
     (t
      (let* ((brief (bp/agent-orchestration--write-brief coordinator-id task))
             (label (if (and title (not (string-empty-p (string-trim title))))
                        (string-trim title)
                      (bp/agent-orchestration--task-title task)))
             ;; `--create-terminal' shows the new terminal in another window,
             ;; which is right when the user pressed `+' and wrong when an agent
             ;; spawned three workers in a row: the layout the user is reading
             ;; must not be rearranged by a background action.  The buffer and
             ;; its process outlive the restored configuration.
             (buf (save-window-excursion
                    (let ((bp/agent-sessions--extra-terminal-env
                           (list "EMACS_AGENT_ROLE=worker"
                                 (format "EMACS_AGENT_COORDINATOR=%s" coordinator-id)
                                 (format "EMACS_AGENT_TASK_BRIEF=%s" brief))))
                      (bp/agent-sessions--create-terminal
                       dir (plist-get (bp/agent-session--repo-info dir) :worktree))))))
        (if (not (buffer-live-p buf))
            "Could not create a terminal for the worker"
          (let ((worker-id (buffer-local-value 'bp/agent-session-id buf))
                (parent-sid (plist-get coordinator :agent-session-id)))
            (with-current-buffer buf
              ;; Parentage rides the same slot a `b'-branch uses, which is what
              ;; makes the dashboard nest this row under its coordinator's; the
              ;; worker's first hook event picks it up from the buffer (see
              ;; `bp/agent-hook-notify').  It needs the coordinator's *agent*
              ;; session id, so a coordinator that has not fired a hook yet
              ;; simply gets a flat row — the spawn still works.
              (when parent-sid
                (setq-local bp/agent-session--branched-from
                            (cons parent-sid
                                  (bp/agent-sessions--session-short-label coordinator))))
              (setq-local bp/agent-orchestration-coordinator coordinator-id)
              ;; The task is what the human wants to read on the row, not
              ;; whatever title the agent's TUI is about to advertise.
              (setq-local bp/agent-session-title-override label))
            (bp/agent-sessions--terminal-send
             buf (format template (shell-quote-argument brief)))
            (bp/agent-sessions--refresh-if-visible)
            (format "Spawned %s (%s) in %s\nTask: %s\nBrief: %s"
                    worker-id type (abbreviate-file-name dir) label brief))))))))

(defun bp/agent-orchestration-send (from-id to type subject body files report)
  "Queue a message from FROM-ID to TO and try to deliver it.  Returns a string."
  (let ((type (if (or (null type) (string-empty-p type)) "status" type)))
    (cond
     ((null (bp/agent-sessions--session-for-id from-id))
      (format "No Emacs terminal registered for session id %s" from-id))
     ((not (member type bp/agent-orchestration-message-types))
      (format "Unknown message type `%s'; known types: %s" type
              (string-join bp/agent-orchestration-message-types ", ")))
     ((or (null to) (string-empty-p to))
      "A message needs a --to (an Emacs terminal id)")
     ((null (bp/agent-sessions--session-for-id to))
      (format "No live Emacs terminal for recipient %s" to))
     ((or (null subject) (string-empty-p (string-trim subject)))
      "A message needs a --subject")
     (t
      (let* ((sender (bp/agent-sessions--session-for-id from-id))
             (recipient (bp/agent-sessions--session-for-id to))
             (msg (list :id (bp/agent-orchestration--next-id "msg")
                        :from from-id
                        :from-key (bp/agent-orchestration--mail-key from-id sender)
                        :from-label (bp/agent-sessions--session-short-label sender)
                        :to to
                        :to-key (bp/agent-orchestration--mail-key to recipient)
                        :type type
                        :subject (string-trim subject)
                        :body (or body "") :files (or files "")
                        :report (or report "")
                        :sent-at (bp/agent-sessions--now) :read nil)))
        (bp/agent-orchestration--mail-insert msg)
        (puthash to (append (bp/agent-orchestration--messages to) (list msg))
                 bp/agent-orchestration--mail)
        (bp/agent-orchestration-deliver to)
        (bp/agent-sessions--refresh-if-visible)
        (format "Sent %s to %s" type to))))))

(defun bp/agent-orchestration-check (id types consume)
  "Return ID's unread mail, marking it read unless CONSUME is nil.
TYPES is a comma-separated filter or nil.  Recording the read is what tells
`bp/agent-orchestration-deliver' that this agent is collecting its own mail."
  (if (null (bp/agent-sessions--session-for-id id))
      (format "No Emacs terminal registered for session id %s" id)
    (bp/agent-orchestration--record-poll id)
    (bp/agent-orchestration--hydrate id)
    (let* ((filter (and types (not (string-empty-p types))
                        (split-string types "," t "[ \t]+")))
           (unknown (seq-remove
                     (lambda (candidate)
                       (member candidate bp/agent-orchestration-message-types))
                     (or filter '()))))
      (if unknown
          (format "Unknown message type(s) in --types: %s" (string-join unknown ", "))
        (let ((pending (bp/agent-orchestration--pending id filter)))
          (when consume (bp/agent-orchestration--consume pending))
          (when pending (bp/agent-sessions--refresh-if-visible))
          (bp/agent-orchestration--format-messages pending))))))

(defun bp/agent-orchestration-list (id)
  "Describe the workers ID spawned, and ID's own coordinator if it has one."
  (if (null (bp/agent-sessions--session-for-id id))
      (format "No Emacs terminal registered for session id %s" id)
    (let* ((entries (bp/agent-sessions--live-entries))
           (mine (seq-filter
                  (lambda (entry)
                    (let ((buf (plist-get (plist-get entry :session) :buffer)))
                      (and (buffer-live-p buf)
                           (equal id (buffer-local-value
                                      'bp/agent-orchestration-coordinator buf)))))
                  entries))
           (buf (plist-get (bp/agent-sessions--session-for-id id) :buffer))
           (my-coordinator (and (buffer-live-p buf)
                                (buffer-local-value
                                 'bp/agent-orchestration-coordinator buf))))
      (concat
       (if my-coordinator (format "your coordinator: %s\n" my-coordinator) "")
       (format "workers: %d\n" (length mine))
       (mapconcat
        (lambda (entry)
          (let* ((wid (plist-get entry :id))
                 (session (plist-get entry :session))
                 (unread (bp/agent-orchestration--pending-count wid)))
            (format "%s  %-7s %-15s %s%s\n"
                    wid
                    (plist-get session :agent-type)
                    (plist-get session :status)
                    (or (plist-get entry :title) "")
                    (if (> unread 0) (format "  (%d unread for it)" unread) ""))))
        mine "")))))

(defun bp/agent-orchestration-worktrees (id)
  "List the worktrees of ID's repo with what is running in each.
Reads git rather than the render path's cache: the caller is about to allocate
a worktree, and a cache that predates the last `git worktree add' would hand
out one that is already taken — or hide one that just became available."
  (let ((session (bp/agent-sessions--session-for-id id)))
    (if (null session)
        (format "No Emacs terminal registered for session id %s" id)
      (let* ((root (or (plist-get session :repo-root)
                       (let ((buf (plist-get session :buffer)))
                         (and (buffer-live-p buf)
                              (plist-get (bp/agent-sessions--buffer-repo-info buf)
                                         :repo-root)))))
             (worktrees (and root (bp/agent-sessions--compute-worktrees root)))
             (entries (bp/agent-sessions--live-entries)))
        (if (null worktrees)
            "Not inside a git repository Emacs knows about"
          (concat
           (format "repo: %s\n" (abbreviate-file-name root))
           (mapconcat
            (lambda (wt)
              (let* ((path (plist-get wt :path))
                     (canon (bp/agent-sessions--canonical-path path))
                     (busy (seq-filter
                            (lambda (entry)
                              (equal canon (bp/agent-sessions--canonical-path
                                            (plist-get (plist-get entry :session)
                                                       :worktree-path))))
                            entries))
                     (dirty (let ((default-directory path))
                              (ignore-errors (magit-anything-modified-p)))))
                ;; A count rather than free/busy: several agents can share a
                ;; worktree when their writes do not overlap, so what the
                ;; caller needs is who is there, not our verdict on whether it
                ;; may use the place.
                (format "%-9s %-40s %-28s %-6s%s\n"
                        (format "%d agent%s" (length busy)
                                (if (= (length busy) 1) "" "s"))
                        (abbreviate-file-name path)
                        (plist-get wt :label)
                        (if dirty "dirty" "clean")
                        (if busy
                            (concat "  "
                                    (mapconcat
                                     (lambda (entry)
                                       (format "%s(%s)"
                                               (plist-get (plist-get entry :session) :agent-type)
                                               (plist-get (plist-get entry :session) :status)))
                                     busy " "))
                          ""))))
            worktrees "")))))))

(define-key bp/agent-sessions-mode-map (kbd "RET") #'bp/agent-sessions-jump)
(define-key bp/agent-sessions-mode-map (kbd "v") #'bp/agent-sessions-display)
(define-key bp/agent-sessions-mode-map (kbd "k") #'bp/agent-sessions-kill)
(define-key bp/agent-sessions-mode-map (kbd "b") #'bp/agent-sessions-branch)
(define-key bp/agent-sessions-mode-map (kbd "B") #'bp/agent-sessions-mark-parent)
(define-key bp/agent-sessions-mode-map (kbd "t") #'bp/agent-sessions-edit-title)
(define-key bp/agent-sessions-mode-map (kbd "u") #'bp/agent-sessions-mark-unread)
(define-key bp/agent-sessions-mode-map (kbd "e") #'bp/agent-sessions-edit-note)
(define-key bp/agent-sessions-mode-map (kbd "R") #'bp/agent-sessions-restore)
(define-key bp/agent-sessions-mode-map (kbd "g") #'bp/agent-sessions-refresh)
(define-key bp/agent-sessions-mode-map (kbd "s") #'bp/agent-sessions-toggle-sort)
(define-key bp/agent-sessions-mode-map (kbd "+") #'bp/agent-sessions-new-vterm)
(define-key bp/agent-sessions-mode-map (kbd "n") #'bp/agent-sessions-next)
(define-key bp/agent-sessions-mode-map (kbd "p") #'bp/agent-sessions-prev)
(define-key bp/agent-sessions-mode-map (kbd "N") #'bp/agent-sessions-next-attention)
(define-key bp/agent-sessions-mode-map (kbd "P") #'bp/agent-sessions-prev-attention)
(define-key bp/agent-sessions-mode-map (kbd "M-n") #'bp/agent-sessions-move-down)
(define-key bp/agent-sessions-mode-map (kbd "M-p") #'bp/agent-sessions-move-up)
(define-key bp/agent-sessions-mode-map (kbd "C-c C-o") #'bp/agent-sessions-clear-manual-order)

;;; Wiring our hooks into Claude Code / Codex ------------------------------
;;
;; Both agents read a `{"hooks": {EVENT: [GROUP ...]}}' JSON config; each GROUP
;; is `{"hooks": [{"type":"command","command":SCRIPT,...}], ["matcher":M]}'.  We
;; own exactly the groups whose command runs one of our forwarder scripts (by
;; filename), so we can add/remove them idempotently while leaving every other
;; hook (orca's, the user's) untouched.

(defcustom bp/agent-sessions-claude-settings-file
  (expand-file-name "~/.claude/settings.json")
  "Claude Code user settings file that `bp/agent-sessions-install' wires."
  :type 'file)

(defcustom bp/agent-sessions-codex-hooks-file
  (expand-file-name "~/.codex/hooks.json")
  "Codex hooks file that `bp/agent-sessions-install' wires."
  :type 'file)

(defcustom bp/agent-sessions-claude-hook-events
  '("SessionStart" "SessionEnd" "UserPromptSubmit" "Stop" "StopFailure"
    "SubagentStop" "TeammateIdle"
    ("PreToolUse" . "*") ("PostToolUse" . "*") ("PermissionRequest" . "*"))
  "Claude hook events to forward to the dashboard.
Each item is either an event name, or (EVENT . MATCHER) for tool-scoped events.
`SessionEnd' earns its place by closing the session's log row the moment the
agent exits, so `R' can offer it back while its terminal is still sitting
there at a shell prompt.  Codex has no equivalent event; there the end is
noticed when the terminal dies or another session starts in it."
  :type '(repeat (choice string (cons string string))))

(defcustom bp/agent-sessions-codex-hook-events
  '("SessionStart" "UserPromptSubmit" "PreToolUse" "PostToolUse"
    "PermissionRequest" "Stop")
  "Codex hook events to forward to the dashboard."
  :type '(repeat (choice string (cons string string))))

(defconst bp/agent-sessions--claude-hook-script "agent-sessions-claude-hook.sh")
(defconst bp/agent-sessions--codex-hook-script "agent-sessions-codex-hook.sh")

(defun bp/agent-sessions--read-json (file)
  "Parse FILE as JSON into hash-tables + lists, or an empty object if absent.
Signals a `user-error' (rather than clobbering) if FILE holds invalid JSON."
  (if (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; Arrays as vectors (not lists): native `json-serialize' treats a
            ;; Lisp list as an alist/plist object, so JSON arrays must round-trip
            ;; as vectors to come back out as arrays.
            (json-parse-buffer :object-type 'hash-table :array-type 'array
                               :null-object :null :false-object :false))
        (error (user-error "agent-sessions: %s is not valid JSON (%s); left untouched"
                           file (error-message-string err))))
    (make-hash-table :test 'equal)))

(defun bp/agent-sessions--write-json (file obj)
  "Write OBJ to FILE as pretty-printed JSON, backing up any existing FILE."
  (make-directory (file-name-directory file) t)
  (when (file-exists-p file)
    (copy-file file (concat file ".bak") t))
  (with-temp-file file
    (insert (json-serialize obj :null-object :null :false-object :false))
    (json-pretty-print-buffer)))

(defun bp/agent-sessions--our-group-p (group script)
  "Non-nil if hook GROUP runs our SCRIPT (matched by basename)."
  (let ((cmds (and (hash-table-p group) (gethash "hooks" group))))
    (and (vectorp cmds)
         (seq-some (lambda (h)
                     (let ((c (and (hash-table-p h) (gethash "command" h))))
                       (and (stringp c) (string-match-p (regexp-quote script) c))))
                   cmds))))

(defun bp/agent-sessions--make-group (script matcher)
  "Build a hook GROUP hash-table running SCRIPT, with optional MATCHER."
  (let ((cmd (make-hash-table :test 'equal))
        (grp (make-hash-table :test 'equal)))
    (puthash "type" "command" cmd)
    (puthash "command" script cmd)
    (puthash "timeout" 10 cmd)
    (when matcher (puthash "matcher" matcher grp))
    (puthash "hooks" (vector cmd) grp)
    grp))

(defun bp/agent-sessions--strip-hooks (events-map script)
  "Remove our SCRIPT groups from EVENTS-MAP, dropping events left empty.
Returns t when anything was removed."
  (let (changed drop)
    (maphash
     (lambda (event groups)
       (let* ((orig (if (vectorp groups) (append groups nil) nil))
              (kept (seq-remove (lambda (g) (bp/agent-sessions--our-group-p g script))
                                orig)))
         (unless (= (length kept) (length orig)) (setq changed t))
         (if kept (puthash event (vconcat kept) events-map)
           (push event drop))))
     events-map)
    (dolist (event drop) (remhash event events-map))
    changed))

(defun bp/agent-sessions--wire (file events script)
  "Add SCRIPT hook groups for EVENTS into FILE's `hooks' map, idempotently.
Strips any stale groups of ours first (so re-running after moving the package
or editing the event list self-heals), then appends fresh ones."
  (let* ((root (bp/agent-sessions--read-json file))
         (hooks (let ((h (gethash "hooks" root)))
                  (if (hash-table-p h) h (make-hash-table :test 'equal)))))
    (bp/agent-sessions--strip-hooks hooks script)
    (dolist (ev events)
      (let* ((event (if (consp ev) (car ev) ev))
             (matcher (and (consp ev) (cdr ev)))
             (existing (gethash event hooks))
             (base (if (vectorp existing) existing [])))
        (puthash event
                 (vconcat base (vector (bp/agent-sessions--make-group script matcher)))
                 hooks)))
    (puthash "hooks" hooks root)
    (bp/agent-sessions--write-json file root)))

(defun bp/agent-sessions--unwire (file script)
  "Remove our SCRIPT hook groups from FILE, leaving other hooks intact.
Returns t if FILE was changed."
  (when (file-exists-p file)
    (let* ((root (bp/agent-sessions--read-json file))
           (hooks (gethash "hooks" root)))
      (when (and (hash-table-p hooks)
                 (bp/agent-sessions--strip-hooks hooks script))
        (if (zerop (hash-table-count hooks))
            (remhash "hooks" root)
          (puthash "hooks" hooks root))
        (bp/agent-sessions--write-json file root)
        t))))

;;;###autoload
(defun bp/agent-sessions-install (&optional force)
  "Wire this package's hooks into Claude Code and Codex.
So a `claude'/`codex' run in any Emacs terminal reports to the dashboard,
without depending on any external tool (e.g. orca).  Idempotent — re-run any
time: after installing a second agent, moving this package, changing the hook
event lists, or removing orca.  Only touches hook entries pointing at this
package's own forwarder scripts; other hooks are left as-is.

Wires Claude when `~/.claude' exists and Codex when `~/.codex' exists; with a
prefix arg (FORCE) wires both regardless, creating the config as needed.  Also
starts the Emacs server, which the hooks need to reach us."
  (interactive "P")
  (let ((claude-script (expand-file-name bp/agent-sessions--claude-hook-script
                                         bp/agent-sessions--dir))
        (codex-script (expand-file-name bp/agent-sessions--codex-hook-script
                                        bp/agent-sessions--dir))
        done)
    (dolist (s (list claude-script codex-script))
      (if (file-exists-p s)
          (set-file-modes s #o755)
        (error "agent-sessions: missing hook script %s" s)))
    (when (or force (file-directory-p
                     (file-name-directory bp/agent-sessions-claude-settings-file)))
      (bp/agent-sessions--wire bp/agent-sessions-claude-settings-file
                               bp/agent-sessions-claude-hook-events claude-script)
      (push "Claude" done))
    (when (or force (file-directory-p
                     (file-name-directory bp/agent-sessions-codex-hooks-file)))
      (bp/agent-sessions--wire bp/agent-sessions-codex-hooks-file
                               bp/agent-sessions-codex-hook-events codex-script)
      (push "Codex" done))
    (unless (server-running-p) (server-start))
    (if done
        (message "agent-sessions: wired %s (server running)"
                 (string-join (nreverse done) " + "))
      (message "agent-sessions: neither ~/.claude nor ~/.codex found; use `C-u M-x bp/agent-sessions-install' to force"))))

;;;###autoload
(defun bp/agent-sessions-uninstall ()
  "Remove this package's hooks from Claude Code and Codex config.
Leaves every other hook (orca's, yours) intact, and leaves the forwarder
scripts on disk.  Safe to run whether or not `bp/agent-sessions-install' ran."
  (interactive)
  (let ((changed (delq nil
                       (list (and (bp/agent-sessions--unwire
                                   bp/agent-sessions-claude-settings-file
                                   bp/agent-sessions--claude-hook-script)
                                  "Claude")
                             (and (bp/agent-sessions--unwire
                                   bp/agent-sessions-codex-hooks-file
                                   bp/agent-sessions--codex-hook-script)
                                  "Codex")))))
    (message (if changed
                 (format "agent-sessions: unhooked from %s" (string-join changed " + "))
               "agent-sessions: nothing to remove"))))

;;;###autoload
(defun bp/agent-session-setup ()
  "Install all side effects this package makes on systems outside itself.
Namely: the vterm/eat advice (session-id injection + terminal-title capture),
the focus hook that clears the needs-attention highlight, the `agent-session'
Org link type, the `C-x p a' project binding, and starting the Emacs server
\(hook scripts reach us via emacsclient).  Idempotent; call from init/config."
  ;; vterm: inject EMACS_AGENT_SESSION_ID and capture the terminal title.
  (with-eval-after-load 'vterm
    (unless (advice-member-p #'bp/agent-sessions--vterm-advice 'vterm--internal)
      (advice-add 'vterm--internal :around #'bp/agent-sessions--vterm-advice))
    (unless (advice-member-p #'bp/agent-sessions--capture-title 'vterm--set-title)
      (advice-add 'vterm--set-title :before #'bp/agent-sessions--capture-title)))
  ;; eat: the same id injection and title capture.
  (with-eval-after-load 'eat
    (unless (advice-member-p #'bp/agent-sessions--eat-advice 'eat--1)
      (advice-add 'eat--1 :around #'bp/agent-sessions--eat-advice))
    (unless (advice-member-p #'bp/agent-sessions--eat-capture-title 'eat--t-set-title)
      (advice-add 'eat--t-set-title :after #'bp/agent-sessions--eat-capture-title)))
  ;; Clear a session's needs-attention highlight once the user focuses it.
  (add-hook 'buffer-list-update-hook #'bp/agent-session--clear-attention-on-focus)
  ;; Record that sessions still running at exit were *not* closed by the user.
  ;; This is the half of the close-detection scheme that runs when Emacs goes
  ;; away in an orderly fashion; `kill-buffer-hook' covers the other half, and
  ;; the fact that Emacs runs one but not the other is what distinguishes them.
  (add-hook 'kill-emacs-hook #'bp/agent-sessions--log-close-all)
  ;; A periodic heartbeat so a session lost to a *crash* can still be dated:
  ;; nothing runs at that point, so the last time we know Emacs was alive is
  ;; the best available answer.
  (unless bp/agent-sessions--heartbeat-timer
    (setq bp/agent-sessions--heartbeat-timer
          (run-with-timer 60 60 #'bp/agent-sessions--db-heartbeat)))
  ;; `agent-session:' Org links that resume/jump to a session.
  (with-eval-after-load 'ol
    (org-link-set-parameters "agent-session"
                             :follow #'bp/agent-sessions--org-follow-link
                             :store #'bp/agent-sessions--org-store-link))
  ;; C-x p a: switch between / create agent sessions for the current project.
  (with-eval-after-load 'project
    (define-key project-prefix-map (kbd "a") #'bp/agent-session-switch-or-new))
  ;; Hook scripts forward events via emacsclient, so the server must be up.
  (unless (server-running-p)
    (server-start)))

;;;###autoload
(defun bp/agent-sessions-list ()
  "Open the dashboard of Claude/Codex sessions running in vterm."
  (interactive)
  (let ((buf (get-buffer-create bp/agent-sessions-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'bp/agent-sessions-mode)
        (bp/agent-sessions-mode))
      (bp/agent-sessions--refresh))
    (pop-to-buffer buf)))

(provide 'agent-sessions)
