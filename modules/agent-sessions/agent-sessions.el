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
(require 'magit)
(require 'magit-section)
(require 'project)

(defconst bp/agent-sessions--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "agent-sessions") default-directory))
  "Directory this package lives in; its hook forwarder scripts sit beside it.")

(defvar bp/agent-session-id->buffer (make-hash-table :test 'equal)
  "Session id -> vterm buffer, populated for every vterm buffer at creation.")

(defvar bp/agent-sessions (make-hash-table :test 'equal)
  "Session id -> plist (:buffer :agent-type :status :last-event :updated-at :repo :worktree).
Populated lazily: an entry only exists once a hook event has fired for it.")

(defvar bp/agent-session-counter 0)

(defvar-local bp/agent-session-id nil
  "Unique id injected into this vterm buffer's shell environment, if any.")

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
Otherwise the default order is used.  Toggle with `s'
\(`bp/agent-sessions-toggle-sort').")

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
             (worktree-label (if (file-equal-p toplevel main-root)
                                  (or branch "main")
                                (or branch (file-name-nondirectory
                                            (directory-file-name toplevel))))))
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
                        (label (if (file-equal-p path root)
                                   (or branch "main")
                                 (or branch (file-name-nondirectory
                                             (directory-file-name path))))))
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
    (remhash bp/agent-session-id bp/agent-session-id->buffer)
    (remhash bp/agent-session-id bp/agent-sessions)
    (bp/agent-sessions--refresh-if-visible)))

(defun bp/agent-sessions--vterm-advice (orig-fun pop-to-buf-fun &optional arg)
  (let* ((id (format "%d-%d" (emacs-pid) (cl-incf bp/agent-session-counter)))
         (vterm-environment (cons (format "EMACS_AGENT_SESSION_ID=%s" id) vterm-environment))
         (buf (funcall orig-fun pop-to-buf-fun arg)))
    (when (buffer-live-p buf)
      (puthash id buf bp/agent-session-id->buffer)
      (with-current-buffer buf
        (setq-local bp/agent-session-id id)
        (add-hook 'kill-buffer-hook #'bp/agent-session--cleanup nil t))
      (bp/agent-sessions--refresh-if-visible))
    buf))

(defvar-local bp/agent-session-title nil
  "Terminal title most recently set by this vterm's process (OSC escape), if any.
Agents typically set this to something like the current task/session summary.")

(defvar-local bp/agent-session-title-override nil
  "A user-chosen title for this session, set via the dashboard's `e' command.
When non-nil it takes precedence over `bp/agent-session-title' everywhere the
session is labelled, so the agent's OSC title updates don't clobber it.")

(defvar-local bp/agent-session--branched-from nil
  "When non-nil, a cons (PARENT-SID . LABEL) recording the session this one was
branched/forked from.  Set on a freshly branched terminal (by the `b' command
or `bp/agent-sessions-mark-parent') so its agent session records the parentage
once its first hook fires, and preserved across subsequent hook events.")

(defun bp/agent-sessions--capture-title (title)
  (setq-local bp/agent-session-title title))

(defun bp/agent-sessions--eat-advice (orig-fun program arg display-fn)
  "Give eat sessions the same id injection/registration as vterm.
Injects EMACS_AGENT_SESSION_ID via `process-environment' (which `eat-exec'
inherits when it spawns the shell) and registers the resulting buffer."
  (let* ((id (format "%d-%d" (emacs-pid) (cl-incf bp/agent-session-counter)))
         (process-environment
          (cons (format "EMACS_AGENT_SESSION_ID=%s" id) process-environment))
         (buf (funcall orig-fun program arg display-fn)))
    (when (buffer-live-p buf)
      (puthash id buf bp/agent-session-id->buffer)
      (with-current-buffer buf
        (setq-local bp/agent-session-id id)
        (add-hook 'kill-buffer-hook #'bp/agent-session--cleanup nil t))
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
         (id (and (buffer-live-p buf) (buffer-local-value 'bp/agent-session-id buf))))
    (when id
      (let ((session (gethash id bp/agent-sessions)))
        (when (and session (eq (plist-get session :status) 'needs-attention))
          (puthash id (plist-put session :status 'stopped) bp/agent-sessions)
          (bp/agent-sessions--refresh-if-visible))))))

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
        ;; Notify only on the transition *into* an attention state, so a
        ;; session that stays waiting isn't re-announced on every later event.
        (when (and bp/agent-session-notify-on-attention
                   (bp/agent-sessions--attention-p status)
                   (not (bp/agent-sessions--attention-p
                         (and existing (plist-get existing :status)))))
          (bp/agent-session--notify-attention buf agent-type info status))))
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

(defun bp/agent-sessions--terminal-session (buf)
  "Synthesize a session plist for a plain terminal BUF with no agent session.
These fill in for vterm/eat buffers that have not (yet) fired an agent hook,
so idle terminals still appear in the dashboard tree."
  (let ((info (bp/agent-sessions--buffer-repo-info buf)))
    (list :buffer buf
          :agent-type (bp/agent-sessions--buffer-terminal-type buf)
          :status 'idle
          :last-event nil
          :updated-at nil
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
             (push (list :id id :session session
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
                          :session (bp/agent-sessions--terminal-session buf)
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
    (magit-insert-section (bp/agent-session-row id)
      (insert (propertize
               (concat "    " indent marker
                       (format "%-7s %-16s %s%s%s\n"
                               (plist-get session :agent-type)
                               status
                               title
                               (if last-event (format " (%s)" last-event) "")
                               (if (and branched-from (not suppress-parent-note))
                                   (format "  ↳ from %s" (cdr branched-from))
                                 "")))
               'font-lock-face face)))))

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

(defun bp/agent-sessions--build-tree (entries)
  "Group live ENTRIES into a list of repo plists for rendering.
Each element is (:name NAME :root ROOT :worktrees (WORKTREE ...)).
When `bp/agent-sessions-sort-by-activity' is set, every level is ordered
most-recently-active first."
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
      repos)))

(defun bp/agent-sessions--insert-worktree (wt)
  (let ((entries (plist-get wt :entries)))
    (magit-insert-section (bp/agent-session-worktree
                           (list :label (plist-get wt :label)
                                 :path (plist-get wt :path)))
      (magit-insert-heading (format "  %s" (plist-get wt :label)))
      (bp/agent-sessions--insert-entries entries))))

(defun bp/agent-sessions--insert-repo (repo)
  (magit-insert-section (bp/agent-session-repo
                         (list :name (plist-get repo :name)
                               :root (plist-get repo :root)))
    (magit-insert-heading (propertize (plist-get repo :name) 'face 'bold))
    (dolist (wt (plist-get repo :worktrees))
      (bp/agent-sessions--insert-worktree wt))
    (insert "\n")))

(defun bp/agent-sessions--refresh ()
  (interactive)
  (let* ((inhibit-read-only t)
         (section (magit-current-section))
         ;; Remember the section at point by its stable identity, not by
         ;; absolute position: erase-buffer + rebuild (and sorting) shift and
         ;; reorder everything, so a char offset is meaningless.
         (ident (and section (magit-section-ident section)))
         (win (get-buffer-window (current-buffer)))
         (wstart (and win (window-start win))))
    (erase-buffer)
    (magit-insert-section (bp/agent-sessions-root)
      (let ((repos (bp/agent-sessions--build-tree (bp/agent-sessions--live-entries))))
        (if (null repos)
            (insert "No active sessions.\n")
          (dolist (repo repos)
            (bp/agent-sessions--insert-repo repo)))))
    (let ((target (and ident (magit-get-section ident))))
      (goto-char (if target (oref target start) (point-min))))
    (when win
      (set-window-point win (point))
      (when wstart
        (set-window-start win (min wstart (point-max)) t)))))

(defun bp/agent-sessions-refresh ()
  "Refresh the dashboard, re-reading git worktrees from disk.
Unlike the internal re-render used by hooks and the sort toggle, this
invalidates the worktree cache."
  (interactive)
  (clrhash bp/agent-sessions--worktrees-cache)
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

(defun bp/agent-sessions-jump ()
  "Act on the thing at point.
On a session row, pop to its vterm buffer.  On a worktree or repo heading,
run `project-switch-project' in that directory."
  (interactive)
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
             (project-switch-project dir)
           (message "No worktree directory at point."))))
      (_ (message "Nothing to do here.")))))

(defun bp/agent-sessions-display ()
  "Display the session at point in another window, staying in the dashboard."
  (interactive)
  (let* ((session (bp/agent-sessions--session-at-point))
         (buf (and session (plist-get session :buffer))))
    (if (buffer-live-p buf)
        ;; inhibit-same-window: never reuse/replace the dashboard's own window.
        (display-buffer buf '(nil (inhibit-same-window . t)))
      (message "No session at point."))))

(defun bp/agent-sessions-kill ()
  (interactive)
  (let* ((session (bp/agent-sessions--session-at-point))
         (buf (and session (plist-get session :buffer))))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (bp/agent-sessions--refresh)))

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

(defun bp/agent-sessions-new-vterm ()
  "Create a new vterm in the worktree at point, named after that worktree."
  (interactive)
  (let* ((ctx (bp/agent-sessions--context-at-point))
         (dir (plist-get ctx :path)))
    (if (or (null dir) (not (file-directory-p dir)))
        (message "No worktree at point.")
      (bp/agent-sessions--create-terminal dir (plist-get ctx :name)))))

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
  "Return (LINE-POS . STATUS) for each session row, in buffer order."
  (let (rows)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((sec (magit-current-section)))
          (when (and sec (eq (oref sec type) 'bp/agent-session-row))
            (let ((session (gethash (oref sec value) bp/agent-sessions)))
              (push (cons (line-beginning-position)
                          (and session (plist-get session :status)))
                    rows))))
        (forward-line 1)))
    (nreverse rows)))

(defun bp/agent-sessions--goto-row (direction predicate)
  "Move point to the next/previous session row (DIRECTION is `next' or `prev').
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
          ((null positions) (message "No matching session."))
          (t (message "No %s session." (if (eq direction 'next) "next" "previous"))))))

(defun bp/agent-sessions-next ()
  "Move to the next session row."
  (interactive)
  (bp/agent-sessions--goto-row 'next nil))

(defun bp/agent-sessions-prev ()
  "Move to the previous session row."
  (interactive)
  (bp/agent-sessions--goto-row 'prev nil))

(defun bp/agent-sessions-toggle-sort ()
  "Toggle the dashboard between activity order and default order."
  (interactive)
  (setq bp/agent-sessions-sort-by-activity
        (not bp/agent-sessions-sort-by-activity))
  (bp/agent-sessions--refresh)
  (message "Sorting by %s"
           (if bp/agent-sessions-sort-by-activity
               "most recent activity"
             "default order")))

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
           (worktree (plist-get session :worktree))
           (title (let ((buf (plist-get session :buffer)))
                    (and (buffer-live-p buf)
                         (buffer-local-value 'bp/agent-session-title buf)))))
      (when (and path sid)
        (org-link-store-props
         :type "agent-session"
         :link (format "agent-session:%s::%s::%s"
                       agent sid (file-name-as-directory path))
         :description (format "%s %s %s" (or worktree "?") agent
                              (or title sid)))
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

(define-key bp/agent-sessions-mode-map (kbd "RET") #'bp/agent-sessions-jump)
(define-key bp/agent-sessions-mode-map (kbd "v") #'bp/agent-sessions-display)
(define-key bp/agent-sessions-mode-map (kbd "k") #'bp/agent-sessions-kill)
(define-key bp/agent-sessions-mode-map (kbd "b") #'bp/agent-sessions-branch)
(define-key bp/agent-sessions-mode-map (kbd "B") #'bp/agent-sessions-mark-parent)
(define-key bp/agent-sessions-mode-map (kbd "e") #'bp/agent-sessions-edit-title)
(define-key bp/agent-sessions-mode-map (kbd "g") #'bp/agent-sessions-refresh)
(define-key bp/agent-sessions-mode-map (kbd "s") #'bp/agent-sessions-toggle-sort)
(define-key bp/agent-sessions-mode-map (kbd "+") #'bp/agent-sessions-new-vterm)
(define-key bp/agent-sessions-mode-map (kbd "n") #'bp/agent-sessions-next)
(define-key bp/agent-sessions-mode-map (kbd "p") #'bp/agent-sessions-prev)
(define-key bp/agent-sessions-mode-map (kbd "N") #'bp/agent-sessions-next-attention)
(define-key bp/agent-sessions-mode-map (kbd "P") #'bp/agent-sessions-prev-attention)

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
  '("SessionStart" "UserPromptSubmit" "Stop" "StopFailure"
    "SubagentStop" "TeammateIdle"
    ("PreToolUse" . "*") ("PostToolUse" . "*") ("PermissionRequest" . "*"))
  "Claude hook events to forward to the dashboard.
Each item is either an event name, or (EVENT . MATCHER) for tool-scoped events."
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
