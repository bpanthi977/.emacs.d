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

(defvar bp/agent-session-id->buffer (make-hash-table :test 'equal)
  "Session id -> vterm buffer, populated for every vterm buffer at creation.")

(defvar bp/agent-sessions (make-hash-table :test 'equal)
  "Session id -> plist (:buffer :agent-type :status :last-event :updated-at :repo :worktree).
Populated lazily: an entry only exists once a hook event has fired for it.")

(defvar bp/agent-session-counter 0)

(defvar-local bp/agent-session-id nil
  "Unique id injected into this vterm buffer's shell environment, if any.")

(defconst bp/agent-sessions-buffer-name "*Agent Sessions*")

(defcustom bp/agent-sessions-show-all-worktrees nil
  "When non-nil, list every worktree of a repo, even those with no session.
This makes it easy to spin up a new session in an idle worktree with the
`+' key (`bp/agent-sessions-new-vterm').  Only repos that already have at
least one session are shown; the option controls whether their empty
worktrees are listed too."
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
        (add-hook 'kill-buffer-hook #'bp/agent-session--cleanup nil t)))
    buf))

(with-eval-after-load 'vterm
  (unless (advice-member-p #'bp/agent-sessions--vterm-advice 'vterm--internal)
    (advice-add 'vterm--internal :around #'bp/agent-sessions--vterm-advice)))

(defvar-local bp/agent-session-title nil
  "Terminal title most recently set by this vterm's process (OSC escape), if any.
Agents typically set this to something like the current task/session summary.")

(defun bp/agent-sessions--capture-title (title)
  (setq-local bp/agent-session-title title))

(with-eval-after-load 'vterm
  (unless (advice-member-p #'bp/agent-sessions--capture-title 'vterm--set-title)
    (advice-add 'vterm--set-title :before #'bp/agent-sessions--capture-title)))

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

(add-hook 'buffer-list-update-hook #'bp/agent-session--clear-attention-on-focus)

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
                               (and existing (plist-get existing :agent-session-id)))))
    (when (buffer-live-p buf)
      (let ((info (bp/agent-session--repo-info (buffer-local-value 'default-directory buf))))
        (puthash session-id
                 (list :buffer buf
                       :agent-type agent-type
                       :agent-session-id agent-session-id
                       :status status
                       :last-event event-name
                       :updated-at (current-time)
                       :repo (plist-get info :repo)
                       :repo-root (plist-get info :repo-root)
                       :worktree (plist-get info :worktree)
                       :worktree-path (plist-get info :worktree-path))
                 bp/agent-sessions)))
    (bp/agent-sessions--refresh-if-visible))
  nil)

(defun bp/agent-sessions--live-entries ()
  "Return (:id ID :session PLIST :title TITLE) for each session with a live buffer.
Opportunistically drops registry entries whose buffer has been killed,
since `kill-buffer-hook' cleanup can be missed (e.g. buffer killed without
running local hooks)."
  (let (entries)
    (maphash
     (lambda (id session)
       (let ((buf (plist-get session :buffer)))
         (if (buffer-live-p buf)
             (push (list :id id :session session
                         :title (buffer-local-value 'bp/agent-session-title buf))
                   entries)
           (remhash id bp/agent-sessions)
           (remhash id bp/agent-session-id->buffer))))
     bp/agent-sessions)
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

(defun bp/agent-sessions--insert-session (entry)
  (let* ((id (plist-get entry :id))
         (session (plist-get entry :session))
         (status (plist-get session :status))
         (face (bp/agent-sessions--face-for status))
         (marker (pcase status
                   ('needs-attention "● ")
                   ('error "✖ ")
                   (_ "  ")))
         (title (or (plist-get entry :title) "")))
    (magit-insert-section (bp/agent-session-row id)
      (insert (propertize
               (concat "    " marker
                       (format "%-7s %-16s %s (%s)\n"
                               (plist-get session :agent-type)
                               status
                               title
                               (plist-get session :last-event)))
               'font-lock-face face)))))

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
      (mapc #'bp/agent-sessions--insert-session entries))))

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
         (gethash (oref section value) bp/agent-sessions))))

(defun bp/agent-sessions-jump ()
  "Act on the thing at point.
On a session row, pop to its vterm buffer.  On a worktree or repo heading,
run `project-switch-project' in that directory."
  (interactive)
  (let ((section (magit-current-section)))
    (pcase (and section (oref section type))
      ('bp/agent-session-row
       (let* ((session (gethash (oref section value) bp/agent-sessions))
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

(defun bp/agent-sessions-kill ()
  (interactive)
  (let* ((session (bp/agent-sessions--session-at-point))
         (buf (and session (plist-get session :buffer))))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (bp/agent-sessions--refresh)))

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

(defun bp/agent-sessions--create-vterm (dir &optional name)
  "Open a new vterm in DIR named <NAME>-<n>-vterm.
NAME defaults to DIR's basename (typically the worktree name)."
  (let* ((default-directory (file-name-as-directory dir))
         (base (or name (file-name-nondirectory (directory-file-name dir)))))
    (vterm-other-window (format "%s-vterm" base))))

(defun bp/agent-sessions-new-vterm ()
  "Create a new vterm in the worktree at point, named after that worktree."
  (interactive)
  (let* ((ctx (bp/agent-sessions--context-at-point))
         (dir (plist-get ctx :path)))
    (if (or (null dir) (not (file-directory-p dir)))
        (message "No worktree at point.")
      (bp/agent-sessions--create-vterm dir (plist-get ctx :name)))))

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
      (bp/agent-sessions--create-vterm
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

;;;###autoload
(defun bp/agent-session-start (worktree-path agent-type session-id)
  "Open a vterm in WORKTREE-PATH and resume AGENT-TYPE's SESSION-ID.
AGENT-TYPE is a symbol such as `claude' or `codex'.  This is the target of
the `elisp:' links produced by `org-store-link' on a session row."
  (let* ((agent (if (symbolp agent-type) agent-type (intern agent-type)))
         (template (alist-get agent bp/agent-session-resume-commands))
         (default-directory (file-name-as-directory worktree-path)))
    (unless template
      (error "No resume command configured for agent type `%s'" agent))
    (unless (file-directory-p default-directory)
      (error "Worktree no longer exists: %s" worktree-path))
    (let ((buf (bp/agent-sessions--create-vterm
                default-directory
                (plist-get (bp/agent-session--repo-info default-directory) :worktree))))
      (with-current-buffer buf
        (vterm-send-string (format template session-id))
        (vterm-send-return))
      buf)))

(defun bp/agent-sessions--org-store-link (&optional _interactive)
  "Store an `elisp:' link that resumes the session on the current row.
Registered as the `:store' handler for `org-store-link' in
`bp/agent-sessions-mode'."
  (when (derived-mode-p 'bp/agent-sessions-mode)
    (let ((session (bp/agent-sessions--session-at-point)))
      (when session
        (let* ((path (or (plist-get session :worktree-path)
                         (let ((buf (plist-get session :buffer)))
                           (and (buffer-live-p buf)
                                (buffer-local-value 'default-directory buf)))))
               (agent (plist-get session :agent-type))
               (sid (plist-get session :agent-session-id))
               (worktree (plist-get session :worktree)))
          (when (and path sid)
            (org-link-store-props
             :type "elisp"
             :link (format "elisp:(bp/agent-session-start %S '%s %S)"
                           (file-name-as-directory path) agent sid)
             :description (format "%s %s %s" (or worktree "?") agent sid))
            t))))))

(with-eval-after-load 'ol
  (org-link-set-parameters "agent-session"
                           :store #'bp/agent-sessions--org-store-link))

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
(define-key bp/agent-sessions-mode-map (kbd "k") #'bp/agent-sessions-kill)
(define-key bp/agent-sessions-mode-map (kbd "g") #'bp/agent-sessions-refresh)
(define-key bp/agent-sessions-mode-map (kbd "s") #'bp/agent-sessions-toggle-sort)
(define-key bp/agent-sessions-mode-map (kbd "+") #'bp/agent-sessions-new-vterm)
(define-key bp/agent-sessions-mode-map (kbd "n") #'bp/agent-sessions-next)
(define-key bp/agent-sessions-mode-map (kbd "p") #'bp/agent-sessions-prev)
(define-key bp/agent-sessions-mode-map (kbd "N") #'bp/agent-sessions-next-attention)
(define-key bp/agent-sessions-mode-map (kbd "P") #'bp/agent-sessions-prev-attention)

;; C-x p a: switch between / create agent sessions for the current project.
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "a") #'bp/agent-session-switch-or-new))

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
