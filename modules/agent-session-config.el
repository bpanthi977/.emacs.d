(use-package agent-sessions
  :load-path "modules"
  :defer nil
  :config
  (setf bp/agent-sessions-show-all-worktrees t)
  (add-to-list 'project-switch-commands '(bp/agent-session-switch-or-new "Agent Session"))
  ;; Hook scripts forward events via emacsclient, so the Emacs server must be
  ;; running for the dashboard to receive them.
  (unless (server-running-p)
    (server-start)))
