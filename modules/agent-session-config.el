(use-package agent-sessions
  :load-path "modules/agent-sessions"
  :bind (:map bp/global-prefix-map
	      (("a" . bp/agent-sessions-list)))
  :defer nil
  :config
  (setf bp/agent-sessions-show-all-worktrees t)
  (setf bp/agent-sessions-terminal 'eat)
  (add-to-list 'project-switch-commands '(bp/agent-session-switch-or-new "Agent Session"))

  ;; Let M-m (bp/global-prefix) reach Emacs instead of being swallowed by vterm.
  ;; `vterm-keymap-exceptions' keys are removed from `vterm-mode-map'; its :set
  ;; rebuilds the map, so use setopt (not setq) to apply the change.
  (with-eval-after-load 'vterm
    (unless (member "M-m" vterm-keymap-exceptions)
      (setopt vterm-keymap-exceptions (cons "M-m" vterm-keymap-exceptions))))

  ;; Same for eat: keys in `eat-semi-char-non-bound-keys' are NOT sent to the
  ;; terminal in semi-char mode, so they fall through to Emacs.  eat spells
  ;; M-KEY as [?\e KEY]; its :set rebuilds the keymap, so use setopt.
  (with-eval-after-load 'eat
    (unless (member [?\e ?m] eat-semi-char-non-bound-keys)
      (setopt eat-semi-char-non-bound-keys
              (cons [?\e ?m] eat-semi-char-non-bound-keys))))

  ;; Install the advice, hooks, Org link type, project binding, and server.
  (bp/agent-session-setup)

  ;; One-time (re-run any time) wiring of our hooks into Claude Code / Codex, so
  ;; sessions report to the dashboard without depending on orca.  Run manually
  ;; via `M-x bp/agent-sessions-install' (and `-uninstall' to remove them).
  ;; It writes ~/.claude/settings.json and ~/.codex/hooks.json, so it is left
  ;; explicit rather than run on every startup.
  )
