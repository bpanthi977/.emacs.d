(use-package agent-sessions
  :load-path "modules"
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

  ;; Hook scripts forward events via emacsclient, so the Emacs server must be
  ;; running for the dashboard to receive them.
  (unless (server-running-p)
    (server-start)))
