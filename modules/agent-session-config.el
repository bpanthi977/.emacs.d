(define-prefix-command 'bp/agent-prefix-map)
(define-key bp/global-prefix-map (kbd "a") #'bp/agent-prefix-map)

(use-package agent-sessions
  :load-path "modules/agent-sessions"
  :bind (:map bp/agent-prefix-map
              ("a" . bp/agent-sessions-list)
              ("s" . bp/agent-sessions-send-region))
  :defer nil
  :config
  (setf bp/agent-sessions-show-all-worktrees t)
  (setf bp/agent-sessions-terminal 'ghostel)
  ;; The region command has already put the sharing agent first and the rest
  ;; in view-recency order; keep Ivy from replacing that with alphabetic order.
  (with-eval-after-load 'ivy
    (add-to-list 'ivy-sort-functions-alist
                 '(bp/agent-sessions-send-region . nil)))
  (add-to-list 'project-switch-commands '(bp/agent-session-switch-or-new "Agent Session"))
  ;; Let M-m (bp/global-prefix) reach Emacs instead of being swallowed by vterm.
  ;; `vterm-keymap-exceptions' keys are removed from `vterm-mode-map'; its :set
  ;; rebuilds the map, so use setopt (not setq) to apply the change.
  (with-eval-after-load 'vterm
    (unless (member "M-m" vterm-keymap-exceptions)
      (setopt vterm-keymap-exceptions (cons "M-m" vterm-keymap-exceptions)))
    (define-key vterm-mode-map (kbd "C-c C-l")
                #'bp/agent-session-insert-org-link))

  ;; Same for ghostel: `ghostel-keymap-exceptions' keys pass through to Emacs;
  ;; its :set rebuilds the semi-char keymap, so use setopt.  `C-c C-l' is
  ;; ghostel's `ghostel-line-mode'; the org link wins here, as in vterm and eat.
  (with-eval-after-load 'ghostel
    (unless (member "M-m" ghostel-keymap-exceptions)
      (setopt ghostel-keymap-exceptions (cons "M-m" ghostel-keymap-exceptions)))
    (define-key ghostel-mode-map (kbd "C-c C-l")
                #'bp/agent-session-insert-org-link))

  ;; agent-sessions already notifies when an agent needs attention; ghostel's
  ;; own status/OSC 9 notifications would announce the same turn a second time.
  (setq ghostel-notification-functions nil)

  ;; Same for eat: keys in `eat-semi-char-non-bound-keys' are NOT sent to the
  ;; terminal in semi-char mode, so they fall through to Emacs.  eat spells
  ;; M-KEY as [?\e KEY]; its :set rebuilds the keymap, so use setopt.
  (with-eval-after-load 'eat
    (unless (member [?\e ?m] eat-semi-char-non-bound-keys)
      (setopt eat-semi-char-non-bound-keys
              (cons [?\e ?m] eat-semi-char-non-bound-keys)))
    (define-key eat-mode-map (kbd "C-c C-l")
                #'bp/agent-session-insert-org-link))

  ;; Install the advice, hooks, Org link type, project binding, and server.
  (bp/agent-session-setup)

  ;; One-time (re-run any time) wiring of our hooks into Claude Code / Codex, so
  ;; sessions report to the dashboard without depending on orca.  Run manually
  ;; via `M-x bp/agent-sessions-install' (and `-uninstall' to remove them).
  ;; It writes ~/.claude/settings.json and ~/.codex/hooks.json, so it is left
  ;; explicit rather than run on every startup.
  )
