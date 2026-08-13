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
              (cons [?\e ?m] eat-semi-char-non-bound-keys)))

    ;; Whole-frame flicker while a Codex TUI is on screen.  Codex asks for a
    ;; blinking cursor (DECSCUSR), which puts eat into `eat--cursor-blink-mode';
    ;; its blink timer calls `redraw-frame' on every tick (see the "REVIEW: This
    ;; is expensive, and some causes flickering" comment in `eat.el'), so the
    ;; entire frame repaints twice a second.  It outlives Codex because the
    ;; buffer stays in blink mode until something resets the cursor style.
    ;; Blinking frequency nil in these three (the only blinking cursor types)
    ;; means eat never enables that mode; `blink-cursor-mode' already blinks the
    ;; cursor of the selected window without redrawing anything else.
    (setopt eat-very-visible-cursor-type
            (list (car eat-very-visible-cursor-type) nil nil)
            eat-very-visible-vertical-bar-cursor-type
            (list (car eat-very-visible-vertical-bar-cursor-type) nil nil)
            eat-very-visible-horizontal-bar-cursor-type
            (list (car eat-very-visible-horizontal-bar-cursor-type) nil nil)))

  ;; Install the advice, hooks, Org link type, project binding, and server.
  (bp/agent-session-setup)

  ;; One-time (re-run any time) wiring of our hooks into Claude Code / Codex, so
  ;; sessions report to the dashboard without depending on orca.  Run manually
  ;; via `M-x bp/agent-sessions-install' (and `-uninstall' to remove them).
  ;; It writes ~/.claude/settings.json and ~/.codex/hooks.json, so it is left
  ;; explicit rather than run on every startup.
  )
