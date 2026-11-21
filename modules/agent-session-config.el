(define-prefix-command 'bp/agent-prefix-map)
(define-key bp/global-prefix-map (kbd "a") #'bp/agent-prefix-map)

(use-package agent-sessions
  :load-path "modules/agent-sessions"
  :demand t
  :bind (:map bp/agent-prefix-map
              ("a" . bp/agent-sessions-list)
              ("s" . bp/agent-sessions-send-region))
  :custom
  (bp/agent-sessions-show-all-worktrees t)
  ;; Let M-m (bp/global-prefix-map) reach Emacs from inside agent terminals.
  (bp/agent-sessions-terminal-passthrough-keys '("M-m"))
  :config
  (bp/agent-sessions-mode 1))
