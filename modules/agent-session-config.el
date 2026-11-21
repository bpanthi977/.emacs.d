(define-prefix-command 'bp/agent-prefix-map)
(define-key bp/global-prefix-map (kbd "a") #'bp/agent-prefix-map)

(use-package agent-sessions
  :load-path "modules/agent-sessions"
  :demand t
  :bind (:map bp/agent-prefix-map
              ("a" . agent-sessions-list)
              ("s" . agent-sessions-send-region))
  :custom
  (agent-sessions-show-all-worktrees t)
  ;; Let M-m (bp/global-prefix-map) reach Emacs from inside agent terminals.
  (agent-sessions-terminal-passthrough-keys '("M-m"))
  :config
  (agent-sessions-mode 1))
