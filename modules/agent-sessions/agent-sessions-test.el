(ert-deftest bp/agent-sessions-ghostel-pre-spawn-registers-plain-terminals ()
  "A plain ghostel terminal gets an id; its compile and eshell buffers don't."
  (let ((bp/agent-session-id->buffer (make-hash-table :test 'equal))
        (term (generate-new-buffer "*ghostel*"))
        (compile (generate-new-buffer "*ghostel-compile*")))
    (unwind-protect
        (cl-letf (((symbol-function 'bp/agent-sessions--log-open) #'ignore)
                  ((symbol-function 'bp/agent-sessions--refresh-if-visible)
                   #'ignore))
          (with-current-buffer term
            (setq-local ghostel-identity '((kind . term) (name . "x")))
            (let ((process-environment '("PATH=/usr/bin")))
              (bp/agent-sessions--ghostel-pre-spawn)
              (should bp/agent-session-id)
              (should (equal (getenv "EMACS_AGENT_SESSION_ID")
                             bp/agent-session-id))
              (should (eq (gethash bp/agent-session-id
                                   bp/agent-session-id->buffer)
                          term))))
          (with-current-buffer compile
            (setq-local ghostel-identity '((kind . compile)))
            (let ((process-environment '("PATH=/usr/bin")))
              (bp/agent-sessions--ghostel-pre-spawn)
              (should-not bp/agent-session-id)
              (should-not (getenv "EMACS_AGENT_SESSION_ID")))))
      (mapc #'kill-buffer (list term compile)))))

(ert-deftest bp/agent-sessions-ghostel-terminal-pid ()
  "A ghostel terminal's pid is its native shell's, not its event pipe's."
  (let ((buf (generate-new-buffer "*ghostel*")))
    (unwind-protect
        (let ((pipe (make-pipe-process :name "test-ghostel-pipe" :buffer buf
                                       :noquery t)))
          (with-current-buffer buf
            (setq major-mode 'ghostel-mode)
            (setq-local ghostel--pid 4242))
          (should (eq (bp/agent-sessions--buffer-terminal-type buf) 'ghostel))
          (should (eql (bp/agent-sessions--terminal-pid buf) 4242))
          (delete-process pipe)
          (should-not (bp/agent-sessions--terminal-pid buf)))
      (kill-buffer buf))))

(ert-deftest bp/agent-orchestration-ghostel-faint-is-ghost-text ()
  "ghostel's dimmed default color reads as Claude ghost text; other text doesn't.
The colors are what a live ghostel rendered for `ESC[2m' on this theme."
  (cl-letf (((symbol-function 'ghostel--face-hex-color)
             (lambda (_face attr)
               (if (eq attr :foreground) "#b2b2b2" "#292b2e"))))
    (let ((faint (propertize "fix the tests" 'face '(:foreground "#818283")))
          (faint-on-red (propertize "x" 'face '(:foreground "#c8807e"
                                                :background "#f2241f")))
          (green-faint (propertize "x" 'face '(:foreground "#518122")))
          (typed "fix "))
      (should (bp/agent-orchestration--claude-ghost-completion-p faint))
      (should (bp/agent-orchestration--claude-ghost-completion-p faint-on-red))
      (should-not (bp/agent-orchestration--claude-ghost-completion-p green-faint))
      (should-not (bp/agent-orchestration--claude-ghost-completion-p
                   (concat typed faint))))))

