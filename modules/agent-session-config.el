(defun bp/eat--echo-area-tall-p (windows)
  "Non-nil when a frame showing any of WINDOWS has a grown echo area."
  (seq-some (lambda (window)
              (let ((mini (minibuffer-window (window-frame window))))
                (and (window-live-p mini)
                     (or (minibuffer-window-active-p mini)
                         (> (window-height mini) 1)))))
            windows))

(defvar bp/eat-resize-coalesce-latency 0.25
  "Output latency, in seconds, to impose on an eat buffer just after a resize.
eat normally redisplays every `eat-maximum-latency' (0.033 s); raising it
makes the TUI's repaint arrive as one batch and so one redisplay.")

(defvar bp/eat-resize-coalesce-duration 1.0
  "Seconds to keep `bp/eat-resize-coalesce-latency' in force after a resize.")

(defvar-local bp/eat--coalesce-timer nil
  "Timer that restores the normal output latency after a resize.")

(defun bp/eat--restore-latency (buffer)
  "Drop the post-resize latency override in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq bp/eat--coalesce-timer nil)
      (kill-local-variable 'eat-minimum-latency)
      (kill-local-variable 'eat-maximum-latency))))

(defun bp/eat--coalesce-after-resize ()
  "Batch this eat buffer's output for a moment, to hide a repaint in progress.

A resize that reaches the pty makes the TUI redraw its whole screen, and
eat receives that redraw as a dozen-odd pty chunks.  eat flushes every
`eat-maximum-latency', so each chunk gets its own redisplay showing the
buffer part-refilled -- the buffer *is* the terminal, so the screen is
first emptied and then rebuilt, and you watch it grow from the top with
`window-start' pinned at 1 because there is not yet enough text to scroll.
That growing-from-the-top is the whole symptom; no amount of window
anchoring can help, because the viewport is already exactly where it
should be for the text that currently exists (measured: eat's own sync
list contained the window at every intermediate frame).

So don't show the intermediate frames.  Raising the latency for a beat
collapses the repaint into one flush and one redisplay: the terminal
freezes for a fraction of a second and then appears already redrawn."
  (setq-local eat-minimum-latency bp/eat-resize-coalesce-latency)
  (setq-local eat-maximum-latency bp/eat-resize-coalesce-latency)
  (when (timerp bp/eat--coalesce-timer)
    (cancel-timer bp/eat--coalesce-timer))
  (setq bp/eat--coalesce-timer
        (run-with-timer bp/eat-resize-coalesce-duration nil
                        #'bp/eat--restore-latency (current-buffer))))

(defun bp/eat--adjust-process-window-size (fn process windows)
  "Keep the pty size when only the echo area changed the window height.
Intended as :around advice for `eat--adjust-process-window-size'."
  (let ((have (and (bound-and-true-p eat-terminal) (eat-term-size eat-terminal)))
        (want (funcall window-adjust-process-window-size-function
                       process windows)))
    (unless (and have want
                 (eq (car want) (car have))   ; width unchanged
                 (/= (cdr want) (cdr have))   ; height changed...
                 (bp/eat--echo-area-tall-p windows)) ; ...and the echo area is why
      ;; A size we actually push to the pty means a full repaint is coming back
      ;; over the next several output chunks.  Coalesce them.
      (when (and have want (not (equal want have)))
        (bp/eat--coalesce-after-resize))
      (funcall fn process windows))))

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
            (list (car eat-very-visible-horizontal-bar-cursor-type) nil nil))

    ;; Codex sweeping from the top of the buffer to the bottom whenever the
    ;; echo area grows (ivy's tall minibuffer, a long `message', ...).  Growing
    ;; the minibuffer window steals a row from every other window on the frame,
    ;; so eat pushes the new height to the pty, Codex gets SIGWINCH and repaints
    ;; from scratch.  In eat the buffer *is* the terminal, so the repaint first
    ;; empties it (point-max drops to 1) and then refills it across several
    ;; redisplays, and the window visibly races from the top of the buffer down.
    ;; vterm keeps the screen in libvterm and rewrites the same buffer region,
    ;; which is why the same SIGWINCH is invisible there.
    ;;
    ;; So: don't propagate a pure height change while the echo area is grown.
    ;; The old size is kept until the echo area shrinks back, at which point the
    ;; wanted size equals the current one again and nothing reaches the pty.
    ;; Width changes and real layout changes still resize immediately.
    (advice-add 'eat--adjust-process-window-size :around
                #'bp/eat--adjust-process-window-size)

    ;; The other half of the same story: a *genuine* height change (switching
    ;; the buffer into a window of a different size, splitting, resizing the
    ;; frame) is supposed to reach the pty, so the suppression above does not
    ;; apply -- but Codex's answering repaint still sweeps the viewport from the
    ;; top.  That is handled inside the same advice, by coalescing the repaint
    ;; into a single redisplay; see `bp/eat--coalesce-after-resize'.  Codex can
    ;; do nothing about it: it only sees SIGWINCH and knows nothing about Emacs
    ;; windows.
    )

  ;; Install the advice, hooks, Org link type, project binding, and server.
  (bp/agent-session-setup)

  ;; One-time (re-run any time) wiring of our hooks into Claude Code / Codex, so
  ;; sessions report to the dashboard without depending on orca.  Run manually
  ;; via `M-x bp/agent-sessions-install' (and `-uninstall' to remove them).
  ;; It writes ~/.claude/settings.json and ~/.codex/hooks.json, so it is left
  ;; explicit rather than run on every startup.
  )
