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

(defvar bp/eat-resize-resync-duration 1.5
  "Seconds after a real resize during which eat output re-anchors the window point.
Slightly longer than `bp/eat-resize-coalesce-duration', so the resync window
stays open until the TUI's answering repaint has fully flushed through the
coalesced latency (pi's repaints run longer than Codex's).")

(defvar-local bp/eat--resync-scroll-until nil
  "Time until which each eat output batch re-anchors the window point.
Armed by `bp/eat--adjust-process-window-size' when a resize actually reaches
the pty.  Scoped to a resize: outside the window the hook is inert, so
reading scrollback still pins the view (that is what eat's point-equality
test exists for).")

(defun bp/eat--force-synchronize-scroll ()
  "Re-anchor point in every window showing the current eat buffer.
`eat--synchronize-scroll-windows' with force-selected, generalised: eat
lists only the windows whose point still equals the display cursor — but a
repaint moves the cursor, dropping every window from its list, after which
eat never scrolls them again (until a keypress forces it).  Passing the
buffer plus *every* window showing it restores the equality eat's
synchronization needs, in one call, regardless of where point was left."
  (when (bound-and-true-p eat-terminal)
    (eat--synchronize-scroll
     (cons 'buffer (get-buffer-window-list nil nil 'all-frames)))))

(defun bp/eat--resync-scroll-on-display ()
  "Re-anchor the scroll when this eat buffer is displayed in a window.
`window-buffer-change-functions' is where a stale remembered point gets
restored by `set-window-buffer' — the position that makes eat drop the
window — so this is also what makes a buffer switch land on the composer
instead of wherever the window last was."
  (bp/eat--force-synchronize-scroll))

(defun bp/eat--resync-scroll-on-update ()
  "Re-anchor the scroll on each output batch inside the post-resize window.
The repaint answering a resize arrives over several batches, each moving
the display cursor; re-anchor on every one of them while the window is
armed.  Inert outside it, so scrolling back to read stays put."
  (when (and bp/eat--resync-scroll-until
             (time-less-p (current-time) bp/eat--resync-scroll-until))
    (bp/eat--force-synchronize-scroll)))

(defun bp/eat--setup-scroll-sync ()
  "Install the scroll re-anchoring hooks for this eat buffer."
  (add-hook 'window-buffer-change-functions
            #'bp/eat--resync-scroll-on-display nil t)
  (add-hook 'eat-update-hook #'bp/eat--resync-scroll-on-update nil t))

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
      ;; over the next several output chunks.  Coalesce them, and re-anchor the
      ;; window on every batch while it lands: pi/codex only see SIGWINCH, know
      ;; nothing about Emacs windows, and their repaint moves the display cursor
      ;; far from where the window was looking.
      (when (and have want (not (equal want have)))
        (bp/eat--coalesce-after-resize)
        (setq bp/eat--resync-scroll-until
              (time-add (current-time) bp/eat-resize-resync-duration)))
      (funcall fn process windows))))

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
                #'bp/agent-session-insert-org-link)

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
    ;; apply -- but Codex's and pi's answering repaint still sweeps the viewport
    ;; from the top.  Coalescing hides most of it; the scroll-sync hooks in
    ;; `bp/eat--setup-scroll-sync' handle what remains, re-anchoring the window
    ;; on every batch of the repaint and on every buffer display.  Codex and pi
    ;; can do nothing about it: they only see SIGWINCH and know nothing about
    ;; Emacs windows.

    ;; `eat-mode-hook' only fires for buffers created after this ran; re-anchor
    ;; the ones already open (the running agent TUIs).
    (bp/eat--setup-scroll-sync)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'eat-mode)
          (bp/eat--setup-scroll-sync))))
    )

  ;; Install the advice, hooks, Org link type, project binding, and server.
  (bp/agent-session-setup)

  ;; One-time (re-run any time) wiring of our hooks into Claude Code / Codex, so
  ;; sessions report to the dashboard without depending on orca.  Run manually
  ;; via `M-x bp/agent-sessions-install' (and `-uninstall' to remove them).
  ;; It writes ~/.claude/settings.json and ~/.codex/hooks.json, so it is left
  ;; explicit rather than run on every startup.
  )
