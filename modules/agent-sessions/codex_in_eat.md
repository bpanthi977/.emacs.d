# Codex-in-eat frame flicker — SOLVED

When Codex runs in an **eat** terminal, the **whole Emacs frame** flickers a
couple of times a second. It does not happen with vterm, nor with Claude in eat.

The decisive clue (which an earlier investigation didn't have) is that the
flicker **outlives Codex**: quit Codex and the flicker continues for as long as
that eat buffer is *displayed*; bury the buffer and it stops. So it is not
driven by Codex's output at all — it's a timer in eat that stays armed.

## Root cause

Codex asks the terminal for a blinking cursor (DECSCUSR / `\e[?12h`). eat honors
that by enabling `eat--cursor-blink-mode`, whose blink timer is `eat.el`:

```elisp
(defun eat--flip-cursor-blink-state ()
  (when (and eat--cursor-blink-mode (display-graphic-p))
    (setq-local cursor-type ...)
    ;; REVIEW: This is expensive, and some causes flickering.  Any better way?
    (when-let* ((window (get-buffer-window nil 'visible)))
      (redraw-frame (window-frame window)))))
```

eat blinks the cursor by calling **`redraw-frame`** — a full repaint of every
window on the frame — at the blink frequency (2 Hz). That *is* the flicker; eat's
own comment admits it. It persists after Codex exits because nothing resets the
cursor style, so the buffer stays in blink mode with the timer running.

This also explains the negative cases: Claude's TUI never requests a blinking
cursor, and vterm sets `cursor-type` without redrawing the frame.

## Fix

In `../agent-session-config.el` (inside `with-eval-after-load 'eat`), set the
blinking frequency to nil in the three cursor types that carry one — the only
ones that can turn `eat--cursor-blink-mode` on:

- `eat-very-visible-cursor-type`
- `eat-very-visible-vertical-bar-cursor-type`
- `eat-very-visible-horizontal-bar-cursor-type`

The cursor *shape* Codex asks for is preserved (only the blink frequency is
dropped), and `blink-cursor-mode` still blinks the selected window's cursor the
normal way — without repainting anything else. Nothing is lost.

Buffers already stuck in blink mode keep flickering until fixed; to repair a
running Emacs without restarting, turn `eat--cursor-blink-mode` off in each eat
buffer that has it on and reset `cursor-type` from
`(car eat--cursor-blink-type)`.

## Superseded analysis (kept so it isn't re-tried)

An earlier pass attributed the flicker to eat's rendering model and concluded it
was unfixable without libvterm-style grid diffing. Both mitigations it proposed
were later reverted and are **not** in the config; neither is needed:

1. **Raising `eat-minimum-latency` / `eat-maximum-latency`** (0.02 / 0.1).
   Slowed the flicker without stopping it — consistent with the real cause only
   if you misread it; latency actually just changes how often eat redisplays,
   while the frame redraw came from the blink timer. Costs choppier bulk output.
2. **Replacing `eat--synchronize-scroll`** with a version pinning `window-start`
   to `eat-term-display-beginning` instead of `recenter`-ing per frame. Aimed at
   a scroll-bounce judder, a different symptom from the frame flicker. Not
   currently present; re-add only if you actually observe viewport bounce.

Route-Codex-to-vterm is no longer a needed workaround.

---

# Codex-in-eat top-to-bottom scroll on buffer switch — SOLVED

Switching buffers (or coming back to the Codex buffer) makes Codex race from the
top of the buffer to the bottom. Does not happen with vterm.

## Root cause

Nothing to do with switching buffers as such — it is the **echo area**. `ivy`
grows the minibuffer window to ~11 lines for `C-x b`, and a grown minibuffer
window steals rows from every other window on the frame. So:

    minibuffer grows -> eat window loses N rows
      -> `window--adjust-process-windows' -> `eat--adjust-process-window-size'
      -> `set-process-window-size' -> SIGWINCH -> Codex repaints from scratch

In eat the buffer *is* the terminal, so a full repaint first **empties the
buffer** (`point-max` drops to 1) and then refills it over ~100 ms across
several redisplays; `window-start` follows the refill from 1 up to the bottom.
That sweep is the symptom. Measured, codex in eat, minibuffer +10/-10 lines:

    t=0.000 ws=2733 h=36 pm=5532     <- steady
    t=1.021 ws=3593 h=26 pm=5532     <- minibuffer grew, SIGWINCH
    t=1.101 ws=1    h=26 pm=386      <- buffer wiped, window at top
    t=1.122 ws=214  h=26 pm=1506
    ...
    t=1.202 ws=3593 h=26 pm=5532     <- settled again (and again on shrink)

The same run under vterm keeps `pm` pinned at 3955 the whole time: libvterm owns
the screen and vterm rewrites the same buffer region, so Codex's repaint never
moves the viewport. That is the entire eat/vterm difference — vterm gets the
identical SIGWINCH, it just doesn't show.

Everything else that grows the echo area (a long `message', eldoc, which-key)
triggers the same one-row version of this.

## Fix

`bp/eat--adjust-process-window-size' in `../agent-session-config.el', :around
`eat--adjust-process-window-size': if the *width* is unchanged, the *height*
changed, and the frame's minibuffer window is taller than one line, return
without calling the original — Emacs then skips `set-process-window-size' and
eat skips its own `eat-term-resize'/redisplay, so the pty keeps its size.

Self-correcting: when the echo area shrinks back, another adjust runs, the
wanted size now equals the current one, and nothing reaches the pty. Width
changes and genuine layout changes (splits, frame resize) still resize at once.

Cost: while the echo area is grown, the terminal is a row or two taller than its
window, so the bottom of Codex's composer is clipped for as long as the
minibuffer is up. Cheap next to the sweep.

## Rejected

- **Debouncing the resize by 0.25 s.** ivy stays open for seconds, so the resize
  lands anyway. Also polls a timer for as long as a `message' sits in the echo
  area.
- **Suppressing only while `active-minibuffer-window'.** Misses the leftover
  two-line `message' case, which resizes by one row and sweeps just the same.

---

# Codex-in-eat viewport drifts until you press a key — SOLVED

The other half of the above. On a **genuine** height change — switching the
Codex buffer into a window of a different size than it last had, splitting,
resizing the frame — the resize *should* reach the pty, so the suppression above
deliberately doesn't apply. Codex repaints, and the window is left scrolling
somewhere up in the scrollback and *stays* there, drifting with every further
repaint. Pressing any key snaps it straight to the composer box and it never
happens again for that buffer.

## Root cause

`eat--synchronize-scroll-windows' returns only the windows whose `window-point'
**already equals** `eat-term-display-cursor'; everything downstream
(`eat--process-output-queue', `eat--adjust-process-window-size') anchors just
those. Equality is how eat asks "is the user still following the terminal, or
have they scrolled off to read something?" — reasonable, but it has no way back:
once a window's point differs, it is dropped from the list on every subsequent
batch and eat never scrolls it again.

Displaying the buffer in a window is exactly what breaks the equality.
`set-window-buffer' restores that window's *remembered* point for the buffer
from `window-prev-buffers' — a position that made sense whenever that window
last showed it, and is meaningless in a TUI whose whole screen has since been
rewritten in place. So the window is dropped before the SIGWINCH repaint even
starts, and the repaint then moves the display cursor kilobytes away.

The keypress cure is the giveaway: `eat-self-input' calls
`(eat--synchronize-scroll-windows 'force-selected)', which puts the selected
window in the list *regardless* of point. That single forced re-anchor restores
the equality and the buffer behaves from then on.

## Fix

Two hooks, both in `../agent-session-config.el', installed from
`bp/eat--setup-scroll-sync' on `eat-mode-hook'. Both call
`bp/eat--force-synchronize-scroll', which is `force-selected' generalised: it
passes `buffer' plus *every* window showing the buffer, unfiltered.

- `window-buffer-change-functions' (buffer-local) — re-anchor whenever the
  buffer is displayed in a window, which is where the stale point comes from.
  This is also what makes a buffer switch land on the composer box directly
  instead of wherever the window used to be looking.
- `eat-update-hook', gated on `bp/eat--resync-scroll-until' — a resize we
  actually push to the pty arms a 0.5 s window (set in
  `bp/eat--adjust-process-window-size' *before* calling the original, since the
  original ends by running `eat-update-hook' itself) during which every output
  batch re-anchors. That covers the repaint, which arrives over several batches
  and ~100 ms rather than in one.

Gating the second hook matters: re-anchoring on *every* update would yank you
back to the bottom while you're reading scrollback, which is the behaviour eat's
point-equality test exists to provide. Keep it scoped to the resize.

Codex itself can do nothing about any of this — it sees a SIGWINCH and repaints;
which Emacs window is showing its pty, and where that window is scrolled to, are
not things a terminal program can know.

## Note for a running Emacs

`eat-mode-hook' only fires for new buffers. To fix already-open ones without
restarting, map `bp/eat--setup-scroll-sync' over the live `eat-mode' buffers.
