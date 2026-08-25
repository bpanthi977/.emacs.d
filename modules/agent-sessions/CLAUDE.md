# agent-sessions — design notes

A dashboard for Claude Code / Codex sessions running in terminal buffers. The
code says *what* it does; this file records *why*, so the intent survives edits.

## Orientation (just enough to navigate)

- `agent-sessions.el` — everything; `agent-sessions-codex-hook.sh` — Codex event
  forwarder. The Claude forwarder lives outside the repo
  (`~/.orca/agent-hooks/claude-hook.sh`, wired via `~/.claude/settings.json`).
- `bin/` — commands agents run *inside* a session terminal: `emacs-share-file`
  (see "Shared files") and `emacs-agent` (see "Orchestration"). On every session
  terminal's PATH.
- `skills/orca/` — the `/orca` skill an agent loads to learn orchestration, and
  the two guides `emacs-agent guide` serves.
- `orchestration/` — notes on how *Orca* (the app) does the same thing; read
  before redesigning this, not as a description of what is here.
- Wiring/config is in `../agent-session-config.el`.

## Why it's built this way

**Zero-friction tracking beats an explicit launcher.** Rather than a wrapper
command you must remember to use, every terminal buffer gets a unique id
injected into its shell env, so *any* `claude`/`codex` you type in *any*
eat/vterm is tracked automatically. The cost is that we advise the terminal
packages' internals — accepted deliberately in exchange for never having to
think about launching sessions specially.

**The per-buffer id is the identity, not the agent's own session id.** Sessions
sharing a worktree must still resolve to the right buffer, and the agent's own
UUID isn't known until a hook fires (and isn't in every payload). So the stable
key is the injected `EMACS_AGENT_SESSION_ID`; the agent's UUID is tracked
separately, only for resume/branch/links.

**The dashboard is ephemeral; the record of what happened is not.** Rows are
rebuilt from live buffers on every render and nothing on disk feeds them —
that is what stops a dead session ever being displayed as alive, which was the
original point of the rule. What *is* persisted lives in one SQLite file
(`bp/agent-sessions-db-file`), and the bar for adding to it is that it must be
either something the user chose, or something they cannot reconstruct once it
is gone:

- **notes** (`e`) and **manual order** (`M-n`/`M-p`) — things the user wrote or
  arranged by hand; evaporating on restart would make them useless.
- **the session log** — observed session state, which the rule above would
  otherwise forbid. It earns its place because a session id is the one thing
  you cannot get back after a crash, and because it records endings as
  faithfully as beginnings. A log that knows a session died isn't lying about
  it; it is the only way `R` and `bp/agent-sessions-restore-previous` can offer
  it back. What would break the rule is letting it populate rows.

One store, not three files, because the log forced the issue: it is
append-heavy, queried by recency, and unbounded, so the old rewrite-the-file
approach didn't fit it — and once a database existed, a second and third
persistence format was pure cost. Notes and order are still read through their
hash tables; **the render path must never query the database.** Load once,
write on change.

Two consequences of persisting things keyed by identity: a note keyed on a
session id outlives the session that had it (nothing prunes it — a stale row
is invisible and cheap, and guessing that a session is gone *for good* is
exactly the kind of lying the ephemerality rule exists to avoid), and note
keys are type-tagged (`("repo" …)` vs `("worktree" …)`) because a repo's main
worktree has the same canonical path as the repo itself and would otherwise
share its note. The order table carries the same tag as `kind`, and its scope
columns default to `''` rather than `NULL` — SQLite treats `NULL`s as distinct
under a primary key, so a `NULL` scope would silently turn every upsert into a
duplicate insert.

**How a session ended is inferred from which hook fired, not from a flag.**
Emacs runs `kill-emacs-hook` on exit but does *not* run `kill-buffer-hook` for
live buffers at that point (verified, not assumed). That asymmetry is the whole
scheme, and it is why `k` needs no special case: it is a plain `kill-buffer`,
so it and `C-x k` land in the same place.

| ending | what fired | means |
|---|---|---|
| `killed` | `kill-buffer-hook` | you closed it |
| `agent-exit` | `SessionEnd` | agent quit, terminal lives on |
| `superseded` | a new agent id in the same terminal | previous one ended unheard |
| `emacs-exit` | `kill-emacs-hook` | Emacs quit with it running |
| `crash` | nothing — found open at the next startup | Emacs died |

`R` offers the first three ("you ended it"), restore-previous the last two
("it was taken from you"). **Crash detection assumes a single Emacs**: a
starting Emacs has no sessions yet, so any row still open must belong to a dead
one. A second Emacs would reap the first's live rows — they would read as
restorable while still running. Nothing corrupts, and that was the accepted
price for dropping an instances table; if that assumption ever stops holding,
that is the thing to revisit. Because a crash runs no code, close *times* come
from a periodic heartbeat in `meta` — the last moment Emacs is known to have
been alive.

**A reorder writes two rows, not the group.** `M-n`/`M-p` are adjacent
transpositions, and `--merge-order` turns one into a list differing from the
saved one in exactly two slots, so `--order-write-group` diffs old against new
and upserts only what moved. Positions stay contiguous forever without
renumbering, because a transposition preserves the *set* of positions and only
exchanges which member holds which. Note what this depends on: if a
"move to top" or drag-to-position operation is ever added, arbitrary placement
between two neighbours is where gap or fractional positions start to earn their
keep — `position` can become `REAL` without touching the read path.

**Hooks are unordered and untrusted for sequencing.** Hook invocations are
independent async processes with no delivery-order guarantee, so an
informational event can land *after* a `Stop`. The status map therefore only
promotes known events and leaves everything else untouched — status must never
be clobbered back to "running" by a straggler. This is the single most
important invariant when editing the status logic.

**Notifications fire on the transition into attention, once.** A session that
sits waiting must not re-announce on every subsequent event. If you add states,
preserve "edge-triggered, not level-triggered."

**The manual unread mark (`u`) rides on the buffer, not on the status.** It
makes a row *look* like needs-attention (face, marker, sort bucket, `N`/`P`)
without ever being written into the registry, for three reasons: a plain
terminal that no hook has fired for has no registry entry to write to; hook
status computation falls back to the *previous* status for informational
events, so a written-in `needs-attention` would outlive the thing the user
wanted to be reminded of; and inventing a registry entry for a bare terminal
would suppress fork auto-detection, which is guarded on the session being new.
So the flag is buffer-local and applied at display time on a *copy* of the
plist — never mutate the registry entry from the render path. It clears the
same way a real one does, by focusing the session, and is deliberately not
persisted: it's session state, which by the rule above evaporates on restart.

**A row with no registry entry names whatever is running, from the kernel.**
Hooks can't be relied on to say what a terminal is running, because Codex
creates its session *lazily*: its `SessionStart` fires on the first prompt
submission, in the same instant as `UserPromptSubmit`, not when the process
starts (measured, not assumed — a fresh Codex TUI sat drawn at its prompt for
~90s emitting nothing, and wrote no rollout file either, so watching
`~/.codex/sessions` is the same dead end). Claude fires `SessionStart` on
`--resume`, so before this, a batch of restored sessions left every Codex row
reading `eat` while the Claude rows read `claude`.

`bp/agent-sessions--foreground-command` instead asks the pty for its
*foreground process group* (`tpgid` from `process-attributes`) and names that
job. Why this shape:

- **It generalises past the two agents.** Any command shows its own name, and
  the label returns to the backend when it exits, with no state to keep in
  sync. A python REPL reads `Python`; `sleep 40` reads `sleep`.
- **It is self-correcting, which a launch-time tag is not.** The first attempt
  here stamped the agent onto the buffer at spawn; a terminal where Codex had
  since been replaced by Claude kept the stale label, and no amount of care in
  the spawn path fixes that class of bug.
- **`args`, never `comm`.** Claude's executable lives in a version-numbered
  directory, so its `comm` reads `2.1.231`.
- **No subprocess.** `process-attributes` is a kernel read, not a `ps` fork —
  which is what makes this affordable at all.
- **Cached exactly, never on a timer.** Uncached this took a warm hook-tick
  refresh from 23ms to 41ms — nearly double, exactly the hot-path cost the git
  note below exists to prevent. Reading the *command line* is the expensive
  half, so the name is cached and reused while `tpgid` is unchanged: an exact
  test, not a heuristic, since the same foreground group is the same job. That
  alone brings it to ~+3ms. A 0.3s time-based layer on top was tried and
  **removed**: it saved only the remaining ~3.6ms of group reads (12 terminals
  × one kernel read; `--live-entries` runs once per render, so there is no
  duplicate work to collapse) and paid for it with a label up to a third of a
  second stale. For a display whose purpose is showing what is running now,
  that is the wrong trade — and `g` already costs ~1.4s, so this was never the
  expensive thing on the path. If this ever does need to be cheaper, cut the
  number of rows or the refresh rate, not the freshness.

`:status` stays `idle` for these rows rather than being promoted to `running`:
knowing a process holds the terminal says nothing about whether the agent is
working or waiting, and inventing that is the kind of lying the rules here
exist to prevent. Registry rows are untouched — a hook that has reported
carries its agent type *and* real status, which the process table cannot give
once the process is gone.

The corollary, learned the hard way: **a row's status is whatever was
rendered, so nothing may re-derive it from the registry.** `N`/`P` did exactly
that (`gethash` on `bp/agent-sessions`) and so navigated past every row `u` had
marked, while the face, marker and sort bucket — all fed by `--live-entries`,
which applies the overlay — agreed the row needed attention. The render now
records `(line-pos . status)` into `bp/agent-sessions--row-status` as it draws,
and navigation reads that. Any future consumer of "what state is this row in"
belongs on that list, not on a second lookup.

**Row order must not depend on anything that changes while a session runs.**
The dashboard re-renders on *every* hook event, so ordering by `:updated-at`
made two concurrently busy sessions swap places several times a second — the
row you were aiming at moved out from under point. The default order therefore
uses only keys that are stable under activity: a coarse attention bucket
(error → needs-attention → running → rest), then buffer creation time, then
buffer name as a deterministic final tie-break (hash iteration order must never
leak into the layout). A row *does* move when its status changes — that's a
rare, meaningful event, unlike a tool call. Repos and worktrees are plain
alphabetical: a fixed skeleton is what makes the tree navigable from memory, so
urgency is expressed only *within* a worktree. Recency sort survives behind `s`
for when you want it; don't make it the default again.

**Manual order overrides the automatic one per group, and only per group.**
`M-n`/`M-p` move the repo / worktree / row at point within its own siblings and
persist that group's key list. Design points worth keeping: items with no
recorded key keep the automatic order and sort *after* the placed ones, so a new
session never displaces a slot the user chose; rewriting a group splices the
visible keys back into the saved list instead of replacing it, so a repo or
worktree whose terminals are all closed doesn't lose its remembered slot; and
keys are stable identities (canonical paths, buffer name for a row) rather than
display labels, which change on a branch checkout. Reordering acts on the flat
per-worktree list that `--insert-entries` then nests, so moving a *fork* row
past a sibling of its parent's group may not visibly move it — the nesting wins.
Manual order is disabled while `s` (recency) is active; those two orderings
would fight, and recency should stay a temporary lens.

**A stray key closes the dispatch menu instead of trapping you in it.** `RET`
on a repo/worktree heading opens project.el's switch menu, whose own
`project--switch-project-command` loops until it reads a key it recognises —
fine when you deliberately invoked `project-switch-project`, wrong here, where
`RET` is a glance at what's available and every *other* dashboard key is a
one-shot action. `bp/agent-sessions--switch-project` therefore swaps in a
one-shot reader (returning `ignore` for unknown input) via `cl-letf`, scoped to
that single call so the looping behaviour survives everywhere else. Build the
keymap and prompt from project.el's own `project-switch-commands` /
`project-prefix-map` / `project--menu-prompt` rather than a private list, so a
user's customised menu keeps working.

**A note follows the most durable identity its subject has.** Repos and
worktrees are keyed by canonical path, as above, but a *session* prefers the
agent's own session id and falls back to the buffer name only for a plain
terminal that has no id yet — otherwise renaming a terminal (which the `t`
title flow encourages) would silently orphan its note. Both keys are read, and
writing collapses them to the preferred one, so a note typed into a bare
terminal survives `claude` starting inside it.

**Re-rendering has to re-apply fold state itself.** Notes render as their own
magit subsection (first line as heading, rest as body) so `TAB` collapses a
long one. Magit caches visibility per section identity and revives it into the
new sections' `hidden` slots, but nothing applies it to the freshly inserted
*text* — `magit-refresh-buffer` does that with a `magit-section-show` pass over
the root, and since this dashboard erases and rebuilds by hand it must do the
same. Without that pass a folded note silently springs open on the next hook
event.

**Re-rendering is on the hot path, so git is cached aggressively.**
`magit-list-worktrees` and repo-info each spawn git subprocesses; doing that per
refresh made trivial actions (sorting, a hook tick) slow. Hence the worktree /
repo-info / transcript-uuid caches. The tension is freshness vs. speed: the
internal refresh trusts the caches; only `g` re-reads git from disk. Keep new
per-refresh work off the git path.

## Orchestration — an agent spawning and talking to other agents

`bin/emacs-agent` (spawn/send/done/ask/check/wait/list/worktrees) lets an agent
start another agent in another worktree and block until it reports. It reaches
Emacs the way `emacs-share-file` does, and returns its failures as the printed
value for the same reason.

**Emacs owns only what an agent cannot do for itself.** There are no task rows,
no dependency graph and no scheduler, and adding them is the obvious wrong turn.
The orchestrator is an agent with a context window; that context *is* the plan,
and a second copy in a table would be a second thing to keep honest — with the
table losing, because only the agent knows why it changed its mind. What is left
is the part an agent genuinely cannot do: start a terminal, address a live one,
and wait for it.

**Several workers may share a worktree, and nothing here stops them.** The
constraint is not one agent per checkout — parallel readers (research, review,
audit) writing findings to separate files want the *same* tree, and a checkout
each would be overhead that also gives each a different view of a dirty tree.
What actually collides is concurrent writes to one file, and git state: the
index and HEAD belong to the worktree, so a co-tenant running `git add` stages
everyone's half-finished edits. So `spawn` deliberately has no exclusivity
check — adding one would forbid the cheapest useful case — and the rule lives
where it can be stated with its exception: the launch preamble tells every
worker to touch only the files its task names and to leave git alone unless the
task says the worktree is its own, and the coordinator guide tells the
orchestrator to say which of those it is.  `emacs-agent worktrees` reports the
count of agents in each worktree rather than free/busy for the same reason: the
caller needs the facts, not our verdict on whether it may work there.

**A worker learns who it is from its environment, not from its launch command.**
The id a worker is addressed by is minted inside the vterm/eat advice that
builds its environment, so at the moment we assemble the command there is no id
to write into it. Hence `bp/agent-sessions--extra-terminal-env`, bound around
the create call: role, coordinator and brief path ride in alongside
`EMACS_AGENT_SESSION_ID`, and `emacs-agent` reads them so the worker never has
to be told its own name.

**The task is passed at launch, as a file the shell reads.** `claude "$(cat
BRIEF)"` rather than starting the agent and sending the task afterwards: a TUI
that is still booting silently drops input, and a two-kilobyte multi-line prompt
as one quoted argument is exactly the line a TUI mangles. The brief file is kept
after launch — it is the only record of what a worker was actually told, and
re-running that one line reproduces the launch by hand.

**Spawning restores the window configuration.** `+` showing its new terminal is
right because the user asked for it; three background spawns rearranging the
layout the user is reading is not. Same `save-window-excursion` the restore path
uses, for the same reason.

**Mail is persisted, and keyed by durable identity — not by the address it was
sent to.** Unread mail is exactly what the persistence rule above is for: the
recipient cannot reconstruct a message it never saw, and a crash between a
worker reporting and its coordinator reading would otherwise lose the report
silently. But mail is *addressed* to a terminal id, which carries this Emacs's
pid and dies with it, so the record files each message under the same identity a
note or shared file uses: the agent's own session id, falling back to the
terminal id. That fallback is deliberately not the buffer name that notes fall
back to — a name is reused by a later terminal, and inheriting a stranger's
unread mail (which is then *pasted into its prompt*) is a different order of
wrong than inheriting a stale note. Mail under a terminal key therefore never
survives a restart, which is correct: nothing can resume a bare shell.

So there are two structures, and they answer different questions. The hash table
is the working copy, keyed the way mail is addressed, and it is what the render
path reads — the render path must never query the database. The `mail` table is
the record; `bp/agent-orchestration--hydrate` copies unread rows into the working
copy at the two moments a session's durable identity becomes known: a hook event
(which is also how a session restored with `R` gets its mail back, attached to
the conversation rather than to the terminal that used to hold it) and an
explicit inbox read. Hydration is idempotent by message id, and ids carry the
minting Emacs's pid so a restored message cannot collide with a new one. Reads
write `read_at` through, or a restart would redeliver mail the agent has already
acted on. A killed terminal drops its working copy in
`bp/agent-session--cleanup`, never the record.

One consequence worth stating: a message can outlive the terminal that sent it,
so the sender is resolved through its durable key at display time. When that
conversation is live again the reply command names its *current* terminal; when
it is not, the message says so instead of printing an address that would reach
either nobody or somebody else.

**Waiting polls from the shell, not from Emacs.** An emacsclient call runs on
Emacs's only thread, so a blocking wait inside Emacs would freeze the editor for
its whole duration — which for this feature is minutes. `emacs-agent wait` loops
over cheap `check` calls instead, and prints a keepalive to stderr every 15s so
the calling agent's tool does not read a long wait as a hung command.

**Mail is pushed into an agent that is resting, and never into one mid-turn.**
An orchestrator that is waiting collects its own mail; the case that needs help
is mail arriving for an agent that has finished its turn, which would otherwise
sit unread until the human prompted it. So delivery fires on the `Stop` hook
(via `bp/agent-orchestration-deliver`) and on arrival, and is gated on two
things — three, counting the one learned the hard way. It has not read its
inbox recently, which is what stops a waiting orchestrator being handed the same
message twice (and when it defers for that reason it schedules a retry, because
mail landing just after a wait gave up would otherwise wait for the next hook
event). Its status is not `running', i.e. no turn is in progress. And something
holds its terminal's foreground.

Both of the latter are phrased as exclusions on purpose. Enumerating the states
that *are* fine has already failed once: the first version listed `idle' and
`needs-attention', and `needs-attention' becomes `stopped' the moment the user
looks at the terminal, so delivery quietly stopped for exactly the session an
orchestrator is most likely to be watched in. And the foreground test is not
tidiness — with the shell's own prompt in front, a paste plus Enter is not
reaching a TUI at all, it is the shell *running the message as a command line*,
where the message was written by another agent. Mail for a bare shell stays
queued for `emacs-agent check' instead.
Delivery is one bracketed paste plus a delayed Enter: typed plainly, every
newline in the message would submit, firing the agent's turn on the first line.

**Pre-trusting the spawn directory is a deliberate widening, not a convenience.**
Both agent CLIs gate a directory they have never run in behind an interactive
trust prompt, and a worker stopped there never reads its brief — so for the case
this feature exists for, spawning into a just-allocated worktree, every spawn
would be a wait for a report that cannot come. `spawn` therefore registers the
directory with the agent first, additively and idempotently, leaving an existing
entry alone even when it says untrusted. The cost is that any directory an
orchestrator spawns into becomes trusted for that agent from then on, which is
why it is a defcustom and why each registration is reported in what `spawn`
prints rather than done quietly.

**The dashboard needed almost nothing.** A worker records its coordinator in the
same `:branched-from` slot a `b`-branch uses, so the existing nesting renders it
under its parent (same worktree) or tags it `↳ from …` (elsewhere), and its task
becomes the row's title override. The only addition is the `✉N` badge, which
reads the in-memory mailbox — the render path stays off the database, as the
rule above requires.

**The skill is a stub; the guide ships with the tool.** `/orca` (and `/orca
--main`) tell an agent when to engage and to run `emacs-agent guide [--main]`;
the guides themselves live next to the command and are symlinked, not copied,
into `~/.claude/skills` and `~/.codex/prompts`. Two copies of a protocol, one of
them cached in a config directory, is how a skill ends up describing flags the
command no longer has. This is the one idea worth keeping from Orca's own design
(`orchestration/provenance.md`).

## Fork/branch tracking — the one place with no ground truth

There is **no hook event or transcript field that names a fork's parent** (this
was verified, not assumed — Claude's `SessionStart` `source` has no "fork", and
the child transcript carries no parent id). Three consequences shaped the
design:

- **We infer parentage from evidence, not authority.** A fork copies the
  parent's transcript verbatim, so the child's message uuids contain (nearly)
  all of the parent's and share the same root. Auto-detection is a *heuristic*
  built on that fact — good enough to be worth doing automatically, but with
  honest failure modes (misses a fork if you keep working in the parent before
  the child's first hook; Claude-only). It's a convenience, not a source of
  truth.
- **Manual `B` is the escape hatch, not a fallback afterthought.** Because
  detection is heuristic, there must always be a way to state the relationship
  by hand (Codex forks, closed parents, mis-detections).
- **A fork briefly *is* its parent.** At `SessionStart`, `--fork-session`
  reports the resumed (parent) id before minting its own, so a fresh child's
  session id transiently equals its parent's. This is why nesting resolves a
  parent to a *different entry* carrying the id, and why cycle-safety comes from
  a visited set — **not** from a `sid == parent-sid` rejection. That check looks
  like a reasonable simplification and would silently break fork nesting; don't
  add it back.

  The transient also reaches the session log, which briefly records the parent's
  id against the child's terminal. `--log-supersede` cleans that up by asking
  the *log* whether the same id is open on another row — if so the row is an
  artifact of the fork and is deleted rather than offered by `R` as a resumable
  duplicate. Note where that test lives: in the log, on log rows. It is exactly
  the `sid == parent-sid` reasoning the bullet above forbids, and it is only
  safe because it never touches parentage detection. Keep it that side of the
  line.

## Shared files — an inbox, not a file manager

`emacs-share-file FILE ["why"]` lets an agent hand the user something to read;
it lands as a fourth level of the tree, under the agent that sent it, and stays
highlighted until opened. It exists because a terminal is a bad inbox: "look at
/tmp/plan.md" scrolls away, and is only seen at all if you are already in that
buffer.

**It is attached to the session, by the session's most durable identity.** The
key is `--session-note-keys` reused verbatim — the agent's own session id when
it has one, the buffer name otherwise, reading both and collapsing onto the
first on write. So renaming a terminal keeps its files, a file shared into a
bare shell survives `claude` starting in it, and a session brought back with
`R` comes back with its files. The *caller* passes the terminal id, because
that is what the shell environment has; resolving it to a session happens in
`bp/agent-sessions-share-file`, the same split the hook path makes. Like a
note, a row outlives the session that owned it and nothing prunes it — the same
trade, for the same reason.

**Re-sharing a path means "I changed it, look again", not "add it twice".**
This is why the table carries two timestamps. `visited_at` is cleared so the
row lights up exactly as it did the first time, but `shared_at` — the sort key
— is left at the original share, so the row does not move. That is the
ordering rule the dashboard already lives by: a row may move when something
meaningful changes, and a row jumping to the bottom of its list is precisely
the thing the user was trying to aim at moving out from under them.

**Submitting refreshes an open buffer only when it has no unsaved changes.**
The Emacs entry point looks for a buffer already visiting the submitted path
and reverts it from disk, so an open plan immediately shows the version the
agent just handed over. It does not open a buffer merely to revert it, and a
modified visiting buffer is left exactly as it is: sharing a file must never
discard the user's edits.

**`k` detaches and never touches disk.** This list is an inbox; clearing an
item out of it must not be able to destroy the thing it points at. There is
deliberately no prefix argument that deletes the file — if that is ever added,
it is a different key, not a modifier on this one.

**An unopened file borrows the needs-attention face, deliberately.** An unread
file and a waiting session are the same claim on the user's attention, and the
dashboard should not make anyone learn two highlights for it. The rows push
onto `--row-status` like every other row, which is what makes `N`/`P` walk to
them; see that variable's docstring for why status is recorded at render time
rather than looked up.

**Reading clears it, however the file was opened.** `RET` is the common path,
but the focus hook that already clears the `u` mark also matches the selected
window's `buffer-file-truename` against the shared paths, so a file shared by
one path and opened by another still counts as read. That hook runs at every
buffer switch, so the check is a set lookup and is gated on the database being
already open — which it is whenever a terminal has ever existed, and is not in
an Emacs with no sessions.

**`u` toggles here, where on a session it sets.** Both directions are wanted
for a file — lighting one up to re-read, and dismissing one you have decided
not to read at all — and a file row, unlike a session, has somewhere to read
its current state back from.

**The script fails loudly, unlike the hook forwarders.** Those exit 0 into the
void by design: a hook firing into a dead Emacs must not break the agent's
turn. This one is the opposite — an agent that believes it has handed over a
file when it has not will go on to tell the user about it — so a missing file,
a terminal outside Emacs, an unreachable server and a refusal from Emacs each
print to stderr and exit non-zero. `bp/agent-sessions-share-file` therefore
*returns* its failures as strings rather than signalling: emacsclient's printed
value is the only channel back to the agent.

**PATH injection is additive by construction.** The terminal env sets
`PATH=<our bin>:<the PATH the terminal was going to inherit anyway>`, read from
`process-environment`. Building it from any other source could silently shrink
the shell's PATH.

## Wiring is self-owned and additive, never authoritative

`bp/agent-sessions-install` / `-uninstall` manage the hook entries in
`~/.claude/settings.json` and `~/.codex/hooks.json`. The design constraints,
which are the whole point of these commands existing:

- **Own our forwarders, depend on nothing external.** The package ships its own
  `agent-sessions-{claude,codex}-hook.sh` and points the configs at them, so it
  works with orca absent/removed. (The machine this was written on happened to
  be wired through orca's script; that's incidental, not required.)
- **Only ever touch our own entries.** Add/remove is keyed by the forwarder
  *filename* in the command string, so any other hook (orca's, the user's) is
  invisible to us. Install and uninstall are therefore both safe to run blind,
  and both idempotent. Install self-heals stale entries (moved package, edited
  event list) by stripping ours and re-adding. This is why several hooks can
  coexist forwarding the same events — double-forwarding is harmless (status is
  edge-triggered), and not-clobbering-others matters more than deduping.
- **Round-trip the config losslessly.** Parse/emit uses the *native* JSON with
  arrays as **vectors** — a Lisp list would be re-serialized as an alist/plist
  object and corrupt the file. Everything not ours (permissions, model, …) must
  survive untouched; verify round-trips by equality against the original, not by
  eyeballing. A `.bak` is written before each change.

Setup does **not** auto-run install — it writes user config files, so it stays
an explicit `M-x`. Point users there rather than editing the JSON by hand.

## Editing

**A schema addition needs the load-time re-migration to be reachable.**
`--db` runs the schema only when it *opens* the file and then caches the handle
for the life of the Emacs, so adding a table left every running Emacs querying
one its connection had never created — a flood of `no such table`, not a clean
failure. The top-level `when (sqlitep …)` form after `--db` re-runs the
statements against an already-open handle; every statement is `IF NOT EXISTS`,
so it is free, and it makes a reload behave like a restart. Keep it in step
with the schema list. Note what it does *not* do: it cannot alter an existing
table, so a change that is not a pure addition needs a real migration keyed on
`meta.schema_version`.

Reload into the running server after any change (definitions don't hot-swap):
`emacsclient -e '(load-file ".../agent-sessions/agent-sessions.el")'`. A
`-L modules` batch byte-compile mis-resolves `(require 'magit)` against the
sibling `modules/magit.el` config file, so trust the live load for validation.
