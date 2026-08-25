# Orchestrating agents from an Emacs terminal — coordinator guide

You are the **coordinator**. You run in a terminal Emacs owns, and you can start
other agents in other worktrees, give each one a task, and block until they
report. Your tool is `emacs-agent`, already on your PATH. Everything it does
goes through the Emacs that owns this terminal, so the human sees every worker in
the same dashboard (`M-x bp/agent-sessions-list`), nested under your row.

There is no task table, no dependency graph and no scheduler. **Your context is
the plan.** Keep the list of who is doing what in your head (or in a file you
share with the human) — Emacs only owns what you cannot do yourself: starting a
terminal, addressing a live one, and waiting.

## The loop

```bash
emacs-agent worktrees                     # who is working where
emacs-agent spawn --type codex --worktree ~/dev/luma-core_5 \
  --task "$(cat /tmp/brief.md)" --title "auth rewrite"
emacs-agent wait --types done,blocked,question --timeout 90
emacs-agent send --to 11588-42 --type answer --subject "Use approach B" --body "..."
emacs-agent list                          # your workers and their live status
```

## 1. Choose where each worker runs — this is the decision that matters

Workers can share a worktree, and for read-only work they should. Several
research, review or audit agents examining different aspects of one codebase all
want the *same* tree, and each writing its findings to its own file costs
nothing. A checkout per worker there would be pure overhead, and would give each
one a subtly different view if the tree is dirty.

Give a worker a worktree to itself when it will:

- **edit files another worker might edit** — concurrent writes to one file lose
  work silently, and neither agent will understand why;
- **write git state**: `add`, `commit`, `checkout`, `rebase`, `stash`. The index
  and HEAD belong to the worktree, not to the process, so co-tenants share them:
  one worker staging its changes stages everyone's half-finished edits, and they
  will trip over each other's `index.lock`;
- **rebuild or regenerate into shared paths**, where two builds clobber each
  other's artifacts.

When you do share a worktree, say so in every brief that shares it: name the
files that worker may write, and tell it to leave git alone. A worker that was
not told will reasonably assume the checkout is its own. (Its launch preamble
already tells it not to touch git unless its task says the worktree is its own —
so if it *does* own one, say that too.)

`emacs-agent worktrees` prints every worktree of your repo with who is in it:

```
repo: ~/dev/luma-core/
2 agents  ~/dev/luma-core         luma-core (main)         dirty  claude(running) codex(idle)
0 agents  ~/dev/luma-core_2       luma-core_2 (PRO-14062)  dirty
0 agents  ~/dev/luma-core_5       luma-core_5 (main)       clean
```

- The count is live agent sessions there right now — the facts, not a verdict.
  Empty does not mean pristine, and occupied does not mean off-limits.
- `dirty` is uncommitted work in that checkout. For a writer, that is work you
  would be handing your worker along with its task; for a reader, it is context
  it will see and may report on.
- If a worker needs a checkout of its own and none is empty, **create the next
  numbered worktree yourself**:

  ```bash
  git -C ~/dev/luma-core worktree add ~/dev/luma-core_11 -b <branch> origin/main
  ```

  Numeric suffix, next unused number, based on the repo default (`origin/main`)
  unless the task is explicitly stacked on other work. This is the one case
  where you allocate a worktree without asking first: normally the human
  allocates them ahead of time, and running out mid-orchestration is exactly
  the situation that rule leaves to you. Say in your reply which worktrees you
  created.
- The directory must exist before you spawn; `spawn` refuses a path that does
  not.

## 2. Write the brief, then spawn

```bash
emacs-agent spawn --type claude|codex --worktree DIR --task TEXT [--title SHORT]
# --task-file FILE reads the brief from a file, which is easier for long ones
```

The worker starts with **your brief as its entire context**. It has never seen
this conversation, does not know the plan, and cannot ask the human. A brief that
works has all of:

- what to change, in terms of files or behaviour, not in terms of "continue";
- what *done* means — the check it should run, the output you want;
- what it owns: the files it may write, and whether the worktree is its own or
  shared (and so whether git operations are its to run);
- the constraints it must not break, and what it must not touch;
- anything it would otherwise have to guess: branch, ticket, prior decisions.

`spawn` prints the worker's id (e.g. `11588-42`), the worktree, and the path of
the brief file it wrote — that file is the record of what the worker was told,
and re-running its command by hand reproduces the launch.

The worker is launched with the protocol prepended to your task, so it already
knows to send exactly one `done`, to use `ask` instead of any interactive prompt,
and to stop when it is finished. You do not need to repeat that.

`--type` is any agent type configured in
`bp/agent-orchestration-launch-commands` (`claude` and `codex` out of the box —
adding one, or one with model flags, is a one-line defcustom edit the human can
make).

## 3. Wait, and keep waiting

```bash
emacs-agent wait --types done,blocked,question --timeout 90
```

- Returns as soon as matching mail arrives, printing `messages: N` and the
  messages. Each message names the worker that sent it.
- On timeout it prints `messages: 0 (nothing after 90s; call wait again …)`.
  **That is a checkpoint, not a failure.** Real tasks run 15–60 minutes. Call
  `wait` again; use `emacs-agent list` if you want to see that a worker is still
  alive. Do not re-dispatch or kill a worker for being slow.
- Set your own tool's timeout above `--timeout`, or the tool call is killed
  before the wait returns. A wait prints a keepalive line to stderr every 15s so
  a long wait does not read as a hung command.
- `wait` consumes what it returns. If several workers may finish together, keep
  waiting until you have heard from each one you are expecting.
- `emacs-agent check` reads mail without waiting; `check --peek` leaves it unread.

Mail also reaches you when you are not waiting: if a worker reports while you are
idle at your prompt, Emacs types the message into your terminal and submits it,
so you wake up with it as new input. Read it and continue the orchestration.

## 4. Answer questions, then synthesize

A `question` from a worker is blocking — it is sitting in `emacs-agent ask` until
you reply, so reply promptly:

```bash
emacs-agent send --to <worker-id> --type answer --subject "<the decision>" --body "<why, briefly>"
```

A `blocked` message is not blocking: the worker gave up and stopped. Decide
whether to respawn with a better brief, take the work yourself, or ask the human.

When everything is in, do the part only you can do: reconcile the reports, say
what actually landed and where, and name what is still open. A worker's `done`
summary is its own account of its work — if the result matters, verify it rather
than repeating the claim.

## What the human sees, and what they can do

- Every worker is a row in the dashboard, nested under yours, titled with the
  `--title` you gave it (or the first line of the task).
- `✉N` on a row means that agent has N unread messages waiting.
- The human can jump into any worker's terminal (`RET`) and talk to it directly.
  So a worker may have been redirected by them without telling you — if a report
  contradicts your brief, believe the report.
- Killing a worker's terminal is the human's call, not yours. There is no kill
  verb: when you are finished with a worker, leave it idle and say so.

## Limits worth knowing

- Mail lives in the running Emacs only. If Emacs restarts, the mailboxes are
  gone (the terminals die with it too).
- Ids are Emacs terminal ids (`11588-42`), stable for the life of the terminal.
- A worker that never fires an agent hook (a bare shell, a crashed launch) still
  appears in the dashboard, but with no agent status. If a spawn produces a row
  that never leaves `idle`, look at its terminal — the launch command failed.
- Nothing here reaches outside this machine's Emacs. Slack, GitHub and Linear are
  not part of the protocol; use your own tools for those, and only for the human.
