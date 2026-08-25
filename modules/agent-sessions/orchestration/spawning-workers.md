# How a subagent comes to exist in a specific worktree

Orca separates two things that are easy to conflate: **making another agent
exist somewhere** (worktree/terminal commands) and **putting it under
supervision** (orchestration task + dispatch). You can do either without the
other, and the shipped guide spends most of its length telling agents *not* to
combine them by reflex.

## The two placements

`Fresh worker` means a fresh agent session — not necessarily a new checkout.

**Same worktree** — when the task needs the current uncommitted state, must
validate or PR the current branch, or is explicitly scoped to this checkout:

```bash
orca terminal create --worktree active --title <task-name> --command "codex" --json
orca terminal wait --terminal <handle> --for tui-idle --timeout-ms 60000 --json
orca orchestration dispatch --task <task_id> --to <handle> --inject --json
```

**New worktree** — only when isolated checkout state is wanted:

```bash
orca worktree create --name <task-name> --agent codex --json   # read startupTerminal.handle
orca terminal wait --terminal <handle> --for tui-idle --timeout-ms 60000 --json
orca orchestration dispatch --task <task_id> --to <handle> --inject --json
```

`--agent` is described as **required for ordinary agent workers**: it launches
the agent *in the worktree's first terminal*, whereas bare `worktree create`
followed by `terminal create --command <agent>` leaves a stray fallback shell
next to the agent. Use the two-step path only when you need custom argv (Codex
`--model` / `-c model_reasoning_effort=…`, which `--agent` cannot pass) or
against an older CLI that rejects `--agent`. Known `--agent` ids seen in the
docs: `codex`, `claude`, `omp`, `pi`, `grok`.

Where the new agent's handle comes from, in order of preference:
`result.agentTerminalHandle` (with `--agent --json`), else
`result.startupTerminal.handle`, else re-resolve with `orca terminal list
--worktree … --json`. Message exactly one handle per worker; a stale one
surfaces as `terminal_handle_stale`.

### Lineage and base branch are separate decisions

- `--parent-worktree active|folder:<id>|worktree:<id>` records Orca *lineage*
  (the sidebar tree); `--no-parent` makes the new worktree top-level. Orca infers
  a parent from the calling terminal when you say nothing.
- Lineage does **not** choose the git base. Omit `--base-branch` to take the
  repo default; never silently branch from the current feature branch. The
  guide's rule: child lineage only for conceptually stacked work, `--no-parent`
  for independent repo-wide fixes and unrelated follow-ups.
- Worktree ids are the full `<repo-id>::<path>` values from `worktree list
  --json`; a bare repo id cannot target a worktree.

## Supervised dispatch vs. full handoff

This is the distinction the guide cares about most, because getting it wrong
either strands a worker under a coordinator that stopped listening, or leaves a
coordinator waiting on a worker that was never told to report.

|  | Supervised dispatch | Full handoff |
|---|---|---|
| Trigger | user explicitly asks to *supervise, monitor, wait, track completion, coordinate a DAG, use a decision gate, ask/reply* | "hand off", "handoff", "handover", "give this to another agent/worktree", "launch another agent to own this" |
| Create | `task-create` + `dispatch --to … --inject` | `worktree create --agent … --prompt …`, or `terminal send --text … --enter` |
| Worker obligations | `worker_done` exactly once, heartbeats, `ask`, `escalation` | none |
| Coordinator afterwards | `check --wait` loops | stops watching entirely |

Explicitly forbidden for a handoff: `task-create`, `dispatch --inject`,
`check --wait`, and reading the worker's terminal after prompt delivery to watch
progress. Naming a model or effort level ("gpt-5.5", "xhigh") does not make a
handoff supervised.

The custom-argv handoff, spelled out because `--agent` can't express it:

```bash
orca worktree create --name <task-name> --no-parent --json
orca terminal create --worktree id:<newFullWorktreeId> --title <task-name> \
  --command 'codex --model gpt-5.5 -c model_reasoning_effort="xhigh"' --json
orca terminal wait --terminal <handle> --for tui-idle --timeout-ms 60000 --json
orca terminal send --terminal <handle> --text "<task brief>" --enter --json
```

`terminal wait --for tui-idle` here is only about not losing the prompt to a
still-starting TUI — not about monitoring.

## What `--inject` actually puts in the worker

A ~2KB preamble (`buildDispatchPreamble`) followed by `=== TASK ===` and the
spec. It hard-codes the worker's identity and its whole protocol:

- "You are working inside Orca, a multi-agent IDE. You are a dispatched worker."
- the coordinator's handle and this worker's `taskId` / `dispatchId`;
- ready-to-run `send --type worker_done` (with the rule that `--body` is a
  three-sentence executive summary — what you did, what you found, what's left —
  and that failure is still a `worker_done`, never a silent exit);
- `send --type heartbeat` **every 5 minutes** while working, skipped only while
  blocked in `check --wait` or `ask`, which are themselves liveness;
- `ask` instead of any interactive prompt, and `escalation` for pre-completion
  blockers;
- an `=== AFTER YOU SEND worker_done ===` section: stop, idle at the prompt, do
  not poll, do not start unrelated work — a re-engagement arrives as fresh input
  with a new preamble. For a bare-shell worker the instruction inverts to *exit
  the shell*, since there is no idle agent prompt to reuse.
- "You talk to the coordinator only through the CLI commands below. Do not use
  Slack, GitHub comments, or any other channel to reach a human during the run."

Two conditional blocks get appended: `--- BASE DRIFT ---` when the worktree is
behind its base (with the five most recent missing subjects, and instructions to
pull or escalate), and `--- DECISION GATE RESOLVED ---` carrying a gate's
question and resolution when the task was previously blocked.

## Coordinator etiquette worth stealing

- `task-list --ready` is external memory; dispatch parallel waves; keep
  dependency chains to 3–4 steps.
- `check --wait` returns **one** message; loop N times for N workers, dispatching
  newly-ready tasks after each completion.
- A `check --wait` timeout or `{count:0}` is a checkpoint, not a failure: "long
  coding tasks routinely run 15-60 minutes". Heartbeats and terminal activity
  mean alive, not done — do not kill or restart a worker for not having finished.
- Reuse an idle agent in the required worktree only if the prompt allows it;
  otherwise create a fresh terminal there.
