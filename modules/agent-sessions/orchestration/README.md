# Orca agent orchestration — what it is and how it works

Notes on the orchestration feature of **Orca** (`/Applications/Orca.app`, an
Electron "IDE for parallel agentic development" by stablyai). They exist because
this module's dashboard tracks the same terminals Orca orchestrates, and because
the feature is only documented inside the app binary.

**Researched against Orca 1.4.146** (`Contents/Resources/app.asar`,
`package.json` version), on 2026-08-25. Everything below was read out of that
bundle or run from it; see [provenance.md](provenance.md) for how to re-derive
and re-verify any claim.

## The one-paragraph version

Orca runs every agent in a PTY it owns, gives each pane a **handle**
(`term_<uuid>`) and injects identifying env vars into its shell. A `orca` CLI on
each terminal's PATH turns those handles into an RPC surface against the running
app. Two command families sit on top: `orca worktree create` / `orca terminal
create` **make new agents** (a fresh checkout, or just a fresh agent session in
an existing one), and `orca orchestration …` gives them a **coordination
protocol** — a message bus with typed mail and group addresses, a task DAG with
dispatch records, decision gates, and a supervised worker lifecycle
(`worker_done` / `heartbeat` / `escalation` / `ask`). State lives in one SQLite
file, `~/Library/Application Support/orca/orchestration.db`. Delivery is not
polling: when a recipient's agent goes idle, the app types its pending mail into
that pane's PTY and presses Enter.

So "subagent" here means *another real agent process in another real terminal*,
tracked by a task row — not an in-process sub-agent like Claude Code's `Task`
tool. There is no LLM in the loop on Orca's side: the coordinator is itself an
agent driving the CLI, and `orca orchestration run`'s coordinator loop is plain
scheduling code.

## What it provides

| Capability | Surface |
|---|---|
| Create an agent in a **new worktree** | `orca worktree create --name … --agent codex [--prompt …]` |
| Create an agent in an **existing worktree** | `orca terminal create --worktree active --command "claude"` |
| Wait until the new agent's TUI is ready | `orca terminal wait --for tui-idle` |
| Give a worker tracked work | `orca orchestration task-create` + `dispatch --to <handle> --inject` |
| Worker→coordinator mail | `orchestration send --type worker_done\|heartbeat\|escalation\|status` |
| Blocking question from a worker | `orchestration ask` (blocks until the coordinator replies) |
| Coordinator waiting on workers | `orchestration check --wait --types worker_done,escalation,…` |
| Fan-out addressing | `--to @all` / `@idle` / `@worktree:<id>` / `@claude` / `@codex` / … |
| Dependency graph | `task-create --deps '["task_x"]'`; deps satisfied ⇒ auto-promote to `ready` |
| Human/coordinator decision points | `gate-create` / `gate-resolve` / `gate-list` |
| Automatic dispatch loop | `orchestration run --spec … [--max-concurrent N]` |
| Free-form handoff (no lifecycle) | `orca terminal send --text … --enter` |

## Reading order

- **[cli.md](cli.md)** — the command surface, flag by flag, with the semantics
  the handlers actually enforce (not just the usage strings).
- **[spawning-workers.md](spawning-workers.md)** — how a subagent comes to
  exist in a specific worktree, and the sharp distinction Orca draws between a
  *supervised dispatch* and a *full handoff*.
- **[internals.md](internals.md)** — transport, env vars, handles vs pane keys,
  the SQLite schema, how mail reaches a running TUI, the dispatch preamble, who
  is allowed to complete a task, and the coordinator loop's constants.
- **[emacs-notes.md](emacs-notes.md)** — how this relates to `agent-sessions.el`:
  what the two systems share on this machine, where the designs agree, and where
  they deliberately differ.
- **[provenance.md](provenance.md)** — where each fact came from and how to
  re-check it after an Orca update.

## Two caveats up front

- **The agent-facing guide is the spec, and it is shipped in the binary.** `orca
  skills get orchestration` prints 22KB of markdown that tells an agent exactly
  how to coordinate — including the etiquette rules ("do not use orchestration
  merely because the user says handoff"). It is quoted throughout these notes,
  and a verbatim copy is kept as [orchestration-skill-guide.md](orchestration-skill-guide.md)
  so the prose can be diffed after an update.
- **Nothing here has ever run on this machine.** `orchestration.db` exists at
  schema version 6 with **zero rows** in every table, and no orca skill stubs
  are installed under `~/.claude/skills/`. These notes describe the mechanism as
  built, not as exercised locally.
