# The `orca orchestration` command surface

Command list verified by running the bundled CLI directly
(`node out/cli/index.js orchestration --help`); flags come from
`out/cli/specs/orchestration.js`; the *behaviour* notes come from the RPC
handlers in the main bundle (`ORCHESTRATION_METHODS`) and from
`out/cli/handlers/orchestration.js`.

Every one of these is an RPC call into the running Orca app. With no app
running they fail; with `--json` they print machine-readable results, which is
what agents are told to use.

```
send           Send an inter-agent message
check          Check messages for a terminal
reply          Reply to a message
inbox          Show messages across (or for) recipients
task-create    Create an orchestration task
task-list      List orchestration tasks
task-update    Update a task status
dispatch       Dispatch a task to a terminal
dispatch-show  Show dispatch context for a task
ask            Ask the coordinator a question and block until answered
run            Start the coordinator loop
run-stop       Stop the active coordinator run
gate-create    Create a decision gate blocking a task
gate-resolve   Resolve a pending decision gate
gate-list      List decision gates
reset          Reset orchestration state (one scope; bare command resets all)
```

## Messaging

### `send`

```
orca orchestration send --to <handle|@group> --subject <text> [--from <handle>]
  [--body <text>] [--type <type>] [--priority normal|high|urgent]
  [--thread-id <id>] [--payload <json>]
  [--task-id <id>] [--dispatch-id <id>] [--files-modified <csv>]
  [--report-path <path>] [--phase <text>] [--json]
```

- **Types**: `status` (default), `dispatch`, `worker_done`, `merge_ready`,
  `escalation`, `handoff`, `decision_gate`, `heartbeat`. Enforced by a CHECK
  constraint in SQLite as well as by the Zod schema.
- **`--from` is normally omitted** — the CLI fills it from `ORCA_TERMINAL_HANDLE`,
  and re-resolves a stale handle through `ORCA_PANE_KEY` before using it.
- **Structured payload flags** (`--task-id`, `--dispatch-id`, `--files-modified`,
  `--report-path`, `--phase`) are assembled into the `payload` JSON for you, and
  are mutually exclusive with a raw `--payload`. They exist because "raw JSON
  arguments are fragile in Windows PowerShell".
- **Group addresses fan out** into one row per resolved recipient, sharing a
  generated `thread_id`. `worker_done` and `heartbeat` are *rejected* for group
  addresses at schema level: they are completion authority for one dispatch.
- Sending is what wakes a blocked `check --wait` on the recipient's handle, and
  what triggers push delivery into an idle recipient's terminal.

### `check`

```
orca orchestration check [--terminal <handle>] [--unread | --peek | --all]
  [--types <type,...>] [--inject] [--wait] [--timeout-ms <n>] [--json]
```

- `--unread` (default) returns unread mail **and marks it read**; `--peek`
  returns it without consuming; `--all` returns full history, consuming nothing.
  At most one mode.
- `--wait` blocks until a matching message arrives or the timeout expires. While
  blocked it prints a JSON keepalive line to **stderr every 15s**
  (`{"_keepalive":true,"_heartbeat":true,"elapsedMs":…}`) — sized deliberately:
  "15s is well under Claude Code's empirical ~2 min Bash-tool silence budget".
  `_heartbeat` is a deprecated alias of `_keepalive` and has nothing to do with
  `--type heartbeat` mail.
- `--inject` additionally returns the messages pre-rendered as banners for the
  agent that ran the command. It does **not** poke another terminal.
- Default wait timeout, when `--timeout-ms` is omitted, is 2 minutes.

### `reply`, `inbox`, `ask`

```
orca orchestration reply --id <msg_id> --body <text> [--from <handle>] [--json]
orca orchestration inbox [--limit <n>] [--terminal <handle>] [--full] [--json]
orca orchestration ask --to <handle> --question <text> [--options <csv>]
  [--timeout-ms <n>] [--from <handle>] [--json]
```

- `reply` marks the original read, then sends `Re: <subject>` back to its
  sender, threaded on the original's `thread_id` (or its id).
- `ask` is the worker's blocking question: it inserts one `decision_gate`
  message carrying `{question, options}`, then waits for a threaded reply and
  prints the answer. Default timeout **600s**; returns `timedOut: true` rather
  than erroring. Group addresses are refused.
- `ask` exists to replace interactive TUI prompts. The injected preamble states
  the rule bluntly: *"NEVER use AskUserQuestion … it opens a local TUI prompt
  that the coordinator cannot see and cannot answer — your session will hang
  forever waiting on a human."*

## Tasks and dispatch

```
orca orchestration task-create --spec <text> [--task-title <text>]
  [--display-name <text>] [--deps <json_array>] [--parent <task_id>] [--json]
orca orchestration task-list [--status <status>] [--ready] [--brief] [--json]
orca orchestration task-update --id <task_id> --status <status> [--result <json>] [--json]
orca orchestration dispatch --task <task_id> --to <handle> [--from <handle>]
  [--inject] [--dry-run] [--return-preamble] [--json]
orca orchestration dispatch-show --task <task_id> [--preamble] [--from <handle>] [--json]
```

- **Statuses**: `pending`, `ready`, `dispatched`, `completed`, `failed`,
  `blocked`. A task created **without** `--deps` starts `ready`; with deps it
  starts `pending` and is promoted to `ready` automatically when the last
  dependency completes.
- **Only a `ready` task can be dispatched** (checked in the handler *and* in
  `createDispatchContext`), and **a terminal may hold only one active dispatch**
  — a second one throws `Terminal <handle> already has an active dispatch`.
- `--inject` writes the dispatch preamble + task spec into the target's agent
  prompt. It first requires a recognised agent in that terminal; against a bare
  shell it refuses with a message telling you to start an agent CLI or drop
  `--inject`.
- `--dry-run` renders the preamble for inspection without creating anything;
  `--return-preamble` returns it alongside a real dispatch.
- A valid `worker_done` completes the task by itself. The guide is explicit:
  *"Do not follow it with `task-update --status completed`; reserve manual
  updates for explicit recovery or overrides."*
- `--brief` collapses whitespace and caps each spec at 160 chars, marking
  shortened rows with `spec_truncated` — meant for coordinator sweeps.

## Gates and the coordinator loop

```
orca orchestration gate-create --task <task_id> --question <text> [--options <json_array>] [--json]
orca orchestration gate-resolve --id <gate_id> --resolution <text> [--json]
orca orchestration gate-list [--task <task_id>] [--status pending|resolved|timeout] [--json]
orca orchestration run --spec <text> [--from <handle>] [--poll-interval-ms <n>]
  [--max-concurrent <n>] [--worktree <selector>] [--json]
orca orchestration run-stop [--json]
```

- Creating a gate **completes the task's active dispatch and sets the task
  `blocked`**; resolving one puts it back to `ready`, and the next dispatch of
  that task appends a `--- DECISION GATE RESOLVED ---` block (question +
  resolution) to the worker's preamble.
- Gates are for coordinator-owned DAG decisions. A worker's `ask` is answered
  with `reply`, not with a gate.
- `run` returns immediately with a `runId` and runs the loop in the background;
  progress is read with `task-list`. Only one run at a time — a second throws
  `Coordinator already running`.
- `run` does **no decomposition**: it errors with *"No tasks found. Create tasks
  with orchestration.taskCreate before running the coordinator"*. The comment in
  the source says AI-driven decomposition "belongs in a future phase where the
  coordinator itself is an LLM agent".

## `reset`

```
orca orchestration reset [--all | --tasks | --messages] [--json]
```

Exactly one scope is required (the bare form is rejected by the schema despite
the summary line). State is runtime-global, so this is a recovery tool: *"Do not
run it during active coordination unless explicitly abandoning that state."*

## Adjacent commands a coordinator needs

Not part of `orchestration`, but the guide treats them as one workflow:

```
orca status --json                       # is the runtime up?
orca worktree create --name … --agent …  # new checkout + agent (see spawning-workers.md)
orca worktree list|show|current|ps       # `ps` is a compact cross-worktree summary
orca terminal list|show|read|send|wait|create|split|close|rename|switch
orca skills get orchestration            # print the full agent-facing guide
```

`orca claude-teams [claude args…]` is a *different* feature: it starts Claude
Code's own Agent Teams in the current pane and surfaces its teammates as native
Orca splits. Those teammates are Claude's subagents, tracked for display by
`out/shared/claude-subagent-roster.js`; they are not orchestration tasks and
carry no `worker_done` lifecycle.
