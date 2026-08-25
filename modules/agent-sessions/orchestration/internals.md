# Internals

How the pieces actually fit, in the order a message travels: identity → CLI →
RPC → SQLite → back into a terminal.

## Identity: handles, pane keys, env vars

Every Orca-managed terminal gets env vars injected at spawn (see
`out/main/index.js`, terminal spawn env assembly):

| Variable | Meaning |
|---|---|
| `ORCA_TERMINAL_HANDLE` | `term_<uuid>`, the routing address used by every command |
| `ORCA_PANE_KEY` | stable pane identity (`<tabId>:<leafId>`-shaped); survives handle reminting |
| `ORCA_TAB_ID` | tab the pane lives in |
| `ORCA_WORKTREE_ID` | `<repo-id>::<path>` of the enclosing worktree |
| `ORCA_AGENT_LAUNCH_TOKEN` | per-launch token used by the hook forwarders |
| `ORCA_AGENT_HOOK_PORT` / `_TOKEN` / `_ENV` / `_VERSION` | local hook endpoint for agent events |
| `ORCA_CLI_COMMAND` | set to `orca-ide` / `orca-dev` where bare `orca` is wrong (WSL, dev) |

**Handles are routing metadata; pane keys are authority.** A pane can be
re-minted a new handle after a restart, so the CLI validates
`ORCA_TERMINAL_HANDLE` with `terminal.show` and, on `terminal_handle_stale` /
`terminal_gone`, re-resolves through `terminal.resolvePane` with the pane key
before baking a handle into anything. Lifecycle mail carries `sender_pane_key`
separately for exactly this reason.

Ids elsewhere are `<prefix>_<12 hex>`: `task_…`, `ctx_…` (dispatch context),
`gate_…`, `msg_…`, `run_…`.

## Transport

`orca` is a thin client: it reads `orca-runtime.json` from the app's userData
directory for the transport list plus an auth token, connects (a unix socket at
`~/Library/Application Support/orca/daemon/daemon-v*.sock`, or a websocket for
remote/SSH cases), sends a JSON envelope `{id, authToken, method, params}` and
prints the result. `orchestration.*` and `terminal.wait` are flagged as
long-poll methods so the transport does not time them out.

Params are validated with Zod on the app side, which is where most of the
"you can't do that" errors come from (mutually exclusive read modes, group
addresses for `worker_done`, reset scope, `--payload` vs structured flags).

The whole surface is registered unconditionally in this build:
`ORCHESTRATION_METHODS` is spread into the method table with no feature-flag
guard, and no `experimentalOrchestration` setting exists. The shipped guide
still lists "the orchestration experimental feature must be enabled in Settings
> Experimental" as a precondition — treat that as UI gating or stale copy, not
as an RPC gate.

## Storage: `orchestration.db`

One SQLite file (`~/Library/Application Support/orca/orchestration.db`), WAL,
`synchronous=NORMAL`, `busy_timeout=5000`, `user_version` = **6** on both disk
and code here. Schema is created with `IF NOT EXISTS` plus an explicit
transactional `migrate()` — the comment notes that `CREATE TABLE IF NOT EXISTS`
is a no-op against an existing DB, so widened CHECK constraints and new columns
need real migration steps (the v2 step rebuilds `messages` wholesale to admit
the `heartbeat` type).

- **`messages`** — `id`, `from_handle`, `to_handle`, `subject`, `body`, `type`
  (CHECK over the 8 types), `priority`, `thread_id`, `payload`, `read`,
  `sequence` (AUTOINCREMENT primary key, and the ordering everything relies on),
  `created_at`, `delivered_at`, `sender_pane_key`. Indexed on `(to_handle, read)`
  and on the undelivered-inbox tuple.
- **`tasks`** — `id`, `parent_id`, `created_by_terminal_handle`, `task_title`,
  `display_name`, `spec`, `status` (CHECK over the 6 statuses), `deps` (JSON
  array of task ids), `result`, `created_at`, `completed_at`.
- **`dispatch_contexts`** — `id`, `task_id`, `assignee_handle`,
  `assignee_pane_key`, `status` (`pending|dispatched|completed|failed|circuit_broken`),
  `failure_count`, `last_failure`, `dispatched_at`, `completed_at`,
  `last_heartbeat_at`.
- **`decision_gates`** — `id`, `task_id`, `question`, `options`, `status`
  (`pending|resolved|timeout`), `resolution`, timestamps.
- **`coordinator_runs`** — `id`, `spec`, `status`, `coordinator_handle`,
  `poll_interval_ms`, timestamps.

State is **runtime-global**, not per-worktree or per-repo: one message bus and
one task DAG per Orca instance. That is why `reset` is scoped by kind rather
than by project, and why two unrelated coordinations would collide.

Timestamps are stored as SQLite `datetime('now')` (UTC, space-separated) and
converted to ISO-8601 with a `Z` on the way out.

## Delivery: how mail reaches a running agent

Three paths, and only one of them is polling:

1. **Push into an idle TUI.** The app tracks each pane's agent status from OSC
   titles. On the transition *into* `idle` it calls `deliverPendingMessages`:
   fetch `read = 0 AND delivered_at IS NULL` for that handle, format them as
   banners, write them straight to the PTY, then write `\r` **500ms later** and
   mark them delivered. Two exceptions skip the Enter: the active coordinator's
   own handle, and cursor-agent panes — both are marked delivered without
   submitting. This is what makes coordination work without every agent running
   a poll loop.
2. **Blocking wait.** `check --wait` / `ask` register a waiter keyed by handle;
   `notifyMessageArrived` resolves the matching waiters (respecting `--types`)
   the moment a row is inserted, so the wait returns immediately rather than on
   a poll tick.
3. **Explicit read.** `check` / `inbox` at the agent's own initiative.

Each injected banner is self-documenting, ending with the exact reply command:

```
──── From: TERM_ABC (term_abc) [URGENT] (escalation) ────
Subject: …
<body>
[Payload: {"taskId":…}]
[Reply: orca orchestration reply --id msg_… --from <me> --body "..."]
────────────────────────────────────────────────────────────
```

Prompt injection itself (`sendTerminalAgentPrompt`) wraps the text in
**bracketed paste** markers with escapes sanitised (`ESC` → `<ESC>`), then
submits with `\r` — the same 500ms delay constant. That is how a 2KB preamble
lands in a TUI's composer as one paste instead of being interpreted as
keystrokes.

## Group addressing

`resolveGroupAddress` expands, always excluding the sender:

- `@all` — every live terminal;
- `@idle` — terminals whose agent status is `idle`;
- `@worktree:<worktreeId>` — terminals in that worktree;
- `@<agent-name>` — from a fixed list (`claude`, `openclaude`, `codex`,
  `opencode`, `mimo`, `gemini`, `droid`, `grok`, `cursor`), matched against the
  **terminal title**, i.e. against what the agent advertises, with a special
  matcher for cursor.

Anything else resolves to nothing, and `send` then errors rather than silently
dropping the message.

## Who may complete a task

`hasLifecycleAuthority`: if the dispatch recorded an `assignee_pane_key`,
the message's `sender_pane_key` must match it (leaf-id equality counts, so a
re-minted pane still passes); only for legacy dispatches without a pane key does
it fall back to comparing handles. A `worker_done` that fails this is not
dropped — it is **converted into a rejection in place**, its payload rewritten
with `_orcaLifecycleRejection: {code: "sender_not_assignee", reason}` naming the
expected and received handle/pane, and still delivered so the coordinator sees
what happened.

A valid `worker_done` must carry both `taskId` and `dispatchId`; it is ignored
if either is missing, the ids don't refer to each other, the dispatch is not the
task's current one, or the task is not `dispatched`. On success it sets the task
`completed` with `{completedBy, filesModified, completedAt}` as its result,
completes the dispatch, and **suppresses that dispatch's earlier heartbeats**
(marks them read+delivered) so the coordinator's inbox does not replay stale
liveness after a completion. Requiring both ids is deliberate: it stops a late
completion from a previously-failed retry completing the current dispatch.

`heartbeat` reconciliation is the same shape, updating
`dispatch_contexts.last_heartbeat_at` for the specific dispatch — again so a
straggler heartbeat from a dead dispatch cannot mask a hung retry.

## DAG mechanics

- `task-create` with no deps ⇒ `ready`; with deps ⇒ `pending`.
- Completing a task runs `promoteReadyTasks`: any `pending` task listing it as a
  dep whose *every* dep is now `completed` flips to `ready`. Same transaction as
  the status update, so there is no window where a task is completable but its
  children aren't promoted.
- `createDispatchContext` refuses a non-`ready` task and refuses a terminal that
  already holds an active dispatch; it carries forward the task's prior
  `failure_count` so retries accumulate.
- `failDispatch` increments that count: **3 failures ⇒ `circuit_broken`** and the
  task is marked `failed`; below 3 the task returns to `ready` for another
  attempt.
- `gate-create` completes the active dispatch and blocks the task;
  `gate-resolve` returns it to `ready`.

## The coordinator loop (`orchestration run`)

Constants, all from the `Coordinator` class: poll interval **2000ms**,
`maxConcurrent` **4**, stale/hung warning threshold **10 minutes**, stale-base
threshold **20 commits**, worker heartbeat cadence advertised as **5 minutes**.

Each tick: `processMessages` (worker_done / escalation / decision_gate /
heartbeat / status) → `processDecisionGates` (mark gated tasks `blocked`) →
`warnStaleDispatches` → `dispatchReadyTasks` → `checkConvergence`.

Notable choices:

- **It does not decompose.** Tasks must exist before `run`; the loop only
  schedules them.
- **Terminal supply is minimal.** It dispatches to terminals in its worktree
  that are connected, writable, not the coordinator, and not already assigned.
  If there are *none* it creates exactly one, titled `Worker: <first 40 chars of
  spec>`. It does not scale terminals to the ready set.
- **An escalation is treated as a dispatch failure**, feeding the same
  three-strikes circuit breaker.
- **A hung worker is warned about, never auto-failed.** The comment gives the
  reason: "the false-positive cost (a slow worker producing correct output) is
  higher than the false-negative cost (a hung worker keeps its terminal slot
  until a human notices)".
- **Stale base is a dispatch-time guard.** If the coordinator's worktree is more
  than 20 commits behind its base, the task stays `ready` and is retried next
  tick, unless the spec contains `allow-stale-base: true` — which is stripped
  out of the spec before the worker sees it, so the infra flag never reads as
  part of the instructions.
- Convergence = every task `completed` or `failed`. All-blocked-with-nothing-
  active logs "Stuck: … Resolve decision gates to continue" and keeps looping.

## How the UI reflects it

Orchestration context (task id, `task_title`, `display_name`) is tracked per
pane key and used to title panes and label coordinators. Because a dispatch
arrives as a *pasted prompt*, the app also recognises its own preamble in a
pane's prompt text — matching `"You are working inside Orca, a multi-agent
IDE."`, `"Your task ID is:"` and `"=== TASK ==="` — and compacts it to
`<prefix> Your task ID is: <id> === TASK === <first line of spec>` so the status
line shows the task instead of two kilobytes of protocol.
