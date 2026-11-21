# emacs-lock — implementation plan

Design rationale lives in `CLAUDE.md` → "Locks". This file is the build plan.

## Settled decisions

- `bin/emacs-lock` (POSIX sh + `/usr/bin/sqlite3`) reads and writes `agent-sessions.db` directly. Emacs is not in the lock path.
- Emacs owns the schema; the CLI fails loudly if tables are missing.
- Holder = durable conversation id; held locks persist across terminal death, `k`, crashes and restarts. A dead holder shows *not running* and still blocks.
- No timeouts on held locks. The user force-releases from the dashboard.
- Force-release writes a notice to the holder's mail: delivered if running, stored without reviving if not.
- Directory locks conflict by containment; free-form names are registered, listed for 30 days, and creation is gated by a seen-cursor.
- `LOCAL_STACK.md` migrates last, after tests.

## CLI surface

```
emacs-lock list [--all]
emacs-lock acquire NAME [--why TEXT] [--new DESC] [--owner NAME]
emacs-lock wait    NAME [--why TEXT] [--new DESC] [--owner NAME] [--timeout S] [--interval S]
emacs-lock release NAME [--owner NAME]
```

Exit codes: 0 granted/released/listed; 1 refused (held, unknown name, stale cursor, not holder); 2 usage; 3 environment (no DB, no tables, no identity, sqlite3 missing).

Output on refusal names the holder, whether it is running, how long it has held, its `why`, and the queue length.

## Name resolution (CLI)

1. Contains `/`, or starts with `~` or `.` → directory. Expand `~`, `cd -- DIR && pwd -P`, must exist. Stored with a trailing `/` (`/Users/x/dev/project/`) so containment is a plain prefix test and `/` itself works.
2. Otherwise free-form: trim, lowercase, collapse whitespace runs to `-`. Reject empty or anything containing `/` after normalisation. The two namespaces cannot collide because only directories contain `/`.

## Holder identity (CLI)

In order, first that is set:

| source | kind | key |
|---|---|---|
| `--owner NAME` | `owner` | NAME |
| `CLAUDE_CODE_SESSION_ID` | `session` | value |
| `CODEX_THREAD_ID` / `CODEX_SESSION_ID` | `session` | value |
| `EMACS_AGENT_SESSION_ID` → newest open `sessions` row's `agent_sid` | `session` | agent_sid |
| none | — | exit 3 |

`session` matches mail's `to_kind`, so a notice can be filed under the same key. The process pid is `CLAUDE_PID` when set, else empty (Emacs fills it; see below). Label = session title from `sessions` if found, else the key.

DB path: `$EMACS_AGENT_SESSIONS_DB`, else `~/.emacs.d/agent-sessions.db`. Emacs injects the env var into terminals so it follows `bp/agent-sessions-db-file`.

## Schema (pure additions → `IF NOT EXISTS`, no version bump)

```sql
CREATE TABLE IF NOT EXISTS locks (
  name         TEXT PRIMARY KEY,      -- normalised name or dir with trailing '/'
  is_dir       INTEGER NOT NULL,
  holder_kind  TEXT NOT NULL,
  holder_key   TEXT NOT NULL,
  holder_label TEXT,
  holder_pid   INTEGER,               -- NULL = unknown
  holder_pid_start TEXT,              -- `ps -o lstart=` at record time
  why          TEXT,
  acquired_at  INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS lock_waiters (
  ticket       INTEGER PRIMARY KEY AUTOINCREMENT,
  name         TEXT NOT NULL,
  is_dir       INTEGER NOT NULL,
  waiter_kind  TEXT NOT NULL,
  waiter_key   TEXT NOT NULL,
  waiter_label TEXT,
  why          TEXT,
  expires_at   INTEGER NOT NULL,
  UNIQUE (name, waiter_kind, waiter_key));

CREATE TABLE IF NOT EXISTS lock_names (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  name         TEXT NOT NULL UNIQUE,
  description  TEXT NOT NULL,
  created_by   TEXT,
  created_at   INTEGER NOT NULL,
  last_used_at INTEGER NOT NULL);

CREATE TABLE IF NOT EXISTS lock_name_seen (
  holder_kind  TEXT NOT NULL,
  holder_key   TEXT NOT NULL,
  seen_id      INTEGER NOT NULL,
  PRIMARY KEY (holder_kind, holder_key));
```

Added to the schema list *and* the load-time re-migration form (CLAUDE.md "Editing").

## Transactions (each one `sqlite3` invocation, `.timeout 5000`, `BEGIN IMMEDIATE … COMMIT`)

Conflict predicate for candidate `N` against row `L` (both dirs): `substr(N,1,length(L.name)) = L.name OR substr(L.name,1,length(N)) = N`; for names: equality. Never `LIKE` — `_`/`%` in paths.

**acquire**
1. Delete expired waiters (`expires_at < now`).
2. Free-form only: if name not in `lock_names` → without `--new` refuse "unknown name" + recent names; with `--new`, refuse if `max(id) > seen_id` (or no `seen` row) with names newer than `seen_id`; else insert. Every refusal that prints names upserts `seen_id = max(id)`.
3. If holder already holds exactly `N` → print "already held", success.
4. If any conflicting `locks` row → refuse with holder info.
5. If any live conflicting waiter that isn't us → refuse "queued: K ahead".
6. Insert lock; delete our own waiter row; bump `lock_names.last_used_at`.

**wait** — loop: same transaction but step 5 compares tickets: we proceed only if no conflicting live waiter has a lower ticket; otherwise upsert our waiter with `expires_at = now + 3*interval` and sleep. First blocked poll prints the holder once. `--timeout` deletes our waiter and exits 1. Default: no timeout (agents run it in the background).

**release** — delete where `name = N AND holder = us`; 0 rows → refuse "not holder" + who does hold it. Bump `last_used_at`.

**list** — held locks with queue, then `lock_names` with `last_used_at >= now - 30d` (all with `--all`); upsert `seen_id`. Running status computed per row: pid set, `ps -o lstart= -p PID` equals `holder_pid_start`.

SQL values are passed by doubling `'`; numbers are validated as digits before interpolation. (Use `.parameter set` if the quoting gets hairy.)

After every committed change: `emacsclient -e '(bp/agent-locks-changed)' >/dev/null 2>&1 &` — best-effort, backgrounded so a hung server cannot stall the agent, never affects the exit code.

## Emacs side

- Schema additions + re-migration (above). Inject `EMACS_AGENT_SESSIONS_DB` in `--terminal-environment`.
- `bp/agent-locks-changed` → reload working copy (`locks`, `lock_waiters`) into a hash table, refresh dashboard if visible. Plus a 30s timer while the dashboard is visible.
- Dashboard: a marker on session rows that hold / wait for locks; a `Locks` section listing each held lock (holder, running/not running, age, why) with its queue.
- Force-release (key on a lock row, `yes-or-no-p`): delete the row in a transaction; write a `lock-revoked` notice:
  - holder running in a live terminal → normal mail delivery path;
  - otherwise → `bp/agent-orchestration--mail-insert` only (no `send`, no revival).
  - Needs a mail type: add `lock` to `bp/agent-orchestration-message-types`, or a dedicated system sender — decide while implementing.
- Restore: when a hook event attaches an agent sid that holds locks whose pid is not alive, record the terminal's process pid + start time.

## Verify while implementing

- What `CLAUDE_CODE_CHILD_SESSION` means, and whether Agent-tool subagents get their own `CLAUDE_CODE_SESSION_ID`. If they do, they become separate holders (good); if not, document that subagents share their parent's locks.
- pi's identity: the session-log fallback should cover it; confirm.
- Whether `CLAUDE_PID` is the long-lived Claude process (not a per-command shell).

## Tests

ERT file `agent-sessions-lock-test.el`, batch Emacs: create a temp DB through the real schema code, then drive `bin/emacs-lock` via `call-process` with `EMACS_AGENT_SESSIONS_DB` and fake identity env vars.

- acquire/release round-trip; re-acquire is a no-op; release by non-holder refused.
- Directory containment both ways; sibling dirs don't conflict; path with `_`, `%`, `'`, spaces.
- Name normalisation; unknown name refused; `--new` cursor race (A list, B list, A new OK, B new refused with A's name, B new again OK).
- Concurrency: N parallel `acquire` processes on one name → exactly one wins.
- Queue: FIFO order; non-waiting acquire doesn't jump; expired waiter skipped; `--timeout` removes the ticket.
- Running status: live pid, dead pid, reused pid (start time mismatch).
- Missing tables / missing DB / no identity → exit 3, nothing written.
- Emacs: force-release of a not-running holder writes mail and spawns nothing; dashboard reload after poke.

No live-Emacs tests (CLAUDE.md "Live Emacs safety").

## Commit sequence

1. Lock tables in the schema and re-migration.
2. `emacs-lock` acquire/release/list over SQLite (identity, name resolution, containment).
3. Fair waiting queue and `wait`.
4. Name registry with the seen-cursor gate.
5. Emacs: working copy, poke entry point, DB path in terminal env.
6. Dashboard: lock markers, Locks section, force-release with revocation notice.
7. Re-record holder process on restore.
8. Tests.
9. Docs: `CLAUDE.md` "Locks" (already drafted), orca guides mention `emacs-lock`.
10. Afterwards, separately: move `~/dev/project/LOCAL_STACK.md` to `emacs-lock wait ~/dev/project` (holding the stack lock protocol while editing it).
