# Relation to `agent-sessions.el`

Why these notes live in this module, what the two systems already share on this
machine, and where the designs converge or deliberately part.

## They already share a hook forwarder here

`~/.orca/agent-hooks/claude-hook.sh` — the script this machine's
`~/.claude/settings.json` was originally wired to — serves **both** dashboards
from one invocation:

1. bail out if the payload is empty, or if `DEVIN_PROJECT_DIR` is set;
2. if `EMACS_AGENT_SESSION_ID` is set and `emacsclient` exists, spool the payload
   to a temp file and call `(bp/agent-hook-notify <id> "claude" <file>)`
   `--no-wait`;
3. source `$ORCA_AGENT_HOOK_ENDPOINT` (a four-line env file under
   `~/Library/Application Support/orca/agent-hooks/endpoint.env` carrying port,
   token, env and version);
4. `POST http://127.0.0.1:$ORCA_AGENT_HOOK_PORT/hook/claude` with
   `X-Orca-Agent-Hook-Token`, form-encoding `paneKey`, `tabId`, `launchToken`,
   `worktreeId`, `env`, `version` and the raw payload;
5. exit 0 regardless.

Both consumers are addressed by an id the terminal's own environment carries —
`EMACS_AGENT_SESSION_ID` for us, `ORCA_PANE_KEY`/`ORCA_TERMINAL_HANDLE` for Orca
— which is why one script can fan out to both without either knowing about the
other. This module's `CLAUDE.md` records that agent-sessions ships its own
forwarders precisely so it does not depend on this file; the sharing is
incidental, and this note is here so a future edit does not mistake it for a
dependency.

## The parts that arrived at the same answer

- **Inject an id into the terminal's env; make that the identity.** Orca injects
  `ORCA_TERMINAL_HANDLE`/`ORCA_PANE_KEY`; we inject `EMACS_AGENT_SESSION_ID`.
  Both then treat the agent's own session UUID as secondary metadata.
- **Identity survives, addresses don't.** Orca's handle can be re-minted, so
  lifecycle authority is checked against the *pane key* and a stale handle is
  re-resolved through it. Our equivalent is keying notes and shared files on the
  agent session id with the buffer name as fallback, so renaming a terminal
  doesn't orphan them.
- **Push, don't poll.** Orca writes pending mail into a pane the moment its agent
  goes idle; our shared-file inbox lights a row up on the hook event. Neither
  makes the human (or the agent) go looking.
- **Re-sharing means "look again", not "add a row".** Their `worker_done`
  suppresses earlier heartbeats for the same dispatch rather than letting the
  inbox replay them; ours clears `visited_at` but keeps `shared_at` so a re-share
  re-highlights in place. Same instinct: the inbox must not accumulate noise, and
  a row must not jump out from under the reader.
- **Don't let a status be re-derived.** They record `delivered_at` and reconcile
  lifecycle mail exactly once; we record `(line-pos . status)` at render time
  because "a row's status is whatever was rendered".

## The parts we deliberately don't have

- **A task DAG and a dispatch record.** Orca's whole point is provenance: a task
  row, a dispatch context, an assignee pane, and a completion authorised by that
  pane. Our dashboard is explicitly ephemeral — rows are rebuilt from live
  buffers, and the persisted set is limited to what the user chose or cannot
  reconstruct. Adding task state would cross that line.
- **Typing into another agent's TUI.** Orca dispatches by pasting a preamble into
  a pane and pressing Enter 500ms later. We only ever send to a terminal we just
  created (`--terminal-send` for resume/branch), never to one an agent is
  currently using.
- **A coordinator.** Nothing in this module schedules agents; `b` (branch) forks
  a conversation and stops there.

## Interop, if it ever comes up

Today there is none, and two facts block it:

- **`orca` is not on PATH on this machine.** The CLI ships as
  `/Applications/Orca.app/Contents/Resources/bin/orca` (a bash shim that resolves
  the app and execs its bundled node); nothing symlinks it into `/usr/local/bin`
  or Homebrew's bin. Agents in our eat/vterm terminals cannot run it at all
  unless the shim is linked or invoked by full path.
- **An Emacs terminal is not an Orca pane.** With no `ORCA_TERMINAL_HANDLE` /
  `ORCA_PANE_KEY` in the environment, `orchestration send/check` fall back to the
  handle `unknown`, `--from` must be passed by hand, and nothing can dispatch
  *into* the terminal — there is no pane for Orca to write to and no pane key to
  grant lifecycle authority. An Emacs-hosted agent could at most act as a
  read-only observer of the message bus with explicit `--terminal` flags.

If observing were ever wanted, the honest route is reading
`~/Library/Application Support/orca/orchestration.db` directly (WAL, read-only,
schema in [internals.md](internals.md)) rather than pretending to be a pane. Note
what that would import, though: orchestration state is runtime-global and
persists across sessions, which is exactly the kind of thing this module's
ephemerality rule keeps out of rows.
