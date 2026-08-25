---
name: orca
description: >-
  Coordinate other agents from an Emacs-managed terminal: start a claude/codex
  worker in another git worktree, give it a task, and block until it reports.
  Use when the user says "/orca", "/orca --main", "orchestrate this", "spawn a
  subagent", "run these in parallel", "dispatch this to another agent", or asks
  you to supervise, wait for, or coordinate other agents. Also use when you find
  yourself running as someone's worker (EMACS_AGENT_ROLE=worker) and need the
  reporting protocol.
---

# Orca — agent orchestration through Emacs

The full, version-matched guide is printed by the `emacs-agent` command itself,
so it cannot drift from the tool it documents. Load it now, then follow it:

- **Coordinating others** — you were asked to orchestrate, the invocation
  included `--main`, or you are about to spawn a worker:

  ```bash
  emacs-agent guide --main
  ```

- **Working for a coordinator** — `$EMACS_AGENT_ROLE` is `worker`, or you were
  handed a task brief that names a coordinator:

  ```bash
  emacs-agent guide
  ```

Read whichever applies before running any other `emacs-agent` verb. If both
apply (you are a worker that needs to spawn workers of its own), read both.

`emacs-agent` only exists inside a terminal Emacs manages, and needs that Emacs
running with its server started. If the command is missing, say so plainly —
there is nothing to orchestrate from here, and the user's dashboard is where this
feature lives (`M-x bp/agent-sessions-list`, `M-x bp/agent-orchestration-install-skill`).
