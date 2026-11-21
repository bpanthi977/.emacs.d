---
name: orca
description: >-
  Coordinate and exchange messages with other agents from an Emacs-managed
  terminal: start a claude/codex worker, send or reply to agent mail, and
  receive reports either by waiting in the current turn or after yielding.
  Use when the user says "/orca", "/orca --main", "orchestrate this", "spawn a
  subagent", "run these in parallel", "dispatch this to another agent", or asks
  you to supervise, wait for, coordinate, or message other agents. Also use when
  another agent's message identifies itself as "$orca (/orca)", or when you are
  running as someone's worker (EMACS_AGENT_ROLE=worker) and need the reporting
  protocol.
---

# Orca — inter-agent coordination through Emacs

The full, version-matched guide is printed by the `emacs-agent` command itself,
so it cannot drift from the tool it documents. Load it now, then follow it:

- **Messaging a peer or coordinating workers** — you were asked to message,
  ask, or answer another existing agent session; you were asked to orchestrate;
  the invocation included `--main`; you are about to spawn a worker; or you
  need to send or answer agent mail outside a worker lifecycle:

  ```bash
  emacs-agent guide --main
  ```

- **Playing a role from a mailbox** — you were started in a directory whose
  `context.md` makes you a persistent role, or told to `wait --mailbox`: read
  the *Mailboxes* section of `emacs-agent guide --main`.

- **Working for a coordinator** — `$EMACS_AGENT_ROLE` is `worker`, or you were
  handed a task brief that names a coordinator:

  ```bash
  emacs-agent guide
  ```

Read whichever applies before running any other `emacs-agent` verb. If both
apply (you are a worker that needs to spawn workers of its own), read both.

Peer messaging is not orchestration. When you are only exchanging messages with
an agent that already works for the human, there is no worker, no brief, and no
final report: use `send`, `send-and-wait`, and the `Reply:` line, and follow the
guide's *Messaging a peer* section. The worktree, brief, model, and
coordinator-ledger rules below apply only when you dispatch workers.

While coordinating workers, maintain a durable list of every question or decision that
still requires the human's answer. Agent mail and later status updates must not
push those items out of view: include a numbered `Pending your decisions:`
section in every user-facing status or handoff until each item is explicitly
answered or becomes inapplicable. Handle routine choices already determined by
the human's standing instructions yourself instead of adding them to the list.

Agent mail is persisted immediately. Codex uses its native session queue;
Claude stashes and restores any human draft around its queue-submit action.
Mail is `status` (the default), `question` (the sender waits on a reply) or
`answer`. Questions and answers can steer a running Claude or Codex
conversation; plain status updates wait unless later actionable mail carries
them along. Other agent types receive pending mail when idle.
After sending, choose based on what the current turn needs:

- If you need the other agent's result before responding, use
  `emacs-agent wait`. It polls the mailbox and returns the message as command
  output; it does not depend on idle-terminal delivery.
- Otherwise, yield normally instead of polling. Claude and Codex can receive
  actionable mail at a safe boundary during an active turn; idle delivery
  starts a new turn for pending mail that was not queued earlier.

You may continue unrelated work in the current turn, but actionable mail may
steer a running Claude or Codex recipient. A worker that sent a question remains
blocked until you collect the answer with `wait` or the agent applies it as
queued input.

For a conversational request whose answer is needed immediately, use
`emacs-agent send-and-wait` instead of separate `send` and `wait` calls. It
defaults to sending a `question` and returns on the first mail back; use plain
`send` and yield when the current response does not depend on the reply.

`send --to` accepts a current terminal id, a durable agent session id, a full
`agent-session:AGENT::SESSION-ID::WORKTREE` link, or a mailbox directory. Dead terminal addresses and
durable addresses restore resumable conversations in a new worker terminal;
already-restored conversations route to their live terminal. The command
reports the new/current terminal id; a killed bare shell has no conversation to
restore and still fails.

`emacs-agent` only exists inside a terminal Emacs manages, and needs that Emacs
running with its server started. If the command is missing, say so plainly —
there is nothing to orchestrate from here, and the user's dashboard is where this
feature lives (`M-x agent-sessions-list`, `M-x agent-sessions-orchestration-install-skill`).
