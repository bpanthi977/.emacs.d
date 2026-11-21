# Coordinating and messaging agents from an Emacs terminal

You run in a terminal Emacs owns, and can start other agents, give each one a
task, send agent mail, and block until a reply or report arrives. Your tool is
`emacs-agent`, already on your PATH. Everything it does goes through the Emacs
that owns this terminal, so the human sees every agent you start or message in
the same dashboard (`M-x agent-sessions-list`).

The same mail works in two relationships. Decide which one you are in first:

- **Peer messaging** — you and another agent each already work for the human,
  and you are only exchanging information: asking what another session found,
  telling it something changed, answering its question. Nobody dispatched
  anybody. Read *Messaging a peer* below and skip the coordinator sections.
- **Coordinating** — you start workers, hand each a brief, and wait for their
  final reports. Read *Messaging a peer* for the mail mechanics, then §1–§5.
- **Playing a role** — you were started in a mailbox directory to be a
  persistent role (its `context.md` says so). Read *Mailboxes* below.

If the human asks you to talk to an existing session, that is peer messaging,
even if you are asking it to do something. Do not turn it into a worker with a
brief, and do not expect a final report from it.

## Messaging a peer

```bash
echo "$EMACS_AGENT_SESSION_ID"            # your own address, if a peer needs it
emacs-agent send-and-wait --to <address> --subject "<topic>" --body "<question>" \
  --timeout 90                            # you need the reply in this turn
emacs-agent send --to <address> --type status --subject "<fyi>" --body "..."
emacs-agent send --to <address> --type answer --subject "<reply>" --body "..."
```

- **Addresses.** A peer's address comes from the human (a terminal id like
  `11588-42`, a durable session id, an `agent-session:` link, or a mailbox
  directory) or from the `Reply:` line of mail it sent you. `emacs-agent list` shows only workers you
  spawned, and `emacs-agent worktrees` shows who is where but not their ids, so
  neither finds a peer for you. If you have no address, ask the human for one
  instead of guessing. Your own id rides along on everything you send, so the
  peer can always reply.
- **Types.** There are three. Use `question` when you need a reply, `answer`
  to reply to one (the `Reply:` line already says so), and `status` (the
  default) for everything else. A question's `Reply:` line names the command
  to answer it; any other mail's says `Reply (optional):`. `status` does not
  interrupt a running peer; `question` and `answer` can steer it at its next
  safe boundary.
- **Waiting.** Use `send-and-wait` only when your current response depends on
  the reply; otherwise `send` and yield. A peer's reply arrives as new input
  later, and the peer may be busy with its own task for a long time.
- **Write a self-contained message.** The peer has its own conversation and has
  not seen yours: give paths, branch names, and exactly what you need back.
- **The peer owns its work.** It answers to the human, not to you. Ask it, tell
  it, or suggest; do not edit its files or git state, and do not assume it will
  drop its task for yours. If it declines or the human redirected it, take that
  as the answer.
- **Answering a peer.** When `$orca (/orca)` mail arrives from a peer, reply
  with an `answer` through its `Reply:` command once you have something useful,
  even if the answer is "not known here". If the question needs the human's
  decision, say so in your answer and put it on your own pending-decisions list
  rather than deciding for them.

Peer mail has the same delivery, persistence, and revival behavior described in
§4, §5, and *Limits worth knowing*: a stopped peer conversation is revived by a
message sent to its durable address.

The remaining sections are for coordinating workers.

There is no task table, no dependency graph and no scheduler. **Your context is
the plan.** Keep the list of who is doing what in your head (or in a file you
share with the human) — Emacs only owns what you cannot do yourself: starting a
terminal, addressing a live one, and waiting.

## The coordinator loop

```bash
emacs-agent worktrees                     # who is working where
emacs-agent spawn --type claude --worktree ~/dev/project_5 \
  --task "$(cat /tmp/brief.md)" --title "auth rewrite" --model opus
emacs-agent wait --timeout 90
emacs-agent send --to 11588-42 --type answer --subject "Use approach B" --body "..."
emacs-agent send-and-wait --to 11588-42 --subject "Follow-up" --body "..."
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

Workers in separate worktrees can still collide on something outside them: a
shared local stack, a database, a port. Tell each such worker which
`emacs-lock` name guards it (a directory, such as `~/src/app`, names
itself), so they take turns
instead of each inventing a different name for the same resource.

When you do share a worktree, say so in every brief that shares it: name the
files that worker may write, and tell it to leave git alone. A worker that was
not told will reasonably assume the checkout is its own. (Its launch preamble
already tells it not to touch git unless its task says the worktree is its own —
so if it *does* own one, say that too.)

`emacs-agent worktrees` prints every worktree of your repo with who is in it:

```
repo: ~/dev/project/
2 agents  ~/dev/project           project (main)           dirty  claude(running) codex(idle)
0 agents  ~/dev/project_2         project_2 (fix-login)    dirty
0 agents  ~/dev/project_5         project_5 (main)         clean
```

- The count is live agent sessions there right now — the facts, not a verdict.
  Empty does not mean pristine, and occupied does not mean off-limits.
- `dirty` is uncommitted work in that checkout. For a writer, that is work you
  would be handing your worker along with its task; for a reader, it is context
  it will see and may report on.
- If a worker needs a checkout of its own and none is empty, **create the next
  numbered worktree yourself**:

  ```bash
  git -C ~/dev/project worktree add ~/dev/project_11 -b <branch> origin/main
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
emacs-agent spawn --type claude|codex --worktree DIR --task TEXT \
  [--title SHORT] [--model NAME]
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
knows to send you exactly one final report (a `status` whose subject starts
`Done:` or `Failed:`), to ask with `send-and-wait` instead of any interactive
prompt, and to stop when it is finished. You do not need to repeat that.

`--type` is any agent type configured in
`agent-sessions-orchestration-launch-commands` (`claude` and `codex` out of the box —
adding one is a one-line defcustom edit the human can make).

## 3. Choose the model, when the task has an opinion about it

`--model NAME` launches that worker on a specific model instead of the human's
default. `NAME` is whatever the agent's own CLI accepts — for `claude`, an alias
like `opus`, `sonnet`, `haiku` or `fable`, or a full model id; for `codex`, one
of its model names. Nothing here validates the name (a list would be stale
within a release), so spell it the way that CLI does.

Match the model to the work, not to your own:

- **a big or subtle task** — design, a tricky bug, code whose correctness you
  cannot cheaply verify yourself — wants the strongest model. This is also the
  case where a weaker model costs you *more* than it saves: you pay for the run
  and then for reading a report you cannot trust.
- **mechanical, verifiable, or wide work** — a rename across files, collecting
  facts, running a check and reporting what it printed, one summary per file —
  runs fine on a smaller, faster model, and that is what makes spawning ten of
  them reasonable.
- **omit it** when you have no reason to prefer one. The default is the human's
  own configured model, which is the right thing to inherit by default.

Say which model you gave each worker when you report, so the human can read a
thin result as "wrong model for it" rather than as "the worker failed".

A model name the CLI does not recognise is not caught here: the worker dies on
its own error message and never reports. `spawn` echoes the model it launched
with (`Spawned 11588-42 (claude, opus) in …`) — if a worker goes silent
immediately, that line is where to check the spelling.

## 4. Wait, and keep waiting

```bash
emacs-agent wait --timeout 90
```

- Returns as soon as any mail is there, and prints all of it. A progress
  `status` ends the wait too: read it, and call `wait` again if it is not what
  you were waiting for.
- On timeout it prints `messages: 0 (no mail after 90s; call wait again …)`.
  **That is a checkpoint, not a failure.** Real tasks run 15–60 minutes. Call
  `wait` again; use `emacs-agent list` if you want to see that a worker is still
  alive. Do not re-dispatch or kill a worker for being slow.
- Set your own tool's timeout above `--timeout`, or the tool call is killed
  before the wait returns.
- `wait` consumes everything it prints. If several workers may finish together,
  keep waiting until you have heard from each one you are expecting.
- To stop a `wait` you started in the background, stop that task by its id
  (your harness's stop-task tool) or kill the exact pid you started. Never
  `pkill -f "emacs-agent wait"` or kill by any other pattern: it matches every
  agent's mailbox watcher on the machine and silently cuts them off from mail.
- `emacs-agent check` reads mail without waiting; `check --peek` leaves it
  unread.

Mail also reaches you when you are not waiting. Questions and answers are queued
into a running Claude or Codex conversation and can steer it at a safe boundary;
plain status updates, final reports included, do not steer a running Codex turn
on their own. Codex uses its native session queue. Claude stashes and restores any
partially written human prompt around its queue-submit action. When another
agent type is idle, Emacs types pending `$orca (/orca)` mail into its terminal
and submits it as new input. Read it and continue the coordination or
conversation.

## 5. Answer questions, then synthesize

For a conversational request whose result you need in the current turn, send
it and wait for the reply in one command:

```bash
emacs-agent send-and-wait --to <address> --subject "<topic>" --body "<question>" \
  --timeout 90
```

It defaults to an outgoing `question` and, like `wait`, returns on any
incoming mail; `--type` overrides the outgoing type. Delivery completes before
polling starts, so
the same durable-address routing and revival behavior as `send` applies. If the
current response does not need the reply, use `send` alone and yield normally.

A `question` from a worker is blocking — it is sitting in `emacs-agent
send-and-wait` until you reply, so reply promptly:

```bash
emacs-agent send --to <worker-id> --type answer --subject "<the decision>" --body "<why, briefly>"
```

A `Failed:` report is not blocking: the worker gave up and stopped. Decide
whether to respawn with a better brief, take the work yourself, or ask the human.

### Keep human decisions from getting buried

Maintain a durable coordinator-side ledger of every question or decision that
still requires the human's answer. A newer status, answer, or completion message
does not supersede an older unresolved decision unless it actually resolves or
invalidates it.

Whenever the ledger is nonempty, end every user-facing status update and final
handoff with a numbered section in this form:

```text
Pending your decisions:
1. <decision and why it matters>
2. <decision and why it matters>
```

Repeat unresolved items across turns until the human explicitly answers them or
the underlying work makes them inapplicable. When the human chooses to discuss
an item directly with a worker, keep it on the list and label that direct
conversation as its owner until the worker reports the decision. Remove an item
only after recording its resolution and routing it to the affected workers.

Do not burden the human with routine coordination choices already settled by
their standing instructions. Apply those instructions yourself and reserve the
ledger for genuine product, scope, architecture, approval, or explicitly
human-gated operational decisions.

`send` also accepts the old terminal id of a killed resumable session. If that
conversation is already live under a new id, the message is routed there. If it
is still dead, Emacs resumes it in a new worker terminal, nests it under the
sending agent in the dashboard, and supplies the message as the first prompt;
the success response names the new terminal id. A terminal that never started
an agent has no durable conversation and cannot be revived.

The `--to` address may also be the agent's durable session id or a complete
`agent-session:AGENT::SESSION-ID::WORKTREE` link copied from Org. Prefer one of
those when it is available: `emacs-agent` resolves a running conversation to its
current terminal and can revive a stopped one without the caller translating
the durable identity into an ephemeral terminal id.

When everything is in, do the part only you can do: reconcile the reports, say
what actually landed and where, and name what is still open. A worker's final
report is its own account of its work — if the result matters, verify it rather
than repeating the claim.

## Mailboxes — addressing a role, not a conversation

A role agent (a reviewer, a planner, …) is restarted with fresh conversations
from time to time, so its session id is no address for it. A **mailbox** is a
directory standing for the role — typically holding its `context.md` and notes
— and mail addressed to the directory waits there for whichever conversation
currently plays the role.

```bash
emacs-agent send --to ~/agents/reviewer --subject "Review PR 123" --body "..."
emacs-agent wait --mailbox ~/agents/reviewer --timeout 600   # as the role
emacs-agent send --to <address> --from ~/agents/reviewer --subject "..."
```

- **Listening is holding the lock.** `wait --mailbox DIR` and `check --mailbox
  DIR` first take DIR's `emacs-lock` (nothing happens if you already hold it),
  and are refused while another conversation holds it. One agent plays a role
  at a time. A holder that died keeps the lock until the human releases it from
  the dashboard; until then a new instance is refused and should say so and
  wait for the human.
- **Delivery is pull-only.** Mailbox mail is never pushed into your terminal.
  Keep a background watcher running `emacs-agent wait --mailbox DIR` (in Claude
  Code, a background Bash command wakes you when it exits), handle what it
  prints, and start it again.
- **Speak as the role.** `send --from DIR` sends as the mailbox (taking its
  lock the same way), so replies are filed in the mailbox rather than with this
  conversation, which may be gone by then. The `Reply:` line of mail you read
  from a mailbox already includes `--from DIR`. `send-and-wait` does not take
  `--from`: your watcher collects the reply.
- **Spawn as the role.** `spawn --from DIR` makes the worker report to the
  mailbox instead of to your terminal, so its final report and questions reach
  whichever conversation plays the role when they arrive, even if you were
  restarted meanwhile. Without it, a worker reporting to a dead terminal
  revives that old conversation instead. The worker still appears nested under
  your terminal in the dashboard, and `list` still shows it only there.
- **No listener.** Mail to a mailbox nobody is listening on queues. The
  dashboard lists such mailboxes at the top, and the human starts an agent
  there (`RET`), which is launched in DIR with the prompt `You are <name>.
  Read <agents dir>/PLAYBOOK.md and then DIR/context.md`, where `<name>` is
  DIR's own name.
- **History.** Emacs writes every message into the mailbox as well: incoming to
  `DIR/mail/in/<timestamp>.md`, sent as the mailbox to `DIR/mail/out/`. Read
  those for the role's past; the mail itself still arrives through `wait`.
- **Addressing.** `--to`, `--from` and `--mailbox` take any existing directory,
  resolved from your current directory; a path that does not exist is refused.

## What the human sees, and what they can do

- Every worker is a row in the dashboard, titled with the `--title` you gave it
  (or the first line of the task). The dashboard groups rows by repo and
  worktree, so a worker in *your* worktree is indented under your row, while one
  in another worktree appears under that worktree with `↳ from <you>`.
- `✉N` on a row means that agent has N unread messages waiting.
- The human can jump into any worker's terminal (`RET`) and talk to it directly.
  So a worker may have been redirected by them without telling you — if a report
  contradicts your brief, believe the report.
- Killing a worker's terminal is the human's call, not yours. There is no kill
  verb: when you are finished with a worker, leave it idle and say so.

## Limits worth knowing

- Mail is stored, so a crash does not lose a report you had not read yet. It
  comes back attached to the *conversation*, not the terminal: when the human
  restores a session (`R` in the dashboard), its unread mail is waiting. Mail to
  a terminal that never started an agent is the exception — there is nothing to
  restore it to.
- Terminal ids (`11588-42`) are live routing addresses. Agent session ids and
  `agent-session:` links are durable addresses. Any form routes to the current
  terminal when one exists; a recovered conversation is attached as the
  sending agent's child.
- A worker that never fires an agent hook (a bare shell, a crashed launch) still
  appears in the dashboard, but with no agent status. If a spawn produces a row
  that never leaves `idle`, look at its terminal — the launch command failed.
- **A directory the agent has never run in is gated behind a trust prompt**
  ("Do you trust the contents of this directory?"), and a worker stopped there
  never reads your brief. `spawn` registers the directory as trusted before
  launching, so this normally just works — when it does something, it tells you
  (`Note: pre-trusted … for codex`). Two cases still bite: the human turned that
  off (`agent-sessions-orchestration-trust-new-worktrees`), or the type is one they
  added themselves, where Emacs does not know where trust lives. If a fresh
  worker goes quiet immediately and `spawn` printed no such note, assume it is
  sitting at a prompt: say so in your reply rather than waiting for a report
  that cannot come.
- Nothing here reaches outside this machine's Emacs. Slack, GitHub and Linear are
  not part of the protocol; use your own tools for those, and only for the human.
