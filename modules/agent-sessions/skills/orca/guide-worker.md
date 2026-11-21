# Working for another agent — worker guide

You are (or may be) a **worker**: another agent started you in this terminal with
a task, and it is waiting to hear from you. Check your environment:

```bash
echo "$EMACS_AGENT_ROLE"          # "worker" when you were spawned as one
echo "$EMACS_AGENT_COORDINATOR"   # the terminal id that dispatched you
cat "$EMACS_AGENT_TASK_BRIEF"     # the brief you were launched with
```

If `EMACS_AGENT_ROLE` is empty you are nobody's worker — you are working directly
for the human. Do not send lifecycle mail in that case; `emacs-agent guide --main`
is the guide for coordinating others.

## Your obligations, in full

```bash
# REQUIRED when you finish — including when you fail or give up. Exactly once.
# Subject starts with "Done:" or "Failed:"; body is three sentences: what you
# did, what you found, what is left, naming any file worth reading.
emacs-agent send --to "$EMACS_AGENT_COORDINATOR" --subject "Done: <short>" \
  --body "<three sentences>"

# Blocked on a decision only the coordinator can make. Sends a question, then
# blocks until mail arrives and prints it.
emacs-agent send-and-wait --to "$EMACS_AGENT_COORDINATOR" --subject "<question>" \
  --body "<context>" [--timeout 600]

# Optional progress note, when something is worth knowing mid-task.
emacs-agent send --to "$EMACS_AGENT_COORDINATOR" --subject "<short>" --body "<detail>"
```

Mail has three types: `status` (the default, and what your final report is),
`question` (you wait on a reply; `send-and-wait`'s default) and `answer`.

If the coordinator's terminal was killed, `send` and `send-and-wait` revive its
saved conversation as a child of the sender and deliver the message there. The
command output identifies the replacement terminal id.

Rules:

- **Never** ask a question through an interactive prompt (no AskUserQuestion, no
  reading stdin). Your coordinator is an agent and cannot see a TUI prompt; your
  turn would hang until a human noticed. `emacs-agent send-and-wait` is how you
  ask.
- Failure still gets a final report, with a subject that says so. Never exit
  silently.
- After your final report, end your turn and sit at your prompt. Do not poll for
  more work, do not start something unrelated. More work arrives as new input;
  reply with the `Reply:` command included in that mail rather than a second
  final report.
- You may be sharing this worktree with other workers, so touch only the files
  your task names, and leave git state alone (`add`, `commit`, `checkout`,
  `rebase`, `stash`) unless your task says the worktree is yours. The index and
  HEAD are shared per worktree: staging anything would stage a co-tenant's
  half-finished edits along with yours.
- To stop a `wait` you started in the background, stop that task by its id or
  kill the exact pid you started. Never `pkill -f "emacs-agent wait"` or kill
  by any other pattern: it matches every agent's mailbox watcher on the machine.
- Your final report's body is read first and often only. Lead with what changed
  and whether it works; put long detail in a file and name its path.
- `emacs-share file FILE "why"` hands a file to the *human* in their dashboard.
  That is not a substitute for your final report to your coordinator.

The human can also talk to you directly in this terminal. If they redirect you,
do what they say — and say so in your final report, because your coordinator's
plan is now out of date.
