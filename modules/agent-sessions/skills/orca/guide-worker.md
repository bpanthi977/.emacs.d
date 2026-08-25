# Working under a coordinator — worker guide

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
emacs-agent done --summary "<three sentences: what you did, what you found, what is left>" \
  [--files path/a,path/b] [--report path/to/notes.md]

# Blocked on a decision only the coordinator can make. Blocks until it answers.
emacs-agent ask --question "<question>" [--timeout 600]

# Optional progress note, when something is worth knowing mid-task.
emacs-agent send --type status --subject "<short>" --body "<detail>"
```

Rules:

- **Never** ask a question through an interactive prompt (no AskUserQuestion, no
  reading stdin). Your coordinator is an agent and cannot see a TUI prompt; your
  turn would hang until a human noticed. `emacs-agent ask` is how you ask.
- Failure is still a `done`, with a subject that says so. Never exit silently.
- After `done`, end your turn and sit at your prompt. Do not poll for more work,
  do not start something unrelated. More work arrives as new input.
- You may be sharing this worktree with other workers, so touch only the files
  your task names, and leave git state alone (`add`, `commit`, `checkout`,
  `rebase`, `stash`) unless your task says the worktree is yours. The index and
  HEAD are shared per worktree: staging anything would stage a co-tenant's
  half-finished edits along with yours.
- `--summary` is read first and often only. Lead with what changed and whether
  it works; put detail in a file and pass `--report` if it is long.
- `emacs-share-file FILE "why"` hands a file to the *human* in their dashboard.
  That is not a substitute for reporting `done` to your coordinator.

The human can also talk to you directly in this terminal. If they redirect you,
do what they say — and say so in your `done` summary, because your coordinator's
plan is now out of date.
