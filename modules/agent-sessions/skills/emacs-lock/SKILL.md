---
name: emacs-lock
description: >-
  Take turns with other agents on a resource only one of them may use at a
  time — a shared local stack, a database, a checkout that is not yours — with
  the `emacs-lock` command. Use before touching any resource the user's
  instructions say is shared or guarded by a lock (for example a local dev
  stack several agents test against), when another agent's work could collide with
  yours on something outside your own worktree, or when mail tells you a lock
  of yours was released.
---

# emacs-lock — taking turns on a shared resource

`emacs-lock` coordinates agents through a lock table that every agent on this
machine shares. It is on PATH in Emacs agent terminals; anywhere else run
`~/.emacs.d/modules/agent-sessions/bin/emacs-lock`.

```bash
emacs-lock list [--verbose]                       # what is held (--verbose: by whom, why, who waits)
emacs-lock acquire NAME --why "what you are doing"  # one attempt; exit 1 if taken
emacs-lock wait NAME --why "what you are doing"     # blocks until you hold it
emacs-lock release NAME
```

You are identified by your agent session id, so the lock is yours across
commands and even across a restart of your session. Exit status: 0 done,
1 refused, 2 usage, 3 cannot run (read the message; do not work around it).

## Naming the lock

- **A directory names itself.** Pass a path (anything containing `/` or
  starting with `~` or `.`), e.g. `emacs-lock wait ~/src/app`. Directory
  locks conflict when one contains the other.
- **Use the name your instructions give.** When the user's instructions or the
  resource's docs name the lock, use exactly that.
- **Otherwise reuse before you invent.** Try your name without `--new`: an
  unknown name is refused with the names used recently. Use one of those if it
  means the same resource. Only then create one:
  `emacs-lock acquire NAME --new "what this resource is"`. A new name is also
  refused if names were added since you were last shown them; read the ones it
  prints and retry only if none of them fits.

## Holding it

- **Hold it for the whole cycle**, not just one command: everything from
  changing the resource until you have read the results you need from it.
- **Wait in the background.** If the lock is taken, run `emacs-lock wait NAME`
  as a background command when your harness can, and carry on with work that
  does not need the resource. Otherwise wait in the foreground with
  `--timeout SECONDS` and repeat. You keep your place in the queue while you
  wait; a plain `acquire` never jumps the queue.
- **Do not touch the resource without holding the lock**, not even to look at
  something that changes it.
- **Always release**, including when tests fail, you hit an error, or you
  abandon the work: `emacs-lock release NAME`.

## When something is off

- **Never release a lock someone else holds**, and do not delete rows or edit
  the database. The command refuses, and that is deliberate.
- **A holder shown as `not running`** still holds the lock: its session may be
  restored and continue. Tell the user who holds it and since when, and let
  them decide; they can release it from the dashboard.
- **If mail says the user released your lock**, stop using the resource at once.
  Someone else may have it now. Run `emacs-lock wait NAME` before using it
  again, and mention it to the user.
