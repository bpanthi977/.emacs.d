Load the Emacs agent-orchestration guide and follow it.

Run `emacs-agent guide --main` when you are being asked to coordinate or message
other agents (spawn workers, send or answer agent mail, wait for reports, answer
questions) — including when this was invoked as `/orca --main`. For messages
between peer sessions with no worker relationship, follow its *Messaging a peer*
section.

Run `emacs-agent guide` when you are the worker: `$EMACS_AGENT_ROLE` is `worker`,
or you were launched with a task brief naming a coordinator.

Read the guide before running any other `emacs-agent` verb. `emacs-agent` exists
only inside a terminal Emacs manages; if it is missing, say so rather than
improvising another way to reach other agents.
