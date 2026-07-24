#!/bin/sh
# Forwards Claude Code hook events to bp/agent-hook-notify via emacsclient.
# Wired into ~/.claude/settings.json by `bp/agent-sessions-install'. Gated on
# EMACS_AGENT_SESSION_ID, which modules/agent-sessions/agent-sessions.el injects
# into every terminal buffer's shell env, so a session only reports when it runs
# inside an Emacs-managed terminal.
payload=$(cat)
if [ -z "$payload" ]; then
  exit 0
fi
if [ -z "$EMACS_AGENT_SESSION_ID" ] || ! command -v emacsclient >/dev/null 2>&1; then
  exit 0
fi
emacs_hook_payload_file=$(mktemp "${TMPDIR:-/tmp}/emacs-agent-hook.XXXXXX")
printf '%s' "$payload" > "$emacs_hook_payload_file"
emacsclient --no-wait -e "(bp/agent-hook-notify \"${EMACS_AGENT_SESSION_ID}\" \"claude\" \"${emacs_hook_payload_file}\")" >/dev/null 2>&1 \
  || rm -f "$emacs_hook_payload_file"
exit 0
