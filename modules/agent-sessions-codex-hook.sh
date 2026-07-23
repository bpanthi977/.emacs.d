#!/bin/sh
# Forwards Codex hooks.json events to bp/agent-hook-notify via emacsclient.
# Referenced from ~/.codex/hooks.json. Gated on EMACS_AGENT_SESSION_ID, which
# modules/agent-sessions.el injects into every vterm buffer's shell env.
payload=$(cat)
if [ -z "$payload" ]; then
  exit 0
fi
if [ -z "$EMACS_AGENT_SESSION_ID" ] || ! command -v emacsclient >/dev/null 2>&1; then
  exit 0
fi
emacs_hook_payload_file=$(mktemp "${TMPDIR:-/tmp}/emacs-agent-hook.XXXXXX")
printf '%s' "$payload" > "$emacs_hook_payload_file"
emacsclient --no-wait -e "(bp/agent-hook-notify \"${EMACS_AGENT_SESSION_ID}\" \"codex\" \"${emacs_hook_payload_file}\")" >/dev/null 2>&1 \
  || rm -f "$emacs_hook_payload_file"
exit 0
