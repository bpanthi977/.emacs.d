# Provenance — where these facts came from

Orca ships no public docs for this feature; everything here was read out of the
installed app or run from it. Version **1.4.146**
(`app.asar:package.json`, and `CFBundleShortVersionString` in `Info.plist`),
read on **2026-08-25**.

## What is directly readable without unpacking

Most of the interesting code is already on disk unpacked, because Electron
leaves `node`-executed files outside the archive:

```
/Applications/Orca.app/Contents/Resources/app.asar.unpacked/out/
  cli/specs/orchestration.js        # command list, usage strings, flag allowlists, notes
  cli/handlers/orchestration.js     # CLI-side semantics: handle resolution, keepalive, payload flags
  cli/specs/core.js                 # worktree create / terminal create|send|wait specs + notes
  cli/bundled-skill-guides.js       # the agent-facing markdown guides, as JS string literals
  cli/runtime/{transport,metadata}.js   # socket transport + orca-runtime.json
  shared/runtime-bootstrap.js       # orca-runtime.json name, transport selection
  shared/claude-subagent-roster.js  # Claude Agent Teams / Task subagent tracking (separate feature)
/Applications/Orca.app/Contents/Resources/bin/orca   # the CLI shim (bash → bundled node)
```

The CLI runs offline for anything local:

```bash
cd /Applications/Orca.app/Contents/Resources/app.asar.unpacked
node out/cli/index.js orchestration --help          # verified command list
node out/cli/index.js skills list
node out/cli/index.js skills get orchestration      # the full agent-facing guide
```

`orchestration-skill-guide.md` in this directory is a verbatim copy of that last
command's output — diff it after an Orca update to see what the contract changed.

## What needs the archive unpacked

The main process (schema, coordinator, preamble builder, delivery) is inside
`app.asar` as `out/main/index.js` — 7.5MB, bundled but **with comments intact**,
which is where most of the "why" quoted in [internals.md](internals.md) comes
from. `npx @electron/asar` works if you have network; otherwise the header is
plain JSON and a dozen lines of node will do it:

```js
// header: [0..3]=4, [4..7]=payload size, [8..11], [12..15]=json length, json at 16
const fs = require('fs');
const fd = fs.openSync('/Applications/Orca.app/Contents/Resources/app.asar', 'r');
const head = Buffer.alloc(16); fs.readSync(fd, head, 0, 16, 0);
const size = head.readUInt32LE(4), jsonLen = head.readUInt32LE(12);
const buf = Buffer.alloc(jsonLen); fs.readSync(fd, buf, 0, jsonLen, 16);
const header = JSON.parse(buf.toString());   // {files: {...}} with per-file {size, offset}
const base = 8 + size;                       // file contents start here
```

Entries marked `"unpacked": true` are the ones already on disk under
`app.asar.unpacked/`; everything else is at `base + offset`.

Identifiers to grep for in `out/main/index.js`, rather than line numbers (they
move every release):

| Topic | Grep for |
|---|---|
| SQLite schema + migrations | `class OrchestrationDb`, `SCHEMA_VERSION` |
| DAG promotion, retries | `promoteReadyTasks`, `failDispatch`, `createDispatchContext` |
| RPC surface | `ORCHESTRATION_METHODS`, `ORCHESTRATION_GATE_METHODS` |
| Worker preamble | `buildDispatchPreamble`, `buildPostWorkerDoneInstructions`, `buildDriftSection` |
| Completion authority | `hasLifecycleAuthority`, `reconcileWorkerDoneMessage`, `isSamePane` |
| Group addresses | `resolveGroupAddress`, `AGENT_NAME_GROUPS` |
| Push delivery | `deliverPendingMessages`, `notifyMessageArrived`, `formatMessagesForInjection` |
| Paste mechanics | `sendTerminalAgentPrompt`, `buildAgentPromptPasteBytes`, `AGENT_PROMPT_SUBMIT` |
| Coordinator loop | `class Coordinator`, `DISPATCH_STALE_THRESHOLD`, `HUNG_THRESHOLD_MS` |
| Status-line scraping | `ORCA_DISPATCH_STATUS_TASK_MARKER` |

Also worth knowing: `app.asar` carries three design notes under `notes/`
(`skill-freshness-design.md` is the current one) explaining why guide bodies live
in the binary and are fetched with `orca skills get` instead of being written
into `~/.claude/skills/`.

## Live state on this machine

- `~/Library/Application Support/orca/orchestration.db` — exists,
  `user_version = 6`, **0 rows** in `messages`, `tasks`, `dispatch_contexts`,
  `decision_gates`, `coordinator_runs`. The feature has never been used here (or
  was reset).
- No orca skill stubs under `~/.claude/skills/`, so nothing currently points an
  agent at `orca skills get orchestration` automatically.
- `orca` is not on `PATH`; only the in-bundle shim exists.
- `~/.orca/agent-hooks/` holds the multi-agent hook forwarders this machine was
  wired through; `~/Library/Application Support/orca/daemon/daemon-v2{1,2,3}.*`
  are the CLI's unix sockets and tokens from past runs.

## Confidence notes

- Command list, flags, guide text, DB schema version and row counts: **verified
  by execution**.
- Runtime behaviour (delivery timing, authority checks, coordinator constants):
  **read from source**, not exercised — the app was not running during this
  research.
- The guide's "must be enabled in Settings > Experimental" precondition could
  **not** be confirmed: no `experimentalOrchestration`-style flag exists in this
  build and the RPC methods are registered unconditionally.
