/**
 * Forwards Pi session events to the Emacs agent-sessions dashboard.
 *
 * Installed into ~/.pi/agent/extensions/ by `agent-sessions-install'.
 * Gated on EMACS_AGENT_SESSION_ID, which modules/agent-sessions/
 * agent-sessions.el injects into every terminal buffer's shell env, so a
 * session only reports when it runs inside an Emacs-managed terminal.
 *
 * The payload goes to a temp file and reaches `agent-sessions-hook-notify' over
 * emacsclient — the same channel the Claude/Codex shell forwarders use —
 * with pi's event names translated into the Claude vocabulary the
 * dashboard's status table keys on (SessionStart / UserPromptSubmit /
 * Stop / SessionEnd). The temp file is cleaned up here when emacsclient
 * fails; when it succeeds, the Elisp side deletes it after reading.
 */
import { spawnSync } from "node:child_process";
import { mkdtempSync, writeFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

export default function (pi) {
	const terminalId = process.env.EMACS_AGENT_SESSION_ID;
	if (!terminalId) return;

	const notify = (eventName, ctx, userPrompt) => {
		let tempDir;
		try {
			// mkdtempSync for a race-free unique name; plain mktemp does not
			// exist in this runtime, and a predictable name two pi processes
			// could collide on would be worse than a directory each.
			tempDir = mkdtempSync(join(tmpdir(), "emacs-agent-hook."));
			const payloadFile = join(tempDir, "payload.json");
			const payload = {
				hook_event_name: eventName,
				session_id: ctx.sessionManager.getSessionId(),
				cwd: ctx.cwd,
				transcript_path: ctx.sessionManager.getSessionFile(),
			};
			if (userPrompt !== undefined) {
				payload.user_prompt = userPrompt;
			}
			writeFileSync(payloadFile, JSON.stringify(payload));
			const result = spawnSync(
				"emacsclient",
				[
					"--no-wait",
					"-e",
					`(agent-sessions-hook-notify "${terminalId}" "pi" "${payloadFile}")`,
				],
				{ stdio: "ignore" },
			);
			if (result.error || result.status !== 0) {
				throw new Error("emacsclient failed");
			}
		} catch {
			if (tempDir) {
				try {
					rmSync(tempDir, { recursive: true, force: true });
				} catch {
					// Nothing to do; a stray temp file is harmless.
				}
			}
		}
	};

	pi.on("session_start", (_event, ctx) => notify("SessionStart", ctx));
	pi.on("before_agent_start", (event, ctx) =>
		notify("UserPromptSubmit", ctx, event.prompt),
	);
	pi.on("agent_end", (event, ctx) => {
		// An errored turn still ends the run, so without this check it would
		// surface as the ordinary waiting-for-you state rather than the
		// dashboard's distinct error bucket (Claude's StopFailure).
		const last = event.messages && event.messages[event.messages.length - 1];
		notify(last && last.stopReason === "error" ? "StopFailure" : "Stop", ctx);
	});
	pi.on("session_shutdown", (_event, ctx) => notify("SessionEnd", ctx));
}
