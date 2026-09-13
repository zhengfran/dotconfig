# Orchestrator implementation loop

Status: active. Authorized by the user's 2026-09-13 request to continue the remaining tickets with fresh Codex 5.6 Terra agents, high effort, YOLO mode, and review before advancing.

## Trigger and destination

Begin after the preceding implementation has passed independent review. The scoped runtime API ticket already qualifies at runtime commit `bbdffa5fe72a55414a346b91eaf077ec88133223`, pinned by dotconfig `128e90b9f6048a23fb069e876dd59af5b639e7d6`, with 437/437 passing tests and clear Standards and Spec reviews.

Repeat until the active **Fully Functional Orchestrator** map is genuinely complete, the user pauses the loop, or every remaining executable ticket requires an external change or user decision. A five-minute Codex thread heartbeat supervises progress and resumes this workflow. It must not launch overlapping implementers.

The canonical map is `~/obsidian/06-Spaces/03-Projects/Orchestrator/Fully Functional Orchestrator/map.md`. Ticket details and accepted architecture stay there; do not duplicate them into this workflow. The user explicitly authorizes this supervisor to continue across tickets. Each child implementation session still owns exactly one ticket.

## Agent contract

- Use Herdr and native Codex CLI with exactly `--model gpt-5.6-terra -c model_reasoning_effort=high --dangerously-bypass-approvals-and-sandbox --no-alt-screen`.
- For each new ticket, create a new uniquely named Herdr session and a new native Codex conversation. Do not fork, resume, or reuse the preceding ticket's conversation. Record and verify model, effort, mode, native session ID, Herdr session, agent name, pane, branch and baseline.
- Corrective turns belong to that ticket's existing session. A reconnect or rate-limit recovery may resume only that same ticket's recorded conversation; never use it for another ticket.
- YOLO is the user's requested CLI execution mode. It does not expand ticket scope or authorize unrelated configuration changes, human communications, purchases, pushes, or production deployment.
- The supervisor owns claims, independent reviews, acceptance, parent submodule pins and vault updates. The implementer edits only its assigned repository/ticket scope, reads the canonical contracts, uses meaningful regression tests, and reports actual commits and validation. It must not resolve its own ticket, launch other ticket implementers or silently substitute models.

## One iteration

1. Read the current map and child frontmatter, the saved loop state, and any new user steering. Search Nowledge once for cross-tool continuation context; if unavailable, proceed from repository/vault evidence and record that limitation.
2. Reconcile the recorded active session before doing anything else. An uncertain or timed-out prompt is not permission to launch a duplicate. Use only explicit named Herdr sessions and recorded agent/pane IDs; never target the user's focused session.
3. If no ticket is active, select the first open, unclaimed ticket by filename whose dependencies are all resolved. Skip only a previously documented external blocker that still applies. Preserve concurrent user changes. Pin the accepted starting commit and create an isolated branch from it; use a separate worktree if another checkout owner would otherwise conflict.
4. Atomically claim the child note with a named Terra assignee before dispatch. Regenerate the minimap from child state. Save a temporary, self-contained handoff with the ticket path, accepted contracts, repository instructions, baseline, permitted scope, tests and delivery requirements.
5. Start the fresh agent and verify the requested settings and actual working state. Persist its identifiers immediately. Observe rather than edit its implementation concurrently. Continue useful independent verification preparation while it works.
6. When it reports completion, freeze the actual commit and include any remaining tracked/untracked changes in the review snapshot. Apply the code-review skill with separate Standards and Spec reviewers against the recorded baseline. Check all ticket acceptance requirements; use independent probes where a plausible failure is not covered by the author's tests.
7. Send actionable findings to the same ticket agent and repeat review after its corrections. Never advance merely because tests pass or the child says it is done. If the child is rate-limited, preserve its session and wait for the reported reset; do not switch models or open another session to evade limits. If that prevents useful progress, report the actual blocker once.
8. Once both review axes have no actionable findings, run the relevant independent validation and required acceptance evidence. Capture source hashes during long test runs so results correspond to the reviewed revision. Preserve migrations, the two-rework limit in the runtime's domain model, and every previously accepted regression. Do not equate this supervisory review cycle with the runtime's two-rework accounting.
9. Record the accepted commit, test results, migration/compatibility effects and live-evidence limits in the ticket's `## Answer`. Commit approved repository changes locally and update the parent submodule pin. Set only this ticket resolved; append its outcome pointer to the map, update the audited baseline and regenerate its minimap/frontier. Verify links, counts and working-tree state.
10. Mark the session completed in loop history and immediately choose the next eligible ticket with a fresh session. Never reuse the completed conversation.

## Durable state and recovery

Store local execution state outside the repository at `~/.local/state/codex-orchestrator-ticket-loop/state.json`. Include the workflow path, heartbeat ID, phase, current ticket, baseline, branch, all session identifiers, last observed status, evidence paths, completed tickets, and external blockers. Update atomically. Keep handoffs and review artifacts beside it or in an explicitly recorded temporary directory; acceptance evidence belongs in the vault.

Use one supervisor at a time. Before claiming or launching, check for a live owner and recorded session. A heartbeat must continue the existing iteration rather than rediscovering and relaunching it. If state is missing, recover from the map's named claim and explicitly named Herdr session inventory; do not guess that nothing is running.

An interrupted child, failed transport or ambiguous native status does not prove completion. Inspect its actual process/session and repository before deciding to resume or report a blocker. A normal running agent is not blocked. Stop polling unchanged state after a bounded observation and let the heartbeat check again.

## Checkpoints and limits

Routine implementation, tests, reviews, local commits and requested vault updates proceed autonomously. The accepted architecture is already decided; do not reopen settled choices for routine implementation.

For a genuine missing decision or operational target, prepare the concrete commands, configuration, evidence and alternatives first. Then present one concise brief explaining the decision, recommended action and artifact link. Do not fabricate Linux, live backend, managed service or activation evidence. Existing requirements for an explicitly authorized concrete operational namespace remain in force.

If one ticket cannot proceed because of a documented external prerequisite, retain its evidence and blocker without resolving it. Continue other eligible independent tickets. When none remain executable, notify the user once and pause the heartbeat until the prerequisite or instruction changes. A failed model launch, usage limit or permission denial must be reported accurately, not worked around by changing the requested model or broadening scope.

## Notifications and completion

Stay quiet during unchanged waiting and ordinary progress. Notify on an accepted ticket, material failure, a required user decision or completion of the entire map. Each brief states the concrete outcome, validation, relevant commit and vault link.

When every ticket and the map's operational destination are verified, record the final evidence, stop launching agents and pause the heartbeat. Never mark the map complete while a required backend/host/deployment gate remains unmet.
