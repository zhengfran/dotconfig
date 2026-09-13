# Orchestrator implementation loop

Status: active. Authorized by the user's 2026-09-13 request to continue the remaining tickets with fresh Codex 5.6 Terra agents, high effort, YOLO mode, and review before advancing.

Acceptance policy updated on 2026-09-13: the user wants forward progress once a ticket is mostly implemented and functionally correct. This supersedes the earlier requirement to keep reviewing until no actionable findings remain.

Herdr topology updated on 2026-09-13: for future tickets, create a new workspace in the existing `default` session, with a fresh native Codex conversation. Do not create a separate Herdr session per ticket. The already-running workspace-lifecycle ticket stays in its recorded session through completion.

## Trigger and destination

Begin after the preceding implementation has passed independent review. The scoped runtime API ticket already qualifies at runtime commit `bbdffa5fe72a55414a346b91eaf077ec88133223`, pinned by dotconfig `128e90b9f6048a23fb069e876dd59af5b639e7d6`, with 437/437 passing tests and clear Standards and Spec reviews.

Repeat until the active **Fully Functional Orchestrator** map is genuinely complete, the user pauses the loop, or every remaining executable ticket requires an external change or user decision. A five-minute Codex thread heartbeat supervises progress and resumes this workflow. It must not launch overlapping implementers.

The canonical map is `~/obsidian/06-Spaces/03-Projects/Orchestrator/Fully Functional Orchestrator/map.md`. Ticket details and accepted architecture stay there; do not duplicate them into this workflow. The user explicitly authorizes this supervisor to continue across tickets. Each child implementation session still owns exactly one ticket.

## Agent contract

- Use Herdr and native Codex CLI with exactly `--model gpt-5.6-terra -c model_reasoning_effort=high --dangerously-bypass-approvals-and-sandbox --no-alt-screen`.
- For each new ticket, use explicit `herdr --session default` and create a new uniquely named workspace with `--no-focus`, a unique agent name, and a new native Codex conversation. Use the workspace/tab/pane IDs returned by creation; never assume `w1:p1` or target an existing user's pane. Do not create another Herdr server/session per ticket, or fork, resume, or reuse the preceding ticket's conversation. Record and verify model, effort, mode, native session ID, Herdr session, workspace/tab/pane IDs, agent name, branch and baseline.
- Corrective turns belong to that ticket's existing session. A reconnect or rate-limit recovery may resume only that same ticket's recorded conversation; never use it for another ticket.
- YOLO is the user's requested CLI execution mode. It does not expand ticket scope or authorize unrelated configuration changes, human communications, purchases, pushes, or production deployment.
- The supervisor owns claims, independent reviews, acceptance, parent submodule pins and vault updates. The implementer edits only its assigned repository/ticket scope, reads the canonical contracts, uses meaningful regression tests, and reports actual commits and validation. It must not resolve its own ticket, launch other ticket implementers or silently substitute models.

## One iteration

1. Read the current map and child frontmatter, the saved loop state, and any new user steering. Search Nowledge once for cross-tool continuation context; if unavailable, proceed from repository/vault evidence and record that limitation.
2. Reconcile the recorded active session before doing anything else. An uncertain or timed-out prompt is not permission to launch a duplicate. Use its explicit recorded Herdr session and agent/pane IDs for continuation. For future tickets, inspect `default` and create only a new dedicated workspace there; do not manipulate unrelated workspaces or target implicit focused panes/agents. Existing ticket sessions remain valid through completion.
3. If no ticket is active, select the first open, unclaimed ticket by filename whose dependencies are all resolved. Skip only a previously documented external blocker that still applies. Preserve concurrent user changes. Pin the accepted starting commit and create an isolated branch from it; use a separate worktree if another checkout owner would otherwise conflict.
4. Atomically claim the child note with a named Terra assignee before dispatch. Regenerate the minimap from child state. Save a temporary, self-contained handoff with the ticket path, accepted contracts, repository instructions, baseline, permitted scope, tests and delivery requirements.
5. Start the fresh agent and verify the requested settings and actual working state. Persist its identifiers immediately. Observe rather than edit its implementation concurrently. Continue useful independent verification preparation while it works.
6. When it reports completion, freeze the actual delivery. Perform one focused review of core ticket behavior, relevant integration and the changed code. Standards and Spec may run in parallel under the code-review skill, but together they are one review pass. Give reviewers the acceptance policy below: identify material blockers and separate non-blocking follow-ups; do not search for ever more edge cases after the core works.
7. Consolidate material blockers into one correction request to the same agent. Normally allow one correction pass, then check the requested fixes and their direct regressions only. Do not restart a full independent review or broaden the search after each correction. Further correction is justified only by a demonstrated failure that still breaks core functionality or presents material data-loss/authorization risk in intended use; explain that concrete impact. A severity label alone does not justify another round. Defer remaining hardening and polish as described below.
8. Independently verify representative core behavior and run the relevant existing tests. Run the full suite once on the final candidate when appropriate; rerun migrations when schema changes warrant it. After a correction, repeat only checks affected by it, plus any required final suite. Do not rerun unchanged expensive checks or build new adversarial fixture families without a material blocker. Passing author tests alone is insufficient, but zero review findings is not required. Preserve the runtime's two-rework domain rule; it is separate from this supervisory process.
9. Once the ticket is mostly implemented, its core behavior is functionally correct and relevant validation passes, accept it. Record the accepted commit, tests, migration/compatibility effects, known follow-ups and live-evidence limits in `## Answer`. Commit approved repository changes locally and update the parent submodule pin. Set only this ticket resolved; append its outcome pointer to the map, update the audited baseline and regenerate its minimap/frontier. Verify links, counts and working-tree state.
10. Mark the native conversation completed in loop history and immediately choose the next eligible ticket with a new workspace in Herdr `default` and a fresh native conversation. Never reuse the completed conversation.

## Durable state and recovery

Store local execution state outside the repository at `~/.local/state/codex-orchestrator-ticket-loop/state.json`. Include the workflow path, heartbeat ID, phase, current ticket, baseline, branch, all session identifiers, last observed status, evidence paths, completed tickets, and external blockers. Update atomically. Keep handoffs and review artifacts beside it or in an explicitly recorded temporary directory; acceptance evidence belongs in the vault.

Use one supervisor at a time. Before claiming or launching, check for a live owner and recorded session. A heartbeat must continue the existing iteration rather than rediscovering and relaunching it. If state is missing, recover from the map's named claim and explicitly named Herdr session inventory; do not guess that nothing is running.

An interrupted child, failed transport or ambiguous native status does not prove completion. Inspect its actual process/session and repository before deciding to resume or report a blocker. A normal running agent is not blocked. Stop polling unchanged state after a bounded observation and let the heartbeat check again.

## Checkpoints and limits

Routine implementation, tests, reviews, local commits and requested vault updates proceed autonomously. The accepted architecture is already decided; do not reopen settled choices for routine implementation.

### Functional acceptance and review budget

- Accept when the ticket delivers its main user-visible behavior and the next dependent work can use it reliably. Broad implementation coverage and correct core behavior matter more than exhaustive edge-case closure.
- Block acceptance for missing core capability, a reproducible failure of a representative intended workflow, a material regression, or credible data corruption or authorization bypass in intended use. Assess impact and likelihood, not just a reviewer's priority label.
- Rare input combinations, adversarial scheduling variations, speculative robustness, code smells, cosmetic issues and optional refinements normally become non-blocking follow-ups. They must not repeatedly reopen the current ticket when normal operation is correct.
- Record deferred findings concisely in the ticket's Answer, with an evidence pointer and the existing downstream ticket that owns them when applicable. Do not create a new blocking dependency or expand the current acceptance criteria merely to carry a follow-up. Broader hardening belongs in the existing acceptance, migration or backend-conformance work as appropriate.
- Preserve explicit operational gates for the final deployment and support claims. Accepting a code ticket does not fabricate Linux, native backend or live-service evidence.
- The already-running correction for “Unify execution admission, launch and dispatch” may finish. Then perform a targeted final check and advance if core functionality is sound. Its earlier nine reviews already satisfy the independent-review requirement; do not launch another full Standards/Spec review cycle for it.

If the child is rate-limited, preserve its session and wait for the reported reset; do not switch models or open another session to evade limits. If that prevents useful progress, report the actual blocker once.

For a genuine missing decision or operational target, prepare the concrete commands, configuration, evidence and alternatives first. Then present one concise brief explaining the decision, recommended action and artifact link. Do not fabricate Linux, live backend, managed service or activation evidence. Existing requirements for an explicitly authorized concrete operational namespace remain in force.

If one ticket cannot proceed because of a documented external prerequisite, retain its evidence and blocker without resolving it. Continue other eligible independent tickets. When none remain executable, notify the user once and pause the heartbeat until the prerequisite or instruction changes. A failed model launch, usage limit or permission denial must be reported accurately, not worked around by changing the requested model or broadening scope.

## Notifications and completion

Stay quiet during unchanged waiting and ordinary progress. Notify on an accepted ticket, material failure, a required user decision or completion of the entire map. Each brief states the concrete outcome, validation, relevant commit and vault link.

When every ticket and the map's operational destination are verified, record the final evidence, stop launching agents and pause the heartbeat. Never mark the map complete while a required backend/host/deployment gate remains unmet.
