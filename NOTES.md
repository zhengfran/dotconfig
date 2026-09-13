# Workflow context

- Orchestrator implementation is tracked in the Obsidian vault under `06-Spaces/03-Projects/Orchestrator/Fully Functional Orchestrator`. Child tickets own scope, dependencies and acceptance evidence.
- The runtime and pi clients live in separate submodules at `tools/ai/orchestrator` and `tools/ai/pi`. Runtime changes are committed there, then pinned in dotconfig.
- The user wants continuous ticket implementation supervised by Codex, using Herdr to run a fresh Codex conversation for every ticket. As requested on 2026-09-13, implementation uses exactly `gpt-5.6-terra`, high reasoning effort, and YOLO mode.
- The latest Herdr preference is one new workspace per future ticket inside the existing `default` session, with a fresh native conversation and unique agent name. Use explicit `--session default` and returned workspace/pane IDs; preserve unrelated workspaces. Continue the already-running ticket in its recorded session rather than moving or restarting it.
- The user refined acceptance on 2026-09-13: advance once a ticket is mostly implemented and functionally correct. Use one focused review and normally one consolidated correction pass, then targeted verification. Only material core failures block; track lesser hardening and polish as follow-ups. Do not repeat full reviews until every edge case is eliminated. The supervisor verifies the core and relevant tests independently, updates the vault, then starts the next ticket.
- The current workstation is macOS. The accepted operational deployment target remains Linux; native conformance, managed deployment and activation evidence cannot be inferred from local unit tests.
- Routine progress stays quiet. Ticket completion, material failures, required user decisions and completion of the entire queue warrant a concise update.

The execution specification is [Orchestrator implementation loop](workflows/orchestrator-implementation-loop.md).
