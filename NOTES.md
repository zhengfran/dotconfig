# Workflow context

- Orchestrator implementation is tracked in the Obsidian vault under `06-Spaces/03-Projects/Orchestrator/Fully Functional Orchestrator`. Child tickets own scope, dependencies and acceptance evidence.
- The runtime and pi clients live in separate submodules at `tools/ai/orchestrator` and `tools/ai/pi`. Runtime changes are committed there, then pinned in dotconfig.
- The user wants continuous ticket implementation supervised by Codex, using Herdr to run a fresh Codex conversation for every ticket. As requested on 2026-09-13, implementation uses exactly `gpt-5.6-terra`, high reasoning effort, and YOLO mode.
- The supervisor reviews each delivery, requests corrections until no actionable problems remain, verifies it independently, and updates the vault before starting another ticket. A coding agent's completion message alone does not resolve a ticket.
- The current workstation is macOS. The accepted operational deployment target remains Linux; native conformance, managed deployment and activation evidence cannot be inferred from local unit tests.
- Routine progress stays quiet. Ticket completion, material failures, required user decisions and completion of the entire queue warrant a concise update.

The execution specification is [Orchestrator implementation loop](workflows/orchestrator-implementation-loop.md).
