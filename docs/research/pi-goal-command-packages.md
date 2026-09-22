# Pi `/goal` package comparison

> Evidence checked: **2026-09-20**. This machine currently runs **Pi 0.86.0** and **Node 26.8.1**.

## Bottom line

Pi does not currently ship a first-party `/goal` command; this functionality comes from third-party packages. For this machine:

1. **My pick for the requested Claude/Codex-like behavior: `@pify/goal`** — Codex-shaped controls plus an optional independent, read-only completion auditor; it also has the smallest supply-chain and runtime surface. Its drawbacks are youth, lower adoption, and an audit that deliberately fails open when inconclusive.
2. **Best compatibility/maturity choice: `@narumitw/pi-goal`** — actively developed against Pi 0.86.0, session-scoped state, strong runaway-loop guards, token budgets, explicit completion/block/wait tools, and the largest current user signal among compatible candidates.
3. **Focused but less safe unattended choice: `pi-codex-goal`** — strong branch-aware session persistence and broad platform testing, but completion is self-audited, continuation uses an older `agent_end` polling pattern, and there is no hard automatic-turn/no-progress ceiling.
4. **Do not install `pi-goal-x` yet** — it has the richest planning/task/auditor UI, but version 0.31.6 explicitly supports Pi `>=0.83.0 <0.85.0`, excluding this machine's Pi 0.86.0.

## Reference behavior: what Claude Code and Codex provide

Claude Code's native `/goal` sets one session completion condition, starts another turn after each settled turn, and asks a separate small/fast model to return `not yet met`, `met`, or `impossible`. It restores active goals on resume, tracks turns/tokens/time, pauses or retries selected errors, and defers evaluation while background work is still running. Source: [Claude Code `/goal` documentation](https://code.claude.com/docs/en/goal.md).

Codex's native `/goal` exposes `/goal <objective>`, status via bare `/goal`, and `edit`, `pause`, `resume`, and `clear`; the objective remains attached to the active chat and is limited to 4,000 characters. Source: [Codex CLI slash commands](https://developers.openai.com/codex/cli/slash-commands.md#set-or-view-a-task-goal-with-goal).

These are useful comparison points because Pi packages differ in two important ways:

- whether completion is declared by the working agent or checked by a separate model;
- whether they understand outstanding background work before starting another turn.

Pi packages execute with full user permissions, so their source should be reviewed before installation. Source: [Pi package documentation](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/packages.md).

## Shortlist comparison

| Package | Best for | State and continuation | Completion decision | Safety limits | Pi 0.86 status |
|---|---|---|---|---|---|
| [`@narumitw/pi-goal`](https://github.com/narumiruna/pi-extensions/tree/main/packages/pi-goal) 0.54.8 | A dependable everyday goal loop | One goal per session; restores across reload/resume/forks/compaction; continues at `agent_settled` | Worker calls evidence-gated `goal_complete`; no second-model auditor | 25 automatic responses and 3 no-progress runs by default; token budget; blocker and wait tools | **Best-supported:** package dev dependencies are exactly Pi 0.86.0 |
| [`@pify/goal`](https://github.com/pifydev/goal) 0.7.0 | Claude-like independent completion review | One goal per session; hidden follow-ups at settled idle boundaries | Optional second agent audits with read-only tools; inconclusive audit is accepted | 20 automatic turns, 3 identical tool-free turns, token budget, wrap-up mode | Wildcard peer supports loading; developed against 0.85.1, not explicitly validated against 0.86 |
| [`pi-codex-goal`](https://github.com/fitchmultz/pi-codex-goal) 0.3.0 | Focused Codex-style UX and persistence | Session custom entries follow resume/fork/tree/reload/compaction; continuation uses `agent_end` plus polling | Worker calls `update_goal(status: complete)` after a prompted evidence audit | Optional token budget (minimum 500k); **no turn/no-progress ceiling** | README supports Pi >=0.84; Node >=24; compatible with this machine |
| [`@pinet/agent-goal`](https://github.com/gugu91/pinet/tree/main/agent-goal) 0.2.21 | A durable external evaluator and cross-session inventory | SQLite database plus per-session continuation claims/checkpoints | Separate evaluator runs after every settled goal turn | Optional turn/runtime limits; retries evaluator failures | Peer floor >=0.74; compatible, but heavier than needed |
| [`pi-goal-x`](https://github.com/tmonk/pi-goal-x) 0.31.6 | Rich guided plans, task trees, multiple goals, Sisyphus mode, configurable auditor | Multi-goal/project workflow with dashboards | Optional independent auditor | Run allowance, token budgets, recovery tooling | **Unsupported:** peers exclude Pi >=0.85 |
| [`pi-better-goal`](https://github.com/1aboveio/pi-better-harness/tree/main/packages/pi-better-goal) 0.4.0 | Companion `pi-better-*` background workflows | Goal loop can wait for registered background providers to drain | Prompted evidence audit, not an independent model | Progress-stall detection | Compatible (Pi >=0.84.4), but no integration with this machine's custom subagent manager was found |

## 1. `@narumitw/pi-goal` — strongest Pi 0.86 compatibility and maturity

Install:

```bash
pi install npm:@narumitw/pi-goal
```

Core commands:

```text
/goal
/goal status
/goal [--tokens 100k] <objective>
/goal edit [--tokens 100k] <objective>
/goal pause
/goal resume
/goal clear
```

Why it fits this machine:

- The latest package uses Pi 0.86.0 as its exact development dependency, matching the installed Pi version. Source: [package.json](https://raw.githubusercontent.com/narumiruna/pi-extensions/main/packages/pi-goal/package.json).
- State is stored in Pi session entries and follows reload, resume, compatible forks, and compaction.
- It waits for Pi's `agent_settled` event rather than blindly continuing at `turn_end`, avoiding races with retries, queued work, and compaction.
- The model must use explicit `goal_complete`, `goal_blocked`, or `goal_wait` tools. Calls carry a goal ID, which rejects stale continuations after pause/replacement.
- Defaults cap autonomous work at 25 automatic responses and pause after three repeated tool-free/no-progress runs. Optional token budgets add another bound.
- `goal_wait` is useful when an external job or background worker must finish before the next meaningful turn.

Trade-offs:

- Completion is still claimed by the same working model. Its evidence contract is stronger than plain prose, but it is not Claude Code's separate evaluator.
- The package has one runtime dependency, `@narumitw/pi-tui-kit` (with transitive UI dependencies), and the loaded entry is a generated bundle rather than the readable source modules.
- Earlier releases had severe runaway-loop bugs ([#338](https://github.com/narumiruna/pi-extensions/issues/338), [#544](https://github.com/narumiruna/pi-extensions/issues/544)). Both reports are closed, and the current hard response/no-progress caps directly address that failure class; nevertheless, keep the finite defaults enabled.
- Its background waiting is explicit: it does not automatically inspect this repository's custom `subagents` extension. The agent must call `goal_wait` or otherwise avoid unnecessary continuation while children run.
- Settings are written to `~/.pi/agent/pi-goal.json`. On this machine `~/.pi/agent` is a symlink into `tools/ai/pi`, so installation should also add `pi-goal.json` and legacy `pi-goal-state.json` to the submodule's local Git exclude to prevent accidental commits.

Maintenance signal: 100 published versions, 0.54.8 released on the evidence date, green repository CI, and 33,473 npm downloads in the preceding 30-day window. Download counts may include CI/cache traffic and are only a rough adoption indicator. Sources: [npm registry](https://registry.npmjs.org/@narumitw/pi-goal), [npm downloads API](https://api.npmjs.org/downloads/point/last-month/%40narumitw%2Fpi-goal), [README](https://pi.dev/packages/@narumitw/pi-goal).

## 2. `@pify/goal` — closest Claude/Codex blend and my overall pick

Install:

```bash
pi install npm:@pify/goal
```

Notable commands:

```text
/goal <objective>
/goal
/goal pause | resume | clear
/goal budget 500k
/goal audit on
/goal steps
```

Why choose it:

- It combines Codex-like session goal controls with a Claude-like optional second-agent completion check.
- The audit agent uses the current session model, only `read`, `grep`, `find`, and `ls`, an in-memory session, and no extensions or prompt templates. It is instructed to disprove the completion claim against the workspace. Source: [audit implementation](https://github.com/pifydev/goal/blob/main/extensions/goal.ts).
- Ordered goals require step-by-step evidence before final completion.
- Only the user can create or resume goals; the worker model cannot silently expand intent.
- It has no non-peer runtime dependencies and is small compared with the richer packages.

Trade-offs:

- Audit is opt-in and costs another model call for each completion claim.
- The audit **fails open**: timeout, cancellation, startup failure, or unreadable output becomes `inconclusive`, and completion is accepted with a warning. This avoids trapping a session but is weaker than a fail-closed verification gate. Source: [audit decision code](https://github.com/pifydev/goal/blob/main/src/audit.ts).
- It was developed against Pi 0.85.1 and uses wildcard Pi peers. That should load on 0.86.0, but the manifest does not make a specific 0.86 validation claim.
- It is young: 15 published versions, no GitHub stars at evidence time, and 1,726 preceding-month npm downloads.

Use this when the independent check is more valuable than choosing the most established compatible implementation.

## 3. `pi-codex-goal` — best for a focused Codex-style port

Install:

```bash
pi install npm:pi-codex-goal
```

Commands and tools:

```text
/create-goal <task>        # recommended prompt template
/goal
/goal <objective>
/goal pause | resume | resume cancel | copy | clear
```

Model tools are `create_goal`, `get_goal`, and `update_goal`.

Strengths:

- The narrow feature set is easy to understand and closely follows Codex's session-owned goal idea.
- Goal state lives in Pi custom session entries and follows branch history, resume, fork, `/tree`, reload, and compaction without a separate database.
- The included `/create-goal` prompt expands a casual request into objective, verification, constraints, iteration, audit, and blocked-stop requirements.
- It has no runtime dependencies beyond Pi peers. Its only subprocess path is the user-invoked `/goal copy`, which calls a fixed platform clipboard command with a timeout.
- The project runs ordinary CI plus package/platform smoke workflows; its README claims Pi >=0.84 and Node 24 support, so this machine satisfies both.

Trade-offs:

- It does not launch an independent completion model. The worker performs a prompted audit and then calls `update_goal(status: "complete")`.
- It has no hard automatic-turn ceiling and no repeated-no-progress detector. Omitting the optional token budget leaves continuation unlimited; the minimum explicit budget is 500,000 tokens. This is the largest unattended-spend risk in the compatible shortlist.
- Unlike the newer candidates, it schedules continuation from `agent_end` plus a short idle/pending-message poll rather than Pi's `agent_settled` lifecycle event.
- Its command surface is not exact current Codex parity: replacing with `/goal <objective>` substitutes for Codex's documented `/goal edit` route.
- The npm artifact has a registry signature but no npm provenance attestation; the other main shortlisted packages publish provenance.

Maintenance signal: 43 published versions, 190 GitHub stars, recent green CI, and 2,835 preceding-month npm downloads. Sources: [README](https://github.com/fitchmultz/pi-codex-goal), [package.json](https://raw.githubusercontent.com/fitchmultz/pi-codex-goal/main/package.json), [npm downloads](https://api.npmjs.org/downloads/point/last-month/pi-codex-goal).

## Other options

### `@pinet/agent-goal`

This is the closest structural match to Claude's “evaluate every settled turn” approach. A separate evaluator classifies each run as `continue`, `complete`, or `blocked`; it also supports durable checkpoints and `/goal list` across sessions. The cost is more machinery: a SQLite database at `~/.pi/agent/agent-goals.sqlite`, six model-visible tools, and an extra model evaluation after every settled goal run rather than only on a completion claim. Its UX also differs from Codex/Claude: timed snooze replaces an ordinary indefinite pause/resume flow. Source: [package README](https://pi.dev/packages/@pinet/agent-goal).

Choose it only if continuous independent evaluation and cross-session goal inventory justify the extra calls and state.

### `pi-goal-x`

Feature-for-feature this is the most capable package: conversational goal drafting, tasks/subtasks, multiple open goals, regular and ordered Sisyphus modes, dashboards, evidence, recovery tools, and a configurable independent auditor. It is also active and widely downloaded. However, its published 0.31.6 manifest pins Pi packages to `>=0.83.0 <0.85.0`, while this machine runs 0.86.0. Source: [package.json](https://raw.githubusercontent.com/tmonk/pi-goal-x/main/package.json).

Wait for a release that explicitly includes Pi 0.86 before considering it. Do not bypass the peer range for a globally installed autonomous loop.

### `pi-better-goal`

Its differentiator is deferring continuation while known background work is active. Source inspection shows its built-in subagent collector reads metadata produced by the companion `pi-better-subagents` runtime under the OS temp directory. This machine uses a different local `subagents` extension, and no existing adapter/event integration was found. Installing it alone would therefore not automatically make this machine's current subagents visible to the goal loop. Source: [package source](https://github.com/1aboveio/pi-better-harness/tree/main/packages/pi-better-goal).

## Interaction with this machine's subagents

All shortlisted packages have an important limitation with the local `extensions/subagents` implementation:

- `subagent_spawn` is fire-and-forget. Pi's `ctx.isIdle()` only means the foreground agent is not streaming; it does not mean detached subagents have finished.
- The local subagent manager and goal packages both deliver follow-up turns from `agent_settled`. A goal continuation can therefore run before a pending subagent result is delivered or while child edits are still happening.
- Neither `@pify/goal` nor `@narumitw/pi-goal` automatically reads the local subagent manager. Their `goal_wait` tools are the practical mitigation: after spawning background work, the agent should wait rather than repeatedly audit an incomplete workspace.
- `pi-better-goal` can accept a custom background provider through its event protocol, but no adapter to the current subagent manager exists. Its built-in collector only understands companion `pi-better-subagents` metadata.

Until an adapter is added, avoid goals that launch concurrent write-capable subagents, or explicitly instruct the agent to call `goal_wait` after dispatch and continue when results arrive.

## Supply-chain/source review summary

- `@pify/goal`: zero runtime dependencies, no filesystem writes, subprocesses, network calls, or telemetry; optional auditor runs in-memory with read-only tools and no extensions.
- `@narumitw/pi-goal`: one direct UI dependency plus transitives; writes settings under the Pi agent directory; no outbound network/telemetry found.
- `pi-codex-goal`: zero runtime dependencies; fixed clipboard subprocess only; no outbound network/telemetry found; no npm provenance attestation.
- `pi-goal-x`: zero runtime dependencies but the broadest/invasive hook surface, including raw provider-payload cache-point manipulation; its independent auditor includes `bash`, and it writes project-local `.pi/goals/` state.
- `pi-better-goal`: one `yaml` dependency and persistent background polling/timers.

All Pi extensions run with the user's full host permissions; these observations reduce but do not remove installation risk.

## Recommendation matrix

- Choose **`@pify/goal`** for the closest blend of Claude's independent completion check and Codex's straightforward controls, with the cleanest safety/supply-chain profile.
- Choose **`@narumitw/pi-goal`** if exact Pi 0.86 development alignment, maturity, timed waiting, and richer lifecycle handling outweigh independent auditing.
- Avoid **`pi-codex-goal`** for unattended runs unless every goal has an explicit budget and you accept the absence of a turn/no-progress ceiling.
- Choose **`@pinet/agent-goal`** only for continuous independent evaluation plus cross-session goal inventory.
- Wait on **`pi-goal-x`** until its peer range includes Pi 0.86.
- Do not choose **`pi-better-goal`** solely for background awareness unless its provider protocol is integrated with the local subagent manager.

For the stated “like Claude Code and Codex” requirement, start with **`@pify/goal`** and enable `/goal audit on`. If you value the most battle-tested Pi 0.86 lifecycle instead, use **`@narumitw/pi-goal`** with its finite 25-response and three-no-progress defaults. Install exactly one: these packages register the same `/goal` command and overlapping tool names.
