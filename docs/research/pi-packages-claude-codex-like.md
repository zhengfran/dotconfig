# Pi Packages Providing Claude Code / Codex-like Workflows

Date: 2026-09-21
Scope: Third-party (and official-example) packages for the [Pi coding agent](https://pi.dev) (`@earendil-works/pi-coding-agent`, currently v0.85.0 locally) that replicate features baked into Claude Code or OpenAI Codex CLI — plan mode, subagents, permission/approval gates, MCP support, todo/task tracking, Claude-Code-style UI/themes, git checkpoints, and session sharing.

Primary sources used:
- Local Pi install docs: `/opt/homebrew/Cellar/pi-coding-agent/0.85.0/libexec/lib/node_modules/@earendil-works/pi-coding-agent/README.md` and `docs/packages.md`, `docs/security.md`
- Pi package gallery: https://pi.dev/packages
- npm registry API (`registry.npmjs.org`, `api.npmjs.org/downloads`) for each package
- GitHub API (`api.github.com/repos/...`) and raw READMEs for each package's repo

## Why Pi needs these packages at all

Pi's own README and philosophy section state explicitly that pi ships **without** sub-agents, plan mode, permission popups, built-in to-dos, or MCP support by design, and expects users to "install a third party pi package that matches your workflow" instead (`README.md` lines 15–17, 496–512). The `docs/packages.md` and README both carry the same explicit security warning:

> "Pi packages run with full system access. Extensions execute arbitrary code, and skills can instruct the model to perform any action including running executables. Review source code before installing third-party packages." (`README.md` line 413; `docs/packages.md` line 20)

Pi's `docs/security.md` reinforces this: there is **no built-in sandbox** — built-in tools and extensions "run with the permissions of the pi process," project trust is "only an input-loading guard" (not a security boundary), and prompt injection from repo content is an "expected local-agent risk" pi does not try to prevent (`docs/security.md` lines 31–37). This means every package below inherits the full privileges of whatever user account runs `pi` — file read/write, arbitrary shell execution, and network access — unless the user separately sandboxes the process (container/VM, per `docs/containerization.md`).

Packages are installed via `pi install npm:<pkg>` or `pi install git:<repo>` (`docs/packages.md` lines 22–39, 76–105) and are discoverable on npm under the `pi-package` keyword or on the gallery at https://pi.dev/packages, which as of this research listed **5,368 total packages**.

## Feature-by-feature comparison

### 1. Subagents (Claude Code's `Task` tool / subagent workflow)

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`pi-subagents`](https://github.com/nicobailon/pi-subagents) (npm `pi-subagents`, maintainer `nicopreme`/`nicobailon`) | Delegates work to focused child Pi sessions (`scout`, `researcher`, `worker`, `reviewer`, `evidence-auditor`, etc.), foreground streaming or detached background children, saved/custom workflows | `pi install npm:pi-subagents` | v0.70.0, published 2026-09-20; ~88.6K weekly npm downloads; GitHub 3,713 stars / 720 forks, actively pushed same day | MIT license. Spawns child Pi processes with the same tool access as the parent (file write, bash); background children run in a detached Node runner outside the interactive session, so work can continue unattended |
| [`@tintinweb/pi-subagents`](https://github.com/tintinweb/pi-subagents) (maintainer `tintinweb`) | Explicitly markets itself as bringing "Claude Code-like sub-agents and workflow orchestration to pi — parallel execution, live widget, fleet view, custom agent types, mid-run steering, dynamic workflows, Claude Code compatibility, look and feel" | `pi install npm:@tintinweb/pi-subagents` | v0.19.0, published 2026-08-27; ~5.5K weekly downloads; 1,198 stars / 278 forks, 109 open issues (higher issue count relative to stars vs. the `nicobailon` package) | MIT. Same class of risk as above: parallel child agents with tool access; higher open-issue count is a maintenance signal worth checking before adoption |
| [`@gotgenes/pi-subagents`](https://github.com/gotgenes/pi-packages/tree/main/packages/pi-subagents) | "Focused, in-process autonomous sub-agent core," part of a broader personal monorepo (`gotgenes/pi-packages`, 227 stars) | `pi install npm:@gotgenes/pi-subagents` | Part of an actively maintained monorepo (pushed 2026-09-20); smaller footprint than the two above | MIT-adjacent (repo has no top-level license file per GitHub API — verify per-package license before use) |
| [`pi-background-tasks`](https://github.com/ismailsaleekh/pi-background-tasks) (maintainer `ismailsaleekh`) | Durable background shell tasks, read-only delegated agents, "local attested Pi runs," fixed-purpose workflows through child Pi processes | `pi install npm:pi-background-tasks` | v2.5.0, published 2026-09-04; ~50.8K weekly downloads; only 26 GitHub stars / 25 forks despite high downloads (small, less battle-tested project relative to its usage) | ISC license. Spawns child processes and grants "read-only delegated agents" as a safety-reducing option, but base package still executes shell commands with host permissions |

**Comparison**: `pi-subagents` (nicobailon) is the most mature/widely used pure subagent package by stars and downloads. `@tintinweb/pi-subagents` is explicitly Claude-Code-flavored (naming, UI) but has a rougher issue backlog. `pi-background-tasks` trades codebase size/scrutiny for high adoption — worth extra review given the download/star mismatch.

### 2. Plan Mode (Claude Code's plan mode / Codex's read-only planning)

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`@narumitw/pi-plan-mode`](https://github.com/narumiruna/pi-extensions/tree/main/packages/pi-plan-mode) | Adds a "Codex-like read-only `/plan` collaboration mode" before implementation begins | `pi install npm:@narumitw/pi-plan-mode` | v0.58.2, published 2026-09-20; ~4.2K weekly downloads; parent monorepo `narumiruna/pi-extensions` has 587 stars / 116 forks, active same-day pushes | MIT. Part of a broader personal extension monorepo; README for the monorepo carries the same "run with full user permissions" warning verbatim (`narumiruna/pi-extensions` README) |
| [`@plannotator/pi-extension`](https://github.com/backnotprop/plannotator) | Broader "Plannotator" tool: interactive plan review with inline annotations on agent messages and code/PR review, not just a plan-mode toggle | `pi install npm:@plannotator/pi-extension` | v0.27.16, published 2026-09-18; ~23.3K weekly downloads; parent repo `backnotprop/plannotator` has 8,836 stars / 667 forks (by far the highest star count in this survey), 143 open issues | Dual-licensed MIT OR Apache-2.0. Largest community/maintenance signal of any package reviewed; higher open-issue volume is proportional to its much larger user base |

**Comparison**: `@narumitw/pi-plan-mode` is a narrower, more direct plan-mode analog explicitly modeled on Codex. `@plannotator/pi-extension` is a much larger, more general "plan review + annotation" product (also usable outside Pi) with far higher community traction — the strongest maturity signal among plan-related packages.

### 3. Permissions / Approval Gates (Claude Code's tool-approval prompts)

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`pi-verdict`](https://github.com/jesset/pi-verdict) | "A minimal permission gate for Pi in the style of Claude Code's auto mode": every tool call is checked (allow/deny/ask) via deterministic rules first, then a model classifier for ambiguous cases; fails closed on uncertainty; self-protects its own config from tampering | `pi install npm:pi-verdict` (also installable for `omp`/oh-my-pi) | v0.10.0, published 2026-09-21 (day of research); only ~717 weekly downloads; GitHub only 3 stars / 3 forks — **very new/unproven** despite a well-documented security design (ADRs, threat-model docs) | MIT. Explicitly states in its own README that Pi "has no built-in permission prompts — every tool call executes with the permissions of the pi process," matching Pi's own security docs. Because it is new and low-adoption, treat its guarantees as unverified by community use despite the thoughtful design |
| [`@gotgenes/pi-permission-system`](https://github.com/gotgenes/pi-packages/tree/main/packages/pi-permission-system) | Permission enforcement extension, paired with a separate `@gotgenes/pi-permission-model-judge` "deny-first typo-path model judge" | `pi install npm:@gotgenes/pi-permission-system` | v33.0.5 (high version number suggests frequent iterative releases), published 2026-09-20; ~8.7K weekly downloads; part of `gotgenes/pi-packages` monorepo (227 stars, actively maintained) | License not declared at repo root per GitHub API — check the specific package's `LICENSE`/`package.json` before adoption |

**Comparison**: `pi-verdict` most directly reproduces Claude Code's "ask/allow/deny" UX and documents its threat model well, but is brand new (published today, single-digit stars) — a maturity red flag for something governing tool execution. `@gotgenes/pi-permission-system` has more real-world usage and is under active iteration, but license status should be double-checked.

### 4. MCP (Model Context Protocol) Support

Pi's philosophy explicitly excludes MCP by design ("No MCP," README line 500), pointing to [this blog post](https://mariozechner.at/posts/2025-11-02-what-if-you-dont-need-mcp/) arguing CLI tools + skills are preferable, and instead recommends installing an extension for MCP support.

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`pi-mcp-adapter`](https://github.com/nicobailon/pi-mcp-adapter) (maintainer `nicopreme`) | MCP adapter extension for Pi — lets Pi connect to MCP servers | `pi install npm:pi-mcp-adapter` | v2.34.0, published 2026-09-14; **highest download count in this entire survey at ~179.6K/week**; GitHub 1,502 stars / 357 forks, only 5 open issues, pushed same day | MIT. Because it's an MCP *client* adapter, its risk surface is compounded by whatever MCP servers the user then connects — each MCP server is itself a separate trust boundary Pi does not sandbox |
| [`pi-mcp-extension`](https://www.npmjs.com/package/pi-mcp-extension) (maintainer `irahardianto`) | Alternative MCP client for Pi | `pi install npm:pi-mcp-extension` | ~26.6K weekly downloads (per gallery listing) — smaller adoption than `pi-mcp-adapter` | Same MCP-client caveats as above |

**Comparison**: `pi-mcp-adapter` is the dominant choice by a wide margin (downloads, stars, issue-to-star ratio) and is maintained by the same author (`nicobailon`/`nicopreme`) behind the most popular subagent and web-access packages — this author's packages collectively account for a large share of the gallery's top downloads.

### 5. Todo / Task Management (Claude Code's `TodoWrite` tool)

Pi's philosophy explicitly rejects built-in to-dos ("They confuse models. Use a TODO.md file, or build your own with extensions," README line 508).

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`@juicesharp/rpiv-todo`](https://github.com/juicesharp/rpiv-mono/tree/main/packages/rpiv-todo) | "A todo list for the model, rendered as a live overlay that survives `/reload` and conversation compaction" | `pi install npm:@juicesharp/rpiv-todo` | v2.10.1, published 2026-09-13; ~34K weekly downloads; parent monorepo `juicesharp/rpiv-mono` has 807 stars / 161 forks, 80 open issues | MIT. Low-risk category — a UI/state extension, not one that itself executes shell commands, though it still runs as arbitrary TypeScript with full process permissions per Pi's extension model |
| [`pi-goal-x`](https://github.com/tmonk/pi-goal-x) (maintainer `tmonk`) | Broader than a todo list: `/goal` command for conversational goal planning, flexible/ordered goals, persistent progress, plus an "independent completion auditor" | `pi install npm:pi-goal-x` | v0.31.6, published 2026-09-17; ~23.7K weekly downloads; only 65 stars / 35 forks — modest community size relative to downloads | MIT |

**Comparison**: `@juicesharp/rpiv-todo` is the closer 1:1 analog to Claude Code's todo widget (a persistent overlay) and has the largest community footprint of the two. `pi-goal-x` is a heavier goal-tracking system with an added "auditor" agent role.

### 6. Claude-Code-style UI/Theme/Status

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`pi-cc-extensions`](https://github.com/minuque/pi-cc-extensions) (maintainer `minuque`) | "A Pi productivity suite with Claude Code-style UI, context inspection, and agent/session references"; ships `cc-dark.json`/`cc-light.json` themes per its npm/gallery listing | `pi install npm:pi-cc-extensions` | v0.9.1, published 2026-09-20; ~14.8K weekly downloads; only 96 stars / 18 forks — smallest/newest project in the UI category (created 2026-07-21) | MIT. Themes are low-risk (pure JSON) but the "productivity suite" also bundles extensions, so review those separately |
| [`pi-claude-bridge`](https://github.com/elidickinson/pi-claude-bridge) (maintainer `elidickinson`) | Different category: uses Claude Code itself (via the Anthropic Agent SDK) as a model *provider* inside Pi, plus an `AskClaude` tool | `pi install npm:pi-claude-bridge` | v0.8.0, published 2026-09-21 (day of research); ~10.6K weekly downloads; 406 stars / 94 forks, 43 open issues | MIT. This package actually shells out to/embeds Claude Code — it is not merely mimicking Claude Code's look, so it inherits Claude Code's own local-agent risk profile in addition to Pi's |
| [`pi-powerline-footer`](https://www.npmjs.com/package/pi-powerline-footer) (maintainer `nicopreme`) | Powerline-style status bar, cosmetic footer replacement (Codex/Claude Code-adjacent status-line trend) | `pi install npm:pi-powerline-footer` | ~44.4K weekly downloads per gallery listing | Cosmetic — lowest risk category, but still runs as an extension with full permissions per Pi's model |

**Comparison**: `pi-cc-extensions` is the most direct "make Pi look like Claude Code" package (explicit in its own description, matching the README's own line "Make pi look like Claude Code" as a stated *possibility* of the extension system, README line 397) but is the newest/smallest by community size. `pi-claude-bridge` is a different, higher-risk category — it's not just cosmetic, it actually invokes the real Claude Code agent as a backend.

### 7. Git Checkpoints / Auto-commit

Pi's own repository ships an **official, first-party example** extension for this, not installed by default:
- [`packages/coding-agent/examples/extensions/git-checkpoint.ts`](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/examples/extensions/git-checkpoint.ts) — git-stash-based checkpointing per turn
- A companion `auto-commit-on-exit.ts` example commits on shutdown

These are documented as example code you copy/adapt (per `docs/extensions.md`'s "What's possible" list: "Git checkpointing and auto-commit," README line 394), not published npm packages — the officially blessed way to get this feature is to read and adapt the example rather than trust a third party.

Third-party alternatives:

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`@335g/pi-autocommit`](https://github.com/335g/pi-autocommit) | "Checkpoint-then-reorganise" strategy: lightweight checkpoint commits per turn, then soft-reset + LLM-reorganized into Conventional Commits at the end of the agent loop | `pi install npm:@335g/pi-autocommit` | v0.1.20, published 2026-09-18; only ~274 weekly downloads; GitHub only **1 star**, single-day-old push history — **essentially unproven, single-maintainer, minimal community validation** | MIT. This extension runs `git commit`/`git reset` autonomously on the user's behalf — a category where an unreviewed, low-adoption package is a meaningfully higher risk (it can rewrite local git history without confirmation) |
| `tmonk/pi-committer` (Conventional commit automation, per search results) | `commit_changes` tool plus `/commit`, `/commit-config`, `/commit-model` commands | via npm/git per its README | Found via search only; not independently verified against npm registry in this pass — verify before use | Same class of risk as above: autonomous git-history mutation |

**Comparison**: For git checkpointing, the **official example extension in the pi repo itself is the most trustworthy starting point** given it's first-party code you read and adapt rather than an opaque third-party dependency. Both third-party alternatives found (`@335g/pi-autocommit`, `pi-committer`) have very low adoption/scrutiny and directly manipulate git history, so they warrant the most caution of any category surveyed.

### 8. Session Sharing (Claude Code's session/transcript sharing)

Pi has first-party primitives for this that ship in core, not as a package:
- `/export` (session → HTML/JSONL) and `/share` (upload as a private GitHub gist with a shareable HTML link) are **built-in commands**, not extensions (`README.md` lines 195, 197).
- For bulk/public sharing of OSS sessions, Pi's own README recommends the first-party-adjacent tool [`badlogic/pi-share-hf`](https://github.com/badlogic/pi-share-hf) (maintainer `badlogicgames`, the author of Pi itself) for publishing sessions to Hugging Face datasets (README lines 21–35).

| Package | Scope | Install | Maturity | Security notes |
|---|---|---|---|---|
| [`pi-share-hf`](https://github.com/badlogic/pi-share-hf) | CLI tool (not a pi extension) that publishes Pi session JSONL data to a Hugging Face dataset for public sharing, e.g. `badlogicgames/pi-mono` | Separate npm/GitHub install per its own README; requires a Hugging Face account + CLI | 257 stars / 24 forks, but **last pushed 2026-04-13** — stale relative to Pi's near-daily-active core and most extensions surveyed (5+ months without a push as of this research date, 2026-09-21) | No declared license found via GitHub API. Publishes potentially sensitive session content (file paths, code, prompts) to a public dataset — review session contents before publishing; this is a data-exfiltration-adjacent risk by design (opt-in, but irreversible once public) |

**Comparison**: Unlike the other categories, session sharing doesn't need a third-party "Claude-Code-like" package — Pi's built-in `/share` (private gist) already covers the common case. `pi-share-hf` is a narrower, opt-in tool for public OSS session publishing, endorsed by Pi's own author, but shows the weakest recent-maintenance signal of anything cited here (no push in 5+ months).

## Cross-cutting maturity/security observations

- **Concentration of trust in a few authors.** A handful of npm maintainers (`nicopreme`/`nicobailon`, `narumitw`/`narumiruna`, `gotgenes`, `juicesharp`) each publish multiple top-download packages spanning several categories (subagents, MCP, web access, plan mode, permissions, todos). Installing several "best in class" packages from the gallery may concentrate a large share of your effective trust surface in 2–3 individuals' repos.
- **Downloads vs. stars mismatches are a real signal.** `pi-background-tasks` (~50.8K weekly downloads, 26 stars) and `pi-goal-x` (~23.7K downloads, 65 stars) have download counts far exceeding their GitHub community size — plausible causes include recency, being pulled in as a transitive dependency of another package, or genuinely under-starred but useful tools; worth a closer read of the source before trusting either with shell/file access.
- **New != vetted.** `pi-verdict` and `@335g/pi-autocommit` were both published within days of this research and have single-digit-to-low-triple-digit adoption. Both operate in high-stakes categories (permission gating; autonomous git history mutation, respectively) where "review source before installing" (Pi's own stated policy, `README.md` line 413) matters most.
- **No sandboxing changes with any of these packages.** None of the packages surveyed change Pi's fundamental lack of a built-in sandbox (`docs/security.md` lines 31–37). Permission-gate packages (`pi-verdict`, `@gotgenes/pi-permission-system`) add a policy layer in front of tool calls, but Pi's own docs and `pi-verdict`'s README both state this is "a permission gate, not a sandbox" — real isolation still requires an OS/container/VM boundary per `docs/containerization.md`.

## Recommendation table

| Need | Top pick | Why | Caveat |
|---|---|---|---|
| Subagents (Claude Code `Task`-like) | [`pi-subagents`](https://www.npmjs.com/package/pi-subagents) (nicobailon) | Highest stars (3.7K), high downloads (88.6K/wk), actively maintained daily | Grants child agents full tool access; review workflows before enabling unattended background runs |
| Plan mode (Codex-like) | [`@plannotator/pi-extension`](https://www.npmjs.com/package/@plannotator/pi-extension) for full plan-review UX, or [`@narumitw/pi-plan-mode`](https://www.npmjs.com/package/@narumitw/pi-plan-mode) for a minimal Codex-style toggle | Plannotator has by far the largest community (8.8K stars); pi-plan-mode is smaller/simpler if you want just the read-only gate | Plannotator is a larger surface area (annotation UI, PR review) than a narrow plan-mode need requires |
| Permission/approval gate | [`@gotgenes/pi-permission-system`](https://www.npmjs.com/package/@gotgenes/pi-permission-system) for now; watch [`pi-verdict`](https://www.npmjs.com/package/pi-verdict) | gotgenes package has real adoption (8.7K wk) and active iteration (v33.x); pi-verdict has the best-documented security model but is brand new (3 GitHub stars) | Neither replaces a sandbox; still a policy layer only |
| MCP support | [`pi-mcp-adapter`](https://www.npmjs.com/package/pi-mcp-adapter) | Dominant by every metric: 179.6K weekly downloads, 1.5K stars, only 5 open issues | Each connected MCP server is its own separate trust boundary Pi does not sandbox |
| Todo/task tracking | [`@juicesharp/rpiv-todo`](https://www.npmjs.com/package/@juicesharp/rpiv-todo) | Closest analog to Claude Code's persistent todo widget; survives reload/compaction; 807-star parent repo | Lower risk category overall (UI state, not exec), but still full-permission TypeScript |
| Claude Code look-alike UI/theme | [`pi-cc-extensions`](https://www.npmjs.com/package/pi-cc-extensions) | Directly targets Claude-Code-style UI/theme; matches Pi README's own stated "make pi look like Claude Code" use case | Newest/smallest project surveyed in this category (96 stars) — read the bundled extensions, not just the theme JSON |
| Git checkpoints/auto-commit | Official [`git-checkpoint.ts` example](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/examples/extensions/git-checkpoint.ts) (copy/adapt) | First-party code from the Pi maintainers themselves — the most trustworthy path for a feature that mutates git history | Requires manual copy/adapt rather than `pi install`; third-party alternatives (`@335g/pi-autocommit`) are very low-adoption |
| Session sharing | Built-in `/share` (private gist) for normal use; [`pi-share-hf`](https://github.com/badlogic/pi-share-hf) only for deliberate public OSS session publishing | `/share` is first-party and covers the common case with no extra install | pi-share-hf hasn't been pushed in 5+ months; review session content before any public publish |

---

*This document was generated by researching primary sources only (local Pi docs, npm registry API, GitHub API, and package READMEs) on 2026-09-21. Download/star counts are point-in-time snapshots and will drift.*
