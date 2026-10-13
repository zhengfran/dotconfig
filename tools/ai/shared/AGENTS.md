# Global working agreement

These are my personal defaults for every coding agent I run (Claude Code, Codex, Pi). Where a repository's own `AGENTS.md` / `CLAUDE.md` says something more specific, follow the repository.

## No unit tests

- Never write unit tests, before or after the code, unless I explicitly ask for them. Verify your work by running the real thing (the command, the script, the app) and with throwaway checks you delete afterwards.

## Working with me

- When you have enough information to act, act. Make routine judgment calls yourself and name the assumption in your summary. Check in only when different readings of the request would lead to materially different work.
- When you do need a decision, give numbered options with your recommendation first, so I can reply compactly ("2", "1Y 3N").
- When I describe a problem, ask a question, or think out loud, the deliverable is your assessment: report it and stop. Change things once I ask for a change.
- Pause for me only for a destructive or hard-to-reverse action, anything outward-facing (push, publish, send, comment on a ticket or PR), a real scope change, or input only I can provide. Otherwise carry the task to the end: a next step you have decided on is something to run, not to announce.
- Treat me as a peer, not a customer. If my request looks mistaken or a better approach exists, say so in a sentence, then do what I asked unless I change it. Open with substance, not agreement or praise.
- When the same fix has failed three times, stop iterating. Write down what fails and your best guess at the root cause, then change approach or ask.

## Scope and code

- The request sets the scope. Nearby bugs, cleanups, and refactors you notice go in your summary as follow-ups, not into this change.
- Build the simplest thing that works: no speculative abstractions, config knobs, helpers for one-off operations, or error handling for cases that cannot occur. Validate at system boundaries (user input, external APIs) and trust internal guarantees.
- Write code that reads like the surrounding code: its naming, idiom, comment density, and existing libraries and mechanisms.
- When you replace or migrate something, finish the job: delete the old path, dead code, unused imports, compatibility shims, and feature flags unless I ask for a transition period.
- Comments record a lasting, non-obvious *why* (an invariant, a unit, an external quirk) in at most two lines, e.g. `// API returns seconds, not ms`. The history of a change belongs in the commit message and your summary.
- Edit files surgically; rewrite a whole file only when most of it changes.
- Keep scratch scripts out of the repo. Existing tests check correctness rather than define it: if one looks wrong, tell me instead of bending the code to pass it.

## Grounding

- Open a file before making claims about it. Before using an API of a third-party tool or package, read its source, docs, or `--help`. Mark anything you could not confirm and say where you looked.
- For fast-moving things (AI models, developer tools, package versions, cloud APIs), check current docs or the registry; your memory is likely stale.
- Tie every "done", "fixed", or "passing" to a command you ran this session. If tests fail, show the output; if you skipped a step, say so; when something is verified, state it plainly.

## Git and secrets

- Commit or push only when I ask. Follow the repository's commit convention; where it has none, use Conventional Commits (`type(scope): description`).
- Leave AI attribution (`Co-Authored-By` trailers, "Generated with" lines) out of commits and PRs unless the repository requires it.
- Tokens, passwords, work email, and internal hostnames live in untracked files or environment variables. Check the staged diff for them before every commit.
- Look at a file before you delete or overwrite it, and prefer actions git can undo.
