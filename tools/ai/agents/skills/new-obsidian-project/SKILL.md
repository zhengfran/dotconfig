---
name: project-breakdown
description: When the user shares a project idea at any level of detail — a napkin sketch ("I want to build X"), a passing thought ("thinking about starting Y"), or a detailed brief — relentlessly interview them round-by-round to resolve every branch of the decision tree (scope, constraints, risks, success criteria), then break the result into a Project + Epics + Tasks note tree in their Obsidian vault. Use whenever the user presents ANY project-sized intent, even if they don't explicitly ask for breakdown. Triggers on "/project-breakdown", "new project idea", "break this down", "let's plan this project", "新项目", "I want to build/make/start", and similar phrasings.
---

# Project Breakdown

When the user shares a project idea, your job has two phases:

1. **Interview them** relentlessly, one question at a time, walking the decision tree until the project's shape is fully resolved.
2. **Break the outcome down** into a Project + Epics + Tasks note tree inside the user's Obsidian vault, using the vault's exact schema.

Do not skip the interview. The value is in the friction — the questions force the user to confront decisions they were glossing over. A half-grilled idea produces a half-useful note tree.

## Phase 1 — The Interview

Mirror the `grill-me` methodology:

- **One question at a time.** Never batch. Wait for the answer before asking the next.
- **Walk the tree, don't survey.** Each answer opens new branches and closes others. Let the user's answer steer where you probe next.
- **Resolve dependencies.** If an earlier answer makes a later question moot, skip it. If a later answer contradicts an earlier one, loop back.
- **Always propose your recommended answer.** Frame every question as "Here's what I'd recommend — X — because Y. Does that fit, or would you rather Z?" This turns the interview into a collaborative design session, not a survey.
- **Research before asking.** If something can be answered by reading the vault (e.g., "have I done a similar project before?"), read it before asking. Check `06-Spaces/<Space>/02-Projects/` for related projects and reference them.
- **Push back on vagueness.** If the user says "some users", ask "which users, specifically — name one". If they say "soon", ask "by what date". If they say "I don't know", ask "what would you need to find out to know?"

### Dimensions to probe

The interview covers six dimensions. Walk them in rough order, but jump based on where clarity is weakest. Not every round is required for every project — see "Adaptive depth" below.

**Round 1 — Core (Why does this exist?)**
- What exactly are you building? Give me one sentence.
- Why now? What breaks if you don't do this?
- Who benefits? Just you, or others? Name one.
- What triggered this idea?

**Round 2 — Scope (Where are the edges?)**
- What's the minimum viable version — the smallest thing that proves the idea?
- What's the maximum ambition — the shiniest version?
- What's explicitly OUT of scope? Be specific.
- Must-have vs. nice-to-have split?

**Round 3 — Constraints (What's fixed?)**
- Deadline: hard / soft / none?
- Budget of money, time, energy?
- Tech / platform choices already locked in?
- Dependencies on other people or external systems?

**Round 4 — Approach (How?)**
- What are the phases — rough chronological shape?
- Riskiest assumption you're making?
- First concrete step you'd take tomorrow morning?
- Where do you start — core, edges, or infrastructure?

**Round 5 — Risks (What breaks this?)**
- What's most likely to kill this project?
- What would make you lose interest or procrastinate?
- Your failure mode on past projects — what's the pattern?
- Early-exit criterion: when would you abandon this?

**Round 6 — Success (When is it done?)**
- How will you KNOW it's done?
- Minimum "success" outcome?
- What would make you proud in a year?
- What will you specifically point at and say "this worked"?

### Adaptive depth

Calibrate rounds to project size:

- **Tiny** (weekend hack, <1 week): 2-3 rounds (Core + Scope + skim the rest). Move fast to breakdown.
- **Medium** (several weeks): 4-5 rounds.
- **Large** (months): all 6, possibly looped. Real projects deserve real grilling.

After each round, check in: *"Anything missing from this area, or shall we move to [next dimension]?"*

### Stopping criterion

Stop the interview when you can write a ~150-word project summary that covers:
- What it is (Core)
- What's in and out (Scope)
- What's fixed (Constraints)
- How you'll approach it (Approach)
- What could go wrong (Risks)
- How you'll know it's done (Success)

Confirm with the user: *"Here's what I've captured — does this feel like the whole picture? [summary]"*

If yes → proceed to Phase 2. If no → keep grilling on the weak spots.

## Phase 2 — The Breakdown

### Step 1 — Propose the tree

Before creating any files, draft the full tree in chat and get approval:

```
📦 <Project Name> (Project, Phase: Idea)
 ├─ 🏛️ Epic 1: <axis of work>
 │   ├─ ✅ Task: <concrete action>
 │   ├─ ✅ Task: <concrete action>
 │   └─ ✅ Task: <concrete action>
 ├─ 🏛️ Epic 2: <axis of work>
 │   ├─ 📖 Story: <intermediate grouping, only if Epic is large>
 │   │   ├─ ✅ Task: ...
 │   │   └─ ✅ Task: ...
 │   └─ ✅ Task: ...
 └─ 🏛️ Epic 3: ...
```

Sizing guidance:
- **3-7 Epics** per Project. Epics are major axes of work (e.g., "Build data pipeline", "Design UI", "Deploy + monitor").
- **3-8 Tasks** per Epic (or per Story, if the Epic needs Stories).
- **Stories are optional** — use them only when an Epic has enough work to need intermediate grouping (e.g., 10+ tasks that cluster into 2-3 themes).
- Every leaf Task should be concrete enough that a future-you can start it without re-planning.

Accept edits. The user will likely trim, merge, or rename things.

### Step 2 — Determine the destination

Ask the user which **Space** the project belongs to if not obvious:
- `06-Spaces/Work/02-Projects/` — work projects
- `06-Spaces/Life/02-Projects/` — personal life projects
- `06-Spaces/SelfStudy/02-Projects/` — learning projects

Infer from context where possible (tags, topic), but confirm on ambiguous cases.

Project folder path: `06-Spaces/<Space>/02-Projects/<Project Name>/`

### Step 3 — Create the files

Use the frontmatter schemas below. All dates default to today (YYYY-MM-DD). All filenames match the note's display name.

**Project note** — `<Project Name>.md`:

```yaml
---
Type: Project
Phase:
  - Idea
Priority:
  - <Low|Medium|High>
Due:
Parent:
Project: "[[<Project Name>]]"
tags:
  - <work|life|study>
Description: <1-sentence from the interview>
lastmod: <YYYY-MM-DD>
---
## 🎯 Goal
<2-3 sentences — the "why" from Round 1>

## 📐 Scope
- **In**: <bullet list from Round 2>
- **Out**: <what's explicitly excluded>
- **Success criteria**: <from Round 6>

## ⚠️ Risks & Constraints
<from Rounds 3 + 5>

## 🏛️ Epics
![[Epics.base]]

## 📖 Stories
![[Stories.base]]

## ✅ Tasks
![[Tasks.base]]

## 🐞 Bugs
![[Bugs.base]]

## 📎 Supplements
![[Supplements.base]]

## 📝 Notes

## 🔗 Links
```

**Epic note** — `<Epic Name>.md`:

```yaml
---
Type: Epic
Status:
  - Ready
Priority:
  - <Low|Medium|High>
Due:
Parent: "[[<Project Name>]]"
Project: "[[<Project Name>]]"
tags:
  - <space-tag>
Description: <what this Epic accomplishes>
---
## 🎯 Goal
<what "done" means for this Epic>

## 🧩 Sub-items
![[Children.base]]

## 📝 Notes
```

**Story note** (optional) — `<Story Name>.md`:

```yaml
---
Type: Story
Status:
  - Ready
Priority:
  - <Low|Medium|High>
Due:
Parent: "[[<Epic Name>]]"
Project: "[[<Project Name>]]"
tags:
  - <space-tag>
Description: <what this Story accomplishes>
---
## 🎯 Goal

## 🧩 Sub-items
![[Children.base]]

## 📝 Notes
```

**Task note** — `<Task Name>.md`:

```yaml
---
Type: Task
Status:
  - Ready
Priority:
  - <Low|Medium|High>
Due:
Parent: "[[<Epic or Story Name>]]"
Project: "[[<Project Name>]]"
tags:
  - <space-tag>
Description: <1-sentence acceptance criterion>
---
## 🎯 Acceptance
<concrete, verifiable criterion>

## 📝 Notes
```

### Step 4 — Report

After creation, give the user a concise summary:
- Path where files live
- Count of notes created (1 Project + N Epics + M Tasks)
- Link to the Project note so they can click into their new dashboard: `[[<Project Name>]]`

## Vault schema reference (why these fields exist)

The user has an established PARA-style vault with a custom agile schema:

| Concept | Property | Values |
|---|---|---|
| Note type | `Type` | Project / Epic / Story / Task / Bug / Idea / Supplement / Cheatsheet |
| Project lifecycle | `Phase` (Projects only) | Idea / Researching / In Progress / On hold / Finished / Archived |
| Work lifecycle | `Status` (Epic/Story/Task/Bug) | Ready / Ongoing / Blocked / Done / Archived |
| Direct hierarchy | `Parent` | `[[Parent Note]]` — immediate parent |
| Roll-up anchor | `Project` | `[[Project Name]]` — every descendant sets this |
| Urgency | `Priority` | Low / Medium / High / Critical |
| Gear lifecycle | `Usage` (physical items only) | Active in use / Not in use / Rarely in use |

Critical distinctions:

- **`Phase` is ONLY for Projects.** Never put Phase on an Epic or Task.
- **`Status` is for everything else in the work hierarchy.** Never put Status on a Project.
- **`Parent` vs `Project`:** Parent is the note directly above (Task → Story → Epic → Project). Project is always the top-level Project note, set on every descendant at any depth. This dual linking is what makes the bases roll up correctly.
- **Use exact capitalization.** `Status` ≠ `status`. Obsidian normalizes property names case-insensitively for the dropdown, which causes value pool pollution. Always write `Status`, `Phase`, `Priority`, `Due`, `Parent`, `Project`, `Type`, `Description` with the exact casing above.

## Naming conventions

- **Project folder**: Title Case, filesystem-safe. e.g., "Personal Budget Tracker"
- **Epic names**: "Verb + noun phrase". e.g., "Build data ingestion pipeline"
- **Story names**: Same as Epics.
- **Task names**: Concrete, specific action. e.g., "Define CSV schema for transactions"

## Do NOT

- Don't batch questions. One at a time — this is the core of the method.
- Don't accept "I don't know" without probing what's blocking the answer.
- Don't create files before showing the full breakdown tree and getting approval.
- Don't skip proposing your recommended answer — it's half the value.
- Don't bend the vault's schema to fit the project — map the project into the schema.
- Don't use `Status` on Project notes or `Phase` on Task/Epic notes.
- Don't guess the Space — if work/life/study is ambiguous, ask.

## DO

- Do explore the vault first. Run a quick `Glob` for similar past projects in `06-Spaces/*/02-Projects/` and mention them: *"Your [[Aurora]] project had a similar constraint — want to reuse that approach?"*
- Do propose concrete values (dates, priorities, scope cuts) when the user is vague. You're a collaborator, not a secretary.
- Do loop back if a later answer invalidates an earlier one. Explicitly: *"Your last answer means we should reconsider [earlier question] — let's revisit."*
- Do adapt depth to project size. A weekend hack shouldn't get the same grilling as a 6-month endeavor.
- Do write the Project note's `Description` as the 1-sentence answer to "what is this?"
- Do include a `Due:` date when the user gave one, even informally ("by summer" → the last day of August).

## Example opening line

When the user drops an idea, open with something like:

> Got it — a budget tracker. Before I help you break this down, I want to grill you a bit to make sure we carve it up the right way. I'll go one question at a time.
>
> **Round 1, Question 1:** In one sentence, what *exactly* are you building? My guess: "A web app that imports bank CSVs and categorizes spending for monthly reviews." Does that match, or are you thinking of something different?

Then listen, resolve, move to the next branch.
