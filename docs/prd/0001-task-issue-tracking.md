# PRD: Task & Issue Tracking in Denote Project Notes

Status: ready-for-agent
Related: [ADR 0001](../adr/0001-issue-as-keyword-class.md), [CONTEXT.md](../../CONTEXT.md)

## Problem Statement

I track my Projects in Emacs org-mode via Denote notes, and the meta-project
note already gives me a Kanban and status tables across all Projects. But
inside a Project I only have planned work — a free-form `* Tasks` tree. Two
things are missing:

1. When something breaks during work (a bug, a hardware fault, a vendor
   blocker), I have nowhere structured to put it. Today it gets buried as a
   checkbox or a pasted log inside whatever Task I was doing, and I lose track
   of open problems — especially once the surrounding Task is collapsed or
   marked done.
2. When a single Task or Issue accumulates a lot of content (an 80-line kernel
   panic, a long investigation log), it bloats the Project note and makes the
   whole file hard to scan.

I want the same first-class treatment for Tasks and Issues that I already have
for Projects, plus a clean way to move long-form content out of the way
without losing the item's place in my tracking.

## Solution

Introduce **Issue** as a first-class item type alongside **Task**, both living
as headings inside a Project note and identified purely by their org keyword.
Give each Project note a Dashboard with live tables of open Tasks and open
Issues, wire open Issues into the daily agenda, and prevent a Task from
closing while it still has an open Issue underneath it. When an item's body
gets too long, one command extracts it into its own **Detail Note**, leaving a
linked stub — with lifecycle state — behind in the Project note.

## User Stories

1. As a Project owner, I want a dedicated Issue item type distinct from a Task, so that unplanned problems are tracked separately from planned work.
2. As a Project owner, I want an Issue to be any heading carrying an issue keyword, so that I can create one by typing a heading anywhere without a special command.
3. As a Project owner, I want Issue keywords to have their own lifecycle (Issue → Investigating → Resolved / Won't-Fix), so that an Issue's progress reads differently from a Task's.
4. As a Project owner, I want resolving an Issue to log a timestamp and abandoning one (Won't-Fix) to prompt for a reason, so that months later I understand what happened and why.
5. As a Project owner, I want to place an Issue as a child of the Task it blocks, so that the relationship between a problem and the work it obstructs is visible in the outline.
6. As a Project owner, I want to place an Issue directly under a project-level heading, so that problems not tied to a specific Task still have a home.
7. As a Project owner, I want a Task to be prevented from being marked done while it has an open child Issue, so that open problems can never get buried inside a collapsed done Task.
8. As a Project owner, I want to be able to unblock a Task by resolving, won't-fixing, or promoting the child Issue to project level, so that the blocking rule has clear, easy escapes.
9. As a Project owner, I want child Tasks NOT to block their parent Task from closing, so that my existing Project files keep behaving exactly as before.
10. As a Project owner, I want each Project note to have a Dashboard with a table of its open Tasks, so that I can see live planned work at a glance.
11. As a Project owner, I want a separate table of the Project's open Issues, so that I can see what is currently broken without mixing it with planned work.
12. As a Project owner, I want the Task table to show state, priority, and deadline per row, so that I can triage without opening each heading.
13. As a Project owner, I want the Issue table to show state and which Task each Issue blocks, so that I can see the impact of each open problem.
14. As a Project owner, I want closed items to stay in the outline rather than clutter the Dashboard, so that the Dashboard answers only "what is live right now".
15. As a Project owner, I want to cycle an item's state with C-c C-c on its Dashboard row, so that I get the same in-place interaction the Project status tables already give me.
16. As a Project owner, I want an agenda view listing my open Issues grouped by Project, so that I get a cross-Project "what is broken anywhere" picture.
17. As a Project owner, I want the Issues agenda view to cover only Active Projects, so that On-Hold Projects stay fully paused, Issues included.
18. As a Project owner, I want my actively-worked Issues (Investigating) to appear alongside my Ongoing Tasks in the daily Dashboard agenda, so that firefighting days don't look empty.
19. As a Project owner, I want Resolved and Won't-Fix Issues excluded from agenda "todo" scans the same way Done and Cancel Tasks are, so that closed problems don't reappear as actionable.
20. As a Project owner, I want an Issue to belong to exactly one Project, so that its state lives in one place and can't drift out of sync.
21. As a Project owner, I want other affected Projects to link to an Issue rather than duplicate it, so that a shared problem is still discoverable from each Project without double bookkeeping.
22. As a Project owner, I want to extract a long Task or Issue body into its own Detail Note with one command, so that my Project note stays scannable.
23. As a Project owner, I want extraction to leave the heading — with its keyword, priority, and child Issues — in the Project note, so that all my tracking keeps working untouched.
24. As a Project owner, I want the extracted Detail Note to carry a `__task` or `__issue` Denote keyword, so that it is typed and findable but never mistaken for a Project or reference note.
25. As a Project owner, I want the stub and the Detail Note to link to each other, so that I can navigate between the tracking heading and its long-form content in both directions.
26. As a Project owner, I want the Detail Note to live outside the projects directory, so that the Active-Project agenda scanner never picks it up as a Project.
27. As a Project owner, I want to change an extracted item's lifecycle state on the stub in the Project note, so that there is one obvious place to manage state.
28. As a Project owner, I want the old Epic/Story capture paths removed, so that the config reflects the one structure-agnostic model and doesn't invite divergent workflows.
29. As a Project owner, I want my existing free-form Task groupings (e.g. `** Nvidia`, `** AMD`) to keep working, so that adopting this feature requires no migration of my current notes.

## Implementation Decisions

**Domain model** (see CONTEXT.md and ADR 0001):

- A **Task** is any heading whose keyword is in the task sequence; an **Issue**
  is any heading whose keyword is in the issue sequence. Identity is the
  keyword, never the outline position — tracking must not depend on outline
  shape.
- Issues are a second `org-todo-keywords` sequence, not tags and not separate
  notes (ADR 0001): `(sequence "ISSUE(i!)" "INVESTIGATING(v)" "|" "WONTFIX(w@)"
  "RESOLVED(r!)")`, added alongside the existing task sequence
  `(sequence "TODO(t)" "ONGOING(o)" "|" "CANCEL(c@)" "DONE(d!)")`.
  `RESOLVED` logs a timestamp (`!`); `WONTFIX` prompts for a note (`@`).
- An Issue belongs to exactly one Project (the one driving the investigation);
  other affected Projects reference it with a Denote/org link.
- Putting a Project On-Hold pauses its Tasks and open Issues — they leave
  ambient views until the Project is Active again.

**Modules touched** (`basic/editor/emacs/modular/modules/`):

- `org-agenda-config.el` — add the issue keyword sequence; add an Issues
  agenda command (proposed key `i`) matching open issue-class keywords across
  Active Project files (reusing the existing `org-agenda-files` = Active
  Projects mechanism); widen the daily Dashboard "Active" section from
  `ONGOING` to also match `INVESTIGATING`; extend the global done-skip
  (`org-agenda-skip-function-global`) to treat `RESOLVED`/`WONTFIX` like
  `DONE`/`CANCEL`.
- `denote-config.el` — add the Dashboard dblock writers, the item classifier,
  the blocker predicate + hook, and the extraction command; remove the
  Epic/Story capture paths from `my/denote-capture-task`.

**New functions / seams:**

- `my/project-item-type` — given a heading (at point), return `task`, `issue`,
  or nil based on which keyword sequence its keyword belongs to. The single
  source of truth every other piece consults.
- `my/project-collect-items` — given a Project file, return a list of item
  plists (`:title :link :state :priority :deadline :type :blocks`). The
  Dashboard dblock writers render this; they own no query logic themselves.
- `org-dblock-write:task-table` / `org-dblock-write:issue-table` — render the
  collected open items of the corresponding type into a table under a
  `* Dashboard` heading. Open items only; closed items stay in the outline.
- A `C-c C-c` handler for Dashboard rows that cycles the underlying heading's
  state, registered on `org-ctrl-c-ctrl-c-hook` next to the existing
  `my/project-table-ctrl-c-ctrl-c`.
- `my/task-blocked-by-open-issue-p` — given a Task heading, return non-nil if
  any direct or descendant child heading is issue-class and open. Wired into
  `org-blocker-hook` so a Task cannot become `DONE` while blocked. Scoped to
  issue-class children only — child Tasks never block a parent.
- `my/denote-extract-item` — with point on a Task/Issue heading: create a
  Detail Note via Denote (title = heading title, keyword = `task` or `issue`
  per item type, located in the notes root, not `projects/`), move the
  heading's body text (NOT its keyword, priority, or child headings) into the
  note, insert a link from the stub to the note and a back-link from the note
  to the Project note. One-way; no merge-back command.

**Interactions:**

- The blocker check and the item classifier both key off the same keyword-set
  membership test, so adding/renaming a keyword only changes one definition.
- Extraction is deliberately a command (multi-step, error-prone by hand),
  whereas Issue creation is deliberately NOT a command (typing a heading and
  setting the keyword is creation) — see ADR 0001 consequences.

## Testing Decisions

**What makes a good test here:** exercise external behavior through the
highest seam — the data a function returns or the buffer state it produces —
not org-mode internals or table-string formatting. Set up a temp buffer (or
load a fixture `.org` file), call the function, assert on the result.

New infrastructure: a `modular/tests/` directory with fixture `.org` files and
an `emacs --batch -l ert` runner. **Prior art:** the vendored ERT suites under
`straight/repos/org/testing/` and `straight/repos/straight.el/tests/`.

**Modules/functions under test:**

- `my/project-item-type` — fixtures with headings across every task and issue
  keyword (and a plain heading); assert the returned type for each. This is
  the cheapest, highest-value test — everything else depends on it.
- `my/project-collect-items` — a fixture Project file with a mix of open/closed
  Tasks and Issues under varied groupings (flat, nested, free-form like
  `** Nvidia`); assert the returned plist list contains exactly the open items
  with correct type, state, and blocks. Testing the collector (not the table
  string) is the deliberate high seam.
- `my/task-blocked-by-open-issue-p` — fixture Tasks with: no children; a child
  Task only; an open child Issue; a resolved child Issue; a nested open Issue.
  Assert blocked vs not for each. Covers story 7/8/9 directly.
- Extraction buffer-rewrite — split the stub rewrite from the Denote file I/O
  so the rewrite is unit-testable: given a fixture heading with a long body and
  child Issues, assert the post-rewrite stub keeps its keyword, priority, and
  children, gains a link line, and has lost the body. The Denote note creation
  and back-link are verified by a lighter integration check.

**Not ERT-tested (configuration, validated by load + manual):** the issue
keyword sequence, `INVESTIGATING` in the daily Dashboard, the `RESOLVED`/
`WONTFIX` done-skip, and the `i` agenda command. These are declarative
org-agenda settings; unit-testing them would assert org internals. Validate
with `emacs --batch --init-directory modular/ -l init.el` plus manual checks.

## Out of Scope

- A cross-Project Issue rollup dblock in the meta-project note, and any in-file
  kanban for items (considered, not selected).
- A capture command for Issues (creation is manual by design).
- A merge-back / inline command for Detail Notes (extraction is one-way).
- Making Detail Notes themselves agenda or tracking sources — they are pure
  content; state stays on the stub.
- Duplicating an Issue across Projects or a shared cross-Project issues note.
- Migrating existing Project files to any new structure — the model is
  structure-agnostic, so no migration is needed.
- Showing Issues from On-Hold Projects anywhere.

## Further Notes

- Keyword letters chosen to avoid collision with the task sequence: `i` Issue,
  `v` inVestigating, `w` Wontfix, `r` Resolved. Confirm none clash with
  existing single-key org bindings on first load.
- The daily Dashboard "Active" match becomes the regexp/keyword set
  `ONGOING|INVESTIGATING`; keep the two lifecycles' terminal states aligned in
  the done-skip so the agenda stays a view of only what is actionable now.
- Publication note: filed as a local markdown PRD rather than a GitHub issue
  because the repo's `gh` auth is currently broken (an invalid `GITHUB_TOKEN`
  overrides stored credentials) and no triage-label vocabulary is configured.
