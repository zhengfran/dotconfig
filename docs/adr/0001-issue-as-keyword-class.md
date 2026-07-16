# Issues are a second org TODO keyword class, not tags or separate notes

Project tracking needed a first-class Issue concept (unplanned
problems/blockers) alongside Tasks (planned work) in Denote project notes. We
decided an Issue is any org heading carrying a keyword from a dedicated second
`org-todo-keywords` sequence — `ISSUE(i!) INVESTIGATING(v) | WONTFIX(w@)
RESOLVED(r!)` — mirroring the identity rule for Tasks (any heading with a
task-sequence keyword, regardless of outline position).

## Considered Options

- **Tag `:issue:` on normal TODO headings** — rejected: org tag inheritance
  makes children of an issue look like issues, and Task vs Issue could no
  longer be distinguished by keyword in agenda/table queries.
- **One Denote note per issue (`__issue`)** — rejected as the *identity*
  model: state and tracking live on headings in the project note. Long-form
  content may still be extracted to a Detail Note (`__task`/`__issue` denote
  keyword, outside `projects/`), but the note is pure content — the keyword
  and children stay on the originating heading, which links to the note.
- **Epic/Story/Task hierarchy** (previously implemented in
  `my/denote-capture-task`) — rejected in favor of structure-agnostic
  identity; the capture machinery is removed. Real project files use free-form
  groupings (e.g. `** Nvidia`), so tracking must not depend on outline shape.

## Consequences

- An open Issue under a Task blocks that Task from closing (custom blocker
  check scoped to issue-class children only — child Tasks do not block
  parents, preserving existing file behavior).
- An Issue belongs to exactly one Project; other affected projects link to it.
- No capture command exists for Issues by design: typing a heading and setting
  the keyword *is* creation. Extraction to a Detail Note, by contrast, *is* a
  command (one-way, no merge-back): it is multi-step and error-prone by hand.
- Agenda machinery must treat RESOLVED/WONTFIX as done-class, and
  "actively working" views match `ONGOING|INVESTIGATING`.
