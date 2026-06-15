# Workspace buffer tabs filter by a tab-local linked project

The tab-line shows file buffers belonging to the workspace's **Linked Project**;
an unlinked workspace shows all buffers. The link is stored as a `linked-project`
parameter (project root string) inside the tab-bar tab's own alist, set explicitly
by `SPC w p` (at creation) or `SPC w P` (relink), and read by both the tab-line
filter and the consult buffer switcher.

## Considered Options

- **Frame parameter keyed by tab index** (`tab-project-N`, the prior approach):
  rejected because closing or reordering a workspace shifts indices and silently
  corrupts every link to its right.
- **Filter by `project-current`** (the current buffer's project): rejected because
  the buffer list would change meaning whenever you visit a file outside the
  workspace's project, breaking the "fixed for the life of the workspace" intent.
- **Tab-local `linked-project` alist entry** (chosen): the link travels with the
  tab through close/reorder, and — confirmed against the Emacs 30.2 `tab-bar.el`
  source — survives desktop save/restore for free, since `frameset-filter-tabs`
  strips only the `wc-*` keys and keeps printable custom params.

## Consequences

- The prior `tab-project-N` frame-parameter mechanism is removed; `linked-project`
  is the single source of truth, which also fixes the index bug in
  `my/consult-project-buffer`.
- Membership is a path-prefix test against the linked root, so nested sub-projects
  count as members. The active buffer is always shown even if it is a non-member.
- Terminals keep their separate name-based per-workspace grouping, unchanged.
