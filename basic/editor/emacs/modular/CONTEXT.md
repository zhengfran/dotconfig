# Modular Emacs Config

Personal Emacs configuration. This glossary captures terms whose meaning is
specific to this config and easy to confuse.

## Language

**Workspace**:
A tab-bar tab representing one working context. Each workspace may have one
fixed, explicitly-linked project.
_Avoid_: Tab (ambiguous with buffer tabs)

**Linked Project**:
The single project explicitly associated with a workspace at the time it is
created or opened, fixed for the life of that workspace. This is the project
the workspace's buffer tabs are filtered by.
_Avoid_: Current project (means something different — see below)

**Current Project**:
The project of whichever buffer is currently selected, derived from that
buffer's directory via `project-current`. Changes as you move between buffers;
NOT the same as the workspace's Linked Project.

**Buffer Tab**:
An entry on the tab-line (line 2) representing one open buffer. Distinct from a
Workspace, which lives on the tab-bar (line 1).

**Project Membership**:
A buffer belongs to a workspace's project when its file's truename is under the
Linked Project root (path-prefix test). Nested sub-projects count as members
because they sit physically inside the tree. Non-file buffers are not members.
