# Project Tracking (Emacs org-mode)

The personal project/task/issue tracking domain built on Denote notes and
org-mode, managed by the Emacs configuration in `basic/editor/emacs/`.

## Language

**Project**:
A unit of ongoing work with its own Denote note (`__project` keyword) and a
lifecycle status of Active, On-Hold, or Archived. Putting a Project On-Hold
pauses everything inside it — its Tasks and open Issues disappear from
ambient views until the Project is Active again.
_Avoid_: workspace, repo

**Task**:
A planned unit of work inside a Project — something you chose to do. Any
outline position counts; a Task is identified by its lifecycle state, not by
where it sits. Lifecycle: Todo → Ongoing → Done or Cancelled.
_Avoid_: ticket, action item

**Issue**:
An unplanned problem discovered during work — a bug, defect, or obstacle that
happened *to* you rather than work you planned. An Issue can block a specific
Task or affect the Project as a whole. Lifecycle: Issue (open) →
Investigating → Resolved or Won't-Fix. An Issue placed under a Task blocks
that Task: the Task cannot close until every child Issue is Resolved,
Won't-Fixed, or promoted to a project-level Issue. An Issue belongs to
exactly one Project — the one driving the investigation; other affected
Projects link to it rather than duplicating it.
_Avoid_: bug (too narrow), ticket, problem

**Detail Note**:
A Denote note (`__task` or `__issue` keyword) holding the long-form content
of a single Task or Issue that outgrew its Project note. Pure content: the
lifecycle state stays on the originating heading in the Project note, which
links to the Detail Note; the Detail Note links back.
_Avoid_: sub-note, log note
