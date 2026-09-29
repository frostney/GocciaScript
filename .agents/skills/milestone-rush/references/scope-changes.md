# Scope changes and issue filing

Read this when required milestone work was closed without delivery, execution
needs a missing delivery capability, execution discovers new work, issues are
added to the milestone externally, or a milestone merge causes an integrated
regression. The stop rules in `SKILL.md` for material replanning still apply.

## Required work closed without delivery

Comment on the closed item with the source and merge evidence, create a linked
replacement through `/create-issue`, add it to the milestone, and implement the
replacement. A closure that records a material rejected or deferred product
decision is material replanning, not a replacement.

## Filing an issue from the coordinator

This applies to replacements, missing-capability prerequisites, and
independently trackable required work.

- File through `/create-issue` and retain its evidence and attribution gates in
  any delegated packet. If those gates block posting, record the item in the
  ignored checkpoint without claiming an issue was created.
- A delegation request is not completion. Inspect the returned result and
  verify the issue URL, repository, and intended prerequisite or work item
  before reporting it as filed.
- If the worker stops or its result is uncertain, reconcile GitHub and the
  checkpoint before retrying so an accepted write is not duplicated.

A missing required delivery capability becomes a repository-owned prerequisite
issue; the orchestration reference defines what may and may not be changed on
its behalf.

## Work discovered during execution

- Add newly discovered work to the milestone only when evidence shows it is
  required by an existing requirement, dependency, regression, or Definition of
  Done. Keep tightly coupled fixes in the current PR; create an issue for
  independently trackable required work. Record desirable follow-ups without
  expanding the milestone.
- Absorb externally added issues only when they clearly fit the confirmed plan.
  Anything else is material scope expansion.
- When a milestone merge causes an integrated regression, create and implement
  the required repair. Treat unrelated or materially ambiguous failures as
  blockers.
