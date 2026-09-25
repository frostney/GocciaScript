---
name: deliver
description: >-
  Carries one feature, bug, issue, branch, or PR through verified delivery.
  Use when asked to deliver a work item or run /deliver.
license: Unlicense OR MIT
compatibility: >-
  Uses the project's implementation, GitHub and integration tools and available
  workflow skills; needs access to the selected delivery destination.
---

# Deliver

Own one work item's outcome through its selected endpoint. Enter from its actual
state, reuse settled decisions and valid evidence, and keep the parent task
active when a child skill returns a fixable failure or pending transition.

## Resolve scope and endpoint

Infer the repository and work item from the request, issue, bug description,
confirmed idea, current branch or PR. Verify identities and existing work before
mutation. Use `/create-issue` only when tracking is requested or required; an
unfiled idea can go directly to `/implement`.

Honor an explicit endpoint: `ready-to-merge`, `merged`, or `deployed`. The default
is `deployed` to the project's configured default integration destination, which
may be production, staging, nightly or another established target. Read project
instructions, delivery documentation and actual workflows to identify it; do not
assume production or choose among ambiguous destinations.

A delivery request authorizes the in-scope development, validation, publication,
feedback handling and Git operations needed for its endpoint, including merge
and the configured integration path for `deployed`. Preserve explicit limits,
repository protections and existing authorization. An earlier endpoint does not
authorize later operations. Complete independent work before asking for a
specific unresolved destination, provider, new infrastructure, spending or other
material choice. Repair an existing integration workflow when an established
in-scope defect blocks delivery; do not replace its provider or invent a new
service as an incidental fix.

## Coordinate the existing workflows

- Use `/implement` for development and required-behavior gaps. It owns running
  and inspecting the result, code review, specification testing and in-scope
  repair. Pass the agreed outcome and fidelity criteria, not just a mechanism.
- Use `/create-pr` for first publication and `/update-pr` for an existing PR.
  Keep the same work item and reuse its open PR through repairs. Reconcile
  uncertain writes before retrying; do not create a replacement PR merely
  because a call failed. If integration verification finds a required gap after
  merge, create a linked repair PR from the fresh default branch. Keep delivery
  active through its review, merge and integration verification; never rewrite
  merged history or try to update a closed PR.
- Use `/address-feedback` to converge required reviews and current findings.
  Validate each finding against the agreed outcome; optional extras do not
  enlarge scope. Await pending checks and provider transitions through existing
  deterministic helpers. Inspect failures and route fixable causes through the
  development loop, then publish and verify the new exact head.
- At `ready-to-merge`, require the selected PR's complete readiness contract:
  all verified requirement gaps resolved, current behavior and project evidence,
  successful required CI and reviews, and completed required thread handling.
  For a native stack, require complete-stack readiness, including every needed
  fix layer. Ready for review alone does not satisfy this endpoint.
- For `merged` or `deployed`, recheck readiness and use `git-workflow` to merge
  the authorized ordinary PR or complete native stack. Verify the resulting
  integrated revision. A partial prefix below a required fix layer is not done.
- For `deployed`, run or await the existing integration path, then verify its
  destination, delivered revision or artifact provenance, and the required
  behavior in that environment. When later commits are included, verify that
  the delivered revision contains the integrated change and that its acceptance
  evidence still applies. A green deployment job or merge alone is insufficient.

Use required companion skills when available; when one is unavailable, follow
its reachable documented contract directly or report the specific missing
capability. Do not invent a successful handoff. Reuse matching evidence for
unchanged content, command, environment and requirements; rerun only missing or
invalidated checks. Preserve independent review judgment.

Integration delivery and release publication are separate outcomes. Never
invoke `/create-release` from this single-item loop or use a milestone release
as a substitute for integration delivery. A configured nightly integration can
satisfy `deployed` when its revision and behavior are verified. If the project
has only a release path and no identifiable integration destination, finish
independent work and ask for that concrete delivery decision.

## Continue or finish

Continue through in-scope failures while a safe corrective action or useful
investigation remains. When repeated attempts produce no new evidence or
progress, reassess the cause and approach; escalate the specific remaining
blocker after safe alternatives are exhausted. Do not loop unchanged probes,
weaken acceptance or expand scope to manufacture progress.

Finish only when the selected endpoint has observed evidence. An external
blocker, unresolved material decision or needed authority can leave it
incomplete; report the exact gap and next transition. Keep resumable state in
the project's handoff convention. Return the work item, chosen endpoint,
verified revision and destination when applicable, PR and integration evidence,
and any remaining limitation. Match each completion claim to a returned result
for that specific action and target. A successful merge does not confirm branch
deletion: a deletion flag is a request, and deletion of an earlier branch says
nothing about the repair branch. An acknowledgment likewise does not confirm
cleanup or message delivery. Verify required outcomes; omit optional unconfirmed
claims or label them unconfirmed, without implying failure. Apply this to the
whole report, including tables and parenthetical remarks; see
[agent-writing](../agent-writing/SKILL.md) for shared writing guidance. Release
publication belongs to a separately requested release or the milestone boundary.
