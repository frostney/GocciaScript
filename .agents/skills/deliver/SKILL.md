---
name: deliver
description: >-
  Carries one feature, bug, issue, branch, or PR through verified delivery.
  Use when asked to deliver or ship a work item end to end, such as getting it
  merged or deployed, or when the user runs /deliver.
license: Unlicense OR MIT
compatibility: >-
  Uses the project's implementation, GitHub and integration tools and available
  workflow skills; needs access to the selected delivery destination. Reads the
  delivery settings with Node.js 20 or newer.
metadata:
  agents-role: entry-point
  agents-text: Carry one feature, bug, issue, branch, or PR to its verified delivery endpoint.
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

The endpoint comes from the [delivery settings](#delivery-settings). `deployed`
means the project's configured default integration destination, which may be
production, staging, nightly or another established target. Read project
instructions, delivery documentation and actual workflows to identify it; do not
assume production or choose among ambiguous destinations.

A delivery request authorizes the in-scope development, validation, publication,
feedback handling and Git operations needed for its endpoint, including merge
and the configured integration path for `deployed`. Preserve explicit limits,
repository protections and existing authorization. An earlier endpoint does not
authorize later operations. Complete independent work before asking for a
specific unresolved destination, provider, new infrastructure, spending or other
[material](../software-engineering-excellence/SKILL.md#blockers-and-decisions)
choice. Repair an existing integration workflow when an established
in-scope defect blocks delivery; do not replace its provider or invent a new
service as an incidental fix.

## Delivery settings

Run `node scripts/delivery-settings.mjs <project root>`; the path is relative to
this skill directory. It reads the frontmatter of `ORCHESTRATION.md` beside
`AGENTS.md` and prints:

- `endpoint`: `ready-to-merge`, `merged` or `deployed`, default `ready-to-merge`;
- `entry-points`: `deliver` or `stop`, default `deliver`.

`ORCHESTRATION.md` only sets the default. A request in the prompt decides that
one run in either direction, for example a later or earlier endpoint, read-only
or one step. A missing file or key takes its default, so without such a request
nothing is merged unless the project sets `merged` or `deployed`. A failed read
is a blocker: report its error.

With `entry-points: deliver`, a skill started directly that develops, reviews,
tests or publishes a change continues through `/deliver` from its own stage to
the endpoint, instead of stopping after its own step. These are `/implement`,
`/code-review`, `/test-against-spec`, `/create-pr`, `/update-pr`,
`/address-feedback`, and any other skill whose job edits the repository. Other
entry points, such as an audit or a status report, finish their own job. With
`stop`, each returns after its own step. A skill that `/deliver` or another
workflow invoked returns to its caller.

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
- At `ready-to-merge`, require the complete readiness contract defined by
  `address-feedback`'s readiness references for the selected PR's exact head: all verified
  requirement gaps resolved, current behavior and project evidence, successful
  required CI and reviews, and completed required thread handling. When the
  repository's declared full-CI rule requires a full or extended CI run for
  the change, start it as that rule says once required checks and reviews have
  converged on the final head, and await it; it is part of readiness. For a
  native stack, require its complete-stack readiness, including every needed
  fix layer. Ready for review alone does not satisfy this endpoint.
- For `merged` or `deployed`, recheck readiness and use `git-workflow` to merge
  the authorized ordinary PR or complete native stack as soon as it is ready.
  Verify the resulting integrated revision. A partial prefix below a required
  fix layer is not done.
- For `deployed`, run the existing integration path or await its run for the
  merged revision through `delivery-wait`. Only after that wait reports the
  merged revision deployed, verify its destination, delivered revision or
  artifact provenance, and the required behavior in that environment. When later commits are included, verify that
  the delivered revision contains the integrated change and that its acceptance
  evidence still applies. A green deployment job or merge alone is insufficient.

Delegated work runs through the same skills. A worker packet routes each step
it hands over through the skill that owns it, such as `/implement`,
`/create-pr`, `/update-pr` or `/address-feedback`. It never prescribes raw
publication or merge commands such as `gh pr create` or `gh pr merge`; those
bypass the skill's gates, native stack detection and readiness checks. Each
packet records the capability tier its lane runs on. Read
[capability tiers](../milestone-rush/references/orchestration.md#capability-tiers)
when you write a packet, run one, or receive a lane's escalation. Include the
[long-command and lane rules](../delivery-wait/SKILL.md#long-commands-and-delegated-lanes)
and the owner that resumes pending work in every packet.

Use required companion skills when available; when one is unavailable, follow
its reachable documented contract directly or report the specific missing
capability. Do not invent a successful handoff. Reuse matching evidence for
unchanged content, command, environment and requirements; rerun only missing or
invalidated checks. Preserve independent review judgment.

Apply `git-workflow`'s
[default-branch health](../git-workflow/SKILL.md#default-branch-health) gate
before development starts and again before each merge, including its
coverage sampler and any staleness bound repository-root `ORCHESTRATION.md`
declares. A default-branch repair it calls for is a separate work item that
comes before this one.

Integration delivery and release publication are separate outcomes. Never
invoke `/create-release` from this single-item loop or use a milestone release
as a substitute for integration delivery. A configured nightly integration can
satisfy `deployed` when its revision and behavior are verified. If the project
has only a release path and no identifiable integration destination, finish
everything that does not depend on it, including the merge when the endpoint
allows it, then ask for that concrete delivery decision.

## Continue or finish

Continue through in-scope failures while a safe corrective action or useful
investigation remains. When repeated attempts produce no new evidence or
progress, reassess the cause and approach; escalate the specific remaining
blocker after safe alternatives are exhausted. Do not loop unchanged probes,
weaken acceptance or expand scope to manufacture progress.

When only external transitions remain, such as CI, review automation or a
deployment, park the work item: arm one wait for each awaited transition under
[`delivery-wait`'s running rules](../delivery-wait/SKILL.md#running-a-wait),
record resumable state, and end the turn. Resume from each returned result,
and arm the next wait before ending another turn while the host supports it.
If it cannot notify or keep a wait armed, use the shared bounded-check fallback
and report the exact time or event to resume at.

When the session goes on to another work item, start it from the recorded
result and handoff, not from this item's working context. Compact the context
or start a fresh one at that boundary where the host allows it.

Finish only when the selected endpoint has observed evidence. An external
blocker, unresolved material decision or needed authority can leave it
incomplete; report the exact gap and next transition. Keep resumable state in
the project's handoff convention. Return the work item, the endpoint reached
by name, such as ready to merge, rather than only the PR state, the verified
revision and destination when applicable, PR and integration evidence,
and any remaining limitation. Match each completion claim to a returned result
for that specific action and target. A successful merge does not confirm branch
deletion: a deletion flag is a request, and deletion of an earlier branch says
nothing about the repair branch. An acknowledgment likewise does not confirm
cleanup or message delivery. Verify required outcomes; omit optional unconfirmed
claims or label them unconfirmed, without implying failure. Apply this to the
whole report, including tables and parenthetical remarks; see
[agent-writing](../agent-writing/SKILL.md) for shared writing guidance.

Call remote CI green, or a PR ready, only after `delivery-wait` reports it
satisfied for the current head commit, and then say so in plain words. Call a
local check passing from its actual result on the tested content. Treat a
failure as flaky or transient only after its failing log shows it is
intermittent. Blaming something other than the change, or leaving the failure
out of the required repair, needs its own evidence; a flaky failure the change
introduced is in scope.
