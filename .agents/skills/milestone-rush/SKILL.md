---
name: milestone-rush
description: >-
  Autonomously completes a confirmed milestone by reconciling existing work,
  coordinating work-item delivery and the configured milestone release, and
  closing the verified milestone. Use when the user runs /milestone-rush or
  asks to complete one exact, confirmed milestone, including selecting it
  after /roadmap-review.
license: Unlicense OR MIT
compatibility: >-
  Requires authenticated GitHub access, git worktrees, the internal
  `delivery-wait` skill, and a host that supports subagents and passive
  foreground-process waiting; implementation, review, and validation use the
  project's installed workflow skills and declared gates.
---

# Milestone rush

Finish the named milestone from its actual current state. Own the coordinated
work-item deliveries and the release at the verified milestone boundary.

## Authority and gates

- Require an exact repository and milestone. An explicit invocation authorizes
  scoped issue comments and replacement issues, branches and worktrees,
  implementation, validation, commits, plain pushes, pull requests, review
  remediation, squash merges, configured integration delivery, the milestone's
  configured release path, branch cleanup, and closing that milestone.
- Accept either a confirmed `/roadmap-review` handoff or direct invocation. For
  direct invocation, verify current scope, direction, readiness, dependencies,
  and measures of success before executing. Stop for material replanning rather
  than silently changing the milestone, for example for a closure that records a
  material rejected or deferred product decision, or a material scope expansion.
- Treat a confirmed roadmap item as the mini-spec for `/deliver` only when it
  states the outcome, scope and non-goals, and testable measures of success.
- Respect project instructions, Definitions of Ready and Done, branch
  protection, review policy, and the remote default branch. Never amend, bypass
  a gate, or overwrite unrelated work. Ordinary branches remain merge-only;
  only `git-workflow` may apply its guarded native-stack rewrite exception.
- Before planning or spawning, read and validate repository-root
  `ORCHESTRATION.md` under
  [references/orchestration.md](references/orchestration.md). Then start the
  ignored event ledger by passing the run's first lifecycle event to the
  `ingest` command under [references/event-ledger.md](references/event-ledger.md)
  before the first issue, PR, or worker action, even when the run then stops at
  a prerequisite. When the host exposes ingestion as a ledger or telemetry
  operation instead of a shell, use it; only when neither exists, report the
  ledger as unavailable.

## Reconcile and plan

1. Pull fresh milestone, issue, pull-request, review, check, default-branch,
   branch, worktree, and relevant local working-tree state. Read current project
   direction, completion contracts, and the validated orchestration policy or
   reported provider-neutral fallback.
2. Run every repository-declared lane-admission preflight from the orchestration
   reference before adopting or creating a worktree. Relocate or reject an unsafe
   worktree before implementation or a complete local gate; never learn a known
   environmental limit by burning the full suite.
3. Classify every milestone item and related local change as delivered, open PR,
   active implementation, ready, blocked, or invalid. Reuse valid work instead
   of restarting it.
4. Verify closed items against source and merge evidence. Read
   [references/scope-changes.md](references/scope-changes.md) when required work
   was closed without delivery.
5. Inspect the current delivery surface and include the provider-neutral CI
   integration recommendation required by the orchestration reference. This is
   a plan artifact, not authority to change workflows, labels, rulesets,
   controllers, credentials, or provider configuration. When execution requires
   a missing capability, file it as a repository-owned prerequisite under
   [references/scope-changes.md](references/scope-changes.md); otherwise
   document the safe current-CI fallback and its cost.
6. Build a dependency and likely-conflict graph. A native stack may represent a
   true dependency chain or a confirmed logical decomposition of one large
   issue, provided each layer is independently reviewable. Prioritize the
   longest pole and early risk reduction, then dispatch every independent ready
   node to isolated subagents and worktrees up to current platform capacity. Do
   not impose a separate issue, wave, review-round, or retry limit.
7. Maintain the outer workflow's resumable state in the ignored
   `.agent/HANDOFF.md` after each issue or PR transition. Record the milestone
   identity, graph, active worktrees, issue-to-PR state, blockers, observed
   validation, and the stable decision registry. Never stage or commit the
   checkpoint; reconcile it with live state when resuming.

## Execute and integrate

1. Keep the coordinator thin: it owns confirmed decisions and provenance,
   graph state, worker admission and replacement, delivery-state promotion,
   integration, merge, and milestone closure. Detailed investigation,
   implementation, remediation, and validation belong to bounded workers.
2. Give each worker one context-isolated task packet and its dependencies.
   Use `/deliver` with the issue or confirmed roadmap item, required endpoint,
   configured integration destination and applicable evidence. Pass the selected
   approach so `/implement` need not reopen it. Retain this workflow's
   `/code-review fix-all` review default in the packet. A work item defaults to
   its configured integration delivery; the milestone owns shared sequencing and
   release publication. Material choices remain with the user.
3. Adopt an existing PR when it satisfies the issue and project gates. Adopt
   relevant local state only when its ownership and scope are clear; preserve
   ambiguous, dirty, pre-existing, or unrelated state and report it.
4. Let each `/deliver` own its implementation, publication, feedback and
   integration loop. Reconcile returned revision, behavior, PR and destination
   evidence before promoting its work item. Give a native dependency stack one
   delivery owner for its complete readiness and atomic merge; never give
   competing workers authority over the same stack or merge a partial prefix
   beneath a required fix layer.
5. Keep remediation validation focused on the changed behavior. Run the
   repository's complete local gate once only after implementation and bounded
   review fixes converge on the intended head, unless a new material source
   change invalidates it. Do not repeatedly use a complete suite as the
   diagnostic loop.
6. Treat heavyweight full CI as terminal promotion evidence, never as a remote
   debugger. Dispatch it only after the current base, required PR checks, and
   every active review tool have converged on the candidate head. Cancel
   superseded runs when the CI service supports safe cancellation; record
   otherwise unavoidable waste. A later head, base, topology, or review change
   invalidates the proof.
7. Integrate continuously rather than waiting for a batch. After every squash
   merge, re-pull milestone scope and default-branch state; merge the updated
   remote default into every affected remaining branch and rerun its applicable
   gates. Review and CI evidence is valid only for the current PR head.
8. Read [references/scope-changes.md](references/scope-changes.md) when
   execution discovers new work, issues are added to the milestone externally,
   or a milestone merge causes an integrated regression.

Manage implementation and review workers within the host's shared capacity.
Keep implementation nodes running while useful work remains, and queue
review-axis lanes until slots free up; temporary slot exhaustion is not
sub-agent unavailability. If review sub-agents are unsupported, remain
unavailable after bounded retry, or return incomplete evidence, the
implementation worker completes those lanes directly and records the fallback
for the milestone report.

Normalize every material lifecycle, decision, wait, gate, usage, retry, rework,
and integration transition, then pass it to the event-ledger `ingest` command.
Host adapters own translation from native events; never add provider transcript
parsers to this skill. Inner delivery and review loops launch the bundled
deterministic foreground waits; the outer milestone loop passively awaits
worker or command completion as the orchestration reference's event-driven
waits describe and reconciles its checkpoint after each returned transition.
Unsupported passive waiting required by repository policy blocks spawning.

## Blockers and completion

- Quarantine a blocked node and its dependents, then continue every independent
  runnable node. Pause only when no further safe progress remains. Never close
  a milestone with blocked or unverified work.
- Retry transient review, CI, issue, and pull-request states through
  event-driven waits under the host's platform limits. Use an exact safe
  `retry_at` when available. A rate limit or missing verdict is pending, not
  green.
- Before closure, re-fetch milestone, issue, pull-request, review, and CI state
  and verify that every in-scope item is delivered and closed with evidence; no
  milestone PR, required check, review thread, or active review-tool pass
  remains pending; and the synced default branch passes the applicable full
  project gate. A failure resumes execution.
- Run the event-ledger `validate` and `summarize` commands for the current
  `runId` before closure. Any invalid closure evidence the event-ledger
  reference lists blocks closure until corrected or explicitly marked
  unavailable under the schema.
- After the work-item and integrated gates pass, invoke `/create-release`
  with the milestone's release authority, the settled release plan and version
  or deterministic project version policy, and current evidence; ask only for
  an unresolved material release choice. It owns release preparation and
  publication through the configured release path. A pending or failed
  required release keeps milestone delivery active. A project with no release
  path requires a concrete decision, not an invented publisher or an assumed
  successful release.
- Close the milestone only after its required release outcome is verified.
  Remove only clean, merged worktrees created by this run; preserve and report
  every other worktree.

## Report

Match each completion claim to a returned result for that specific action and
target. Release success does not confirm cleanup or message delivery. Sending a
retrospective question and receiving an acknowledgment does not establish that
it was queued: ask it directly in the report or describe it as requested unless
delivery was confirmed. Verify required outcomes; omit optional unconfirmed
claims or label them unconfirmed, without implying failure. Apply this to the
whole report, including tables and parenthetical remarks; see
[agent-writing](../agent-writing/SKILL.md) for shared writing guidance.

Return one audit-style summary covering every item below. Use these item names
as labels, and keep the orchestration policy's and this skill's own terms for
requirements, capabilities, and prerequisites (for example `stack prefix`,
`repository-owned prerequisite`, `fallback`): repository audits and the
retrospective match them literally, so a paraphrase loses the link. When an
item's result is missing or unconfirmed, say so rather than dropping it:

- initial and final scope, including scope drift;
- issue, worker/worktree, PR, and squash-merge mapping;
- each PR's review-axis-to-lane map, lane statuses, and every single-agent
  fallback with its reason, in `code-review`'s report terms;
- reused local or PR state;
- orchestration policy status, decision IDs and conflicts, worker context
  modes, and any monitoring fallback, named as a fallback;
- the CI integration recommendation, labelled as a recommendation even when a
  prerequisite issue carries the same content, with each repository-owned
  prerequisite or the current-CI fallback cost;
- event-ledger path and completeness, intervention checkpoints, and unavailable
  telemetry fields;
- validation and reviewer evidence for final PR heads and integrated default;
- required additions, deferred follow-ups, blockers, and remaining work;
- integration destinations, release result and revision/artifact evidence;
- cleanup or preserved state and milestone closure status.

If the milestone stays open, end with the remaining work and the steps that
still lead to closure. If the milestone closed, ask in the report whether to run
`/run-retro`; invoke it only after explicit approval.
