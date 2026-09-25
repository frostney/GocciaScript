---
name: roadmap-review
description: >-
  Reviews a roadmap from fresh project evidence and produces a verified,
  throughput-anchored version plan, with execution gated on confirmation. Use
  when reviewing a roadmap, planning releases, or sequencing a backlog.
license: Unlicense OR MIT
compatibility: >-
  Requires access to the Git hosting service; default data pulls use GitHub/gh.
  A CI-published
  conformance, coverage, or benchmark metric is used when present but is
  optional.
---

# Roadmap review

Produce a source-verified roadmap and throughput-anchored release plan. Analysis
is the deliverable; issue, pull-request, milestone, and release changes are
optional and confirmation-gated.

## Evidence contract

- Pull current issues, releases, milestones, merged PR history, and project
  direction documents this run. Record missing sources and lower confidence
  rather than filling gaps from memory.
- Measure 90-day merged-PR throughput and issue-created-to-PR-merged lead time.
  Use PR-opened-to-merged only as a flagged fallback. Exclude closed triage with
  no implementation.
- When a domain metric exists, measure its slope from spaced historical CI
  artifacts. Allocation by label or area describes past effort, not capacity.
- Before proposing or characterizing work, classify it against current code,
  tests, or primary specifications as Done, Partial, or Absent with evidence.
  Propose only Partial or Absent work.

Use a small bounded subagent fan-out only for genuinely independent, sizeable
evidence areas; otherwise work directly. Synthesize before planning.

## Workflow

1. Ground the review in the evidence contract and reconcile issue, pull-request,
   release, and milestone counts from the Git hosting service.
2. Assess release cadence, merged-but-unreleased work, milestone-versus-commit
   drift, vision, scope, and non-goals.
3. Apply measured rates to the counted backlog, with basis and confidence. Never
   pad the timeline. When evidence cannot support calendar dates, report ranges
   and dependencies without a dated Gantt or placeholder dates.
4. Verify every candidate against source and remove work already delivered.
5. Plan themed releases across independent tracks, marking dependencies and the
   longest pole. Surface genuine human decisions with a recommendation.
6. Stop for those decisions and present:
   - current state, drift, and release-cadence recommendation;
   - measured velocity and confidence;
   - versioned, sized tracks;
   - throughput-anchored timeline with a Mermaid Gantt when available;
   - open decisions.
7. Offer a date-stamped `ROADMAP-YYMMDD.md`; do not write it without the user's
   request.
8. Create or modify milestones, issues, due dates, or labels only after explicit
   confirmation of the finalized plan. Create each issue through `/create-issue`
   and report exactly what changed.
9. After the confirmed milestone and its tracked scope exist, offer
   `/milestone-rush <milestone>` as an explicit next action. Never start the
   rush automatically; selecting it grants that skill's scoped implementation
   and merge authority.

Keep the report decision-relevant and snapshot-dated. Re-pull before acting on
an old report.
