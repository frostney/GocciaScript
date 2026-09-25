---
name: run-retro
description: >-
  Review a workstream and agree process improvements when the user requests or
  accepts a retrospective. Apply only selected follow-up actions.
license: Unlicense OR MIT
compatibility: >-
  Requires the registered grilling skill and access to
  the workstream's available conversation, repository, issue, pull-request, and
  CI evidence. Selected tickets also require create-issue and access to the Git
  hosting service; selected immediate deliveries require deliver. Timing
  ledger analysis uses Python 3 when available telemetry calls for it.
---

# Run retrospective

Assess the completed workstream through delivery-speed, process, and
codebase-health lenses. The actual `grilling` skill owns the decision loop.
Apply only documentation and ticket actions the user selects from the detailed
summary. An explicitly selected immediate implementation enters its normal
workflow rather than being implemented directly by this skill. Present the
retrospective in the conversation. The request alone authorizes analysis;
file edits and other mutations require the selection described below.

## Gates

- Define the workstream boundary from the current conversation, handoff, diffs,
  commits, issues, PRs, reviews, checks, outcomes, and rework. Record unavailable
  evidence and lower confidence; do not invent a narrative or broaden into a
  repository audit.
- Before proposing a process or design correction, trace its originating
  decision, the current implementation or executable configuration, and the
  current documentation. Distinguish missing design from implementation drift
  and documentation drift. Preserve a settled choice unless contradictory
  current evidence requires a new decision; do not re-grill it merely because
  the implementation or prose drifted.
- Inspect the complete available coordinator and subagent record, including
  process events and handoffs. Identify repeated reasoning, rebuilt or dropped
  evidence, loaded-but-unused or bypassed skills, manual resumes,
  capability-routing mistakes, and other context consumption that added no
  value. Report this no-value context separately from other findings and name
  any unavailable conversation or process evidence.
- For a completed Milestone Rush, read its ignored
  `.agent/milestone-rush-events.jsonl` under
  [references/timing-analysis.md](references/timing-analysis.md). Reconcile it
  with current repository, issue, pull-request, and CI evidence; partial or
  missing telemetry is a confidence limitation, not permission to infer values.
- Invoke `grilling` with the evidence and candidates. Do not imitate it with
  ad-hoc questions; stop if it is unavailable. Let it ask one decision at a time
  with a recommendation. Act only after it reaches shared understanding and
  confirms the exact action set.
- Assess all three lenses, even when one produces no durable finding:
  - **Delivery speed:** less waiting, rework, handoff friction, unnecessary
    scope, or cognitive load without weakening quality.
  - **Process:** planning, decisions, handoffs, gates, tools, and collaboration.
  - **Codebase:** architecture, maintainability, tests, developer experience,
    reliability, and accumulated friction.
- Promote only generalized, project-level lessons supported by evidence. Exclude
  chronology, one-off mistakes, personal preferences, duplicates, existing
  rules, and speculation.
- Apply de-duplication to the workstream: reuse prior investigations and
  decisions, coalesce the same event from multiple evidence systems, and combine
  candidate lessons with the same cause and action while retaining provenance.
  Report repeated implementation encountered in the workstream, but do not turn
  the retro into a repository-wide duplication or discoverability audit.

## Route each lesson

- **Documentation edit:** durable guidance in existing contracts, READMEs,
  `docs/`, ADRs, AGENTS, skills, templates, policies, or contributor guidance.
  Prefer tightening or coupling with existing text. Direct edits are limited to
  documentation.
- **Follow-up ticket:** source, executable configuration, or other implementation
  is needed. Offer more detail, further grilling, normal or automatic
  `create-issue`, or skip; the delegated workflow retains its own gates.
- **Implement before next cycle:** recommend this route when a selected
  improvement should be delivered before another cycle begins. Recommendation
  alone authorizes nothing. After explicit user selection, reuse or create its
  visibility issue through `create-issue`, enter the normal `/deliver`
  workflow, and keep this retrospective active until the action is delivered or
  genuinely blocked. The delegated workflows retain all of their gates.
- **Report only:** useful evidence warrants neither an edit nor a ticket.

Use both edit and ticket only when the guidance and its implementation are
separately necessary. A missing document may be created only when the user
selects its exact proposed contents.

## Workflow

1. Resolve the workstream boundary and read relevant project documentation.
2. Trace the process origins, implementation/configuration, documentation, and
   complete available coordinator/subagent record. Then build an evidence
   ledger of outcomes, friction, rework, surprises, effective or missed gates,
   successful practices, and separately classified no-value context under all
   three lenses. Select one or more timing profiles from the reference and map
   both high-level lifecycle phases and available ground-level operations.
3. Reconstruct exclusive critical-path contribution, overlapping or masked
   work, and aggregate resource consumption without double-counting. Rank
   bottlenecks by exclusive wall-clock impact; report confidence and missing
   attribution.
4. Remove unsupported, session-specific, duplicate, and already-covered
   candidates; classify the rest using the routes above.
5. Summarize key delivered changes and material outcomes in conversation with
   evidence-backed Before and After states, uncertainty, and source links. Use
   a diagram or comparison table when it clarifies a mechanism or decision.
6. Run `grilling` one decision at a time with the boundary, evidence, current
   docs, absences, and candidates. Offer to deep-dive into any finding before
   selecting actions; incorporate new evidence into the comparison.
7. Present the detailed summary:
   - findings under every lens, including no-finding results;
   - lifecycle and selected surface profiles, exclusive critical-path ranking,
     masked or overlapping work, ground-level build/test/tool timings, aggregate
     resource totals, evidence provenance, and confidence gaps;
   - key outcomes, Before/After states, and available deep dives;
   - exact proposed documentation additions, replacements, or removals by file;
   - concise ticket summaries with all available action paths;
   - report-only observations, supporting evidence, confidence, and gaps.
8. Obtain exact user selections through `grilling`. More detail or further
   grilling returns to that loop and regenerates the summary.
9. Apply only selected documentation changes, preserving structure and avoiding
   duplication. Run only selected ticket actions through `create-issue`. For
   each explicitly selected implement-before-next-cycle action, reuse or create
   the visibility issue, invoke normal `/deliver`, and remain in the
   retrospective until it is delivered or genuinely blocked.
10. Compare the result with the confirmed action set, reread edited sections,
    and run declared documentation checks.
11. Report changed docs, created issue links, immediate-action
    delivery or blocker evidence, report-only findings, confidence limits, and
    observed validation. Keep workstream history in chat and remind the user
    they can request a deep dive into any finding.

Confirmation authorizes only the selected documentation and ticket actions and
invocation of any explicitly selected immediate implementation through its
normal workflow. It does not authorize unrelated commits, pushes, PRs, issues,
or file edits. The original request to run a retrospective is not this final
confirmation.
