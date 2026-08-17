---
name: run-retro
description: >-
  Reviews a completed workstream from conversation, repository, and forge
  evidence, maps lifecycle and ground-level timings, uses grilling to agree
  improvements to delivery speed, process, and codebase health, then applies
  selected documentation edits and follow-up ticket actions. Use when ending a
  substantial workstream or running a project retrospective.
license: Unlicense OR MIT
compatibility: >-
  Requires a registered grilling skill and access to the workstream's available
  conversation, repository, and forge evidence. HTML report rendering requires
  Python 3. Selected tickets also require create-issue and forge access.
---

# Run retrospective

Assess the completed workstream through delivery-speed, process, and
codebase-health lenses. The actual `grilling` skill owns the decision loop.
Apply only documentation edits and ticket actions the user selects from the
detailed summary. Every run also produces an HTML impact report under
[references/html-report.md](references/html-report.md); creating or refreshing
that report is authorized by the retrospective request, but committing,
publishing, and all other edits are not.

## Gates

- Define the workstream boundary from the current conversation, handoff, diffs,
  commits, issues, PRs, reviews, checks, outcomes, and rework. Record unavailable
  evidence and lower confidence; do not invent a narrative or broaden into a
  repository audit.
- For a completed Milestone Rush, read its ignored
  `.agent/milestone-rush-events.jsonl` under
  [references/timing-analysis.md](references/timing-analysis.md). Reconcile it
  with current repository and forge evidence; partial or missing telemetry is a
  confidence limitation, not permission to infer values.
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
- **Report only:** useful evidence warrants neither an edit nor a ticket.

Use both edit and ticket only when the guidance and its implementation are
separately necessary. A missing document may be created only when the user
selects its exact proposed contents.

## Workflow

1. Resolve the workstream boundary and read relevant project documentation.
2. Build an evidence ledger of outcomes, friction, rework, surprises, effective
   or missed gates, and successful practices under all three lenses. Select one
   or more timing profiles from the reference and map both high-level lifecycle
   phases and available ground-level operations.
3. Reconstruct exclusive critical-path contribution, overlapping or masked
   work, and aggregate resource consumption without double-counting. Rank
   bottlenecks by exclusive wall-clock impact; report confidence and missing
   attribution.
4. Remove unsupported, session-specific, duplicate, and already-covered
   candidates; classify the rest using the routes above.
5. Draft one impact card per key delivered change or material outcome. Follow
   the HTML contract, render with its helper, and verify the file. Each impact
   item is at most 300 characters and has evidence-backed Before and After
   states. Add a mechanism diagram when the change alters a pattern, lifecycle,
   state flow, or interaction across at least three meaningful elements.
6. Run `grilling` one decision at a time with the boundary, evidence, current
   docs, absences, candidates, and report path. Explicitly offer to deep-dive
   into any impact card with the user. Use a host-supplied app link when one is
   available; never invent an undocumented route. The copyable prompt remains
   required. A selected deep dive may regenerate the report before selection.
7. Present the detailed summary:
   - findings under every lens, including no-finding results;
   - lifecycle and selected surface profiles, exclusive critical-path ranking,
     masked or overlapping work, ground-level build/test/tool timings, aggregate
     resource totals, evidence provenance, and confidence gaps;
   - the HTML report path and its key impact cards, Before/After states, and
     available deep dives;
   - exact proposed documentation additions, replacements, or removals by file;
   - concise ticket summaries with all available action paths;
   - report-only observations, supporting evidence, confidence, and gaps.
8. Obtain exact user selections through `grilling`. More detail or further
   grilling returns to that loop and regenerates the summary.
9. Apply only selected documentation changes, preserving structure and avoiding
   duplication. Run only the selected ticket actions through `create-issue`.
10. Compare the result with the confirmed action set, reread edited sections, and
   run declared documentation checks.
11. Report the HTML path, changed docs, created issue links, report-only
    findings, confidence limits, and observed validation. Keep workstream
    history in chat and remind the user they can request a deep dive by card.

Confirmation does not authorize commits, pushes, PRs, unselected issues, or
other file edits. The original request to run a retrospective is not this final
confirmation.
