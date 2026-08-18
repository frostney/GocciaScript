# Lifecycle timing analysis

Use available conversation, repository, forge, command, test-runner, browser,
deployment, and Milestone Rush JSONL evidence. Coalesce the same event across
sources by stable identity, timestamp, head, and causal relationship while
retaining provenance. Never manufacture missing start times, durations, usage,
or causal links.

## Canonical lifecycle

Map these high-level phases when they occur: discovery, design, decision waiting,
implementation, local validation, PR handoff, CI, review, remediation, merge,
integrated validation, and retrospective. For each measured span record stable
identity, start, end, parent, blocking dependencies, result, source, and whether
it overlaps or masks other work.

Choose one or more surface profiles from the actual workstream:

- **Web:** delivery timings for dependency install and cache, typecheck, lint,
  bundle or build, static generation, artifact upload, and preview deployment;
  automated-interaction timings for server readiness, browser launch,
  navigation, fixtures, tests, steps, retries, and teardown; and product-runtime
  navigation, server, and user timings. Include LCP, INP, and CLS only when
  already captured or relevant to the shipped claim, and keep them separate
  from delivery duration.
- **Backend or service:** dependency and compile/package work, environment and
  service readiness, request, queue, database, cache, background-job, integration
  test, container, deployment, and health-check timings.
- **Library or SDK:** dependency resolution, compile or typecheck, unit and
  integration suites, packaging, consumer smoke tests, examples, documentation,
  compatibility matrices, and publication validation.
- **CLI or tooling:** compile or package, process startup, argument parsing,
  command resolution, command execution, subprocesses, filesystem or network
  work, focused and end-to-end tests, installer, and invocation-level timings.
- **Native application:** compile, link, sign, package, install, launch,
  interaction, platform test, and distribution validation.
- **Data or infrastructure:** plan, provision, migration, ingestion, transform,
  query, validation, deployment, and rollback timings, using only safe observed
  evidence and keeping external system wait separate.

Select multiple profiles for mixed systems. Use current platform-standard data
already present in the workstream; for web semantics, consult current official
[Navigation Timing](https://www.w3.org/TR/navigation-timing-2/),
[Server Timing](https://www.w3.org/TR/server-timing/),
[Core Web Vitals](https://web.dev/articles/vitals), and the active browser test
runner's official reporter documentation. Standards define measurements, not
performance targets for an unrelated repository.

## Attribution

Build a dependency-aware timeline rather than adding durations blindly.

- Report elapsed wall time and rank phases by exclusive critical-path
  contribution.
- Show waits partly or fully hidden by productive work as masked time. Show
  genuinely concurrent spans as overlap and count their shared interval once in
  elapsed time.
- Keep exclusive coordinator work, worker-active time, worker-result waiting,
  commands and local gates, CI waiting, review execution, review cooldown,
  decision waiting, remediation or rework, and genuine idle time distinct when
  evidence permits.
- Report aggregate runner time, concurrent agent time, tool calls, inferences,
  input/cached/uncached/output/reasoning tokens, and compactions separately.
  Aggregate consumption may exceed elapsed time and is not lead time.
- Use explicit `decision_requested` and `decision_resolved` events for decision
  waiting. Do not infer it from a conversation gap.

For missing or contradictory span relationships, provide a range or mark the
attribution unavailable. State whether each conclusion is ledger-backed,
forge-reconciled, log-derived, or approximate.

## Retrospective de-duplication

- **Work:** identify repeated investigations, questions, decisions, findings,
  remediation, reruns, and handoffs within the workstream.
- **Evidence:** count one underlying event once when the ledger, forge, CI, and
  logs expose it in multiple forms; retain all source references.
- **Output:** merge lessons with the same cause, impact, and proposed action;
  preserve provenance and reconcile conflicts.

Implementation duplication may be reported when it directly caused workstream
friction. Wider repository discovery belongs to `codebase-audit`. If the
workstream already contains SEO, AI-assisted discovery, or other discoverability
evidence, include its delivery impact; do not start a fresh discoverability
audit from the retro.
