---
name: roadmap-review
description: >-
  Reviews a roadmap from fresh project evidence and produces a verified,
  throughput-anchored version plan, with execution gated on confirmation. Use
  when reviewing a roadmap, planning releases, or sequencing a backlog.
license: Unlicense OR MIT
compatibility: >-
  Requires the GitHub CLI (gh) authenticated to the target repository and
  network access. The gates are forge-agnostic but the default data pulls are
  GitHub/gh. A CI-published progress metric (conformance/coverage/benchmark) is
  used when present but is optional.
---

# Roadmap review

## Instructions

Review a project's roadmap and produce a grounded, parallelized, throughput-anchored plan. The skill's whole value is that **every claim is grounded in fresh data and verified against the source** — never memory, estimates, or stale knowledge. Analysis and planning are the core deliverable; creating milestones and issues is an optional, confirmation-gated final phase.

Work in six phases: **Ground → Assess → Measure velocity → Verify against source → Plan → (optional) Execute.** Verify runs *before* Plan on purpose — proposing work that already exists is the most common failure this skill prevents.

Defer the general engineering discipline (ground-in-reality, the After-Correction Rule, recommend-and-wait under uncertainty) to `software-engineering-excellence`; this skill encodes only the four roadmap-specific gates below.

### Non-negotiable gates

#### GATE A — Ground every claim in fresh data

Every quantitative or status claim must come from data pulled *this session* — never from memory, training data, or a previous session's numbers; re-pull if a number is reused across a long session. If something genuinely cannot be measured, say so explicitly — never substitute an estimate for a measurement.

- **Core forge sources:** open issues, releases, milestones, and merged-PR
  history. Pull each source when present and state material absences.
- **Project direction sources** (best effort; may be absent): `VISION.md` and
  dedicated roadmap/product documentation. When unavailable, record the
  absence, continue with lower confidence, and do not infer scope, quality
  objectives, or architectural boundaries unsupported by the available data.
- **Best-effort source** (auto-detect; may be absent): a domain progress metric published to CI artifacts — conformance, coverage, or benchmark results. If absent, state that and proceed without it; never fabricate one.

#### GATE B — Derive timelines from measured rates, applied literally

Never pad. An estimate is a measured rate applied to a counted backlog, with a stated basis and confidence.

- **Throughput**: the merged-PR rate over a **90-day** window.
- **Lead time**: **issue-created → PR-merged** where the PR links an issue (the fuller signal); fall back to **PR-opened → merged** only when no issue exists, and flag that as a likely **outlier that understates lead time** (un-tracked work skews small and quick).
- **Exclude triage**: issues opened and closed **without an implementation** (invalid / duplicate / wontfix / answered / already-fixed — no linked PR or code change) are not delivery. Exclude them from throughput and lead time, and factor the historical no-implementation close rate into backlog sizing.
- **Domain burndown**: when a domain metric exists, **measure its slope from spaced historical CI artifacts** (download the metric artifact from several past runs and diff the counts) — do not estimate the slope.
- **Allocation ≠ capacity**: per-label or per-area rates show where effort *went*, not the ceiling — a focused milestone can draw most of total throughput.

#### GATE C — Verify before you propose or characterize

Before proposing any feature or work item, classify it against the **current source** as **Done / Partial / Absent** with file:line evidence — propose only Absent or Partial. Confirm characterizations against code, tests, or primary specifications rather than memory or positioning. When subagents are available and the evidence surface splits into independent areas, fan out verification and synthesize it before planning.

#### GATE D — Never mutate the forge without confirmation

The Execute phase is opt-in. Never create or re-theme milestones, file issues, set due-dates, or relabel without explicit confirmation of the **finalized** plan. Every issue created meets `/create-issue` quality — delegate to it.

### Defer to sibling skills

- **`software-engineering-excellence`** — the general engineering bar (ground-in-reality, After-Correction Rule, recommend-and-wait). Not duplicated here.
- **`/create-issue`** — all issue creation in the Execute phase (mechanics and body quality).
- **`/create-release`** — this skill *recommends whether and when to cut the next release* but never cuts it; hand off to `/create-release`.
- **`project-structure`** — locating `VISION.md` / docs when present and the milestone & changelog conventions.
- **`git-workflow`** — any branch/PR mechanics during execution.

### Steps

1. **Ground** (GATE A). Pull repo metadata, open issues by milestone and label, releases with dates, milestones, and merged-PR history. Locate and read `VISION.md` / dedicated roadmap or product docs when present for scope (in/out), the quality/conformance objective, and architectural boundaries. When they are absent, record that fact, proceed with lower confidence, and restrict conclusions to what the available evidence supports. Auto-detect a domain progress metric and its CI artifacts. Reconcile the open-issue count against the forge's number (it may include PRs).

2. **Assess current state.** Time since the last release and the historical cadence; the volume of merged-but-unreleased work; **milestone-on-paper vs. work-in-commits drift** (what is actually being worked vs. what the milestones claim); the stated scope and non-goals. Produce a **release-cadence recommendation**: "N days since the last release with M unreleased commits → cut now / wait," plus a suggested cadence.

3. **Measure velocity** (GATE B). Throughput over 90 days; lead time (issue→merge, with the PR→merge fallback caveat and triage exclusion); the domain slope from historical artifacts; and the allocation-vs-capacity read. Attach a basis and confidence to every number.

4. **Verify against source** (GATE C). For every candidate feature or characterization, check the actual code/tests/spec; classify Done / Partial / Absent with evidence; drop anything already shipped. When subagents are available and the work splits cleanly, fan out independent evidence areas and synthesize them before planning.

5. **Plan.** Produce the report (see Output). Group the verified work into **independent tracks that can progress in parallel**, using the project's own architectural seams as the axis (e.g. an engine/runtime split). Size each epic from counted work, sequence into **themed releases**, and anchor each release to the measured rates. Call out cross-track dependencies and the longest pole. Surface the **decisions that are genuinely the human's** (scope cuts, policy forks) with a recommendation each — then stop for those decisions; do not pre-decide them.

6. **Execute (optional, GATE D).** Only after the plan is confirmed and the human opts in. Ask whether the project uses **milestones**: if yes, default to **milestone → track parent issue → sub-issues**, with due-dates from the timeline; if not (smaller projects), use **track parent issues + labels** and no milestones. Create every issue through `/create-issue`. Report exactly what was created.

### Output

Default to an **in-chat markdown report**; offer to also write it to **`ROADMAP-YYMMDD.md`** (date-stamped with the run date, e.g. `ROADMAP-260626.md`) when the user wants a file. Required sections:

1. **Current state & drift** — cadence, unreleased work, milestone-vs-reality drift, scope, and the release-cadence recommendation.
2. **Measured velocity** — throughput, lead time, and the domain slope; each with its basis and confidence.
3. **Versioned plan** — themed releases, each a sized backlog grouped into independent parallel tracks, with cross-track dependencies marked.
4. **Throughput-anchored timeline** — per-release estimate and confidence, longest pole identified, rendered as a **mermaid `gantt`** diagram (portable; best-effort — omit only if mermaid is unavailable).
5. **Open decisions for the human** — each with a recommendation.

### Notes

- **No CI metric:** skip the domain-slope step, state it is unavailable, and size domain work from issue counts + lead time instead — flagged as lower-confidence.
- **New repo (few issues/releases):** fall back to commit history for cadence and throughput, and say the signal is thin.
- **Adapt the data pulls to the forge** — the defaults are GitHub/`gh`; the gates are forge-agnostic.
- **A report is a snapshot** of the data at run time; re-pull before acting on an old one.
