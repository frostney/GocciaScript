---
name: test262-profile-review
description: Review GocciaScript weekly full-corpus test262 profile artifacts and retained Vercel Blob reports. Use when Codex is asked to inspect uploaded test262 performance/profile reports, compare week-over-week or main-run trends, investigate aggregate/detailed profile data, or turn test262 corpus profiling findings into compiler, bytecode, AST, or parser improvement recommendations. This skill is for reviewing existing reports, not generating or wiring CI reports.
---

# Test262 Profile Review

Use this skill to review already-uploaded full-corpus test262 profile reports.
Do not generate new profiles or change CI unless the user explicitly asks.

## Sources

Prefer the exact GitHub Actions artifact when reviewing a specific run. Use the
Vercel Blob `test262-profiles/` namespace for retained main-run history,
week-over-week comparison, and daily pointers. If both exist for the same run,
check that commit SHA, test262 SHA, mode, and generated timestamp match before
comparing trends.

## Review Order

1. Read the aggregate report first. Confirm provenance, corpus size, mode,
   timeout/memory settings, wrapper-infra count, and whether the run is
   comparable to the baseline.
2. Compare against the previous weekly report and the nearest main-run baseline.
   Separate broad corpus shifts from localized path-group changes.
3. Rank hotspots by actionable signal: opcode share, hot opcode pairs, scalar
   fast-path hit rate, function self-time, allocation count, and affected
   test262 path groups.
4. Open detailed profiles only for the top questions raised by the aggregate:
   a surprising regression, a repeated hot pair, a high-allocation path group,
   or a feature area that moved week over week.
5. Turn findings into recommendations tied to engine ownership areas:
   compiler lowering, bytecode opcodes/superinstructions, AST shape, parser
   compatibility paths, value boxing, property access, or call-frame overhead.

## Output

Lead with the trend and recommendation, then cite the evidence. Include commit
SHA, test262 SHA, report source, compared runs, affected path groups, and the
specific aggregate or detailed profile fields used. Label confidence and call
out likely noise when run settings, corpus SHA, or wrapper-infra counts differ.
