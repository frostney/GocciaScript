---
name: profile-report-review
description: Review GocciaScript retained VM profile reports from test262 and benchmark main-CI runs. Use when Codex is asked to inspect uploaded test262 or benchmark performance/profile artifacts, compare week-over-week or main-run trends, investigate aggregate/detailed profile data, or turn profiling findings into compiler, bytecode, AST, parser, allocation, property-access, or call-frame improvement recommendations. This skill is for reviewing existing reports, not generating or wiring CI reports.
---

# Profile Report Review

Use this skill to review already-uploaded GocciaScript VM profile reports from
main CI. Do not generate new profiles or change CI unless the user explicitly
asks.

## Sources

Prefer the exact GitHub Actions artifact when reviewing a specific run. Use Blob
daily pointers for retained main-run history and week-over-week comparison.

- `test262-profile` artifact / `test262-profiles/` Blob namespace: full-corpus
  conformance-profile runs. Check commit SHA, test262 SHA, mode, corpus size,
  timeout/memory settings, and wrapper-infra counts before comparing trends.
- `benchmark-profile` artifact / `benchmark-profiles/` Blob namespace:
  deterministic bytecode benchmark profiles. Check commit SHA, deterministic
  mode, profile count, benchmark file set, and whether the normal benchmark
  baseline for the same run moved in the same direction.

If both artifact and Blob payloads exist for the same run, confirm commit SHA,
mode, generated timestamp, and profile count match before using them together.

## Review Order

1. Read the aggregate report first. Confirm provenance, run shape, profile
   count, and comparability to the baseline.
2. Compare against the previous weekly report and nearby main-run baselines.
   Separate broad VM shifts from localized path/file changes.
3. Rank hotspots by actionable signal: opcode share, hot opcode pairs, scalar
   fast-path hit rate, function self-time, allocation count, and affected
   test262 path groups or benchmark files.
4. Open detailed profiles only for the top questions raised by the aggregate:
   a surprising regression, a repeated hot pair, a high-allocation path group,
   a hot benchmark file, or a feature area that moved week over week.
5. Turn findings into recommendations tied to engine ownership areas:
   compiler lowering, bytecode opcodes/superinstructions, AST shape, parser
   compatibility paths, value boxing, property access, or call-frame overhead.

## Interpretation

- Use test262 profiles to find broad conformance-corpus pressure: parser and
  compatibility paths, feature-family cost, and VM behavior across many small
  programs.
- Use benchmark profiles to explain measured benchmark movement: focused hot
  files, opcode mixes, allocation attribution, and candidate optimizations with
  clearer performance intent.
- Prefer changes supported by both report families, but do not require both.
  A test262-only signal can still justify compatibility-path work; a
  benchmark-only signal can still justify a targeted VM optimization.

## Output

Lead with the trend and recommendation, then cite the evidence. Include commit
SHA, report source, compared runs, affected path groups or benchmark files, and
the specific aggregate or detailed profile fields used. Include test262 SHA for
test262 reports. Label confidence and call out likely noise when run settings,
corpus SHA, profile count, wrapper-infra counts, or benchmark file sets differ.
