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
daily pointers for retained main-run history and window comparison.

- `test262-profile` artifact / `test262-profiles/` Blob namespace: full-corpus
  conformance-profile runs. Check commit SHA, test262 SHA, mode, corpus size,
  timeout/memory settings, and wrapper-infra counts before comparing trends.
  The `run` block carries `durationSeconds`, `passed`, `failed`, `timeouts`,
  `totalRun`, and `test262Sha`; `pathBreakdown[]` carries per-path
  `totalOpcodes`/`allocations`/`functionSelfTimeNs`.
- `benchmark-profile` artifact / `benchmark-profiles/` Blob namespace:
  deterministic bytecode benchmark profiles. Check commit SHA, deterministic
  mode, profile count, benchmark file set, and whether the normal benchmark
  baseline for the same run moved in the same direction. Per-file data is under
  `pathBreakdown[]` (`totalOpcodes`, `functionSelfTimeNs`); benchmark
  deterministic mode records `allocations` as `0` (not tracked here — use
  test262 for allocation trends).

If both artifact and Blob payloads exist for the same run, confirm commit SHA,
mode, generated timestamp, and profile count match before using them together.

Profile artifacts are retained ~30 days, so review the **whole retained window,
not two runs**. Two adjacent runs cannot separate a real change from runner
noise. Enumerate every run's artifact and build a time series:

```
gh api --paginate 'repos/{owner}/{repo}/actions/artifacts?per_page=100' \
  --jq '.artifacts[] | select(.name=="benchmark-profile" and .expired==false)
        | [.created_at, (.workflow_run.id|tostring), .workflow_run.head_sha] | @tsv'
```

`benchmark-profile` is tiny (~60 KB) — download the full window. `test262-profile`
is large (~90 MB each) — extract only `*-aggregate.json` and discard the details
unless a specific run warrants a deep dive. Map each commit to its PR
(`git log -1 --format=%s <sha>`) so any movement is attributable.

## Review Order

1. Read the aggregate report first. Confirm provenance, run shape, profile
   count, and comparability to the baseline.
2. Build the full-window series (see Sources) and correlate each movement with
   the PR that landed at that commit. Separate broad VM shifts from localized
   path/file changes. Do not draw conclusions from a two-run comparison.
3. Rank hotspots by actionable signal: opcode share, hot opcode pairs, scalar
   fast-path hit rate, function self-time, allocation count, and affected
   test262 path groups or benchmark files. Read metric reliability (below)
   before trusting any delta.
4. Open detailed profiles only for the top questions raised by the aggregate:
   a surprising regression, a repeated hot pair, a high-allocation path group,
   a hot benchmark file, or a feature area that moved across the window.
5. Turn findings into recommendations tied to engine ownership areas:
   compiler lowering, bytecode opcodes/superinstructions, AST shape, parser
   compatibility paths, value boxing, property access, or call-frame overhead.

## Metric reliability

Not every field is trustworthy. Separate deterministic signal from noise before
attributing anything to a PR.

- **Benchmark opcode/pair counts are deterministic** (noise-free). Any per-file
  `totalOpcodes` step is a real lowering or coverage change — attribute it to the
  exact commit between data points. This is the reliable regression detector.
- **Benchmark self-time is runner noise.** Single-run timing swings materially
  (order ~10%); a two-run timing delta is almost always noise, not a regression.
  Normalize each file's self-time by that run's total (its share of work) and
  look for a *sustained* share shift across many runs, not a single jump.
- **test262 conformance counts are deterministic** (`passed`/`failed`/`timeouts`)
  and attributable per PR — the trustworthy conformance signal.
- **test262 per-path opcode/alloc/self-time counts are NOT deterministic.** A
  handful of tests run to the per-test deadline and truncate at a
  runner-speed-dependent point, so their counts vary with runner load, not with
  the change. Verify before trusting a per-path delta: correlate the suspect
  path's opcodes with the run's `durationSeconds` across the window — a strong
  negative correlation means truncation, not a PR effect. Trust only conformance
  counts and *sustained, isolated, target-path* level-shifts held across many
  commits; never a single adjacent-commit per-path delta.
- **`durationSeconds` is the wall-clock signal** but is itself noisy (order
  ~15%): only a large move or one confirmed across multiple runs is real.
- **Both families measure JS-level execution and are largely blind to native /
  boot / parser / allocator optimizations.** Such PRs move `durationSeconds` (and
  the same-runner benchmark-timing job) while leaving opcode counts and
  JS-function self-time essentially flat. Absence of an opcode/self-time change
  is not evidence a native perf PR did nothing — check `durationSeconds`.

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
