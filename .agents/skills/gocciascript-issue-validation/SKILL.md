---
name: gocciascript-issue-validation
description: Validate GocciaScript engine issues against the project-specific test262 harness. Use alongside implement for GocciaScript issues that mention test262, ECMA-262/ECMA-402 conformance, Intl, or parser compatibility flags.
---

# GocciaScript Issue Validation

Project-specific validation rules for GocciaScript issues. Use this skill with the generic `implement` workflow when the issue references test262, ECMAScript/ECMA-402 behavior, or compatibility flags.

## Spec Lookup

For ECMA-262 or ECMA-402 semantics, use the pinned `tc39-mcp@0.6.5` server from the project MCP config under `.agents/mcp/` when it is available. `tc39-mcp` spec-reading tools default to ECMA-262, so pass the spec explicitly for the standard you are checking:

- ECMA-262 language/core built-ins: `spec: "262"`
- ECMA-402 Intl behavior: `spec: "402"`

1. Use `spec.search` when the exact clause id is unknown, for example `spec.search({ spec: "402", query: "Intl.DateTimeFormat" })` for Intl work.
2. Use `clause.get` for the relevant clause before deciding expected behavior, for example `clause.get({ spec: "402", id: "sec-intl.datetimeformat" })`.
3. Use `spec.crossrefs` when behavior depends on abstract operations or cross-spec references. For ECMA-402 algorithms that call ECMA-262 operations, start from `spec: "402"` and set `include_cross_spec: true`.
4. Use `spec.diff` or `spec.history` with the same explicit `spec` when the issue may involve recent prose drift.
5. Use `test262.search` to map a clause id or feature area to conformance tests.
6. Use `test262.get` to inspect a specific conformance test when the MCP server returns one.
7. Use `proposal.list` or `proposal.get` for proposal-stage features.

Record the spec, edition, clause id, section number, and snapshot SHA when the MCP response provides one. If `tc39-mcp` is not available, fall back to the official TC39 sources (`tc39.es/ecma262`, `tc39.es/ecma402`) rather than guessing.

## Test262 Validation

When an issue references test262 files, patterns, or categories:

1. Read `docs/test262.md` before running the reproduction.
2. Reproduce with the native `GocciaTest262Runner`, the runner CI uses. Do not run stock test262 files through `GocciaRunner`, `GocciaTestRunner`, `GocciaScriptLoaderBare`, or hand-built wrappers, and do not use `scripts/run_test262_suite.ts` to run tests: it no longer discovers or executes them.
3. Use the test262 SHA in `scripts/test262-suite-sha.txt` unless the issue explicitly targets another SHA. CI reads the same file, so do not take the pin from the workflows. Check that your checkout is at that commit:

```bash
cat scripts/test262-suite-sha.txt
git -C /path/to/test262-suite rev-parse HEAD
```

If you have no checkout at the pinned SHA, clone into a new, empty path and use
that path for the remaining commands:

```bash
git clone --filter=blob:none --no-checkout https://github.com/tc39/test262 /path/to/test262-suite
git -C /path/to/test262-suite checkout "$(cat scripts/test262-suite-sha.txt)"
```

4. Build the native runner:

```bash
./build.pas test262runner
```

`build.pas` retries a target once from a clean unit directory when FPC
reports stale compiled state, so a build failure that survives that retry is
a source failure. Use `./build.pas --clean test262runner` only to rebuild from
empty unit directories on purpose (see `docs/contributing/tooling.md`
§ Stale FPC Build Artifacts).

5. Run the exact issue paths from the repository root. The runner resolves its
   bundled `$262.js` relative to the working directory (stock harness files
   come from `--suite-dir`), and every test that loads it (all but `raw`
   tests) reports `WRAPPER_INFRA` from anywhere else. `--filter` is a glob
   over test IDs (paths relative to `test/`), `--categories` narrows the
   top-level categories, and `--verbose` prints every result:

```bash
./build/GocciaTest262Runner \
  --suite-dir=/path/to/test262-suite \
  --categories=intl402 \
  --filter='intl402/NumberFormat/prototype/format/example.js' \
  --jobs=1 \
  --verbose \
  --output=.agent/test262-results.json
```

The runner supplies the compatibility flags, host hooks, harness handling,
strict and non-strict runs, timeout and memory limits, and the
PASS/FAIL/WRAPPER_INFRA/TIMEOUT classification that CI reports. An unflagged
Script test reports two results, `[non-strict]` and `[strict]`, and the JSON
report gives each its `mode`; state in the issue which mode fails (see the
strict-mode section of `docs/test262.md`). Pass `--timeout=<duration>` and
`--max-memory=<size>` only to match a specific CI step or to cap a broad
probe. For a wide `--categories` or `--filter` run,
cap the process too, as `AGENTS.md` describes. Direct loader experiments are
only supplemental after the runner result is known.

`scripts/run_test262_suite.ts` keeps only the CI artifact work: `--merge-shards`
combines the shard reports from `GocciaTest262Runner --shard-index/--shard-count`
into the canonical report, and `--comment` renders the PR comment. See the
CI sharding section of `docs/test262.md` before reproducing a shard-level
result.
