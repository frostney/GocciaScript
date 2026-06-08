---
name: gocciascript-issue-validation
description: Validate GocciaScript engine issues against the project-specific test262 harness. Use alongside implement-issue for GocciaScript issues that mention test262, ECMA-262/ECMA-402 conformance, Intl, or parser compatibility flags.
---

# GocciaScript Issue Validation

Project-specific validation rules for GocciaScript issues. Use this skill with the generic `implement-issue` workflow when the issue references test262, ECMAScript/ECMA-402 behavior, or compatibility flags.

## Spec Lookup

For ECMA-262 or ECMA-402 semantics, use the pinned `tc39-mcp@0.4.0` server from the project MCP config under `.agents/mcp/` when it is available:

1. Use `spec.search` when the exact clause id is unknown.
2. Use `clause.get` for the relevant clause before deciding expected behavior.
3. Use `spec.crossrefs` when behavior depends on abstract operations or cross-spec references.
4. Use `spec.diff` or `spec.history` when the issue may involve recent prose drift.
5. Use `test262.search` to map a clause id or feature area to conformance tests.
6. Use `test262.get` to inspect a specific conformance test when the MCP server returns one.
7. Use `proposal.list` or `proposal.get` for proposal-stage features.

Record the spec, edition, clause id, section number, and snapshot SHA when the MCP response provides one. If `tc39-mcp` is not available, fall back to the official TC39 sources (`tc39.es/ecma262`, `tc39.es/ecma402`) rather than guessing.

## Test262 Validation

When an issue references test262 files, patterns, or categories:

1. Read `docs/test262.md` before running the reproduction.
2. Use `scripts/run_test262_suite.ts` with `GocciaScriptLoaderBare`; do not run stock test262 files directly through `GocciaScriptLoader`, `GocciaTestRunner`, or hand-built wrappers.
3. Use the pinned test262 SHA from `.github/workflows/pr.yml` or `.github/workflows/ci.yml` unless the issue explicitly targets another SHA.
4. Build the bare loader from a clean artifact state first:

```bash
./build.pas --clean loaderbare
```

Use the clean form after merges, branch switches, PR syncs, generated
resource changes, or any unexplained FPC/resource compiler failure. A plain
`./build.pas loaderbare` is only appropriate for tight iteration after a
known-clean build.

5. Run exact issue paths through the project runner, for example:

```bash
bun scripts/run_test262_suite.ts \
  --suite-dir /path/to/test262-suite \
  --categories intl402 \
  --filter 'intl402/NumberFormat/prototype/format/example.js' \
  --mode bytecode \
  --jobs 1 \
  --verbose
```

The runner supplies the compatibility flags, harness substitutions, non-strict script handling, timeout/memory limits, pinned harness behavior, and PASS/FAIL/WRAPPER_INFRA classification used by CI. Direct loader experiments are only supplemental after the project runner result is known.
