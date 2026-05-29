---
name: gocciascript-issue-validation
description: Validate GocciaScript engine issues against the project-specific test262 harness. Use alongside implement-issue for GocciaScript issues that mention test262, ECMA-262/ECMA-402 conformance, Intl, or parser compatibility flags.
---

# GocciaScript Issue Validation

Project-specific validation rules for GocciaScript issues. Use this skill with the generic `implement-issue` workflow when the issue references test262, ECMAScript/ECMA-402 behavior, or compatibility flags.

## Test262 Validation

When an issue references test262 files, patterns, or categories:

1. Read `docs/test262.md` before running the reproduction.
2. Use `scripts/run_test262_suite.ts` with `GocciaScriptLoaderBare`; do not run stock test262 files directly through `GocciaScriptLoader`, `GocciaTestRunner`, or hand-built wrappers.
3. Use the pinned test262 SHA from `.github/workflows/pr.yml` or `.github/workflows/ci.yml` unless the issue explicitly targets another SHA.
4. Build the bare loader first:

```bash
./build.pas loaderbare
```

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
