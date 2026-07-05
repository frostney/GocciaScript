# Parser errors for unsupported default syntax

**Date:** 2026-07-05
**Area:** `parser`, `source-pipeline`, `cli`
**Issue:** [#769](https://github.com/frostney/GocciaScript/issues/769)
**Refines:** [ADR 0058](0058-centralized-parser-policy.md)
**Supersedes:** The default warning/recovery behavior described by [ADR 0043](0043-opt-in-traditional-for-loops.md), [ADR 0047](0047-opt-in-loose-equality.md), [ADR 0052](0052-opt-in-while-loops.md), [ADR 0059](0059-opt-in-labels.md), and [ADR 0060](0060-opt-in-for-in-loops.md).

Unsupported or default-disabled parser syntax now fails by default with a parser `SyntaxError`. This applies to parser-recognized syntax that GocciaScript deliberately excludes from the recommended default surface, such as `var`, `function`, loose equality, labels, traditional `for(;;)`, `for...in`, `while`/`do...while`, and script-source `with`.

The recovery behavior remains available through a separate source-pipeline policy switch: `--warning-unsupported-features` on CLI tools, `"warning-unsupported-features": true` in config files, and `TGocciaEngine.WarningUnsupportedFeatures` / `TGocciaSourcePipelineOptions.WarningUnsupportedFeatures` for embedders and manual parse paths. This switch is intentionally not part of `TGocciaCompatibilityFlags`. Compatibility flags such as `--compat-var`, `--compat-function`, `--compat-loose-equality`, `--compat-label`, `--compat-traditional-for-loop`, `--compat-for-in-loop`, `--compat-while-loops`, and `--compat-non-strict-mode` continue to mean "enable the actual semantics"; the warning switch only changes parser diagnostics and recovery.

All runtime surfaces that parse source must honor the same policy: Script Loader, Bare Loader, Test Runner, Benchmark Runner, Bundler, REPL bytecode mode, module loading, bytecode execution, direct eval, dynamic `Function` parsing, and ShadowRealm host hooks. Manual bytecode paths keep the active source-pipeline options in scope while executing compiled modules so nested parses, especially direct eval, inherit the same warning policy.

Module syntax that is now part of the language, including side-effect imports and wildcard re-exports, is outside this gate and remains supported by default. Project style concerns such as "prefer named exports" belong in linting or review policy rather than parser compatibility. Module-source `with` remains a strict-mode `SyntaxError` even when warning recovery is enabled, because modules cannot opt into non-strict script semantics.

Consequences:

- Default parser behavior is fail-fast and actionable: disabled syntax points at the source location and suggests either the modern form or the relevant compatibility flag.
- The old warning/no-op behavior remains available for migrations, diagnostics, and compatibility harnesses that intentionally need to keep scanning after unsupported syntax.
- Tests that exercise warning recovery live under local `goccia.json` files with `"warning-unsupported-features": true`, making the opt-in visible at the fixture boundary.
- Documentation and examples must not describe supported module syntax as parser warnings/no-ops, and runnable examples should avoid disabled syntax unless they explicitly opt into the warning policy.
