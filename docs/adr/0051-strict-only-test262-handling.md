# Strict-only test262 handling

**Date:** 2026-05-17
**Area:** `testing`
**Pull Request:** [#645](https://github.com/frostney/GocciaScript/pull/645)

The test262 runner now injects a `"use strict"` directive for `onlyStrict` tests while still passing `--compat-non-strict-mode` to Script tests. This lets strict-only coverage exercise strict `arguments`, `with`, assignment, `delete`, and `this` semantics without losing the parser/runtime support needed for non-strict Script tests. [test262.md](../test262.md#strict-mode).

**Refined by [ADR 0175](0175-test262-harness-includes-are-separate-non-strict-scripts.md):** the `"use strict"` directive goes in front of the test file only, after the harness Scripts have run non-strict, and every test, not only a Script test, gets `--compat-non-strict-mode`.
