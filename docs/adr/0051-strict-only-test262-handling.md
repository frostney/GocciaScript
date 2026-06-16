# Strict-only test262 handling

**Date:** 2026-05-17
**Area:** `testing`
**Pull Request:** [#645](https://github.com/frostney/GocciaScript/pull/645)

The test262 runner now injects a `"use strict"` directive for `onlyStrict` tests while still passing `--compat-non-strict-mode` to Script tests. This lets strict-only coverage exercise strict `arguments`, `with`, assignment, `delete`, and `this` semantics without losing the parser/runtime support needed for non-strict Script tests. [test262.md](../test262.md#strict-mode).
