# Control flow records for break and return

**Date:** 2026-03-08
**Area:** `interpreter`
**Pull Request:** [#45](https://github.com/frostney/GocciaScript/pull/45)

`TGocciaControlFlow` for break/return. `break` and `return` use result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions, eliminating `FPC_SETJMP` overhead from the interpreter's hot path. [#45](https://github.com/frostney/GocciaScript/pull/45). [architecture.md § Error Handling Strategy](../architecture.md#error-handling-strategy).

**Superseded by [ADR 0138](0138-bytecode-is-the-only-way-gocciascript-executes.md):** the control-flow records existed only in the
tree-walk interpreter and were deleted with it in GocciaScript 0.15.0.
