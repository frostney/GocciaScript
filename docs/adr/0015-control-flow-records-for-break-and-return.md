# Control flow records for break and return

**Date:** 2026-03-08
**Area:** `interpreter`
**Pull Request:** [#45](https://github.com/frostney/GocciaScript/pull/45)

`TGocciaControlFlow` for break/return. `break` and `return` use result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions, eliminating `FPC_SETJMP` overhead from the interpreter's hot path. [#45](https://github.com/frostney/GocciaScript/pull/45). [interpreter.md § Error Handling Strategy](../interpreter.md#error-handling-strategy).
