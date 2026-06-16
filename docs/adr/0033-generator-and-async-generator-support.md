# Generator and async generator support

**Date:** 2026-04-27
**Area:** `runtime`

Generator and async generator support. Generator method shorthand (`*method()` and `async *method()`) is now part of the default method syntax, while `function*` and `async function*` remain behind `--compat-function` because they use the compatibility-gated `function` keyword. The interpreter uses resumable generator continuations and the bytecode format reserves a single `OP_YIELD` opcode; `yield*` is represented as delegation logic rather than a second opcode. [language.md § Generators](../language.md#generators).
