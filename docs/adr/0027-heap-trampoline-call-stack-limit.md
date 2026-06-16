# Heap trampoline call stack limit

**Date:** 2026-04-20
**Area:** `bytecode`
**Pull Request:** [#356](https://github.com/frostney/GocciaScript/pull/356)

Call stack depth limit with heap-resident trampoline. Added a configurable call depth limit (`--stack-size=N`, default 3 500) that throws `RangeError` when exceeded. The bytecode VM dispatches bytecode-to-bytecode calls iteratively via an explicit frame stack (`FFrameStack`) on the heap rather than recursing on the Pascal call stack. V8/JSC check the native stack pointer against a guard address and are bounded by the OS thread stack size; this implementation uses a heap-allocated frame array and a counter, which are independent concepts — the depth limit is a runtime safety guard, the trampoline is an implementation detail of call dispatch, and neither is related to JIT compilation. The interpreter mode retains Pascal recursion with the depth check as a guard. [bytecode-vm.md](../bytecode-vm.md). [embedding.md § Call Stack Depth Limit](../embedding.md#call-stack-depth-limit).
