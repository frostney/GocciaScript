# Compiler-side desugaring

**Date:** 2026-02-26
**Area:** `bytecode`
**Pull Request:** [#38](https://github.com/frostney/GocciaScript/pull/38)

Compiler-side desugaring as default. Nullish coalescing, template literals, and object spread compile into existing instruction sequences rather than expanding the opcode surface. [#38](https://github.com/frostney/GocciaScript/pull/38). [bytecode-vm.md § Compiler-Side Desugaring](../bytecode-vm.md#compiler-side-desugaring).
