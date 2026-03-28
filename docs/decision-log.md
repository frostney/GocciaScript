# Decision Log

Chronological record of key architectural and implementation decisions. Each entry gives a 1–3 sentence summary with links to the detailed documentation.

## Bytecode VM Fold-In

**Fold the old Souffle VM into GocciaScript proper.** The bytecode backend is no longer treated as a language-agnostic subsystem. The decision was driven by repeated failed attempts to remove the bridge, constraints on fast-path optimization while preserving generic abstractions, and Win32 memory pressure caused by bridged builtin-heavy execution. The target architecture keeps the interpreter path, but runs bytecode directly on `TGocciaValue`, shared runtime objects, and Goccia-owned bytecode units and opcodes. See [design-decisions.md § Bytecode VM](design-decisions.md#bytecode-vm) and [bytecode-vm.md](bytecode-vm.md).

**Register-based bytecode remains the execution model.** The fold-in changed ownership and runtime semantics, not the core execution shape. The VM still uses a register machine because it reduces instruction count and avoids redundant operand shuffling compared with a stack VM. See [design-decisions.md § Why Register-Based](design-decisions.md#why-register-based).

**Bytecode now operates on `TGocciaValue` directly.** The old standalone bytecode value layer was removed once the VM became Goccia-specific. That simplification removed bridge caches, duplicate object representations, and generic runtime abstractions that were blocking further cleanup and optimization. See [design-decisions.md § Bytecode VM](design-decisions.md#bytecode-vm).

**Dedicated opcodes replace core extension dispatch.** The current VM favors first-class opcodes for core Goccia semantics instead of routing them through a generic extension opcode. That keeps the active opcode surface explicit and lets the compiler and VM evolve together instead of preserving a fake language-neutral boundary. See [design-decisions.md § Bytecode VM](design-decisions.md#bytecode-vm) and [bytecode-vm.md § Opcode Layout](bytecode-vm.md#opcode-layout).

**Compiler-side desugaring remains the default for syntax sugar.** Source-level conveniences such as nullish coalescing, template literals, and object spread are still compiled into ordinary bytecode sequences unless a dedicated opcode is justified by hot-path cost or irreducible semantics. See [design-decisions.md § Compiler-Side Desugaring](design-decisions.md#compiler-side-desugaring).

**Measured optimization findings were folded into the final VM design.** Earlier experiments on record sizing, opcode factoring, and helper abstractions were only kept when they still made sense after the fold-in. The current bytecode architecture keeps the useful results, but not the old subsystem boundaries they were originally attached to.

## Interpreter & Evaluator

**Pure evaluator functions.** The evaluator is pure — same AST node + context always produces the same result. State changes happen through scope and value objects, never evaluator-internal state. See [design-decisions.md § Pure Evaluator Functions](design-decisions.md#pure-evaluator-functions).

**Virtual dispatch value system.** Property access, `IsPrimitive`, and `IsCallable` use VMT dispatch on `TGocciaValue` instead of `is` type check chains. Eliminates multi-way branching at every call site. See [design-decisions.md § Virtual Dispatch Value System](design-decisions.md#virtual-dispatch-value-system).

**`TGocciaControlFlow` for break/return.** `break` and `return` use result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions, eliminating `FPC_SETJMP` overhead from the interpreter's hot path. See [design-decisions.md § Error Handling Strategy](design-decisions.md#error-handling-strategy).

**Singleton special values.** `undefined`, `null`, `true`, `false`, `NaN`, `Infinity` are singletons enabling pointer equality instead of type checks. See [design-decisions.md § Singleton Special Values](design-decisions.md#singleton-special-values).

**Number dual representation.** Numbers use `Double` + `TGocciaNumberSpecialValue` enum to handle `NaN`, `Infinity`, `-0` correctly without IEEE 754 comparison pitfalls. See [design-decisions.md § Number Representation](design-decisions.md#number-representation).

## Data Structures

**Custom hash maps over TDictionary.** Purpose-built `TOrderedStringMap`, `THashMap`, and `TOrderedMap` replace `TDictionary` on hot paths with 4–6× faster inserts at typical sizes. See [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**TOrderedStringMap for scope bindings.** A `TScopeMap` with linear scan was attempted and abandoned after profiling showed 2.7× slowdown vs hash-based lookup. Scope bindings use `TOrderedStringMap<TLexicalBinding>` with recursive parent walking. See [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**No TFPDataHashTable.** Catastrophic insert performance due to per-node heap allocation (400,000 ns/insert vs 50 ns for `TOrderedStringMap`). See [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

## Strings

**TStringBuffer over TStringBuilder.** Both `TUnicodeStringBuilder` and `TAnsiStringBuilder` trigger a 750× slowdown from FPC's heap manager without preallocation. `TStringBuffer` (advanced record with doubling growth) is ~2× faster even with preallocation. See [spikes/fpc-string-performance.md](spikes/fpc-string-performance.md).

**No string interning.** Dictionary-based string interning was benchmarked at −4% across 172 benchmarks. FPC's COW semantics make allocation effectively free; the hash + lookup cost exceeds it. See [design-decisions.md § String Interning](design-decisions.md#string-interning--attempted-and-rejected).

**Delete generic indirection when it blocks real wins.** Performance work that only helps an abstraction layer is not worth preserving once that abstraction is removed. The bytecode fold-in kept register execution and compiler desugaring, but dropped the separate value model, generic extension dispatch, and bridge-heavy runtime split because they were no longer paying for their complexity.

## FPC Platform

**Generics have zero runtime cost.** Type aliases, subclasses, and multi-level generic inheritance produce byte-identical machine code. Design for API clarity, not performance. See [spikes/fpc-generics-performance.md](spikes/fpc-generics-performance.md).

**Virtual dispatch is constant-time.** Depth 1 and depth 5 dispatch cost the same ~2.5ns. Avoid `inherited` chains on hot paths (linear scaling), but hierarchy depth is free. See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).

**ObjFPC vs Delphi mode: zero performance impact.** Both modes produce byte-identical machine code; choose based on syntax preference. See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).
