# Decision Log

Chronological record of key architectural and implementation decisions. Each entry gives a 1–3 sentence summary with links to the detailed documentation.

## Interpreter & Evaluator

**Pure evaluator functions.** The evaluator is pure — same AST node + context always produces the same result. State changes happen through scope and value objects, never evaluator-internal state. See [design-decisions.md § Pure Evaluator Functions](design-decisions.md#pure-evaluator-functions).

**Virtual dispatch value system.** Property access, `IsPrimitive`, and `IsCallable` use VMT dispatch on `TGocciaValue` instead of `is` type check chains. Eliminates multi-way branching at every call site. See [design-decisions.md § Virtual Dispatch Value System](design-decisions.md#virtual-dispatch-value-system).

**`TGocciaControlFlow` for break/return.** `break` and `return` use result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions, eliminating `FPC_SETJMP` overhead from the interpreter's hot path. See [design-decisions.md § Error Handling Strategy](design-decisions.md#error-handling-strategy).

**Singleton special values.** `undefined`, `null`, `true`, `false`, `NaN`, `Infinity` are singletons enabling pointer equality instead of type checks. See [design-decisions.md § Singleton Special Values](design-decisions.md#singleton-special-values).

**Number dual representation.** Numbers use `Double` + `TGocciaNumberSpecialValue` enum to handle `NaN`, `Infinity`, `-0` correctly without IEEE 754 comparison pitfalls. See [design-decisions.md § Number Representation](design-decisions.md#number-representation).

## Souffle VM

**Two-tier ISA.** Tier 1 (0–127) has universal semantics inlined in the dispatch loop; Tier 2 (128–255) dispatches through `TSouffleRuntimeOperations` for language-specific behavior. See [design-decisions.md § Souffle VM](design-decisions.md#souffle-vm-two-tier-isa-with-pluggable-runtime) and [souffle-vm.md](souffle-vm.md).

**Register-based, not stack-based.** Fewer instructions per operation and no redundant stack manipulations, at the cost of wider instruction encoding. See [design-decisions.md § Why Register-Based](design-decisions.md#why-register-based).

**Self-contained value system.** `TSouffleValue` (16 bytes, 6 kinds) is independent of `TGocciaValue` to keep the VM language-agnostic. See [design-decisions.md § Why a Self-Contained Value System](design-decisions.md#why-a-self-contained-value-system).

**16-byte TSouffleValue record.** Reduced from 26 bytes (`string[23]`) to 16 bytes (`string[13]`) after benchmarking 8, 10, 16, and 26-byte variants. 16 bytes is the sweet spot — 13-char inline strings cover most property names while fitting more values per cache line. See [optimization-log.md § TSouffleValue Record Size](optimization-log.md#tsoufflevalue-record-size--16-bytes-pr-96).

**`OP_RT_EXT` over per-feature opcodes.** Language-specific features use a single `OP_RT_EXT` with sub-opcode dispatch instead of dedicated Tier 2 opcodes, keeping the VM language-agnostic. See [design-decisions.md § Why OP_RT_EXT](design-decisions.md#why-op_rt_ext-over-per-feature-opcodes).

**Compiler-side desugaring.** Language features (nullish coalescing, template literals, object spread) are compiled into sequences of generic VM instructions rather than adding specialized opcodes. See [design-decisions.md § Compiler-Side Desugaring](design-decisions.md#compiler-side-desugaring).

## Data Structures

**Custom hash maps over TDictionary.** Purpose-built `TOrderedStringMap`, `THashMap`, and `TOrderedMap` replace `TDictionary` on hot paths with 4–6× faster inserts at typical sizes. See [optimization-log.md § Custom Hash Map Hierarchy](optimization-log.md#custom-hash-map-hierarchy-pr-66) and [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**TOrderedStringMap for scope bindings.** A `TScopeMap` with linear scan was attempted and abandoned after profiling showed 2.7× slowdown vs hash-based lookup. Scope bindings use `TOrderedStringMap<TLexicalBinding>` with recursive parent walking. See [optimization-log.md § Custom Hash Map Hierarchy](optimization-log.md#custom-hash-map-hierarchy-pr-66).

**No TFPDataHashTable.** Catastrophic insert performance due to per-node heap allocation (400,000 ns/insert vs 50 ns for `TOrderedStringMap`). See [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

## Strings

**TStringBuffer over TStringBuilder.** Both `TUnicodeStringBuilder` and `TAnsiStringBuilder` trigger a 750× slowdown from FPC's heap manager without preallocation. `TStringBuffer` (advanced record with doubling growth) is ~2× faster even with preallocation. See [spikes/fpc-string-performance.md](spikes/fpc-string-performance.md).

**No string interning.** Dictionary-based string interning was benchmarked at −4% across 172 benchmarks. FPC's COW semantics make allocation effectively free; the hash + lookup cost exceeds it. See [design-decisions.md § String Interning](design-decisions.md#string-interning--attempted-and-rejected).

## FPC Platform

**Generics have zero runtime cost.** Type aliases, subclasses, and multi-level generic inheritance produce byte-identical machine code. Design for API clarity, not performance. See [spikes/fpc-generics-performance.md](spikes/fpc-generics-performance.md).

**Virtual dispatch is constant-time.** Depth 1 and depth 5 dispatch cost the same ~2.5ns. Avoid `inherited` chains on hot paths (linear scaling), but hierarchy depth is free. See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).

**ObjFPC vs Delphi mode: zero performance impact.** Both modes produce byte-identical machine code; choose based on syntax preference. See [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).
