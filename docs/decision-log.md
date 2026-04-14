# Decision Log

Chronological record of key architectural and implementation decisions, newest first. Each entry has a date, an area tag, a summary, and links to the PR and/or detailed documentation.

## How to add an entry

```markdown
**YYYY-MM-DD** · `area` — Summary of the decision and why. [#PR](../../pull/N). [detail link](file.md#anchor).
```

- **Date** — When the decision was made
- **Area** — One of: `bytecode`, `interpreter`, `data-structures`, `strings`, `fpc-platform`, `runtime`, `testing`, `build`
- **Summary** — 1–3 sentences explaining the decision and why
- **PR / Detail** — Link to the pull request and/or the `docs/` section where the full rationale lives

---

**2026-03-25** · `bytecode` — Unify heap object hierarchy. `TGocciaValue` now inherits from `TGocciaCompiledHeapObject`, removing the bridge between interpreter and bytecode value models. [bytecode-vm.md § Design Rationale](bytecode-vm.md#design-rationale).

**2026-03-18** · `data-structures` — Custom hash maps over TDictionary. Purpose-built `TOrderedStringMap`, `THashMap`, and `TOrderedMap` replace `TDictionary` on hot paths with 4–6× faster inserts at typical sizes. [#66](../../pull/66). [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**2026-03-11** · `strings` — TStringBuffer over TStringBuilder. Both `TUnicodeStringBuilder` and `TAnsiStringBuilder` trigger a 750× slowdown from FPC's heap manager without preallocation. `TStringBuffer` (advanced record with doubling growth) is ~2× faster even with preallocation. [#65](../../pull/65). [spikes/fpc-string-performance.md](spikes/fpc-string-performance.md).

**2026-03-08** · `interpreter` — VMT dispatch on AST nodes. Expression and statement evaluation uses virtual dispatch instead of `if ... is` type check chains. Eliminates `TObject.InheritsFrom` overhead (18.4% of interpreted instructions in callgrind profiling). [#51](../../pull/51). [core-patterns.md § Virtual Dispatch Value System](core-patterns.md#virtual-dispatch-value-system).

**2026-03-08** · `interpreter` — `TGocciaControlFlow` for break/return. `break` and `return` use result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions, eliminating `FPC_SETJMP` overhead from the interpreter's hot path. [#45](../../pull/45). [interpreter.md § Error Handling Strategy](interpreter.md#error-handling-strategy).

**2026-03-05** · `bytecode` — Feature parity between bytecode and interpreter. The bytecode backend passes the full JavaScript test suite. [#39](../../pull/39). [bytecode-vm.md](bytecode-vm.md).

**2026-02-20** · `strings` — No string interning. Dictionary-based string interning was benchmarked at −4% across 172 benchmarks. FPC's COW semantics make allocation effectively free; the hash + lookup cost exceeds it. [core-patterns.md § String Interning](core-patterns.md#string-interning--attempted-and-rejected).

**2026-02-16** · `interpreter` — Eliminate global mutable state. All runtime state flows through `TGocciaEvaluationContext`, the scope chain, and value objects. The `GlobalEvaluationContext` mutable global was removed. [interpreter.md § Pure Evaluator Functions](interpreter.md#pure-evaluator-functions).

**2026-02-16** · `interpreter` — `CreateChild` factory for scopes. Direct `TGocciaScope.Create` replaced with `ParentScope.CreateChild(kind)` to ensure proper parent linkage and callback propagation. [interpreter.md § Scope Chain Design](interpreter.md#scope-chain-design).

---

The following decisions predate the PR workflow and do not have associated PRs or precise dates:

**Early** · `bytecode` — Fold the old Souffle VM into GocciaScript proper. The bytecode backend is no longer a language-agnostic subsystem — it executes directly on `TGocciaValue` with Goccia-owned opcodes. [bytecode-vm.md § Design Rationale](bytecode-vm.md#design-rationale).

**Early** · `bytecode` — Register-based bytecode. The VM uses a register machine because it reduces instruction count and avoids redundant operand shuffling compared with a stack VM. [bytecode-vm.md § Why Register-Based](bytecode-vm.md#why-register-based).

**Early** · `bytecode` — Compiler-side desugaring as default. Nullish coalescing, template literals, and object spread compile into existing instruction sequences rather than expanding the opcode surface. [bytecode-vm.md § Compiler-Side Desugaring](bytecode-vm.md#compiler-side-desugaring).

**Early** · `bytecode` — Core vs semantic opcode split follows hotness, not old runtime layering. [bytecode-vm.md § Opcode Layout](bytecode-vm.md#opcode-layout).

**Early** · `runtime` — Singleton special values. `undefined`, `null`, `true`, `false`, `NaN`, `Infinity` are singletons enabling pointer equality instead of type checks. [core-patterns.md § Singleton Special Values](core-patterns.md#singleton-special-values).

**Early** · `runtime` — Number dual representation. Numbers use `Double` + `TGocciaNumberSpecialValue` enum to handle `NaN`, `Infinity`, `-0` correctly. [value-system.md § Number Representation](value-system.md#number-representation).

**Early** · `data-structures` — TOrderedStringMap for scope bindings. A `TScopeMap` with linear scan was attempted and abandoned after profiling showed 2.7× slowdown vs hash-based lookup. [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**Early** · `data-structures` — No TFPDataHashTable. Catastrophic insert performance (400,000 ns/insert vs 50 ns for `TOrderedStringMap`). [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**Early** · `fpc-platform` — Generics have zero runtime cost. Type aliases and multi-level generic inheritance produce byte-identical machine code. [spikes/fpc-generics-performance.md](spikes/fpc-generics-performance.md).

**Early** · `fpc-platform` — Virtual dispatch is constant-time. Depth 1 and depth 5 cost the same ~2.5ns. [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).

**Early** · `fpc-platform` — ObjFPC vs Delphi mode: zero performance impact. Both produce byte-identical machine code. [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).
