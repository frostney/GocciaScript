# Decision Log

Chronological record of key architectural and implementation decisions, newest first. Each entry has a date, an area tag, a summary, and links to the PR and/or detailed documentation.

## How to add an entry

```markdown
**YYYY-MM-DD** · `area` — Summary of the decision and why. [#PR](https://github.com/frostney/GocciaScript/pull/N). [detail link](file.md#anchor).
```

- **Date** — When the decision was made
- **Area** — One of: `bytecode`, `interpreter`, `parser`, `data-structures`, `strings`, `fpc-platform`, `runtime`, `testing`, `build`
- **Summary** — 1–3 sentences explaining the decision and why
- **PR / Detail** — Link to the pull request and/or the `docs/` section where the full rationale lives

> **Entries are immutable.** Each entry records what was decided at that point in time. Do not retroactively update entries to match the current implementation. If the implementation changes, add a new entry. Links may be updated if targets are renamed.

---

**2026-04-12** · `bytecode` · [#289](https://github.com/frostney/GocciaScript/pull/289) — Source Maps (v3). Source map generation for bytecode compilation and preprocessors (JSX transformer). Maps compiled bytecode instructions back to original source locations for debugging. [bytecode-vm.md § Design Rationale](bytecode-vm.md#design-rationale).

**2026-04-11** · `runtime` · [#276](https://github.com/frostney/GocciaScript/pull/276) — `Goccia.build` platform metadata. Deno.build-compatible shape exposing `os`, `arch`, and `target` for runtime platform detection. [built-ins.md § Global Constants](built-ins.md#global-constants-functions-and-error-constructors-gocciabuiltinsglobalspas).

**2026-04-09** · `bytecode` · [#237](https://github.com/frostney/GocciaScript/pull/237) — Bytecode VM profiling. Opcode histograms, pair frequency, function self-time, allocation tracking, and flame graph export via `--profile` flag. [profiling.md](profiling.md).

**2026-04-08** · `parser` · [#220](https://github.com/frostney/GocciaScript/pull/220) — Opt-in automatic semicolon insertion. Added ASI behind a `--asi` CLI flag. Previously against the project philosophy (GocciaScript requires explicit semicolons), added for compatibility with code written for standard ECMAScript. Off by default; does not change the language subset, only relaxes the semicolon requirement. [language.md § Automatic Semicolon Insertion](language.md#automatic-semicolon-insertion).

**2026-03-23 → 2026-04-10** · `bytecode` · [#107](https://github.com/frostney/GocciaScript/pull/107) [#109](https://github.com/frostney/GocciaScript/pull/109) [#110](https://github.com/frostney/GocciaScript/pull/110) [#136](https://github.com/frostney/GocciaScript/pull/136) [#137](https://github.com/frostney/GocciaScript/pull/137) [#254](https://github.com/frostney/GocciaScript/pull/254) — VM unification. Fold the separate Souffle VM into a GocciaScript-specific bytecode VM: eliminate interpreter bridges for modules (#107) and async/await (#110), add native-value-backed Map/Set (#109), refactor built-in object model registration (#136), fold the VM directly (#137), and extract the GC into a standalone `Goccia.GarbageCollector` unit (#254). [bytecode-vm.md § Design Rationale](bytecode-vm.md#design-rationale). [garbage-collector.md](garbage-collector.md).

**2026-03-25** · `bytecode` — Unify heap object hierarchy. `TGocciaValue` now inherits from `TGocciaCompiledHeapObject`, removing the bridge between interpreter and bytecode value models. [bytecode-vm.md § Design Rationale](bytecode-vm.md#design-rationale).

**2026-03-18** · `data-structures` — Custom hash maps over TDictionary. Purpose-built `TOrderedStringMap`, `THashMap`, and `TOrderedMap` replace `TDictionary` on hot paths with 4–6× faster inserts at typical sizes. [#66](https://github.com/frostney/GocciaScript/pull/66). [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**2026-03-11** · `strings` — TStringBuffer over TStringBuilder. Both `TUnicodeStringBuilder` and `TAnsiStringBuilder` trigger a 750× slowdown from FPC's heap manager without preallocation. `TStringBuffer` (advanced record with doubling growth) is ~2× faster even with preallocation. [#65](https://github.com/frostney/GocciaScript/pull/65). [spikes/fpc-string-performance.md](spikes/fpc-string-performance.md).

**2026-03-08** · `interpreter` — VMT dispatch on AST nodes. Expression and statement evaluation uses virtual dispatch instead of `if ... is` type check chains. Eliminates `TObject.InheritsFrom` overhead (18.4% of interpreted instructions in callgrind profiling). [#51](https://github.com/frostney/GocciaScript/pull/51). [core-patterns.md § Virtual Dispatch Value System](core-patterns.md#virtual-dispatch-value-system).

**2026-03-05** · `runtime` — Switch number representation from enum to IEEE 754. Removed `TGocciaNumberSpecialValue` enum and `FSpecialValue` field from `TGocciaNumberLiteralValue`. Numbers now store a single `Double` using standard IEEE 754 bit patterns for NaN, Infinity, and -0. Enabled by masking all FPU exceptions via `SetExceptionMask` so that operations like `0.0 / 0.0` produce IEEE 754 NaN instead of raising a Pascal exception. The `IsNaN`/`IsInfinity`/`IsNegativeZero` property accessors delegate to `Math.IsNaN`/`Math.IsInfinite` and an endian-neutral sign-bit check. The engine saves and restores the previous exception mask on creation/destruction. [#39](https://github.com/frostney/GocciaScript/pull/39). [value-system.md § Number Representation](value-system.md#number-representation).

**2026-03-08** · `interpreter` — `TGocciaControlFlow` for break/return. `break` and `return` use result records (`cfkBreak`, `cfkReturn`) instead of Pascal exceptions, eliminating `FPC_SETJMP` overhead from the interpreter's hot path. [#45](https://github.com/frostney/GocciaScript/pull/45). [interpreter.md § Error Handling Strategy](interpreter.md#error-handling-strategy).

**2026-03-05** · `bytecode` — Feature parity between bytecode and interpreter. The bytecode backend passes the full JavaScript test suite. [#39](https://github.com/frostney/GocciaScript/pull/39). [bytecode-vm.md](bytecode-vm.md).

**2026-02-20** · `strings` — No string interning. Dictionary-based string interning was benchmarked at −4% across 172 benchmarks. FPC's COW semantics make allocation effectively free; the hash + lookup cost exceeds it. [core-patterns.md § String Interning](core-patterns.md#string-interning--attempted-and-rejected).

**2026-02-16** · `interpreter` — Eliminate global mutable state. All runtime state flows through `TGocciaEvaluationContext`, the scope chain, and value objects. The `GlobalEvaluationContext` mutable global was removed. [interpreter.md § Pure Evaluator Functions](interpreter.md#pure-evaluator-functions).

**2026-02-16** · `interpreter` — `CreateChild` factory for scopes. Direct `TGocciaScope.Create` replaced with `ParentScope.CreateChild(kind)` to ensure proper parent linkage and callback propagation. [interpreter.md § Scope Chain Design](interpreter.md#scope-chain-design).

**2026-03-18** · `data-structures` — TOrderedStringMap for scope bindings. A `TScopeMap` with linear scan was attempted and abandoned after profiling showed 2.7× slowdown vs hash-based lookup. [#66](https://github.com/frostney/GocciaScript/pull/66). [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**2026-03-18** · `data-structures` — No TFPDataHashTable. Catastrophic insert performance (400,000 ns/insert vs 50 ns for `TOrderedStringMap`). [#66](https://github.com/frostney/GocciaScript/pull/66). [spikes/fpc-hashmap-performance.md](spikes/fpc-hashmap-performance.md).

**2026-03-18** · `fpc-platform` — Generics have zero runtime cost. Type aliases and multi-level generic inheritance produce byte-identical machine code. [#105](https://github.com/frostney/GocciaScript/pull/105). [spikes/fpc-generics-performance.md](spikes/fpc-generics-performance.md).

**2026-03-18** · `fpc-platform` — Virtual dispatch is constant-time. Depth 1 and depth 5 cost the same ~2.5ns. [#105](https://github.com/frostney/GocciaScript/pull/105). [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).

**2026-03-18** · `fpc-platform` — ObjFPC vs Delphi mode: zero performance impact. Both produce byte-identical machine code. [#105](https://github.com/frostney/GocciaScript/pull/105). [spikes/fpc-dispatch-performance.md](spikes/fpc-dispatch-performance.md).

**2026-02-26** · `bytecode` — Register-based bytecode. The VM uses a register machine because it reduces instruction count and avoids redundant operand shuffling compared with a stack VM. [#38](https://github.com/frostney/GocciaScript/pull/38). [bytecode-vm.md § Why Register-Based](bytecode-vm.md#why-register-based).

**2026-02-26** · `bytecode` — Compiler-side desugaring as default. Nullish coalescing, template literals, and object spread compile into existing instruction sequences rather than expanding the opcode surface. [#38](https://github.com/frostney/GocciaScript/pull/38). [bytecode-vm.md § Compiler-Side Desugaring](bytecode-vm.md#compiler-side-desugaring).

**2026-02-26** · `bytecode` — Core vs semantic opcode split follows hotness, not old runtime layering. [#38](https://github.com/frostney/GocciaScript/pull/38). [bytecode-vm.md § Opcode Layout](bytecode-vm.md#opcode-layout).

**2025-06-25** · `runtime` — Singleton special values. `undefined`, `null`, `true`, `false`, `NaN`, `Infinity` are singletons enabling pointer equality instead of type checks. [core-patterns.md § Singleton Special Values](core-patterns.md#singleton-special-values).

**2025-06-25** · `runtime` — Number dual representation. Numbers use `Double` + `TGocciaNumberSpecialValue` enum to handle `NaN`, `Infinity`, `-0` correctly. [value-system.md § Number Representation](value-system.md#number-representation).
