# 0138 - Bytecode is the only way GocciaScript executes, and the engine owns its VM

**Date:** 2026-10-14
**Area:** `engine`, `bytecode VM`, `cli`, `embedding`
**Supersedes:** [ADR 0014](0014-bytecode-and-interpreter-feature-parity.md), [ADR 0015](0015-control-flow-records-for-break-and-return.md) and [ADR 0017](0017-vmt-dispatch-for-ast-evaluation.md); in part [ADR 0068](0068-goccia-sandbox-runner.md) and [ADR 0137](0137-bootstrap-shims-load-on-first-use-through-the-executor.md)
**Related:** [#825](https://github.com/frostney/GocciaScript/issues/825), [#875](https://github.com/frostney/GocciaScript/issues/875), [ADR 0134](0134-direct-eval-compiles-as-a-closure-of-its-call-site.md), [ADR 0136](0136-module-function-declarations-are-compiled-while-linking.md)

## Context

The tree-walk interpreter was GocciaScript's first implementation. The bytecode
VM arrived later and was held at feature parity with it (ADR 0014), and for a
long time the interpreter was the oracle that checked the VM: CI ran the test
suite, the benchmarks and the memory-safety checks in both modes, the
differential lane required both modes to agree, and the fuzz harness and the
es-toolkit validation compared them.

That role ended once bytecode was good enough to stand on its own. Bytecode
became the default for every entry point
([#871](https://github.com/frostney/GocciaScript/issues/871)), and direct eval
(ADR 0134), module function declarations (ADR 0136), the bootstrap shims
(ADR 0137) and the value units' call paths
([#874](https://github.com/frostney/GocciaScript/issues/874)) stopped
depending on the evaluator. What remained was cost without a user: every
feature and fix had to land twice, assertions were weakened until both modes
passed them, and the object model carried bridge code whose only purpose was
to hand classes, generators and scopes between two executors. The
interpreter-only units alone were about 18,000 lines.

## Decision

- **Bytecode only.** Every program, module body, shim, `Function` body and
  eval compiles to Goccia bytecode and runs on the VM. The interpreter, the
  evaluator units, the AST nodes' `Evaluate`/`Execute` methods and the code
  that existed only to bridge the two executors are deleted.
- **`--mode` is removed without an alias in 0.15.0.** As with the 0.14
  removals, `--mode`, with any value or none, and a `"mode"` config key exit
  with status 2 and a message naming the release that removed them. The same
  holds for the options `GocciaScriptLoaderBare` and `GocciaTest262Runner`
  parse by hand. `"mode"` leaves the JSON reports, and the
  `__gocciaTestRunnerMode` global is removed. CI runs one execution leg.
- **The executor is folded into the engine.** `TGocciaEngine` creates, owns and
  frees its `TGocciaVM`. Engine and runtime constructors take no executor,
  settings the executor held (`GlobalBackedTopLevel`) are engine properties,
  and the global scope is `Engine.GlobalScope`.
- **Interpreter-only issues are closed as not planned** when this removal
  merges: [#1275](https://github.com/frostney/GocciaScript/issues/1275),
  [#1157](https://github.com/frostney/GocciaScript/issues/1157),
  [#1389](https://github.com/frostney/GocciaScript/issues/1389),
  [#1403](https://github.com/frostney/GocciaScript/issues/1403),
  [#1508](https://github.com/frostney/GocciaScript/issues/1508),
  [#1615](https://github.com/frostney/GocciaScript/issues/1615), and the
  interpreter halves of [#1273](https://github.com/frostney/GocciaScript/issues/1273)
  and [#1274](https://github.com/frostney/GocciaScript/issues/1274).

## Consequences

- One engine: a feature or fix lands once, and the class, generator and scope
  code no longer carries a second representation. The removal deleted about
  24,700 lines.
- Every binary that links the engine is about 5–6% smaller (571–579 KB per
  release binary when the interpreter was deleted).
- There is no mode-parity oracle any more. The JavaScript suite, test262, and
  the external oracles of the differential lane, Vitest and Bun, remain. A
  behavior only the evaluator had is now simply absent rather than caught by a
  parity check; the deletion surfaced two such gaps, a decorator on a private
  instance method ([#1648](https://github.com/frostney/GocciaScript/issues/1648))
  and the `RangeError` conversion of a native stack overflow.
- Embedders get a compile-time break, not a behavior change: code that creates
  an executor, passes one to an engine or runtime constructor, or reads
  `Engine.Interpreter` stops compiling.
  [Embedding § Upgrading from 0.14](../embedding.md#upgrading-from-014) lists
  the replacements.
- Scripts and configs that pass `--mode` or `"mode"` fail with status 2 until
  they drop it. The website playground still offers a mode toggle, but only
  for a selected release binary whose `--help` lists `--mode`.
- Older ADRs that describe the interpreter, the executor or mode parity stay
  as history. Those this record supersedes carry a note saying so.
