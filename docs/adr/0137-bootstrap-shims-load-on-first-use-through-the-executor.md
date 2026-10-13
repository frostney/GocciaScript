# 0137 - Bootstrap shims load on first use through the engine's executor, and a bytecode engine builds no interpreter

**Date:** 2026-10-13
**Area:** `engine`, `shims`, `bytecode VM`, `object model`
**Related:** [ADR 0136](0136-module-function-declarations-are-compiled-while-linking.md), [ADR 0085](0085-defer-annex-b-before-1-0.md)

## Context

The legacy ECMAScript surfaces GocciaScript provides as JavaScript-source
shims (`Date`, `btoa`, `parseInt`, `Object.prototype.__proto__`,
`__defineGetter__` and its siblings, the `toLocaleString` of `Number.prototype`
and `Array.prototype`) were evaluated by the tree-walk interpreter in both
execution modes. The engine built a `TGocciaInterpreter` only to own the global
scope and to run them. Every function a shim defined was AST-backed and
tree-walked on every call, also when bytecode called it: a `Date.parse` of an ISO
date cost about 200 µs, and the SpiderMonkey `dst-offset-caching` test262 shards
time out on that cost
([#911](https://github.com/frostney/GocciaScript/issues/911),
[#829](https://github.com/frostney/GocciaScript/issues/829)). The interpreter
cannot be removed while the engine needs it to boot
([#873](https://github.com/frostney/GocciaScript/issues/873),
[#825](https://github.com/frostney/GocciaScript/issues/825)).

`Date` already loaded on first use. The seven shims that patch built-in
prototypes ran at every boot, and parsing and evaluating them was more than half
of empty-script boot. Compiling them to bytecode at boot instead costs about a
third more than tree-walking them.

## Decision

- **The executor loads a shim** (`TGocciaExecutor.EvaluateShim`). The bytecode
  executor compiles the shim as a module, as the loader compiles one while
  linking it (ADR 0136), and runs it in the shim's own module environment, so
  every function it defines is a VM closure. A shim is engine code: it compiles
  strict, with `arguments` available, never with strict types, its functions and
  classes keep no source text (`TGocciaCompiler.HideSourceText`), and the
  coverage tracker leaves its file out of every report. The interpreter executor
  evaluates the shim as before.
- **No shim loads at boot.** A shim that provides a global stays a lazy global.
  A shim that provides members of a built-in prototype installs each member as a
  lazy property of its prototype (`ShimPrototypeMembers`), at the position the
  member always had, and loads the first time a member is read, written,
  deleted or reflected on. `__proto__` is an accessor, so a lazy property can now
  materialize into an accessor (`TGocciaLazyPropertyDescriptorData.CreateAccessor`).
  A shim loads once per realm, in the realm of the engine that owns it, also
  when code of another realm touches it first.
- **A Date of another realm is a Date.** Each realm keeps its Dates'
  time values in its own store, so the stores of a thread's live realms are
  listed (`Goccia.Values.DateData`) and a Date of another realm is found in
  its realm's store (ES2026 §21.4.2.1 Date step 4.b, and the
  RequireInternalSlot(dateObject, [[DateValue]]) step of each Date.prototype
  method, such as §21.4.4.10 getTime).
- **The engine owns the global scope** (`TGocciaEngine.GlobalScope`) and builds
  a `TGocciaInterpreter`, which shares it, only for an interpreter executor.
  With a bytecode executor `Engine.Interpreter` is nil.

## Consequences

- No function of a shim tree-walks in bytecode mode, so nothing the engine
  creates in bytecode mode is AST-backed. The VM's construction bridge for
  evaluator-built classes (`InstantiateClass`, `UsesOwnInstantiation`,
  `TryConstructOnReceiver`) is no longer reached in bytecode mode, and goes
  with the interpreter ([#875](https://github.com/frostney/GocciaScript/issues/875)).
- Empty-script boot no longer parses or runs any shim in either mode.
- A shim function's frames are located in the shim (`<shim:Date>`), where they
  used to carry the calling file's name with the shim's line, and a bytecode
  caller's own frames are no longer dropped from the stack.
- `Function.prototype.toString` names a shim method by its property name
  (`function __defineGetter__() { [native code] }`, `function get __proto__() {
  [native code] }`), where it used to show the name of the descriptor key the
  shim defined it under (`value`, `get`).
- `TGocciaInstanceValue` materializes a lazy placeholder on its own property
  reads and writes, as `TGocciaObjectValue` does: `Number.prototype` and
  `Array.prototype` are instance values.
- A realm made with `$262.createRealm()` gets its own `Date`. It used to get
  the `Date` of whichever realm had loaded the shim first, which is how a Date
  passed between realms used to be recognized.
- Embedders read the global scope from `Engine.GlobalScope`; code that read it
  through `Engine.Interpreter` fails with a bytecode executor.
