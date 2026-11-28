# Strict types keep a Script's top-level types on its global bindings

**Date:** 2026-11-28
**Area:** `engine`, `strict-types`, `bytecode`
**Related:** [ADR 0038](0038-opt-in-strict-types-enforcement.md), [#2284](https://github.com/frostney/GocciaScript/issues/2284), [#2205](https://github.com/frostney/GocciaScript/pull/2205), [#2204](https://github.com/frostney/GocciaScript/pull/2204)

## Context

`--strict-types` checks a store with `opCheckType`, which the compiler emits from the type it records on a
binding. That works for the bindings it resolves to a register or an upvalue. A Script's top-level bindings are
different: a `var` is a property of the global object, and a `let` or `const` is a global lexical binding that
other Scripts reach by name. So a top-level `var x: T` was checked nowhere, and a top-level `let` only by stores
compiled in its own Script. A `Function` body, the REPL's next input or an embedder's next Script could store any
value into either.

Three designs were weighed: checking every write through a type that lives with the binding at run time, checking
only the stores the compiler sees (and documenting the rest as unchecked), or the first plus the host's
global-writing entry points. In the specification each of these writes is one [[Set]] on the global object (ES2026
§10.1.9, through §9.1.1.4.5 SetMutableBinding) or a store into the global declarative record, and TypeScript
(tsc 5.9.3 and 7.1.0-beta) reports every one of them as a type error. Johannes chose the first.

## Decision

- **A typed top-level `var` tags its global object property.** The compiler records each typed var of a Script's
  top level (annotated, or with a type its initializer implies) on the template (`GdiTypedVars`,
  `GdiTypedVarCodes`); `GlobalDeclInst` adds them to the realm's table of typed global properties. Every write of
  the property's value is checked against the tag (`GlobalTypeOK`): the own data store of ordinary [[Set]]
  (`SetOnObjectOk`), which `opSetGlobal`, property and element stores, `Reflect.set` and `Object.assign` reach;
  [[DefineOwnProperty]] with a value; and `CreateDataPropertyOrThrow`.
- **A typed top-level `let` keeps its type on its global lexical binding** (`GdiLexTypes`, `GLexTypes`), checked
  in `GLexSet`, the one store into the binding's cell from code compiled apart from it.
- **The fast paths never cache a typed property or binding.** `opSetGlobal`'s shape cache, in the interpreter and
  the baseline JIT, and the property store cache skip a tagged key; the JIT's cached lexical-cell store skips a
  typed binding as it skips a `const`. Untyped globals keep every fast path.
- **A function declaration cannot replace a typed var with a function** unless the type is `object`:
  `GlobalDeclInst` refuses it before declaring anything, as entering a function body does.
- **The first type stays.** A later Script's `var` of the same name keeps the first type; deleting the property
  (a configurable one) drops its tag.
- The `.gbc` format version is 202, since templates carry the new type codes.

## Consequences

- Without `--strict-types` nothing changes: no type is recorded, so every check finds none.
- Under `--strict-types`, a top-level `var` with a literal initializer now has the inferred type a function's
  `var` and a top-level `let` already had.
- Direct `eval`, which only an embedding host can enable, still compiles its caller's function-local bindings
  without their types, so eval code's stores into them are unchecked; its stores into global bindings are checked
  like any other code's.
- Writes made by the host's own API (globals injection) are not stores of script code and are not checked.
