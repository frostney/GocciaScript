# ShadowRealm full test262 conformance

**Date:** 2026-06-25
**Area:** `engine`

Completes `built-ins/ShadowRealm` test262 conformance (64/64) on top of [ADR 0073](0073-opt-in-shadowrealm.md), which scoped four cases out. Two were deliberate divergences this change reverses for spec conformance; the other two were plain bugs.

**`eval` in child realms (reversing the no-eval boundary).** SetDefaultGlobalBindings installs `eval` on a realm's global object, so a spec-conformant ShadowRealm child global exposes it. ADR 0073 omitted it to honor the no-eval default, but ShadowRealm is already an explicit dynamic-evaluation capability (`evaluate` / `importValue`). The child realm now mirrors the creating realm: when — and only when — the creating realm exposes an own `eval`, the child gets its own realm-bound `eval` that evaluates in the child realm, so isolation holds and a host without `eval` (the default) still yields a child without `eval`.

**Shared Symbol registry (reversing the per-realm registry).** The GlobalSymbolRegistry is one per agent (ES2026 §20.4.2.2): every realm in a thread — the main realm, ShadowRealm child realms, and `$262.createRealm` realms — shares it, so `Symbol.for(key)` returns the same symbol across realms and `Symbol.keyFor` observes it. It is a thread-scoped store owned by the first `TGocciaGlobalSymbol` created in the thread; because child realms are torn down before their creator, the owner outlives every sharer and frees it at teardown. The garbage collector is a thread singleton, so registered symbols live on the shared heap regardless of which realm created them.

**Bug fixes.** A wrapped function's `length` now carries `+∞` — CopyNameAndLength may yield `+∞`, which an integer arity cannot represent, so the wrapper defines `length` as the Number `L` directly. A static early error in `evaluate` (e.g. assigning to `arguments` in a strict body) is now a caller-realm `SyntaxError`, validated before evaluation rather than conflated with a runtime abrupt completion — which §3.1.3 still wraps as a caller-realm `TypeError`, including a runtime `SyntaxError` thrown by a nested `eval`.
