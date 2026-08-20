# Native AsyncLocalStorage over continuation snapshots

**Date:** 2026-08-20
**Area:** `runtime`

GocciaScript implements `node:async_hooks` natively — `AsyncLocalStorage` and
`AsyncResource` — and the engine, not the JavaScript layer, propagates the
async context. The propagation mechanism is general: one immutable snapshot per
continuation, holding the bindings of every `AsyncLocalStorage` instance at
once, rather than a per-instance stack.

## Why the engine has to do it

A userland shim cannot express these semantics, and the reason is structural
rather than a matter of effort. `storage.run(store, callback)` returns as soon
as an `async` callback reaches its first `await`. A shim's `finally` therefore
pops the store at that moment, and the resumed continuation — the part of the
callback that actually does the work — reads nothing. The context has to travel
with each continuation, and only the code that creates continuations can attach
it. The acceptance target is `convex-test`, which is built on three concurrent
`AsyncLocalStorage` instances; a mechanism that handles one instance, or that
handles `run` but not interleaving, does not run it.

Declining the feature was the alternative. It was rejected for the same reason:
`AsyncLocalStorage` is the last engine-level wall in front of npm-shaped test
suites, and there is no workaround a suite author can apply.

## The mechanism

A snapshot is an immutable association list from `AsyncLocalStorage` instance to
store. `run` derives a new snapshot from the current one, installs it, calls the
callback, and restores the previous snapshot when the callback's synchronous
part returns. Deriving copies the entries once, so every continuation that
already captured a snapshot keeps seeing exactly the bindings that were in
effect when it was created.

Two seams carry snapshots into continuations, and both sit in machinery the
interpreter and the bytecode VM share, so mode parity is structural rather than
maintained:

- **Registration** — a promise reaction records the current snapshot when it is
  registered on a still-pending promise (`TGocciaPromiseReaction.Context` in
  `Goccia.Values.PromiseValue`). Recording it at settlement instead would be
  wrong: `run(store, () => pending.then(handler))` must reach `handler` with
  the store even though whatever resolves `pending` runs outside the `run`.
- **Execution** — the microtask queue installs a job's snapshot before running
  it and restores the previous one afterwards
  (`TGocciaMicrotaskQueue.ExecuteTask` in `Goccia.MicrotaskQueue`). Every
  enqueue path captures the current snapshot unless it passes one explicitly,
  so no call site can forget.

`await` needs no seam of its own. GocciaScript drains awaits synchronously
inside the awaiting call, so the awaiting frame never leaves the Pascal stack;
the jobs drained during the await restore the snapshot they found, which leaves
the awaiting frame's own snapshot in place across the await. The restore is also
what keeps a foreign continuation, drained by an unrelated `await`, from leaking
its bindings into the frame that drained it.

The current snapshot is a garbage-collection root through a `TGCRootSource`, and
queued snapshots are rooted with the microtask they belong to. `nil` is the
empty snapshot rather than a missing one, so a program that never touches
`AsyncLocalStorage` allocates nothing and pays one null check per enqueue.

## Availability

`node:async_hooks` is registered by the loader runtime profile, which means it
is present in `GocciaScriptLoader`, `GocciaTestRunner`, `GocciaREPL`, and
`GocciaBenchmarkRunner` without an opt-in. It carries no capability: no I/O, no
clock, no ambient authority, no way to observe anything the running program did
not already have. It is context bookkeeping over values the program itself
supplies, so gating it would add a switch that protects nothing. The address is
Node's own — there is no `goccia:` spelling — because the surface is Node's
rather than one GocciaScript invented.

## Semantics and scope

The implemented surface was probed against Node v24.0.1 rather than read off the
documentation, and matches it on every probe: `run`, `getStore`, `enterWith`,
`exit`, `disable`, the `defaultValue` and `name` constructor options, the
`AsyncLocalStorage.bind` and `AsyncLocalStorage.snapshot` statics, and
`AsyncResource` with `runInAsyncScope`, `bind` (instance and static), `asyncId`,
`triggerAsyncId`, and `emitDestroy`. Two behaviours are worth recording because
they are not obvious from the prose: `exit` binds `undefined` rather than
dropping the binding, so `getStore` inside it reports `undefined` even when the
instance has a `defaultValue`; and `disable` leaves the default value reachable,
so `getStore` reports it rather than `undefined`.

Deliberately out of scope:

- **Host-scheduled callbacks.** GocciaScript has no timer task queue and no
  general event loop, so there is no `setTimeout` continuation for a context to
  travel into. When a host-scheduled callback surface is added, its scheduling
  point becomes a third seam; nothing about the snapshot representation has to
  change for that.
- **The `async_hooks` observer API.** `createHook`, `executionAsyncId`,
  `triggerAsyncId`, and the `init`/`before`/`after`/`destroy` callbacks are not
  provided. They describe an async-resource lifecycle GocciaScript does not
  have — awaits do not suspend, so there are no resources to report. The
  per-resource ids `AsyncResource` exposes are unique and stable but relate to
  nothing else, and `triggerAsyncId` reports the resource's own id.
- **Cross-realm propagation.** A ShadowRealm child gets its own snapshot state,
  as it gets its own module records and intrinsics.

`disable()` is implemented as a per-instance flag that `run` and `enterWith`
clear, which is what Node's observable behaviour requires. It is not a retroactive
erasure of bindings from snapshots that were already captured: a continuation
created before the `disable`, resumed after a later `run` re-enabled the
instance, can still see the store it captured. Reaching that state requires
disabling an instance and then re-enabling it while an older continuation is
still pending, and no other behaviour depends on it.

## Consequences

A latent bytecode-VM bug had to be fixed to ship this. `OP_CALL_METHOD`
recognised `Function.prototype.bind` by the callee's *name*, so any own static
named `bind` on a function object was silently redirected into
`TGocciaBoundFunctionValue` — which is exactly what `AsyncLocalStorage.bind` and
`AsyncResource.bind` are. The check now matches the intrinsic by identity
(`nikFunctionBind`). The sibling `call` and `apply` fast paths carry the same
name-based hazard behind a narrower guard and are untouched here.
