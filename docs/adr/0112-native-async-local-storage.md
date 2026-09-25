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

`await` needs no seam of its own, but not because nothing suspends. An async
function does suspend at each `await` — `EGocciaAsyncAwaitSuspend` in the
interpreter, `EGocciaBytecodeAsyncSuspend` in the bytecode VM — and its
resumption is attached with `InvokeThen` on the awaited promise. That lands the
continuation on the registration seam above, which records the snapshot in
effect at the point of the `await`, so the resumed body sees exactly the
bindings the suspending frame had. What GocciaScript does not do is *park*:
the awaited promise is driven to settlement by draining the microtask queue on
the same Pascal stack, so the resumption is reached from inside the awaiting
call rather than from a later turn of an event loop. Either way the snapshot
travels with the reaction, not with the stack. The restore in the execution
seam is what additionally keeps a foreign continuation, drained by an unrelated
`await`, from leaking its bindings into the frame that drained it.

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

  **Superseded by [ADR 0113](0113-deterministic-virtual-timer-queue.md).** The
  virtual timer queue is that surface, and the prediction held: timer
  registration captures the current snapshot and the queue installs it around
  the callback through the same `EnterAsyncContext` / `LeaveAsyncContext` pair,
  with nothing in `Goccia.AsyncContext` changed.
- **The `async_hooks` observer API.** `createHook`, `executionAsyncId`,
  `triggerAsyncId`, and the `init`/`before`/`after`/`destroy` callbacks are not
  provided. They describe an async-resource lifecycle GocciaScript does not
  have — awaits do not suspend, so there are no resources to report. The
  per-resource ids `AsyncResource` exposes are unique and stable but relate to
  nothing else, and `triggerAsyncId` reports the resource's own id.
- **Cross-realm propagation.** Snapshot state is thread-local, not
  realm-local: a ShadowRealm child shares the thread's current snapshot rather
  than getting one of its own. Nothing can observe that, because a child realm
  has no way to import `node:async_hooks` and therefore no way to read or bind
  a store. If a child ever gains access to the module, whether the state should
  be partitioned per realm becomes a real question; today it is unobservable
  rather than isolated.

`disable()` deletes the binding from the current frame and does nothing else,
matching Node v24's AsyncContextFrame model. Every observable consequence
follows from that one edit rather than from a rule of its own: `getStore()`
reports the default value afterwards because no binding is left in this frame;
a later `run` or `enterWith` re-binds; and a continuation captured *before* the
disable still reports the store it captured, because the disable never reached
that frame.

An earlier draft used a per-instance `disabled` flag instead. It reproduced the
first two consequences and got the third wrong in both directions, which is why
the model rather than the flag is the mechanism: the flag masked bindings in
already-captured continuations where Node keeps them, and it made `exit()` on a
disabled instance report the default value where Node reports `undefined`. Both
were probed against Node v24.0.1, along with the cases that agreed either way
(disable-then-getStore with a default value; disable followed by a re-binding
`run` straddling a pending continuation).

## Async generators

Each executor drives async generators through its own request queue
(`TGocciaAsyncGeneratorObjectValue` in `Goccia.Values.GeneratorValue`,
`TGocciaBytecodeAsyncGeneratorObjectValue` in `Goccia.VM`), and neither queue
records an async context of its own. That is deliberate and was checked rather
than assumed: every resumption reaches the generator body through a promise
reaction, which already carries the snapshot captured where it was registered,
so the body observes the context of whichever call resumed it — which is what
Node does.

Four shapes were probed against Node v24.0.1 and match in both executors: a
`for await` inside a `run`; a generator created under one store and resumed
with none; a generator started with no store and resumed inside a `run`; and
two overlapping `next()` calls from two different stores, where the second is
queued behind the first. `tests/built-ins/AsyncHooks/async-generators.js` locks
all four in, and the differential suite gates the first two against bun. The two
queues are duplicated machinery, so each carries a comment pointing at the
other.

## Consequences

Installing a snapshot has to go through a saved-context stack that the
collector marks, not through a Pascal local. The current-snapshot slot is a
snapshot's only root, so a scope that displaced one and held it in a local
across guest code let a collection inside that code free it, and the restore
then wrote a dangling pointer — `run` nested in `run` with a `Goccia.gc()` in
the inner callback crashed both executors. `EnterAsyncContext` /
`LeaveAsyncContext` in `Goccia.AsyncContext` push the displaced snapshot onto a
per-thread stack that the async-context root source also marks, and all three
scoped-installation sites use them. The token they exchange is the stack depth
at entry, so a frame restores to its own depth and cannot unwind past an
enclosing frame's entry.

A latent bytecode-VM bug had to be fixed to ship this. `OP_CALL_METHOD`
recognised `Function.prototype.bind` by the callee's *name*, so any own static
named `bind` on a function object was silently redirected into
`TGocciaBoundFunctionValue` — which is exactly what `AsyncLocalStorage.bind` and
`AsyncResource.bind` are. The check now matches the intrinsic by identity
(`nikFunctionBind`). The sibling `call` and `apply` fast paths carry the same
name-based hazard behind a narrower guard and are untouched here.
