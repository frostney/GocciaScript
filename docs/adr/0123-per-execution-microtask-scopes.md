# 0123 - A nested engine run gets its own microtask scope

**Date:** 2026-10-01
**Area:** `runtime`, `sandbox`

## Context

The microtask queue is one object per thread, and its jobs had no owner.
Engines nest on a thread: a sandbox `runScript` child executes synchronously
inside its caller's statement. The child's idle drain therefore ran the
caller's pending promise callbacks in the middle of that statement, with their
output captured as the child's `stdout`, and a child that failed cleared the
queue and silently deleted them ([#1293](https://github.com/frostney/GocciaScript/issues/1293)).

ES2026 §9.5 (`sec-jobs`) invokes a job only when there is no running execution
context in its agent and that agent's execution context stack is empty, and
requires every enqueued job to be invoked eventually. A child engine is a
separate agent; its caller's stack is not empty while the child runs.

## Decision

**A nested `Execute` runs in a scope of the queue.** `TGocciaMicrotaskQueue`
keeps a stack of scopes. `EnterScope` hides the current jobs and starts on an
empty queue; `LeaveScope` discards what the scope left behind and puts the
hidden jobs back in their original order. Every drain, `HasPending` check and
`ClearQueue` sees only the current scope, so no call site that pumps the queue
(await, the fetch and timer pumps, the idle drain) needed a change.

**Only a nested run is scoped.** `TGocciaEngine` counts the runs in progress
on the thread (`Execute`, `ExecuteProgram`, `RunModule`). An `Execute` that
starts while the count is non-zero enters a scope; the outermost one keeps the
thread's own scope and clears it on exit, exactly as before. Hosts queue jobs
ahead of the entry script — the runner evaluates a `--globals` module before
`Execute` — and those jobs have to drain with it.

**A promise's jobs belong to the scope the promise was created in**, and a
`FinalizationRegistry` cleanup job to the scope that created the registry.
Completion pumps and the collector serve the whole thread, so a nested engine's
drain can settle its caller's fetch or `Atomics.waitAsync` promise and can
collect its caller's registered targets. The resulting jobs — reactions, the
job that calls a thenable's `then`, cleanup callbacks — are enqueued into the
owning scope rather than the current one.

**A job whose scope has ended is enqueued into the current scope.** Scope
identifiers are never reused, so such a job cannot land in an unrelated later
scope by accident.

**A ShadowRealm shares its creator's scope.** It is a second realm of the same
agent and never calls `Execute`.

## Considered options

- **Tag each job with its owning engine or realm and filter in `DrainQueue` and
  `ClearQueue`.** Mirrors the fetch manager, which tags requests with a realm.
  Rejected: a ShadowRealm has its own realm but shares its creator's jobs, so a
  realm tag splits one agent, and a single list needs a head index per owner to
  stay O(1).
- **Give a nested engine its own queue object and swap the thread instance.**
  Equivalent for jobs enqueued by running code, but the pumps settle promises
  on behalf of other engines, and addressing another engine's queue by pointer
  ties job ownership to that object's lifetime. Scopes inside one queue are
  addressed by identifier and fall back safely.
- **Scope every `Execute`.** Simpler to state, and the first implementation.
  Rejected: jobs queued before `Execute` were never drained, which broke a
  `--globals` module's pending callbacks in interpreted mode.
- **Defer another engine's fetch completions instead of settling them.** Would
  keep even a `then` getter from running during the child. Rejected for this
  change: it reverses the settlement behavior [#1258](https://github.com/frostney/GocciaScript/pull/1258)
  established and tested.

## Consequences

- `runScript`, `{ sandbox: true }` children and shell `goccia` return before
  any of the caller's pending jobs run, and a failing child leaves them queued.
- Only `Execute` isolates. A nested `ExecuteProgram` or `RunModule` drains the
  scope it was called in.
- Reading a thenable's `then` property still happens where the promise is
  resolved. A caller's `then` *getter* on a fetch response can therefore run
  during a nested drain; the job that calls `then` does not.
- `TGocciaPromiseValue` and `TGocciaFinalizationRegistryValue` each carry one
  scope identifier.

## Related

- [Interpreter — Synchronous Microtask Queue](../interpreter.md#synchronous-microtask-queue)
- [ADR 0112](0112-native-async-local-storage.md) — the engine-lifetime bracket
  for the async context, which this bracket resembles but does not share: it
  spans one `Execute`, not the engine's life.
