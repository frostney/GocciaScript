# 0171 - The wait that drives one promise to settlement runs extensions' host work

**Date:** 2026-10-31
**Area:** `engine`, `runtime`
**Related:** [ADR 0142](0142-the-run-waits-until-no-extension-has-pending-work.md), [ADR 0113](0113-deterministic-virtual-timer-queue.md), [ADR 0148](0148-heap-snapshots-at-idle-points.md)

## Context

An embedder's runtime extension can expose an async API whose promise it
settles from `WaitForIdle`, reporting `HasPendingWork` until then
([ADR 0142](0142-the-run-waits-until-no-extension-has-pending-work.md)).
Awaited at the top level of a module, such a promise settles: the run's idle
wait calls the extension. Awaited inside a `goccia:test` `test()`, it failed
with `Promise still pending after microtask drain`
([#1556](https://github.com/frostney/GocciaScript/issues/1556)). The test
library drives an async test's returned promise, `.resolves`/`.rejects` and
async hooks to settlement with `WaitForFetchPromise`, and a synchronous
`await` uses it too. That wait knew fetch, `Atomics.waitAsync` and real-mode
timers, and nothing an extension owns.

## Decision

**`WaitForFetchPromise` runs the running engine's extension work before it
polls fetch or runs a timer.** `TGocciaEngineExtension` and
`TGocciaRuntimeExtension` gain a virtual `RunPendingWorkForPromiseWait`. Its
default runs `WaitForIdle` when `HasPendingWork` is True and returns whether
it was. `TGocciaRuntimeCore` forwards it to each of its extensions. The engine
installs a hook (`GPromiseWaitHostWork` in `Goccia.FetchManager`) that calls
it on every extension of the innermost running engine on the thread. When any
had work, the wait drains microtasks again before it checks anything else.

- **Fetch and timers opt out.** The wait already polls fetch completions and
  runs real timers one at a time, in an order chosen so that a fetch raced
  against a timeout decides the race. Their `WaitForIdle` would wait for every
  request, or run virtual time ahead of outstanding work, so both extensions
  return False.
- **The host-call owner keeps its checkpoint.** With `OnPauseCheckpoint` set,
  the owner's work is left for the idle wait, which offers the pause
  checkpoint before it calls the owner ([ADR 0148](0148-heap-snapshots-at-idle-points.md)).

## Considered options

- **Run `WaitForRuntimeIdle` from the promise wait.** It runs every
  extension, timers and fetch included, and offers pause checkpoints in the
  middle of a test.
- **A separate hook the test library calls.** A synchronous `await` reaches
  the same wait and has the same gap.

## Consequences

- An extension that settles its promises from `WaitForIdle` works inside
  `test()`, `.resolves`/`.rejects`, async hooks and a synchronous `await`,
  with no change to the extension.
- An extension whose `WaitForIdle` should not run there, as for fetch and
  timers, overrides `RunPendingWorkForPromiseWait` to return False.
- With a pause-checkpoint handler installed, a host call awaited inside a
  test still is not served by the promise wait.

## Related

- [Embedding — Runtime Extensions](../embedding.md#runtime-extensions)
- [Architecture — Synchronous Microtask Queue](../architecture.md#synchronous-microtask-queue)
