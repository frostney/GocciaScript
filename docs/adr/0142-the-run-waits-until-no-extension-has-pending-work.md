# 0142 - A run waits until no extension has pending work, and an entry still pending then fails the run

**Date:** 2026-10-15
**Area:** `engine`, `runtime`, `cli`
**Related:** [ADR 0113](0113-deterministic-virtual-timer-queue.md), [ADR 0124](0124-unhandled-rejections-fail-the-run.md), [ADR 0125](0125-unhandled-rejection-hook.md)

## Context

A module entry whose first top-level `await` settled at once, followed by
`await fetch(url)`, ended right after the request started, with exit 0
([#1559](https://github.com/frostney/GocciaScript/issues/1559)). That happened
on every path through `TGocciaEngine.Execute`: `--copy` and `--sandbox` in
both modes, and host mode interpreted. `Execute` runs the entry as an async
module, so the rest of its body runs as a job in the run's final
`WaitForRuntimeIdle`. That wait ran each extension's `WaitForIdle` once,
before it drained the microtask queue. A fetch started by a job in that drain
had nothing left waiting for it, so the run ended and the request was
discarded. The entry's evaluation promise was still pending, and `Execute`
reported that as success. Host-mode bytecode worked only because it compiles
top-level `await` as a synchronous await that pumps fetch itself.

ES2026 §16.2.1.6.1.3 `Evaluate()` returns the module's evaluation promise;
§9.5 leaves it to the host to decide when its run is finished.

## Decision

**Extensions report pending work, and the idle wait repeats until none does.**
`TGocciaEngineExtension` and `TGocciaRuntimeExtension` gain a virtual
`HasPendingWork`, which returns False by default. `TGocciaRuntimeCore` returns
True when any of its extensions does. `WaitForRuntimeIdle` runs a round
(every extension's `WaitForIdle`, then the microtask and `Atomics.waitAsync`
drain) and repeats it while any extension reports work.
- The fetch extension reports a request in flight for its realm.
- The timers extension reports a real-mode timeout its host drain would run.
  Once the drain's loop limit has stopped it, it reports nothing, so a timer
  that keeps rescheduling itself still ends the run.

Every run path shares this one wait, so `Execute` needs no special case.

The contract for an extension is: return True only when `WaitForIdle` will
block on that work or make progress on it. A True that `WaitForIdle` does
nothing about spins the engine thread. An extension that does not override
the hook keeps the old single pass, so one that can gain work after a
microtask drain must implement it.

**An entry still pending when the run is idle fails the run, in every mode.**
Nothing left in the run can settle it. `Execute` first reports the oldest
unhandled rejection the usual way ([ADR 0124](0124-unhandled-rejections-fail-the-run.md),
[ADR 0125](0125-unhandled-rejection-hook.md)): a reaction that threw instead
of settling what the entry awaits is the real cause, and under `urThrow` it is
the run's error. Otherwise it raises
`TypeError: await: Promise did not settle after microtask drain`, which host
bytecode already raised at the await itself. A rejected entry still raises its
own reason.

**The idle wait waits out `Atomics.waitAsync` timeouts in real time.**
It waits for waiters that have a timeout or were already notified. A waiter
with no timeout can be settled only by `Atomics.notify`, which nothing left in
an idle run will call, so the wait does not block on it. The wait sleeps 1 ms
between polls and checks the run deadline and the instruction budget each
time, as every other engine wait does, so a host stop ends it promptly.
Timers stay virtual ([ADR 0113](0113-deterministic-virtual-timer-queue.md)):
no real time passes for them.

## Considered options

- **Wait on the entry promise in `Execute` with the synchronous await's
  helper.** That fixes an awaited fetch, but not a `fetch().then()` started
  after the first await, whose request still ends in flight.
- **Repeat the extension pass whenever the microtask drain did work.** That
  misses work a job starts inside an extension's own drain: the fetch
  extension's idle wait drains microtasks itself when no request is in
  flight, so the engine's drain then finds nothing to do.
- **Report a pending entry as success, as before.** A run that stopped
  partway looked like a passing one.

## Consequences

- A `--copy`, `--sandbox` or host-interpreted entry whose top-level `await`
  can never settle now exits non-zero instead of 0. Host bytecode already did.
- A reaction that throws while the entry waits reports that error on those
  paths. Host bytecode still reports the generic `TypeError`, because its
  synchronous await fails before any rejection is reported.
- An embedder's extension with host work of its own must implement
  `HasPendingWork` to have that work waited for after a late job starts it.

## Related

- [Interpreter — Synchronous Microtask Queue](../interpreter.md#synchronous-microtask-queue)
- [Embedding — Runtime Extensions](../embedding.md#runtime-extensions)
