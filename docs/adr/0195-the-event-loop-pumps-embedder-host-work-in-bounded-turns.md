# 0195 - The event loop pumps embedder host work in bounded turns

**Date:** 2026-11-27
**Area:** `engine`, `embedding`
**Related:** [ADR 0171](0171-the-promise-wait-runs-extension-host-work.md), [ADR 0113](0113-deterministic-virtual-timer-queue.md), [ADR 0193](0193-fetch-requests-past-the-worker-cap-wait-their-turn.md)

## Context

The new engine had no way for an embedder to settle its own promises from native work, such as a request it
answers on another thread. The previous engine's extension API ([ADR 0171](0171-the-promise-wait-runs-extension-host-work.md))
ran an extension's work only once no fetch was in flight, so a native request and a fetch could not make progress
together: one waited for the other. The run also has to keep its deadline, its instruction budget and its virtual
timers ([ADR 0113](0113-deterministic-virtual-timer-queue.md)) while it waits for native work it cannot see.

## Decision

**The event loop asks the embedder's work source for ready results on every turn and waits on it only in short,
bounded slices.** `TGocciaEmbedderWork.Pump(0)` takes what is ready; a positive timeout waits for a wake, capped at
10 ms, the run's remaining time and the next `Atomics.waitAsync` timeout. Between slices the loop takes ready
Atomics and fetch results without blocking on either, so each source makes progress while the others wait.

- Pending host work comes before a virtual timer jump, as an in-flight fetch does
  ([ADR 0113](0113-deterministic-virtual-timer-queue.md)): while either waits, only a timer already due at the current
  virtual time runs, so a long timer cannot overtake a real result.
- Waiting for host work spends no instructions. The loop checks deadlines and cancellation before and after each
  pump without charging the budget, and does not run the timer queue again while it holds only timers that cannot
  fire (intervals in an ordinary run, or fake timers) until a job has run.
- A pump during which the deadline passes delivers late: the reactions its results queued are dropped, as a job
  taken after an abort is, so a timed-out test does not resume.
- Failure discards the source's outstanding requests (`Discard`), whichever exit the run takes. A timed-out test
  discards only the requests it started (`Mark`, `DiscardFrom`).

## Consequences

- A host settles its promises on the engine thread inside `Pump`; background threads only hand it native data.
- A result delivered in the pump during which a test's deadline passes settles its promise, but the reactions that
  settlement queued are dropped, so anything derived from it never settles, even when it belongs to work started
  before that test. The loop caps a pump's wait at the time left, so a source that keeps to its timeout delivers such
  a result only at the deadline itself.
- The bounded slice adds up to 10 ms of latency to a wake the source does not signal.
