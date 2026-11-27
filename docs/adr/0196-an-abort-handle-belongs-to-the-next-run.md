# 0196 - An abort handle belongs to the next run and expires with it

**Date:** 2026-11-27
**Area:** `runtime`, `embedding`
**Related:** [ADR 0174](0174-a-host-stops-a-run-from-another-thread-through-an-abort-handle.md), [#2051](https://github.com/frostney/GocciaScript/issues/2051)

## Context

[ADR 0174](0174-a-host-stops-a-run-from-another-thread-through-an-abort-handle.md) gave hosts an abort handle captured
for the thread's current run and cleared with `ClearAbort`. The engine that replaced that runtime had no handle: a host
could bound a run only with `TimeoutMs`. A daemon that cancels a run when its client disconnects needs the handle back,
and a handle a host forgets to clear must not stop a later run on a reused engine.

## Decision

**`TGocciaEngine.CaptureAbortHandle` returns a handle for the engine's next `Run` or `RunArtifact`; the handle binds
when that run starts and expires when it ends.** There is no `ClearAbort`.

- A request before the run starts keeps it from starting. A request during it ends it past guest handlers with
  `gaHostAbort`, and the engine interrupts its fetches and Atomics waits.
- A request that arrives after the run's own last check, while it finishes, still reports the run as cancelled, and
  the work it left is discarded as after any abort, so the result and the cleanup agree.
- A request after the run ended does nothing. Capturing again before the run replaces the earlier handle, which then
  never binds.
- The handle keeps no engine state once its run ended, so any thread may release its reference at any time.

## Consequences

- A host captures a fresh handle for each run it may cancel.
- A cancelled run has no completion value, whenever the request came.
- A host environment setup that a cancellation cuts short loads again on the next run.
