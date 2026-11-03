# 0174 - A host stops a run from another thread through an abort handle that both limit polls observe

**Date:** 2026-11-02
**Area:** `runtime`, `embedding`
**Related:** [ADR 0109](0109-engine-integrity-faults-are-uncatchable.md), [#1541](https://github.com/frostney/GocciaScript/issues/1541), [#1862](https://github.com/frostney/GocciaScript/issues/1862), [#1509](https://github.com/frostney/GocciaScript/issues/1509), [#864](https://github.com/frostney/GocciaScript/issues/864), [#867](https://github.com/frostney/GocciaScript/issues/867)

## Context

A host that runs scripts on worker threads has two limits it can set before a
run, `StartExecutionTimeout` and `StartInstructionLimit`, and no way to stop a
run it has already started. A sandbox daemon has to cancel a run when its
client disconnects. The only options were to kill and recycle the worker, or
to write the unit-private instruction-limit record from another thread, which
the issue's spike did. That worked until [#1862](https://github.com/frostney/GocciaScript/issues/1862)
made the poll compare the count with `NextLimitCount` alone: a stop written as
`MaxInstructions := 1` stopped nothing, and a stop written as both fields is
undone by the next scope push or pop on the running thread, which recomputes
`NextLimitCount` from a `MaxInstructions` it read before the stop. The spike
also reached bytecode dispatch only: native loops poll the deadline, not the
count.

The two polls are the place to stop a run. The bytecode loop polls the count
at every instruction on its instrumented path, a call or an allocation polls
both when `GThreadPolls` says one is armed, native loops poll
`CheckExecutionTimeout` (the chokepoint rule in `Goccia.Timeout`), and every
engine wait loops on `CheckExecutionTimeoutNow` and `CheckInstructionLimit`.
Both raise errors that every boundary lets unwind to the host
([ADR 0109](0109-engine-integrity-faults-are-uncatchable.md) and
`Goccia.UncatchableFault`), so unwinding to the host is a path the engine, the
collector and the hosts already survive.

## Decision

### The host API: a handle per run

`Goccia.ExecutionAbort` is the whole host surface:

```pascal
Handle := CaptureAbortHandle;   // engine thread, before the run
RequestAbort(Handle);           // any thread; also Handle.Request
ClearAbort;                     // engine thread, after the run
```

`CaptureAbortHandle` arms the calling thread and returns an
`IGocciaAbortHandle`, reference counted so that it outlives the run in
whichever thread holds it. `ClearAbort` disarms the thread and detaches the
handle; a request through a detached handle only records itself. A run
captures a fresh handle, so a late request for the previous run reaches
nothing, and the host needs no lock of its own around the capture and the
clear. A thread that ends is disarmed by the thread cleanup registry, which
`ShutdownThreadRuntime` runs as a worker exits; a thread that ends without
either leaves the handle attached to a record that is freed, and a request on
it then writes into that memory, so the engine thread must `ClearAbort` (in a
`finally`) before it ends. A request is sticky until `ClearAbort`: every poll raises,
so a nested `runScript` child's result, which carries the error back to its
parent as a resource-limit failure, cannot hide it, and the parent's next poll
ends the run ([#1509](https://github.com/frostney/GocciaScript/issues/1509):
the child shares the parent's polls). The handle covers the capturing thread's
engine only; a worker thread an embedder spawns for another engine needs its
own handle.

### The error: a subclass of the instruction limit's

`TGocciaAbortError` descends from `TGocciaInstructionLimitError`. About
twenty-five boundaries re-raise that class by name, `IsUncatchableFault`
answers for it, and `Goccia.CLI.SandboxHost` classifies it as
`sfkResourceLimit`; a subclass is covered by every one of them without a new
arm, and a new sibling class missed in one arm would be a catchable stop. A
reporter that wants to say "aborted" tests for the subclass first:
`GocciaRunner --output=json` reports `error.type` `RunAbortedError`. The
`failureKind` of a `runScript` result and the sandbox protocol
([ADR 0107](0107-out-of-process-sandbox-protocol.md)) gain no value: an abort
is a resource-limit outcome there, and the message ("Execution aborted by the
host") says which.

### One compare stays one compare

The instruction poll is `InstructionCount >= NextLimitCount`, inlined into the
VM. A request lowers `NextLimitCount` to 0, so the same compare fails, and
`InstructionLimitReached` raises the abort ahead of any budget. Three things
make that safe from another thread:

- **Where it writes.** The handle holds the address of the capturing thread's
  `NextLimitCount`, under the handle's lock, and `ClearAbort` takes it away
  under the same lock before disarming. A request therefore never writes into
  a thread record that is disarmed or has ended.
- **A recompute cannot undo it.** Every recompute of `NextLimitCount` (start,
  clear, scope push and pop, the slow path) stores the new limit, issues a full
  fence, and reads the request word again, lowering the limit if it is set. A
  request stores the word, issues a fence and then writes the limit. Each side
  stores before it loads, so either the recompute sees the request or the
  request's write lands after the recompute's.
- **It stays on.** While armed, `Active` stays true whatever the budgets are,
  and `ClearInstructionLimit` and the scope procedures keep it so.

The timeout poll's fast path is unchanged. Its slow path, which runs every
1,024th poll, or every poll under a deadline of 16 ms or less, and in every
wait loop, reads the request word first, ahead of the deadline, and raises. A
thread with an abort and no deadline carries a placeholder deadline that never
arrives, with the long-deadline sampling mask, so `TimeoutArmed` stays the one
mirror in `GThreadPolls` and `RemainingExecutionTimeoutMilliseconds` reports no
deadline for it. A native loop sees a request within 1,024 of its polls.

### What it costs, and what it reaches

An armed thread runs the VM's instrumented dispatch path, the one an
instruction limit selects, because the production path has no per-instruction
poll and a run already inside it would not see a request until its next call
or allocation. So capture has to precede the run, and being abortable costs
about what an instruction limit costs (a development build of a loop of cheap
instructions ran about 7 % slower under a limit and about 9 % slower armed,
best of five). A thread that is not armed pays nothing: the inlined polls, the
flags and the production path are as before.

A request ends:

| Where the run is | How it notices | Latency |
|---|---|---|
| Bytecode dispatch, including a loop with no call | the instruction poll | the next instruction |
| A native loop that follows the chokepoint rule (`Array.prototype.reverse`, `join`, `sort`, typed array sorts, `RegExp` matching, and the rest that call `CheckExecutionTimeout`; a loop that calls `CheckNativeWork` also checks the count) | the timeout poll | 1,024 polls |
| A value allocation, a bytecode call | `RunAllocationPolls`, `RunArmedPolls` | the next one |
| `Atomics.wait` | its wait loop, in 50 ms slices | at most 50 ms |
| `Atomics.waitAsync` and a promise wait | `Sleep(1)` loops | about 1 ms |
| A `fetch` nobody has answered, a `fetch` transport that never answers | the fetch wait loop | about 1 ms |
| A microtask chain that never ends | the callbacks' own polls | the next instruction |

Not interrupted: a native call blocked outside the engine, such as foreign
code reached through FFI or an extension's own blocking call, until it
returns; and a native loop that breaks the chokepoint rule, which no limit
stops either. In-flight network work on its own threads is cancelled when the
run's engine discards its requests, not at the request.

The engine is as safe to free or to run again as after a timeout, and for the
same reason: the abort is an ordinary exception on a path every boundary
already honours. Pending promises, queued microtasks and values only native
code held are reclaimed by the collector when the engine is freed; the
native tests free and rebuild the engine on the worker, and run one case with
a collection before every reservation.

### Rejected

- **A flag in the poll states, read at every poll.** One more load and branch
  per instruction for every run that has an instruction limit, to avoid a
  fence on the engine thread at a recompute, which is rare.
- **A public writer for `NextLimitCount` and the deadline.** It would freeze
  the private record's layout as host API, the thing that broke the spike.
- **A new sibling error class.** See above: one missed arm is a catchable
  stop.
- **A new `failureKind`.** The kind is part of the sandbox protocol and of the
  result `runScript` returns to script.
- **Waking a blocked wait through its event.** `Atomics.wait` would stop in
  microseconds, not up to 50 ms, but needs a handle-to-waiter registry for the
  one wait whose latency matters little.
- **Writing the thread's polls directly, with no handle.** A record that
  belongs to a thread, written from others, with no way to know the thread
  still exists.

## Consequences

- A daemon can cancel a run from a connection thread and recycle nothing: stop
  it, `ClearAbort`, free the engine, run the next script on the same worker.
- The host-facing cost is one capture before each abortable run and the
  instrumented dispatch path while it lasts.
- `GocciaRunner` and the other CLI hosts are single-threaded and do not
  request aborts; only their error reporting names the new error.
- A boundary that converts the error anyway, such as the module-resolution
  wrappers, hands script a catchable error only until the next poll, which
  raises again because the request is sticky. The testing library's per-test
  and per-file arms now re-raise it, as they do the memory limit.
- A native loop that does not poll, such as a host call with no check, is as
  unstoppable by an abort as by a deadline; the fix is the chokepoint rule's,
  not a second mechanism.
