# Engine-integrity faults are never guest-catchable

**Date:** 2026-08-15
**Area:** `runtime`, `errors`, `gc`
**Related:** [ADR 0105](0105-argument-collections-root-their-elements.md), [ADR 0106](0106-sandbox-hardening-scope.md)

## Context

Every guest-visible boundary in the engine ends in a generic `on E: Exception`
arm that turns a Pascal exception into a script value: an `Error` object handed
to `catch`, a promise rejection, a recorded test failure. Roughly thirty such
arms exist, across the evaluator, the VM, the promise and await machinery, the
generator continuations, the microtask queue, JSON, fetch, and the testing
library. They were written for the engine's own domain exceptions, and they are
correct for those.

They are not correct for everything that can reach them. A `TGocciaValue`
collected while a native Pascal local still points at it fails on the next
virtual call — `EObjectCheck` in a development build, where `$OBJECTCHECKS ON`
is set in `source/shared/Shared.inc`, and an unchecked read into reclaimed
memory in a production build. Reaching one of those arms, that use-after-free
becomes an ordinary `catch (e)`, and the script carries on running on top of a
heap the engine has already lost track of. The same is true of an
`EAccessViolation` from any other source, and of an `EInvalidPointer` raised
when the heap manager finds its own bookkeeping destroyed.

This is the argument the limit family already makes for
`TGocciaMemoryLimitError`: a ceiling the guest can catch is a ceiling the guest
can ignore in a loop, so the limit re-raises past every handler to the host.
An integrity fault deserves the same treatment for a stronger reason. A refused
allocation has a defined continuation — the heap is intact, and the script may
free something and retry. A corrupted heap has none.

## Decision

`Goccia.EngineFault.IsEngineIntegrityFault` names a fixed family of exception
classes that mean the process state is no longer trustworthy, and every
conversion boundary opens its generic arm with

```pascal
on E: Exception do
begin
  if IsEngineIntegrityFault(E) then
    raise;
  ...convert E into a guest value...
end;
```

The bare `raise` re-raises inside the handler that caught it, so the fault keeps
its original class and its stack all the way to the host. The predicate lives in
its own unit that depends on nothing but `SysUtils`, so any unit can guard
without acquiring a dependency.

The family is: `EObjectCheck` (virtual dispatch through a nil or freed
instance), `EAccessViolation` (an invalid dereference, covering its descendant
`EBusError`), `EInvalidPointer` (freeing or resizing a pointer the heap does not
own), `EDivByZero`, `EPrivilege`, and `EExternalException`. `EDivByZero` is in
the family because no JavaScript operator can produce one: `/` and `%` are
IEEE-754 double operations, and BigInt division checks its divisor and throws a
guest `RangeError` first, so an `EDivByZero` at a boundary is an engine bug in
native index or size arithmetic.

Boundaries that suppress rather than convert are guarded too. The iterator-close
helpers implement ES2026 §7.4.10 step 5 — an error from `iterator.return()` must
not replace the abrupt completion that caused the close — with a bare
`try..except end`. That contract is about close errors, and an integrity fault
is not one, so those handlers now re-raise the family and swallow everything
else exactly as before. The same applies to the RegExp compile boundary, which
converts any compile failure into a guest `SyntaxError`: it keeps converting
`EConvertError`, which is how invalid flags are signalled, and re-raises the
family.

Where a boundary does terminal bookkeeping before a re-raise, the integrity guard
does the same bookkeeping. In the testing library that means clearing the
microtask queue and pending fetch completions, and setting the flag that keeps
guest `afterEach` / `onTestFinished` hooks from running while the process
unwinds — the hooks would otherwise execute on the unsound heap.

### Two deliberate carve-outs

**`EOutOfMemory` is not in the family.** It descends from `EHeapMemoryError`
alongside `EInvalidPointer`, and the first draft of the predicate tested the
shared ancestor. That was wrong: the two halves of `EHeapMemoryError` are
opposites. `EInvalidPointer` means the heap's own structures are broken;
`EOutOfMemory` means the allocator declined a request and left the heap exactly
as it was. On FPC 3.2.2 a failed `SetLength` raises a catchable `EOutOfMemory`,
and the engine's own handlers unwind cleanly through it —
`ArrayBufferValue.SetDataLength` releases the bytes it had already reserved
before re-raising, so the byte accounting stays true either way. Making it fatal
would also break a realistic guest program: on 32-bit targets the budget cap is
700 MB against roughly 2 GB of address space, so asking for a 600 MB
`ArrayBuffer`, catching the failure, and continuing with a smaller one is
ordinary defensive code, not a corrupted engine.

**`EStackOverflow` is not in the family.** It descends from `EExternal`
alongside `EAccessViolation`, but the engine converts it to the guest
`RangeError` "Maximum call stack size exceeded" on purpose
(`PascalExceptionToErrorObject`). Recursion depth is a guest-observable limit,
not corruption. No class in the family is an ancestor of it, so it keeps
converting without a special case.

The other exclusions follow the same test — each is raised by the engine on a
path with a defined continuation, so classifying it as an integrity fault would
turn working behavior into a fatal exit: `ERangeError` (deliberate index-bounds
validation), `EIntOverflow` (fires only in non-production builds, so making it
fatal would give dev and production different unwind behavior for the same
arithmetic), `EConvertError` (invalid RegExp flags, malformed TOML integers,
invalid base64 and UTF-8), `EInvalidOpException` / `EInvalidOperation` (FFI,
module and engine domain invariants), and `EInvalidCast`. The authoritative list,
with the reason for each membership and each exclusion, stays in
`Goccia.EngineFault.pas` and is not duplicated elsewhere.

### Host tier: the test runner

Re-raising past every conversion boundary only moves the decision up one level:
it hands the fault to whatever host called `Engine.Execute`. `GocciaTestRunner`
is that host for the entire JavaScript suite, and it had the same generic arms
one tier up — one per execution mode, one in the sequential aggregator, one in
the parallel worker body — each of which relabelled an escaped fault as "this
file failed" and moved on to the next of roughly fifteen hundred files. That is
the boundary bug again with a results row in place of the guest's `catch`: the
suite would go on reporting passes and failures for the rest of the run from a
process that had already lost track of its heap, and the verdict would be
worthless without ever looking wrong.

So the runner **aborts the run** when `IsEngineIntegrityFault` holds at one of
those arms, and at the end-of-run inline-snapshot write-back, whose own arm
would otherwise file a fault over the recorded results as one recorded
finalization failure and publish the results anyway. It writes a diagnostic naming the faulting file and the exception
class to stderr under a fixed `Integrity fault:` prefix, flushes it, stops
dispatching files, and exits `70` — sysexits' `EX_SOFTWARE`, chosen so it is
distinct from `1` (the suite ran and reported failures) and `2` (the invocation
was unusable) and a harness can tell "these tests failed" from "stop believing
this process" (see [CLI
Conventions](../contributing/cli-conventions.md#exit-codes)). The two per-mode
arms only re-raise, which keeps one abort site per tier: the sequential
aggregator for a `--jobs=1` run, the worker body for a parallel one. The abort
is deliberately not an unwind: the aggregation would read the very objects the
fault calls into question, and the summary at the end of it would overwrite the
exit code with a pass/fail verdict the process is in no position to give.

In the parallel case a faulting worker does two things, and it is worth being
precise about which one is load-bearing. It cancels the pool's shared queue, so
peer workers stop reaching for new files; then it ends the process from its own
thread. The first draft stopped at the cancel, reasoning that the pool had a
cleaner stop than halting from a worker and that in-flight files could finish
while `RunAll` returned normally. That is true only while the main thread is
still listening. A worker the watchdog abandoned outlives `RunAll`, so a fault
in it lands after the main thread has already made whatever check it was going
to make: the cancel reaches a queue nobody is draining, and the run prints a
full summary and exits `0` beneath a stderr line that says it was aborted. No
fixed checkpoint fixes that, because the zombie can report at any later moment
— a second check only moves the window. So the thread that knows ends the
process, and the cancel becomes what it always really was, the orderly half.

*How* it ends the process is not `Halt`, and that distinction was paid for.
`Halt` runs unit finalization on the calling thread before the process dies; on
a worker thread that tears down process-wide RTL state — the thread manager
included — while peer workers are still executing tests. The next threading
operation in a peer then fails with the RTL's own `Thread error`
(`sysconst.SThreadError`), the testing library's generic per-test arm converts
it into a recorded test failure, and the abort prints an ordinary-looking red
line for a test that never failed. It reproduced in roughly one aborting run in
seventy-five, and the halt was provably its cause: removing only the worker-side
halt took it to zero in three hundred runs, and forty non-aborting runs of the
same files — each executing every file rather than the handful an abort reaches
— never produced one. The abort therefore terminates through the C runtime
(`_exit`, `ExitProcess` on Windows), running no finalization at all. That
removes the window instead of papering over it, and it trusts the suspect heap
*less*: no finalizer runs on a heap the engine has already said it cannot vouch
for. Buffered stdout dies with the process, which is what an abort wants.

Three smaller mechanics, because the obvious version of each is wrong. The
worker cancels through the pool's cancellation *flag* rather than the pool
object: the pool is freed while an abandoned worker is still running, and it
leaks the flag rather than freeing it for precisely that reason, so the flag is
the only handle a zombie can safely hold. The main thread keeps a check of its
own on a process-level first-fault flag — not because the abort depends on it,
but because a terminating thread waits for the diagnostic to be written and the
main thread can surface from `RunAll` inside that window; without the check it
would spend the window printing a summary that is about to vanish. And that wait
is itself the fix to a second defect: the diagnostic is written by whichever
thread wins the first-fault gate, so a *loser* reaching the exit first would end
the process mid-write and the abort would truncate its own message. That was
seen once per hundred and fifty aborting runs with two or more faulting files,
and never with one — the signature of a race between two threads rather than of
the teardown above. Every thread that terminates now waits for the written flag,
under a bounded timeout so a reporter that dies mid-write cannot hang the abort.

This is one tier's policy, not a guarantee about every exit path. The shared CLI
entry point every Goccia binary runs under (`TGocciaApplication.Run`) still ends
in a generic arm of its own, so a fault raised outside per-file execution and
the snapshot flush — argument parsing, path expansion, config discovery — is
still reported as an ordinary error and exits `1` with no `Integrity fault:`
line. Closing that one is a change to every CLI binary at once and belongs to
its own decision.

The limit family keeps its per-file treatment, and the contrast is the argument.
`TGocciaMemoryLimitError` is uncatchable *by the guest* for the reason above,
but it is still a verdict on one test file delivered by an intact heap: the file
asked for more than the budget allowed, the runner records the refusal, and the
next file's result means exactly what it says. An integrity fault is not a
verdict on the file at all — it is the engine reporting that it can no longer
vouch for anything, that file's result included. Per-file isolation was rejected
for it on exactly that ground: isolation presupposes the failure is contained,
and containment is the one thing an integrity fault disproves.

## Consequences

Embedders can now see these classes escape `Engine.Execute`. They could always
escape from outside a conversion boundary; what changes is that a fault raised
*inside* guest execution no longer stops at the first `catch`. A host that wants
to survive one must decide that for itself — and it should not resume the same
process on the same heap, which is the whole point.

The guard is a class test on an exception that is already being handled, so it
costs nothing on any path that does not throw. It is also not a substitute for
correctness: the faults it catches are engine bugs, and the right fix is always
the missing root or the missing check. What the guard buys is that the bug
surfaces as a crash the next run can reproduce, instead of a script that quietly
kept going. That is the same trade [ADR 0106](0106-sandbox-hardening-scope.md)
made for the hardening programme — find the bugs rather than jail them.

`Goccia.MemoryLimit.Test.pas` covers both families in both execution modes, with
a native that raises `EObjectCheck` from inside guest code and asserts the class
reaches the host rather than the script's `catch`.

The runner's abort has no such automated coverage, and deliberately so: reaching
it needs a fault raised inside a test file's execution, and the only way to
arrange that would be an injection hook in the runner itself — a switch whose
sole purpose is to corrupt a production binary on request. The behaviour was
verified by hand with a temporary local build, in both execution modes and both
`--jobs` shapes; what CI keeps honest is the surrounding contract, that an
ordinary failing file still exits `1` with an unchanged report.
