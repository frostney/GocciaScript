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
