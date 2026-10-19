# Heap Snapshots

*Writing what a paused run's heap holds and restoring it in a fresh engine, for embedders that resume runs later.*

## Executive Summary

- **Opt-in, for embedders** — a `TGocciaHeapSnapshotSession` writes what a set of roots reaches; a `TGocciaHeapSnapshotRestore` reads it back in a fresh engine of the same build
- **Refusal is normal** — a class without a codec, a built-in the run changed or state outside the heap refuses the snapshot, and the embedder resumes by replay instead
- **Built-ins are named, not written** — a snapshot refers to `Math`, `Array.prototype.map` or a lazily built `Temporal.PlainDate` by name, and the restoring engine resolves its own
- **The reader expects hostile input** — every count, index and length is checked against the input and the limits, the run's memory ceiling and execution limits apply, and every truncation and mutant of a real snapshot is refused or restored, never a fault
- **The pause checkpoint** — an engine with a host-call owner calls `OnPauseCheckpoint` when its run waits on nothing but host calls, and a handler that pauses ends the run with `EGocciaRunPaused`
- **Functions and generators** — closures, the cells they share and suspended generators are written, each function named by its module's bytecode and its place there; the restoring engine must have compiled the same module
- **Still growing** — plain objects, object literals, arrays, symbols and VM state are written; the codec families that follow (modules, built-in differences, the object model and the rest) are listed in [ADR 0148](adr/0148-heap-snapshots-at-idle-points.md)

The design, its trust model and its phases are recorded in
[ADR 0148](adr/0148-heap-snapshots-at-idle-points.md); the prototype it grew
from is the [heap snapshot spike](spikes/heap-snapshot-prototype.md).

## Taking a snapshot

Create the session right after the engine, before anything runs on it: it
names the engine's built-ins then, records their state (a built-in the run
changes refuses the snapshot until built-in differences are written), and
marks where the run's objects begin, which the accounting check counts.

Built-ins the run builds later (a lazily built global such as `Temporal`, an
iterator prototype made on first use) are named too. Lazy members are watched
as they are built, so building one is not a change; a built-in the session
never saw before the run is written with its fingerprint, and the restoring
engine builds the same built-in by name and compares, refusing the snapshot
when the run had changed it.

```pascal
uses
  Goccia.HeapSnapshot,
  Goccia.HeapSnapshot.Codec;

Session := TGocciaHeapSnapshotSession.Create(Engine);
try
  // ... the run builds Value ...
  try
    Session.Snapshot([Value], Output);
  except
    on E: EGocciaHeapSnapshotRefused do
      ResumeByReplayLater(E.Message);
  end;
finally
  Session.Free;
end;
```

`Snapshot` writes what its roots reach from the stream's position and ends
the stream there, or refuses with `EGocciaHeapSnapshotRefused` and leaves the
stream as it was. The run must be idle, as at a pause checkpoint: a running
bytecode frame, a queued job, a pending `Atomics.waitAsync` waiter, pending
work on an extension other than `HostCallOwner`, or state outside the heap
(below) refuses it, and `Engine.SnapshotIdleRefusal` says which. The message
names the reason and, for an object without a codec, the chain of objects
that reached it. The run's timeout and instruction limit still end the run
while the writer works; its memory ceiling, reached by the writer's own
buffers, refuses the snapshot instead.

The **accounting check** (on by default, `CheckAccounting`) collects, then
refuses when a live object the run created is neither written nor a
built-in: it finds objects a codec missed, not fields. Objects the embedder
keeps alive itself go into the baseline with `AddToBaseline`. The collection
runs before anything is written; one that queues a job (a
`FinalizationRegistry` whose target died) refuses the snapshot, since the run
is not idle any more.

## Restoring

```pascal
Restore := TGocciaHeapSnapshotRestore.Create(FreshEngine, Input,
  TGocciaHeapSnapshotLimits.Defaults);
try
  if Restore.Roots[0] is TGocciaValue then
    Value := TGocciaValue(Restore.Roots[0]);
  // ... root what you keep, then free the restore ...
finally
  Restore.Free;
end;
```

The roots come back in the order they were written and stay rooted until
the restore is freed. A root is whatever object was written, and a hostile
snapshot can make it any object it holds, a closure's cell among them, so
check its class before using it. A snapshot from another engine build, format version
or codec schema, one naming a built-in this engine does not have, one over
the limits or one that is malformed is refused with
`EGocciaHeapSnapshotInvalid` (a subclass of `EGocciaHeapSnapshotRefused`).

Authenticate a snapshot before restoring it, for example with an HMAC under a
key no script can reach, and never accept one from a script or a user: the
reader is written for hostile input, but a snapshot that passes its checks
still decides what the restored run holds. Compression is the embedder's:
decompress after authenticating and keep the decoded size within the
reader's input limit.

### Reader limits

`TGocciaHeapSnapshotLimits` bounds what a reader accepts before it allocates
anything for it:

| Field | Default | Bounds |
|---|---|---|
| `MaxInputBytes` | 512 MiB | the snapshot's size |
| `MaxObjects` | 16 Mi | the object table |
| `MaxStrings` | 16 Mi | the string table |
| `MaxStringBytes` | 256 MiB | one string |
| `MaxBuiltins` | 1 Mi | built-in names |
| `MaxCodecs` | 4,096 | codecs named in the header, and modules named in the module section |
| `MaxRoots` | 1 Mi | roots |

The reader also charges its input, its tables and what it restores to the
run's memory ceiling, and polls the run's timeout and instruction limit in
every loop whose length follows the input. A restored BigInt keeps the charge
for its digits for as long as it lives.

## The pause checkpoint

An embedder that owns the run's host calls (a tool call, an approval) names
that extension `HostCallOwner` and sets `OnPauseCheckpoint`. Where the idle
wait would call the owner's `WaitForIdle`, before it blocks, the engine calls
the handler if the run is at a pause checkpoint: no frame running, no job
queued, no `Atomics.waitAsync` waiter, no other extension with pending work,
and pending host calls on the owner. `IsAtPauseCheckpoint` answers the same
question.

```pascal
procedure THost.OnCheckpoint(const AEngine: TGocciaEngine;
  var APause: Boolean);
begin
  try
    FSession.Snapshot(FPendingCallPromises, FOutput);
    APause := True;
  except
    on E: EGocciaHeapSnapshotRefused do
      ; // keep waiting; resume by replay later
  end;
end;

Engine.HostCallOwner := MyToolsExtension;
Engine.OnPauseCheckpoint := Host.OnCheckpoint;
try
  Engine.Execute;
except
  on E: EGocciaRunPaused do
    StoreForLater(FOutput);
end;
```

Leaving `APause` false goes on waiting for the host calls as usual. An
extension that keeps state outside the heap no codec covers reports it
through `HeapSnapshotRefusal` (the timer extension does for any scheduled
timer, a mocked clock or an unreported timer error); while any extension
does, the run is not at a checkpoint, and `Snapshot` refuses too.

Until the module and promise codecs land, a run paused at a checkpoint
holds a suspended module body and pending promises that no codec writes yet,
so a snapshot of a paused run is refused; the values API above already
round-trips the graphs the codecs so far cover.

## Functions and generators

A function is written as its closure: the code it runs, the cells it
shares with other closures, its home object and its own properties. The code
is never written. Each compiled module the engine keeps is named by the
SHA-256 of its bytecode as the bytecode format writes it, its source path
included, and each function's code by its depth-first place in its module;
the restoring engine names its own compiled modules the same way and finds
one with the same bytecode, so it must compile the module from the same
path. Two modules with the same bytecode are interchangeable. A snapshot
naming a module the restoring engine has not compiled is refused. Global
`eval` code has no bytecode file form and so no name: its functions refuse
the snapshot. The hashes are streamed and polled. The writer hashes only the
modules a snapshot names; the reader hashes every module its engine keeps
(eval code aside), once, at the first function it restores.

A suspended generator is written with its frame: its registers, its cells,
its exception handlers and where it resumes. The reader checks that frame
against the code it runs: the resume point on an instruction boundary,
registers and cells within the frame's windows, and each handler one that
the function's own push instructions push, no more of them than it has push
instructions. It charges each frame array to the run's memory ceiling before
allocating it, and the restored generator keeps that charge, as a suspended
frame always does. A register may hold the marker of a binding still in its
temporal dead zone, which stays there until the generator runs the
declaration. A generator inside `yield*`, an async generator and an async
function waiting on a promise refuse until the promise and iterator codecs
land; so does a function whose code closes over a module's scope, until the
module codecs land.

The reader keeps the frame within its code, but cannot tell a forged
register value from a real one: that is what authenticating snapshots is for.

## Writing a codec

A class gets a codec by registering one, by exact class, in the
initialization of the unit that owns the class, which can reach its private
state:

```pascal
RegisterSnapshotCodec(TMyValue, TMyValueSnapshotCodec.Create);
```

A codec has a unique `Tag`, a `SchemaVersion` that changes whenever the
fields it writes change, and four steps: `Write` the object, `Allocate` an
empty shell (it may read non-reference data), `Fill` it from its payload,
reading every byte (references resolve to shells), and optionally `Finish`
once every object is filled. `WriteObjectCore` and `ReadObjectCore` write
and read an ordinary object's prototype, extensibility, integrity and own
properties. A reader codec checks what it reads and calls `Refuse` rather
than guessing, and calls `Poll` once per item in any loop whose length comes
from the input. `source/units/Goccia.Values.ArrayValue.pas` holds the array
codec as an example.

A unit whose realm slot, or owned slot, is filled on first use registers how
to fill it with `RegisterSnapshotSlotMaterializer`, under the slot's name, so
a restore can build the intrinsic a snapshot names (the iterator prototypes,
the function-object intrinsics, Intl's legacy fallback symbol and the segment
prototypes do). A restore builds the built-ins the run built in the order the
run named them, so a slot that building an earlier one fills needs none; one
nothing fills refuses the restore.

## Testing

`source/units/Goccia.HeapSnapshot.Test.pas` round-trips graphs through two
engines and checks them by script, and feeds the reader every truncation
and 10,000 mutants of a real snapshot, and of one holding closures and a
suspended generator, which it then resumes. The fuzz harness restores one input
as a snapshot with `--heap-snapshot <file>` and writes its own seed
snapshots with `--write-heap-snapshot-seeds <dir>`; the scheduled
[fuzz workflow](../.github/workflows/fuzz.yml) fuzzes the reader with
them ([Tooling — Fuzzing](contributing/tooling.md#fuzzing-and-memory-safety)).

`source/units/Goccia.HeapSnapshot.Hardening.Test.pas` takes two real
snapshots apart along the format, one of a plain graph and one holding
closures and suspended generators: every count, index and length of the
header and its tables set to its boundary values, every object entry read by
every other codec, dropped, doubled, swapped and cut short, references to
objects and built-ins pointed at other objects and built-ins, and payload bytes set
to the value tags. Each mutant restores or is refused, and what restores is
used (`THeapSnapshotExerciser` in `Goccia.HeapSnapshot.Exercise`: up to 600
distinct objects, what the run's values hold before any prototype, each
property read, each function called, each generator resumed, returned or
thrown into while it is suspended). It also round-trips deep chains of every
kind of link, refuses at the end of one and stops a restore of one at half
its memory, so no walk recurses per link; restores with the collector's
`CollectBeforeEveryReservation` on, so each reservation the reader makes
collects first; restores under 401 ceilings spread evenly up to what a
restore needs; and restores the same mutants again to check that the heap
stays where it was.

| Size | Payload values and reference targets | Chains | Runs |
|---|---|---|---|
| default | one in 4 at each, rotating | 200,000 links | on every pull request |
| `GOCCIA_HEAP_SNAPSHOT_HARDENING=full` | all | 1,000,000 links | in the scheduled fuzz workflow |

The fuzz workflow also runs every heap snapshot test program at its default
size under heaptrc.
