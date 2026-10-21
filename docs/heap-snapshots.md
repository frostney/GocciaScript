# Heap Snapshots

*Writing what a paused run's heap holds and restoring it in a fresh engine, for embedders that resume runs later.*

## Executive Summary

- **Opt-in, for embedders** — a `TGocciaHeapSnapshotSession` writes what a set of roots reaches; a `TGocciaHeapSnapshotRestore` reads it back in a fresh engine of the same build
- **Refusal is normal** — a class without a codec or state outside the heap refuses the snapshot, and the embedder resumes by replay instead
- **Built-ins are named, not written** — a snapshot refers to `Math`, `Array.prototype.map` or a lazily built `Temporal.PlainDate` by name, and the restoring engine resolves its own
- **Changed built-ins are written as differences** — a built-in the run changed (`Object.prototype.polyfill = …`, `globalThis.answer = 42`, `Object.freeze(Math)`) is written as its whole own state, which the restore gives the same built-in last
- **The reader expects hostile input** — every count, index and length is checked against the input and the limits, the run's memory ceiling and execution limits apply, and every truncation and mutant of a real snapshot is refused or restored, never a fault
- **The pause checkpoint** — an engine with a host-call owner calls `OnPauseCheckpoint` when its run waits on nothing but host calls, and a handler that pauses ends the run with `EGocciaRunPaused`
- **Functions and generators** — closures, the cells they share and suspended generators are written, each function named by its module's bytecode and its place there; the restoring engine must have compiled the same module
- **Still growing** — plain objects, object literals, arrays, symbols, VM state and built-in differences are written; the codec families that follow (modules, the object model and the rest) are listed in [ADR 0148](adr/0148-heap-snapshots-at-idle-points.md)

The design, its trust model and its phases are recorded in
[ADR 0148](adr/0148-heap-snapshots-at-idle-points.md); the prototype it grew
from is the [heap snapshot spike](spikes/heap-snapshot-prototype.md).

## Taking a snapshot

Create the session right after the engine, before anything runs on it: it
names the engine's built-ins then, records their state (a built-in the run
changes is written as its difference, see
[Built-ins the run changed](#built-ins-the-run-changed)), and marks where
the run's objects begin, which the accounting check counts.

Built-ins the run builds later (a lazily built global such as `Temporal`, an
iterator prototype made on first use) are named too. Lazy members are watched
as they are built: what one holds is named and recorded right then, so
building it is not a change and changing it later is a difference. A
built-in filled on first use some other way (natively, as the iterator
prototypes are) is named before the snapshot and written with its
fingerprint; the restoring engine builds the same built-in by name and
compares, refusing the snapshot when the run had changed it. A built-in's
state is its prototype, flags and
own properties, and for a function also its kind, whether its code is a
shim's, and its synthesized `length` and `name`: a run's function standing
where a built-in's was is not the engine's own.

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
built-in: it finds objects a codec missed, not fields. What the named
built-ins reach counts as theirs: a built-in written in JavaScript (a shim
such as `Date`) keeps its module scope, cells and private functions, built
when the run first used it, and those are not missed objects. Only what a
shim's load created counts as its own, and only when the load ran no code
but shims' (the engine marks the code it compiles from a shim). A shim
loads with the built-ins the engine had before any script ran (`WeakMap`,
`Object.defineProperty`, `Number.parseInt` and `Number.parseFloat`, bound in
its own scope), so what the run did to those globals does not reach it; a
load that still ran a run's function, on any engine of the thread (a getter
the run put on `Object.prototype`, say), has all it made counted. What the
run's own top-level bindings hold is counted too.
Should the collector's mark stack fail
to grow while it marks what built-ins reach, the snapshot is refused, since
its recovery traces the global scope as well. Objects the embedder
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

## Built-ins filled on first use

A snapshot may name a built-in by the realm slot, or owned slot, that holds
it, and a slot filled on first use may be named before anything else that
fills it, as when a runtime extension loads the `Date` shim natively. So the
unit that owns such a slot registers, under the slot's name, how a restore
fills it:

- `RegisterSnapshotSlotMaterializer`, a procedure that builds it in the
  current realm (the iterator prototypes, the function-object intrinsics,
  Intl's legacy fallback symbol, the segment and module source prototypes,
  and the `hasOwnProperty` shim, which native code can load although no
  script installs it);
- or `RegisterSnapshotSlotGlobalPath`, the lazy built-in whose first read
  fills it, by its path from the global object (`Temporal.PlainDate` for
  Temporal's and Intl's shared prototypes, `DisposableStack`, and for each
  shim its global or prototype member, `Date` for the `Date` shim's own
  slots).

Every slot a run can fill resolves by its name on its own in a fresh engine.
`Goccia.HeapSnapshot.Builtins.Test` builds everything an engine builds lazily
and checks each slot's names, and lists the slots nothing fills: `eval`,
which the host installs, and the shared prototypes of runtime extensions an
engine without them never builds.

An extension whose native built-in is created only when imported implements
`MaterializeHeapSnapshotBuiltin` on its engine or runtime extension. The
resolver requests a missing `%builtin` name through that hook, then checks
the resulting object's fingerprint. The hook must build only the named
native built-in, without guest execution or host I/O, and leave unknown names
alone. The data-format extensions use it for their native namespace objects.
FFI slot materializers rebuild shared prototypes without opening libraries;
ordinary FFI instances still require a codec and remain refused.

A `Date` keeps its time value outside the object, in the `Date` shim's
WeakMap, which is a built-in and named, not written: an object with a time
value refuses the snapshot until a codec covers it.

## Built-ins the run changed

A built-in whose state differs from what the session recorded is written in
its own section (format version 3, between the object table and the roots)
as its whole own state: its prototype, its extensibility and integrity, a
function's or a class's synthesized `length` and `name`, an array's length
and elements, and every own property, string- and symbol-keyed, in its
final order. A member the engine built and the run left alone, or one still
lazy, is written as kept: the restoring engine's own member of that key,
lazy or built, stays. The run's objects a changed built-in holds are written like
any others, and the naming walk does not reach them through it.

The reader checks each difference against the built-in it names in this
engine, as it checks a restored object: no built-in changed twice, flags that
agree with each other and with every member (kept ones included), parts that
fit the built-in's kind, a kept member the built-in has, an array's elements
within its length, no new prototype for `Object.prototype` and no prototype
chain that loops through a changed built-in. The differences are applied
last, after the roots are read and every check has passed: each built-in's
new properties and elements are built aside first (under the run's memory
ceiling and execution limits), then installed in a step that allocates
nothing and cannot fail, so a refused restore leaves this engine's built-ins
as they were. [ADR 0152](adr/0152-heap-snapshot-builtins-commit-last.md)
replaces ADR 0148's earlier ordering; nothing the fill does reads a built-in's
own state. Resolving names can
still build lazy built-ins, which a later restore into the same engine
cannot tell from changes: restore into a fresh engine. To snapshot a restored run again, create the session before
the restore, as for any run; the restore is then the run's first change.

A change to a built-in's native internal state (an error's or a RegExp's
data, a Date's time value) has no difference yet, and refuses the snapshot.

## Testing

`source/units/Goccia.HeapSnapshot.Test.pas` round-trips graphs through two
engines and checks them by script, and feeds the reader every truncation
and 10,000 mutants of a real snapshot, and of one holding closures and a
suspended generator, which it then resumes.
`Goccia.HeapSnapshot.Builtins.Test` round-trips each kind of difference,
checks the reader's refusals of impossible ones, restores every cut and
11,000 mutants of snapshots with differences, and restores under rising
memory ceilings and deadlines, every refusal leaving the built-ins as they
were. The fuzz harness restores one input
as a snapshot with `--heap-snapshot <file>` and writes its own seed
snapshots with `--write-heap-snapshot-seeds <dir>`; the scheduled
[fuzz workflow](../.github/workflows/fuzz.yml) fuzzes the reader with
them ([Tooling — Fuzzing](contributing/tooling.md#fuzzing-and-memory-safety)).

`source/units/Goccia.HeapSnapshot.Hardening.Test.pas` takes two real
snapshots apart along the format, one of a plain graph with built-ins the
run changed and one holding closures and suspended generators: every count, index and length of the
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

### Round-tripping the JavaScript suite

`./build/GocciaTestRunner -P tests --heap-snapshots --silent --no-progress`
collects each distinct object passed to `expect()` and keeps it rooted until
that file finishes. It writes each object at file idle, with accounting off,
then restores each accepted object into its own fresh engine that compiled
the same file without running it. The source's retained compiled modules are
also copied through the bytecode format without execution, preserving generated
private identifiers and code from imports and dynamically compiled functions.
Objects are checked in their **final file state**, not their state at the
assertion: writing inside `expect()` would violate the idle-frame requirement.

The checker compares a native description of identities, prototypes, stored
properties and descriptors, symbols, array holes and lengths, and function
name/length state. It does not call getters, proxy traps, `toJSON` or testing
serializers. Re-snapshot byte equality also checks state owned by the codecs.
A writer refusal is counted by reason; a restore refusal, either mismatch or
an unexpected exception fails the file. An integrity fault aborts the runner.

This diagnostic mode runs files serially, including when `--jobs` is supplied.
Counts and refusal reasons go to stderr, leaving JSON stdout intact. PR CI
and the Linux x64 full-CI job run the mode over `tests/` and retain its report,
so newly supported codec families become checked without another corpus.
