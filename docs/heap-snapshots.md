# Heap Snapshots

*Writing what a paused run's heap holds and restoring it in a fresh engine, for embedders that resume runs later.*

## Executive Summary

- **Opt-in, for embedders** — a `TGocciaHeapSnapshotSession` writes what a set of roots reaches; a `TGocciaHeapSnapshotRestore` reads it back in a fresh engine of the same build
- **Refusal is normal** — a class without a codec, a built-in the run changed or state outside the heap refuses the snapshot, and the embedder resumes by replay instead
- **Built-ins are named, not written** — a snapshot refers to `Math`, `Array.prototype.map` or a lazily built `Temporal.PlainDate` by name, and the restoring engine resolves its own
- **The reader expects hostile input** — every count, index and length is checked against the input and the limits, the run's memory ceiling and execution limits apply, and every truncation and mutant of a real snapshot is refused or restored, never a fault
- **The pause checkpoint** — an engine with a host-call owner calls `OnPauseCheckpoint` when its run waits on nothing but host calls, and a handler that pauses ends the run with `EGocciaRunPaused`
- **A foundation** — this release writes plain objects, object literals, arrays and symbols; the codec families that follow (VM state, modules, built-in differences, the object model and the rest) are listed in [ADR 0148](adr/0148-heap-snapshots-at-idle-points.md)

The design, its trust model and its phases are recorded in
[ADR 0148](adr/0148-heap-snapshots-at-idle-points.md); the prototype it grew
from is the [heap snapshot spike](spikes/heap-snapshot-prototype.md).

## Taking a snapshot

Create the session right after the engine, before anything runs on it: it
names the engine's built-ins then, records their state (a built-in the run
changes refuses the snapshot until built-in differences are written), and
records the objects alive before the run, which the accounting check does
not count.

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

`Snapshot` writes what its roots reach, or refuses with
`EGocciaHeapSnapshotRefused` and leaves the stream as it was. The message
names the reason and, for an object without a codec, the chain of objects
that reached it. The run's timeout and instruction limit still end the run
while the writer works; its memory ceiling, reached by the writer's own
buffers, refuses the snapshot instead.

The **accounting check** (on by default, `CheckAccounting`) collects, then
refuses when a live object the run created is neither written nor a
built-in: it finds objects a codec missed, not fields. Objects the embedder
keeps alive itself go into the baseline with `AddToBaseline`.

## Restoring

```pascal
Restore := TGocciaHeapSnapshotRestore.Create(FreshEngine, Input,
  TGocciaHeapSnapshotLimits.Defaults);
try
  Value := TGocciaValue(Restore.Roots[0]);
  // ... root what you keep, then free the restore ...
finally
  Restore.Free;
end;
```

The roots come back in the order they were written and stay rooted until
the restore is freed. A snapshot from another engine build, format version
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
| `MaxCodecs` | 4,096 | codecs named in the header |
| `MaxRoots` | 1 Mi | roots |

The reader also charges its input, its tables and what it restores to the
run's memory ceiling, and polls the run's timeout and instruction limit in
every loop whose length follows the input.

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
  if AEngine.HeapSnapshotRefusal <> '' then
    Exit; // state outside the heap: keep waiting, replay later
  FSession.Snapshot(FPendingCallPromises, FOutput);
  APause := True;
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
timer, a mocked clock or an unreported timer error); `Snapshot` refuses while
any extension does.

Until the VM-state and module codecs land, a run paused at a checkpoint
holds a suspended module body and pending promises that no codec writes yet,
so a snapshot of a paused run is refused; the values API above already
round-trips the graphs the foundation's codecs cover.

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

## Testing

`source/units/Goccia.HeapSnapshot.Test.pas` round-trips graphs through two
engines and checks them by script, and feeds the reader every truncation
and 10,000 mutants of a real snapshot. The fuzz harness restores one input
as a snapshot with `--heap-snapshot <file>` and writes its own seed
snapshots with `--write-heap-snapshot-seeds <dir>`; the scheduled
[fuzz workflow](../.github/workflows/fuzz.yml) fuzzes the reader with
them ([Tooling — Fuzzing](contributing/tooling.md#fuzzing-and-memory-safety)).
