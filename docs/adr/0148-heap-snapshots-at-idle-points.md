# 0148 - A paused run's heap can be snapshotted at an idle point and restored in a fresh engine, behind an embedder option with replay as the fallback

**Date:** 2026-10-19
**Area:** `embedding`, `garbage collector`, `bytecode runtime`, `modules`
**Related:** [Heap snapshot prototype spike](../spikes/heap-snapshot-prototype.md), [ADR 0103](0103-layered-untrusted-execution-boundaries.md), [ADR 0133](0133-modules-root-their-value-exports.md), [ADR 0142](0142-the-run-waits-until-no-extension-has-pending-work.md), [ADR 0146](0146-embedder-fetch-transport.md), [#1792](https://github.com/frostney/GocciaScript/pull/1792)

## Context

An embedder can pause a run while it waits for a host answer: a tool result,
or a person's approval. It then resumes the run later, possibly in another
process on another machine. The embedder can always resume by **replay**: run
the program again from its start, and answer the host calls it already made
from a record. That works when the run is deterministic, and it costs the
whole run up to the pause again.

A **heap snapshot** taken at the pause can be restored instead. The
prototype on the `prototype/heap-snapshot` branch showed this works for the
core language (see the [spike](../spikes/heap-snapshot-prototype.md)). Its
design was a structured object graph written at an idle point, built-ins
named rather than written, and a refusal whenever something has no codec.

The measurements split by workload:

- Restoring beat replay by 6x to 16x for rendering workloads (markdown, SSR)
  and by hundreds of times for long computations that keep little alive.
- It beat replay by about 3x for data-heavy heaps.
- It was no faster (0.5x to 1x) for short programs that pause within a few
  milliseconds.

An embedder that measured its own programs (document generation, data
processing, test runs and agent loops) found the same split. The spike
recommended waiting for real resume times before building. The decision here
is to build now, because a fast resume of document and data workloads is
wanted before that traffic exists. Replay stays the guarantee, so nothing
depends on snapshots while they mature.

Three limits of the prototype stood out:

- **Imported modules.** Every real program imports a module. The prototype
  writes only the entry module's state, so it refused all of them.
- **Changed built-ins.** Bundled runtimes and test libraries change built-ins
  (`Object.prototype`, `globalThis`). The prototype refuses any changed
  built-in.
- **Host state outside the heap.** Files the run wrote to its sandbox are not
  in the heap, so a restored run did not see them.

## Decision

### The pause checkpoint

**A snapshot is taken only at a pause checkpoint, which the engine defines.**
A run reaches the checkpoint when all of these hold:

- no bytecode frame is running;
- the microtask queue is empty;
- every extension except the one that owns the run's host calls reports no
  pending work (`HasPendingWork`, ADR 0142);
- no `Atomics.waitAsync` waiter is pending: the engine services those outside
  the extensions, and a waiter's buffer subscription and deadline live outside
  the heap;
- the host-call owner reports only host calls the embedder answers.

The engine checks for the checkpoint where it would otherwise call that
owner's `WaitForIdle`, before any blocking wait. If the embedder asked for a
snapshot then, the engine:

1. releases the idle VM's scratch (registers, cells and argument stacks);
2. writes the snapshot;
3. ends the run with a paused result, not with an exception a script can see.

Other pending work at that point means the run is not at a checkpoint: a
timer still due, a fetch in flight (ADR 0146), an `Atomics.waitAsync` waiter,
or an extension's own work.
The snapshot is then refused, and the embedder keeps waiting or replays
later. Every resume point is a pending promise the host owns, and all control
state lives in heap objects: suspended async and generator continuations,
promise reactions, and a module body suspended by top-level `await`.

### The format

**The format is a structured object graph, versioned and self-describing.**
A snapshot contains:

- a header: magic, format version, the engine's build identity, and a schema
  version per codec;
- each module's identity: its bytecode's SHA-256 for a source module, its
  content hash for a data module (JSON, text), and its name for a host or
  runtime module;
- a string table;
- the names of the built-ins it refers to;
- an object table, each entry a tag and a payload;
- the roots:
  - every module's record;
  - the pending host promises, by the embedder's ids;
  - every object the engine roots for the paused run itself (the entry's
    evaluation promise and pending completion);
  - the ordered unhandled-rejection tracker.

Before writing, the engine checks every native temp root against those
roots. A temp root it cannot account for refuses the snapshot. A mismatched
build, schema or module identity refuses the restore.

### Codecs and refusal

**Every class is written by a codec it registers.** A live object with no
codec, or in a state its codec does not handle, refuses the snapshot. A
refusal is a normal outcome: the embedder resumes by replay. Two checks
reduce the risk of a silently wrong restore without removing it:

- an **accounting check** collects and then confirms that every live object
  the run created is either written or named; it detects objects a codec
  missed, not fields;
- a **round-trip lane** over the test suite detects missing fields for the
  states the tests reach.

A codec's schema version changes with its fields, and each codec has its own
round-trip tests.

### Built-ins

**Built-ins are named, not written.** A built-in is named by the path that
reaches it from a fresh engine's roots: the global object, the global scope,
the realm's slots and shared prototypes, and the well-known symbols.

- Lazily built members are resolved by walking their path on restore, which
  builds exactly what the path crosses.
- The internals of built-ins written in JavaScript and built on first use are
  named through the module or scope that holds them.
- A built-in the run changed is written as differences from the same
  built-in in a fresh engine with the same lazy members built. A difference
  covers:
  - added, deleted and redefined own properties, string- and symbol-keyed,
    with their data or accessor descriptors and attributes;
  - a changed prototype;
  - a changed `[[Extensible]]` flag (`Object.preventExtensions`), recorded
    apart from sealing and freezing;
  - a changed integrity level.

  A restore applies the differences after the built-in table is resolved and
  before the object graph is filled: property and prototype changes in the
  order they were recorded, then extensibility and integrity. Each kind of
  difference has a round-trip test.
- A change to a built-in's native internal state (its slots) refuses unless
  a codec covers that state.

### Modules

**Every module's state is written, and a restore evaluates no module body.**
The embedder gives the restore the same module graph, its sources through
the engine's source provider. The loader gains a restore operation that:

1. links the graph without evaluating it;
2. checks each module's identity;
3. fills every module record with its restored environment, exports,
   namespace, evaluation state, evaluation promise and async-cycle root;
4. retires the records' load states as evaluated, so a later import or
   dynamic `import()` finds them evaluated and runs no body.

How each kind of module is restored:

- **Source modules** are restored as above.
- **Data modules** are read again and checked by content hash.
- **Host and runtime modules** (`fs`, `goccia:*`, an embedder's own modules)
  are created by the engine as usual. Each is identified by its name and a
  version: the engine build for the engine's own modules, and for a host
  module a version the embedder supplies when it registers the module, which
  it changes whenever the module's exports or behaviour change. A host module
  registered without a version refuses the snapshot. Their state is written
  only through their codecs; without one they refuse if the run changed them.
- **Pending dynamic imports and deferred namespaces** refuse until a codec
  covers them.

### Trust and resource bounds

**Snapshots are untrusted input, even though the engine produces them.** A
snapshot encodes VM state such as instruction pointers, register counts and
handler stacks. Two rules follow:

- The embedder must authenticate every snapshot it restores (for example with
  an HMAC under a key the script cannot reach), and must never accept one
  from a script or a user.
- The reader is still written for hostile input, as ADR 0103 requires of
  untrusted processing:
  - it checks every count, index and length with overflow-safe arithmetic,
    and against limits on the input's total size, the number of objects and
    strings, string lengths and nesting depth;
  - it polls the run's execution limits (`CheckNativeWork`, as ADR 0103
    requires of native parsers) in every loop whose length follows the input:
    the passes over the object table, codec loops, string decoding and
    decompression;
  - it charges restored objects and its temporary tables to the run's memory
    ceiling before allocating them;
  - it checks VM state against the bytecode it belongs to (instruction
    pointers on instruction boundaries, register and handler counts within
    the frame's template);
  - it refuses rather than guesses;
  - it is fuzzed with mutated real snapshots under heaptrc and
    memory-checking tools.

### Iterative walks

**Every walk over the heap is iterative.** The writer numbers objects
breadth first, the reader restores in passes over the object table, and the
census and the accounting check use the collector. No walk recurses natively
once per object, the rule [#1792](https://github.com/frostney/GocciaScript/pull/1792)
sets for marking. Each recursive object shape (chains, nesting, deep closures
and prototype chains) has a deep-chain round-trip test.

### State outside the heap

**State outside the heap is recorded or left to the embedder.**

- **Recorded and checked by the engine:** the deterministic clock, the engine
  options and compatibility flags, and the limits the run started with. A
  mismatch refuses the restore.
- **The random source.** The host random provider (`IGocciaHostRandom`) gains
  an optional, versioned interface to save and restore its state. The default
  provider implements it, so `Math.random` continues the same sequence after a
  restore; a provider without it refuses the snapshot.
- **Execution budgets.** The instructions the run has consumed and its active
  instruction-limit scopes are recorded, and the restored run continues with
  what was left, so restoring never grants a fresh budget. Time limits count
  only time the run executes: the interval the snapshot is stored does not
  count, and each active timeout scope is re-armed with the time it had left
  at the pause. An embedder may give a restored run tighter limits.
- **Carried by the embedder:** its own host state. That includes the sandbox
  file system the run wrote to (or the run's file changes), the ids of its
  pending host calls, and the authentication.

### The API

**The API is opt-in.**

- An embedder asks its engine for a snapshot at the next pause checkpoint.
- It restores with a source provider and the snapshot, then answers the
  pending host calls by its ids.
- An engine that is never asked pays only for its registered codec table.

## Consequences

- A resume that would replay a long run can restore in milliseconds. Replay
  stays the guarantee: any refusal, mismatch or failed authentication falls
  back to it, so no embedder depends on snapshots.
- Every new built-in class needs a codec or an explicit refusal, and a change
  to a codec-covered class's fields needs its codec and schema version
  updated. This is a standing cost on built-in work. The accounting check and
  the round-trip lane detect some omissions, not all.
- Codecs read private state of many classes, so the snapshot units depend on
  the value units. Each family of codecs lives beside the classes it writes,
  registered with one registry.
- Delivery is phased:
  1. the format, the registry, built-in naming, the pause checkpoint and API,
     and the reader's bounds and fuzzing;
  2. the VM-state codecs;
  3. the codec families (modules, built-in differences, the object model,
     control state, promise machinery, collections and buffers, Temporal and
     Intl, host objects);
  4. the round-trip lane.

  Until a family lands, its classes refuse.

## Alternatives rejected

- **A process snapshot (CRIU and the like).** It is not portable across
  machines or platforms, it is as large as the process, and it captures
  native state the engine does not own.
- **Writing the VM's memory as it is.** Pointers, allocator state and
  per-process identities do not survive a new process.
- **Snapshots at any point, including inside a running frame.** That would
  need native stack capture. At the checkpoint all control state is already
  in the heap.
- **Refusing every changed built-in.** That refuses the bundled runtimes and
  test libraries real programs use.
- **Re-running imported modules on restore and naming their state.** This is
  the prototype's measurement shortcut. It is wrong for any module whose
  state the run changed, such as a cache filled during the run.
- **Waiting for real resume times before building.** This was the spike's
  recommendation. It was overridden because fast resumes of document and
  data workloads are wanted first, and replay keeps the risk bounded.
