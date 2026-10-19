# 0148 - A paused run's heap can be snapshotted at an idle point and restored in a fresh engine, behind an embedder option with replay as the fallback

**Date:** 2026-10-19
**Area:** `embedding`, `garbage collector`, `bytecode runtime`, `modules`
**Related:** [Heap snapshot prototype spike](../spikes/heap-snapshot-prototype.md), [ADR 0133](0133-modules-root-their-value-exports.md), [ADR 0142](0142-the-run-waits-until-no-extension-has-pending-work.md), [ADR 0146](0146-embedder-fetch-transport.md), [#1792](https://github.com/frostney/GocciaScript/pull/1792)

## Context

An embedder can pause a run while it waits for a host answer: a tool result,
or a person's approval. It then resumes the run later, possibly in another
process on another machine. The embedder can always resume by **replay**: run
the program again from its start, and answer the host calls it already made
from a record. That works when the run is deterministic, and it costs the
whole run up to the pause again.

A **heap snapshot** taken at the pause can be restored instead. The
prototype on the `prototype/heap-snapshot` branch showed this works and
measured where it pays off (see the [spike](../spikes/heap-snapshot-prototype.md)).
Its design was a structured object graph written at an idle point, built-ins
named rather than written, and a refusal whenever something has no codec.

The prototype also measured programs a sandboxing embedder runs: document
rendering, data processing, test runs and agent loops. Restoring was 3.8x to
6.1x faster than replay for programs that did a lot of work before their pause
and keep little alive. For short programs that pause within a few
milliseconds, restoring was as fast as replay or slower.

Three limits of the prototype stood out in those measurements:

- **Imported modules.** Every real program imports a module. The prototype
  writes only the entry module's state, so it refused all of them.
- **Changed built-ins.** Bundled runtimes and test libraries change built-ins
  (`Object.prototype`, `globalThis`). The prototype refuses any changed
  built-in.
- **Host state outside the heap.** Files the run wrote to its sandbox are not
  in the heap, so a restored run did not see them.

## Decision

**A snapshot is taken only at an idle point.** An idle point is when no
bytecode frame is running and the microtask queue is empty. Every resume
point is then a pending promise the host owns (a host call), and all control
state lives in heap objects: suspended async and generator continuations,
promise reactions, and a module body suspended by top-level `await`. The
engine releases the idle VM's scratch (registers, cells and argument stacks)
before it writes a snapshot.

**The format is a structured object graph, versioned and self-describing.**
A snapshot contains:

- a header: magic, format version, the engine's build identity, and a schema
  version per codec;
- the SHA-256 of every module's bytecode;
- a string table;
- the names of the built-ins it refers to;
- an object table, each entry a tag and a payload;
- the roots: every module's environment and the pending host promises, by
  the embedder's ids.

A mismatched build, schema or bytecode hash refuses the restore.

**Every class is written by a codec it registers.** A live object with no
codec, or in a state its codec does not handle, refuses the snapshot. A
refusal is a normal outcome: the embedder resumes by replay. Two checks back
this up:

- an **accounting check** collects and then confirms that every live object
  the run created is either written or named, so state a codec missed refuses
  rather than restoring silently wrong;
- a **round-trip lane** over the test suite catches fields a codec forgot.

**Built-ins are named, not written.** A built-in is named by the path that
reaches it from a fresh engine's roots: the global object, the global scope,
the realm's slots and shared prototypes, and the well-known symbols.

- Lazily built members are resolved by walking their path on restore, which
  builds exactly what the path crosses.
- The internals of built-ins written in JavaScript and built on first use are
  named through the module or scope that holds them.
- A built-in the run changed is written as its differences from a fresh one.
  The prototype refused instead; production does not.

**Every module's state is written, and a restore evaluates no module body.**
The embedder gives the restore the same module graph, its sources through the
engine's source provider. The engine links it, checks each module's bytecode
hash, then restores every module's environment, its namespace and the
entry's import bindings instead of running the module bodies.

**Snapshots are untrusted input, even though the engine produces them.** A
snapshot encodes VM state such as instruction pointers, register counts and
handler stacks. Two rules follow:

- The embedder must authenticate every snapshot it restores (for example with
  an HMAC under a key the script cannot reach), and must never accept one
  from a script or a user.
- The reader is still written for hostile input:
  - it bounds-checks every count, index and length;
  - it checks VM state against the bytecode it belongs to (instruction
    pointers on instruction boundaries, register and handler counts within
    the frame's template);
  - it refuses rather than guesses;
  - it is fuzzed with mutated real snapshots under heaptrc and
    memory-checking tools.

**Every walk over the heap is iterative.** The writer numbers objects
breadth first, the reader restores in passes over the object table, and the
census and the accounting check use the collector. No walk recurses natively
once per object, the rule [#1792](https://github.com/frostney/GocciaScript/pull/1792) sets for marking. Each recursive object
shape (chains, nesting, deep closures and prototype chains) has a deep-chain
round-trip test.

**State outside the heap is recorded or left to the embedder.**

- **Recorded and checked by the engine:** the random source's state, the
  deterministic clock, the engine options and compatibility flags, and the
  limits the run started with. A mismatch refuses the restore.
- **Refused:** pending host work other than the host calls the embedder
  answers, such as a timer due or a fetch in flight (ADR 0146), unless that
  work's owner provides a codec for it.
- **Carried by the embedder:** its own host state. That includes the sandbox
  file system the run wrote to (or the run's file changes), the ids of its
  pending host calls, and the authentication.

**The API is opt-in.** The engine gains snapshot and restore entry points.
An embedder asks for a snapshot at an idle point. It restores with a source
provider and the snapshot, then answers the pending host calls by its ids. An
engine that is never asked carries no cost beyond a registered codec table.

## Consequences

- A resume that would replay a long run can restore in milliseconds. Replay
  stays the guarantee: any refusal, mismatch or failed authentication falls
  back to it, so no embedder depends on snapshots.
- Every new built-in class needs a codec or an explicit refusal, and a change
  to a codec-covered class's fields needs its codec and schema version
  updated. This is a standing cost on built-in work, which the round-trip
  lane and the accounting check enforce.
- Codecs read private state of many classes, so the snapshot units depend on
  the value units. Each family of codecs lives beside the classes it writes,
  registered with one registry.
- Delivery is phased:
  1. the format, the registry, built-in naming, the engine API and the
     reader's fuzzing;
  2. the VM-state codecs;
  3. the codec families (modules, the object model, control state, promise
     machinery, collections and buffers, Temporal and Intl, host objects);
  4. the round-trip lane.

  Until a family lands, its classes refuse.

## Alternatives rejected

- **A process snapshot (CRIU and the like).** It is not portable across
  machines or platforms, it is as large as the process, and it captures
  native state the engine does not own.
- **Writing the VM's memory as it is.** Pointers, allocator state and
  per-process identities do not survive a new process.
- **Snapshots at any point, including inside a running frame.** That would
  need native stack capture. At idle points all control state is already in
  the heap.
- **Refusing every changed built-in.** That refuses the bundled runtimes and
  test libraries real programs use.
- **Re-running imported modules on restore and naming their state.** This is
  the prototype's measurement shortcut. It is wrong for any module whose
  state the run changed, such as a cache filled during the run.
