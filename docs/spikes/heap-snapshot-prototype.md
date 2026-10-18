# Heap Snapshot Prototype

Point-in-time investigation of snapshotting a paused run's heap at an idle point and restoring it in a fresh process

## Executive Summary

- A structured object-graph snapshot taken at an idle pause point (every
  microtask has run, the run waits on a host promise) restores correctly in a
  fresh process for all 15 measured workloads: closures, async functions
  between awaits, a suspended generator inside `for...of`, a module suspended
  by top-level `await`, classes with private fields, `Promise.all`, RegExps,
  typed arrays, Maps and Sets, and five AWFY programs.
- Restoring costs about 2-20 ms for small and medium heaps, most of it
  creating the engine and compiling the module, which a replay pays too. The
  graph itself restores at roughly 1.3-1.8 µs per serialized object on the
  larger heaps.
- A snapshot beats replaying from the start only when the run did much more
  work before the pause than it keeps alive: 6-650x for long computations
  with small state (SSR, markdown, image processing, AWFY Richards, Havlak,
  Json). Short I/O-bound runs that pause within a few milliseconds replay as
  fast as they restore (0.5-1.0x), and heap-heavy data processing gains only
  about 3x.
- Snapshots are 0.3-0.7 bytes per byte of live heap uncompressed; zstd at
  level 3 brings anything above a few KiB to 3-7% of that.
- At the test suite's sampled await points, every heap held classes the
  prototype has no codec for (module namespaces, error objects, async
  generators, async context, test-runner natives). The prototype covers the
  core language; the long tail is the built-in library.
- The prototype took about 1.2 agent hours of active work. Estimate to
  production quality: about 30 agent hours (range 20-45), plus a standing
  cost on every future built-in. A coverage-gated first version that always
  falls back to replay is about 13 agent hours (range 9-20).
- Recommendation: keep replay as the guarantee and do not productize
  snapshots yet. Measure how long real resumes take to replay; build the
  coverage-gated version only if a meaningful share of resumes replay for
  more than about 100 ms.

## Question

The use case is an embedder that pauses a run while it waits for a host
answer (a tool result, a human approval) and resumes it later, possibly on
another machine. Replaying the program from its start, answering earlier host
calls from a log, is the guarantee. The question is whether a heap snapshot
taken at the pause can be a faster way to resume, what it costs, and how much
work a production version is.

The design under test is a structured graph serialization at idle pause
points, with replay as the fallback whenever the snapshot refuses:

- Snapshots are taken only at idle points: no bytecode frame is running and
  the microtask queue is empty, so the only live control state is in heap
  objects (suspended async frames, generator continuations, promise
  reactions).
- Each object is written by a codec its class registers. A class without a
  codec, or a state a codec does not handle, refuses the snapshot.
- Built-ins are not written. They are named by a path from a root of a fresh
  engine and resolved by name on restore.
- Restore recompiles the same source, checks the module's bytecode hash,
  skips the module body, rebuilds the graph and settles the pending host
  promises.

## What Was Built

The prototype lives on the `prototype/heap-snapshot` branch.

| Piece | Source |
| --- | --- |
| Format, writer, reader, plain object and symbol codecs | `source/units/Goccia.HeapSnapshot.Codec.pas` |
| Built-in table, late intrinsics, path resolver | `source/units/Goccia.HeapSnapshot.pas` |
| Live-class census (baseline, per-class counts) | `source/units/Goccia.HeapSnapshot.Census.pas` |
| Probe host (`run`, `census`, `snapshot`, `restore`) | `source/app/GocciaHeapSnapshotProbe.dpr` |
| Measurement harness | `scripts/heap-snapshot-measure.ts` |
| Workloads | `fixtures/heap-snapshot/` |

The format is a header (magic, format version, SHA-256 of the entry module's
bytecode), the names of the built-ins the snapshot refers to, a table of
objects (tag, payload) and the roots: the entry module's lexical bindings and
the pending host-call promises. Primitives are written inline. Restore runs
three passes over the object table: allocate an empty shell per object, fill
each payload (references resolve to shells), and finish state that needs
other objects filled first. Every shell is rooted until the restore ends.

Codecs exist for: plain and literal objects (descriptors, accessors,
integrity levels, symbol keys), arrays with holes, `Map`, `Set`, compiled
classes and their instances (private names are remapped to the restored
class's brand), bytecode functions with their upvalue cells, generator and
async continuations (registers, local cells, handler stack, resume state),
await handlers, promises with their reactions and capabilities,
`Promise.all`'s state and element functions, native functions whose callback
is registered by name (promise resolving functions, `Promise.all`),
`ArrayBuffer` (resizable, detached, immutable), typed arrays, array
iterators, RegExps (compiled again from source and flags), the error stack
slot of a plain object, local and registered symbols, and a module body
suspended by top-level `await`.

Built-ins are numbered by a breadth-first walk from the global object, the
global scope, the realm slots, the realm's shared prototypes, every live
builtin's own object and the well-known symbols. Paths separate their
segments with a control character, because realm slot names contain dots.
The walk does not materialize lazy members: materializing every lazy global
(Intl, Temporal, the bootstrap shims) cost about 30 ms per engine in a
production build, against about 1 ms for the walk. Before a snapshot the
table is walked again, so built-ins the run materialized since are named
too; on restore, a name the table does not hold is resolved by walking its
path, which materializes exactly the members on it. Realm slots filled on
first use (an array iterator's prototype) are created through registered
materializers. A fingerprint of every built-in taken at engine start refuses
a snapshot when the script changed a built-in.

Two safety nets make a wrong snapshot unlikely to restore silently. The
accounting check collects, then refuses when any live object the run created
is neither serialized nor a built-in; this caught native-held state the walk
missed. And the reader bounds-checks every count, index and payload length
and refuses rather than guessing.

### Problems Found on the Way

| Problem | Resolution in the prototype |
| --- | --- |
| A zero count underflowed a `Cardinal` loop bound | Integer loop variables |
| FPC emits one VMT per unit that specializes `TOrderedStringMap<TGocciaValue>`; the development build's object check refused virtual calls across units | Calls made in the unit that owns the specialization, or with object checks off around the helpers (see [code-style](../contributing/code-style.md)) |
| An array's stored length trails its element count until read | Write the length a script would read |
| Materializing all lazy built-ins dominated every snapshot and restore | Lazy walk plus path resolution, above |
| The register, cell and argument stacks keep what the last frames left in them, and the collector marks those slots, so garbage looked live | `TGocciaVM.ReleaseIdleScratch` clears them at the idle point before a snapshot |
| Restoring a handler before its state was filled | Cross-object checks moved to the finish pass |

## Method

Machine: Linux x86_64, 32 cores, FPC 3.2.2, production build (`-O4`), branch
head at the time of measurement. Each mode ran 5 times; the table shows
medians. To re-run:

```bash
./build.pas --prod heapsnapshotprobe
cp build/GocciaHeapSnapshotProbe /tmp/probe   # keep the binary fixed while building other targets
bun scripts/heap-snapshot-measure.ts workloads /tmp/probe --reps=5 \
  --awfy-dir=<are-we-fast-yet>/benchmarks/JavaScript --json=results.json
```

For each workload the harness runs the program uninterrupted in `run` mode
(the replay), takes a snapshot at the chosen pause in `snapshot` mode,
compresses the file with gzip level 6 and zstd level 3, restores it in a
fresh process in `restore` mode, and checks that the output printed before
the pause plus the output after the restore equals the uninterrupted output.

- **Live objects** and **live heap**: every collector-managed object the run
  created that survives a collection at the pause, and the collector's byte
  count for them (instance sizes plus charged external bytes).
- **Snapshot objects**: the objects in the snapshot's table. Strings, numbers
  and booleans are written inline and not counted.
- **Serialize**: walking the graph and writing it, including the re-walk of
  the built-in table. The accounting check is reported separately below.
- **Restore**: rebuilding the graph from the file. **Restore total**: from the
  start of the process's work to the moment the run can resume, including
  engine creation, the built-in table and compiling the module.
- **Replay**: from the start of the process's work to the same pause in an
  uninterrupted run. It includes engine creation and compilation, as a
  replay would, and answers earlier host calls immediately, as a replay
  from a log would.

The AWFY workloads use the repository's AWFY bundle builder and the AWFY
lane's compatibility flags; each runs a number of benchmark iterations,
pauses on a host call, then runs one more verified iteration.
`json-data-x4` is `json-data` with four times the orders.

## Results

| Workload | Live objects | Live heap KiB | Snapshot objects | Raw KiB | gzip KiB | zstd KiB | Serialize ms | Restore ms | Restore total ms | Replay ms | Replay / restore | Output equal |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| fixture | 70 | 6.6 | 42 | 3.9 | 1.2 | 1.2 | 1.08 | 0.21 | 3.64 | 2.40 | 0.7x | yes |
| agent-loop@1 | 687 | 95 | 347 | 56 | 3.7 | 3.4 | 1.64 | 0.65 | 3.63 | 3.81 | 1.0x | yes |
| agent-loop@3 | 716 | 97 | 357 | 57 | 3.8 | 3.6 | 1.30 | 0.70 | 3.64 | 3.15 | 0.9x | yes |
| top-level-await | 23 | 2.2 | 10 | 1.1 | 0.5 | 0.5 | 0.74 | 0.05 | 2.13 | 1.02 | 0.5x | yes |
| lazy-builtins | 281 | 39 | 9 | 1.2 | 0.6 | 0.6 | 1.13 | 0.27 | 2.44 | 1.32 | 0.5x | yes |
| ssr | 11942 | 970 | 860 | 283 | 20 | 18 | 4.32 | 3.90 | 7.85 | 125.2 | 16.0x | yes |
| markdown | 15426 | 1301 | 9525 | 846 | 40 | 32 | 14.7 | 12.3 | 17.9 | 108.6 | 6.1x | yes |
| json-data | 138936 | 10412 | 49164 | 7518 | 585 | 374 | 99.8 | 88.3 | 94.1 | 290.4 | 3.1x | yes |
| image-text | 493 | 651 | 29 | 432 | 17 | 15 | 2.24 | 1.82 | 5.89 | 1572.9 | 266.9x | yes |
| json-data-x4 | 549936 | 41266 | 193164 | 29843 | 2283 | 1808 | 393.1 | 329.1 | 338.8 | 1108.7 | 3.3x | yes |
| awfy-richards | 134 | 15 | 118 | 19 | 4.3 | 4.7 | 1.54 | 0.68 | 9.13 | 1856.1 | 203.4x | yes |
| awfy-deltablue | 325 | 35 | 263 | 45 | 9.1 | 10 | 2.06 | 1.33 | 15.7 | 27.0 | 1.7x | yes |
| awfy-havlak | 225 | 27 | 203 | 42 | 10 | 11 | 2.11 | 1.57 | 20.8 | 13577.1 | 652.6x | yes |
| awfy-cd | 249 | 29 | 226 | 43 | 9.8 | 11 | 2.30 | 1.23 | 18.2 | 165.9 | 9.1x | yes |
| awfy-json | 285 | 183 | 221 | 38 | 8.0 | 9.0 | 2.52 | 1.67 | 14.4 | 913.4 | 63.6x | yes |

The workloads, in `fixtures/heap-snapshot/`:

| Workload | What is live at the pause |
| --- | --- |
| fixture | Closures over shared variables, an async function between two awaits, a `.then` chain, a class instance with a private field, a `Map`, a typed array |
| agent-loop@1, @3 | An agent-style loop: 300-message history, tool closures, class-instance tasks, a generator suspended inside `for...of`, at its first and third host call |
| top-level-await | A module body suspended by top-level `await` |
| lazy-builtins | References to built-ins materialized during the run (`JSON.parse`, `Temporal.Now.timeZoneId`, `Temporal.Instant`), the array iterator prototype, registered and local symbols |
| ssr | A React-like render of a 400-product page with one async component suspended inside `Promise.all` |
| markdown | A markdown/MDX-style pipeline over 60 documents: ASTs, module-level RegExps, slug registry, embeds waiting for their props |
| json-data | 4,000 orders through `JSON.stringify` and `JSON.parse`, indexed in Maps, waiting for an exchange rate |
| image-text | A 192x192 image blurred and edge-filtered in typed arrays, word frequencies in a Map, waiting for a threshold |
| awfy-* | Upstream AWFY programs, paused between benchmark iterations |

Other measurements, medians:

- Built-in table: 1.0-1.4 ms per engine, both at snapshot and at restore,
  down from 28-37 ms when the walk materialized every lazy member.
- Accounting check: under 1 ms for most workloads, 3.9 ms for markdown,
  31 ms for json-data and 118 ms for json-data-x4 (a full collection plus a
  scan of every managed object).
- zstd level 3 compresses in 0.04-21 ms; the biggest snapshot (29 MiB) took
  21 ms.
- Engine creation, the built-in table and compiling take 2-5 ms for the
  in-repository workloads and 8-19 ms for the larger AWFY bundles. A replay
  pays this too.

### Reading the Results

Restore time follows the number of serialized objects: about 1.3-1.8 µs per
object on the larger heaps (more where objects hold many strings), plus the
fixed cost of an engine and a compile. Replay time follows
the work the run did before the pause. A snapshot wins by the ratio of the
two, so it pays off for runs that compute a lot and keep little (Havlak,
Richards, image-text: 200-650x), helps for render-like work (SSR 16x,
markdown 6x), helps modestly for data held in large graphs (json-data 3x at
both sizes), and loses or ties for short runs that pause early. That last
group is what an agent-style program waiting on a tool usually looks like: its
replay to the pause took 1-4 ms.

Size: raw snapshots are 0.3-0.7 bytes per live heap byte. Strings are written
inline at every reference and every object repeats its property keys, so
data-heavy heaps compress very well (json-data: 7.3 MiB raw, 374 KiB zstd).
A string table and shared shapes would shrink the raw format several fold.

The snapshot also costs the paused run: serialize plus the accounting check
is under 20 ms for everything except the JSON workloads (130 ms at 4,000
orders, 510 ms at 16,000), paid at every pause whether or not the snapshot is
ever used.

## Test-Suite Census

With `GOCCIA_HEAP_CENSUS=<dir>`, the test runner records the live classes the
file created at its 1st, 2nd, 4th, ... 64th await that suspends, after the
module body (`module-idle`) and after its tests (`tests-done`). The census
counts classes, not snapshot success: a class with a codec can still refuse
(a native closure with an unregistered callback), and objects the run
materialized from lazy built-ins (Temporal and Intl prototypes) are counted
although a snapshot would name them. It is still a fair map of what real
programs keep alive. To re-run:

```bash
./build.pas testrunner
GOCCIA_HEAP_CENSUS=/tmp/census ./build/GocciaTestRunner -P tests
bun scripts/heap-snapshot-measure.ts census /tmp/census
```

| Point | Samples | Files | Every class has a codec | Native closures counted as covered | Median live objects |
| --- | ---: | ---: | ---: | ---: | ---: |
| await points | 333 | 115 | 0.0% | 22.2% | 81 |
| module idle | 1852 | 1852 | 78.0% | 79.0% | 5 |
| tests done | 1852 | 1852 | 46.3% | 75.0% | 45 |

Every sampled await point holds a native closure the prototype cannot name
(mostly the test runner's and the promise machinery's). The next classes
without codecs, by the number of await-point samples they occur in: module
namespace objects (83), error objects (79), test-runner expectations (67),
async generators (36), async iterator records (30), `AsyncLocalStorage` (24),
async context snapshots (23), async-from-sync iterators (17), fetch `Headers`
and `Response` (13 each), dynamic import state (12), deferred module
namespaces (6), `SharedArrayBuffer` (5) and Temporal values. Across all
samples, 117 distinct classes were alive; the prototype has codecs for 21.

## Coverage

| Area | Prototype | Missing for production |
| --- | --- | --- |
| Closures | Bytecode closures, upvalue cells, home objects, `new.target`, class private scopes; function declarations under the compatibility flags | Scopes other than the entry module's (direct `eval`, `with`); closures over templates compiled at run time (`eval`, `Function`) refuse |
| Generators and async frames | Suspended generator and async continuations: registers, local cells, handler stack, resume state; await handlers; top-level `await` | `yield*` delegation, a pending yield, async generators and their request queue, async-from-sync iterators, `for await`, iterator helpers, the map, set, string and RegExp string iterators, `using` and `DisposableStack` |
| Promises | Promises, reactions, capabilities, resolving functions, `Promise.all` | `allSettled`, `any`, `race`, the keyed forms, `finally` wrappers, thenable jobs, reactions carrying an async context |
| Module records | The entry module's lexical bindings, checked against a SHA-256 of its bytecode | Every other module: records, namespaces, deferred namespaces, `import.meta`, dynamic import state, JSON and text modules, module `var` bindings |
| Native and host objects | Native functions whose callback is registered by name and bound to their captured root | Every other native closure (about 50 call sites bind one), bound functions, Proxy, timers, fetch objects, `AbortController`, text codecs, `AsyncLocalStorage`, FFI values (always refuse) |
| Typed arrays and ArrayBuffers | `ArrayBuffer` including resizable, detached and immutable; typed arrays of every kind | `SharedArrayBuffer`, `DataView`, `Atomics` waiters; bulk copies instead of byte-at-a-time writes |
| WeakRef and FinalizationRegistry | None | `WeakMap`, `WeakSet`, `WeakRef`, `FinalizationRegistry`: write weak edges only when the target is otherwise reachable, keep cleanup callbacks and held values |
| Intl | References to Intl built-ins resolve by name | Every Intl object (10 kinds): recreate from resolved options |
| Temporal | References to Temporal built-ins resolve by name, materializing Temporal on restore | Temporal values (8 kinds), `Date` |
| Symbols | Local, registered (`Symbol.for`) and well-known symbols; symbol-keyed properties | Symbol wrapper objects |
| Errors and RegExps | RegExps compiled again from source and flags; the error stack slot of plain objects | `TGocciaErrorObjectValue` and its source-location metadata, including a process-local source principal that cannot cross processes |
| Bytecode and function templates | Templates by their depth-first index in the recompiled entry module; the snapshot refuses unless the bytecode hashes match | Templates of other modules, of `eval` and of the `Function` constructor |
| Built-ins | Lazy table, path resolution, late realm slots, live builtin objects, refusal when a built-in changed | Fingerprints for members materialized during the run (a primitive written to a lazily created built-in is not detected); materializers for every lazy realm slot; built-ins changed by the script written as differences instead of refused |
| Versioning across engine builds | One format version and the module's bytecode hash | An engine build identity and a schema version per codec; a mismatch refuses and the embedder replays |
| Run state outside the heap | The microtask queue is empty by construction | `Math.random` state, the deterministic clock, engine options and compatibility flags, instruction and memory limits, unhandled-rejection tracking |

## Effort Estimate to Production Quality

Effort is counted in agent hours: the active time of one agent session,
with idle gaps over 10 minutes (stalls, waiting for input) removed. Helper
agents an agent starts in parallel are counted separately.

The prototype took about 1.2 agent hours across two sessions:

| Session | Work | Active time |
| --- | --- | ---: |
| First | Explored the engine, wrote the format, the built-in table, the census and the first codecs (the work-in-progress commit; never built) | 31 min, plus 23 min in four parallel exploration helpers |
| Second | Merged `main`, made it build and restore, added the remaining codecs, the lazy built-in table and path resolution, the idle-scratch release, the harness, the workloads, these measurements and this note | 41 min |
| | **Total** | **72 min (95 min counting the helpers)** |

The estimate below starts from the rate measured in the second session, at
prototype quality: about 5 minutes per codec for a class with plain fields
(`Set`, the array iterator), 5-10 minutes when it has cross-object state
(`Promise.all`), and 15-30 minutes for a subsystem (the lazy built-in table
and its resolver). That rate already includes builds, failed builds and
debugging. Production quality multiplies it by about 2.5: a test or fixture
per codec (about +50%), a second agent's review and the fixes it asks for
(about +50%), the edge cases the prototype refuses instead of handling (about
+25%), and pull request and CI overhead (about +25%). Runs that are long by
nature (fuzzing, GC-stress, the round-trip lane) are added at their own
length.

| # | Piece | Prototype rate, min | Agent hours |
| ---: | --- | ---: | ---: |
| 1 | Engine API instead of probe hooks: take a snapshot at an idle point, restore with a source provider, map pending host calls to the embedder's ids, release idle scratch inside the VM; wire into the out-of-process sandbox protocol | 30 | 1.25 |
| 2 | Format: engine build identity and per-codec schema versions, string table and shared property keys, bulk byte copies, streaming writer and reader, optional host-side compression | 30 | 1.25 |
| 3 | Built-in naming: fingerprints for members materialized during the run, materializers for every lazy realm slot, decide refusal versus differences for changed built-ins | 25 | 1 |
| 4 | Object model: wrapper objects, `TGocciaErrorObjectValue` with its source principal, `Date`, bound functions, Proxy, `arguments`, classes extending native constructors, decorators and static blocks, dictionary-mode and sparse arrays | 70 | 3 |
| 5 | Control state: `yield*`, async generators, async-from-sync iterators, every built-in iterator and iterator helper, `using` and `DisposableStack` | 80 | 3.25 |
| 6 | Promise machinery: the remaining combinators, `finally`, thenable jobs, rejection tracking, async context and `AsyncLocalStorage` | 40 | 1.75 |
| 7 | Multi-module programs: records, namespaces, deferred namespaces, `import.meta`, dynamic import, per-module bytecode hashes, link order | 60 | 2.5 |
| 8 | Weak collections and finalization, Map and Set iterator cursors across tombstones, `SharedArrayBuffer` (refuse or copy), `DataView` | 30 | 1.25 |
| 9 | Temporal values (8 kinds) and Intl objects (10 kinds) | 90 | 3.75 |
| 10 | Host objects: timers, fetch objects, `AbortController`, text codecs; explicit refusal for FFI | 30 | 1.25 |
| 11 | Run state: random and clock state, engine options and flags recorded and checked, memory charged during restore | 15 | 0.5 |
| 12 | Hardening: the reader treated as untrusted input (semantic checks of instruction pointers, register and handler counts), a fuzzer over mutated real snapshots under heaptrc, GC-stress runs (1.5 hours of runs included) | 40 | 3.25 |
| 13 | A round-trip lane over the whole test suite (snapshot at every idle and sampled await point, restore, compare), and fixing what it finds (3 hours of fixes included) | 30 | 4.25 |
| 14 | ADR, embedding documentation, review rounds | | 2 |
| | **Total** | **570** | **about 30** |

The total is about 30 agent hours, 25 times the prototype, with a range of
20-45: a multiplier between 2 and 4 moves the coded pieces by about ±8
hours, and item 13 is where the long tail surfaces, its size unknown until
the lane runs. Pieces 4-10 are independent of each other, so parallel
sessions could bring the elapsed time to roughly 12-15 hours. Beyond the
one-time work, every built-in class added later needs a codec or an explicit
refusal, and every change to a codec-covered class's fields needs its codec
and schema version updated: a standing cost of roughly 10-20% on built-in
work.

A coverage-gated first version is about 13 agent hours (range 9-20): items
1, 2, 3 and 11, half of 12 and 13, the promise combinators, the built-in
iterators, error objects and async generators, and a smaller documentation
and review share. It covers single-module programs on the core language and
refuses everything else, which the embedder answers by replaying.

## Risks

- **Silent divergence.** A codec that forgets a field restores a run that
  behaves differently instead of refusing. The accounting check catches
  unserialized objects, not unserialized fields; only the round-trip lane
  catches those, and only for the states the tests reach.
- **Trust.** A snapshot encodes VM state such as instruction pointers and
  register counts. The reader bounds-checks, but a crafted snapshot can still
  describe a state the VM never produces. Snapshots must come only from the
  engine, authenticated by the embedder, and never from the script or a
  user.
- **Coupling.** Codecs read private fields of about a hundred classes in a
  codebase that changes daily, and the engine's lazy built-ins and cached
  intrinsics make built-in identity subtle (two prototype bugs came from
  there).
- **FPC.** Generic specializations in different units have different VMTs;
  interface changes in base units needed `--clean` builds several times
  during the prototype.
- **Payoff.** For short I/O-bound runs, the common case for a run waiting on
  a tool, replay is already as fast as a restore, so the work may buy little
  for the workloads that pause most.
- **Determinism.** Replay needs a deterministic run; a snapshot does not,
  but a restored run differs from an uninterrupted one wherever state lives
  outside the heap (random state, clocks) unless that state is captured too.

## Recommendation

Do not productize heap snapshots now. Replay stays the guarantee, and the
measurements say a snapshot is worth having only when the run did much more
work before its pause than it keeps alive. Before deciding:

1. Record, in the embedder, how long resumed runs take to replay to their
   pause point.
2. If a meaningful share replays for more than about 100 ms, where a
   snapshot measured at least 5x faster, build the coverage-gated first
   version (about 13 agent hours) behind an engine option, with the round-trip
   lane from the start and replay as the answer to every refusal.
3. Extend coverage by what refusals in real traffic show, not by the full
   list above.

Keep the branch as the reference implementation: the lazy built-in table and
path resolution, the idle-scratch release and the census are useful on
their own if the work resumes.
