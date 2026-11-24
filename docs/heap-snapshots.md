# Heap Snapshots

*Writing what a value reaches and restoring it in a fresh engine of the same build; on G2 a test-runner round trip and a fuzz target.*

## Executive Summary

- **A small writer and reader** — `G2.HeapSnapshot` writes what one root value reaches (`SnapshotWrite`) and restores it in a fresh engine set up the same way (`SnapshotRestore`)
- **Where it is used** — GocciaTestRunner's `--heap-snapshots` round-trips every object `expect()` is given ([below](#round-tripping-the-javascript-suite)), and GocciaFuzzHarness fuzzes the reader with real snapshots. `TG2Engine` exposes only that round trip; G2 has no embedder session or pause checkpoint
- **Refusal is normal** — A value whose state lives outside the cells the writer copies (a Map's table, a buffer's bytes, a promise's reactions, a generator's frame, a Proxy, a Date, a RegExp) refuses the snapshot with a reason
- **Built-ins are named, not written** — What `globalThis` reaches before the run's code is named by its path; a built-in is written as that name and a fingerprint of its state, and a restore refuses a built-in the run changed
- **Functions are named by their code's place** — A closure is written with its bindings, and its code as a path in the entry's or a module's function tree; the restoring engine compiles that code without running it
- **The reader expects hostile input** — Every count, index and value is checked against the input, the object kinds and the code, and a prototype or bound-function chain that loops is refused

## Scope

G2's heap snapshots copy cells as they are. The writer handles these kinds:

| Cell | Written as |
|---|---|
| String | Its text. A private name or an engine key no source text names refuses |
| BigInt | Its sign and limbs |
| Symbol | A registered symbol by its registry key; any other by its description |
| Value array | Its capacity, length and values |
| Closed binding (upvalue) | Its value. A binding a running frame still holds open refuses |
| Accessor pair | Its getter and setter |
| Object | Its kind, flags, prototype, keys with their attribute flags and values, elements, and its two internal values (`Aux1`, `Aux2`) |

An object is written only when it is an ordinary object, an array, a closure, a bound function, an error, a
`Boolean`, `Number`, `String` or `Symbol` wrapper, or an unmapped arguments object, and holds no native state outside
its fields. Everything else refuses with `no snapshot codec for ...`: a Map, a Set, an `ArrayBuffer`, a typed array,
a `DataView`, a promise, a RegExp, a Date, a Proxy, a generator, an async generator, a module namespace, an array
iterator, a mapped arguments object and a native function the engine did not have before the run. The refusal names
the kind as the old engine's class name, so refusal reports stay comparable.

What the old engine's heap snapshots also had and G2 does not: the embedder session and restore classes, the pause
checkpoint, built-in differences (a changed built-in refuses instead), module records (a restored engine that imports
a module evaluates it again), configurable reader limits, the accounting check, and a format version or build
identity in the header.

## Taking a snapshot

Three calls, all on the engine's thread:

1. `SnapshotNameBuiltins`, before the run's code. It runs a minor collection, so every cell it names is in old space,
   then walks everything `globalThis` and the symbol registry reach (prototypes, properties, accessors, elements and
   a function's internal values) and names each object and symbol by its path, such as
   `globalThis.Array.prototype.map`. The named cells go into one rooted array, so a built-in the run drops is never
   mistaken for a cell made later.
2. `SnapshotNoteEntry` once the entry Script is compiled, or `SnapshotSetEntry` for a module entry: the entry's file
   name, source and kind, which the writer uses to name functions compiled from it.
3. `SnapshotWrite(Root, Data, Refusal)` at idle. It returns `False` with the reason in `Refusal` when it cannot write.
   It allocates nothing on the heap.

`TG2Engine` makes these calls when its `HeapSnapshots` property is set before `Run`: it names the built-ins before the
entry compiles, notes the entry once compiled, and `HeapSnapshotObserved` writes every object `expect()` was given.

### The format

A snapshot starts with the magic number `2GH1` and then holds, in order:

- **The built-ins it refers to**, each as its path and a 64-bit fingerprint. A fingerprint covers the built-in's
  kind, flags, prototype, keys with their flags and values, the elements present, a native function's entry point and
  a closure's name and declaration position; another built-in counts by its index and a string by its text.
- **The code roots it refers to**: the entry Script by its file name, or a module record by its path and a hash of
  the source it was compiled from.
- **The function templates it refers to**, each as its code root, its path of child indices in that root's function
  tree (-1 for a template's field initializer), and its name and declaration line and column.
- **The cells**, in the order the writer reached them, each referring to others by index, to built-ins by their
  table index, and holding immediates (numbers, `undefined`, `null`, booleans, the array hole) as they are.
- **The root value.**

A closure is written as its object: its home object and its array of bindings sit in its internal values, and the
bindings it shares with other closures are written once. Its code is never written. A function compiled outside the
entry and its module records, such as eval code or the `Function` constructor's, refuses.

## Restoring

`SnapshotRestore(Data, Root, Refusal)` restores into a fresh engine of the same build that has named its own
built-ins (`SnapshotNameBuiltins`) and been told the entry (`SnapshotSetEntry`), set up as the writing engine was:
the same globals, testing API and settings. The caller roots `Root` at once. The restore:

1. Checks the magic number, then resolves each built-in by its path and compares its fingerprint in this engine with
   the one written: a missing built-in refuses (`the snapshot names a built-in this engine does not have`), and a
   different fingerprint means the run changed it (`the run changed the built-in ...`).
2. Finds each code root: the entry, compiled from the source it was told without running it, or a module, loaded
   with what it imports, compiled and not evaluated, whose source hash must match the one written.
3. Finds each template at its path, compiling lazy functions on the way, and refuses one whose name or declaration
   position differs (`the snapshot names a function this engine compiled differently`).
4. Allocates every cell empty into a rooted table, then fills each one, applying the write barrier to every store,
   since a cell allocated early may be old by the time it is filled.
5. Checks every object's internal values against its kind, checks that no prototype chain or bound-function target
   chain loops, reads the root, and refuses trailing bytes.

The format holds no build identity: a snapshot from another build is caught only when one of these checks fails, so
restore only snapshots the same build wrote.

### Reader checks

The reader treats a snapshot as hostile input. Every refusal is `ERefused` inside the unit and comes back as `False`
with the reason; none is a fault.

- **Counts and lengths** — Built-ins at most what this engine named, code roots at most 4,096, templates at most
  1,048,576 with paths of at most 4,096 steps, cells at most 16,777,216 (`MAX_CELLS`) and never more than the input's
  bytes, an inline capacity at most 4,096, a value array's length at most its capacity and its capacity at most the
  longest dense array (`ArrayDenseMax`), and every string, BigInt, value array and key count within the bytes left. A
  short input is `the snapshot is truncated`.
- **Values** — An immediate must be one the engine makes: a number (a NaN only as the canonical one), an Int32 in its
  tag, or one of the five special constants; a pointer is never an immediate. A cell or built-in reference must be in
  range, and a prototype must be `null` or an object.
- **Kinds** — Only the object kinds the writer writes, and each object's internal values must fit its kind: a
  wrapper's primitive, a bound function's callable target and argument array, a closure's home object and at least as
  many closed bindings as its code has, an array's elements and, past them, a length up to 2^32 - 1.
- **Keys and attributes** — A symbol key must be a symbol, a key may not repeat on one object, and an accessor flag
  must come with an accessor pair.
- **Chains** — A prototype chain or bound-target chain that loops is refused; ordinary objects cannot have such a
  prototype chain (ES2026 §10.1.2.1 OrdinarySetPrototypeOf), and `[[Get]]`, `[[HasProperty]]` and `instanceof` walk
  it to its end.

The reader keeps a restored value within these rules, but cannot tell a forged property value from a real one.
Authenticate snapshots, for example with an HMAC under a key no script can reach, before restoring them, and never
accept one from a script or a user.

## Testing

GocciaScript's own suite exercises the round trip ([below](#round-tripping-the-javascript-suite)): PR CI and the G2
workflow (`.github/workflows/g2.yml`) run `tests/` with `--heap-snapshots`, and the G2 workflow fails when any
round trip fails.

GocciaFuzzHarness restores one input as a snapshot with `--heap-snapshot <file>` and writes its own seed snapshots
with `--write-heap-snapshot-seeds <dir>`. Seeds and restores run one program, which in a seed run hands each seed
graph to the writer and in a restore run uses what restored, calling its functions: data graphs with holes, cycles
and shared objects, symbols, frozen and sealed objects, closures, bound functions, errors and wrapper objects. The
scheduled [fuzz workflow](../.github/workflows/fuzz.yml) writes the seeds, checks that each one restores, and fuzzes
the reader with AFL++ from them ([Tooling — Fuzzing](contributing/tooling.md#fuzzing-and-memory-safety)).

### Round-tripping the JavaScript suite

`./build/GocciaTestRunner -P tests --heap-snapshots --silent --no-progress` keeps each distinct object passed to
`expect()` (the `observe` native in the testing API, `SnapshotObserve`), rooted until the file finishes; a primitive
is not observed. Once the file's tests have run without an error, `TG2Engine.HeapSnapshotObserved` writes each object
at idle and describes it (`SnapshotDescribe`). Objects are therefore checked in their **final file state**, not their
state at the assertion: writing inside `expect()` would write a running frame.

After the file's engine is freed, each written object is restored by `TG2Engine.HeapSnapshotVerify` in a fresh
engine of its own, set up as the file's was (globals, testing API, settings). It passes when writing the restored
object again gives the same bytes and its description matches the original's. The description lists each object's
kind, extensibility, prototype, code position or primitive, and its own keys in order with their descriptors,
read without calling getters; built-ins appear by name.

Each object ends `passed`, `refused` (the writer refused it, or the restoring engine could not restore it) or
`failed`. A failed round trip adds a suite error to its file (`Heap snapshot round-trip failed`). The mode runs files
serially, including when `--jobs` is given. The report goes to stderr, leaving JSON stdout intact:

```text
Heap snapshot failure: <file>: <what differed>
Heap snapshots: observations=<n> objects=<n> passed=<n> refused=<n> failed=<n>
Heap snapshot refusal: <count> <reason>
```

`observations` counts every object `expect()` was given, repeats included; `objects` the distinct ones written or
refused. Refusal reasons are listed in sorted order with how often each came.
