# 0184 - A definition takes its descriptor on every exit

**Date:** 2026-11-24
**Area:** `object model`, `gc`, `embedding`, `memory safety`
**Related:** [ADR 0143](0143-parse-arenas-caller-owned-descriptors-collector-teardown.md), [ADR 0174](0174-a-host-stops-a-run-from-another-thread-through-an-abort-handle.md), [ADR 0110](0110-growth-gate-collects-before-refusing.md)

## Context

`DefineProperty` and `DefineSymbolProperty` took ownership of their descriptor
only when they succeeded: when they threw, the caller had to free it. Their
`Try` forms took it on every exit except a raise from inside them. Callers that
built the descriptor in a variable wrapped the call in `try ... except
Descriptor.Free; raise; end;`. Most callers build it inline,
`Obj.DefineProperty(Name, TGocciaPropertyDescriptorData.Create(...))`, as
built-in registration does hundreds of times, and cannot free it, so any raise
inside such a call leaked it.

A raise inside a definition is not only its own `TypeError`. Every value
allocation polls the deadline, the instruction budget and a host's abort when
one is armed, and the memory limit can refuse it or the growth of the property
map. A definition allocates when it materializes a function's or a class's
`name` or `length`, a lazy property it replaces, a String object's index
descriptor, or the holes of an array it extends. An embedder that arms the
abort handle before it builds the engine, so that a stop can end the build
too, leaked a descriptor whenever the request landed in one of those
allocations during built-in registration, and the same happened when an abort,
a deadline or the memory limit ended a script inside such a definition
(`Reflect.defineProperty` on a function's `name`, for one).

Measured with a test that requests the abort at the Nth heap allocation of the
worker's thread, for every N: before this change 157 of the build's trials and
80 of a definition-heavy script's leaked a descriptor, at seven call sites in
built-in registration (the `%ThrowTypeError%` intrinsic's `length` and `name`,
`Promise`'s, `%TypedArray%`'s, the copied statics of every built-in
constructor) and in `ToPropertyDescriptor`'s descriptor at `Reflect.defineProperty`.
A memory-limit sweep over the build leaked at the same sites and at the lazy
globals, whose descriptor a refused growth of the global object's map dropped.

The split contract had also produced double frees: `TGocciaClassValue` and the
module namespace each freed a descriptor the caller freed again, and comments
there recorded the fix.

## Options

1. **Free at every creation site,** through a helper that wraps the call. About
   380 call sites, and each new one is a leak until someone remembers.
2. **An exception frame at the entry of each definition.** One place, but
   [the gate's own measurement](../garbage-collector.md#contributor-rules) put
   a frame at every store entry point at +2.4% to +4.0% on the store-heavy
   benchmark shapes.
3. **The definition takes the descriptor on every exit,** freeing it at each
   point that can raise, and only there.

## Decision

**Option 3.** `DefineProperty`, `DefineSymbolProperty`, `TryDefineProperty`
and `TryDefineSymbolProperty` store or free their descriptor whatever happens,
and a caller never frees a descriptor it passed.

- A definition frees the descriptor before it throws its own error, and before
  it stores the descriptor it built from it, so the plain path gains no frame.
- Where a definition allocates before it decides, the descriptor is freed in a
  frame that path already had or that only that path pays for:
  `MaterializeIntrinsicProperty` (function and class `name` and `length`),
  `MaterializeOwnLazyPropertyForDefine`, the String object's index descriptor,
  a typed array's non-integer numeric key, the array length's value
  conversion, a dense element moved into the map, the mapped `arguments`
  object's copy of a lazy descriptor, a deferred namespace's module
  evaluation, and the Proxy's trap lookup and call. The ones that can collect
  root what the descriptor holds across it.
- A refused growth of a property map frees the descriptor the `Add` was
  storing, in the growth gate's existing frame
  (`TGocciaShapedPropertyMap.RequireStorageBytes`). That covers every store
  into an object's map, the registration helpers and the intrinsic seeding
  included. A map built from descriptors that something else still owns, a
  heap snapshot restore's, sets `KeepsRefusedValue` and leaves them to their
  owners until the commit installs it, which clears the flag.
- `ExtendElementsWithHoles` takes the defining descriptor as an object handed
  over and frees it if the extension raises, in the frames its gate and its
  long fill already have, so an array element definition gains none.
- The callers that freed on a throw (`Object.defineProperty`,
  `Object.defineProperties`, the VM's accessor and data definitions, array
  `length` assignment, the heap snapshot reader's property store, an instance's
  new-property store) no longer do.

## Consequences

- An embedder may arm the abort handle before it builds the engine: a request
  that lands during the build raises `TGocciaAbortError` from the engine's
  constructor, and once the worker's runtime has shut down nothing the build
  allocated is left.
- `Goccia.DefinePropertyOwnership.Test` raises inside each implementation at
  each point that can raise and checks that the descriptor is freed exactly
  once. It also sweeps a host abort over every second heap allocation of an
  engine's construction, and separately of a definition-heavy script's run,
  on a worker thread, with a tracking memory manager that fails on any block
  left allocated after the thread's runtime shuts down. A request lands at
  the next value allocation, so consecutive requests stop the run at the same
  point; every leaking range before this change was at least two blocks wide.
  The sweeps cover what that script and the build reach, not every
  definition the engine has.
- A new override frees its descriptor before anything it does ahead of the
  inherited call can raise. A new caller passes the descriptor and forgets it.
- An `EOutOfMemory` from FPC's own allocator (a descriptor's, a map's or a
  list's block, a Proxy's argument list) is outside the contract: the process
  is out of memory, which no limit of the engine reports, and a descriptor in
  flight may leak then. A refused growth gate is the memory limit, and is
  covered.

Cost, as callgrind instruction counts of a production `GocciaRunner`:

| Workload | Base | After | Change |
| --- | ---: | ---: | ---: |
| 150,000 `Object.defineProperty` and `Reflect.defineProperty` calls on fresh objects | 4,646.47 M | 4,633.78 M | -0.27% |
| 20,000 classes with statics, and a function's `name` defined | 1,961.82 M | 1,956.71 M | -0.26% |
| `Object.fromEntries` of 200,000 keys, 20,000 small `JSON.parse` objects | 8,709.89 M | 8,712.53 M | +0.03% |
| 180,000 array element and `length` definitions | 7,728.04 M | 7,729.30 M | +0.02% |

Store-heavy work costs at most 0.03% more, and `Object.defineProperty` no
longer opens a frame of its own around the call. A first version that freed
through a frame at each store that had to grow a map, and around each array
element's extension, measured +0.5% to +1.0% on the same workloads.
