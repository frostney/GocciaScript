# Argument collections root their elements

**Date:** 2026-08-06
**Area:** `runtime`

A live `TGocciaArgumentsCollection` is a garbage-collection root source: for as
long as one exists, every value it holds is marked. The collection registers
itself with the thread-local collector when it is constructed and unregisters
when it is destroyed, and `TGarbageCollector.MarkRoots` walks the registered
sources on every collection. This closes a hole that had no owner before: the
collection is the container every native builtin receives its arguments in, and
it was a plain `TInterfacedObject` holding a non-owning value list — never
marked, never a root.

Two shapes made that observable rather than theoretical. In bytecode,
`TGocciaRegister` is a tagged union whose `grkInt` and `grkFloat` cases hold raw
scalars, and `MarkRegisterReferences` marks only `grkObject`. Building a call
therefore boxes a fresh `TGocciaNumberLiteralValue` out of a scalar register
straight into the argument collection, and the source register still holds only
the scalar — the boxed number has no root at all. Separately, any builtin that
reads an argument into a native local and uses it after re-entering user code
holds it across a safe point: a getter, a callback, a Proxy trap, and a thenable
are all arbitrary script, and all of them can reach a collection. `ADR 0020`'s
unified heap hierarchy means every such value is collectable; nothing about the
argument path exempted it.

The fix is central rather than per-site because the per-site surface is roughly
528 construction sites across more than 30 units, and the two shapes above are
not local to any of them — the first is created by the VM's calling convention,
not by the builtin. A per-site sweep would also have to be repeated for every
builtin added afterwards. Rooting the container instead states the contract once
and makes it structural: a builtin cannot opt out of it, and a new builtin
inherits it without knowing it exists.

The mechanism is a `TGCRootSource` base declared in the collector unit, which is
a leaf and stays one. It keeps `TInterfacedObject` as its ancestor so the
collection's existing ancestry is unchanged, carries the index of its slot in the
collector's root-source list so unregistration is O(1), and carries a
back-pointer to the collector it registered with. The back-pointer is what makes
the lifetime safe in both directions: collectors are thread-local, so an instance
must unregister from the same one it registered with, and a collector that is
destroyed first nils the back-pointers of everything still registered so a later
destructor cannot unregister into freed memory. Registration is driven from
`AfterConstruction`/`BeforeDestruction` rather than from the constructors, so it
happens once per instance regardless of which constructor ran — including the
callback-argument subclasses, whose constructors do not chain to an inherited
one. Removal swaps the last entry into the vacated slot instead of leaving a nil
behind: nothing depends on root-source order, there is no sweep pass to compact
against, and nil slots would otherwise accumulate for the whole of a bytecode
run, during which automatic collection is disabled.

The cost is bounded because the hot paths do not construct a collection per
callback invocation. The VM pools its collections through
`AcquireArguments`/`ReleaseArguments` with a pool capped at 32, so a pooled
collection registers once and is reused — the bytecode call path pays nothing per
call. The array callbacks hoist: `forEach`, `map`, `filter`, and `reduce` build
one `TGocciaArrayCallbackArgs` or `TGocciaReduceCallbackArgs` and rewrite its
slots per element, and `Array.from`/`fromAsync` and the sort comparator do the
same. Some builtins do construct per iteration — `Map.prototype.forEach`,
`Set.prototype.forEach`, the WeakMap/WeakSet iteration helpers, and several
TypedArray conversion paths among them — and those pay one list append and one
O(1) swap-removal per element, with no hashing. At collection time the extra work
is one pass over a list whose length is the number of concurrently live
collections: the VM pool plus the handful of frames in the current builtin's
recursion.

This contract covers what a collection holds, and nothing else. Values a builtin
keeps in its own native state are still its own responsibility, and still need
explicit rooting through `AddTempRootIfNeeded` or `TGocciaActiveRootFrame`.
`JSON.stringify`'s replacer traversal is the worked example: its cycle-detection
stack is a plain `TList` holding live objects across replacer invocations, and
its synthetic wrapper object and partially built copy live only in native locals
while user code runs. None of those are arguments, so all of them are rooted at
their own sites.
