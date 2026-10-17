# 0143 - A parse's arena owns its tree, a descriptor built for [[GetOwnProperty]] is its caller's, and a collector frees what it still manages

**Date:** 2026-10-16
**Area:** `gc`, `parser`, `object model`, `memory safety`
**Related:** [ADR 0084](0084-realm-owned-prototype-method-hosts.md), [ADR 0138](0138-bytecode-is-the-only-way-gocciascript-executes.md)

## Context

The scheduled memory-safety job runs the JavaScript suite in a heaptrc build
and fails on any unfreed block. Every run failed
([#1592](https://github.com/frostney/GocciaScript/issues/1592)): the suite
ended with about 1.7 million unfreed blocks, so the Valgrind step after it never
ran. Three ownership gaps caused nearly all of them.

- **Parsed trees.** `TGocciaProgram.Free` freed the program node and nothing
  under it. While the tree-walk interpreter existed, function values pointed
  into the AST, so the tree had to outlive every value made from it. Most node
  classes had no destructor; a few freed some children, so ownership was partial
  and inconsistent. Every parse leaked its tree, the source text its spans
  referenced, and the lexemes it kept, which was over 96% of the blocks. Since
  ADR 0138 nothing executes the AST: the compiler copies what it needs into
  function templates, so nothing reads a tree after its program is compiled.
- **Built property descriptors.** `GetOwnPropertyDescriptor` (and its symbol and
  key variants) answers with the descriptor an object stores, or with one it
  builds for a property it does not store: an array element or `length`, a
  function's or class's `length` and `name`, a class's `prototype`, a string
  index, a typed-array element, a module namespace export, a proxy trap's
  result. Callers could not tell the two apart, so none freed the built ones.
- **Values alive at the end.** `TGarbageCollector.Destroy` freed its object list
  without destroying the objects on it, and the main thread never shut its
  collector down at all. Everything still managed at thread or process exit
  leaked: the primitive singletons, each worker's prototypes and property maps.

## Decision

**A parse's arena owns every AST object the parse creates.**
`TGocciaASTObject` is the base of every node and of the four helper classes that
are not nodes. While `TGocciaParser.Parse` runs, a thread-local arena is active,
and each AST object joins it on construction. The program the parse returns
takes the arena, so freeing the program frees the whole tree; a parse that
raises frees its arena. An AST object never frees another AST object: a node's
destructor frees only what it alone owns that is not a node (a list, a map, a
literal's value), and a list or map that holds nodes is freed without freeing
its elements. That makes a shared or re-parented node safe. An object freed
explicitly before its arena (a node the parser discards) leaves the arena on
destruction, so it is not freed twice. A node created with no parse running (a
compiler temporary) belongs to whoever created it.

Programs keep the lifetimes their owners already gave them. A parse site frees
its program once the program is compiled and linked, as the test runner, the
module loader and eval did before; the per-thread shim programs are freed when
the thread's runtime shuts down. None of them is retained for function values,
because none is read after compilation.

**A descriptor built for [[GetOwnProperty]] belongs to the caller.** The method
that builds one marks it with `AsCallerOwned`. Every caller releases what it
gets with `ReleaseDescriptor`, which frees a caller-owned descriptor and leaves
a stored one alone. The release comes before anything that may replace the
stored descriptor: a definition or deletion, or guest code (a getter, a setter,
a proxy trap) that may define or delete. A caller reads the fields it needs
first, or, where the spec reads the descriptor after such code runs (the proxy
invariant checks), keeps an owned copy with its values rooted. A caller that keeps a descriptor clones it, or takes a caller-owned
one over with `TakeOwnership`. `HasOwnPropertyDescriptor`,
`OwnPropertyIsEnumerable` and their symbol and key forms answer the common
questions without handing a descriptor out.

The flag was chosen over the alternatives. Caching the built descriptor on its
object is unsafe, because a second request would change or free a descriptor a
caller still holds. Making built descriptors managed would let a collection,
which guest code can trigger between the request and the use, free one under
its caller. A separate API for built descriptors would still leave every caller
unable to tell which kind a virtual call returned.

**A collector frees everything it still manages when it is freed.**
`TGarbageCollector.Destroy` drops its roots and pins and sweeps with nothing
marked, as a collection does, repeating while destructors allocate.
`ShutdownThreadRuntime` already freed each worker's collector after draining the
thread-cleanup registry; `TGocciaApplication.RunApplication` now calls it on
the main thread once the application has finished. The primitive singletons
stay with the main thread's collector: an AST literal no longer unregisters
one.

## Consequences

- heaptrc reports no unfreed block for the suite, and the memory-safety job can
  run its Valgrind step.
- A new AST node class that owns a list or map frees it in its destructor
  without freeing the elements, and frees no node. A parser path that drops a
  node may free it, but never a node another node still references. Code that
  builds AST nodes outside a parse frees them itself.
- A new `GetOwnPropertyDescriptor` override that builds a descriptor marks it
  `AsCallerOwned`. A new caller of [[GetOwnProperty]] releases the result,
  reading what it needs before any definition or deletion on that object and
  before any guest code runs.
- A value's destructor still frees only what the value owns natively and never
  calls into another managed object: at teardown, as in any sweep, its peers
  may already be gone. Nothing may use a managed value after its thread's
  runtime has shut down.
- A native object whose method backs a native function, and which holds
  values, is a managed value set as the function's `CapturedRoot`, so the
  collector keeps and frees it with the function. A `TGCManagedObject`
  subclass that does not register itself, as `TGocciaValue`, `TGocciaScope`
  and `TGocciaBytecodeCell` do, is never freed by a collector.
- A thread-local cache of strings or handles registers a cleanup with
  `Goccia.ThreadCleanupRegistry`; a worker frees nothing else.
