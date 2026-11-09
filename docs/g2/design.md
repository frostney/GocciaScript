# G2 design

## Values

`TValue` is a NaN-boxed `QWord`. Doubles are stored as themselves below `$FFF9 0000 0000 0000` (NaN is
canonicalised to `$7FF8...`); above it the top 16 bits tag the rest: `$FFF9` int32, `$FFFA` special
(undefined, null, false, true, the array hole), `$FFFB` a heap cell pointer. Numbers never allocate.

## Heap and GC

Every heap thing is a cell with an 8-byte header: kind, flags (mark, old, remembered) and size.

- Nursery: a bump region. A minor GC copies live young cells into old space (Cheney), promoting on the first
  survival.
- Old space: size-class free lists in 64 KB pages, larger cells as separate allocations; a major GC marks
  precisely and sweeps to the free lists.
- Write barrier: storing a young pointer into an old cell remembers the old cell.
- Roots: the value stack, frame records, registered globals and the temporary root stack natives use. Atoms
  (interned property keys), shapes and function templates are permanent and outside the collected heap.

The collector moves young cells, so code re-reads cell pointers from a root after any allocation.

## Objects

An object is a record: header, shape, elements (dense array storage or nil), overflow slots and inline slots.
The shape maps property keys to slots and flags and holds the prototype, so a receiver's shape fixes the head
of its prototype chain. Shapes form a transition tree; a shape with more than 8 transitions hashes them, and an
object past 64 properties gets a private dictionary shape. Inline caches record (shape, slot) for own
properties and (receiver shape, holder, holder shape) for prototype hits, and add transitions for stores.
Strings are UTF-16 cells with length and hash; long concatenations build growable views.

## Execution

Register bytecode in 64-bit instructions: op (8 bits), A (16), B (16), C (24). One value stack; a frame's
registers are R0 = this, then the parameters, locals and temporaries. Captured variables are upvalues that
stay open on the stack while their frame lives and close into cells when it ends. JS exceptions unwind through
the VM's own handler records: the dispatch loop has no Pascal `try`.
Generators and async functions suspend by saving the registers live at the suspension.

FPC's register allocator spills whole live ranges, the most-interfering first, so in `Execute`, the size of
the whole instruction set, the dispatch variables (PC, the registers' base, the code) live in memory. The hot
instructions therefore also run in `FastLoop`, a small function that makes no calls and allocates nothing, so
its loop variables stay in machine registers. `Execute` enters it before any instruction it handles; it runs
until an instruction it does not handle, or whose fast path does not apply, and hands that one back. It also
runs calls of plain bytecode functions and their returns, so a call does not end the fast path.

## Compilation

`G2.Compiler` walks Goccia's AST once per function. Declarations are hoisted per block; let, const and class
get TDZ checks only where a read can precede the declaration. Modules compile to a generator over a table of
binding cells. A direct eval in function code compiles its source as an arrow inside a stand-in for the
caller: the call site records every binding it sees, and the eval code reaches them as upvalues.

## Built-ins

Most built-ins are native Pascal. Those whose spec steps matter more than their speed are JavaScript preludes
(`source/g2/prelude`), compiled at start-up and snapshotted into the binary. Where a prelude backs a hot API
(RegExp, String.prototype.replace / split / match), a native fast path runs instead while the objects
involved are unmodified: the receiver's shape is the original one and the prototype's slots still hold the
original methods.

Temporal is such a prelude (`temporal.js`): the spec's operations in JS, exact times and time durations as BigInt
nanoseconds, each class's internal slots in one private field. Time zones come from the TZif files and
`tzdata.zi` of the zone directory (`GOCCIA_TZDIR`, else `/usr/share/zoneinfo`), which `G2.Temporal` reads for it.
A prelude marks its classes as built-ins, so a construction whose new.target has no object `prototype` falls back
to the class's own prototype, as a built-in constructor's does, rather than to `Object.prototype`.
