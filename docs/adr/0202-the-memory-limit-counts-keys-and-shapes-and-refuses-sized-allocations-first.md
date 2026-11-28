# The memory limit counts property keys, symbols and shapes, and refuses sized allocations before they are made

**Date:** 2026-11-28
**Area:** `gc`, `engine`, `memory`
**Related:** [ADR 0185](0185-g2-refuses-memory-at-the-next-safe-point.md), [#1173](https://github.com/frostney/GocciaScript/issues/1173)

## Context

`--max-memory` bounded the collected heap and a few charged blocks outside it. Under it, a script was refused when
the heap reached the limit, but the process could hold several times more by then:

- **Keys and layouts were not counted.** An array index past `ArrayDenseMax`, and every fresh string key, became a
  property of a dictionary shape under an atom. Atoms, key atoms and symbols are never freed, and shapes live outside
  the heap; none of them counted. Each sparse index cost about 175 bytes, of which the limit saw its 8-byte slot.
  Since atoms are permanent, a script that caught the refusal and retried with fresh keys grew without bound.
- **Dictionary shapes were never freed.** A dictionary shape is retired only when its prototype dies, which for an
  ordinary object or a proxy is never. Every object past 64 properties, every proxy, and every attribute change or
  delete on a dictionary object left a shape behind for the engine's lifetime.
- **Large cells were refused after they were made.** A cell over a sixteenth of the nursery is allocated first and
  only then tested against the limit (ADR 0185). `new Array(6e7)` reached 477 MB at any limit, and
  `"".padEnd(5e8, "ab")`, which built its result in two Pascal strings before the heap copy, reached 1.9 GB at 16 MiB.
- **Native builders' buffers were not counted.** `join`, `JSON.stringify` and `replace` build their text outside the
  heap. `new Array(2e8).join("ab")` and `JSON.stringify(new Array(1e8))` were killed at the 3 GB cap of the test.
- **The array copy check priced a sparse result at 8 bytes per index.** At 1 GiB,
  `Array.prototype.toSpliced.call({length: 7e7}, 0, 0)` passed the check and was killed at 3 GB.
- Refusals made before an allocation did not carry the suggestion a refusal at a safe point does.

The allocator still cannot fail (ADR 0185): an allocation cannot be refused where it is made.

## Decision

- **Keys, symbols and shapes count against the limit.** Atoms, key atoms, symbols and the engine's permanent cells
  charge their blocks as they are made (`HeapNativeCharge`, permanent); each shape is charged its record and the
  key, flag and hash arrays it owns (`ShapeRecharge`), and given back when it is freed. A charge that takes the heap
  past the limit makes the next allocation collect (the nursery's bound drops to its top); once the permanent
  charges alone pass the limit, which no collection can undo, the refusal is marked at once. Every refusal is still
  delivered at the next safe point, as ADR 0185 decided.
- **A dictionary shape dies with its object.** A major collection records, on each dictionary shape, that a live
  object has it, and retires the dictionaries no live object has through the same path as shapes whose prototype
  died, except the one made last, which its object takes after an allocation that can collect. Every
  shape, and every in-place add to a dictionary, takes a version no earlier shape had, and the caches that keep a
  shape's address check its version too, so a freed shape's address cannot be mistaken for a later shape.
- **Allocations whose size the script chose are reserved first.** `ArrayCreate`, `new Array(n)` and a longer
  `length` reserve the elements they allocate; `repeat`, `padStart` and `padEnd` reserve their result and build it in
  the heap string itself; `join`, `JSON.stringify` and `replace` charge their buffers as they grow and reserve the
  result before copying it. A reservation that does not fit, after a full collection, is a catchable `RangeError`.
- **The array copy check stays and prices sparse results.** #2192's `ReserveArrayCopy` is kept as the copies' check
  before they read their source, which ordering `ArrayCreate` cannot give `toSorted`. Up to `ArrayDenseMax` it
  reserves the elements; past it every index of the result is a dictionary property, and it reserves
  `SPARSE_INDEX_BYTES` (41) for each: the overflow slot, the key and flags, and the hash entry, without the atom.
- **Growth stops doubling at the limit.** When twice an array's elements or an object's overflow slots would not
  fit, they grow to what they need and a sixteenth more; `join`'s buffer halves its growth down to what it needs.
- **Reads and serialization make no key per index.** A lookup of an index no atom names on an array past its
  elements, and `JSON.stringify` of an array's primitive elements and holes, no longer intern the index's atom.
- **Every early refusal carries the suggestion** (`ThrowMemoryLimit`), and making its error does not refuse again.

## Consequences

Peak resident memory before the `RangeError`, Linux x86-64, `GocciaRunner` (about 20 MB alone), each run capped at
3 GB:

| Script | Limit | Before | After |
|---|---|---|---|
| `Array.prototype.toSpliced.call({length: 2e8}, 0, 0)` | 16 MiB | 20 MB | 20 MB |
| `new Array(2e8).fill(0)` | 16 / 64 / 256 MiB | 196 / 729 / 2,797 MB | 41 / 107 / 374 MB |
| `o["k" + i] = 0` in a loop | 16 / 64 MiB | 200 / 721 MB | 46 / 115 MB |
| `a.push(Symbol())` in a loop | 16 / 64 MiB | 156 / 564 MB | 36 / 84 MB |
| 8 rounds of fresh sparse keys, each refusal caught | 16 MiB | 1,453 MB | 42 MB |
| `new Array(6e7)` | 16 / 256 MiB | 477 / 477 MB | 20 / 20 MB |
| `"".padEnd(5e8, "ab")` | 16 MiB | 1,927 MB | 20 MB |
| `"ab".repeat(2.5e8)` | 16 MiB | 1,926 MB | 20 MB |
| `new Array(2e8).join("ab")` | 16 MiB | killed at 3 GB | 34 MB |
| `JSON.stringify(new Array(1e8))` | 16 MiB | killed at 3 GB | 27 MB |
| `"a".repeat(1e6).replaceAll("a", "b".repeat(500))` | 16 MiB | 1,934 MB | 40 MB |
| `Array.prototype.toSpliced.call({length: 7e7}, 0, 0)` | 1 GiB | killed at 3 GB | 20 MB |

- A refused run now peaks at about 1.3 to 1.6 times the limit above the process's own baseline, against 3 to 30
  times before. The rest is the nursery and the native allocator's free blocks, which the limit does not count.
- The engine's built-ins take about 1.5 MB of keys and shapes at startup, so a limit under about 2 MiB refuses even
  a short script.
- Keys and symbols are still never freed. A script that keeps making fresh keys is now refused, and once they alone
  fill the limit, every later collection refuses again until eight refusals end the run; before, it grew without
  bound. A refusal can then arrive inside the handler of the previous one.
- Function templates, bytecode, the WeakMap and WeakSet tables and the parser's AST remain outside the limit.
- An object's dictionary shapes, and proxies' shapes, no longer accumulate for the engine's lifetime.
