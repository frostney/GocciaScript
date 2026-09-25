# 0118 - A parse result is measured in the file, not in what the parser was handed

**Date:** 2026-09-21
**Area:** `runtime`

## Context

[ADR 0117](0117-javascript-visible-ast-module.md) shipped `goccia:ast` with
one honest gap, recorded in its own Consequences: *offsets are into the parsed
source, not the file.* With `jsx: true` the JSX transformer rewrites the text
before the parser sees it, so `start`, `end`, and `loc.column` indexed the
transform. `loc.line` survived only because the transformer is
line-preserving, and `source` returned the transform so that a rule was at
least never comparing offsets against text it had not seen.

That is enough to report a finding on a line. It is not enough to fix one. An
edit is a range and a replacement, and a range that indexes a string the file
does not contain cannot be applied to the file. The next thing anyone wants
from a lint rule is `--fix`, so the gap had to close before anything was built
on top of it.

The two coordinate systems also leaked into the API's shape. `loc` was in the
file, `start`/`end` were in the transform, and `source` was the transform —
three properties, two coordinate systems, and no way for a caller to tell
which was which except by reading this paragraph.

## Decision

**A parse result carries one coordinate system: the text the caller passed.**

`parse` returns that text as `source`, unchanged. Every `start`, every `end`,
and every `loc` on every node and every comment is a position in it. The
transformed text is not exposed, and there is no mapping function, because
after this change there is no second coordinate system to map from. A rule
slices `source`, reports a `loc`, and computes an edit, and all three are the
same arithmetic.

The alternatives were to expose both coordinate systems, or to expose a
mapping function. Both keep the transform in the API, and the transform is an
implementation detail of how this engine happens to parse JSX — a rule that
learns about it is a rule that breaks when the transformer changes. Exposing
both would also have meant naming the second pair, and there is no name for
"the offsets into the thing we rewrote your file into" that a rule author
should ever have to read.

### How the mapping is built

The engine needed something the source map could not give it. A v3 source map
is a list of line/column pairs emitted wherever the generator chose to emit
one; it makes no claim about the text *between* two segments, which is exactly
the claim an offset mapping is.

`Goccia.OriginMap` is that claim and only that claim. It is a list of **runs**,
each asserting that a stretch of the output is character for character a
stretch of the input. Everything synthesized — a `createElement(`, a `", "`,
an escaped attribute string — lies in a gap between runs and has no original
text. A run of zero length is an **anchor**: a position correspondence with no
text, placed at the two edges of a construct that was rewritten whole, so an
element still reports where it began and ended.

Two things make the claim trustworthy rather than aspirational:

- **The transformer copies through one method.** Every character of the input
  that reaches the output goes through `EmitSourceChar`, which emits the
  character at the read position and records the correspondence. The claim is
  structural, not a convention thirty call sites have to keep.
- **Buffered output carries its own runs.** An attribute list is accumulated
  before the transformer knows whether it becomes an object literal or an
  `Object.assign`, so its correspondences are collected in the buffer's own
  coordinates and shifted when it lands. Without this, the body of an inline
  event handler — the one place a `.tsx` file routinely puts statements inside
  an attribute — would have been a gap.

An offset that lands in a gap has no exact answer, and the map rounds it
**outward**: a range start back to where copying stopped, a range end forward
to where it resumed. A range is therefore never narrower than the text it
covers, which is the direction an edit survives. In practice this does not
arise for the tree's nodes: a statement begins and ends at a keyword or a
punctuator, and JSX is an expression, so a statement containing an element
still has both of its own edges in copied text.

### What this exposed in the parser

Proving the mapping meant checking every node against the file, and that
check does not care whether a wrong offset came from the mapping or was wrong
before it. Four statement spans were wrong in plain `.ts` too, and are fixed
here:

- An **expression statement** took its position from its expression, and an
  expression node's position is where the parser built *that* node — so
  `use(next);` started at the `(`, and `x = 1;` at the `=`. A statement starts
  at its first token.
- An **`async function` declaration** started at `function`, dropping `async`.
  Slicing that range gave text that no longer parses, because the body still
  contains `await`. `Function.prototype.toString` had a separate patch for
  the same bug on the `export` path, now unnecessary.
- A **function or method body block** started at the function, not at its
  brace, so a method's body sliced back as `m() { ... }`.
- A **case clause** started at the `switch` keyword and ran to the clause's
  end, so all of a switch's clauses overlapped — and a clause is one of the
  three kinds that own a statement list, which is precisely the thing a
  layout rule reads.

`children` are now in source order for every kind, not only the three that own
a list. A class is reached through several per-kind maps as well as its
element array, and whichever one a member turned up in first decided when it
was emitted; a private field's initializer came out after the methods.

## Consequences

- **Verified, not asserted.** `examples/_experimental/ast-ranges.mjs` checks
  four claims per node over a whole tree: the range is inside the source,
  children nest and siblings do not overlap, `loc` is the position of `start`
  and `end` in that same source, and — the one that catches an offset that is
  merely plausible — re-parsing the text the range selects yields a statement
  of the same kind covering exactly that text. Over the source tree it was
  designed against: 4,006 nodes in 83 files, 1,558 of them in 43 `.tsx` files,
  every node round-trips except the 83 `Program` nodes, which are the file
  itself.
- **Comments the transformer removes are not reported.** A comment between
  JSX attributes, and a child container holding nothing but a comment, are
  dropped before the lexer runs, so they are not in `comments`. They were
  never reported; this is now the only gap rather than one of two.
- **`.ts` costs nothing.** No preprocessor runs, so there is no map, and the
  offsets the parser produced are already the file's.
- **The write-back question is now answerable.** A rule can compute an edit
  against the file. Whether it may apply one is a separate decision.

## Related

- [ADR 0117](0117-javascript-visible-ast-module.md) — what the module exposes
- [ADR 0025](0025-source-map-generation.md) — the v3 map, which keeps doing
  what it is for
