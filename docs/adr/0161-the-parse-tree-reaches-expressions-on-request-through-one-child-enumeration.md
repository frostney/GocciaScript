# 0161 - The parse tree reaches expressions on request, through one child enumeration on the node

**Date:** 2026-10-27
**Area:** `runtime`, `parser`
**Related:** [ADR 0117](0117-javascript-visible-ast-module.md), [ADR 0118](0118-original-file-source-ranges.md), [ADR 0135](0135-calls-and-property-reads-are-located-where-v8-locates-them.md), [#1549](https://github.com/frostney/GocciaScript/issues/1549)

## Context

[ADR 0117](0117-javascript-visible-ast-module.md) shipped `goccia:ast` with
statement structure only and withheld the expression kinds: "a prototype
should not freeze a surface four times larger than the one it was designed
for", and a handle-based or larger surface was "a complication to justify
with a measurement, not ahead of one".

[#1549](https://github.com/frostney/GocciaScript/issues/1549) is that
measurement. A formatter built on the statement tree and a hand tokenizer
matched prettier on 95.0% of the lines of 235 TypeScript files, and every
class of the remaining differences — line breaking in member chains and
operator groups, trailing commas, parentheses, string-literal quotes — is a
rule defined on a node kind the tree does not have. The formatter re-derived
those nodes from tokens with heuristics, and every bug found while building it
was one of them.

ADR 0117 also named the structural problem that adding them would make worse:
the walker that flattens the Pascal tree had its own `case` over the node
classes, a second place, next to the compiler, that has to learn about every
new node. It said the seam it wanted was a shared child enumeration on
`TGocciaASTNode`, and that the change belonged in its own decision.

Two facts about the parser shaped what follows.

- **An expression's span is a diagnostic position, not its extent.** A binary
  expression's span is its operator token; a call's starts at the name its `(`
  follows, as [ADR 0135](0135-calls-and-property-reads-are-located-where-v8-locates-them.md)
  requires for stack traces; a member expression's starts at its property.
  Those positions are observable in error messages and are right for what
  they do. They cannot double as source ranges.
- **Some syntax has no node.** The parser drops parentheses, keeps an object
  literal's members in maps keyed by name, keeps a simple parameter as a name
  in a record, decomposes `x = 1` into a name and a value, and keeps an
  import's module path as a string. None of those has a range to report.

## Decision

### Opt in per call

`parse(text, { expressions: true })` adds the expression level to the tree.
Without the option the result has today's shape: the same nodes, kinds,
ranges and children as before this decision, apart from the two walk
corrections listed under [Consequences](#consequences). A statement-only rule
pays nothing for a surface it does not read.

### One child enumeration, on the node

`TGocciaASTNode.ForEachChild(AVisitor)` is virtual. Every node class reports
the nodes nested immediately beneath it, in source order, to a
`TGocciaASTChildVisitor`. Syntax with a range but no node of its own — an
object literal's member, a parameter, a class member, an import's module
path — is reported as a *part* between `EnterPart` and `LeavePart`, with the
nodes inside it in between.

The `goccia:ast` flattener is a visitor and has no `case` over the node
classes any more: it walks the tree only through `ForEachChild`. A new node
class describes its children next to its own fields. What the flattener keeps
is presentation: which kind name a node is reported as, which flags it
carries, and which nodes are the same text as the node around them (below).

### A node's range is the text it covers, worked out from its children

The flattener does not use an expression's span as its range. A node's range
is the smallest one covering its own span and the ranges of everything
beneath it, so `a + b * c` runs from `a` to `c` whatever the operator span
says. The spans stay what they are, so no diagnostic moves.

The parser records what covering cannot recover, and nothing else:

| Recorded | Because |
|---|---|
| The outermost parentheses around an expression | They are dropped from the tree; `(a + b) * c` and `a + b * c` would otherwise be the same tree |
| Each parameter's extent, type annotation and default included | A simple parameter is a name in a record |
| Each object member's extent | Members live in maps; a static key has no node |
| Each class member's extent, from its first decorator | Its key and its modifiers (`static`, `async`, `get`) have no node |
| The target of an assignment, as the node built before `=` was seen | `x = 1` is decomposed into a name and a value |
| An import's or re-export's module path | It is kept as a string |

Some spans were wrong as well as narrow and are corrected, each keeping its
start where a diagnostic reads it unless the start itself was wrong:

- A template literal with substitutions spanned only its first segment;
  `import.meta`, `new.target` and `import(...)` spanned only their keyword.
- A tagged template whose template spans lines lost its template: its
  position was built from the line the template ends on. It now starts at the
  template's first character, so such a call is also located there.
- An object literal containing a spread took its position from the spread.
- An async arrow function started at its `(`; it starts at `async`, on the
  same line, so only the column its debug position records moves. Its
  parameter diagnostics still point at the `(`.
- A decorated class started at `class`; it starts at its first decorator, its
  first token, as ADR 0118 has it for every statement. Its source text, which
  `toString` returns, still starts at `class`.
- A destructuring assignment's default, `[a = 1] = x`, built its target at
  the `=`; the pattern now reuses the node the parser built for the target, so
  it is located at the name.

A statement keeps the range the parser records for it
([ADR 0118](0118-original-file-source-ranges.md)), and a part the parser
measured keeps that measurement; neither is widened by what it holds.

### Kinds and flags

Names follow ESTree, and Babel where ESTree has one kind for several literal
forms, because a layout rule dispatches on `kind` and a string literal is
treated differently from a number: `StringLiteral` and `NumericLiteral`
rather than one `Literal`. Syntax with no ESTree node of its own takes the
nearest name — a parameter is a `Parameter`, a class field a
`PropertyDefinition`, an element of a `.tsx` file a `JSXElement`. The full
list is in [AST Built-ins](../built-ins-ast.md#the-expression-view).

A node carries the flags its kind dispatches on, and only those: `operator`
on unary, update, binary, logical and assignment expressions; `prefix` on an
update; `computed` and `optional` on a member expression; `optional` on a
call; `async` and `bodyKind` (`"block"` or `"expression"`) on an arrow
function; `async` and `generator` on a function expression; `delegate` on a
`yield`; `computed`, `shorthand` and `method` on a property; `rest` on a
parameter; `static` and `computed` on a class member.

The operator is the one value exposed. It sits between two children with
whitespace, comments and parentheses around it, so slicing it out of `source`
is the error-prone scan this decision exists to remove. Names and literal
values stay withheld for ADR 0117's reason; a rule that wants a quote
character reads `source[node.start]`.

Where the parser builds two nodes for one piece of syntax, only the outer one
is reported: a function declaration and its function, an object method or
class member and its function, an export and the declaration it exports. A
one-element sequence the parser keeps so that `(a?.b).c` ends the chain is not
a sequence in the source, and is not reported.

### One coordinate system still

Every range is in the caller's text, `.tsx` included. A statement whose edge
falls in text the JSX transformer wrote is rounded outward, as before. Below
the statement level the construct that produced such text is usually the node
itself — the call the transformer writes for an element, the string it escapes
an attribute value into — so an expression whose start and end do not both
map exactly into the caller's text is not reported, and its children move up
to its parent. The transformer anchors an element's call to the element's own
edges, so that call does map exactly; its callee is the transformer's text,
which is how it is recognised and reported as `JSXElement` with the element's
range. Expressions inside an element's braces are copied text and are
reported as usual.

### What this does not add

- **A flat `tokens` array.** A separate request ([#1923](https://github.com/frostney/GocciaScript/issues/1923)), delivered separately.
- **Type annotation, type literal and type argument ranges.** Types are erased
  before the tree exists; recording their spans is a parser change of the kind
  ADR 0118 made for statements, and its own slice
  ([#1929](https://github.com/frostney/GocciaScript/issues/1929)).
- **Variable declarators, object-pattern properties and their keys, labels,
  the patterns of `match`, and JSX attributes and text.** Each needs a range
  the parser does not record yet. Until then a declaration's initializers, a
  pattern's targets and an element's braces are reported directly beneath the
  node around them ([#1928](https://github.com/frostney/GocciaScript/issues/1928)).

## Consequences

- **A formatter's rules can be written against the tree.**
  `examples/_experimental/expression-style.mjs` enforces single quotes,
  trailing commas in multi-line lists, and one argument per line for a call
  on an over-long line, fixing what it finds through the sandbox filesystem;
  its writes reach the host only under `--copy-rw`
  ([ADR 0119](0119-host-applied-sandbox-write-back.md)).
- **Verified, not asserted.** `examples/_experimental/ast-ranges.mjs` now
  checks both views with the four claims of ADR 0118, re-parsing each
  expression, property, parameter and class member from the text its range
  selects.
- **The statement view changes in a few places, all toward the source.** A
  decorator's expression is walked, so a block inside one (`@((v) => { ... })`)
  is now in the tree; a block nested in a `case` test is a child of that
  `SwitchCase` rather than of the `switch`; the blocks of the getter and setter
  the parser writes for an `accessor` field are no longer reported, since the
  source has no such blocks; and a decorated class statement starts at its
  decorator. Over 1,016 files (the repository's `website/src`, `scripts` and
  `tests/language`) the statement trees are otherwise identical to the ones
  before this decision.
- **The walk is deeper.** It now reaches every expression, so it recurses as
  deep as the tree is tall; its limit is the parser's own height limit,
  rather than the 512 levels that were enough for statements.
- **The cost is per node.** Without the option the walk does what it did,
  through a virtual call per node instead of a chain of class tests. With it,
  each extra node is one more record and its JavaScript object, `loc` and
  `children` array.

## Related

- [ADR 0117](0117-javascript-visible-ast-module.md) — the module, and what it
  withholds
- [ADR 0118](0118-original-file-source-ranges.md) — one coordinate system
- [ADR 0135](0135-calls-and-property-reads-are-located-where-v8-locates-them.md) —
  why an expression's span is a position, not a range
- [AST Built-ins](../built-ins-ast.md) — the module's reference
