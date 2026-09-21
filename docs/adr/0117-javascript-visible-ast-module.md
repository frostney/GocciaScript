# 0117 - Expose the parse tree to JavaScript as statement structure, not as the AST

**Date:** 2026-09-21
**Area:** `runtime`

## Context

`TGocciaSourcePipeline.Parse` is a Pascal embedding API. A host that wants
parse artifacts links against the engine and walks `TGocciaProgram` in Pascal.
Nothing about the parse is reachable from JavaScript, so a tool that wants to
ask a question about source — a lint rule, a codemod, a metric — has to be
written in Pascal and shipped inside the binary, or has to run somewhere else
entirely.

The concrete consumer this was designed against is a lint checker for house
style: rules about where blank lines belong, which a formatter cannot express
and a type checker does not care about. Two of its rules were used as the
design brief — *a blank line before a `return` that follows another statement*,
and *a blank line after a run of variable declarations*. They are small, but
they need exactly the things a conventional AST is bad at:

- The statements of each list, in source order, with their kind.
- Where each statement starts and ends, both as offsets and as line/column.
- The source text itself, because the question is about the whitespace
  *between* statements, and no AST has a node for a blank line.
- Comments with positions, because a blank line belongs above a statement's
  own-line comment rather than between the comment and the statement, and
  comments are exactly what the parser throws away.

The reference implementation of those rules uses the TypeScript compiler API.
Reading it is what settled the shape below: of everything `ts.SourceFile`
offers, the rules touch `statements`, four `ts.isX` predicates,
`getStart`/`getEnd`, `getLineAndCharacterOfPosition`, `getFullText`, and
`getLeadingCommentRanges`. Nothing else.

## Decision

Ship `goccia:ast` as an experimental runtime module behind
`--experimental-ast`, exposing one function:

```js
import { parse } from "goccia:ast";

const { source, root, comments } = parse(text, { jsx: false, asi: false });
```

`root` is a tree of nodes:

```js
{ kind, start, end, loc: { start: { line, column }, end: { line, column } }, children }
```

`comments` is a flat, source-ordered array of `{ kind, start, end, loc }`,
where `kind` is `"Line"` or `"Block"`.

The tree models **statement structure only**. Every node is a statement, a
`SwitchCase`, or the `Program`, and `children` holds the statements nested
immediately beneath it — *including through expressions*: the block of an
arrow function assigned to a `const` is a child of that declaration, because
that is the only way a layout rule can find it.

Three kinds own a genuine statement *list*, where the children are siblings of
each other: `Program`, `BlockStatement`, and `SwitchCase`. A rule that asks
"what is next to what" walks the tree for those three and reads their
children, which is what the same rule does with `ts.isBlock` and
`ts.isCaseClause` against the TypeScript compiler API. The children of
anything else — the consequent of a braceless `if`, the two blocks of an
`if`/`else` — are nested, not adjacent, and no layout rule should pair them.

### What is exposed, and why

| Exposed | Why the consumer needs it |
|---|---|
| `kind` | The only thing a rule dispatches on. |
| `start` / `end` | The stable identity of a range, and what a future write-back would edit. |
| `loc` | What a finding is reported as. Deriving it in JavaScript would mean re-scanning the source for line starts that the engine has already indexed. |
| `children` | The sibling relation. "Statements next to each other in the same list" is the whole question for a layout rule. |
| `comments` | Trivia the AST does not contain, and which decides where a blank line goes. |
| `source` | Whitespace. No AST models a blank line; the rules read it out of the text between two offsets. |

### What is withheld, and why

- **Expressions.** All 43 expression kinds stay inside. The consumer's
  questions are about statement layout, and a prototype should not freeze a
  surface four times larger than the one it was designed for. A rule that
  needs to look inside a statement has its offsets and the source text.
- **Names, identifiers, and literal values.** Exposing them invites rules
  that are really semantic analysis, which is a different product with
  different requirements (scopes, binding resolution, cross-file state). A
  rule that needs a name can slice `source`.
- **Per-node `text`.** It is `source.slice(start, end)`, and it is the wrong
  primitive anyway: the rules need the gaps *between* nodes, which per-node
  text cannot express by construction. It would also copy every statement's
  text into the heap for every file.
- **Parent links.** Cyclic, and unnecessary for a top-down walk. The
  collector would have to be taught about the cycle to no benefit.
- **Comment attachment.** No `leadingComments` / `trailingComments`
  ownership. Attachment is a policy, and TypeScript, ESLint, and Prettier
  each choose a different one. A sorted list plus offsets lets a rule pick
  its own, which is what the reference implementation does.
- **Type annotations.** They are erased before the AST exists. There is
  nothing to expose.
- **A mutation or write-back API.** `parse` is a read. Rewriting source is a
  separate decision about ownership and atomicity; see
  [Consequences](#consequences).
- **Lazy nodes.** Every node is a plain object, built eagerly. A handle-based
  representation that materializes on access would be faster on a rule that
  visits little of the tree, but it is a complication to justify with a
  measurement, not ahead of one.

### Why a module and not a global

`goccia:ast` follows the existing runtime-module pattern
(`Goccia.RuntimeExtensions.NamespaceModule`), like `goccia:semver` and
`goccia:toml`. Parsing is not ambient authority — it reads a string the caller
already holds — but it is a large, not-yet-stable surface, so it is gated by
`--experimental-ast` in the same place `--unsafe-ffi` gates the FFI global.
The gate is about API stability, not capability.

## Consequences

- A lint rule can be a GocciaScript program. Run under `GocciaSandboxRunner`,
  which supplies `fs` over a seeded virtual filesystem, one program reads a
  tree of sources and reports findings, with no toolchain in front of it.
- **Offsets are into the parsed source, not the file.** With `jsx: true` the
  source goes through the JSX transformer first, so `start`, `end`, and
  `loc.column` index the transformed text. The transformer is
  line-preserving, so `loc.line` is the original file's line. `parse` returns
  the `source` it actually parsed, so a rule is never comparing offsets
  against text it has not seen — but a rule that wants original-file offsets
  in a `.tsx` file cannot have them yet.
- **There is no write-back.** A rule can find the offset to insert a newline
  at; it cannot apply it, because a sandbox rule that reports is a different
  risk from one that edits. `fs.writeFileSync` inside a seeded sandbox is the
  obvious path once the edit model is decided.
- The walker that flattens the Pascal AST into these nodes has its own
  `case` over the node classes. That is a second place that has to learn
  about a new AST node, next to the evaluator and the bytecode compiler. It
  is the main reason this is a prototype rather than a supported API: the
  seam it wants is a shared child-enumeration on `TGocciaASTNode`, which is
  a change to the AST itself and belongs in its own decision.
- Comment positions come from an opt-in trivia sink on the lexer rather than
  a second scanner, so regex-versus-division and template-literal
  classification stay in the one place that already decides them.

## Related

- [Architecture](../architecture.md) — source pipeline and parse artifacts
- [Virtual Module Configuration](../virtual-modules.md) — how module
  addresses resolve
- [ADR 0068](0068-goccia-sandbox-runner.md) — the host that gives a rule
  something to read
