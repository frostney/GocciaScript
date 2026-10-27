# 0161 - goccia:ast reports type-level declarations by kind, with no parse switch

**Date:** 2026-10-26
**Area:** `runtime`, `parser`
**Related:** [ADR 0117](0117-javascript-visible-ast-module.md), [ADR 0118](0118-original-file-source-ranges.md), [Type Annotations](../type-annotations.md), [#1525](https://github.com/frostney/GocciaScript/issues/1525), [#1526](https://github.com/frostney/GocciaScript/issues/1526), [#1549](https://github.com/frostney/GocciaScript/issues/1549)

## Context

Types are comments at run time, so the parser skips each type-level
declaration (`type`, `interface`, `import type`, `export type`,
`export interface`) and leaves a `TGocciaEmptyStatement` covering its text.
[ADR 0117](0117-javascript-visible-ast-module.md) named statement kinds as the
thing a rule dispatches on, and `goccia:ast` reported all of these as
`EmptyStatement`. A rule could not tell a type declaration from a stray `;`.
An import-rewriting codemod that walked `ImportDeclaration` nodes skipped 58
`import type` lines in a 123-file tree.

ADR 0117 also said of type annotations that there was nothing to expose,
because they are gone before the tree exists. The open question was whether
keeping the distinction needs a parse switch that only `goccia:ast` turns on,
so that running a program is not affected.

## Decision

**Each type-level declaration is reported as what it was written as, over its
whole text.** `type` and `interface` declarations get their own kinds:
`TypeAliasDeclaration`, `InterfaceDeclaration`, `ExportTypeAliasDeclaration`
and `ExportInterfaceDeclaration`. The module forms keep the kind of the
statement they look like: `import type` is an `ImportDeclaration`,
`export type { A }` an `ExportDeclaration`, and `export type { A } from` or
`export type * from` a `ReExportDeclaration`. A `typeOnly` property tells them
apart. It is `true` on every type-level node and `false` on a runtime import or
export, and no other kind carries it. `EmptyStatement` is left for a lone `;`.

Keeping the module kinds follows the failure that was reported. A tool walking
imports must find `import type` lines without first learning a second kind.
The TypeScript compiler API, the reference ADR 0117 was designed against, makes
the same choice: `isTypeOnly` is a flag on the import clause and on the export
declaration, and the declarations have kinds of their own.

**The parser records what it erased, and nothing else changes.** The empty
statement the parser already creates now holds an `ErasedTypeKind` (`etkNone`
for a real `;`), set at the five places that skip a type-level declaration. The
skipped tokens are still skipped, and no type text, name or member is kept. The
only extra work is reading the token after `export type` before the skip, and
afterwards checking the tokens already consumed for a `from` after the
specifier list.

**There is no switch.** The bytecode compiler, the only thing that runs a
parsed program, receives the same node it did before: the same class, the same
span and no children. It never reads the new field. The cost is one enum field on each empty statement,
which is small next to the span and label list every statement already has.

The tag is a field and not a subclass, because the compiler tests the exact
class in places: `Goccia.Compiler.OperandSafety` and
`Goccia.Compiler.NumericBindings` compare `ClassType` with
`TGocciaEmptyStatement`, and a subclass would fall out of those branches.

### Rejected

- **A parse-only retention mode.** A switch is worth having only when parsing
  for a tool keeps something that running must not pay for. Here it would keep
  nothing. It belongs with type *content*: names, members or annotation
  ranges. Keeping those allocates and changes what the parser builds, which is
  the per-call parse options of [#1526](https://github.com/frostney/GocciaScript/issues/1526)
  and the expression-level tree of [#1549](https://github.com/frostney/GocciaScript/issues/1549).
  This decision does not constrain either one, because the tag is set
  unconditionally.
- **Classifying in the flattener from the leading keyword.** That repeats a
  decision the parser has already made, and the workaround the codemod used
  did exactly this. Telling `export type { A }` from `export type { A } from`
  also needs the tokens, which only the parser has.
- **Distinct kinds for the module forms** (`ImportTypeDeclaration` and so on).
  These keep the node shape at five properties, but every import walker would
  have to know a second kind, and missing it is the bug that was reported.

## Consequences

- `typeOnly` extends the node shape ADR 0117 fixed at
  `{ kind, start, end, loc, children }`. It is present on seven kinds only, so
  the properties a node has still depend on nothing but its kind.
- `import type from "./x"` and `import type, { y } from "./x"` are value
  imports, the first binding named `type`, but the parser erases both today, so
  neither `type` nor `y` is bound. They are therefore reported as type-only
  imports. The report matches what the engine runs; the erasure itself is
  wrong.
- `declare` forms ([#1527](https://github.com/frostney/GocciaScript/issues/1527))
  and `export default interface` are still syntax errors and have no kind.
- `examples/_experimental/ast-ranges.mjs` re-parses the exported type kinds at
  the top level, as it does the other module kinds. Over `website/src`, run with
  the nine `--compat-*` flags its code needs, every node round-trips except the
  `Program` nodes, and the one file that does not parse fails on
  `declare global`.
