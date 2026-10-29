# 0166 - goccia:ast reports the specifiers and module path of type-only module statements, and nothing else about them

**Date:** 2026-10-28
**Area:** `runtime`, `parser`
**Related:** [ADR 0161](0161-goccia-ast-reports-type-level-declarations-by-kind.md), [ADR 0162](0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md), [#1969](https://github.com/frostney/GocciaScript/issues/1969), [#1970](https://github.com/frostney/GocciaScript/issues/1970), [#1971](https://github.com/frostney/GocciaScript/issues/1971)

## Context

[ADR 0161](0161-goccia-ast-reports-type-level-declarations-by-kind.md) has the
parser skip each type-level declaration and leave a `TGocciaEmptyStatement`
that knows only its span and an `ErasedTypeKind`. It also says that a type-level
node has no children, in the expression view too.

That held while the expression view had nothing to say about module syntax. It
does now. [#1969](https://github.com/frostney/GocciaScript/issues/1969) reports
each import and export specifier as a node, as
[ADR 0162](0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
reports a parameter or an object member: a part with a range, between
`EnterPart` and `LeavePart`, carrying the flags a rule dispatches on. A
formatter that reads the text between a list's last element and its closing
bracket can then put a trailing comma on a multi-line `import { ... }`.

A formatter that normalises quotes or trailing commas has to see
`import type { A } from "m"`, `export type { A }` and `export type { A } from "m"`
as well. They are ordinary module syntax, and common in a TypeScript
repository. Over a 1,906-file repository formatted with single quotes, the
double-quoted module path of every `import type ... from "..."` survived a quote
fix, because a type-only statement reported no `StringLiteral`. Its specifier
lists got no trailing comma for the same reason.

The static keys of [#1971](https://github.com/frostney/GocciaScript/issues/1971)
use the same mechanism as the specifiers, and need no decision of their own.

## Decision

**A type-only module statement reports its specifiers and its module path as
children, in the expression view only.** `import type { A } from "./a"` has an
`ImportSpecifier` and a `StringLiteral`; `export type { T } from "./t"` an
`ExportSpecifier` and a `StringLiteral`; `export type { T }` the
`ExportSpecifier`; `export type * from "./t"` the `StringLiteral`. Each specifier
carries `typeOnly: true`. The statement keeps its kind, its `typeOnly: true`
and its whole-text range. `type` aliases and `interface` declarations, exported
or not, still have no children.

**The parser records offsets from tokens it already consumed.** After the skip,
it reads the specifier list and the `from "m"` back from the tokens between the
word after `type` and the end of the statement. It lexes nothing ahead, it never
raises, and a shape that is not an import or export clause (`import type A =
require("m")`) records nothing. The skip itself is unchanged. The ranges are
held on the empty statement as an array of offsets and flags, and `ForEachChild`
on that class reports them. No name is kept.

**There is no switch, and the compiler is not affected.** The compiler receives
the same node as before: the same class, the same span, and a field it never
reads. The class is not subclassed, for the reason ADR 0161 gives: the compiler
compares `ClassType` with `TGocciaEmptyStatement`. A plain `;` and the type
declarations hold no array, so the cost is one pointer on each empty statement
and an allocation for each type-only module statement.

### Rejected

- **Reporting the children from the flattener, by scanning `source`.** That
  repeats the parser's decision in a second grammar, which is what
  [ADR 0162](0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
  moved the flattener away from. Telling `export type { A }` from
  `export type { A } from` and finding the end of a specifier needs the tokens,
  which the parser has.
- **Parsing the statements into real import and export declarations with an
  erased flag.** The compiler would have to learn to skip them, and a
  type-only import would take part in module linking, which erasure exists to
  avoid.
- **A subclass of `TGocciaEmptyStatement`.** It falls out of the compiler's
  exact-class tests.
- **A parse-only retention mode.** The data is a few offsets for statements that
  are rare next to the rest of a file. A switch pays for itself only when what it
  retains costs what running a program must not pay.
- **Names in the tree.** A rule slices them from `source`, as for every other
  node.

## Consequences

- ADR 0161's "a type-level node has no children" holds for `type` and
  `interface` declarations and no longer for the module forms. Its record of
  what the parser keeps and the node it hands the compiler is unchanged.
- `typeOnly` is now on eleven kinds, not seven: the four specifier kinds
  (`ImportDefaultSpecifier`, `ImportNamespaceSpecifier`, `ImportSpecifier` and
  `ExportSpecifier`) carry it too, `true` for a specifier written with `type` or
  in a type-only statement, `false` otherwise.
- A namespace re-export (`export * as ns from "m"`, `export type * as ns from "m"`)
  reports its module path and no node for `* as ns`. #1969 names four specifier
  kinds and none for it; typescript-estree has `ExportNamespaceSpecifier`, which
  a later change can add the same way.
- `import type D from "m"` and `import type * as NS from "m"` report
  `ImportDefaultSpecifier` and `ImportNamespaceSpecifier` with `typeOnly: true`,
  by the rule that every specifier of a type-only statement is type-only.
- The statement view is unchanged: a specifier is a part, and a part is not
  reported without `expressions: true`.
