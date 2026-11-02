# 0172 - goccia:ast reports erased type syntax as nodes, which the parser records only when the expression view asks

**Date:** 2026-11-02
**Area:** `runtime`, `parser`
**Related:** [ADR 0117](0117-javascript-visible-ast-module.md), [ADR 0161](0161-goccia-ast-reports-type-level-declarations-by-kind.md), [ADR 0162](0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md), [ADR 0168](0168-goccia-ast-reports-the-children-of-type-only-module-statements.md), [#1929](https://github.com/frostney/GocciaScript/issues/1929), [#1549](https://github.com/frostney/GocciaScript/issues/1549)

## Context

The parser erases types before the tree exists. It collects an annotation as
a run of tokens it skips, balancing brackets and nothing more, and skips a
`type` or `interface` declaration whole. [ADR 0162](0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
left type ranges out of the expression view for that reason, and named them
as their own slice.

That slice is now the largest gap for a formatter written against the tree. A
1,890-file TypeScript repository was reformatted with double quotes and no
trailing commas, and a `goccia:ast` program then put both back. Of the 893
files that still differed from Biome's output, 13 do not parse. Nearly all of
the other 880 differ inside types: a trailing comma in a type-member list or
type parameter list, or the quotes of a string literal type such as
`'a' | 'b'`. Nothing in the tree says where those are, and re-deriving them
from tokens is the heuristic work that ADR 0162 removed from the formatter.

[ADR 0161](0161-goccia-ast-reports-type-level-declarations-by-kind.md) decided
that a type declaration has no children. [ADR 0168](0168-goccia-ast-reports-the-children-of-type-only-module-statements.md)
kept that for `type` and `interface` declarations when it gave the type-only
module forms children. ADR 0161 also predicted how this slice would differ:
keeping type content, annotation ranges included, "allocates and changes what
the parser builds", and that is where a parse switch belongs.

## Decision

### Type syntax is a tree of nodes, below the outer range

In the expression view each piece of erased type syntax is a node with a
range in the caller's text: an annotation, a type, and the structure inside a
type. A union reports its members, an object type its members, a tuple its
elements, a generic its type arguments or type parameters, a function type
its parameters and return type, and an indexed access `T["k"]` its object type
and key. A literal type holds its literal, so the quote character of `'a'` is
`source[node.start]` of a `StringLiteral`.

Kinds take the names typescript-estree gives them: `TSTypeAnnotation`,
`TSUnionType`, `TSTypeLiteral`, `TSPropertySignature`, `TSLiteralType`,
`TSTypeParameterInstantiation`, `TSFunctionType` and the rest, listed in
[AST Built-ins](../built-ins-ast.md#type-syntax). A `TSTypeAnnotation` covers
its `:`, and the `=>` of a function type. Where the view already has a kind for
the syntax, it is reused: a function type's parameter is a `Parameter`, a
member's static key an `Identifier`, `StringLiteral` or `NumericLiteral`.
`TSParenthesizedType`, which typescript-estree dropped, is kept, as
`ParenthesizedExpression` is.

Three things differ from typescript-estree:

- **A member's range stops before its `,` or `;`.** typescript-estree includes
  it. An object literal's `Property` and a module specifier stop before their
  comma, so the text between the last member and the closing brace is the
  separator a formatter looks for, in a type as in an expression.
- **A type's name is not a node.** `Map<K, V>` is a `TSTypeReference` holding
  its type arguments, with no `Identifier` or `TSQualifiedName` for `Map`. As
  everywhere in the result, a rule slices names from `source`.
- **A negative number's literal is the number.** In `-1` the `NumericLiteral`
  covers `1`, and the `TSLiteralType` covers the `-`.

### Type nodes are children of what they annotate

A parameter's annotation is a child of its `Parameter`, a return type and
type parameters of the function or method, a variable's annotation of its
`VariableDeclaration`, a field's of its `PropertyDefinition`, a catch
parameter's of the `TryStatement`, and an assertion's type of its
`TSAsExpression` or `TSSatisfiesExpression`, after the expression it asserts.
A call's and a `new` expression's type arguments come between the callee and
the arguments. A class reports its type parameters, its superclass's type
arguments, and each `implements` entry as a `TSClassImplements`.

**A `type` or `interface` declaration has children.** A `type` alias reports
its type parameters and its type; an `interface` its type parameters, each
`extends` entry as a `TSInterfaceHeritage`, and its `TSInterfaceBody`. This
departs from ADR 0161's "a type declaration has no children", and from
ADR 0168, which kept it for these two kinds. The statement view is unchanged:
it still reports no children for them.

### The parser records it, only when asked

`TGocciaParserOptions.RecordTypeRanges` turns recording on, and only
`goccia:ast` sets it, when `expressions: true` is passed. Once the parser has
committed to a run of type tokens, `Goccia.Parser.TypeRanges` reads the run
again and returns the tree as an array of ranges in source order, each
followed by the ranges inside it. It reads only tokens already consumed,
lexes nothing, never reads past the run and never raises: a shape it does not
recognise is passed over. It is never called from inside a probe, so it
cannot turn a speculative reading into a `SyntaxError` (CONTRIBUTING rule 5).
Ranges nest by construction, so a misread type gets the wrong kinds, never
overlapping or escaping ranges. Past 256 levels of nesting the rest of a type
is not structured.

The array is held where the syntax belongs: on the parameter, variable and
class member records, on the expression it annotates, on the class
definition, on the try statement, and on the empty statement an erased
declaration leaves. Each node's `ForEachChild` reports it as parts, between
`EnterPart` and `LeavePart`, so the flattener still walks the tree only
through `ForEachChild`.

Without the switch the parser does what it did, and builds the same nodes.
The compiler never reads the arrays. Each expression, parameter, variable,
class member, class definition, try statement and empty statement carries one
more pointer, which is nil unless the switch is on.

### Rejected

- **Only the outer range of each annotation.** The issue left open whether
  type nodes get structure. The measured gap is trailing commas inside
  type-member and type-parameter lists, and quotes inside unions, so the
  outer range alone would not close it.
- **Recording on every parse, with no switch.** ADR 0168 chose that for
  type-only module statements, which are a few offsets on rare statements.
  Annotations are on most parameters and declarations of a TypeScript file,
  and recording means a second pass over their tokens and an allocation per
  annotation, which running a program must not pay for. This is the switch
  ADR 0161 placed with type content.
- **Re-deriving types in the flattener from `source`.** That is a second
  grammar outside the parser, which ADR 0162 and ADR 0168 rejected. Telling
  `T[]` from `T [K]` on the next line, or a call signature from a
  parenthesized type, needs the tokens the parser consumed.
- **Real type nodes in the parse tree.** Subclasses of `TGocciaASTNode` would
  join the arena and the compiler's view of the tree, for syntax that never
  runs. A flat array of ranges reports the same parts with neither.
- **Including the separator in a member's range,** as typescript-estree does.
  It would make a type literal the one list in the view whose trailing comma
  is inside its last element.

## Consequences

- A formatter can find every comma-separated list and every string literal
  in a type: object-type and interface members, tuple elements, type arguments
  and type parameters, function-type parameters, union members, and the key
  of an indexed access.
- Only syntax the parser accepts is reported. Overload signatures, `declare`,
  `abstract` members, class index signatures, generic object methods and
  parameter properties are not parsed today, and gain nothing here.
- `examples/_experimental/ast-ranges.mjs` re-parses each type node inside the
  smallest declaration that holds it, and over the repository's `website/src`
  and `scripts` every type node round-trips.
- The expression view gains 48 node kinds, all prefixed `TS`. A rule that
  dispatches on expression kinds and ignores the rest is unaffected.
