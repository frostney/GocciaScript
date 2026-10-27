# AST Built-ins

*For script authors who read source code from GocciaScript through the experimental `goccia:ast` module.*

## Executive Summary

- **Experimental opt-in** — `goccia:ast` exists only when the host passes `--experimental-ast`; its shape may change between releases
- **One function** — `parse(text, options)` returns the text, a tree of nodes, and every comment, so a lint rule, a formatter or a codemod can be a GocciaScript program
- **Two views** — by default the tree holds statements; with `expressions: true` it also holds expressions, object properties, parameters and class members, with the flags a layout rule dispatches on
- **One coordinate system** — every offset and position indexes the text that was passed, including a `.tsx` file the engine rewrote before parsing
- **Ranges, not values** — names and literal values stay out of the tree; a rule slices them from `source`

## Parsing

Only available when the host passes `--experimental-ast`. `goccia:ast` makes
the parse result reachable from JavaScript, so a tool that asks questions
about source — a lint rule, a codemod, a metric — can be a GocciaScript
program instead of a Pascal one. It is a read: there is no write-back.

```javascript
import { parse } from "goccia:ast";

const { source, root, comments } = parse(text, { jsx: true, fileName: "App.tsx" });
```

| Option | Meaning |
|--------|---------|
| `jsx` | Run the JSX preprocessor before parsing (default `false`) |
| `module` | Parse as module source rather than script source (default `true`) |
| `fileName` | File name the parser sees; its extension decides JSX handling, so a `.ts` name skips the JSX preprocessor (default `<source>`). A syntax error reports only `(line:column)`, not the file name ([#1277](https://github.com/frostney/GocciaScript/issues/1277)) |
| `expressions` | Report expressions, object properties, parameters and class members as well as statements (default `false`); see [The expression view](#the-expression-view) |

The *language* is the host's, not a second one configured here: the
compatibility flags in force for the run are the ones `parse` accepts, exactly
as `eval` and the `Function` constructor inherit them. A file that needs
`--compat-asi` to parse needs the host to have been given `--compat-asi`.

## The result

`parse` returns `{ source, root, comments }`:

| Property | Description |
|----------|-------------|
| `source` | The text that was passed, returned unchanged. Every offset and position in the result indexes it |
| `root` | The `Program` node |
| `comments` | Every comment, in source order |

Every node is `{ kind, start, end, loc, children }`, and every comment is
`{ kind, start, end, loc }` with `kind` either `"Line"` or `"Block"`.
`start` and `end` are zero-based offsets into `source`, `end` exclusive;
`loc.start` and `loc.end` are `{ line, column }` with one-based lines.

A syntax error in the parsed text is a `SyntaxError` naming its position; it
does not abort the caller's own run unless the caller lets it.

## The statement view

By default the tree holds statements only — no expressions, names, or
literal values. `children` are the statements nested immediately beneath a
node, reached through expressions where it has to be, so the block of an
arrow function assigned to a `const` is a child of that declaration. Three
kinds own a statement *list*, whose children are siblings of one another:
`Program`, `BlockStatement`, and `SwitchCase`.

## The expression view

With `expressions: true` the same tree also holds every expression beneath
its statement, so `children` are the nodes nested immediately beneath a node
of any kind, in source order. A node's range is the text it covers, operands
and parentheses included: in `x = (a + b) * c` the `BinaryExpression` for `*`
runs from `(` to `c`, and the parentheses are a `ParenthesizedExpression`
node around `a + b`.

Kinds follow ESTree, with Babel's literal kinds: `Identifier`,
`StringLiteral`, `NumericLiteral`, `BigIntLiteral`, `BooleanLiteral`,
`NullLiteral`, `RegExpLiteral`, `TemplateLiteral`,
`TaggedTemplateExpression`, `ThisExpression`, `Super`, `MetaProperty`,
`ArrayExpression`, `ObjectExpression`, `Property`, `SpreadElement`,
`FunctionExpression`, `ArrowFunctionExpression`, `ClassExpression`,
`UnaryExpression`, `UpdateExpression`, `BinaryExpression`,
`LogicalExpression`, `AssignmentExpression`, `ConditionalExpression`,
`SequenceExpression`, `CallExpression`, `NewExpression`, `MemberExpression`,
`ImportExpression`, `AwaitExpression`, `YieldExpression`,
`ParenthesizedExpression`, `ArrayPattern`, `ObjectPattern`, `RestElement`,
`AssignmentPattern`, `Parameter`, `MethodDefinition`, `PropertyDefinition`,
`AccessorProperty`, `StaticBlock`, `JSXElement`, `IsExpression`, and
`MatchExpression`. An import's or re-export's module path is a
`StringLiteral` child of its declaration.

A node carries the flags its kind is laid out by, and no others:

| Kind | Flags |
|------|-------|
| `UnaryExpression`, `BinaryExpression`, `LogicalExpression`, `AssignmentExpression` | `operator`, the operator as written (`"+"`, `"??"`, `"instanceof"`, `"+="`) |
| `UpdateExpression` | `operator` (`"++"` or `"--"`), `prefix` |
| `MemberExpression` | `computed` (`a[b]`), `optional` (`a?.b`) |
| `CallExpression` | `optional` (`f?.()`) |
| `ArrowFunctionExpression` | `async`, `bodyKind` (`"block"` or `"expression"`) |
| `FunctionExpression` | `async`, `generator` |
| `YieldExpression` | `delegate` (`yield*`) |
| `Property` | `computed`, `shorthand` (`{ a }`), `method` (`a() {}`, `get a() {}`) |
| `Parameter` | `rest` (`...args`) |
| `MethodDefinition`, `PropertyDefinition`, `AccessorProperty` | `static`, `computed` |

Names and literal values stay out of the tree, as in the statement view: a
rule slices them from `source`, so a string literal's quote character is
`source[node.start]`. A `Parameter` covers its type annotation and default
value; a class member covers its decorators and modifiers; a `Property`
covers its key and value. A method's parameters and body are children of its
`Property` or `MethodDefinition`, and a function declaration's of the
declaration.

Not yet in the view: type annotations (erased before the tree is built),
variable declarators, the properties of an object pattern, labels, the
patterns of `match`, and JSX attributes and text. What they contain is
reported directly beneath the node around them
([#1928](https://github.com/frostney/GocciaScript/issues/1928),
[#1929](https://github.com/frostney/GocciaScript/issues/1929)).

## JSX sources

With `jsx` on, the text is rewritten before it is parsed, but nothing in the
result is measured in that rewrite: offsets and positions alike are mapped back
to the file as written, so `source.slice(node.start, node.end)` is the node's
own text. Comments the transformer removes — one between JSX attributes, or a
child container holding nothing else — are not in `comments`, because the
parser never sees them.

In the expression view an element is a `JSXElement` covering the element as
written, and the expressions in its braces are its children. Nothing the JSX
transformer writes is reported — not the call it turns an element into, not
the string it escapes an attribute value into — so every node's range is text
in `source`.

## Related

See [ADR 0117](adr/0117-javascript-visible-ast-module.md) for what the module
exposes, what it withholds, and why,
[ADR 0118](adr/0118-original-file-source-ranges.md) for the one coordinate
system, and [ADR 0162](adr/0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
for the expression view. `examples/_experimental/` has a blank-line lint
rule, a range checker, and a formatter-style program
(`expression-style.mjs`) written against it.
