# AST Built-ins

*For script authors who read source code from GocciaScript through the experimental `goccia:ast` module.*

## Executive Summary

- **Experimental opt-in** — `goccia:ast` exists only when the host passes `--experimental-ast`; its shape may change between releases
- **One function** — `parse(text, options)` returns the text, a tree of nodes, and every comment, so a lint rule, a formatter or a codemod can be a GocciaScript program
- **Tokens on request** — with `tokens: true` the result also carries every token and comment as a typed range, so a tool can tell code from strings, templates and regexes without its own lexer
- **Two views** — by default the tree holds statements; with `expressions: true` it also holds expressions, object properties, parameters, class members, module specifiers, static keys and type syntax, with the flags a layout rule dispatches on
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
| `tokens` | Also return `tokens`, every token and comment of the file (default `false`). Without it no token work is done; see [Tokens](#tokens) |
| `expressions` | Report expressions, object properties, parameters, class members, module specifiers, static keys and type syntax as well as statements (default `false`); see [The expression view](#the-expression-view) |

The *language* is the host's, not a second one configured here: the
compatibility flags in force for the run are the ones `parse` accepts, exactly
as `eval` and the `Function` constructor inherit them. A file that needs
`--compat-asi` to parse needs the host to have been given `--compat-asi`.

## The result

`parse` returns `{ source, root, comments }`, and `tokens` when asked for:

| Property | Description |
|----------|-------------|
| `source` | The text that was passed, with each CRLF and lone CR normalized to LF and otherwise unchanged. Every offset and position in the result indexes it |
| `root` | The `Program` node |
| `comments` | Every comment, in source order |
| `tokens` | Only with `tokens: true`: every token and comment, in source order |

Every node is `{ kind, start, end, loc, children }`, plus `typeOnly` on the
kinds a type-level declaration or a module specifier can have (below), and every comment is
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

Type-level declarations do nothing at run time ([Type Annotations](type-annotations.md)), but each keeps a kind of its own and its whole text, wherever it appears: `type T = ...` is a `TypeAliasDeclaration`, `interface I {}` an `InterfaceDeclaration`, an overload signature (`function f(a: string): string;`) an `OverloadDeclaration`, and their exported forms `ExportTypeAliasDeclaration`, `ExportInterfaceDeclaration` and `ExportOverloadDeclaration`. `import type ... from`, `export type { A }`, and `export type { A } from` or `export type * from` are the `ImportDeclaration`, `ExportDeclaration` and `ReExportDeclaration` they look like, with `typeOnly: true`. `import type from "m"`, `import type, { y } from "m"` and `import type, * as ns from "m"` are value imports that bind `type`, so they report `typeOnly: false`. Those three kinds carry `typeOnly: false` when the statement runs, including `import { type C, D } from "..."`, where `type` marks one specifier rather than the statement. The six type declaration kinds always carry `typeOnly: true`, and `EmptyStatement` is only ever a lone `;`. Only those nine statement kinds and the four specifier kinds (below) carry `typeOnly`. In the statement view a type declaration has no children and no names, and neither has a type-only `import`, `export { }` or re-export. In the expression view a type-only module statement reports its specifiers and module path like a value one ([ADR 0168](adr/0168-goccia-ast-reports-the-children-of-type-only-module-statements.md)), and a type declaration its [type syntax](#type-syntax); an overload signature has no children in either view, and a class method's overload signature is not a member. See [ADR 0161](adr/0161-goccia-ast-reports-type-level-declarations-by-kind.md).

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
`AccessorProperty`, `StaticBlock`, `ImportDefaultSpecifier`,
`ImportNamespaceSpecifier`, `ImportSpecifier`, `ExportSpecifier`, `JSXElement`, `IsExpression`,
`MatchExpression`, and TypeScript's `TSAsExpression` (`x as T`, `x as const`),
`TSSatisfiesExpression` and `TSNonNullExpression` (`x!`). An import's or
re-export's module path is a `StringLiteral` child of its declaration, after
its specifiers (below). An
assertion wraps the expression it asserts and covers its type, so in
`f(a as T)` the argument ends after `T`; the type is its second child.

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
| `TSPropertySignature`, `TSMethodSignature` | `computed` (`[key]: T`) |
| `Parameter` | `rest` (`...args`) |
| `MethodDefinition`, `PropertyDefinition`, `AccessorProperty` | `static`, `computed` |
| `ImportDefaultSpecifier`, `ImportNamespaceSpecifier`, `ImportSpecifier`, `ExportSpecifier` | `typeOnly`: `true` for a specifier written with `type` (`import { type C }`) and for every specifier of a type-only statement, `false` otherwise |

Names and literal values stay out of the tree, as in the statement view: a
rule slices them from `source`, so a string literal's quote character is
`source[node.start]`. A `Parameter` covers its type annotation and default
value; a class member covers its decorators and modifiers; a `Property`
covers its key and value. A method's parameters and body are children of its
`Property` or `MethodDefinition`, and a function declaration's of the
declaration.

A specifier covers the whole of `d`, `* as ns`, `b as bb` or `f as g`, a `type`
modifier and a string-literal name included, so the text between the last one
and the closing brace is the comma a formatter looks for. `import d, { b, c }`
reports `ImportDefaultSpecifier`, two `ImportSpecifier` and the path, in source
order; `export * as ns from "m"` reports the path only.

A non-computed key is the member's first child, after any decorators: `Identifier`
for a name, including a private name with its `#`, `StringLiteral` for a quoted
key, and `NumericLiteral` or `BigIntLiteral` for a number. `get`, `set`,
`static`, `async`, `accessor` and `*` are not part of it, and a shorthand
property reports one `Identifier`, its value. A computed key is the expression
it is.

Not yet in the view: variable declarators, the properties of an object
pattern and their keys, labels, the patterns of `match`, and JSX attributes
and text. What they contain is reported directly beneath the node around them
([#1928](https://github.com/frostney/GocciaScript/issues/1928)).

## Type syntax

Types do nothing at run time and the parser erases them, but with
`expressions: true` it first records where each piece was, and the view
reports it as nodes ([ADR 0172](adr/0172-goccia-ast-reports-erased-type-syntax-recorded-on-request.md)).
Kinds are typescript-estree's:

| Kind | The range covers |
|------|------------------|
| `TSTypeAnnotation` | `: T` after a parameter, variable, field or catch parameter, or a function's return type; `=> R` in a function type |
| `TSTypeParameterDeclaration`, `TSTypeParameter` | `<T extends U = V>` where generics are declared, and each parameter in it |
| `TSTypeParameterInstantiation` | `<A, B>` where generics are applied: a type reference, a call, a `new`, a superclass |
| `TSTypeReference` | A named type, qualified or not, with its type arguments; `const` in `as const` |
| `TSUnionType`, `TSIntersectionType` | Members joined by `\|` or `&`, a leading operator included |
| `TSTypeLiteral`, `TSInterfaceBody` | `{ ... }`, holding `TSPropertySignature`, `TSMethodSignature`, `TSCallSignatureDeclaration`, `TSConstructSignatureDeclaration` and `TSIndexSignature` members |
| `TSTupleType` | `[...]`, holding its element types, `TSNamedTupleMember` (`a?: T`), `TSOptionalType` (`T?`) and `TSRestType` (`...T`) |
| `TSArrayType`, `TSIndexedAccessType` | `T[]`, and `T["k"]` with its object type and key |
| `TSFunctionType`, `TSConstructorType` | `(a: T) => R` and `new (a: T) => R`, with type parameters, `Parameter` nodes and the `TSTypeAnnotation` of `=> R` |
| `TSLiteralType` | `'a'`, `1`, `-1`, `2n`, `true` or `` `a` ``, holding the `StringLiteral`, `NumericLiteral`, `BigIntLiteral`, `BooleanLiteral` or `TemplateLiteral` |
| `TSTemplateLiteralType` | `` `id-${number}` ``, holding the types in its substitutions |
| `TSMappedType` | `{ [K in C as N]: V }`, holding `C`, `N` and `V` |
| `TSConditionalType`, `TSInferType` | `A extends B ? C : D`; `infer U`, holding its `TSTypeParameter` |
| `TSTypeOperator`, `TSTypeQuery`, `TSImportType` | `keyof T`, `readonly T[]`, `unique symbol`; `typeof x`; `import("m").T`, with its path as a `TSLiteralType` |
| `TSTypePredicate`, `TSParenthesizedType`, `TSThisType` | `x is T`, `asserts x`; `(T)`; `this` |
| `TSInterfaceHeritage`, `TSClassImplements` | An entry of an interface's `extends` list or a class's `implements` list |
| `TSStringKeyword`, `TSNumberKeyword`, and the other predefined types | `string`, `number`, `boolean`, `bigint`, `symbol`, `object`, `any`, `unknown`, `never`, `undefined`, `void`, `null` and `intrinsic` |

Each is a child of what it annotates: a parameter's annotation of its
`Parameter`, a function's type parameters and return type of the function, a
variable's annotation of its `VariableDeclaration`, a call's type arguments of
the `CallExpression`, between its callee and arguments. A `type` alias holds its
type parameters and its type, and an `interface` its type parameters,
heritage and body. A member's static key is an `Identifier`, `StringLiteral`
or `NumericLiteral`, as in an object literal, and so is a computed key that
is one literal (`['k']`); any other computed key has no node. A type's name
is not a node either: slice it from `source`.

A member's range stops before the `,` or `;` that follows it, so the text
between the last member and the closing brace is the separator a formatter
looks for:

```javascript
const { source, root } = parse("type Point = {\n  x: number;\n  y: number;\n};\n",
  { fileName: "point.ts", expressions: true });
const literal = root.children[0].children[0];   // TSTypeLiteral
source.slice(literal.children.at(-1).end, literal.end);   // ";\n}"
```

## Tokens

With `tokens: true`, `tokens` is every token and comment of the file as
`{ type, start, end }`, in source order, with no gaps but whitespace and no
overlaps. They are the tokens the parse consumed, so whether a `/` divides or
opens a regular expression, and where a template's text ends and its code
begins, is the parser's own answer. Blanking every literal, template and
comment range leaves exactly the code, at the same offsets:

```javascript
const CODE = ["Identifier", "PrivateIdentifier", "Keyword", "Punctuator",
  "NumericLiteral", "JSXIdentifier"];

const { source, tokens } = parse(text, { tokens: true });
const code = tokens
  .filter((token) => !CODE.includes(token.type))
  .reduce((blanked, { start, end }) =>
    blanked.slice(0, start) + " ".repeat(end - start) + blanked.slice(end), source);
```

| `type` | The range covers |
|--------|------------------|
| `Identifier` | A name that is not a reserved word, including contextual words such as `let`, `async`, `of` and `type`, and a reserved word spelled with an escape |
| `PrivateIdentifier` | `#name` |
| `Keyword` | A reserved word (ES2026 §12.7.2), by spelling: `x.default` has a `Keyword` token |
| `Punctuator` | An operator or punctuator. `>>` closing two type-argument lists is one token |
| `NumericLiteral` | A number, BigInts included |
| `StringLiteral` | A string with its quotes, or a JSX attribute string |
| `RegularExpressionLiteral` | A regular expression with its slashes and flags |
| `NoSubstitutionTemplate` | A template with no `${`, backticks included |
| `TemplateHead` | `` `text${ `` |
| `TemplateMiddle` | `}text${` |
| `TemplateTail` | `` }text` `` |
| `LineComment`, `BlockComment` | A comment with its delimiters |
| `HashbangComment` | A leading `#!` line. It is not in `comments` |
| `JSXIdentifier` | A JSX tag or attribute name |
| `JSXText` | The text between JSX tags, whitespace included |

A template's parts hold its text and delimiters only; the code inside each
`${...}` is tokenized as code, nested templates included. Values are withheld
as everywhere else in the result, and tokens have no `loc`: slice `source`.
Like every offset in the result, they index `source`, so in a CRLF file they
count one LF for each CRLF.

With `jsx` on, a token the JSX transformer synthesized is not reported, and
JSX's own names, punctuation, strings and text are, measured in the file.
The comments the transformer drops, which are missing from `comments`, are
in `tokens`.

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
system, [ADR 0161](adr/0161-goccia-ast-reports-type-level-declarations-by-kind.md)
for type-level declarations,
[ADR 0162](adr/0162-the-parse-tree-reaches-expressions-on-request-through-one-child-enumeration.md)
for the expression view,
[ADR 0163](adr/0163-goccia-ast-reports-the-committed-token-stream.md) for the
token stream,
[ADR 0168](adr/0168-goccia-ast-reports-the-children-of-type-only-module-statements.md)
for the children of type-only module statements, and
[ADR 0172](adr/0172-goccia-ast-reports-erased-type-syntax-recorded-on-request.md)
for type syntax. `examples/_experimental/` has a blank-line lint
rule, a range checker, and a formatter-style program
(`expression-style.mjs`) written against it.
