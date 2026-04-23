# Language

*GocciaScript's ECMAScript subset: what we implement, TC39 proposals, and what we exclude.*

## Executive Summary

- **Modern subset** — `let`/`const`, arrow functions, classes with private fields, `for...of`, async/await, ES modules (named only)
- **TC39 proposals** — Decorators, decorator metadata, types as comments, enums, `Math.clamp`
- **Excluded by design** — `==`/`!=`, `eval`, `arguments`, traditional loops, `with`, default imports/exports
- **Graceful handling** — Parser-recognized excluded syntax (`==`, loops, `with`) parses successfully but executes as a no-op with a warning and suggestion
- **Opt-in toggles** — ASI (`--asi`), `var` declarations (`--compat-var`), `function` keyword (`--compat-function`)
- **Default preprocessors** — JSX (enabled by default via `DefaultPreprocessors`)

GocciaScript implements a curated subset of ECMAScript. This document details what's supported, what's excluded, and the rationale for each decision. For quick-reference tables of every feature and TC39 proposal, see [Language Tables](language-tables.md).

## Guiding Principle

> **A drop of JavaScript** — GocciaScript includes the parts of JavaScript that lead to clear, predictable, and secure code. Features that are error-prone, redundant, or security risks are excluded.

## Supported Features

### Variables

- `let` — Block-scoped mutable variable.
- `const` — Block-scoped immutable binding.

```javascript
let count = 0;
const name = "Goccia";
```

### Functions

- Arrow functions by default (`function` keyword via `--compat-function`).
- Default parameters (including in destructured parameters).
- Rest parameters (`...args`).
- Destructuring parameters (array and object patterns with defaults).
- `fn.call()`, `fn.apply()`, `fn.bind()` for explicit `this` binding.
- `fn.length` — Number of formal parameters (before defaults/rest).
- `fn.name` — Function name (inferred from variable declarations for anonymous functions).
- Type annotations on parameters and return types (parsed and ignored at runtime — see [Types as Comments](#types-as-comments-stage-1) below).
- `async`/`await` — Async functions return Promises; `await` suspends until the Promise settles (see [Async Functions](#async-functions) below).

```javascript
const add = (a, b) => a + b;
const greet = (name = "world") => `Hello, ${name}!`;
const first = ([head, ...rest]) => head;
add.name;   // "add"
add.length; // 2
```

### Async Functions

`async`/`await` syntax sugar for Promises is fully supported.

**`async` arrow functions:**

```javascript
const fn = async () => { return 42; };
fn(); // Returns a Promise that resolves to 42
```

**`async` methods in objects:**

```javascript
const obj = {
  async fetch() {
    return await fetchData();
  }
};
```

**`async` class methods:**

```javascript
class C {
  async method() {
    return await Promise.resolve(1);
  }
}
```

**`await` expressions:** Valid inside `async` functions and at the top level (ES2022+ top-level `await`). Suspends execution until the Promise settles; returns the fulfilled value or throws the rejection reason. Top-level `await` works identically in both interpreted and bytecode modes.

**Return semantics:** Async functions always return a Promise. If the body returns a value, the Promise resolves to that value. If the body throws, the Promise rejects.

### Classes

- Class declarations and expressions.
- Constructor, instance methods, static methods.
- Getters and setters.
- Private fields and methods (`#field`).
- Static properties.
- Static blocks (`static { ... }`).
- Inheritance with `extends` and `super`.
- Decorators (see [Decorators](#decorators-stage-3) in TC39 Proposals below).

```javascript
class Counter {
  #count = 0;
  increment() { this.#count++; }
  get value() { return this.#count; }
}
```

### Expressions

- All arithmetic operators (`+`, `-`, `*`, `/`, `%`, `**`).
- All comparison operators (`===`, `!==`, `<`, `>`, `<=`, `>=`).
- Logical operators (`&&`, `||`, `!`).
- Nullish coalescing (`??`).
- Nullish coalescing assignment (`??=`).
- Logical assignment (`&&=`, `||=`).
- Optional chaining (`?.` for property access, computed access, and method calls).
- Bitwise operators (`&`, `|`, `^`, `<<`, `>>`, `>>>`).
- Ternary operator (`? :`).
- Numeric literals: decimal, hex (`0x`), binary (`0b`), octal (`0o`), scientific notation, and numeric separators (`1_000_000`, `0xFF_FF`, `0b1010_0001`, `0o77_77`, `1.5e1_0`).
- Template literals with interpolation and tagged templates (``tag`...```,`String.raw`). Tagged templates support the TC39 Template Literal Revision (ES2018): malformed escape sequences (e.g.,`\u{`,`\xG1`) set the cooked segment to`undefined` while preserving the raw source text; untagged templates still throw `SyntaxError` for invalid escapes.
- Destructuring (array and object patterns).
- Spread operator (`...`).
- `typeof`, `instanceof`, `in`, `delete`.
- Computed property names.
- Shorthand property notation.

### Statements

- `if`/`else if`/`else`
- `switch`/`case`/`default`/`break`
- `try`/`catch`/`finally`
- `throw`
- `return`
- Block statements
- `for...of` and `for await...of` (see [Supported Iteration](#supported-iteration))
- `import`/`export` (ES module system — named exports only, no default exports)

### Explicit Resource Management

`using` and `await using` declarations (ES2026 [Explicit Resource Management](https://tc39.es/ecma262/#sec-using-declaration)) are supported.

- `using` — Synchronous disposal. When the enclosing block exits, `[Symbol.dispose]()` is called on the bound value.
- `await using` — Asynchronous disposal. When the enclosing block exits, `[Symbol.asyncDispose]()` is awaited.
- `DisposableStack` and `AsyncDisposableStack` are available as built-in constructors.
- `Symbol.dispose` and `Symbol.asyncDispose` are well-known symbols.
- `SuppressedError` is thrown when both the block body and a disposer throw.

```javascript
{
  using file = openFile("data.txt");
  // file is automatically disposed when this block exits
}

const fn = async () => {
  await using conn = openConnection();
  // conn is asynchronously disposed when this block exits
};
```

### Modules

ES module syntax with named exports. Supported script file extensions: `.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`. Structured-data imports are also supported for `.json`, `.json5`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, and `.tsv`, and text-asset imports are supported for `.txt` and `.md`. Module paths are resolved relative to the importing file. File extensions can be omitted — the resolver tries script, structured-data, and text-asset extensions in order. Directory imports resolve to `index` files. The CLI tools support WHATWG-style import maps through `--import-map=<file.json>`, `--alias key=value`, and implicit `goccia.json` discovery.

```javascript
// Named imports (with or without extension)
import { add, multiply } from "./math.js";
import { add, multiply } from "./math";      // resolves to ./math.js, .jsx, .ts, .tsx, or .mjs
import { "foo-bar" as fooBar } from "./config.json";

// Namespace imports
import * as math from "./math.js";
import * as configJson from "./config.json";
import * as configJson5 from "./config.json5";
import * as configToml from "./config.toml";
import * as configYaml from "./config.yaml";

// Named exports
export { myFunction, myValue };
export { localValue as "0" };

// Inline exports
export const PI = 3.14159;
export let count = 0;

// Re-exports
export { add, multiply } from "./math.js";
export { greet as sayHello } from "./utils.js";
export { "foo-bar" as configValue } from "./config.json";

// JSON imports — top-level keys become named exports
import { name, version } from "./package.json";
import { host as dbHost } from "./config.json";
import { "foo-bar" as featureFlag } from "./feature-flags.json";

// JSON5 imports — top-level keys become named exports
import { name, version } from "./package.json5";
import { host as dbHost } from "./config.json5";

// TOML imports — top-level keys become named exports
import { name, version } from "./package.toml";
import { host as dbHost } from "./config.toml";

// JSONL imports — each non-empty line becomes a string-indexed named export
import { "0" as firstRecord, "1" as secondRecord } from "./events.jsonl";

// YAML imports — top-level keys become named exports
import { name, version } from "./package.yaml";
import { host as dbHost } from "./config.yml";
import { "0" as firstDoc } from "./multi-doc-index.yaml";

// CSV / TSV imports — data rows become string-indexed named exports (headers on by default)
import { "0" as firstRow, "1" as secondRow } from "./data.csv";
import { "0" as firstRow } from "./data.tsv";

// Text asset imports — content plus frozen metadata
import { content, metadata } from "./notes.txt";
import { content as readme, metadata as readmeInfo } from "./README.md";

TOML module imports follow the same top-level-object export model as JSON modules. `.toml` files are parsed as TOML 1.1.0 and exposed as named exports from the root table. Arrays of tables stay arrays, nested tables stay objects, and TOML date/time values currently surface as validated strings rather than Temporal values.

JSON5 module imports follow the same top-level-object export model as JSON modules. `.json5` files are parsed with the permissive JSON5 surface, so comments, trailing commas, single-quoted strings, unquoted identifier keys, hexadecimal numbers, signed numbers, and `Infinity` / `NaN` all work during import while still exposing the parsed root object as named exports.

Single-document YAML module imports follow the same top-level-object export model as JSON modules. Multi-document YAML streams are also supported for `.yaml` and `.yml` imports: each document is exposed as a named export under its zero-based string index (`"0"`, `"1"`, ...). If you want an array of documents inside normal runtime code instead of module exports, use `YAML.parseDocuments(...)`. Block scalars, multiline plain and quoted scalar folding, multiline flow collections, YAML 1.2 numeric scalar resolution, YAML double-quoted escapes, self-referential alias graphs, stricter flow collection validation, empty implicit keys, anchored mapping keys, `%YAML` / `%TAG` directives, standard tags, tagged-value metadata (`.tagName`, `.value`), and explicit keys including omitted explicit values are supported.

JSONL (`.jsonl`), CSV (`.csv`), and TSV (`.tsv`) imports expose each record/row as a named export under its zero-based string index (`"0"`, `"1"`, ...). JSONL parses each non-empty line as strict JSON (blank lines are ignored; invalid JSON fails with a line-number error). CSV uses RFC 4180 semantics (comma-delimited, double-quote escaping); TSV uses IANA `text/tab-separated-values` semantics (tab-delimited, backslash escaping) — both default to headers mode. In runtime code, these formats expose `parse(...)` / `parseChunk(...)` APIs, and CSV/TSV also support reviver/replacer callbacks. See [Data Format Built-ins](built-ins-data-formats.md) for the full API.

Text asset imports (`.txt`, `.md`) expose two named exports: `content` (UTF-8 text with newlines canonicalized to LF) and `metadata` (a frozen object with `kind`, `path`, `fileName`, `extension`, and `byteLength`).

Non-scalar YAML keys are canonicalized into stable JSON-like strings. Keys that are not valid identifiers can be imported with string-literal names such as `import { "foo-bar" as fooBar } from "./config.yaml";`. Namespace imports (`import * as ns from "./module.js"`) are supported for script, structured-data, and text-asset modules and produce a frozen, null-prototype namespace object.

// Directory/index resolution
import { setup } from "./utils";  // resolves to ./utils/index.js (or .ts, .jsx, etc.)
```

**Not supported:** `export default`, `import x from` (default import), `import "module"` (side-effect import), `export * from` (wildcard re-export). The parser accepts these syntactically but treats them as no-ops, emitting a warning with a suggestion:

```text
Warning: Default imports are not supported in GocciaScript
  Suggestion: Use named imports instead: import { name } from 'module'
  --> script.js:1:1

Warning: Wildcard re-exports (export * from ...) are not supported in GocciaScript
  Suggestion: Use named re-exports instead: export { name } from 'module'
  --> script.js:2:1
```

`import.meta` (ES2026 §13.3.12) is supported and provides per-module metadata. The `import.meta` object has a null prototype and is identity-stable — the same object is returned on every access within the same module. Two host-defined properties are available:

- **`import.meta.url`** — a `file://` URL string for the current module's absolute path (e.g., `file:///Users/me/project/src/main.js`).
- **`import.meta.resolve(specifier)`** — a synchronous function that resolves a module specifier relative to the current module's directory and returns the resolved `file://` URL string. Throws `TypeError` if called without arguments.

```javascript
// Access the current module's URL
const currentUrl = import.meta.url;

// Resolve a sibling module's path
const helperUrl = import.meta.resolve("./helpers/math.js");
```

Dynamic `import()` (ES2026 §13.3.10) is supported. It accepts an arbitrary expression as the module specifier, loads the module synchronously, and returns a Promise that resolves with the module namespace object. On failure, the returned Promise is rejected with the error. Dynamic imports work anywhere expressions are valid — including inside functions, conditionals, and async callbacks.

```javascript
// Dynamic import with a computed specifier
const moduleName = "./helpers/math.js";
const mod = await import(moduleName);
console.log(mod.add(2, 3)); // 5

// Dynamic import with .then()
import("./config.json").then((config) => {
  console.log(config.name);
});
```

### Data Structures

- Objects (literal syntax, computed properties, shorthand methods).
- Arrays (literals, sparse arrays, prototype methods).
- Strings (template literals, prototype methods).

### String Escape Sequences

Standard escape sequences in string literals and template literals:

- `\n`, `\r`, `\t`, `\\`, `\'`, `\"`, `\0`
- `\xHH` — Hex byte escape (e.g., `"\x41"` → `"A"`)
- `\uXXXX` — 4-digit Unicode escape (e.g., `"\u0041"` → `"A"`)
- `\u{XXXXX}` — Variable-length Unicode escape (e.g., `"\u{1F4A9}"` → `"💩"`)

### Supported Iteration

GocciaScript supports iteration via `for...of` and `for await...of`, which work with the iterator protocol.

#### `for...of`

Iterates over sync iterables using `[Symbol.iterator]`:

- **Arrays** — Yields each element.
- **Strings** — Yields each character.
- **Sets** — Yields each value.
- **Maps** — Yields `[key, value]` pairs.
- **Custom iterables** — Any object with a `[Symbol.iterator]()` method that returns an iterator.

```javascript
for (const x of [1, 2, 3]) { console.log(x); }
for (const c of "hello") { console.log(c); }
for (const [k, v] of new Map([["a", 1]])) { console.log(k, v); }
```

**Destructuring in bindings:** The loop variable can use destructuring patterns:

```javascript
for (const [a, b] of [[1, 2], [3, 4]]) { console.log(a + b); }
```

**`break`:** Exits the loop early. Supported inside `for...of`.

#### `for await...of`

Iterates over async iterables using `[Symbol.asyncIterator]`. Works inside `async` functions and at the top level (with top-level `await`):

```javascript
// Inside an async function
const fn = async () => {
  for await (const x of asyncIterable) {
    console.log(x);
  }
};

// At the top level (ES2022+)
for await (const x of asyncIterable) {
  console.log(x);
}
```

Each value is awaited before the loop body runs. Works with sync iterables too — Promises yielded by a sync iterator are awaited.

### JSX

**Enabled by default** via `DefaultPreprocessors`. JSX is handled by a source-to-source pre-pass transformer that converts JSX syntax into `createElement` function calls before the main compilation pipeline. This keeps the core lexer/parser/evaluator untouched. Embedders can disable JSX via `Engine.Preprocessors := []`.

Users must provide their own `createElement` (and `Fragment` for `<>...</>`) in scope:

```javascript
const createElement = (tag, props, ...children) => ({ tag, props, children });
const Fragment = Symbol("Fragment");

const el = <div className="active">Hello {name}</div>;
// Transformed to: createElement("div", { className: "active" }, "Hello ", name)
```

**Supported syntax:** Elements, self-closing tags (`<br />`), fragments (`<>...</>`), string/expression/boolean attributes, spread attributes (`{...props}`), shorthand props (`<div {value} />` → `value={value}`), expression children (`{expr}`), nested JSX in expressions, dotted component names (`<Foo.Bar />`).

Lowercase tags produce string tag names (`"div"`, `"span"`); uppercase tags are passed as identifier references (component functions/classes).

**Custom factory:** The factory and fragment function names can be overridden per-file using pragma comments (`@jsxFactory`, `@jsxFragment`) at the top of the file, before any code.

The transformer generates an internal source map for accurate error line/column reporting. JSX is enabled by default via `DefaultPreprocessors`; embedders can disable it with `Engine.Preprocessors := Engine.Preprocessors - [ppJSX]`.

### Regular Expressions

**Supported.** GocciaScript implements:

- `RegExp(pattern, flags?)` and `new RegExp(pattern, flags?)`
- Regex literals: `/pattern/flags`
- Flags: `d`, `g`, `i`, `m`, `s`, `u`, `v`, `y`
- RegExp instance properties: `source`, canonicalized `flags`, `lastIndex`, `global`, `ignoreCase`, `multiline`, `dotAll`, `unicode`, `sticky`, `unicodeSets`, `hasIndices`
- `RegExp.escape()` (ES2026)
- Duplicate named capture groups: the same group name may appear in different alternatives of a disjunction (`|`), e.g., `/(?<year>\d{4})-\d{2}|\d{2}-(?<year>\d{4})/`. The `groups` property returns the value from whichever alternative participated, and `\k<name>` backreferences resolve to the correct group within each alternative.
- `RegExp.prototype.exec()`, `test()`, `toString()`, and the `Symbol.match`, `Symbol.matchAll`, `Symbol.replace`, `Symbol.search`, and `Symbol.split` hooks
- String integrations for `replace`, `replaceAll`, `split`, `match`, `matchAll`, and `search`, including custom protocol objects with the corresponding well-known symbol methods

`RegExp(existingRegExp)` without `new` and without an explicit flags argument returns the original regex object. `new RegExp(existingRegExp)` still creates a clone.

Regex literals are lexed context-sensitively so `/` still works as division in expression contexts.

Current gaps from full ECMAScript RegExp semantics:

- The `u` flag enables Unicode-aware matching with property escapes (`\p{Letter}`) and code point escapes (`\u{1F600}`), but does not yet cover the full ECMAScript Unicode specification.
- The `v` flag (Unicode sets) is accepted and exposed but full set notation is not yet implemented beyond basic `u` flag behavior.
- The `d` flag (indices) is accepted and exposed but match indices are not yet populated.

## TC39 Proposal Details

GocciaScript implements several active TC39 proposals alongside the core ECMAScript subset.

### Decorators (Stage 3)

TC39 Stage 3 decorators ([proposal-decorators](https://github.com/tc39/proposal-decorators)) and decorator metadata ([proposal-decorator-metadata](https://github.com/tc39/proposal-decorator-metadata)) are fully supported.

**Supported decorator targets:**

- Class declarations (`@decorator class C {}`)
- Instance and static methods (`@decorator method() {}`)
- Getters and setters (`@decorator get x() {}`, `@decorator set x(v) {}`)
- Class fields (`@decorator field = value;`)
- Auto-accessors (`@decorator accessor prop = value;`)
- Private elements (`@decorator #method() {}`)

**Decorator context object properties:** `kind`, `name`, `access` (with `get`/`set`), `static`, `private`, `addInitializer`, `metadata`.

**Decorator expressions:** Identifiers (`@log`), member access (`@decorators.log`), call expressions (`@factory(arg)`), and parenthesized expressions (`@(expr)`).

**Auto-accessors:** The `accessor` keyword creates a property backed by a private storage slot with generated getter/setter methods:

```javascript
class C {
  accessor name = "default";
}
// Equivalent to a private #name field with get name() / set name(v)
```

**`addInitializer`:** Decorators can register initialization callbacks via `context.addInitializer()`. Timing depends on decorator kind:

- Class decorators: after the class is fully defined and static fields are assigned.
- Static method/getter/setter: during class definition, after static methods are placed.
- Non-static method/getter/setter: during class construction, before field initialization.
- Field/accessor: during construction, immediately after the decorated element is initialized.

**`Symbol.metadata`:** Each decorated class receives a `[Symbol.metadata]` property containing a null-prototype object. Metadata objects inherit from the parent class's metadata via prototype chain.

```javascript
const meta = (value, context) => {
  context.metadata.decorated = true;
};

@meta
class C {}

C[Symbol.metadata].decorated; // true
```

**Not supported:** Parameter decorators.

### Decorator Metadata (Stage 3)

Decorator metadata works as described in [Decorators](#decorators-stage-3) — each decorated class receives a `Symbol.metadata` property with prototype-chain inheritance. See [proposal-decorator-metadata](https://github.com/tc39/proposal-decorator-metadata).

### Types as Comments (Stage 1)

GocciaScript supports the [TC39 Types as Comments](https://tc39.es/proposal-type-annotations/) proposal. TypeScript-style type annotations are parsed but have **no runtime effect** — they are treated as comments by the evaluator. Raw type strings are preserved on AST nodes for potential future optimization.

#### Supported Syntax

```javascript
// Variable type annotations
let x: number = 42;
const name: string = "hello";
let value: string | number = "test";

// Parameter type annotations (simple, optional, rest, destructuring)
const add = (a: number, b: number): number => a + b;
const greet = (name?: string) => name === undefined ? "hi" : "hi " + name;
const sum = (...nums: number[]) => nums.reduce((a, b) => a + b, 0);
const first = ({ name, age }: { name: string, age: number }) => name;

// Return type annotations
const double = (x: number): number => x * 2;

// Type and interface declarations (skipped entirely)
type Point = { x: number, y: number };
interface Animal { name: string; speak(): string; }

// import type / export type (skipped entirely)
import type { Foo } from './types.js';
export type { Bar };

// Mixed type/value named bindings keep runtime imports/exports for value bindings
import { parseSourceFile, type SourceFile } from "./parser.js";
export { value, type ValueShape };

// export interface declarations are skipped entirely
export interface Serializable {
  toJSON(): string;
}

// as Type and as const assertions
const x = 42 as number;
const colors = ["red", "green"] as const;

// Class annotations: field types, generics, implements, access modifiers
class Box<T> implements Container {
  public value: T;
  private label?: string;
  readonly id: number = 1;
  constructor(value: T) { this.value = value; }
  get(): T { return this.value; }
}

// Catch parameter type annotation
try { throw new Error("oops"); } catch (e: Error) { }
```

#### Not Supported

- Namespaces (`namespace Foo { ... }`).
- Parameter properties in constructors (`constructor(public x: number)`).
- Angle-bracket type assertions (`<string>value`) — use `value as string` instead.

### Enum Declarations (Stage 0)

GocciaScript supports the [TC39 proposal-enum](https://github.com/tc39/proposal-enum) `enum` declaration. Enums create frozen, null-prototype objects with typed member values.

```javascript
enum Direction {
  Up = 0,
  Down = 1,
  Left = 2,
  Right = 3
}

Direction.Up;           // 0
typeof Direction;       // "object"
[...Direction];         // [["Up", 0], ["Down", 1], ["Left", 2], ["Right", 3]]

// Self-references: members can reference prior members and the enum itself
enum Flags {
  Read = 1,
  Write = 2,
  ReadWrite = Read | Flags.Write
}

// Allowed value types: Number, String, Symbol
enum Color { Red = "red", Green = "green" }
enum Tokens { Alpha = Symbol("alpha") }
```

**Semantics:**

- All members require explicit initializers (no auto-initialization).
- Member values must be Number, String, or Symbol — other types throw `TypeError`.
- Enum objects have a `null` prototype and are non-extensible.
- Members are non-writable and non-configurable.
- `Symbol.iterator` yields `[key, value]` entry pairs in declaration order.
- `Symbol.toStringTag` is set to the enum name.
- `export enum` is supported for module exports.

**Not supported (future directions in proposal):**

- Auto-initializers (`enum E { A, B, C }`).
- Algebraic Data Type (ADT) enums.
- Enum decorators.
- `enum` expressions.

### Temporal (ES2027)

Modern date/time API. See [Built-in Objects](built-ins.md) for the full Temporal API reference. See [Temporal specification](https://tc39.es/ecma262/#sec-temporal-objects).

### Math.clamp (Stage 2)

Clamp a value to a range: `Math.clamp(value, min, max)`. See [proposal-math-clamp](https://github.com/tc39/proposal-math-clamp).

### Math.sumPrecise (ES2026)

Precise summation of iterables using a compensated algorithm: `Math.sumPrecise(iterable)`. See [ES2026 §21.3.2.33](https://tc39.es/ecma262/#sec-math.sumprecise).

### Map.prototype.getOrInsert (ES2026)

Get existing value or insert a default/computed value: `map.getOrInsert(key, default)`, `map.getOrInsertComputed(key, callbackFn)`. See [ES2026 §24.1.3](https://tc39.es/ecma262/#sec-map.prototype.getorinsert).

### Error.isError (ES2026)

Reliable brand check for error objects: `Error.isError(value)`. See [ES2026 §20.5.3.2](https://tc39.es/ecma262/#sec-error.iserror).

## Excluded Features

### `var` Declarations

**Opt-in.** Excluded by default; use `let` or `const` instead. Available as a compatibility mode via `--compat-var`. `var` has function-scoping and hoisting behavior that leads to subtle bugs:

```javascript
// In JavaScript, this prints "undefined" then "5"
// because var is hoisted and function-scoped
console.log(x); // undefined (hoisted)
var x = 5;
console.log(x); // 5
```

With `let`/`const`, accessing before declaration is a `ReferenceError` (Temporal Dead Zone), which catches bugs early.

When enabled (CLI: `--compat-var`, engine API: `Engine.VarEnabled := True`, config: `{"compat-var": true}`), `var` declarations follow ES2026 §14.3.2 semantics: function-scoped (escapes blocks), hoisted to function top as `undefined`, redeclaration allowed, no TDZ, with destructuring and for-of support. Var bindings are stored in a separate binding map (`FVarBindings`) on function/module/global scopes, distinct from lexical bindings. See [interpreter.md § Scope Chain Design](interpreter.md#scope-chain-design).

### `function` Keyword

**Opt-in.** Excluded by default; use arrow functions or shorthand methods instead. Available as a compatibility mode via `--compat-function`.

When disabled (default), the parser accepts `function` declarations and expressions but treats them as no-ops (the function body is not executed and the binding is not created), and emits a warning:

```text
Warning: 'function' declarations are not supported in GocciaScript
  Suggestion: Use arrow functions instead: const name = (...) => { ... }
  --> script.js:1:1
```

Function expressions in assignment position evaluate to `undefined`. Generator function declarations (`function*`) are also skipped.

GocciaScript provides two function definition styles that cover all use cases without the `function` keyword's pitfalls (`this` binding confusion, hoisting surprises, implicit `arguments`):

- **Arrow functions** (`(x) => x + 1`) — Lexical `this`, no hoisting, no `arguments`. Use for standalone functions, callbacks, and closures.
- **Shorthand methods** (`method() {}`) — Call-site `this`, like ECMAScript's regular functions. Use in object literals and class definitions where `this` binding is needed.

When enabled (CLI: `--compat-function`, engine API: `Engine.FunctionEnabled := True`, config: `{"compat-function": true}`), `function` declarations and expressions are fully supported. The `arguments` object remains excluded regardless of this flag.

- **Function declarations** (`function name(params) { body }`) are desugared to var-scoped bindings backed by `TGocciaMethodExpression`, which produces call-site `this` binding (not lexical). Declarations are hoisted: both the name and the function value are available before the declaration is reached, matching ES2026 §15.2.6 semantics. Uses the same var binding infrastructure (`DefineVariableBinding`) as `--compat-var`.
- **Function expressions** (`const f = function(params) { body }`) produce the same `TGocciaMethodExpression` node. Named function expressions consume the name but do not create a binding inside the function body.
- **Async functions** (`async function name(params) { body }`) are supported in both declaration and expression forms.
- **Generator functions** (`function*`) remain unsupported; they produce a warning and are skipped.

### Loose Equality (`==` and `!=`)

**Excluded.** Use `===` and `!==` instead. The parser accepts `==` and `!=` but treats them as no-ops (the expression evaluates to `undefined`), and emits a warning:

```text
Warning: '==' (loose equality) is not supported in GocciaScript
  Suggestion: Use '===' (strict equality) instead
  --> script.js:1:10
```

Both operands are parsed but not evaluated at runtime (no side effects). Because the entire expression becomes `undefined`, which is falsy, `==`/`!=` in conditions (e.g., `if (a == b)`) will never enter the truthy branch.

Loose equality's type coercion rules are notoriously confusing:

```javascript
// All true in JavaScript with ==
"" == false   // true
0 == ""       // true
null == undefined // true
```

Strict equality requires matching types, eliminating this entire class of bugs.

### `eval()`

**Excluded.** No alternative provided.

`eval` is a security risk — it executes arbitrary strings as code. In an embedded scripting environment, this is especially dangerous.

### `arguments` Object

**Excluded.** Use rest parameters (`...args`) instead.

The `arguments` object is an array-like (but not array) object with confusing behavior. Rest parameters provide a real array with explicit syntax.

### Automatic Semicolon Insertion

**Opt-in.** Semicolons are required by default.

ASI rules are complex and can lead to unexpected behavior:

```javascript
// In JavaScript, this returns undefined due to ASI
return
  { value: 42 }
```

GocciaScript requires explicit semicolons by default, preventing this class of bugs. However, ASI can be enabled as an opt-in feature for greater ECMAScript compatibility:

```bash
# Enable ASI via CLI
./build/GocciaScriptLoader example.js --asi
./build/GocciaTestRunner tests/ --asi
./build/GocciaREPL --asi
```

```pascal
// Enable ASI via the engine API
Engine := TGocciaEngine.Create(FileName, Source, []);
Engine.ASIEnabled := True;
```

When enabled, GocciaScript follows the ECMAScript ASI rules (ES2026 §12.10):

- A semicolon is inserted when a newline separates the current and next token
- A semicolon is inserted before `}` or at EOF
- Restricted productions (`return`, `throw`, `break`) follow the `[no LineTerminator here]` rules

### Traditional Loops (`for`, `while`, `do...while`)

**Excluded.** Use `for...of`, `for await...of`, or array methods instead. The parser accepts traditional loop syntax but treats it as a no-op (the loop body is not executed), and emits a warning:

```text
Warning: 'for' loops are not supported in GocciaScript
  Suggestion: Use array methods like .forEach(), .map(), .filter(), or .reduce() instead
  --> script.js:1:1
```

The parser uses balanced-parenthesis tracking (`SkipBalancedParens`) to correctly skip the loop condition even when it contains nested parentheses (e.g., `for (let i = Math.max(0, 1); i < fn(x); i++)`), then `SkipStatementOrBlock` to skip the loop body. This ensures subsequent code executes correctly.

Traditional loops encourage imperative, mutation-heavy code. GocciaScript supports `for...of` and `for await...of` for iteration (see [Supported Iteration](#supported-iteration) above), and favors functional iteration through array methods:

```javascript
// Instead of: for (let i = 0; i < items.length; i++) { ... }
for (const item of items) { ... }
items.forEach((item) => { ... });
items.map((item) => transform(item));
items.filter((item) => item.isValid);
items.reduce((acc, item) => acc + item, 0);
```

`break` and `continue` are available inside `switch` statements and inside `for...of`/`for await...of` loops.

### `with` Statement

**Excluded.** No alternative needed. The parser accepts `with` syntax but treats it as a no-op (the body is not executed), and emits a warning:

```text
Warning: The 'with' statement is not supported in GocciaScript
  --> script.js:1:1
```

Like loops, the parser uses `SkipBalancedParens` to safely skip the `with (...)` expression, including nested parentheses.

`with` creates ambiguous scope and is deprecated even in JavaScript's strict mode. Note that `with` is a reserved keyword in GocciaScript (it cannot be used as a variable name), but it can be used as a property name (e.g., `obj.with`).

### Labeled Statements

**Excluded.** No alternative needed. The parser accepts labeled statements but strips the label and emits a warning:

```text
Warning: Labeled statements are not supported in GocciaScript
  --> script.js:1:1
```

The labeled statement itself (the statement after the `:`) is still parsed and executed normally. For example, `myLabel: x = 2;` strips the label and executes `x = 2;`. If the labeled statement is itself unsupported (e.g., `outer: for (...)`), both a label warning and a loop warning are emitted.

Labels exist primarily for `break`/`continue` targets in nested loops. Since GocciaScript excludes traditional loops, labels serve no purpose.

### Generators and Iterators

**Partially implemented.** Iterator protocol and Iterator Helpers are fully implemented. Generator functions (`function*`) are not supported.

### Deferred Built-ins

The following standard ECMAScript built-ins are **not yet implemented** and may be added in future versions:

- **WeakMap / WeakSet / WeakRef / FinalizationRegistry** — Weak reference collections and finalizers. These require tight GC integration. Deferred until demand warrants the complexity.

## Intentional Divergences from ECMAScript

These are deliberate differences from standard ECMAScript behavior, not missing features.

### No Global `parseInt`, `parseFloat`, `isNaN`, `isFinite`

**Intentional.** Use `Number.parseInt`, `Number.parseFloat`, `Number.isNaN`, `Number.isFinite` instead.

In ECMAScript, these exist as both global functions and as `Number` static methods. `parseInt` and `parseFloat` are identical to their `Number.*` counterparts. However, the global `isNaN` and `isFinite` have legacy coercion behavior (e.g., `isNaN("abc")` returns `true` because it coerces the string to a number first), while `Number.isNaN` and `Number.isFinite` are stricter — they return `false` for any non-number argument.

GocciaScript only provides the `Number.*` versions, keeping these functions on the object they belong to rather than polluting the global scope. This avoids the confusing dual behavior of `isNaN`/`isFinite` and encourages explicit type handling.

If needed, they can be polyfilled:

```javascript
const parseInt = Number.parseInt;
const parseFloat = Number.parseFloat;
const isNaN = Number.isNaN;
const isFinite = Number.isFinite;
```

## Strictness Guarantees

GocciaScript operates in an implicit strict mode (see [Errors](errors.md) for the full error type reference):

- All variables must be declared before use.
- Duplicate parameter names are forbidden.
- Temporal Dead Zone is enforced for `let`/`const`.
- `const` reassignment throws `TypeError`.
- Accessing undeclared variables throws `ReferenceError`.
- `this` is `undefined` in standalone function calls (no implicit global `this`).
- Symbol values cannot be implicitly converted to strings or numbers — throws `TypeError`.

### `this` Binding (Strict Mode Semantics)

GocciaScript follows ECMAScript strict mode `this` semantics:

| Context | `this` value |
|---------|-------------|
| Module level | `undefined` |
| Arrow function | Inherited from lexical (enclosing) scope |
| Shorthand method (`method() {}`) | Call-site object (the receiver) |
| Class method | Call-site object (the instance) |
| Getter/setter | Call-site object |
| Standalone function call | `undefined` |
| `fn.call(thisArg)` / `fn.apply(thisArg)` | Explicit `thisArg` |
| `fn.bind(thisArg)` | Bound `thisArg` |

Arrow functions **never** receive their own `this` — they always inherit from their defining scope:

```javascript
const obj = {
  value: 42,
  arrow: () => typeof this,     // undefined — arrow inherits module-level this
  method() { return this.value; } // 42 — shorthand method uses call-site this
};

const outer = {
  value: 10,
  nested() {
    const fn = () => this.value;  // 10 — arrow inherits method's this
    return fn();
  }
};
```
