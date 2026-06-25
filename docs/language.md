<!-- doc-length-limit: 1000 -->
# Language

*GocciaScript's ECMAScript support: recommended defaults, compatibility flags, TC39 proposals, and conformance shims.*

## Executive Summary

- **Recommended defaults** — Modern, explicit ECMAScript: `let`/`const`, arrow functions, classes with private fields, `for...of`, async/await, ES modules
- **Conformance objective** — Full ECMAScript compatibility is a release-track objective, measured by generated test262 reports
- **TC39 proposals** — Decorators, decorator metadata, pattern matching, types as comments, enums, `Math.clamp`
- **Excluded by design** — Runtime `eval`, wildcard re-exports
- **Graceful handling** — Parser-recognized excluded or disabled syntax (`==`/`!=` when `--compat-loose-equality` is off, labels when `--compat-label` is off, `while`/`do...while` when `--compat-while-loops` is off, `with` when `--compat-non-strict-mode` is off, traditional `for(;;)` when `--compat-traditional-for-loop` is off, `for...in` when `--compat-for-in-loop` is off) parses successfully but executes as a no-op with a warning and suggestion
- **Compatibility flags** — `--compat-*` flags primarily exist for ECMAScript conformance and legacy semantic requirements. Userland code should usually prefer default forms instead of enabling ASI (`--compat-asi`), `var` (`--compat-var`), `function` (`--compat-function`), implicit `arguments` (`--compat-arguments-object`), non-strict Script compatibility (`--compat-non-strict-mode`), loose equality (`--compat-loose-equality`), labels (`--compat-label`), traditional loops (`--compat-traditional-for-loop`), `for...in` (`--compat-for-in-loop`), or `while`/`do...while` (`--compat-while-loops`) preemptively.
- **Conformance-only host hooks** — `GocciaScriptLoaderBare --test262-host` exposes private test262 hooks (`eval`, `evalScript`, `createRealm`) without making them part of the public runtime surface
- **Default preprocessors** — JSX (enabled by default via `DefaultPreprocessors`)

GocciaScript implements ECMAScript with curated recommended defaults. This document details what's supported by default, what lives behind compatibility flags, what is shimmed for legacy conformance, and the rationale for each decision. For quick-reference tables of every feature and TC39 proposal, see [Language Tables](language-tables.md).

## Guiding Principle

> **A drop of JavaScript** — GocciaScript recommends the parts of JavaScript that lead to clear, predictable, and secure code by default. Legacy features that are error-prone, redundant, or security risks are isolated behind compatibility flags or shims where conformance requires them.

## Compatibility Flags

Compatibility flags are parser and runtime policy switches for ECMAScript conformance work, test262 execution, and hosts that deliberately need legacy JavaScript behavior. They are not the recommended starting point for generated code or ordinary userland scripts. AI coding agents should usually emit the default forms first, then enable a flag only when the surrounding project, test, or host explicitly needs the legacy semantics.

| Flag | Enables | Recommended userland default |
|------|---------|--------------------------|
| `--compat-asi` | Automatic semicolon insertion | Use explicit semicolons |
| `--compat-var` | `var` declarations and ES script-global `var` binding behavior | Use `let` / `const` |
| `--compat-function` | `function` declarations and expressions | Use arrows, methods, accessors, and class methods |
| `--compat-non-strict-mode` | Script-source `arguments`, `with`, non-strict failed assignments, legacy `delete` return values, and sloppy ordinary-function `this` coercion | Use rest parameters, explicit property access, strict assignment/delete behavior, and explicit receivers |
| `--compat-loose-equality` | `==` and `!=` | Use `===` and `!==` |
| `--compat-label` | Labeled statements and labeled `break` / `continue` | Use helper functions or explicit state |
| `--compat-traditional-for-loop` | `for(init; test; update)` | Use `for...of`, iterators, or array methods |
| `--compat-for-in-loop` | `for...in` property enumeration | Use `Object.keys()` / `Object.entries()` with `for...of` |
| `--compat-while-loops` | `while` and `do...while` | Use `for...of`, iterators, or array methods when possible |

Runtime type enforcement (`--strict-types`) is separate from ECMAScript compatibility. It enforces GocciaScript's parsed type annotations at runtime and is opt-in for hosts that want that extra contract.

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
- Type annotations on parameters and return types (parsed and ignored by default; enforced when `--strict-types` is set — see [Types as Comments](#types-as-comments-stage-1) below).
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

**`await` expressions:** Valid inside `async` functions and at the top level (ES2022+ top-level `await`). Suspends execution until the Promise settles; returns the fulfilled value or throws the rejection reason. Top-level `await` works identically in interpreter mode and bytecode mode.

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
- Comma operator (`,`) for left-to-right sequence expressions.
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
- `for...in` via `--compat-for-in-loop` (see [`for...in`](#forin-loop))
- `while` and `do...while` via `--compat-while-loops` (see [`while` and `do...while`](#while-and-dowhile))
- `import`/`export` (ES module system)

### Explicit Resource Management

`using` and `await using` declarations (ES2026 [Explicit Resource Management](https://tc39.es/ecma262/#sec-using-declaration)) are supported in interpreter and bytecode mode.

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

ES module syntax with default, named, and namespace imports/exports. Project code convention prefers named exports for internal modules, but default imports and exports are language-supported. Supported source file extensions: `.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`; `.mjs` entry files default to module source. Structured-data imports are also supported for `.json`, `.json5`, `.jsonl`, `.toml`, `.yaml`, `.yml`, `.csv`, and `.tsv`, and text-asset imports are supported for `.txt` and `.md`. Module paths are resolved relative to the importing file. File extensions can be omitted — the resolver tries source, structured-data, and text-asset extensions in order. Directory imports resolve to `index` files. The CLI tools support WHATWG-style import maps through `--import-map=<file.json>`, `--alias key=value`, and implicit `goccia.json` discovery.

```javascript
// Named imports (with or without extension)
import { add, multiply } from "./math.js";
import { add, multiply } from "./math";      // resolves to ./math.js, .jsx, .ts, .tsx, or .mjs
import { "foo-bar" as fooBar } from "./config.json";
// Default imports
import add from "./math.js";
import addAgain, { multiply } from "./math.js";
import addModule, * as mathNamespace from "./math.js";
// Namespace imports
import * as math from "./math.js";
import * as configJson from "./config.json";
import * as configJson5 from "./config.json5";
import * as configToml from "./config.toml";
import * as configYaml from "./config.yaml";
// Named exports
export { myFunction, myValue };
export { localValue as "0" };
// Default exports
export default 42;
export default (a, b) => a + b;
export default class {}
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
// Bytes imports — raw file bytes as an immutable-backed Uint8Array (default-only)
import photo from "./photo.png" with { type: "bytes" };
```

Named exports are a project convention for new internal modules because they keep call sites explicit and make refactors easier to review. They are not a language restriction; default exports and imports are supported when a module intentionally exposes a primary value or interoperates with code that uses default exports.

TOML module imports follow the same top-level-object export model as JSON modules. `.toml` files are parsed as TOML 1.1.0 and exposed as named exports from the root table. Arrays of tables stay arrays, nested tables stay objects, and TOML date/time values currently surface as validated strings rather than Temporal values.

JSON5 module imports follow the same top-level-object export model as JSON modules. `.json5` files are parsed with the permissive JSON5 surface, so comments, trailing commas, single-quoted strings, unquoted identifier keys, hexadecimal numbers, signed numbers, and `Infinity` / `NaN` all work during import while still exposing the parsed root object as named exports.

Single-document YAML module imports follow the same top-level-object export model as JSON modules. Multi-document YAML streams are also supported for `.yaml` and `.yml` imports: each document is exposed as a named export under its zero-based string index (`"0"`, `"1"`, ...). If you want an array of documents inside normal runtime code instead of module exports, use `YAML.parseDocuments(...)`. Block scalars, multiline plain and quoted scalar folding, multiline flow collections, YAML 1.2 numeric scalar resolution, YAML double-quoted escapes, self-referential alias graphs, stricter flow collection validation, empty implicit keys, anchored mapping keys, `%YAML` / `%TAG` directives, standard tags, tagged-value metadata (`.tagName`, `.value`), and explicit keys including omitted explicit values are supported.

JSONL (`.jsonl`), CSV (`.csv`), and TSV (`.tsv`) imports expose each record/row as a named export under its zero-based string index (`"0"`, `"1"`, ...). JSONL parses each non-empty line as strict JSON (blank lines are ignored; invalid JSON fails with a line-number error). CSV uses RFC 4180 semantics (comma-delimited, double-quote escaping); TSV uses IANA `text/tab-separated-values` semantics (tab-delimited, backslash escaping) — both default to headers mode. In runtime code, these formats expose `parse(...)` / `parseChunk(...)` APIs, and CSV/TSV also support reviver/replacer callbacks. See [Data Format Built-ins](built-ins-data-formats.md) for the full API.

Text asset imports (`.txt`, `.md`) expose two named exports: `content` (UTF-8 text with newlines canonicalized to LF) and `metadata` (a frozen object with `kind`, `path`, `fileName`, `extension`, and `byteLength`).

Bytes imports (the TC39 [Import Bytes](https://github.com/tc39/proposal-import-bytes) proposal, requested with `with { type: "bytes" }`) load the resolved file as raw bytes and expose a single default export that is a `Uint8Array` backed by an immutable `ArrayBuffer`. Unlike text imports, bytes are preserved exactly — no UTF-8 decoding and no newline normalization, so NUL bytes, non-UTF-8 sequences, and original CRLF/LF newlines are kept verbatim. The attribute selects the loader regardless of the file extension or MIME, matching the proposal's local-host guidance. Because the synthetic module declares only a default export, named imports are rejected; source-phase imports (`import.source`) of bytes modules are rejected as well, while static, dynamic (`import(...)`), and deferred (`import.defer`) forms are all supported and share the module cache by specifier and `type: "bytes"`.

Non-scalar YAML keys are canonicalized into stable JSON-like strings. Keys that are not valid identifiers can be imported with string-literal names such as `import { "foo-bar" as fooBar } from "./config.yaml";`. Namespace imports (`import * as ns from "./module.js"`) are supported for script, structured-data, and text-asset modules and produce a frozen, null-prototype namespace object.

Directory/index resolution:

```javascript
import { setup } from "./utils";  // resolves to ./utils/index.js (or .ts, .jsx, etc.)
```

**Not supported:** `import "module"` (side-effect import), `export * from` (wildcard re-export). The parser accepts these syntactically but treats them as no-ops, emitting a warning with a suggestion:

```text
Warning: Wildcard re-exports (export * from ...) are not supported in GocciaScript
  Suggestion: Use named re-exports instead: export { name } from 'module'
  --> script.js:1:1
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

`new.target` (ES2026 §13.3.12) is supported inside class constructors. It evaluates to the constructor that was directly invoked with `new`, or the `newTarget` argument supplied to `Reflect.construct`. Outside a constructor, `new.target` is `undefined`.

Dynamic `import()` (ES2026 §13.3.10) is supported. It accepts an arbitrary expression as the module specifier, loads the module synchronously, and returns a Promise that resolves with the module namespace object. On failure, the returned Promise is rejected with the error. Dynamic imports work anywhere expressions are valid — including inside functions, conditionals, and async callbacks.

The parser accepts import attributes syntax (`import(specifier, options)`) and trailing commas. The options expression is evaluated for side effects; enumerable import attribute keys other than `type` are rejected, and `type: "json"`, `type: "text"`, and `type: "bytes"` route dynamic and static imports through the corresponding JSON, text, or bytes module loaders (bytes modules expose a default-only immutable-backed `Uint8Array`). Proposal-phase `import.source(specifier)` follows TC39 Source Phase Imports syntax, but ordinary JavaScript module source objects are still part of the separate ESM Phase Imports proposal. By default, JavaScript source-phase requests resolve normally and then reject with `SyntaxError` because the host has no default source representation for source-text modules. With `--experimental-js-module-source` (or `"experimental-js-module-source": true` / `cfExperimentalJSModuleSource`), JavaScript requests produce cached `ModuleSource` objects without linking, instantiating, or evaluating the module body. Source-phase requests for JSON, text, and other non-JavaScript module kinds currently fail after normal resolution because those module kinds do not expose source objects. Proposal-phase `import.defer(specifier)` resolves with a namespace object that evaluates the module when its exports are observed, while async transitive dependencies with top-level `await` are still evaluated eagerly as part of module linking.

```javascript
// Dynamic import with a computed specifier
const moduleName = "./helpers/math.js";
const mod = await import(moduleName);
console.log(mod.add(2, 3)); // 5

// Dynamic import with .then()
import("./config.json").then((config) => {
  console.log(config.name);
});

// Import attributes route to the matching module kind.
const json = await import("./config.json", { with: { type: "json" } });

// Proposal-phase call forms.
const sourcePromise = import.source("./helpers/math.js"); // Promise rejects by default
// With --experimental-js-module-source:
// const sourcePromise = import.source("./helpers/math.js"); // Promise<ModuleSource>
const deferredPromise = import.defer("./helpers/math.js");
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

#### Generators

Generator methods are supported as normal method shorthand:

```javascript
const source = {
  *values() {
    yield 1;
    yield* [2, 3];
  },
};
```

Class generator methods use the same syntax:

```javascript
class Source {
  *values() {
    yield 1;
  }
}
```

The `function*` and `async function*` forms are compatibility syntax and require `--compat-function`, the same flag used for the `function` keyword. Async generator methods (`async *values() { ... }`) are supported by default and return async iterators whose `next()`, `return()`, and `throw()` methods return Promises. Async generators use GocciaScript's existing synchronous async/await drain model.

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
- Match indices through the `d` flag, exposed as UTF-16 code unit `[start, end]` pairs on `match.indices` and `match.indices.groups`
- `RegExp.escape()` (ES2026)
- Named capture group names are validated as identifier-name code points. Duplicate named capture groups may appear in different alternatives of a disjunction (`|`), e.g., `/(?<year>\d{4})-\d{2}|\d{2}-(?<year>\d{4})/`. The `groups` property returns the value from whichever alternative participated, and `\k<name>` backreferences resolve to the correct group within each alternative.
- `RegExp.prototype.exec()`, `test()`, `toString()`, and the `Symbol.match`, `Symbol.matchAll`, `Symbol.replace`, `Symbol.search`, and `Symbol.split` hooks
- String integrations for `replace`, `replaceAll`, `split`, `match`, `matchAll`, and `search`, including custom protocol objects with the corresponding well-known symbol methods

`RegExp(existingRegExp)` without `new` and without an explicit flags argument returns the original regex object. `new RegExp(existingRegExp)` still creates a clone.

Regex literals are lexed context-sensitively so `/` still works as division in expression contexts.

Current gaps from full ECMAScript RegExp semantics:

- The `u` flag enables Unicode-aware matching with property escapes (`\p{Letter}`) and code point escapes (`\u{1F600}`), but does not yet cover the full ECMAScript Unicode specification.
- The `v` flag (Unicode sets) is accepted and exposed but full set notation is not yet implemented beyond basic `u` flag behavior.

## TC39 Proposal Details

GocciaScript implements several active TC39 proposals alongside core ECMAScript support.

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

**Writing wrappers:** Wrapping decorators need call-site `this` to flow through. Arrow functions capture lexical `this`, so they are not suitable. The `function` keyword is not available by default. Use **object-method shorthand** — it produces a real function with call-site `this` binding:

```javascript
const wrap = (method, context) => ({
  [context.name](...args) {
    return method.apply(this, args); // this = call-site receiver
  },
})[context.name];
```

The computed key `[context.name]` preserves the wrapper's `.name` for stack traces.

**Not supported:** Parameter decorators.

### Decorator Metadata (Stage 3)

Decorator metadata works as described in [Decorators](#decorators-stage-3) — each decorated class receives a `Symbol.metadata` property with prototype-chain inheritance. See [proposal-decorator-metadata](https://github.com/tc39/proposal-decorator-metadata).

### Types as Comments (Stage 1)

GocciaScript supports the [TC39 Types as Comments](https://tc39.es/proposal-type-annotations/) proposal. TypeScript-style type annotations are parsed without affecting runtime behaviour by default. Raw type strings are preserved on AST nodes for potential future optimization.

**Opt-in runtime enforcement** — pass `--strict-types` (or set `"strict-types": true` in `goccia.json`) to enforce annotations at runtime in both interpreter and bytecode mode. Annotated variables, function parameters, and primitive-literal-inferred types are checked on initial value and on every assignment; incompatible values throw `TypeError`. Union (`string | number`), `any`, and `unknown` annotations remain unenforced. Without the flag, annotations are treated purely as comments.

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

### Pattern Matching (Stage 1)

GocciaScript supports the Stage 1 [TC39 Pattern Matching](https://tc39.es/proposal-pattern-matching/) draft in interpreter mode and bytecode mode.

```javascript
const result = match (message) {
  { type: "ok", value: const value }: value;
  { type: "error", reason: const reason }: `failed: ${reason}`;
  default: "unknown";
};
if (point is { x: const x, y: const y } and { kind: "cartesian" }) {
  x + y;
}
```

Supported pattern forms include wildcard (`_`), value patterns, `let`/`const` bindings, object and array/list patterns, rest wildcards, relational patterns (`<`, `<=`, `>`, `>=`), guards (`if (...)`), `as`, `and`, `or`, `not`, and `Symbol.customMatcher` value patterns.

Bindings are transactional: failed matches do not mutate the surrounding scope, and successful bindings are visible only in the matched body or clause expression. `or` alternatives must bind the same names with the same declaration kinds, and `not` patterns cannot contain bindings.

GocciaScript also implements proposal-note integrations:

```javascript
for (const item is { id: const id } of items) {
  id;
}
for await (const item is { id: const id } of asyncItems) {
  id;
}
try {
  work();
} catch (error is { code: const code }) {
  code;
}
```

The filtered `for...of` form currently requires an identifier subject before `is`; destructuring subjects before `is` are rejected. Pattern catches cannot be combined with catch type annotations.

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

WeakMap also implements the ES2026 upsert methods: `weakMap.getOrInsert(key, default)` and `weakMap.getOrInsertComputed(key, callbackFn)`. WeakMap keys must be objects or non-registered symbols.

### WeakRef and FinalizationRegistry (ES2021)

Weak references and finalization registries are supported for objects and non-registered symbols, matching the existing weak-key domain used by WeakMap and WeakSet. Primitives and `Symbol.for()` registry symbols are rejected because they cannot be held weakly.

`new WeakRef(target)` creates a weak reference. `weakRef.deref()` returns the target while it is still live and returns `undefined` after the target has been collected. Both construction and `deref()` add the target to the current job's kept-objects set, so repeated `deref()` calls in the same job are stable.

`new FinalizationRegistry(cleanupCallback)` creates a registry. `registry.register(target, heldValue, unregisterToken?)` registers a weak target and held value, and `registry.unregister(unregisterToken)` removes matching registrations. Explicit `Goccia.gc()` performs collection and enqueues cleanup jobs; cleanup callbacks run through the runtime's idle checkpoint after the normal microtask queue, not synchronously inside `Goccia.gc()`. Cleanup callback errors are surfaced as uncaught host callback errors.

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

When enabled (CLI: `--compat-var`, engine API: include `cfVar` in `Engine.Compatibility`, config: `{"compat-var": true}`), `var` declarations follow ES2026 §14.3.2 semantics: function-scoped (escapes blocks), hoisted to function top as `undefined`, redeclaration allowed, no TDZ, with destructuring, for-of, and enabled for-in support. Script-level `var` bindings are global-backed: they create or reuse own `globalThis` properties, new properties are writable/enumerable/non-configurable, and declaration-only redeclarations preserve an existing own global property value. Var bindings are stored in a separate binding map (`FVarBindings`) on function/module/global scopes, distinct from lexical bindings. See [interpreter.md § Scope Chain Design](interpreter.md#scope-chain-design).

### `function` Keyword

**Opt-in.** Excluded by default; use arrow functions or shorthand methods instead. Available as a compatibility mode via `--compat-function`.

When disabled (default), the parser accepts `function` declarations and expressions but treats them as no-ops (the function body is not executed and the binding is not created), and emits a warning:

```text
Warning: 'function' declarations are not supported in GocciaScript
  Suggestion: Use arrow functions: const name = (...) => { ... }; for this binding, use method shorthand: ({ name(...) {} }).name
  --> script.js:1:1
```

Function expressions in assignment position evaluate to `undefined`. Generator function declarations (`function*`) are also skipped.

GocciaScript provides two function definition styles that cover most use cases without the `function` keyword's legacy pitfalls (`this` binding confusion and hoisting surprises):

- **Arrow functions** (`(x) => x + 1`) — Lexical `this`, no hoisting, no own `arguments`. Use for standalone functions, callbacks, and closures.
- **Shorthand methods** (`method() {}`) — Call-site `this`. Use in object literals and class definitions where `this` binding is needed.

When enabled (CLI: `--compat-function`, engine API: include `cfFunction` in `Engine.Compatibility`, config: `{"compat-function": true}`), `function` declarations and expressions are supported. Their implicit `arguments` object still requires `--compat-arguments-object`.

- **Function declarations** (`function name(params) { body }`) parse as `TGocciaFunctionDeclaration` nodes whose body is backed by `TGocciaFunctionExpression`, which produces call-site `this` binding (not lexical). Declarations are hoisted: both the name and the function value are available before the declaration is reached, matching ES2026 §15.2.6 semantics. Uses the same var binding infrastructure (`DefineVariableBinding`) as `--compat-var`.
- **Sloppy block-level function declarations** follow Annex B web-compatibility semantics only when both `--compat-function` and `--compat-non-strict-mode` are active for script source. In that mode, entering a block or switch case initializes the block lexical binding, and reaching an ordinary `function` declaration updates the nearest var binding. Strict code, module source, `async function`, `function*`, and `async function*` keep GocciaScript's block-scoped behavior.
- **Function expressions** (`const f = function(params) { body }`) parse as `TGocciaFunctionExpression` nodes. Named function expressions (`const f = function g(params) { body }`) create a read-only self-binding of the name (`g`) visible only inside the function body for recursion, matching ES2026 §15.2.4 semantics.
- **Async functions** (`async function name(params) { body }`) are supported in both declaration and expression forms.
- **Generator functions** (`function*`, `async function*`) are supported when this flag is enabled. Generator method shorthand (`*method()`, `async *method()`) does not require the flag.
- **`prototype` property** — Per ES2026 §10.2.5 MakeConstructor, function declarations and expressions, generator declarations and expressions, and async generator declarations and expressions all carry an own `prototype` data property whose value is a fresh ordinary object. Plain async functions, arrow functions, concise object/class methods, and getter/setter functions do not. The `prototype` descriptor is `{ writable: true, enumerable: false, configurable: false }` for ordinary functions and `{ writable: false, enumerable: false, configurable: false }` for generators and async generators (§15.5 / §15.6). For ordinary functions only, the prototype object additionally carries an own `constructor` data property — `{ writable: true, enumerable: false, configurable: true }` — back-referencing the function. Generator and async-generator prototypes do **not** receive an own `constructor`: per §27.5.1.1 / §27.7.1.1, `g.prototype.constructor` is inherited from `%GeneratorFunction.prototype.prototype%` / `%AsyncGeneratorFunction.prototype.prototype%` (resolving to the corresponding non-callable function prototype object), not the specific generator function — so an own back-reference would be incorrect. Generator prototype objects inherit from `%GeneratorPrototype%`, and async-generator prototype objects inherit from `%AsyncGeneratorPrototype%`; those intrinsic prototype objects are realm-owned and shared by interpreter and bytecode execution.

### Loose Equality (`==` and `!=`)

**Opt-in.** Excluded by default; use `===` and `!==` unless you are running compatibility code. Available via `--compat-loose-equality` (CLI flag, `cfLooseEquality` in `Engine.Compatibility`, or `{"compat-loose-equality": true}` in config).

When enabled, `==` and `!=` follow [ES2026 §7.2.13 IsLooselyEqual](https://tc39.es/ecma262/2026/multipage/abstract-operations.html#sec-islooselyequal) in both interpreter and bytecode modes, including `null == undefined`, string/number coercion, boolean-to-number coercion, BigInt/string and BigInt/number comparisons, and object `ToPrimitive` conversion.

When disabled (default), the parser accepts `==` and `!=` but treats them as no-ops (the expression evaluates to `undefined`), and emits a warning:

```text
Warning: '==' (loose equality) is not supported in GocciaScript
  Suggestion: Use '===' (strict equality), or enable --compat-loose-equality
  --> script.js:1:10
```

Both operands are parsed but not evaluated at runtime (no side effects). Because the entire expression becomes `undefined`, which is falsy, `==`/`!=` in conditions (e.g., `if (a == b)`) will never enter the truthy branch.

Loose equality's type coercion rules are notoriously confusing, which is why the flag is off by default:

```javascript
// All true with --compat-loose-equality
"" == false       // true
0 == ""           // true
null == undefined // true
```

Strict equality requires matching types, eliminating this entire class of bugs.

### `eval()`

**Excluded from normal runtimes.** Runtime `eval` is intentionally unavailable in `GocciaScriptLoader`, `GocciaREPL`, `GocciaTestRunner`, and default `GocciaScriptLoaderBare`.

`eval` is a security risk — it executes arbitrary strings as code. In an embedded scripting environment, this is especially dangerous. The only implementation is a private conformance host hook: `GocciaScriptLoaderBare --test262-host` installs the official test262 host `eval`, plus `evalScript(sourceText)` and `createRealm()` properties on the private test262 host object, so the stock test262 harness can exercise ECMAScript direct-eval and realm semantics. Those hooks are not exposed outside conformance runs and should not be used as an application API.

### `arguments` Object

**Opt-in.** Excluded by default; prefer rest parameters. Available via `--compat-arguments-object` (CLI flag, `cfArgumentsObject` in `Engine.Compatibility`, or `{"compat-arguments-object": true}` in config). `--compat-non-strict-mode` does not enable `arguments` by itself; it only changes the semantics of an explicitly enabled `arguments` object in sloppy script functions. Strict functions, modules, and functions with non-simple parameter lists create unmapped array-like objects whose indexed entries and `length` reflect the call's argument list without aliasing parameter variables. Sloppy functions with simple parameter lists create mapped arguments exotic objects: indexed properties alias the corresponding parameter binding until the property is deleted, converted to an accessor, or made non-writable. Arrow functions do not create their own `arguments`; they resolve it lexically from the nearest enclosing ordinary function or method that has one. `arguments` is an ordinary identifier, not a reserved keyword, so parameters or body-level lexical declarations named `arguments` shadow the implicit object.

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
# Enable ASI via CLI, or use a subtree goccia.json for tests
./build/GocciaScriptLoader example.js --compat-asi
./build/GocciaTestRunner tests/language/asi
./build/GocciaREPL --compat-asi
```

```pascal
// Enable ASI via the engine API
Executor := TGocciaInterpreterExecutor.Create;
try
  Engine := TGocciaEngine.Create(FileName, Source, Executor);
  try
    Engine.Compatibility := [cfASI];
    Engine.Execute;
  finally
    Engine.Free;
  end;
finally
  Executor.Free;
end;
```

When enabled, GocciaScript follows the ECMAScript ASI rules (ES2026 §12.10):

- A semicolon is inserted when a newline separates the current and next token
- A semicolon is inserted before `}` or at EOF
- Restricted productions (`return`, `throw`, `break`) follow the `[no LineTerminator here]` rules

### Traditional `for(init; test; update)` Loop

**Opt-in for JavaScript compatibility.** Excluded by default. Available via `--compat-traditional-for-loop` (CLI flag, `cfTraditionalFor` in `Engine.Compatibility`, or `{"compat-traditional-for-loop": true}` in config) when a program or conformance suite needs ECMAScript `for(init; test; update)` semantics.

When disabled (default), the parser accepts the syntax but treats it as a no-op and emits a warning. When enabled, `for(init; test; update) body` is fully supported in both interpreter and bytecode modes:

- `let`/`const` in init create a fresh per-iteration lexical environment per [ES2026 §14.7.4.4](https://tc39.es/ecma262/#sec-runtime-semantics-forbodyevaluation), so closures captured during iteration N pin to that iteration's binding (the textbook `fns.push(() => i)` case yields `[0, 1, 2]`, not `[3, 3, 3]`).
- `var` in init requires both `--compat-var` and `--compat-traditional-for-loop`, hoists out of the loop, and is shared across iterations (closures all see the final value).
- All header parts are optional (`for(;;){…break}`, `for(;c;){…}`, `for(i;;u)`); comma expressions and destructuring are supported in init/update; `break`/`continue`/`return` unwind as in `for...of`.
- The bytecode compiler reuses the counted-loop pattern from `CompileCountedForOf` for `for(let i = N; i <op> M; i++ | i--)` shapes (rejecting `var`/`const` and bodies that mutate the loop var); the general path uses an outer scope for the canonical slot plus a per-iteration `BeginScope`/`EndScope` cycle with `OP_CLOSE_UPVALUE` for captures.

The flag exists to run code that depends on JavaScript loop semantics, such as test262 cases or compatibility harnesses. It does not change the default GocciaScript iteration guidance, which remains `for...of`, `for await...of`, or array methods:

```javascript
// Instead of: for (let i = 0; i < items.length; i++) { ... }
for (const item of items) { ... }
items.forEach((item) => { ... });
items.map((item) => transform(item));
items.filter((item) => item.isValid);
items.reduce((acc, item) => acc + item, 0);
```

`break` exits the nearest enclosing `switch`, `for...of`, `for await...of`, enabled traditional `for` loop, enabled `for...in` loop, or enabled `while`/`do...while` loop. `continue` only applies to iteration constructs — `for...of`, `for await...of`, enabled traditional `for`, enabled `for...in`, and enabled `while`/`do...while` — never to `switch`.

### `for...in` Loop

**Opt-in for JavaScript compatibility.** Excluded by default. Available via `--compat-for-in-loop` (CLI flag, `cfForIn` in `Engine.Compatibility`, or `{"compat-for-in-loop": true}` in config) when a program or conformance suite needs ECMAScript property enumeration semantics.

When disabled (default), the parser accepts `for...in` declaration and assignment-target forms but treats the loop as a no-op and emits a warning. When enabled, declaration-head loops such as `for (const key in object) body` / `for (let key in object) body` and assignment-target loops such as `for (key in object) body` are supported in interpreter and bytecode modes:

- The right-hand side follows ECMAScript `ForIn/OfHeadEvaluation`: `null` and `undefined` produce an empty loop; other primitives are boxed with `ToObject`.
- The loop enumerates enumerable string property names, including inherited enumerable properties, and never returns symbol keys.
- A property name appears at most once. An own property shadows a prototype property with the same name even when the own property is non-enumerable.
- `var` declaration heads, including destructuring heads, require both `--compat-var` and `--compat-for-in-loop`; the bindings hoist out of the loop and are shared across iterations. At script top level, even an empty `for (var name in {}) {}` instantiates the same non-configurable global-backed `var` binding as any other script `var`.
- `break`, `continue`, and `return` unwind as they do in other supported loops.

New GocciaScript code should usually prefer `Object.keys(obj)`, `Object.entries(obj)`, or explicit `for...of` over property names:

```javascript
for (const key of Object.keys(obj)) {
  // ...
}
```

### `while` and `do...while`

**Opt-in for JavaScript compatibility.** Excluded by default. Available via `--compat-while-loops` (CLI flag, `cfWhileLoops` in `Engine.Compatibility`, or `{"compat-while-loops": true}` in config) when a program or conformance suite needs ECMAScript `while` or `do...while` semantics.

When disabled (default), the parser accepts `while` and `do...while` syntax but treats each loop as a no-op and emits a warning. When enabled, both loop forms are supported in interpreter and bytecode modes:

- `while (condition) body` evaluates the condition before each iteration and skips the body when the initial condition is falsy.
- `do body while (condition);` runs the body once before testing the condition.
- `break`, `continue`, and `return` unwind as they do in other supported loops.
- Timeouts and instruction limits are checked during loop execution.

```javascript
let i = 0;
let sum = 0;

while (i < 5) {
  sum += i;
  i++;
}

do {
  sum++;
} while (sum < 20);
```

### `with` Statement

**Opt-in for script source.** Excluded by default; use explicit property access. Available via `--compat-non-strict-mode` (CLI flag, `cfNonStrictMode` in `Engine.Compatibility`, or `{"compat-non-strict-mode": true}` in config). Module source remains strict even when this flag is enabled. When enabled in script source, `with (object) statement` evaluates the object expression, converts it with `ToObject`, and resolves unqualified identifiers through that object before falling back to outer lexical scopes. `Symbol.unscopables` is honored. Calls to functions resolved through the object environment use that object as `this`, closures created inside the body retain the object environment, and writes through non-writable or setter-less object properties silently keep the original value instead of throwing. When disabled (default), the parser accepts the syntax but treats the statement as a no-op and emits a warning suggesting explicit property access or the flag. `with` creates ambiguous scope and is forbidden by JavaScript strict mode, so new GocciaScript code should prefer explicit property access. The keyword is reserved (it cannot be used as a variable name), but it can be used as a property name (for example, `obj.with`).

### Non-Strict Assignment Semantics

By default, assigning to a read-only property, setter-less accessor, or non-extensible object throws `TypeError`. With `--compat-non-strict-mode` in script source, failed ordinary object and global object writes are ignored instead. The assignment expression still evaluates to the assigned value, matching ECMAScript non-strict `[[Set]]` behavior. Setters that throw still propagate their error, and property access through `null` or `undefined` still throws `TypeError`.

### `delete` Semantics

By default, GocciaScript follows strict delete behavior: deleting an unqualified identifier is an error, and deleting a non-configurable property throws `TypeError`. With `--compat-non-strict-mode` in script source, legacy non-strict delete return values are enabled in both interpreter and bytecode modes. `delete name` returns `false` for declared bindings, deletes configurable global object properties when the identifier resolves there, and returns `true` for unresolvable names. `delete obj.prop` and `delete obj[key]` return `false` for non-configurable properties instead of throwing. Accessing a property through `null` or `undefined` still throws `TypeError`.

### Labeled Statements

**Opt-in for JavaScript compatibility.** Excluded by default. Available via `--compat-label` (CLI flag, `cfLabel` in `Engine.Compatibility`, or `{"compat-label": true}` in config) when a program or conformance suite needs ECMAScript labeled control flow.

When disabled (default), the parser accepts labeled statements but strips the label and emits a warning:

```text
Warning: Labeled statements are not supported in GocciaScript
  --> script.js:1:1
```

The labeled statement itself (the statement after the `:`) is still parsed and executed normally. For example, `myLabel: x = 2;` strips the label and executes `x = 2;`. If the labeled statement is itself unsupported (e.g., `outer: for (...)`), both a label warning and a loop warning are emitted.

When enabled, labels can target `break` and `continue` statements in interpreter and bytecode modes:

- `break label;` exits the matching enclosing labeled statement, including labeled blocks, `switch`, `for...of`, `for await...of`, traditional `for(;;)` with `--compat-traditional-for-loop`, `for...in` with `--compat-for-in-loop`, and `while`/`do...while` with `--compat-while-loops`.
- `continue label;` targets matching enclosing iteration statements only: `for...of`, `for await...of`, traditional `for(;;)` with `--compat-traditional-for-loop`, `for...in` with `--compat-for-in-loop`, and `while`/`do...while` with `--compat-while-loops`.

With `--compat-function` and `--compat-non-strict-mode`, sloppy labeled function declarations follow the same Annex B compatibility rules as unlabeled block-level function declarations. Direct sloppy labeled function declarations are valid, but a labeled function declaration cannot be the immediate body of `if`, `else`, `with`, or an iteration statement; wrap it in a block if that legacy shape is required.

### Generators and Iterators

Generator method shorthand (`*method()` and `async *method()`) is supported by default. Generator function syntax (`function*` and `async function*`) is supported only when `--compat-function` is enabled. Iterator protocol and Iterator Helpers are also implemented.

## Intentional Divergences from ECMAScript

These are deliberate differences from standard ECMAScript behavior, not missing features.

### Legacy Global Number Helpers

**Shimmed for ECMAScript compatibility.** Prefer `Number.parseInt`, `Number.parseFloat`, `Number.isNaN`, and `Number.isFinite` in new code.

In ECMAScript, these exist as both global functions and as `Number` static methods. `parseInt` and `parseFloat` are identical to their `Number.*` counterparts. However, the global `isNaN` and `isFinite` have legacy coercion behavior (e.g., `isNaN("abc")` returns `true` because it coerces the string to a number first), while `Number.isNaN` and `Number.isFinite` are stricter — they return `false` for any non-number argument.

GocciaScript installs the global names through Goccia.shims so legacy ECMAScript code and test262 cases see the standard surface. The default recommendation remains explicit `Number.*` calls in userland code:

```javascript
Number.parseInt("10", 10);
Number.isNaN(value);

// Shimmed legacy globals still exist:
parseInt("10", 10);
isNaN("abc"); // true, because the global form coerces
```

## Strictness Guarantees

GocciaScript operates in an implicit strict mode (see [Errors](errors.md) for the full error type reference):

- All variables must be declared before use.
- Duplicate parameter names are forbidden.
- Temporal Dead Zone is enforced for `let`/`const`.
- `const` reassignment throws `TypeError`.
- Accessing undeclared variables throws `ReferenceError`.
- Assigning to read-only properties, setter-less accessors, or non-extensible objects throws `TypeError` unless script source `--compat-non-strict-mode` is enabled.
- Deleting an unqualified identifier or non-configurable property is an error unless script source `--compat-non-strict-mode` is enabled.
- `this` is `undefined` in standalone function calls unless script source `--compat-non-strict-mode` is enabled.
- Modules remain strict regardless of `--compat-non-strict-mode`.
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
| Standalone function call | `undefined` by default; `globalThis` for ordinary functions with script source `--compat-non-strict-mode` |
| `fn.call(thisArg)` / `fn.apply(thisArg)` | Explicit `thisArg`; nullish `thisArg` becomes `globalThis` for ordinary functions with script source `--compat-non-strict-mode` |
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
