# Language Restrictions

GocciaScript implements a curated subset of ECMAScript. This document details what's supported, what's excluded, and the rationale for each decision.

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

- Arrow functions only (no `function` keyword).
- Default parameters (including in destructured parameters).
- Rest parameters (`...args`).
- Destructuring parameters (array and object patterns with defaults).
- `fn.call()`, `fn.apply()`, `fn.bind()` for explicit `this` binding.
- `fn.length` — Number of formal parameters (before defaults/rest).
- `fn.name` — Function name (inferred from variable declarations for anonymous functions).
- Type annotations on parameters and return types (parsed and ignored at runtime — see [Types as Comments](#types-as-comments) below).

```javascript
const add = (a, b) => a + b;
const greet = (name = "world") => `Hello, ${name}!`;
const first = ([head, ...rest]) => head;
add.name;   // "add"
add.length; // 2
```

### Classes

- Class declarations and expressions.
- Constructor, instance methods, static methods.
- Getters and setters.
- Private fields and methods (`#field`).
- Static properties.
- Inheritance with `extends` and `super`.

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
- Optional chaining (`?.` for property access, computed access, and method calls).
- Bitwise operators (`&`, `|`, `^`, `<<`, `>>`, `>>>`).
- Ternary operator (`? :`).
- Template literals with interpolation.
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
- `import`/`export` (ES module system — named exports only, no default exports)

### Modules

ES module syntax with named exports. Supported file extensions: `.js`, `.jsx`, `.ts`, `.tsx`, `.mjs`. Module paths are resolved relative to the importing file. File extensions can be omitted — the resolver tries each extension in order. Directory imports resolve to `index` files.

```javascript
// Named imports (with or without extension)
import { add, multiply } from "./math.js";
import { add, multiply } from "./math";      // resolves to ./math.js, .jsx, .ts, .tsx, or .mjs

// Named exports
export { myFunction, myValue };

// Inline exports
export const PI = 3.14159;
export let count = 0;

// Re-exports
export { add, multiply } from "./math.js";
export { greet as sayHello } from "./utils.js";

// JSON imports — top-level keys become named exports
import { name, version } from "./package.json";
import { host as dbHost } from "./config.json";

// Directory/index resolution
import { setup } from "./utils";  // resolves to ./utils/index.js (or .ts, .jsx, etc.)
```

**Not supported:** `export default`, `import x from` (default import), `import * as` (namespace import), `import "module"` (side-effect import), `export * from` (wildcard re-export), dynamic `import()`. The parser accepts these syntactically but treats them as no-ops, emitting a warning with a suggestion:

```text
Warning: Default imports are not supported in GocciaScript
  Suggestion: Use named imports instead: import { name } from 'module'
  --> script.js:1:1

Warning: Wildcard re-exports (export * from ...) are not supported in GocciaScript
  Suggestion: Use named re-exports instead: export { name } from 'module'
  --> script.js:2:1
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

## Excluded Features

### `var` Declarations

**Excluded.** Use `let` or `const` instead.

`var` has function-scoping and hoisting behavior that leads to subtle bugs:

```javascript
// In JavaScript, this prints "undefined" then "5"
// because var is hoisted and function-scoped
console.log(x); // undefined (hoisted)
var x = 5;
console.log(x); // 5
```

With `let`/`const`, accessing before declaration is a `ReferenceError` (Temporal Dead Zone), which catches bugs early.

### `function` Keyword

**Excluded.** Use arrow functions or shorthand methods instead. The parser accepts `function` declarations and expressions but treats them as no-ops (the function body is not executed and the binding is not created), and emits a warning:

```text
Warning: 'function' declarations are not supported in GocciaScript
  Suggestion: Use arrow functions instead: const name = (...) => { ... }
  --> script.js:1:1
```

Function expressions in assignment position evaluate to `undefined`. Generator function declarations (`function*`) are also skipped.

The `function` keyword creates several problems:
- **`this` binding confusion** — Regular functions have their own `this` that changes based on how they're called.
- **Hoisting** — Function declarations are hoisted, making code order misleading.
- **`arguments` object** — Creates an implicit magic variable.

GocciaScript provides two function definition styles that cover all use cases:

- **Arrow functions** (`(x) => x + 1`) — Lexical `this`, no hoisting, no `arguments`. Use for standalone functions, callbacks, and closures.
- **Shorthand methods** (`method() {}`) — Call-site `this`, like ECMAScript's regular functions. Use in object literals and class definitions where `this` binding is needed.

This separation is clean and matches ECMAScript strict mode semantics exactly.

### Loose Equality (`==` and `!=`)

**Excluded.** Use `===` and `!==` instead. The parser accepts `==` and `!=` but treats them as no-ops (the expression evaluates to `undefined`), and emits a warning:

```text
Warning: '==' (loose equality) is not supported in GocciaScript
  Suggestion: Use '===' (strict equality) instead
  --> script.js:1:10
```

Both operands are parsed but discarded. Because `undefined` is falsy, `==`/`!=` in conditions (e.g., `if (a == b)`) will never enter the truthy branch.

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

**Excluded.** Semicolons are required.

ASI rules are complex and can lead to unexpected behavior:

```javascript
// In JavaScript, this returns undefined due to ASI
return
  { value: 42 }
```

GocciaScript requires explicit semicolons, preventing this class of bugs.

### Traditional Loops (`for`, `while`, `do...while`)

**Excluded.** Use array methods instead. The parser accepts loop syntax but treats it as a no-op (the loop body is not executed), and emits a warning:

```text
Warning: 'for' loops are not supported in GocciaScript
  Suggestion: Use array methods like .forEach(), .map(), .filter(), or .reduce() instead
  --> script.js:1:1
```

The parser uses balanced-parenthesis tracking (`SkipBalancedParens`) to correctly skip the loop condition even when it contains nested parentheses (e.g., `for (let i = Math.max(0, 1); i < fn(x); i++)`), then `SkipStatementOrBlock` to skip the loop body. This ensures subsequent code executes correctly.

Traditional loops encourage imperative, mutation-heavy code. GocciaScript favors functional iteration through array methods:

```javascript
// Instead of: for (let i = 0; i < items.length; i++) { ... }
items.forEach((item) => { ... });
items.map((item) => transform(item));
items.filter((item) => item.isValid);
items.reduce((acc, item) => acc + item, 0);
```

`break` and `continue` are only available inside `switch` statements.

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
- **structuredClone** — Deep clone of values. May be added as a utility.
- **URI functions** (`encodeURI`, `decodeURI`, `encodeURIComponent`, `decodeURIComponent`) — URL encoding/decoding.
- **atob / btoa** — Base64 encoding/decoding.
- **Regular Expressions** — `RegExp` constructor and regex literal syntax. String methods like `replace` currently work with string patterns only.
- **async / await** — Syntax sugar for Promises. Use `.then()` chaining instead.

## Types as Comments

GocciaScript supports the [TC39 Types as Comments](https://tc39.es/proposal-type-annotations/) proposal. TypeScript-style type annotations are parsed but have **no runtime effect** — they are treated as comments by the evaluator. Raw type strings are preserved on AST nodes for potential future optimization.

### Supported Syntax

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

### Not Supported

- Enums (`enum Direction { ... }`) — use plain objects instead.
- Namespaces (`namespace Foo { ... }`).
- Parameter properties in constructors (`constructor(public x: number)`).
- Angle-bracket type assertions (`<string>value`) — use `value as string` instead.
- Decorators (`@decorator`).

### JSX (Opt-in)

**Supported** when `ggJSX` is enabled. JSX is handled by a source-to-source pre-pass transformer that converts JSX syntax into `createElement` function calls before the main compilation pipeline. This keeps the core lexer/parser/evaluator untouched.

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

The transformer generates an internal source map for accurate error line/column reporting. JSX is enabled by default in `DefaultGlobals`; to disable it, exclude `ggJSX` from the globals set.

### `async`/`await`

**Not yet implemented.** Promises are fully implemented (constructor, `.then`/`.catch`/`.finally`, `Promise.all`/`allSettled`/`race`/`any`, microtask queue), but `async`/`await` syntax sugar is not yet available. Use `.then()` chaining instead.

### Regular Expressions

**Not yet implemented.** String methods like `replace` work with string patterns.

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

GocciaScript operates in an implicit strict mode:

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
