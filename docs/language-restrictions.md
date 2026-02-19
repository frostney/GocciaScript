# Language Restrictions

GocciaScript implements a curated subset of ECMAScript 2020. This document details what's supported, what's excluded, and the rationale for each decision.

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

ES module syntax with named exports. Module paths are resolved relative to the importing file.

```javascript
// Named imports
import { add, multiply } from "./math.js";
import { greet as sayHello } from "./utils.js";

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
```

**Not supported:** `export default`, `import x from` (default import), `import * as` (namespace import), `import "module"` (side-effect import), `export * from` (wildcard re-export), dynamic `import()`.

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

**Excluded.** Use arrow functions or shorthand methods instead.

The `function` keyword creates several problems:
- **`this` binding confusion** — Regular functions have their own `this` that changes based on how they're called.
- **Hoisting** — Function declarations are hoisted, making code order misleading.
- **`arguments` object** — Creates an implicit magic variable.

GocciaScript provides two function definition styles that cover all use cases:

- **Arrow functions** (`(x) => x + 1`) — Lexical `this`, no hoisting, no `arguments`. Use for standalone functions, callbacks, and closures.
- **Shorthand methods** (`method() {}`) — Call-site `this`, like ECMAScript's regular functions. Use in object literals and class definitions where `this` binding is needed.

This separation is clean and matches ECMAScript strict mode semantics exactly.

### Loose Equality (`==` and `!=`)

**Excluded.** Use `===` and `!==` instead.

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

**Excluded.** Use array methods instead.

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

**Excluded.** No alternative needed.

`with` creates ambiguous scope and is deprecated even in JavaScript's strict mode.

### Generators and Iterators

**Not yet implemented.** May be added in future versions.

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
