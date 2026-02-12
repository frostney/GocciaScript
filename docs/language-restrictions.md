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
- Rest parameters.
- Destructuring parameters (array and object patterns with defaults).
- `fn.call()`, `fn.apply()`, `fn.bind()` for explicit `this` binding.

```javascript
const add = (a, b) => a + b;
const greet = (name = "world") => `Hello, ${name}!`;
const first = ([head, ...rest]) => head;
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
- `for` loops (traditional `for(;;)`)
- `while` and `do...while` loops
- `switch`/`case`/`default`
- `try`/`catch`/`finally`
- `throw`
- `return`
- `break`/`continue`
- Block statements
- `import`/`export` (module system)

### Data Structures

- Objects (literal syntax, computed properties, shorthand methods).
- Arrays (literals, sparse arrays, prototype methods).
- Strings (template literals, prototype methods).

### String Escape Sequences

Standard escape sequences in string literals and template literals:

- `\n`, `\r`, `\t`, `\\`, `\'`, `\"`, `\0`
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

**Excluded.** Use arrow functions instead.

The `function` keyword creates several problems:
- **`this` binding confusion** — Regular functions have their own `this` that changes based on how they're called.
- **Hoisting** — Function declarations are hoisted, making code order misleading.
- **`arguments` object** — Creates an implicit magic variable.

Arrow functions have lexical `this`, no hoisting, and no `arguments` — they're predictable.

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

### `with` Statement

**Excluded.** No alternative needed.

`with` creates ambiguous scope and is deprecated even in JavaScript's strict mode.

### Generators and Iterators

**Not yet implemented.** May be added in future versions.

### `async`/`await`

**Not yet implemented.** The `ggPromise` flag exists as a placeholder.

### Regular Expressions

**Not yet implemented.** String methods like `replace` work with string patterns.

## Strictness Guarantees

GocciaScript operates in an implicit strict mode:

- All variables must be declared before use.
- Duplicate parameter names are forbidden.
- Temporal Dead Zone is enforced for `let`/`const`.
- `const` reassignment throws `TypeError`.
- Accessing undeclared variables throws `ReferenceError`.
