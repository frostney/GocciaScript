# Your First GocciaScript Program

*For developers who know JavaScript and want to learn GocciaScript — a guided walkthrough from hello world to a multi-file async program.*

## What is GocciaScript?

GocciaScript is a subset of JavaScript implemented in FreePascal. It strips away the quirks of early ECMAScript — `var`, `function`, loose equality, `eval`, traditional loops — and keeps the modern parts: arrow functions, classes with private fields, async/await, modules, and implicit strict mode. If you've written modern JavaScript, you already know most of GocciaScript.

## Prerequisites

You need the [FreePascal](https://www.freepascal.org/) compiler (`fpc`):

```bash
# macOS
brew install fpc

# Ubuntu/Debian
sudo apt-get install fpc

# Windows
choco install freepascal
```

Clone the repository and build the script loader:

```bash
git clone https://github.com/frostney/GocciaScript.git
cd GocciaScript
./build.pas loader
```

This produces `build/ScriptLoader`, the command you'll use to run every script in this tutorial.

## Hello, World

Create a file called `hello.js`:

```javascript
const message = "Hello from GocciaScript!";
console.log(message);
```

Run it:

```bash
./build/ScriptLoader hello.js
```

You should see:

```text
Hello from GocciaScript!
```

That's it — GocciaScript files are plain `.js` files. No special extension, no transpilation step.

## Variables

GocciaScript has two variable declarations: `let` (mutable) and `const` (immutable). There is no `var`.

```javascript
const name = "Alice";
let score = 0;

score = score + 10;
console.log(`${name} scored ${score} points`);
// Alice scored 10 points
```

Attempting to reassign a `const` throws an error:

```javascript
const x = 5;
x = 10; // TypeError: Assignment to constant variable
```

## Arrow Functions

GocciaScript uses arrow functions exclusively. The `function` keyword does not exist.

```javascript
// Single expression — implicit return
const double = (n) => n * 2;

// Block body — explicit return
const greet = (name) => {
  const greeting = `Hello, ${name}!`;
  return greeting;
};

console.log(double(21));       // 42
console.log(greet("World"));   // Hello, World!
```

Arrow functions capture their surrounding scope's `this` — there's no `this` rebinding. For methods that need their own `this`, use shorthand method syntax inside classes or object literals.

## Working with Arrays

GocciaScript has no traditional loops (`for`, `while`, `do...while`). Instead, you use array methods and `for...of`:

```javascript
const numbers = [1, 2, 3, 4, 5];

// Transform with map
const doubled = numbers.map((n) => n * 2);
console.log(doubled); // [2, 4, 6, 8, 10]

// Filter
const evens = numbers.filter((n) => n % 2 === 0);
console.log(evens); // [2, 4]

// Reduce
const sum = numbers.reduce((total, n) => total + n, 0);
console.log(sum); // 15

// Iterate with for...of
for (const n of numbers) {
  console.log(n);
}
```

All the modern array methods you'd expect are available: `find`, `findLast`, `some`, `every`, `flat`, `flatMap`, `at`, `toSorted`, `toReversed`, and more. See the [Built-in Objects](built-ins.md) reference for the full list.

## Objects

Object literals work like JavaScript, including shorthand properties, computed keys, and methods:

```javascript
const x = 10;
const y = 20;

const point = {
  x,
  y,
  distanceTo(other) {
    const dx = this.x - other.x;
    const dy = this.y - other.y;
    return Math.sqrt(dx ** 2 + dy ** 2);
  },
};

const origin = { x: 0, y: 0 };
console.log(point.distanceTo(origin)); // 22.360679774997898
```

Note that the `distanceTo` method uses shorthand method syntax (`distanceTo() { ... }`), not an arrow function. This is important: shorthand methods receive the call-site `this` (the object they're called on), while arrow functions inherit `this` from their enclosing scope.

## Classes

Classes support constructors, private fields, getters, setters, static methods, and inheritance:

```javascript
class CoffeeShop {
  #name = "Goccia Coffee";
  #beans = ["Arabica", "Robusta", "Ethiopian"];
  #prices = { espresso: 2.5, latte: 4.0, cappuccino: 3.75 };

  getMenu() {
    return this.#beans.map((bean) => `${bean} blend`);
  }

  calculateTotal(order) {
    return order.reduce((total, item) => total + (this.#prices[item] ?? 0), 0);
  }

  get name() {
    return this.#name;
  }
}

const shop = new CoffeeShop();
console.log(`Welcome to ${shop.name}!`);
console.log("Menu:", shop.getMenu());

const order = ["espresso", "latte"];
console.log(`Total: $${shop.calculateTotal(order).toFixed(2)}`);
```

Inheritance uses `extends` and `super`:

```javascript
class Animal {
  constructor(name) {
    this.name = name;
  }

  speak() {
    return `${this.name} makes a sound`;
  }
}

class Dog extends Animal {
  speak() {
    return `${this.name} barks`;
  }
}

const dog = new Dog("Rex");
console.log(dog.speak());            // Rex barks
console.log(dog instanceof Animal);  // true
```

## Modules

GocciaScript supports ES-style named imports and exports. Default imports/exports are not supported — use named exports instead.

Create a file called `math.js`:

```javascript
export const add = (a, b) => a + b;
export const multiply = (a, b) => a * b;
```

Import it from another file called `app.js` in the same directory:

```javascript
import { add, multiply } from "./math.js";

console.log(add(2, 3));       // 5
console.log(multiply(4, 5));  // 20
```

Run the entry point:

```bash
./build/ScriptLoader app.js
```

You can also rename imports with `as`:

```javascript
import { add as sum } from "./math.js";
console.log(sum(1, 2)); // 3
```

And re-export from one module to another:

```javascript
export { add, multiply } from "./math.js";
```

## Async/Await

Async functions and `await` work as you'd expect from modern JavaScript:

```javascript
const fetchUser = async (id) => {
  const result = await Promise.resolve({ id, name: "Alice" });
  return result;
};

const main = async () => {
  const user = await fetchUser(1);
  console.log(`User: ${user.name}`);
};

main();
```

Promises are fully supported — `.then()`, `.catch()`, `.finally()`, `Promise.all()`, `Promise.race()`, `Promise.any()`, `Promise.allSettled()`, and `Promise.withResolvers()`.

GocciaScript uses a synchronous microtask queue: all pending `.then()` callbacks are drained after synchronous code completes.

## Strict Equality Only

GocciaScript enforces strict equality. The loose equality operators (`==` and `!=`) are not available — use `===` and `!==`:

```javascript
console.log(1 === 1);     // true
console.log(1 === "1");   // false
console.log(null === undefined); // false
```

## What's Different from JavaScript

Here's a quick reference of GocciaScript's key restrictions:

| JavaScript | GocciaScript | Alternative |
|------------|-------------|-------------|
| `var x = 1` | Not supported | `let x = 1` or `const x = 1` |
| `function foo() {}` | Not supported | `const foo = () => {}` |
| `==` / `!=` | Not supported | `===` / `!==` |
| `for (...)` / `while (...)` | Not supported | `for...of`, `.map()`, `.forEach()`, `.reduce()` |
| `eval("code")` | Not supported | No alternative (by design) |
| `arguments` | Not supported | `(...args) => {}` |
| `parseInt("10")` | Not available as global | `Number.parseInt("10")` |
| `isNaN(x)` | Not available as global | `Number.isNaN(x)` |
| `import x from "mod"` | Not supported | `import { x } from "mod"` |

These restrictions are intentional — they eliminate common sources of bugs and security issues. See [Language Restrictions](language-restrictions.md) for the full rationale.

## Next Steps

You now have a working understanding of GocciaScript. Here's where to go from here:

- **[Language Restrictions](language-restrictions.md)** — Full list of what's supported and what's excluded, with rationale
- **[Built-in Objects](built-ins.md)** — Complete API reference for all built-in objects (Array, String, Map, Set, Promise, Temporal, etc.)
- **[`examples/`](../examples/)** — More example programs: classes, promises, and unsupported feature demos
- **[Architecture](architecture.md)** — How the engine works under the hood, for contributors
