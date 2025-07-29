# GocciaScript

A drop of JavaScript - A subset of ECMAScript 2020 implemented in FreePascal

## Features

### Language Features

- **Variables**: `let` and `const` declarations (no `var`)
- **Functions**: Arrow functions only (no `function` keyword)
- **Loops**: No traditional loops - use array methods instead
- **Equality**: Triple equals (`===`) only
- **Strict Mode**: Implicit (no need to declare)
- **No `eval`**: For security
- **No `arguments`**: Simplified argument handling due to not having an ES3-like `function`

### ES6+ specific features

- Template strings
- Object shorthands

### ECMAScript 2020+ native implementations

- `Math.clamp`
- Private fields and methods in classes

## Example

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
const order = ["espresso", "latte"];
const total = shop.calculateTotal(order);

console.log(`Welcome to ${shop.name}!`);
console.log(`Your order total: $${total.toFixed(2)}`);
```

## How To Use

### Pre-compiled binaries

### Integrate into host environment

## Roadmap

- [ ] Module system
- [ ] HMR

### Ideas to evaluate

- Performance tests
-

## Concepts

- Explicitness: Modules, classes, methods and properties are exlicit even at the cost long names. Shortcuts are typically avoided.
- OOP over everything: Rely on type safety of specialised classes
- `Define` vs `Assign`: Explicitly defines a variable, assign re-assigns the value
