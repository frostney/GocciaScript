/*---
description: Private fields can be accessed inside template literal interpolations
features: [private-fields, template-literals]
---*/

describe("Private fields in template literals", () => {
  test("basic private field interpolation", () => {
    class Greeter {
      #name = "World";

      greet() {
        return `Hello, ${this.#name}!`;
      }
    }

    const g = new Greeter();
    expect(g.greet()).toBe("Hello, World!");
  });

  test("multiple private fields in template literal", () => {
    class Person {
      #first = "Jane";
      #last = "Doe";

      fullName() {
        return `${this.#first} ${this.#last}`;
      }
    }

    const p = new Person();
    expect(p.fullName()).toBe("Jane Doe");
  });

  test("private field with expressions in template literal", () => {
    class Calculator {
      #value = 10;

      describe() {
        return `Value is ${this.#value}, doubled is ${this.#value * 2}`;
      }
    }

    const c = new Calculator();
    expect(c.describe()).toBe("Value is 10, doubled is 20");
  });

  test("private field in template literal inside callback", () => {
    class Formatter {
      #prefix = "item";

      format(items) {
        return items.map((item) => `${this.#prefix}: ${item}`);
      }
    }

    const f = new Formatter();
    const result = f.format(["a", "b"]);
    expect(result[0]).toBe("item: a");
    expect(result[1]).toBe("item: b");
  });

  test("private object field with computed access in template literal", () => {
    class PriceList {
      #prices = { coffee: 3.5, tea: 2.0 };

      describe(item) {
        return `${item}: $${this.#prices[item]}`;
      }
    }

    const p = new PriceList();
    expect(p.describe("coffee")).toBe("coffee: $3.5");
  });
});
