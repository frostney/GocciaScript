/*---
description: Calling private getters that return callables
features: [private-fields, private-methods, getters-setters]
---*/

test("private getter returning function can be called", () => {
  class Foo {
    get #getGreeter() {
      return (name) => `Hello, ${name}!`;
    }

    greet(name) {
      return this.#getGreeter(name);
    }
  }

  const foo = new Foo();
  expect(foo.greet("world")).toBe("Hello, world!");
});

test("private getter returning value accessed without call", () => {
  class Bar {
    #secret = 42;

    get #value() {
      return this.#secret;
    }

    getValue() {
      return this.#value;
    }
  }

  const bar = new Bar();
  expect(bar.getValue()).toBe(42);
});
