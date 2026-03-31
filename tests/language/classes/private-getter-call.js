/*---
description: Calling private getters, setters, and methods
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

test("private methods can validate and transform values", () => {
  class TestClass {
    #value = 0;

    #validate(val) {
      return typeof val === "number" && val >= 0;
    }

    #transform(val) {
      return val * 2;
    }

    setValue(val) {
      if (this.#validate(val)) {
        this.#value = this.#transform(val);
      }
    }

    getValue() {
      return this.#value;
    }
  }

  const instance = new TestClass();

  instance.setValue(5);
  expect(instance.getValue()).toBe(10);
});

test("private getters can derive values from private fields", () => {
  class TestClass {
    #value = 10;

    get #computed() {
      return this.#value * 10;
    }

    getComputed() {
      return this.#computed;
    }
  }

  const instance = new TestClass();
  expect(instance.getComputed()).toBe(100);
});

test("private setters can update private fields", () => {
  class TestClass {
    #value = 10;

    set #computed(val) {
      this.#value = val / 10;
    }

    getValue() {
      return this.#value;
    }

    setComputed(val) {
      this.#computed = val;
    }
  }

  const instance = new TestClass();

  instance.setComputed(50);
  expect(instance.getValue()).toBe(5);
});

test("assigning to getter-only private accessors throws TypeError", () => {
  class TestClass {
    #value = 10;

    get #computed() {
      return this.#value * 10;
    }

    setComputed(val) {
      this.#computed = val;
    }
  }

  const instance = new TestClass();
  expect(() => instance.setComputed(50)).toThrow(TypeError);
});

test("reading from setter-only private accessors throws TypeError", () => {
  class TestClass {
    #value = 10;

    set #computed(val) {
      this.#value = val / 10;
    }

    getComputed() {
      return this.#computed;
    }
  }

  const instance = new TestClass();
  expect(() => instance.getComputed()).toThrow(TypeError);
});

test("private compound assignment uses getter and setter accessors", () => {
  class TestClass {
    #value = 2;

    get #computed() {
      return this.#value * 10;
    }

    set #computed(val) {
      this.#value = val / 10;
    }

    increment() {
      this.#computed += 30;
      return this.#value;
    }
  }

  const instance = new TestClass();
  expect(instance.increment()).toBe(5);
});

test("private compound assignment throws on getter-only accessors", () => {
  class TestClass {
    #value = 2;

    get #computed() {
      return this.#value * 10;
    }

    increment() {
      this.#computed += 30;
    }
  }

  const instance = new TestClass();
  expect(() => instance.increment()).toThrow(TypeError);
});
