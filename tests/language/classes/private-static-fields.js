/*---
description: Private static fields
features: [private-static-fields]
---*/

test("private static fields are not exposed on instances or constructors", () => {
  class TestClass {
    static #staticPrivate = "static value";
    #instancePrivate = "instance value";

    static getStaticPrivate() {
      return TestClass.#staticPrivate;
    }

    static setStaticPrivate(value) {
      TestClass.#staticPrivate = value;
    }

    static incrementStaticPrivate() {
      TestClass.#staticPrivate += " modified";
    }

    getInstancePrivate() {
      return this.#instancePrivate;
    }
  }

  const instance = new TestClass();

  expect(TestClass.getStaticPrivate()).toBe("static value");
  TestClass.setStaticPrivate("new static value");
  expect(TestClass.getStaticPrivate()).toBe("new static value");
  TestClass.incrementStaticPrivate();
  expect(TestClass.getStaticPrivate()).toBe("new static value modified");

  expect(instance.getInstancePrivate()).toBe("instance value");
  expect(Object.hasOwn(TestClass, "#staticPrivate")).toBe(false);
  expect(Object.hasOwn(instance, "#instancePrivate")).toBe(false);
});

test("private static accessors can be read, written, and used in compound assignment", () => {
  class TestClass {
    static #value = 2;

    static get #computed() {
      return this.#value * 10;
    }

    static set #computed(value) {
      this.#value = value / 10;
    }

    static getComputed() {
      return this.#computed;
    }

    static add(delta) {
      this.#computed += delta;
      return this.#value;
    }
  }

  expect(TestClass.getComputed()).toBe(20);
  expect(TestClass.add(30)).toBe(5);
  expect(TestClass.getComputed()).toBe(50);
});

test("writing to getter-only private static accessors throws TypeError", () => {
  class TestClass {
    static #value = 2;

    static get #computed() {
      return this.#value * 10;
    }

    static setComputed(value) {
      this.#computed = value;
    }
  }

  expect(() => TestClass.setComputed(40)).toThrow(TypeError);

  try {
    TestClass.setComputed(40);
    throw new Error("expected TypeError");
  } catch (e) {
    expect(e.message).toContain("Private accessor");
    expect(e.message).toContain("was defined without a setter");
  }
});

test("reading from setter-only private static accessors throws TypeError", () => {
  class TestClass {
    static #value = 2;

    static set #computed(value) {
      this.#value = value / 10;
    }

    static getComputed() {
      return this.#computed;
    }
  }

  expect(() => TestClass.getComputed()).toThrow(TypeError);

  try {
    TestClass.getComputed();
    throw new Error("expected TypeError");
  } catch (e) {
    expect(e.message).toContain("Private accessor");
    expect(e.message).toContain("was defined without a getter");
  }
});
