/*---
description: Private static fields
features: [private-static-fields]
---*/

test("private static fields", () => {
  class TestClass {
    static #staticPrivate = "static value";
    #instancePrivate = "instance value";

    static getStaticPrivate() {
      return TestClass.#staticPrivate;
    }

    static setStaticPrivate(value) {
      TestClass.#staticPrivate = value;
    }

    getInstancePrivate() {
      return this.#instancePrivate;
    }

    setInstancePrivate(value) {
      this.#instancePrivate = value;
    }

    // Test compound assignment
    static incrementStaticPrivate() {
      TestClass.#staticPrivate += " modified";
    }
  }

  expect(TestClass.getStaticPrivate()).toBe("static value");
  TestClass.setStaticPrivate("new static value");
  expect(TestClass.getStaticPrivate()).toBe("new static value");

  TestClass.incrementStaticPrivate();
  expect(TestClass.getStaticPrivate()).toBe("new static value modified");

  let instance = new TestClass();
  expect(instance.getInstancePrivate()).toBe("instance value");
  instance.setInstancePrivate("new instance value");
  expect(instance.getInstancePrivate()).toBe("new instance value");
});
