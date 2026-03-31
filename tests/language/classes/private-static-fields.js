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
