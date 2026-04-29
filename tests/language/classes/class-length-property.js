/*---
description: Class.length reflects constructor arity per ES spec
features: [classes, Function.length]
---*/

describe("class length property", () => {
  test("class without constructor has length 0", () => {
    class Foo {}
    expect(Foo.length).toBe(0);
  });

  test("class constructor with no parameters has length 0", () => {
    class Foo {
      constructor() {}
    }
    expect(Foo.length).toBe(0);
  });

  test("class constructor with parameters has matching length", () => {
    class Foo {
      constructor(a, b, c) {}
    }
    expect(Foo.length).toBe(3);
  });

  test("class constructor stops counting at first default", () => {
    class Foo {
      constructor(a, b, c = 1) {}
    }
    expect(Foo.length).toBe(2);
  });

  test("class constructor stops counting at rest parameter", () => {
    class Foo {
      constructor(a, b, ...rest) {}
    }
    expect(Foo.length).toBe(2);
  });

  test("class length descriptor matches spec", () => {
    class Foo {
      constructor(a, b) {}
    }
    const descriptor = Object.getOwnPropertyDescriptor(Foo, "length");
    expect(descriptor.value).toBe(2);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
  });

  test("subclass length tracks own constructor", () => {
    class Base {
      constructor(a, b, c) {}
    }
    class Child extends Base {
      constructor(a) {
        super(a, 1, 2);
      }
    }
    expect(Base.length).toBe(3);
    expect(Child.length).toBe(1);
  });

  test("built-in collection constructors have length 0", () => {
    expect(Map.length).toBe(0);
    expect(Set.length).toBe(0);
    expect(WeakMap.length).toBe(0);
    expect(WeakSet.length).toBe(0);
  });
});
