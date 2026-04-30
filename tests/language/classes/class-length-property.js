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

  test("native single-arg constructors have length 1 per spec", () => {
    // ECMAScript 23.1.1.1, 22.1.1.1, 21.1.1.1, 20.3.1.1, 25.1.1.1, 25.2.1.1
    expect(Array.length).toBe(1);
    expect(String.length).toBe(1);
    expect(Number.length).toBe(1);
    expect(Boolean.length).toBe(1);
    expect(ArrayBuffer.length).toBe(1);
    expect(SharedArrayBuffer.length).toBe(1);
    // WHATWG: URL(url, base?) — required url means length 1.
    expect(URL.length).toBe(1);
    // Fetch: Response constructor reports 1 in WPT/V8.
    expect(Response.length).toBe(1);
  });

  test("zero-arg constructors keep length 0", () => {
    expect(TextEncoder.length).toBe(0);
    expect(TextDecoder.length).toBe(0);
    expect(URLSearchParams.length).toBe(0);
    expect(Headers.length).toBe(0);
  });

  test("explicit length redefinition is honored in own descriptor", () => {
    // length is configurable per spec, so userland can override it.
    class Foo {
      constructor(a, b) {}
    }
    Object.defineProperty(Foo, "length", { value: 99, configurable: true });
    const descriptor = Object.getOwnPropertyDescriptor(Foo, "length");
    expect(descriptor.value).toBe(99);
  });
});
