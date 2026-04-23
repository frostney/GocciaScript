/*---
description: Class method definitions are non-enumerable per ES §14.3.7
features: [classes]
---*/

describe("class method enumerability", () => {
  test("class prototype methods are non-enumerable", () => {
    class Foo {
      bar() { return 1; }
      baz() { return 2; }
    }
    const barDesc = Object.getOwnPropertyDescriptor(Foo.prototype, "bar");
    expect(barDesc.enumerable).toBe(false);
    expect(barDesc.writable).toBe(true);
    expect(barDesc.configurable).toBe(true);

    const bazDesc = Object.getOwnPropertyDescriptor(Foo.prototype, "baz");
    expect(bazDesc.enumerable).toBe(false);
  });

  test("Object.keys on prototype does not include methods", () => {
    class MyClass {
      doStuff() {}
    }
    const keys = Object.keys(MyClass.prototype);
    expect(keys.includes("doStuff")).toBe(false);
  });

  test("constructor property is also non-enumerable", () => {
    class C {}
    const desc = Object.getOwnPropertyDescriptor(C.prototype, "constructor");
    expect(desc.enumerable).toBe(false);
  });
});
