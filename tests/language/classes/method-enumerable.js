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

  test("static methods are non-enumerable on the constructor", () => {
    class Foo {
      static bar() { return 1; }
    }

    const desc = Object.getOwnPropertyDescriptor(Foo, "bar");
    expect(desc.enumerable).toBe(false);
    expect(desc.writable).toBe(true);
    expect(desc.configurable).toBe(true);
    expect(Object.keys(Foo).includes("bar")).toBe(false);
  });

  test("computed accessors and methods are non-enumerable", () => {
    const key = "value";
    const symbol = Symbol("value");
    class Foo {
      get [key]() { return 1; }
      set [key](v) {}
      get [symbol]() { return 2; }
      [key + "Method"]() { return 3; }
      [symbol.description + "Static"]() { return 4; }
    }

    const accessor = Object.getOwnPropertyDescriptor(Foo.prototype, key);
    expect(accessor.enumerable).toBe(false);
    expect(accessor.configurable).toBe(true);
    expect(typeof accessor.get).toBe("function");
    expect(typeof accessor.set).toBe("function");
    expect(Object.getOwnPropertyDescriptor(Foo.prototype, symbol).enumerable).toBe(false);
    const method = Object.getOwnPropertyDescriptor(Foo.prototype, "valueMethod");
    expect(method.enumerable).toBe(false);
    expect(method.writable).toBe(true);
    expect(Object.keys(Foo.prototype)).toEqual([]);
  });

  test("a computed constructor method is an ordinary prototype method", () => {
    const key = "constructor";
    class C {
      [key]() { return "method"; }
    }

    expect(C.prototype.constructor === C).toBe(false);
    expect(new C().constructor()).toBe("method");
    expect(Object.getOwnPropertyDescriptor(C.prototype, "constructor").enumerable).toBe(false);
  });

  test("constructor property is also non-enumerable", () => {
    class C {}
    const desc = Object.getOwnPropertyDescriptor(C.prototype, "constructor");
    expect(desc.enumerable).toBe(false);
  });
});
