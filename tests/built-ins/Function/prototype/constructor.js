/*---
description: Function.prototype.constructor
features: [Function]
---*/

describe("Function.prototype.constructor", () => {
  test("Function.prototype.constructor is Function", () => {
    expect(Function.prototype.constructor).toBe(Function);
  });

  test("function instance .constructor is Function", () => {
    const foo = () => {};
    expect(foo.constructor).toBe(Function);
  });

  test("arrow function .constructor is Function", () => {
    const arrow = () => {};
    expect(arrow.constructor).toBe(Function);
  });

  test("method .constructor is Function", () => {
    const obj = { method() {} };
    expect(obj.method.constructor).toBe(Function);
  });

  test("class .constructor is Function", () => {
    class Foo {}
    expect(Foo.constructor).toBe(Function);
  });

  test("derived class .constructor is Function", () => {
    class Base {}
    class Derived extends Base {}
    expect(Derived.constructor).toBe(Function);
  });

  test("class instance .constructor is the class", () => {
    class Foo {}
    const foo = new Foo();
    expect(foo.constructor).toBe(Foo);
  });

  test("derived class instance .constructor is the derived class", () => {
    class Base {}
    class Derived extends Base {}
    const d = new Derived();
    expect(d.constructor).toBe(Derived);
  });
});
