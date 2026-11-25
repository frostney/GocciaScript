/*---
description: Annotated parameters of class constructors, methods and setters are checked like any other function's (#2156)
features: [types-as-comments, strict-type-enforcement]
---*/

describe("class parameter type enforcement", () => {

test("a constructor parameter rejects the wrong type", () => {
  class Point { constructor(x: number) { this.x = x; } }
  expect(() => new Point("a")).toThrow(TypeError);
  expect(new Point(2).x).toBe(2);
});

test("a method parameter rejects the wrong type", () => {
  class Doubler { double(x: number) { return x * 2; } }
  expect(() => new Doubler().double("a")).toThrow(TypeError);
  expect(new Doubler().double(4)).toBe(8);
});

test("a static method parameter rejects the wrong type", () => {
  class Greeter { static greet(name: string) { return "hi " + name; } }
  expect(() => Greeter.greet(1)).toThrow(TypeError);
  expect(Greeter.greet("you")).toBe("hi you");
});

test("a private method parameter rejects the wrong type", () => {
  class Flag {
    #set(on: boolean) { return on; }
    run(value) { return this.#set(value); }
  }
  expect(() => new Flag().run(0)).toThrow(TypeError);
  expect(new Flag().run(true)).toBe(true);
});

test("a computed method parameter rejects the wrong type", () => {
  const name = "scale";
  class Scaler { [name](x: number) { return x * 10; } }
  expect(() => new Scaler().scale("a")).toThrow(TypeError);
  expect(new Scaler().scale(2)).toBe(20);
});

test("a setter parameter rejects the wrong type", () => {
  class Box {
    #v = 0;
    set v(x: number) { this.#v = x; }
    get v() { return this.#v; }
  }
  const box = new Box();
  expect(() => { box.v = "a"; }).toThrow(TypeError);
  box.v = 3;
  expect(box.v).toBe(3);
});

test("a computed setter parameter rejects the wrong type", () => {
  class Box {
    #v = 0;
    set ["w"](x: number) { this.#v = x; }
    get w() { return this.#v; }
  }
  const box = new Box();
  expect(() => { box.w = "a"; }).toThrow(TypeError);
  box.w = 4;
  expect(box.w).toBe(4);
});

test("a class expression method parameter rejects the wrong type", () => {
  const C = class { m(x: number) { return x; } };
  expect(() => new C().m("a")).toThrow(TypeError);
});

test("an assignment to a typed method parameter is checked", () => {
  class C { m(x: number) { x = "s"; return x; } }
  expect(() => new C().m(1)).toThrow(TypeError);
});

test("an optional parameter is not checked on entry", () => {
  class C { opt(x?: number) { return x; } }
  expect(new C().opt("a")).toBe("a");
});

test("a defaulted parameter is not checked on entry", () => {
  class C { def(x: number = 1) { return x; } }
  expect(new C().def("a")).toBe("a");
});

test("a rest parameter's array is not checked on entry", () => {
  class C { rest(...xs: number) { return xs.length; } }
  expect(new C().rest(1, 2)).toBe(2);
});

test("an unannotated parameter is not checked", () => {
  class C { plain(x) { return x; } }
  expect(new C().plain("a")).toBe("a");
});

test("an argument is checked before a later default reads it", () => {
  class C { m(x: number, y = x + 1) { return y; } }
  expect(() => new C().m("x")).toThrow(TypeError);
  expect(new C().m(1)).toBe(2);
  const f = (x: number, y = x + 1) => y;
  expect(() => f("x")).toThrow(TypeError);
  const o = { m(x: number, y = x + 1) { return y; } };
  expect(() => o.m("x")).toThrow(TypeError);
});

test("an argument is checked before a later default replaces it", () => {
  class C { m(x: number, y = (x = 3)) { return x; } }
  expect(() => new C().m("x")).toThrow(TypeError);
  const f = (x: number, y = (x = 3)) => x;
  expect(() => f("x")).toThrow(TypeError);
});

}); // describe
