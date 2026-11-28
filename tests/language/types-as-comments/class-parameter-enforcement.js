/*---
description: Annotated parameters of class constructors, methods and setters are checked like any other function's (#2156), every argument before the parameter list's code runs (#2159)
features: [types-as-comments, strict-type-enforcement]
---*/

describe("class parameter type enforcement", () => {

test("a constructor parameter rejects the wrong type", () => {
  class Point { constructor(x: number) { this.x = x; } }
  expect(() => new Point("a")).toThrow(TypeError);
});

test("a constructor parameter accepts the right type", () => {
  class Point { constructor(x: number) { this.x = x; } }
  expect(new Point(2).x).toBe(2);
});

test("a method parameter rejects the wrong type", () => {
  class Doubler { double(x: number) { return x * 2; } }
  expect(() => new Doubler().double("a")).toThrow(TypeError);
});

test("a method parameter accepts the right type", () => {
  class Doubler { double(x: number) { return x * 2; } }
  expect(new Doubler().double(4)).toBe(8);
});

test("a static method parameter rejects the wrong type", () => {
  class Greeter { static greet(name: string) { return "hi " + name; } }
  expect(() => Greeter.greet(1)).toThrow(TypeError);
});

test("a private method parameter rejects the wrong type", () => {
  class Flag {
    #set(on: boolean) { return on; }
    run(value) { return this.#set(value); }
  }
  expect(() => new Flag().run(0)).toThrow(TypeError);
});

test("a computed method parameter rejects the wrong type", () => {
  const name = "scale";
  class Scaler { [name](x: number) { return x * 10; } }
  expect(() => new Scaler().scale("a")).toThrow(TypeError);
});

test("a setter parameter rejects the wrong type", () => {
  class Box { set v(x: number) {} }
  const box = new Box();
  expect(() => { box.v = "a"; }).toThrow(TypeError);
});

test("a setter parameter accepts the right type", () => {
  class Box {
    #v = 0;
    set v(x: number) { this.#v = x; }
    get v() { return this.#v; }
  }
  const box = new Box();
  box.v = 3;
  expect(box.v).toBe(3);
});

test("a computed setter parameter rejects the wrong type", () => {
  class Box { set ["w"](x: number) {} }
  const box = new Box();
  expect(() => { box.w = "a"; }).toThrow(TypeError);
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

test("a defaulted parameter is checked once its default applies", () => {
  class C { def(x: number = 1) { return x; } }
  expect(new C().def()).toBe(1);
  expect(() => new C().def("a")).toThrow(TypeError);
});

test("a rest parameter's arguments are not checked on entry", () => {
  class C { rest(...xs: number) { return xs.length; } }
  expect(new C().rest("mismatch", 2)).toBe(2);
});

test("an unannotated parameter is not checked", () => {
  class C { plain(x) { return x; } }
  expect(new C().plain("a")).toBe("a");
});

test("a method argument is checked before a later default reads it", () => {
  class C { m(x: number, y = x + 1) { return y; } }
  expect(() => new C().m("x")).toThrow(TypeError);
});

test("a method argument that a later default reads still works when it matches", () => {
  class C { m(x: number, y = x + 1) { return y; } }
  expect(new C().m(1)).toBe(2);
});

test("an arrow function argument is checked before a later default reads it", () => {
  const f = (x: number, y = x + 1) => y;
  expect(() => f("x")).toThrow(TypeError);
});

test("an object method argument is checked before a later default reads it", () => {
  const o = { m(x: number, y = x + 1) { return y; } };
  expect(() => o.m("x")).toThrow(TypeError);
});

test("a method argument is checked before a later default replaces it", () => {
  class C { m(x: number, y = (x = 3)) { return x; } }
  expect(() => new C().m("x")).toThrow(TypeError);
});

test("an arrow function argument is checked before a later default replaces it", () => {
  const f = (x: number, y = (x = 3)) => x;
  expect(() => f("x")).toThrow(TypeError);
});

test("an argument after a default is checked before that default runs", () => {
  let effects = 0;
  class C { m(y = ++effects, x: number) { return x; } }
  expect(() => new C().m(undefined, "x")).toThrow(TypeError);
  expect(effects).toBe(0);
});

test("an argument after a default runs the default when it matches", () => {
  let effects = 0;
  class C { m(y = ++effects, x: number) { return x; } }
  expect(new C().m(undefined, 1)).toBe(1);
  expect(effects).toBe(1);
});

}); // describe
