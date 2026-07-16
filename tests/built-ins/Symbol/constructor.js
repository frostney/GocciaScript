/*---
description: Symbol constructor behavior
features: [Symbol]
---*/

test("creates unique symbol primitives", () => {
  const first = Symbol("same");
  const second = Symbol("same");
  expect(typeof first).toBe("symbol");
  expect(first).not.toBe(second);
});

test("distinguishes absent and empty descriptions", () => {
  expect(Symbol().description).toBeUndefined();
  expect(Symbol(undefined).description).toBeUndefined();
  expect(Symbol("").description).toBe("");
});

test("converts descriptions with ToString", () => {
  const calls = [];
  const symbol = Symbol({
    valueOf() {
      calls.push("valueOf");
      return {};
    },
    toString() {
      calls.push("toString");
      return "object description";
    },
  });

  expect(symbol.description).toBe("object description");
  expect(calls).toEqual(["toString"]);
  expect(() => Symbol(Symbol("description"))).toThrow(TypeError);
});

test("is not directly constructable", () => {
  expect(() => new Symbol()).toThrow(TypeError);
  expect(() => new Symbol("description")).toThrow(TypeError);
});

test("can be used as a newTarget", () => {
  const value = Reflect.construct(class {}, [], Symbol);
  expect(Object.getPrototypeOf(value)).toBe(Symbol.prototype);
});
