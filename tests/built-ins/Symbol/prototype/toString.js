/*---
description: Symbol.prototype.toString
features: [Symbol]
---*/

test("returns the descriptive string", () => {
  expect(Symbol("foo").toString()).toBe("Symbol(foo)");
  expect(Symbol().toString()).toBe("Symbol()");
});

test("accepts boxed symbols and rejects incompatible receivers", () => {
  expect(Symbol.prototype.toString.call(Object(Symbol("boxed")))).toBe("Symbol(boxed)");
  for (const receiver of [42, "hello", {}, null, undefined]) {
    expect(() => Symbol.prototype.toString.call(receiver)).toThrow(TypeError);
  }
});

test("is not constructable", () => {
  expect(() => new Symbol.prototype.toString()).toThrow(TypeError);
});
