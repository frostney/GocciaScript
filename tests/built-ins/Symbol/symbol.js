/*---
description: Symbol creation and basic behavior
features: [Symbol]
---*/

test("Symbol creates unique values", () => {
  const s1 = Symbol();
  const s2 = Symbol();
  expect(s1).not.toBe(s2);
});

test("Symbol with description via String()", () => {
  const s = Symbol("test");
  expect(String(s)).toBe("Symbol(test)");
});

test("Symbol without description via String()", () => {
  const s = Symbol();
  expect(String(s)).toBe("Symbol()");
});

test("typeof Symbol returns symbol", () => {
  const s = Symbol("foo");
  expect(typeof s).toBe("symbol");
});

test("Symbols with same description are not equal", () => {
  const s1 = Symbol("same");
  const s2 = Symbol("same");
  expect(s1).not.toBe(s2);
  expect(s1 === s2).toBe(false);
});

test("Symbol as object property key", () => {
  const key = Symbol("myKey");
  const obj = {};
  obj[key] = "value";
  expect(obj[key]).toBe("value");
});

test("Symbol properties are not enumerable by string keys", () => {
  const sym = Symbol("hidden");
  const obj = { visible: true, [sym]: "secret" };
  expect(Object.keys(obj)).toEqual(["visible"]);
});

test("Symbol in computed property of object literal", () => {
  const sym = Symbol("name");
  const obj = { [sym]: 42 };
  expect(obj[sym]).toBe(42);
});

test("Symbol.prototype exists", () => {
  expect(typeof Symbol.prototype).toBe("object");
});

test("symbol.toString() returns descriptive string", () => {
  const s = Symbol("foo");
  expect(s.toString()).toBe("Symbol(foo)");
  expect(Symbol().toString()).toBe("Symbol()");
});

test("symbol.description returns the description", () => {
  expect(Symbol("foo").description).toBe("foo");
  expect(Symbol().description).toBe(undefined);
});

test("symbol.toString with non-symbol receiver throws TypeError", () => {
  const fn = Symbol("foo").toString;
  expect(() => fn.call(42)).toThrow(TypeError);
  expect(() => fn.call("hello")).toThrow(TypeError);
  expect(() => fn.call({})).toThrow(TypeError);
  expect(() => fn.call(null)).toThrow(TypeError);
  expect(() => fn.call(undefined)).toThrow(TypeError);
});
