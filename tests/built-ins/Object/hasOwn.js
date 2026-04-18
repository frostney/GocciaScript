test("Object.hasOwn", () => {
  const obj = { name: "John" };
  expect(Object.hasOwn(obj, "name")).toBe(true);
  expect(Object.hasOwn(obj, "age")).toBe(false);
});

test("Object.hasOwn has the correct function length", () => {
  expect(Object.hasOwn.length).toBe(2);
});

test("Object.hasOwn with prototype", () => {
  const obj = Object.create({ name: "John" });
  expect(Object.hasOwn(obj, "name")).toBe(false);
  expect(Object.hasOwn(obj, "age")).toBe(false);
});

test("Object.hasOwn with number coerces to wrapper — no own props", () => {
  expect(Object.hasOwn(42, "toString")).toBe(false);
});

test("Object.hasOwn with string returns true for character indices", () => {
  expect(Object.hasOwn("str", "0")).toBe(true);
  expect(Object.hasOwn("str", "1")).toBe(true);
  expect(Object.hasOwn("str", "2")).toBe(true);
  expect(Object.hasOwn("str", "3")).toBe(false);
  expect(Object.hasOwn("str", "length")).toBe(true);
});

test("Object.hasOwn with string rejects non-canonical indices", () => {
  // "01" and "-0" are not valid string indices per ES2026 §10.4.3
  expect(Object.hasOwn("str", "01")).toBe(false);
  expect(Object.hasOwn("str", "-0")).toBe(false);
});

test("Object.hasOwn with symbol key", () => {
  const sym = Symbol("key");
  const obj = {};
  obj[sym] = 42;
  expect(Object.hasOwn(obj, sym)).toBe(true);
  expect(Object.hasOwn(obj, Symbol("key"))).toBe(false);
  expect(Object.hasOwn({}, sym)).toBe(false);
});

test("Object.hasOwn with symbol key does not match prototype symbol", () => {
  const sym = Symbol("inherited");
  const proto = {};
  proto[sym] = 1;
  const child = Object.create(proto);
  expect(Object.hasOwn(child, sym)).toBe(false);
  expect(Object.hasOwn(proto, sym)).toBe(true);
});

// §20.1.2.9 — Object.hasOwn works on null-prototype objects where
// Object.prototype.hasOwnProperty is unavailable (MDN "Problematic cases").

test("Object.hasOwn works on null-prototype objects", () => {
  const obj = Object.create(null);
  obj.a = 1;
  obj.b = undefined;
  expect(Object.hasOwn(obj, "a")).toBe(true);
  expect(Object.hasOwn(obj, "b")).toBe(true);
  expect(Object.hasOwn(obj, "c")).toBe(false);
  // Confirm hasOwnProperty is not available on the object itself
  expect(typeof obj.hasOwnProperty).toBe("undefined");
});

test("Object.hasOwn via call/apply is not shadowed by target properties", () => {
  const obj = { a: 5, hasOwn: () => false };
  // Own hasOwn returns false — the override is in effect
  expect(obj.hasOwn("a")).toBe(false);
  // Static method with this bound to obj still works correctly
  expect(Object.hasOwn.call(obj, obj, "a")).toBe(true);
  expect(Object.hasOwn.call(obj, obj, "hasOwn")).toBe(true);
  expect(Object.hasOwn.call(obj, obj, "missing")).toBe(false);
});

test("Object.hasOwn throws for null", () => {
  expect(() => Object.hasOwn(null, "foo")).toThrow(TypeError);
});

test("Object.hasOwn throws for undefined", () => {
  expect(() => Object.hasOwn(undefined, "foo")).toThrow(TypeError);
});
