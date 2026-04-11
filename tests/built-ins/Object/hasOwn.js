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

test("Object.hasOwn throws for null", () => {
  expect(() => Object.hasOwn(null, "foo")).toThrow(TypeError);
});

test("Object.hasOwn throws for undefined", () => {
  expect(() => Object.hasOwn(undefined, "foo")).toThrow(TypeError);
});
