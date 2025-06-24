test("Object.hasOwn", () => {
  const obj = { name: "John" };
  expect(Object.hasOwn(obj, "name")).toBe(true);
  expect(Object.hasOwn(obj, "age")).toBe(false);
});

test("Object.hasOwn with prototype", () => {
  const obj = Object.create({ name: "John" });
  expect(Object.hasOwn(obj, "name")).toBe(false);
  expect(Object.hasOwn(obj, "age")).toBe(false);
});
