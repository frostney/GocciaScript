test("object create", () => {
  const obj = Object.create({ name: "John" });
  expect(obj.name).toBe("John");
});

test("object create with null prototype", () => {
  const obj = Object.create(null);
  expect(obj.name).toBe(undefined);
});

test("object create with prototype chain", () => {
  const obj = Object.create(null);
  const obj2 = Object.create(obj);
  expect(obj2.name).toBe(undefined);
});
