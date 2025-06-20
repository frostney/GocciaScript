/*---
description: Spread syntax edge cases, property ordering, and error conditions
features: [object-spread, array-spread, spread-overrides]
---*/

test("object spread with property ordering", () => {
  const obj1 = { a: 1, b: 2 };
  const obj2 = { c: 3, d: 4 };

  const combined = { ...obj1, ...obj2, b: 5, e: 6 };
  expect(combined).toEqual({ a: 1, b: 5, c: 3, d: 4, e: 6 });

  // Property order should be preserved
  const keys = Object.keys(combined);
  expect(keys).toEqual(["a", "b", "c", "d", "e"]);
});

test("object spread with property overrides", () => {
  const base = { name: "John", age: 30, active: true };

  // Later properties override earlier ones
  const updated = {
    ...base,
    age: 31,
    active: false,
    email: "john@example.com",
  };
  expect(updated).toEqual({
    name: "John",
    age: 31,
    active: false,
    email: "john@example.com",
  });

  // Multiple spreads with overrides
  const obj1 = { a: 1, b: 2 };
  const obj2 = { b: 3, c: 4 };
  const obj3 = { c: 5, d: 6 };

  const result = { ...obj1, ...obj2, ...obj3, a: 7 };
  expect(result).toEqual({ a: 7, b: 3, c: 5, d: 6 });
});

test("nested spread operations", () => {
  const level1 = { a: 1, b: 2 };
  const level2 = { ...level1, c: 3 };
  const level3 = { ...level2, d: 4 };

  expect(level3).toEqual({ a: 1, b: 2, c: 3, d: 4 });
});
