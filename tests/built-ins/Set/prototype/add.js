/*---
description: Set.prototype.add adds values and returns the Set for chaining
features: [Set.prototype.add]
---*/

test("add returns the Set for chaining", () => {
  const set = new Set();
  const result = set.add(1);
  expect(result).toBe(set);
});

test("add chaining", () => {
  const set = new Set();
  set.add(1).add(2).add(3);
  expect(set.size).toBe(3);
  expect(set.has(1)).toBe(true);
  expect(set.has(2)).toBe(true);
  expect(set.has(3)).toBe(true);
});

test("add does not insert duplicates", () => {
  const set = new Set();
  set.add(1);
  set.add(1);
  set.add(1);
  expect(set.size).toBe(1);
});

test("add with different types", () => {
  const set = new Set();
  set.add(1);
  set.add("1");
  set.add(true);
  expect(set.size).toBe(3);
});

test("add NaN is treated as equal to NaN", () => {
  const set = new Set();
  set.add(NaN);
  set.add(NaN);
  expect(set.size).toBe(1);
  expect(set.has(NaN)).toBe(true);
});

test("add null and undefined as separate values", () => {
  const set = new Set();
  set.add(null);
  set.add(undefined);
  expect(set.size).toBe(2);
  expect(set.has(null)).toBe(true);
  expect(set.has(undefined)).toBe(true);
});

test("add object references", () => {
  const set = new Set();
  const obj1 = { a: 1 };
  const obj2 = { a: 1 };
  set.add(obj1);
  set.add(obj2);
  expect(set.size).toBe(2);
  set.add(obj1);
  expect(set.size).toBe(2);
});
