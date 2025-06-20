/*---
description: Destructuring with defaults
features: [destructuring]
---*/

test("array destructuring with defaults not triggered", () => {
  let [a = 1, b = 2, c = 3] = [4, 5, 6];
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(6);
});

test("array destructuring with defaults triggered", () => {
  let [a = 1, b = 2, c = 3] = [];
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(c).toBe(3);
});

test("array destructuring with null and undefined", () => {
  let [a = 1, b = 2] = [null, undefined];
  expect(a).toBe(null);
  expect(b).toBe(2);
});

test("object destructuring with defaults not triggered", () => {
  let { a = 1, b = 2, c = 3 } = { a: 4, b: 5 };
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(3);
});

test("object destructuring with defaults triggered", () => {
  let { a = 1, b = 2, c = 3 } = {};
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(c).toBe(3);
});

test("array destructuring with defaults and rest", () => {
  let [a = 1, b = 2, c = 3, ...rest] = [4, 5, 6, 7, 8];
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(6);
  expect(rest).toEqual([7, 8]);
});

test("object destructuring with defaults and rest", () => {
  let { a = 1, b = 2, c = 3, ...rest } = { a: 4, b: 5, c: 6, d: 7, e: 8 };
  expect(a).toBe(4);
  expect(b).toBe(5);
  expect(c).toBe(6);
  expect(rest).toEqual({ d: 7, e: 8 });
});
