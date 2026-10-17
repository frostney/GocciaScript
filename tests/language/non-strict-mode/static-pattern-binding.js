/*---
description: Non-strict code binds `static` in binding patterns (ES2026 §13.1.1, §14.3.3)
features: [compat-non-strict-mode]
---*/

test("array, rest and object pattern targets bind static", () => {
  const [static] = [1];
  const [...rest] = [static, 2];
  const { a: other } = { a: 3 };
  expect(static).toBe(1);
  expect(rest).toEqual([1, 2]);
  expect(other).toBe(3);
});

test("parameter patterns bind static", () => {
  const read = ([static]) => static;
  expect(read([4])).toBe(4);
});
