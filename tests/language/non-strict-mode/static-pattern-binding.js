/*---
description: Non-strict code binds `static` in binding patterns and for-heads (ES2026 §13.1.1, §14.3.3, §14.7.5)
features: [compat-non-strict-mode]
---*/

test("array pattern elements bind static", () => {
  const [static] = [1];
  expect(static).toBe(1);
});

test("rest elements bind static", () => {
  const [...static] = [1, 2];
  expect(static).toEqual([1, 2]);
});

test("object pattern targets bind static", () => {
  const { a: static } = { a: 3 };
  expect(static).toBe(3);
});

test("parameter patterns bind static", () => {
  const read = ([static]) => static;
  expect(read([4])).toBe(4);
});

test("for...of heads bind static", () => {
  const seen = [];
  for (const static of [5, 6]) {
    seen.push(static);
  }
  expect(seen).toEqual([5, 6]);
});
