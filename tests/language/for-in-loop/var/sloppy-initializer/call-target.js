/*---
description: A non-strict for-in or for-of head may name a call; the call runs, then a ReferenceError (Annex B.3.9)
features: [compat-for-in-loop, compat-var, compat-non-strict-mode, compat-traditional-for-loop]
---*/

test("a for-in head that is a call throws after calling it", () => {
  let calls = 0;
  const f = () => { calls += 1; return {}; };
  expect(() => { for (f() in { a: 1 }) {} }).toThrow(ReferenceError);
  expect(calls).toBe(1);
});

test("a for-of head that is a call throws after calling it", () => {
  let calls = 0;
  const f = () => { calls += 1; return {}; };
  expect(() => { for (f() of [1]) {} }).toThrow(ReferenceError);
  expect(calls).toBe(1);
});

test("an empty for-in never evaluates the head", () => {
  let calls = 0;
  const f = () => { calls += 1; return {}; };
  for (f() in {}) {}
  expect(calls).toBe(0);
});
