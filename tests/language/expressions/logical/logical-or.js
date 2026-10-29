/*---
description: Logical OR (||) operator works correctly
features: [logical-or-operator]
---*/

test("logical OR (||)", () => {
  expect(true || true).toBeTruthy();
  expect(true || false).toBeTruthy();
  expect(false || true).toBeTruthy();
  expect(false || false).toBeFalsy();
});

test("logical OR (||) with short-circuit evaluation", () => {
  let a = 0;
  expect(false || (a = 1)).toBeTruthy();
  expect(a).toBe(1);
});

test("right-nested initializers preserve calls and short circuit their suffix", () => {
  let calls = 0;
  const value = (p) => {
    calls += 1;
    return p;
  };
  const first = false || (false || (value(3) || value(4)));
  expect(first).toBe(3);
  expect(calls).toBe(1);
});
