/*---
description: Traditional for update increments mutate while discarding expression results
features: [compat-traditional-for-loop]
---*/

test("post-increment update coerces and mutates while result is discarded", () => {
  let i = "0";
  let iterations = 0;
  for (; i < 3; i++) {
    iterations++;
  }
  expect(iterations).toBe(3);
  expect(i).toBe(3);
});

test("post-decrement update coerces and mutates while result is discarded", () => {
  let i = "3";
  let iterations = 0;
  for (; i > 0; i--) {
    iterations++;
  }
  expect(iterations).toBe(3);
  expect(i).toBe(0);
});

test("prefix update coerces and mutates while result is discarded", () => {
  let i = "0";
  let iterations = 0;
  for (; i < 3; ++i) {
    iterations++;
  }
  expect(iterations).toBe(3);
  expect(i).toBe(3);
});

test("BigInt update preserves ToNumeric semantics while result is discarded", () => {
  let i = 0n;
  let iterations = 0;
  for (; i < 3n; i++) {
    iterations++;
  }
  expect(iterations).toBe(3);
  expect(i).toBe(3n);
});
