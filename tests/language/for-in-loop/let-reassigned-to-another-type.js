/*---
description: A let that a for...in loop body reassigns to another type keeps JavaScript semantics at earlier reads
features: [compat-for-in-loop, let-declaration]
---*/

test("an assignment later in a for...in body", () => {
  let x = 1;
  const seen = [];
  for (const key in { a: 1, b: 2 }) {
    seen.push(x + 1);
    x = key;
  }
  expect(seen).toEqual([2, "a1"]);
});
