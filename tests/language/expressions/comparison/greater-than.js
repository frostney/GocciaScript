/*---
 description: Greater than operator
 features: [greater-than]
---*/

test("greater than operator", () => {
  expect(1 > 0).toBe(true);
  expect(0 > 1).toBe(false);
  expect(1 > 1).toBe(false);
});
