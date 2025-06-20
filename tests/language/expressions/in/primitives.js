/*---
description: |
  Tests for the 'in' operator to check property/index existence
  in objects, arrays, and strings
---*/

test("in operator with other types", () => {
  // Should return false for other types
  expect("property" in null).toBe(false);
  expect("property" in undefined).toBe(false);
  expect("property" in 123).toBe(false);
  expect("property" in true).toBe(false);
});
