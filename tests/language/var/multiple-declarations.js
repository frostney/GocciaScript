/*---
description: var supports comma-separated multiple declarations
features: [compat-var]
---*/

test("multiple var declarations in one statement", () => {
  var a = 1, b = 2, c = 3;
  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(c).toBe(3);
});

test("multiple var declarations with mixed initializers", () => {
  var x = 10, y, z = 30;
  expect(x).toBe(10);
  expect(y).toBeUndefined();
  expect(z).toBe(30);
});
