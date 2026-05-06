/*---
description: var in for-init hoists out of the loop
features: [compat-traditional-for-loop, compat-var]
---*/

test("var in for-init is visible after the loop", () => {
  for (var k = 5; k < 10; k++) {}
  expect(k).toBe(10);
});

test("var in for-init hoists into enclosing function", () => {
  const f = () => {
    for (var n = 0; n < 4; n++) {}
    return n;
  };
  expect(f()).toBe(4);
});

test("multiple var declarations in for-init hoist", () => {
  const f = () => {
    for (var a = 1, b = 2; a < 4; a++, b++) {}
    return [a, b];
  };
  expect(f()).toEqual([4, 5]);
});
