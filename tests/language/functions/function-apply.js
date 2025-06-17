/*---
description: Apply function programatically
features: [function-apply]
---*/

test("apply function programatically", () => {
  const add = (a, b) => a + b;
  const result = add.apply(null, [1, 2]);
  expect(result).toBe(3);
});

test("apply function on object", () => {
  const obj = {
    value: 1,
    add: (a, b) => a + b,
  };
  const result = obj.add.apply(obj, [2, 3]);
  expect(result).toBe(5);
});
