/*---
description: Bind function programatically
features: [function-bind]
---*/

test("bind function", () => {
  const add = (a, b) => a + b;
  const result = add.bind(null)(1, 2);
  expect(result).toBe(3);
});

test("bind function on object", () => {
  const obj = {
    value: 1,
    add: (a, b) => a + b,
  };
  const result = obj.add.bind(obj)(2, 3);
  expect(result).toBe(5);
});
